package com.jamesward.sbta

import scala.collection.mutable
import scala.util.control.NonFatal

import tastyquery.Classpaths.ClasspathEntry
import tastyquery.Contexts.Context
import tastyquery.Symbols.*


/**
 * Symbol search & inspection over TASTy via tasty-query.
 *
 * STUB fidelity notes (compiled against tasty-query 1.8; binary-compatible with
 * the isolated tasty-query 1.9 reader):
 *  - `glob-search` walks packages from the root and matches the unqualified name
 *    case-insensitively (Metals matches the last FQN segment at a name boundary;
 *    we approximate with a boundary-aware `startsWith`/equality plus `contains`
 *    fallback). Results are capped.
 *  - Symbol "kind" is coarse (class/term/type/package); precise object/trait
 *    disambiguation via flags is deferred.
 *  - `inspect` renders members as Scala signatures ([[ScalaSignatures]]), with
 *    types named without their packages.
 */
object SymbolIndex {

  final case class Hit(kind: String, fqn: String)

  def globSearch(
      query: String,
      inPackage: Option[String] = None,
      limit: Int = 100,
      searchEntries: List[ClasspathEntry] = Nil,
  )(using ctx: Context): List[Hit] = {
    val q             = query.toLowerCase.trim
    val listAll       = q.isEmpty || q == "*"
    val out           = mutable.ListBuffer.empty[Hit]
    val deadlineNanos = System.nanoTime() + java.util.concurrent.TimeUnit.SECONDS.toNanos(10)
    var visited       = 0

    def withinBudget: Boolean =
      visited < 20000 && System.nanoTime() < deadlineNanos

    // A glob (`*Service`, `Foo*`, `*Version*Ordering*`) matches whole names; a plain
    // query matches any name containing it.
    val glob = Option.when(q.exists(c => c == '*' || c == '?'))(
      q.split("\\*", -1).map(_.split("\\?", -1).map(java.util.regex.Pattern.quote).mkString(".")).mkString(".*").r
    )
    def matches(name: String): Boolean =
      listAll || {
        val n = name.toLowerCase.stripSuffix("$")
        glob.fold(n.contains(q))(_.matches(n))
      }

    def fqn(sym: Symbol): String =
      try sym.displayFullName
      catch { case NonFatal(_) => sym.toString }

    def addMember(kind: String, sym: TermOrTypeSymbol, name: String): Unit =
      if out.size < limit && withinBudget then
        visited += 1
        if matches(name) && visible(sym) then out += Hit(kind, fqn(sym))

    def visit(sym: Symbol): Unit = {
      if (out.size >= limit || !withinBudget) return
      visited += 1
      try
        sym match {
          case pkg: PackageSymbol =>
            pkg.declarations.iterator
              .takeWhile(_ => out.size < limit && withinBudget)
              .foreach(visit)
          case cls: ClassSymbol =>
            if (matches(cls.name.toString) && visible(cls)) out += Hit(kindOf(cls), fqn(cls))
            // ClassSymbol.declarations is List[TermOrTypeSymbol] (sealed:
            // TermSymbol | TypeSymbol), so these cases are exhaustive.
            cls.declarations.iterator
              .takeWhile(_ => out.size < limit && withinBudget)
              .foreach {
                case nested: ClassSymbol => visit(nested)
                case t: TermSymbol       => addMember("method", t, t.name.toString)
                case ty: TypeSymbol      => addMember("type", ty, ty.name.toString)
              }
          case t: TermSymbol =>
            if (matches(t.name.toString)) out += Hit("term", fqn(t))
          case ty: TypeSymbol =>
            if (matches(ty.name.toString)) out += Hit("type", fqn(ty))
        }
      catch { case NonFatal(_) => () } // some symbols fail to force; skip them
    }

    def inRequestedPackage(sym: Symbol): Boolean =
      inPackage.forall { pkg =>
        val fullName = fqn(sym)
        fullName == pkg || fullName.startsWith(pkg + ".")
      }

    var rootsScanned = 0
    val roots: Iterator[Symbol] =
      if searchEntries.nonEmpty then
        searchEntries.iterator
          .flatMap { entry =>
            try ctx.findSymbolsByClasspathEntry(entry).iterator
            catch { case NonFatal(_) => Iterator.empty }
          }
          .takeWhile { _ =>
            val allowed = rootsScanned < 10000 && out.size < limit && System.nanoTime() < deadlineNanos
            rootsScanned += 1
            allowed
          }
          .filter(inRequestedPackage)
      else
        (inPackage match {
          case Some(pkg) =>
            try List(ctx.findPackage(pkg))
            catch { case NonFatal(_) => Nil }
          case None => List(ctx.defn.RootPackage)
        }).iterator
    roots.foreach(visit)
    // A package the build's own entries have nothing in: a dependency's, read from the
    // whole classpath (which isn't enumerated otherwise).
    if (out.isEmpty && searchEntries.nonEmpty)
      inPackage.foreach(pkg => attempt(ctx.findPackage(pkg)).foreach(visit))
    out.toList.distinct
  }

  /**
   * A symbol by fully-qualified name, with its members as Scala signatures: a class,
   * trait or object (also one nested in an object; `Foo$` names `Foo`'s companion), with
   * its companion's members; a type member of an object (an opaque type, a type alias,
   * an abstract type), with its companion's; a term of an object (a `val`, `def` or
   * `given`); or a package (`com.example`, or `com.example.package`), with its classes
   * and its top-level definitions (those of its `package.scala`, or any file's).
   */
  def inspect(fqn: String)(using ctx: Context): Option[String] = {
    val name       = fqn.stripSuffix("$")
    val companion  = attempt(ctx.findStaticModuleClass(name)).orElse(attempt(ctx.findTopLevelModuleClass(name)))
    // findStaticType also finds classes, which findClass covers.
    val typeMember = attempt(ctx.findStaticType(fqn)).collect { case t: TypeMemberSymbol => t }
    def companionOf(c: ClassSymbol): List[String] =
      (if (c.isModuleClass) None else attempt(c.companionClass).flatten) match {
        case Some(o) => s"  companion object ${o.name.toString.stripSuffix("$")}:" +: membersOf(o).map("  " + _)
        case None    => Nil
      }
    if (fqn.endsWith("$") && companion.isDefined) companion.map(o => describeClass(o, name, Nil))
    else typeMember match {
      case Some(t) =>
        val members = companion.fold(List.empty[String])(c => s"  companion object ${c.name.toString.stripSuffix("$")}:" +: membersOf(c).map("  " + _))
        Some((s"${typeKind(t)} — $fqn" +: members).mkString("\n"))
      case None =>
        findClass(fqn).map(c => describeClass(c, fqn, companionOf(c)))
          .orElse(companion.map(c => describeClass(c, fqn, Nil)))
          .orElse(attempt(ctx.findStaticTerm(fqn)).map(t => s"${ScalaSignatures.term(t)} — $fqn"))
          .orElse(describePackage(fqn.stripSuffix(".package")))
    }
  }

  private def describeClass(c: ClassSymbol, fqn: String, companion: List[String])(using ctx: Context): String =
    ((s"${kindOf(c)} ${c.name.toString.stripSuffix("$")} — $fqn" +: membersOf(c)) ++ companion).mkString("\n")

  /** A package's classes, traits, objects and packages, then its top-level definitions. */
  private def describePackage(fqn: String)(using ctx: Context): Option[String] =
    attempt(ctx.findPackage(fqn)).map { pkg =>
      val decls = attempt(pkg.declarations).getOrElse(Nil)
      // Top-level definitions live in a synthetic object per file: `<File>$package`.
      def topLevel(c: ClassSymbol) = c.isModuleClass && { val n = c.name.toString; n.endsWith("$package$") || n == "package$" }
      val types = decls.collect {
        case c: ClassSymbol if visible(c) && (!c.isModuleClass || (!topLevel(c) && !hasClass(decls, c))) =>
          s"  ${kindOf(c)} ${c.name.toString.stripSuffix("$")}"
        case p: PackageSymbol                        => s"  package ${p.name}"
        case t: TypeMemberSymbol if visible(t)       => s"  ${typeKind(t)}"
      }
      val definitions = decls.collect { case c: ClassSymbol if topLevel(c) => c }.flatMap(membersOf)
      ((s"package $fqn" +: types.sorted) ++ definitions).mkString("\n")
    }

  /** Whether `decls` has the class (or trait) that module class `c` is the companion of. */
  private def hasClass(decls: List[Symbol], c: ClassSymbol): Boolean =
    decls.exists { case other: ClassSymbol => !other.isModuleClass && other.name.toString + "$" == c.name.toString; case _ => false }

  private def kindOf(c: ClassSymbol): String =
    if (c.isModuleClass) "object" else if (c.isTrait) "trait" else "class"

  /**
   * Public members, as Scala: types and objects first, then constructors, then the rest,
   * each by name: `object Nested`, `def this(a: Int)`, `def parse(s: String): Version`.
   */
  private def membersOf(c: ClassSymbol)(using ctx: Context): List[String] =
    try
      c.declarations.collect {
        // An object's or a trait's constructor is nothing the source writes.
        case t: TermSymbol if visible(t) && !((c.isModuleClass || c.isTrait) && t.name.toString == "<init>") =>
          val group = if (t.isModuleVal) 0 else if (t.name.toString == "<init>") 1 else 2
          (group, t.name.toString, s"  ${ScalaSignatures.term(t)}")
        case nested: ClassSymbol if visible(nested) && !nested.isModuleClass => (0, nested.name.toString, s"  ${kindOf(nested)} ${nested.name}")
        case ty: TypeMemberSymbol if visible(ty)                             => (0, ty.name.toString, s"  ${typeKind(ty)}")
      }.sortBy(m => (m._1, m._2)).map(_._3)
    catch { case NonFatal(e) => List(s"  <could not read members: ${e.getMessage}>") }

  /**
   * Public, and written in the source: not private or package-private, and not one the
   * compiler adds (a default argument's `f$default$1`, an object's `writeReplace`).
   */
  private def visible(s: TermOrTypeSymbol): Boolean =
    try {
      val name = s.name.toString
      val scopedPrivate = s.visibility match {
        case _: tastyquery.Modifiers.Visibility.ScopedPrivate => true
        case _                                                => false
      }
      !s.isPrivate && !scopedPrivate && !s.isSynthetic && !name.contains("$default$") && name != "writeReplace"
    } catch { case NonFatal(_) => true }

  /** E.g. `opaque type Version`, `type Id = Long`. */
  private def typeKind(t: TypeMemberSymbol)(using ctx: Context): String =
    try
      t.typeDef match {
        case TypeMemberDefinition.OpaqueTypeAlias(_, _) => s"opaque type ${t.name}"
        case TypeMemberDefinition.TypeAlias(alias)      => s"type ${t.name} = ${ScalaSignatures.show(alias)}"
        case TypeMemberDefinition.AbstractType(bounds)  => s"type ${t.name}${ScalaSignatures.show(bounds)}"
      }
    catch { case NonFatal(_) => s"type ${t.name}" }

  private def attempt[A](f: => A): Option[A] =
    try Some(f) catch { case NonFatal(_) => None }

  /**
   * Source location of a symbol as `path:line`, read from the symbol's defining
   * tree position in TASTy. Works for classes/objects/traits (nested in objects too),
   * type members of objects, and static/top-level terms. Returns None if the symbol
   * isn't found or its position is unknown (e.g. a Java symbol, or TASTy compiled
   * without positions).
   */
  def location(fqn: String)(using ctx: Context): Option[String] = {
    val sym: Option[Symbol] =
      findClass(fqn)
        // A type member before its companion object: `Api.Version` names the type.
        .orElse(attempt(ctx.findStaticType(fqn)))
        .orElse(attempt(ctx.findStaticModuleClass(fqn)))
        .orElse(attempt(ctx.findStaticTerm(fqn)))
    sym.flatMap { s =>
      s.tree.flatMap { t =>
        val pos = t.pos
        if (pos.isUnknown) None
        else Some(s"${pos.sourceFile.path}:${pos.startLine + 1}")
      }
    }
  }

  private def findClass(fqn: String)(using ctx: Context): Option[ClassSymbol] =
    attempt(ctx.findTopLevelClass(fqn))
      .orElse(attempt(ctx.findTopLevelModuleClass(fqn)))
      .orElse(attempt(ctx.findStaticClass(fqn)))

}
