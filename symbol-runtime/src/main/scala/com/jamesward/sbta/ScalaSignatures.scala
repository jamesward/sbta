package com.jamesward.sbta

import scala.util.control.NonFatal

import tastyquery.Contexts.Context
import tastyquery.Symbols.*
import tastyquery.Types.*

/**
 * Members and types as Scala source writes them, for `inspect`: `def parse(s: String): Version`,
 * `extension (v: Version) def value: String`, `val codecs: List[DbCodec[?]]`, `A => B`. Types
 * are named without their packages (and without `Predef.` and `this.` prefixes), so they read
 * as the source does; `inspect` a type itself for where it comes from.
 *
 * Only tasty-query's public types are used, as in the 1.8 API this is compiled against and the
 * 1.9 and 1.10 readers it runs with; a type it doesn't know is written by tasty-query.
 */
object ScalaSignatures {

  /** A term member: `def`, `val`, `given`, `object` or `extension … def`. */
  def term(t: TermSymbol)(using ctx: Context): String = {
    val name = t.name.toString
    if (t.isModuleVal) s"object $name"
    // A constructor: `def this(a: Int)`, without its class's type parameters or `Unit` result.
    else if (name == "<init>")
      val tpe = safely(t.declaredType).map {
        case p: PolyType => p.resultType
        case other       => other
      }
      s"def this${tpe.fold("")(show(_).stripSuffix(": Unit"))}"
    else
      safely(t.declaredType) match {
        case None => s"${keyword(t)} $name"
        case Some(tpe) =>
          if (t.isExtensionMethod) extension(name, tpe).getOrElse(s"${keyword(t)} $name${typed(tpe)}")
          else s"${keyword(t)} $name${typed(tpe)}"
      }
  }

  /** A type, as Scala writes it. */
  def show(tpe: TypeMappable): String =
    try render(tpe)
    catch { case NonFatal(_) => safely(tpe.showBasic).getOrElse(tpe.toString) }

  private def keyword(t: TermSymbol): String =
    if (t.isGivenOrUsing) "given" else if (t.isMethod) "def" else "val"

  /** `(a: Int): R` for a method's type, `: T` for a value's. */
  private def typed(tpe: TypeMappable): String = tpe match {
    case _: MethodType | _: PolyType => show(tpe)
    case other                       => s": ${show(other)}"
  }

  /** `extension [A](x: A) def name(…): R`: the receiver is the first parameter clause. */
  private def extension(name: String, tpe: TypeMappable): Option[String] = tpe match {
    case p: PolyType =>
      p.resultType match {
        case m: MethodType => Some(s"extension [${typeParams(p)}](${params(m)}) def $name${typed(m.resultType)}")
        case _             => None
      }
    case m: MethodType => Some(s"extension (${params(m)}) def $name${typed(m.resultType)}")
    case _             => None
  }

  private def params(m: MethodType): String =
    (if (m.isContextual) "using " else if (m.isImplicit) "implicit " else "") +
      m.paramNames.zip(m.paramTypes).map((n, t) => s"$n: ${show(t)}").mkString(", ")

  private def typeParams(p: PolyType): String =
    p.paramNames.zip(p.paramTypeBounds).map((n, b) => s"$n${show(b)}").mkString(", ")

  private def render(tpe: TypeMappable): String = tpe match {
    case m: MethodType => s"(${params(m)})${typed(m.resultType)}"
    case p: PolyType   => s"[${typeParams(p)}]${typed(p.resultType)}"
    case t: TypeRef    => prefix(t.prefix) + t.name.toString
    case t: TermRef    => prefix(t.prefix) + t.name.toString + ".type"
    case a: AppliedType =>
      scalaName(a.tycon) match {
        case Some(n) if isFunction(n, a.args.size) =>
          val arrow  = if (n.startsWith("Context")) " ?=> " else " => "
          val params = a.args.init match {
            case List(single) if !isTuple(single) => show(single)
            case ps                               => ps.map(show).mkString("(", ", ", ")")
          }
          params + arrow + show(a.args.last)
        case Some(n) if isTupleName(n, a.args.size) => a.args.map(show).mkString("(", ", ", ")")
        case _                                      => show(a.tycon) + a.args.map(show).mkString("[", ", ", "]")
      }
    case t: TypeParamRef   => t.paramName.toString
    case b: ByNameType     => s"=> ${show(b.resultType)}"
    case r: RepeatedType   => s"${show(r.elemType)}*"
    case a: AndType        => s"${show(a.first)} & ${show(a.second)}"
    case o: OrType         => s"${show(o.first)} | ${show(o.second)}"
    case a: AnnotatedType  => show(a.typ)
    case w: WildcardTypeArg => "?" + show(w.bounds)
    case TypeAlias(alias)  => s" = ${show(alias)}"
    case AbstractTypeBounds(low, high) =>
      (if (isScala(low, "Nothing")) "" else s" >: ${show(low)}") + (if (isScala(high, "Any")) "" else s" <: ${show(high)}")
    case _: NothingType => "Nothing"
    case other          => other.showBasic
  }

  /**
   * `Outer.` for a type in an object; nothing for a package's, `this.`'s, `Predef`'s, a
   * package object's (`scala.package.List` reads `List`), a file's top-level definitions'
   * (`IArray$package.IArray` reads `IArray`), or a package's written as a term (`java.sql`,
   * lower-case as packages are, unlike objects).
   */
  private def prefix(p: Prefix): String = p match {
    case t: TermRef =>
      val name = t.name.toString
      val hidden = (name == "Predef" && inScala(t.prefix)) || name == "package" || name.endsWith("$package") ||
        name.headOption.exists(_.isLower)
      if (hidden) "" else prefix(t.prefix) + name + "."
    case _ => ""
  }

  private def scalaName(tpe: Type): Option[String] = tpe match {
    case t: TypeRef if inScala(t.prefix) => Some(t.name.toString)
    case _                               => None
  }

  private def isScala(tpe: Type, name: String): Boolean = tpe match {
    case _: NothingType => name == "Nothing"
    case _              => scalaName(tpe).contains(name)
  }

  private def isFunction(name: String, args: Int): Boolean =
    Seq("Function", "ContextFunction").exists(f => name.startsWith(f) && name.stripPrefix(f).toIntOption.contains(args - 1))

  private def isTupleName(name: String, args: Int): Boolean =
    name.startsWith("Tuple") && name.stripPrefix("Tuple").toIntOption.contains(args)

  private def isTuple(tpe: TypeOrWildcard): Boolean = tpe match {
    case a: AppliedType => scalaName(a.tycon).exists(isTupleName(_, a.args.size))
    case _              => false
  }

  private def inScala(p: Prefix): Boolean = p match {
    case pkg: PackageRef => pkg.fullyQualifiedName.toString == "scala"
    case _               => false
  }

  private def safely[A](f: => A): Option[A] =
    try Some(f) catch { case NonFatal(_) => None }
}
