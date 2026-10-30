package com.jamesward.sbta

import org.objectweb.asm.{ ClassReader, ClassVisitor, FieldVisitor, MethodVisitor, Opcodes, Type }
import zio.*
import zio.test.*

import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path }
import java.util.zip.ZipFile
import scala.jdk.CollectionConverters.*

/**
 * ProGuard removed nothing that sbta's jar reaches by reflection.
 *
 * Every reflective call site in the shrunk jar is found in the bytecode. When its
 * class and member names are constants (`Foo.class.getDeclaredField("bar")`,
 * `lookup.findVarHandle(Foo.class, "bar", …)`, `newUpdater(Foo.class, "bar")`,
 * `Class.forName("a.Foo")`), the target is checked directly: it must still be in the
 * shrunk jar if it was in the assembly. With only the member name constant, every
 * class that had a member of that name must still have it. A call site with a
 * computed name can't be checked, so it must be accounted for in `sbta/reflection.txt`
 * (its targets are the JDK's, a jar sbta loads in its own classloader, or kept by a
 * rule in `proguard.pro`); an entry that matches no call site fails too.
 */
object ReflectionSpec extends ZIOSpecDefault:

  enum Kind derives CanEqual:
    case ClassByName, Field, Method, Members, Other

  /** Reflective entry points and what they look up. */
  private val Reflective: Map[(String, String), Kind] = Map(
    ("java/lang/Class", "forName")                                   -> Kind.ClassByName,
    ("java/lang/ClassLoader", "loadClass")                           -> Kind.ClassByName,
    ("java/lang/invoke/MethodHandles$Lookup", "findClass")           -> Kind.ClassByName,
    ("java/lang/Class", "getField")                                  -> Kind.Field,
    ("java/lang/Class", "getDeclaredField")                          -> Kind.Field,
    ("java/lang/invoke/MethodHandles$Lookup", "findGetter")          -> Kind.Field,
    ("java/lang/invoke/MethodHandles$Lookup", "findSetter")          -> Kind.Field,
    ("java/lang/invoke/MethodHandles$Lookup", "findStaticGetter")    -> Kind.Field,
    ("java/lang/invoke/MethodHandles$Lookup", "findStaticSetter")    -> Kind.Field,
    ("java/lang/invoke/MethodHandles$Lookup", "findVarHandle")       -> Kind.Field,
    ("java/lang/invoke/MethodHandles$Lookup", "findStaticVarHandle") -> Kind.Field,
    ("java/util/concurrent/atomic/AtomicIntegerFieldUpdater", "newUpdater")   -> Kind.Field,
    ("java/util/concurrent/atomic/AtomicLongFieldUpdater", "newUpdater")      -> Kind.Field,
    ("java/util/concurrent/atomic/AtomicReferenceFieldUpdater", "newUpdater") -> Kind.Field,
    // Netty's own wrapper around findVarHandle (for Java 8 bytecode).
    ("io/netty/util/internal/PlatformDependent", "findVarHandleOfIntField") -> Kind.Field,
    ("java/lang/Class", "getMethod")                                 -> Kind.Method,
    ("java/lang/Class", "getDeclaredMethod")                         -> Kind.Method,
    ("java/lang/invoke/MethodHandles$Lookup", "findVirtual")         -> Kind.Method,
    ("java/lang/invoke/MethodHandles$Lookup", "findStatic")          -> Kind.Method,
    ("java/lang/invoke/MethodHandles$Lookup", "findSpecial")         -> Kind.Method,
    ("java/lang/Class", "getConstructor")                            -> Kind.Members,
    ("java/lang/Class", "getDeclaredConstructor")                    -> Kind.Members,
    ("java/lang/invoke/MethodHandles$Lookup", "findConstructor")     -> Kind.Members,
    ("java/lang/Class", "getMethods")                                -> Kind.Members,
    ("java/lang/Class", "getDeclaredMethods")                        -> Kind.Members,
    ("java/lang/Class", "getFields")                                 -> Kind.Members,
    ("java/lang/Class", "getDeclaredFields")                         -> Kind.Members,
    ("java/lang/Class", "getDeclaredConstructors")                   -> Kind.Members,
    ("java/lang/Class", "newInstance")                               -> Kind.Members,
    ("java/util/ServiceLoader", "load")                              -> Kind.Other,
    ("java/util/ServiceLoader", "loadInstalled")                     -> Kind.Other,
    ("java/lang/reflect/Proxy", "newProxyInstance")                  -> Kind.Other,
  )

  final case class Site(caller: String, api: String, kind: Kind, target: Option[String], name: Option[String])

  /** A jar's classes: name → (fields, methods). */
  final case class Classes(members: Map[String, (Set[String], Set[String])]):
    def has(cls: String): Boolean = members.contains(cls)
    def field(cls: String, f: String): Boolean  = members.get(cls).exists(_._1(f))
    def method(cls: String, m: String): Boolean = members.get(cls).exists(_._2(m))

  private def read(jar: Path)(f: (String, Array[Byte]) => Unit): Unit =
    val zip = ZipFile(jar.toFile)
    try
      zip.entries.asScala
        .filter(e => e.getName.endsWith(".class") && !e.getName.startsWith("META-INF/") && e.getName != "module-info.class")
        .foreach(e => f(e.getName.stripSuffix(".class"), zip.getInputStream(e).readAllBytes()))
    finally zip.close()

  def classes(jar: Path): Classes =
    val all = Map.newBuilder[String, (Set[String], Set[String])]
    read(jar) { (_, bytes) =>
      val fields  = Set.newBuilder[String]
      val methods = Set.newBuilder[String]
      var name    = ""
      ClassReader(bytes).accept(
        new ClassVisitor(Opcodes.ASM9):
          override def visit(v: Int, a: Int, n: String, s: String, sup: String, i: Array[String]): Unit = name = n
          override def visitField(a: Int, n: String, d: String, s: String, v: Any): FieldVisitor =
            fields += n
            null
          override def visitMethod(a: Int, n: String, d: String, s: String, e: Array[String]): MethodVisitor =
            methods += n
            null
        ,
        ClassReader.SKIP_CODE,
      )
      all += name -> (fields.result(), methods.result())
    }
    Classes(all.result())

  /** Reflective call sites, with the constant class and member name just before each, if any. */
  def sites(jar: Path): List[Site] =
    val found = List.newBuilder[Site]
    read(jar) { (_, bytes) =>
      ClassReader(bytes).accept(
        new ClassVisitor(Opcodes.ASM9):
          private var caller = ""
          override def visit(v: Int, a: Int, n: String, s: String, sup: String, i: Array[String]): Unit = caller = n
          override def visitMethod(a: Int, n: String, d: String, s: String, e: Array[String]): MethodVisitor =
            new MethodVisitor(Opcodes.ASM9):
              // The constants pushed since the last call, in order (a call's arguments are
              // pushed just before it): the member name is the last String, and its class the
              // last class constant before that name (a field's type may follow it).
              private val pushed = scala.collection.mutable.ArrayBuffer[Either[String, String]]() // Left: class, Right: string
              override def visitLdcInsn(value: Any): Unit = value match
                case s: String                           => pushed += Right(s)
                case t: Type if t.getSort == Type.OBJECT => pushed += Left(t.getInternalName)
                case _                                   => ()
              override def visitMethodInsn(o: Int, owner: String, method: String, desc: String, itf: Boolean): Unit =
                Reflective.get((owner, method)).foreach { kind =>
                  val nameAt = pushed.lastIndexWhere(_.isRight)
                  val string = Option.when(nameAt >= 0)(pushed(nameAt)).collect { case Right(s) => s }
                  val classBeforeName =
                    pushed.take(if nameAt >= 0 then nameAt else pushed.length).reverseIterator.collectFirst { case Left(c) => c }
                  val target = kind match
                    case Kind.ClassByName => string.map(_.replace('.', '/'))
                    case _                => classBeforeName
                  val name = kind match
                    case Kind.Field | Kind.Method => string
                    case _                        => None
                  found += Site(caller, s"${owner.replace('/', '.')}.$method", kind, target, name)
                }
                // `MethodType.methodType(…)`, boxing and `getClassLoader` between the constants and
                // the lookup build its arguments; any other call starts a new expression.
                if !(owner == "java/lang/invoke/MethodType" || method == "valueOf" || method == "lookup" || method == "getClassLoader") then
                  pushed.clear()
        ,
        ClassReader.SKIP_FRAMES,
      )
    }
    found.result()

  /** What a site can't be checked for, or None when its target is known and present. */
  def problem(site: Site, before: Classes, after: Classes): Option[String] =
    import Kind.*
    (site.kind, site.target, site.name) match
      case (ClassByName, Some(cls), _) =>
        Option.when(before.has(cls) && !after.has(cls))(s"removed class $cls")
      case (Field, Some(cls), Some(f)) =>
        Option.when(before.field(cls, f) && !after.field(cls, f))(s"removed field $cls.$f")
      case (Method, Some(cls), Some(m)) =>
        Option.when(before.method(cls, m) && !after.method(cls, m))(s"removed method $cls.$m")
      // Only the member name is constant: a lookup in the caller's own class is checkable;
      // any other class must be accounted for.
      case (Field | Method, None, Some(member)) =>
        val own  = Set(site.caller)
        def had(c: String, b: Classes) = if site.kind == Field then b.field(c, member) else b.method(c, member)
        own.find(c => had(c, before) && !had(c, after)).map(c => s"removed ${site.kind.toString.toLowerCase} $c.$member")
          .orElse(Option.when(!own.exists(had(_, before)))("unchecked"))
      case (Members, Some(cls), _) if !before.has(cls) => None // a JDK class
      case _ => Some("unchecked")

  /** `sbta/reflection.txt`: `<caller class, or package prefix ending in />  <api or *>  # why`. */
  final case class Allowed(caller: String, api: String, reason: String):
    def matches(site: Site): Boolean =
      (if caller.endsWith("/") then site.caller.startsWith(caller) else site.caller == caller) &&
        (api == "*" || site.api == api)

  def allowed(file: Path): List[Allowed] =
    Files.readAllLines(file, StandardCharsets.UTF_8).asScala.toList.map(_.trim).filter(l => l.nonEmpty && !l.startsWith("#")).map { line =>
      val (rule, reason) = line.span(_ != '#')
      rule.trim.split("\\s+").toList match
        case List(caller, api) => Allowed(caller, api, reason.drop(1).trim)
        case _                 => throw IllegalArgumentException(s"bad line in reflection.txt: $line")
    }

  /** `META-INF/services` providers: (service file, provider class). */
  def services(jar: Path): List[(String, String)] =
    val zip = ZipFile(jar.toFile)
    try
      zip.entries.asScala.toList.filter(e => e.getName.startsWith("META-INF/services/") && !e.isDirectory).flatMap { e =>
        String(zip.getInputStream(e).readAllBytes(), StandardCharsets.UTF_8).linesIterator
          .map(_.takeWhile(_ != '#').trim).filter(_.nonEmpty).map(e.getName -> _.replace('.', '/')).toList
      }
    finally zip.close()

  def spec = suite("sbta's shrunk jar")(
    test("every ServiceLoader provider the shrunk jar lists is in it") {
      val shrunk  = Path.of(sys.props("sbta.jar"))
      val before  = classes(Path.of(sys.props("sbta.assembly")))
      val after   = classes(shrunk)
      val missing = services(shrunk).filter((_, cls) => before.has(cls) && !after.has(cls))
      assertTrue(missing.isEmpty)
    },
    test("the check reports a member that shrinking removed, and accepts JDK targets") {
      val before = Classes(Map("a/Foo" -> (Set("count"), Set("run"))))
      val after  = Classes(Map("a/Foo" -> (Set.empty, Set("run"))))
      def site(kind: Kind, target: Option[String], name: Option[String]) = Site("a/Foo", "api", kind, target, name)
      assertTrue(
        problem(site(Kind.Field, Some("a/Foo"), Some("count")), before, after).contains("removed field a/Foo.count"),
        problem(site(Kind.Field, None, Some("count")), before, after).contains("removed field a/Foo.count"),
        problem(site(Kind.Method, Some("a/Foo"), Some("run")), before, after).isEmpty,
        problem(site(Kind.ClassByName, Some("a/Gone"), None), Classes(Map("a/Gone" -> (Set.empty, Set.empty))), after)
          .contains("removed class a/Gone"),
        problem(site(Kind.ClassByName, Some("java/lang/ClassValue"), None), before, after).isEmpty,
        problem(site(Kind.Method, None, None), before, after).contains("unchecked"),
      )
    },
    test("ProGuard removed nothing reached by reflection, and every unchecked reflective call is accounted for") {
      val shrunk   = Path.of(sys.props("sbta.jar"))
      val before   = classes(Path.of(sys.props("sbta.assembly")))
      val after    = classes(shrunk)
      val rules    = allowed(Path.of(sys.props("sbta.reflection")))
      val all      = sites(shrunk)
      val problems = all.flatMap(site => problem(site, before, after).map(site -> _))
      val removed  = problems.filter(_._2 != "unchecked")
      val unknown  = problems.collect { case (site, "unchecked") if !rules.exists(_.matches(site)) => site }.distinct
      val unused   = rules.filterNot(rule => problems.exists((site, _) => rule.matches(site)))
      removed.foreach((site, p) => println(s"ProGuard broke reflection: ${site.caller} ${site.api}: $p"))
      unknown.sortBy(_.caller).foreach(site => println(s"unchecked reflection: ${site.caller}  ${site.api}"))
      unused.foreach(rule => println(s"reflection.txt entry matching no unchecked call site: ${rule.caller}  ${rule.api}"))
      assertTrue(all.nonEmpty, rules.forall(_.reason.nonEmpty), removed.isEmpty, unknown.isEmpty, unused.isEmpty)
    },
  )
