package com.jamesward.sbta

import java.lang.reflect.{ InvocationTargetException, Method }
import java.net.URLClassLoader
import java.nio.file.{ Files, Path, StandardCopyOption }

import scala.jdk.CollectionConverters.*
import scala.util.Using

/**
 * A warm, classloader-isolated Scala 3 compiler used by the `check` tool.
 *
 * The loader contains the embedded check bridge plus the TARGET project's own
 * compiler jars (its `scalaInstance`), parented to the JDK platform loader so
 * sbt's Scala 3.8 runtime and compiler never leak in. The project classpath is
 * passed to the compiler as `-classpath`, exactly like a batch compile, so macro
 * implementations load through the compiler's own macro classloader.
 */
private[sbta] object IsolatedCheck:

  /**
   * A compiler diagnostic, positioned exactly as the Scala 3 compiler prints it
   * (and so as `sbt compile` shows it): `line` is 1-based, `column` is the 0-based
   * offset within the line. `line` is 0 when the diagnostic has no position.
   */
  final case class Diagnostic(
      severity: String,
      errorNumber: Option[Int],
      path: Option[Path],
      line: Int,
      column: Int,
      lineContent: String,
      message: String,
  ):
    def isError: Boolean   = severity == "error"
    def isWarning: Boolean = severity == "warning"

  /**
   * A classloader holding the bridge and one compiler version. It is kept across
   * checks (and classpath changes), so the compiler stays JIT-warm; each check builds
   * a fresh compiler context from the classpath/options it is given.
   */
  final class Session private[IsolatedCheck] (
      loader: URLClassLoader,
      bridge: Object,
      checkMethod: Method,
      outputDirectory: Path,
      val compilerVersion: String,
  ) extends AutoCloseable:

    private[sbta] val isIsolated: Boolean =
      (bridge.getClass.getClassLoader eq loader) &&
        (loader.getParent eq ClassLoader.getPlatformClassLoader)

    /**
     * Check `sources` (path -> content) together in one compiler run against
     * `classpath`, with the project's (sanitized) `options`. Like parallel sbt
     * compiles sharing one compiler loader, concurrent checks are independent runs.
     */
    def check(classpath: Seq[Path], options: Seq[String], sources: Seq[(Path, String)]): List[Diagnostic] =
      val args = (options ++ Seq(
        "-classpath",
        classpath.map(_.toString).mkString(java.io.File.pathSeparator),
        "-d",
        outputDirectory.toString,
        s"-Ystop-before:$stopBefore",
      )).toArray
      val paths    = sources.map(_._1.toAbsolutePath.normalize.toString).toArray
      val contents = sources.map(_._2).toArray
      val encoded = withContextLoader(loader) {
        try checkMethod.invoke(bridge, args, paths, contents).asInstanceOf[java.util.List[String]]
        catch case error: InvocationTargetException => throw error.getCause
      }
      encoded.asScala.toList.flatMap(decode)

    override def close(): Unit =
      try loader.close()
      finally deleteRecursively(outputDirectory)
  end Session

  /** Load the bridge with a project's compiler jars (its `scalaInstance.allJars`). */
  def open(compilerJars: Seq[Path]): Session =
    val urls   = (bridgeJar +: compilerJars).map(_.toUri.toURL).distinct.toArray
    val loader = URLClassLoader(urls, ClassLoader.getPlatformClassLoader)
    // Nothing is written before bytecode generation; this only satisfies `-d`.
    val output = Files.createTempDirectory("sbta-check-out-")
    try
      withContextLoader(loader) {
        val bridgeClass = Class.forName("com.jamesward.sbta.IsolatedCheckBridge$", true, loader)
        val bridge      = bridgeClass.getField("MODULE$").get(null)
        val methods     = bridgeClass.getMethods.iterator.map(method => method.getName -> method).toMap
        val version     = methods("compilerVersion").invoke(bridge).asInstanceOf[String]
        new Session(loader, bridge, methods("check"), output, version)
      }
    catch
      case error: Throwable =>
        loader.close()
        deleteRecursively(output)
        throw error
  end open

  /**
   * The first phase NOT run: everything a compile reports except bytecode generation.
   * Overridable with `-Dsbta.check.stopBefore=<phase>` (e.g. `posttyper` for a
   * typer-only check).
   */
  private[sbta] def stopBefore: String =
    sys.props.get("sbta.check.stopBefore").filter(_.trim.nonEmpty).getOrElse("genBCode")

  /**
   * Drop options that would make a check write outside its scratch directory,
   * rewrite sources, or conflict with the options [[Session.check]] adds.
   */
  def sanitizeOptions(options: Seq[String]): List[String] =
    val withArgument = Set(
      "-d", "-classpath", "-cp", "--class-path", "-semanticdb-target", "-coverage-out",
      "-Ypickle-write", "-Yearly-tasty-output", "-Ystop-before", "-Ystop-after",
    )
    val dropped = Set("-rewrite", "-Xsemanticdb", "-Ysemanticdb", "-Ybest-effort", "-Ywith-best-effort-tasty")
    val result = List.newBuilder[String]
    val it     = options.iterator
    while it.hasNext do
      val option = it.next()
      val name   = option.takeWhile(_ != ':')
      if withArgument(option) then { if it.hasNext then it.next(); () }
      else if withArgument(name) || dropped(name) then ()
      else result += option
    result.result()

  private def decode(value: String): Option[Diagnostic] =
    val fields = value.split("\u0000", -1)
    if fields.length < 7 then None
    else
      val severity = fields(0).toIntOption match
        case Some(2) => "error"
        case Some(1) => "warning"
        case _       => "info"
      val path = Option(fields(2)).filter(_.nonEmpty).map(java.nio.file.Paths.get(_))
      Some(
        Diagnostic(
          severity,
          fields(1).toIntOption.filter(_ >= 0),
          path,
          fields(3).toIntOption.map(_ + 1).getOrElse(0),
          fields(4).toIntOption.getOrElse(0),
          fields(5),
          fields.drop(6).mkString("\u0000"),
        )
      )

  private var extractedBridge: Option[Path] = None

  private def bridgeJar: Path = synchronized {
    extractedBridge.filter(Files.exists(_)).getOrElse {
      val resource = "/sbta-check/check-runtime.jar"
      val stream = Option(getClass.getResourceAsStream(resource)).getOrElse {
        throw IllegalStateException(s"embedded check runtime is missing: $resource")
      }
      val directory = Files.createTempDirectory("sbta-check-")
      val extracted = directory.resolve("check-runtime.jar")
      Using.resource(stream)(input => Files.copy(input, extracted, StandardCopyOption.REPLACE_EXISTING))
      directory.toFile.deleteOnExit()
      extracted.toFile.deleteOnExit()
      extractedBridge = Some(extracted)
      extracted
    }
  }

  /** Called after all sessions close on sbt unload; a later use re-extracts. */
  def shutdownResources(): Unit = synchronized {
    extractedBridge.foreach { jar =>
      Files.deleteIfExists(jar)
      Option(jar.getParent).foreach(Files.deleteIfExists)
    }
    extractedBridge = None
  }

  private def deleteRecursively(directory: Path): Unit =
    if Files.exists(directory) then
      Using.resource(Files.walk(directory)) { paths =>
        paths.sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists)
      }

  private def withContextLoader[A](loader: ClassLoader)(operation: => A): A =
    val thread   = Thread.currentThread()
    val previous = thread.getContextClassLoader
    thread.setContextClassLoader(loader)
    try operation
    finally thread.setContextClassLoader(previous)
end IsolatedCheck
