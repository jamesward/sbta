package com.jamesward.sbta

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path, StandardCopyOption }
import scala.util.Using

/**
 * Everything but `--mcp` is sbt's own launcher (`xsbt.boot.Boot`), run in this JVM
 * with the same arguments, exactly as `java -jar sbt-launch.jar <args>` would: the
 * shell with no arguments, batch commands, `--client`, `-bsp`, `--rt-ext-dir`…
 * The sbt scripts' JVM options (`.jvmopts`, `.sbtopts`, `-mem`) therefore apply to sbt.
 *
 * The launcher jar is embedded and placed where the sbt script keeps it
 * (`~/.cache/sbt/boot/sbt-launch/<version>/sbt-launch-<version>.jar`), so a cached
 * copy is shared with sbt and its path stays stable for anything that records it.
 */
object Launcher:

  val version: String =
    Using.resource(resource("/sbta-launcher/version"))(in => String(in.readAllBytes(), StandardCharsets.UTF_8).trim)

  /** Run sbt's launcher with `args`; it exits the JVM when sbt is done. */
  def run(args: Array[String]): Unit =
    val jar    = launcherJar()
    val loader = URLClassLoader(Array(jar.toUri.toURL), ClassLoader.getPlatformClassLoader)
    val boot   = Class.forName("xsbt.boot.Boot", true, loader)
    // sbt finds its launcher on `java.class.path` (e.g. for the `.bsp/sbt.json` fallback, and
    // `--sbt-launch-jar` when a client starts a server): point it at the launcher, not sbta.
    System.setProperty("java.class.path", jar.toString)
    Thread.currentThread().setContextClassLoader(loader)
    try boot.getMethod("main", classOf[Array[String]]).invoke(null, args)
    catch case e: java.lang.reflect.InvocationTargetException => throw e.getCause

  /** The launcher jar in sbt's boot cache, extracted from sbta when it isn't there yet. */
  def launcherJar(): Path =
    val home   = Path.of(System.getProperty("user.home"))
    val target = home.resolve(".cache").resolve("sbt").resolve("boot").resolve("sbt-launch").resolve(version)
      .resolve(s"sbt-launch-$version.jar")
    if !Files.isRegularFile(target) || Files.size(target) == 0 then
      Files.createDirectories(target.getParent)
      val temp = Files.createTempFile(target.getParent, "sbt-launch", ".tmp")
      try
        Using.resource(resource("/sbta-launcher/sbt-launch.jar"))(in => Files.copy(in, temp, StandardCopyOption.REPLACE_EXISTING))
        try Files.move(temp, target, StandardCopyOption.ATOMIC_MOVE)
        catch case _: java.nio.file.FileAlreadyExistsException => () // another sbta got there first
      finally Files.deleteIfExists(temp)
    target

  private def resource(name: String): java.io.InputStream =
    Option(getClass.getResourceAsStream(name)).getOrElse(throw IllegalStateException(s"sbta is missing $name"))
