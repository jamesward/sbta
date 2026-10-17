import sbt.*

import java.net.URI
import java.nio.file.{ FileSystems, Files, Path }
import java.util.zip.{ ZipEntry, ZipOutputStream }
import scala.jdk.CollectionConverters.*

/**
 * Shrinks sbta's assembly with ProGuard: unused classes are removed, nothing is
 * renamed or optimized (`-dontobfuscate`, `-dontoptimize`), so stack traces and
 * reflective names stay as they are. What must survive although nothing references
 * it statically is listed in `sbta/proguard.pro`; `ReflectionSpec` keeps that list
 * honest by failing on any reflective call site it doesn't know about.
 */
object SbtaShrink {

  /** JDK modules the assembly links against (ProGuard needs them as library classes). */
  private val JdkModules = Seq(
    "java.base", "java.logging", "java.management", "java.naming", "java.net.http", "java.sql", "java.xml",
    "java.desktop", "java.instrument", "java.compiler", "jdk.unsupported", "jdk.management", "jdk.net",
    "jdk.crypto.ec", "java.security.jgss", "java.scripting", "java.rmi",
  )

  /** The running JDK's classes for `modules`, as one jar (from `jrt:/`, so it works on JDKs without jmods). */
  def jdkJar(out: File): File = {
    if (!out.isFile) {
      IO.createDirectory(out.getParentFile)
      val jrt  = FileSystems.getFileSystem(URI.create("jrt:/"))
      val temp = new File(out.getPath + ".tmp")
      val zip  = new ZipOutputStream(Files.newOutputStream(temp.toPath))
      try
        JdkModules.foreach { module =>
          val root = jrt.getPath("modules", module)
          if (Files.isDirectory(root))
            Files.walk(root).iterator.asScala.filter(p => Files.isRegularFile(p) && p.toString.endsWith(".class"))
              .filterNot(_.getFileName.toString == "module-info.class")
              .foreach { p =>
                zip.putNextEntry(new ZipEntry(root.relativize(p).toString))
                zip.write(Files.readAllBytes(p))
                zip.closeEntry()
              }
        }
      finally zip.close()
      IO.move(temp, out)
    }
    out
  }

  /** Run ProGuard (`proguardClasspath`) on `in`, writing `out`, with `config` plus the in/out/library jars. */
  def run(proguardClasspath: Seq[File], config: File, in: File, out: File, jdk: File, javaHome: File, log: Logger): File =
    if (out.isFile && out.lastModified > in.lastModified && out.lastModified > config.lastModified) out
    else shrink(proguardClasspath, config, in, out, jdk, javaHome, log)

  private def shrink(proguardClasspath: Seq[File], config: File, in: File, out: File, jdk: File, javaHome: File, log: Logger): File = {
    IO.delete(out)
    val args = new File(out.getPath + ".pro")
    IO.write(
      args,
      Seq(
        s"-injars '${in.getAbsolutePath}'",
        s"-outjars '${out.getAbsolutePath}'",
        s"-libraryjars '${jdk.getAbsolutePath}'",
        s"-include '${config.getAbsolutePath}'",
        s"-printusage '${out.getPath}.usage.txt'",
      ).mkString("\n") + "\n",
    )
    val javaCmd = new File(javaHome, "bin/java").getAbsolutePath
    val command = Seq(javaCmd, "-cp", proguardClasspath.map(_.getAbsolutePath).mkString(java.io.File.pathSeparator), "proguard.ProGuard", s"@${args.getAbsolutePath}")
    val output  = new StringBuilder
    val code    = scala.sys.process.Process(command).!(scala.sys.process.ProcessLogger(l => output.append(l).append('\n'), l => output.append(l).append('\n')))
    if (code != 0 || !out.isFile) {
      log.error(output.toString)
      sys.error(s"ProGuard failed ($code)")
    }
    log.info(f"sbta: shrunk ${in.length / 1e6}%.1f MB to ${out.length / 1e6}%.1f MB")
    out
  }
}
