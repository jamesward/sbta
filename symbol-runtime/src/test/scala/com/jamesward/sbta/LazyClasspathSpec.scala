package com.jamesward.sbta

import java.nio.file.{ Files, Path }
import java.util.Comparator
import java.util.jar.{ JarEntry, JarOutputStream }

import scala.util.Using

import zio.*
import zio.test.*

object LazyClasspathSpec extends ZIOSpecDefault:

  private def withTempDirectory[A](operation: Path => A): Task[A] =
    ZIO.scoped {
      ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("sbta-lazy-classpath-")))(deleteRecursively)
        .map(operation)
    }

  private def deleteRecursively(root: Path): UIO[Unit] =
    ZIO
      .attemptBlocking {
        if Files.exists(root) then
          Using.resource(Files.walk(root)) { paths =>
            import scala.language.unsafeNulls
            paths.sorted(Comparator.reverseOrder()).forEach(path => Files.deleteIfExists(path))
          }
      }
      .orDie

  private def writeJar(path: Path, classBytes: Array[Byte], tastyBytes: Array[Byte]): Unit =
    Using.resource(JarOutputStream(Files.newOutputStream(path))) { jar =>
      def write(name: String, bytes: Array[Byte]): Unit =
        jar.putNextEntry(JarEntry(name))
        jar.write(bytes)
        jar.closeEntry()

      write("example/Widget.class", classBytes)
      write("example/Widget.tasty", tastyBytes)
    }

  private def indexedWidget(path: Path) =
    val entry = LazyClasspath.read(path :: Nil).head
    val pkg = entry
      .listAllPackages()
      .find(_.dotSeparatedName == "example")
      .getOrElse(throw AssertionError("example package was not indexed"))
    pkg
      .getClassDataByBinaryName("Widget")
      .getOrElse(throw AssertionError("Widget class data was not indexed"))

  def spec = suite("LazyClasspathSpec")(
    test("directory indexing retains metadata and reads changed payloads on demand") {
      withTempDirectory { root =>
        val packageDir = Files.createDirectories(root.resolve("example"))
        val classFile  = packageDir.resolve("Widget.class")
        val tastyFile  = packageDir.resolve("Widget.tasty")
        Files.write(classFile, Array[Byte](1, 2, 3))
        Files.write(tastyFile, Array[Byte](4, 5, 6))

        val data = indexedWidget(root)
        Files.write(classFile, Array[Byte](7, 8))
        Files.write(tastyFile, Array[Byte](9, 10))

        val classBytes = data.readClassFileBytes().toSeq
        val tastyBytes = data.readTastyFileBytes().toSeq
        assertTrue(
          data.hasClassFile,
          data.hasTastyFile,
          classBytes == Seq[Byte](7, 8),
          tastyBytes == Seq[Byte](9, 10),
        )
      }
    },
    test("jar indexing closes the jar and reads changed payloads on demand") {
      withTempDirectory { root =>
        val jar = root.resolve("classpath.jar")
        writeJar(jar, Array[Byte](1, 2, 3), Array[Byte](4, 5, 6))

        val data = indexedWidget(jar)
        writeJar(jar, Array[Byte](11, 12), Array[Byte](13, 14))

        val classBytes = data.readClassFileBytes().toSeq
        val tastyBytes = data.readTastyFileBytes().toSeq
        assertTrue(
          data.hasClassFile,
          data.hasTastyFile,
          classBytes == Seq[Byte](11, 12),
          tastyBytes == Seq[Byte](13, 14),
        )
      }
    },
  )
end LazyClasspathSpec
