package com.jamesward.sbta

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{ FileVisitResult, FileVisitor, Files, Path }
import java.util.jar.JarFile

import scala.collection.mutable
import scala.util.Using

import tastyquery.Classpaths
import tastyquery.Classpaths.{ ClassData, Classpath, ClasspathEntry, PackageData }

/**
 * JDK classpath adapter that retains resource metadata, not resource contents.
 *
 * TASTy Query's `ClasspathLoaders.read` eagerly reads and retains every `.class`
 * and `.tasty` byte array on the classpath. An sbt process already holds the build
 * and compiler in memory, so duplicating all dependency and JDK classfiles can
 * exhaust otherwise reasonable heaps before a symbol query starts. TASTy Query's
 * core classpath API is lazy, however: it only requires metadata up front and calls
 * `ClassData.read*Bytes` when a particular root is forced.
 *
 * All objects returned here are immutable and thread-safe. Jar files and streams
 * are opened only for the duration of a metadata scan or one resource read.
 */
private[sbta] object LazyClasspath:

  def read(paths: List[Path]): Classpath =
    paths.map(readEntry)

  private def readEntry(path: Path): ClasspathEntry =
    val resources =
      if !Files.exists(path) then Nil
      else if Files.isDirectory(path) then scanDirectory(path)
      else if Files.isRegularFile(path) then scanJar(path)
      else throw IllegalArgumentException(s"Illegal classpath entry: $path")

    val packages = resources
      .groupMap(_.packageName)(identity)
      .iterator
      .map { case (packageName, packageResources) =>
        val classes = packageResources
          .groupMap(_.binaryName)(identity)
          .iterator
          .map { case (binaryName, classResources) =>
            val classFile = classResources.collectFirst { case Resource(_, _, Kind.Class, readBytes, debugPath) =>
              ByteSource(readBytes, debugPath)
            }
            val tastyFile = classResources.collectFirst { case Resource(_, _, Kind.Tasty, readBytes, debugPath) =>
              ByteSource(readBytes, debugPath)
            }
            LazyClassData(binaryName, tastyFile, classFile)
          }
          .toList
          .sortBy(_.binaryName)
        LazyPackageData(path, packageName, classes)
      }
      .toList
      .sortBy(_.dotSeparatedName)

    LazyClasspathEntry(path, packages)
  end readEntry

  private enum Kind(val extension: String):
    case Class extends Kind(".class")
    case Tasty extends Kind(".tasty")

  private object Kind:
    def of(resourceName: String): Option[Kind] =
      if resourceName.endsWith(Class.extension) then Some(Class)
      else if resourceName.endsWith(Tasty.extension) then Some(Tasty)
      else None

  private final case class ByteSource(readBytes: () => IArray[Byte], debugPath: String)

  private final case class Resource(
      packageName: String,
      binaryName: String,
      kind: Kind,
      readBytes: () => IArray[Byte],
      debugPath: String,
  )

  private def resource(resourceName: String, kind: Kind, readBytes: () => IArray[Byte], debugPath: String): Resource =
    val withoutExtension = resourceName.dropRight(kind.extension.length)
    val dottedName       = withoutExtension.replace('/', '.').replace('\\', '.')
    val separator        = dottedName.lastIndexOf('.')
    val (packageName, binaryName) =
      if separator < 0 then "" -> dottedName
      else dottedName.substring(0, separator) -> dottedName.substring(separator + 1)
    Resource(packageName, binaryName, kind, readBytes, debugPath)
  end resource

  private def scanJar(path: Path): List[Resource] =
    Using.resource(JarFile(path.toFile)) { jar =>
      import scala.language.unsafeNulls
      val out     = List.newBuilder[Resource]
      val entries = jar.entries()
      while entries.hasMoreElements do
        val entry = entries.nextElement()
        val name  = entry.getName
        if !entry.isDirectory then
          Kind.of(name).foreach { kind =>
            val debugPath = s"$path:$name"
            out += resource(name, kind, () => readJarResource(path, name), debugPath)
          }
      out.result()
    }

  private def readJarResource(path: Path, resourceName: String): IArray[Byte] =
    Using.resource(JarFile(path.toFile)) { jar =>
      import scala.language.unsafeNulls
      val entry = Option(jar.getJarEntry(resourceName)).getOrElse {
        throw IOException(s"Classpath resource no longer exists: $path:$resourceName")
      }
      Using.resource(jar.getInputStream(entry))(stream => IArray.from(stream.readAllBytes()))
    }

  private def scanDirectory(root: Path): List[Resource] =
    val out = List.newBuilder[Resource]
    Files.walkFileTree(
      root,
      new FileVisitor[Path]:
        override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult =
          FileVisitResult.CONTINUE

        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult =
          val relative = root.relativize(file).toString
          Kind.of(relative).foreach { kind =>
            out += resource(relative, kind, () => IArray.from(Files.readAllBytes(file)), file.toString)
          }
          FileVisitResult.CONTINUE

        override def visitFileFailed(file: Path, error: IOException): FileVisitResult =
          FileVisitResult.CONTINUE

        override def postVisitDirectory(dir: Path, error: IOException | Null): FileVisitResult =
          FileVisitResult.CONTINUE
      ,
    )
    out.result()

  private final case class LazyClasspathEntry(path: Path, packages: List[LazyPackageData]) extends ClasspathEntry:
    override def listAllPackages(): List[PackageData] = packages
    override def toString: String                    = path.toString

  private final case class LazyPackageData(path: Path, dotSeparatedName: String, classes: List[LazyClassData])
      extends PackageData:
    private lazy val classesByName = classes.iterator.map(data => data.binaryName -> data).toMap

    override def listAllClassDatas(): List[ClassData] = classes
    override def getClassDataByBinaryName(binaryName: String): Option[ClassData] = classesByName.get(binaryName)
    override def toString: String = s"$path:$dotSeparatedName"

  private final case class LazyClassData(
      binaryName: String,
      tastyFile: Option[ByteSource],
      classFile: Option[ByteSource],
  ) extends ClassData:
    override def hasTastyFile: Boolean = tastyFile.isDefined
    override def readTastyFileBytes(): IArray[Byte] = tastyFile.get.readBytes()
    override def hasClassFile: Boolean = classFile.isDefined
    override def readClassFileBytes(): IArray[Byte] = classFile.get.readBytes()
    override def toString: String = tastyFile.orElse(classFile).fold(binaryName)(_.debugPath)
end LazyClasspath
