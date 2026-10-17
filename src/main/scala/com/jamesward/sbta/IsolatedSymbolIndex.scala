package com.jamesward.sbta

import java.lang.reflect.{ InvocationTargetException, Method }
import java.net.{ URL, URLClassLoader }
import java.nio.file.{ Files, Path, StandardCopyOption }

import scala.jdk.CollectionConverters.*
import scala.util.Using

/**
 * An in-process but classloader-isolated TASTy Query universe.
 *
 * sbt 2.0 runs plugins with Scala 3.8 even when the loaded build targets Scala
 * 3.9. The target project's classpath supplies its matching Scala runtime, while
 * this loader supplies the matching TASTy Query reader. Its parent is the JDK
 * platform loader, deliberately excluding sbt's Scala runtime.
 */
private[sbta] object IsolatedSymbolIndex:

  final case class Hit(kind: String, fqn: String)

  final class Session private[IsolatedSymbolIndex] (
      loader: URLClassLoader,
      bridge: Object,
      context: Object,
      globMethod: Method,
      inspectMethod: Method,
      locationMethod: Method,
      val readerVersion: String,
      val runtimeScalaVersion: String,
  ) extends AutoCloseable:

    private[sbta] val isIsolated: Boolean =
      (bridge.getClass.getClassLoader eq loader) &&
        (loader.getParent eq ClassLoader.getPlatformClassLoader)

    def globSearch(query: String, inPackage: Option[String], limit: Int): List[Hit] = synchronized {
      val encoded = invoke(globMethod, context, query, inPackage.orNull, Int.box(limit))
        .asInstanceOf[java.util.List[String]]
      encoded.asScala.toList.flatMap { value =>
        value.indexOf('\u0000') match {
          case -1        => Nil
          case separator => Hit(value.substring(0, separator), value.substring(separator + 1)) :: Nil
        }
      }
    }

    def inspect(fqn: String): Option[String] = synchronized {
      Option(invoke(inspectMethod, context, fqn).asInstanceOf[String])
    }

    def location(fqn: String): Option[String] = synchronized {
      Option(invoke(locationMethod, context, fqn).asInstanceOf[String])
    }

    override def close(): Unit = loader.close()

    private def invoke(method: Method, args: Object*): Object =
      withContextLoader(loader) {
        try method.invoke(bridge, args*)
        catch {
          case error: InvocationTargetException => throw error.getCause
        }
      }
  end Session

  def open(entries: List[Path], searchEntries: List[Path], targetScalaVersion: String): Session =
    val readerUrl  = readerFor(targetScalaVersion)
    val runtimeUrls = matchingScalaRuntime(entries, targetScalaVersion).map(_.toUri.toURL)
    val urls = (symbolRuntimeUrl :: readerUrl :: (runtimeUrls ++ entries.map(_.toUri.toURL))).distinct.toArray
    val loader = URLClassLoader(urls, ClassLoader.getPlatformClassLoader)

    try
      withContextLoader(loader) {
        val bridgeClass = Class.forName("com.jamesward.sbta.IsolatedSymbolBridge$", true, loader)
        val bridge      = bridgeClass.getField("MODULE$").get(null)
        val methods     = bridgeClass.getMethods.iterator.map(method => method.getName -> method).toMap
        val context = methods("create").invoke(bridge, entries.asJava, searchEntries.asJava).asInstanceOf[Object]
        val version = methods("readerVersion").invoke(bridge).asInstanceOf[String]
        val runtimeVersion = methods("runtimeScalaVersion").invoke(bridge).asInstanceOf[String]
        new Session(
          loader,
          bridge,
          context,
          methods("globSearch"),
          methods("inspect"),
          methods("location"),
          version,
          runtimeVersion,
        )
      }
    catch
      case error: InvocationTargetException =>
        loader.close()
        throw error.getCause
      case error: Throwable =>
        loader.close()
        throw error
  end open

  private def readerFor(targetScalaVersion: String): URL =
    val parts = targetScalaVersion.split("[.-]")
    val major = parts.headOption.flatMap(_.toIntOption)
    val minor = parts.drop(1).headOption.flatMap(_.toIntOption)
    (major, minor) match {
      case (Some(3), Some(10))                  => tastyQuery110Url
      case (Some(3), Some(9))                   => tastyQuery19Url
      case (Some(3), Some(value)) if value <= 8 => tastyQuery18Url
      case (Some(3), Some(value)) =>
        throw IllegalArgumentException(s"No embedded TASTy reader supports Scala 3.$value")
      case _ => throw IllegalArgumentException(s"Scala 3 is required for symbol indexing, got $targetScalaVersion")
    }

  private def matchingScalaRuntime(entries: List[Path], targetScalaVersion: String): List[Path] =
    entries.filter { path =>
      val normalized = path.toString.replace('\\', '/')
      normalized.contains(s"/org/scala-lang/scala-library/$targetScalaVersion/") ||
      normalized.contains(s"/org/scala-lang/scala3-library_3/$targetScalaVersion/") ||
      normalized.endsWith(s"/scala-library-$targetScalaVersion.jar") ||
      normalized.endsWith(s"/scala3-library_3-$targetScalaVersion.jar")
    }

  private def symbolRuntimeUrl: URL =
    embeddedJar("/sbta-symbol/symbol-runtime.jar", "symbol-runtime.jar")

  private def tastyQuery18Url: URL =
    embeddedJar("/sbta-symbol/tasty-query-1.8.0.jar", "tasty-query-1.8.0.jar")

  private def tastyQuery110Url: URL =
    embeddedJar("/sbta-readers/tasty-query-1.10.0.jar", "tasty-query-1.10.0.jar")

  private def tastyQuery19Url: URL =
    embeddedJar("/sbta-readers/tasty-query-1.9.0.jar", "tasty-query-1.9.0.jar")

  private var extractedAssets: Map[String, Path] = Map.empty

  private def embeddedJar(resource: String, fileName: String): URL = synchronized {
    val jar = extractedAssets.get(resource).filter(Files.exists(_)).getOrElse {
      val stream = Option(getClass.getResourceAsStream(resource)).getOrElse {
        throw IllegalStateException(s"embedded symbol asset is missing: $resource")
      }
      val directory = Files.createTempDirectory("sbta-symbol-")
      val extracted = directory.resolve(fileName)
      Using.resource(stream) { input =>
        Files.copy(input, extracted, StandardCopyOption.REPLACE_EXISTING)
      }
      directory.toFile.deleteOnExit()
      extracted.toFile.deleteOnExit()
      extractedAssets = extractedAssets.updated(resource, extracted)
      extracted
    }
    jar.toUri.toURL
  }

  /** Called after all sessions close on sbt unload; a later use re-extracts. */
  def shutdownResources(): Unit = synchronized {
    val paths = extractedAssets.values.toList
    paths.foreach(Files.deleteIfExists)
    paths.flatMap(path => Option(path.getParent)).distinct.foreach(Files.deleteIfExists)
    extractedAssets = Map.empty
  }

  private def withContextLoader[A](loader: ClassLoader)(operation: => A): A =
    val thread   = Thread.currentThread()
    val previous = thread.getContextClassLoader
    thread.setContextClassLoader(loader)
    try operation
    finally thread.setContextClassLoader(previous)
end IsolatedSymbolIndex
