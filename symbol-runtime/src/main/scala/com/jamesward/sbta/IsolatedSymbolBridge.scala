package com.jamesward.sbta

import java.net.URI
import java.nio.file.{ FileSystems, Path }

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import tastyquery.Classpaths.ClasspathEntry
import tastyquery.Contexts.Context

/**
 * Entry point loaded reflectively inside [[IsolatedSymbolIndex]]'s classloader.
 *
 * Do not expose Scala collections, `Option`, TASTy Query types, or project types
 * from these methods. The caller lives in sbt's Scala runtime, while this object
 * lives in the target project's Scala runtime; only bootstrap/JDK classes have
 * identity across that boundary.
 */
object IsolatedSymbolBridge:

  private final case class BridgeContext(context: Context, searchEntries: List[ClasspathEntry])

  def create(entries: java.util.List[Path], searchEntries: java.util.List[Path]): Object =
    val paths     = entries.asScala.toList
    val classpath = LazyClasspath.read(paths ++ jrtBase)
    val byPath    = paths.zip(classpath).toMap
    val searchable = searchEntries.asScala.toList.flatMap(byPath.get)
    BridgeContext(Context.initialize(classpath), searchable)

  /** Each result is encoded as `kind\u0000fully.qualified.Name`. */
  def globSearch(
      context: Object,
      query: String,
      inPackage: String,
      limit: Int,
  ): java.util.List[String] =
    val bridgeContext = context.asInstanceOf[BridgeContext]
    given Context = bridgeContext.context
    SymbolIndex
      .globSearch(query, Option(inPackage), limit, bridgeContext.searchEntries)
      .map(hit => s"${hit.kind}\u0000${hit.fqn}")
      .asJava

  /** Returns null when the symbol cannot be found. */
  def inspect(context: Object, fqn: String): String =
    given Context = context.asInstanceOf[BridgeContext].context
    SymbolIndex.inspect(fqn).orNull

  /** Returns null when no source position is available. */
  def location(context: Object, fqn: String): String =
    given Context = context.asInstanceOf[BridgeContext].context
    SymbolIndex.location(fqn).orNull

  def runtimeScalaVersion(): String = scala.util.Properties.versionNumberString

  def readerVersion(): String =
    Option(classOf[Context].getPackage.getImplementationVersion).getOrElse("unknown")

  private def jrtBase: List[Path] =
    try List(FileSystems.getFileSystem(URI.create("jrt:/")).getPath("modules", "java.base"))
    catch { case NonFatal(_) => Nil }
end IsolatedSymbolBridge
