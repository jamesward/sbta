package com.jamesward.sbta

import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference

import scala.util.control.NonFatal


/**
 * Process-global holder for the classpath entries the MCP symbol tools read.
 *
 * The plugin's automatic refresh (`SbtMcpPlugin.refreshFromState`, run on the sbt
 * command loop where `fullClasspathAsJars` is available) pushes entries + a content
 * fingerprint and target Scala version here before each symbol query; MCP handlers
 * then invoke a lazily-built, cached isolated reader session through a JDK-only
 * reflective bridge. This decouples symbol queries from sbt's task engine and Scala
 * runtime while remaining in-process.
 *
 * Multi-module note: entries are keyed by active project id. The last refresh wins,
 * and an aggregating project's entry contains its own classpath plus all transitive
 * aggregates. A future revision can expose the project as a tool argument.
 */
object SymbolIndexState {
  final case class Hit(kind: String, fqn: String)

  private final case class Entry(
      entries: List[Path],
      searchEntries: List[Path],
      fingerprint: Vector[String],
      targetScalaVersion: String,
      session: Option[IsolatedSymbolIndex.Session],
  )

  private val byProject = new AtomicReference[Map[String, Entry]](Map.empty)
  private val active    = new AtomicReference[Option[String]](None)

  /**
   * Set the active project's classpath and target Scala version. An unchanged
   * fingerprint keeps the isolated reader session; a changed classpath or Scala
   * version closes it and rebuilds lazily on the next query.
   */
  def update(
      projectId: String,
      entries: List[Path],
      fingerprint: Vector[String],
      targetScalaVersion: String = scala.util.Properties.versionNumberString,
      searchEntries: List[Path] = Nil,
  ): Unit = synchronized {
    val effectiveSearchEntries = if searchEntries.nonEmpty then searchEntries else entries
    val previous = byProject.get.get(projectId)
    val keptSession = previous
      .filter(entry =>
        entry.fingerprint == fingerprint &&
          entry.targetScalaVersion == targetScalaVersion &&
          entry.searchEntries == effectiveSearchEntries
      )
      .flatMap(_.session)
    if keptSession.isEmpty then previous.flatMap(_.session).foreach(_.close())
    byProject.set(
      byProject.get.updated(
        projectId,
        Entry(entries, effectiveSearchEntries, fingerprint, targetScalaVersion, keptSession),
      )
    )
    active.set(Some(projectId))
  }

  /** Convenience: index a `File.pathSeparator`-separated classpath string. */
  def updateFromClasspathString(projectId: String, classpath: String): Unit = {
    val entries = classpath
      .split(java.io.File.pathSeparatorChar)
      .iterator
      .filter(_.nonEmpty)
      .map(java.nio.file.Paths.get(_))
      .toList
    update(projectId, entries, entries.map(_.toString).toVector)
  }

  def isReady: Boolean = active.get.exists(byProject.get.contains)

  enum QueryResult[+A]:
    case Available(value: A)
    case Unavailable(message: String)

    def toOption: Option[A] = this match {
      case Available(value) => Some(value)
      case Unavailable(_)   => None
    }

  def globSearchResult(
      query: String,
      inPackage: Option[String] = None,
      limit: Int = 100,
  ): QueryResult[List[Hit]] =
    withSession(_.globSearch(query, inPackage, limit).map(hit => Hit(hit.kind, hit.fqn)))

  def globSearch(query: String, inPackage: Option[String] = None, limit: Int = 100): Option[List[Hit]] =
    globSearchResult(query, inPackage, limit).toOption

  def inspectResult(fqn: String): QueryResult[Option[String]] =
    withSession(_.inspect(fqn))

  def inspect(fqn: String): Option[String] =
    inspectResult(fqn).toOption.flatten

  def locationResult(fqn: String): QueryResult[Option[String]] =
    withSession(_.location(fqn))

  def location(fqn: String): Option[String] =
    locationResult(fqn).toOption.flatten

  def activeReaderVersion: Option[String] =
    withSession(session => session.readerVersion).toOption

  def activeRuntimeScalaVersion: Option[String] =
    withSession(session => session.runtimeScalaVersion).toOption

  /** Close all child classloaders on sbt unload/reload. */
  def shutdown(): Unit = synchronized {
    byProject.get.valuesIterator.flatMap(_.session).foreach(_.close())
    byProject.set(Map.empty)
    active.set(None)
    IsolatedSymbolIndex.shutdownResources()
  }

  private def withSession[A](operation: IsolatedSymbolIndex.Session => A): QueryResult[A] = synchronized {
    import QueryResult.*

    active.get match {
      case None => Unavailable("symbol index is not ready")
      case Some(projectId) =>
        byProject.get.get(projectId) match {
          case None => Unavailable("symbol index is not ready")
          case Some(Entry(_, _, _, _, Some(session))) => attempt(operation(session))
          case Some(Entry(entries, searchEntries, fingerprint, targetScalaVersion, None)) if entries.nonEmpty =>
            attempt(IsolatedSymbolIndex.open(entries, searchEntries, targetScalaVersion)) match {
              case Unavailable(message) => Unavailable(message)
              case Available(session) =>
                byProject.set(
                  byProject.get.updated(
                    projectId,
                    Entry(entries, searchEntries, fingerprint, targetScalaVersion, Some(session)),
                  )
                )
                attempt(operation(session))
            }
          case _ => Unavailable("symbol index classpath is empty")
        }
    }
  }

  private def attempt[A](value: => A): QueryResult[A] =
    try QueryResult.Available(value)
    catch {
      case NonFatal(error) =>
        val message = Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)
        QueryResult.Unavailable(s"symbol reader unavailable: $message")
    }
}
