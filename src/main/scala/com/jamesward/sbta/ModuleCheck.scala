package com.jamesward.sbta

import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path }
import java.util.concurrent.TimeUnit

import scala.util.control.NonFatal

/**
 * Process-global state and logic behind the `check` tool: a fast, warm,
 * compile-free validation of Scala 3 sources against one module's classpath.
 *
 * [[SbtMcpPlugin]] computes a [[ModuleConfig]] per (project, configuration) on the
 * sbt command loop and pushes it here. [[check]] then runs entirely off the loop
 * (it never waits behind a `~` watch) with a cached, JIT-warm compiler
 * ([[IsolatedCheck.Session]], one per compiler version) and a fresh compiler context.
 */
object ModuleCheck {

  /**
   * Everything needed to check one module's sources without the task engine.
   *
   * @param compilerJars the module's own Scala compiler (`scalaInstance.allJars`)
   * @param classpath    the module's class directory (last compile) + dependency classpath
   * @param sources      the module's current Scala/Java sources
   * @param recordedStamps source stamps from the last compile's Zinc analysis (None if never compiled)
   * @param currentStamp computes a source's current stamp in the same format as `recordedStamps`
   */
  final case class ModuleConfig(
      projectId: String,
      configuration: String,
      scalaVersion: String,
      compilerJars: List[Path],
      classpath: List[Path],
      scalacOptions: List[String],
      sources: List[Path],
      recordedStamps: Option[Map[Path, String]],
      currentStamp: (Path, String) => Option[String],
      notes: List[String] = Nil,
  ) {
    def label: String = s"$projectId/$configuration"
  }

  enum Scope {
    /** Requested files plus module sources changed since the last compile. */
    case Files
    /** Every source of the module (with requested content overlays). */
    case Module
  }

  object Scope {
    def parse(value: Option[String]): Either[String, Scope] =
      value.map(_.trim.toLowerCase).filter(_.nonEmpty) match {
        case None | Some("files") | Some("file") => Right(Scope.Files)
        case Some("module")                      => Right(Scope.Module)
        case Some(other)                         => Left(s"unknown scope '$other' (expected \"files\" or \"module\")")
      }
  }

  final case class Result(
      config: ModuleConfig,
      requested: List[Path],
      changedSiblings: List[Path],
      diagnostics: List[IsolatedCheck.Diagnostic],
      fatalWarnings: Boolean,
      compilerVersion: String,
      totalMillis: Long,
      compilerMillis: Long,
      reusedCompiler: Boolean,
  ) {
    def errors: List[IsolatedCheck.Diagnostic]   = diagnostics.filter(_.isError)
    def warnings: List[IsolatedCheck.Diagnostic] = diagnostics.filter(_.isWarning)
    def failed: Boolean                          = errors.nonEmpty || (fatalWarnings && warnings.nonEmpty)
  }

  private final val MaxSessions = 4

  // Compiler loaders keyed by compiler jars, insertion-ordered for LRU eviction;
  // guarded by `this`.
  private val sessions = scala.collection.mutable.LinkedHashMap.empty[List[Path], IsolatedCheck.Session]

  /**
   * Check `requested` files (absolute paths) of the module described by `config`.
   * `overlays` supplies in-memory content for requested files (unsaved edits);
   * everything else is read from disk.
   */
  def check(
      config: ModuleConfig,
      requested: List[Path],
      overlays: Map[Path, String],
      scope: Scope,
  ): Result = {
    val started = System.nanoTime()
    val requestedSet = requested.toSet

    val siblings: List[Path] = scope match {
      case Scope.Module => config.sources.filterNot(requestedSet)
      case Scope.Files  => changedSources(config).filterNot(requestedSet)
    }
    val units: List[(Path, String)] = (requested ++ siblings).map { path =>
      path -> overlays.getOrElse(path, new String(Files.readAllBytes(path), StandardCharsets.UTF_8))
    }

    val (session, reused) = sessionFor(config.compilerJars)
    val compileStarted = System.nanoTime()
    val diagnostics =
      session.check(config.classpath, IsolatedCheck.sanitizeOptions(config.scalacOptions), units)
    val finished       = System.nanoTime()

    Result(
      config = config,
      requested = requested,
      changedSiblings = if (scope == Scope.Files) siblings else Nil,
      diagnostics = diagnostics,
      fatalWarnings = isFatalWarnings(config.scalacOptions),
      compilerVersion = session.compilerVersion,
      totalMillis = TimeUnit.NANOSECONDS.toMillis(finished - started),
      compilerMillis = TimeUnit.NANOSECONDS.toMillis(finished - compileStarted),
      reusedCompiler = reused,
    )
  }

  /**
   * Module sources whose content differs from what the last compile saw. Their
   * compiled classes on the classpath are stale, so they are re-checked from source
   * alongside the requested files (as an incremental compile would). A module that
   * was never compiled has no usable classes, so all of its sources are included.
   */
  def changedSources(config: ModuleConfig): List[Path] =
    config.recordedStamps match {
      case None => config.sources
      case Some(recorded) =>
        config.sources.filter { source =>
          recorded.get(source) match {
            case None => true
            case Some(previous) =>
              try !config.currentStamp(source, previous).contains(previous)
              catch { case NonFatal(_) => true }
          }
        }
    }

  private[sbta] def isFatalWarnings(options: Seq[String]): Boolean =
    options.exists { option =>
      val name = option.takeWhile(_ != ':')
      (name == "-Werror" || name == "-Xfatal-warnings") && !option.endsWith(":false")
    }

  /** The compiler loader for `compilerJars` (and whether it was already loaded). */
  private def sessionFor(compilerJars: List[Path]): (IsolatedCheck.Session, Boolean) = synchronized {
    sessions.remove(compilerJars) match {
      case Some(session) =>
        sessions.update(compilerJars, session)
        (session, true)
      case None =>
        val session = IsolatedCheck.open(compilerJars)
        sessions.update(compilerJars, session)
        while (sessions.size > MaxSessions) {
          val (oldest, evicted) = sessions.head
          sessions.remove(oldest)
          closeQuietly(evicted)
        }
        (session, false)
    }
  }

  private def closeQuietly(session: IsolatedCheck.Session): Unit =
    try session.close()
    catch { case NonFatal(_) => () }

  /** Close all compiler sessions on sbt unload/reload. */
  def shutdown(): Unit = synchronized {
    sessions.valuesIterator.foreach(closeQuietly)
    sessions.clear()
    IsolatedCheck.shutdownResources()
  }

  /** Render a result for an agent. Paths are shown relative to `root`. */
  def render(result: Result, root: Path): String = {
    def rel(path: Path): String =
      try root.relativize(path).toString
      catch { case NonFatal(_) => path.toString }

    val errors   = result.errors.size
    val warnings = result.warnings.size
    def count(n: Int, noun: String) = s"$n $noun${if (n == 1) "" else "s"}"
    val fatal = if (result.fatalWarnings && warnings > 0) " (fatal: -Werror)" else ""
    val status = if (result.failed) "[error]" else "[ok]"
    val header =
      s"$status check ${result.config.label} (Scala ${result.compilerVersion}): " +
        s"${count(errors, "error")}, ${count(warnings, "warning")}$fatal"
    val compilerState = if (result.reusedCompiler) "warm compiler" else "compiler loaded"
    val timing        = s"elapsed: ${result.totalMillis} ms (compiler ${result.compilerMillis} ms, $compilerState)"
    val checked = {
      val files = result.requested.map(rel).mkString(", ")
      if (result.changedSiblings.isEmpty) s"checked: $files"
      else
        s"checked: $files (+ ${count(result.changedSiblings.size, "changed source")} since last compile: " +
          s"${result.changedSiblings.map(rel).mkString(", ")})"
    }
    val body = result.diagnostics.map { d =>
      val location = d.path match {
        case Some(path) if d.line > 0 => s"${rel(path)}:${d.line}:${d.column}: "
        case Some(path)               => s"${rel(path)}: "
        case None                     => ""
      }
      val code = d.errorNumber.map(n => f" [E$n%03d]").getOrElse("")
      val excerpt =
        if (d.line > 0 && d.lineContent.trim.nonEmpty)
          s"\n  ${d.lineContent}\n  ${" " * math.max(0, d.column)}^"
        else ""
      s"$location${d.severity}$code: ${d.message}$excerpt"
    }
    val notes = result.config.notes.map(n => s"(note: $n)")
    (List(header, timing, checked) ++ body ++ notes).mkString("\n")
  }
}
