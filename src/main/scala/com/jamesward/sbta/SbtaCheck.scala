package com.jamesward.sbta

import java.nio.file.Path

import scala.util.control.NonFatal

/**
 * sbta's entry to the `check` engine it shares with the plugin ([[ModuleCheck]] and
 * the isolated compiler, which are package-private here).
 */
object SbtaCheck:

  /** Scala 3.3 and later: what the compiler bridge links against. */
  def isSupportedScala(version: String): Boolean =
    val parts = version.split("[.-]")
    (parts.headOption.flatMap(_.toIntOption), parts.lift(1).flatMap(_.toIntOption)) match
      case (Some(3), Some(minor)) => minor >= 3
      case (Some(major), _)       => major > 3
      case _                      => false

  /** Check `requested` files of `config`'s module and render the result for an agent (paths relative to `root`). */
  def run(
      config: ModuleCheck.ModuleConfig,
      requested: List[Path],
      overlays: Map[Path, String],
      scope: Option[String],
      root: Path,
  ): String =
    ModuleCheck.Scope.parse(scope) match
      case Left(error) => s"check: $error"
      case Right(parsed) =>
        try ModuleCheck.render(ModuleCheck.check(config, requested, overlays, parsed), root)
        catch case NonFatal(e) => s"check: compiler failed: ${Option(e.getMessage).getOrElse(e.toString)}"
