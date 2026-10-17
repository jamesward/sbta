package com.jamesward.sbta

import scala.jdk.CollectionConverters.*

import dotty.tools.dotc.{ Compiler, Driver }
import dotty.tools.dotc.core.Contexts.{ Context, FreshContext }
import dotty.tools.dotc.reporting.{ Diagnostic, HideNonSensicalMessages, StoreReporter, UniqueMessagePositions }
import dotty.tools.dotc.util.SourceFile

/**
 * Entry point loaded reflectively inside [[IsolatedCheck]]'s classloader, next to
 * the TARGET project's own `scala3-compiler` (from its `scalaInstance`).
 *
 * This bridge is compiled once against the oldest supported compiler (3.3.1) and
 * linked at runtime against whichever Scala 3 compiler (3.3+) the project uses; it
 * touches only the long-stable driver surface (`Driver.setup`, `Compiler.newRun`,
 * `Run.compileSources`, `StoreReporter`, `SourceFile.virtual`). Only JDK types cross
 * the boundary: the caller lives in sbt's Scala runtime.
 *
 * Every check runs in a FRESH root context, i.e. exactly as a batch compile would:
 * no symbols survive from one check to the next, so an unsaved overlay can never
 * leak into a later check. What stays warm is this classloader, i.e. the JIT-compiled
 * compiler and its cached classpath archives.
 */
object IsolatedCheckBridge:

  private final class CheckDriver extends Driver:
    override def sourcesRequired: Boolean = false

    def check(args: Array[String], paths: Array[String], contents: Array[String]): List[String] =
      val setupReporter = new StoreReporter(null)
      val base: FreshContext = initCtx.fresh.setReporter(setupReporter)
      val root = setup(args, base) match
        case Some((_, ctx)) => ctx
        case None =>
          val problems = setupReporter.removeBufferedMessages(using base).map(_.message)
          throw IllegalArgumentException(("invalid compiler options" :: problems).mkString("\n"))
      // Same de-duplication / non-sensical-message hiding as sbt's own reporter
      // (dotty's AbstractReporter, which zinc's DelegatingReporter extends).
      val reporter = new StoreReporter(null) with UniqueMessagePositions with HideNonSensicalMessages
      val run      = new Compiler().newRun(using root.fresh.setReporter(reporter))
      val sources  = paths.indices.toList.map(i => SourceFile.virtual(paths(i), contents(i)))
      run.compileSources(sources)
      given Context = root
      reporter.removeBufferedMessages.map(encode)

  /** Check `paths` (with `contents`) together in one run of the compiler pipeline limited by `args`. */
  def check(args: Array[String], paths: Array[String], contents: Array[String]): java.util.List[String] =
    CheckDriver().check(args, paths, contents).asJava

  def compilerVersion(): String =
    dotty.tools.dotc.config.Properties.versionNumberString

  /**
   * `level \0 errorNumber \0 path \0 line \0 column \0 lineContent \0 message`, with
   * 0-based line/column and empty fields when the diagnostic has no position.
   */
  private def encode(diagnostic: Diagnostic)(using Context): String =
    // Like sbt: report inlined code at its call site, and append `-explain` text.
    val pos = diagnostic.pos.nonInlined
    val errorNumber =
      try diagnostic.msg.errorId.errorNumber.toString
      catch case scala.util.control.NonFatal(_) => ""
    val fields =
      if pos.exists then
        List(pos.source.path, pos.line.toString, pos.column.toString, pos.lineContent.stripLineEnd)
      else List("", "", "", "")
    val explanation =
      if Diagnostic.shouldExplain(diagnostic) && diagnostic.msg.explanation.nonEmpty then
        System.lineSeparator() + diagnostic.msg.explanation
      else ""
    (diagnostic.level.toString :: errorNumber :: fields ::: (diagnostic.msg.message + explanation) :: Nil)
      .mkString("\u0000")
end IsolatedCheckBridge
