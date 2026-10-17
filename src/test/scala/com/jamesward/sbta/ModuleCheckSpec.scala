package com.jamesward.sbta

import java.nio.file.{ Files, Path, Paths }

import zio.test.*

object ModuleCheckSpec extends ZIOSpecDefault:

  private def config(
      sources: List[Path],
      recorded: Option[Map[Path, String]],
      stamp: (Path, String) => Option[String] = (_, _) => None,
      options: List[String] = Nil,
  ) =
    ModuleCheck.ModuleConfig(
      projectId = "app",
      configuration = "compile",
      scalaVersion = "3.9.0",
      compilerJars = Nil,
      classpath = Nil,
      scalacOptions = options,
      sources = sources,
      recordedStamps = recorded,
      currentStamp = stamp,
    )

  private def diagnostic(severity: String, path: Path, line: Int, column: Int, content: String, message: String) =
    IsolatedCheck.Diagnostic(severity, Some(7), Some(path), line, column, content, message)

  def spec = suite("ModuleCheckSpec")(
    test("sanitizeOptions drops output, rewrite, semanticdb and phase options (both spellings)") {
      val options = List(
        "-deprecation", "-d", "/out", "-classpath", "/cp", "-Xsemanticdb", "-semanticdb-target", "/sdb",
        "-semanticdb-target:/sdb2", "-rewrite", "-Ystop-after:typer", "-Ypickle-write", "/p.jar",
        "-Wunused:all", "-Werror", "-Xplugin:/plugin.jar", "-source:future",
      )
      assertTrue(
        IsolatedCheck.sanitizeOptions(options) ==
          List("-deprecation", "-Wunused:all", "-Werror", "-Xplugin:/plugin.jar", "-source:future")
      )
    },
    test("fatal warnings are recognized under both names") {
      assertTrue(
        ModuleCheck.isFatalWarnings(List("-Werror")),
        ModuleCheck.isFatalWarnings(List("-Xfatal-warnings")),
        ModuleCheck.isFatalWarnings(List("-Werror:true")),
        !ModuleCheck.isFatalWarnings(List("-Werror:false")),
        !ModuleCheck.isFatalWarnings(List("-Wunused:all")),
      )
    },
    test("scope parsing defaults to files and rejects unknown scopes") {
      assertTrue(
        ModuleCheck.Scope.parse(None) == Right(ModuleCheck.Scope.Files),
        ModuleCheck.Scope.parse(Some(" Module ")) == Right(ModuleCheck.Scope.Module),
        ModuleCheck.Scope.parse(Some("files")) == Right(ModuleCheck.Scope.Files),
        ModuleCheck.Scope.parse(Some("all")).isLeft,
      )
    },
    test("only Scala 3.3 and later is supported") {
      assertTrue(
        SbtaCheck.isSupportedScala("3.3.0"),
        SbtaCheck.isSupportedScala("3.9.0"),
        SbtaCheck.isSupportedScala("3.10.0-RC1"),
        !SbtaCheck.isSupportedScala("3.2.2"),
        !SbtaCheck.isSupportedScala("2.13.16"),
      )
    },
    test("changed sources: all when never compiled; otherwise those whose stamp differs or is new") {
      val a = Paths.get("/src/A.scala")
      val b = Paths.get("/src/B.scala")
      val c = Paths.get("/src/C.scala")
      val stamps = Map(a -> "farm(1)", b -> "farm(2)")
      val current = Map(a -> "farm(1)", b -> "farm(9)")
      assertTrue(
        ModuleCheck.changedSources(config(List(a, b, c), None)) == List(a, b, c),
        ModuleCheck.changedSources(config(List(a, b, c), Some(stamps), (p, _) => current.get(p))) == List(b, c),
        // A stamp that cannot be computed is treated as changed (safe: just re-checked).
        ModuleCheck.changedSources(config(List(a), Some(stamps), (_, _) => throw RuntimeException("io"))) == List(a),
      )
    },
    test("render: header, timing, checked files, located diagnostics with a caret, and notes") {
      val root = Files.createTempDirectory("render")
      val file = root.resolve("app/src/main/scala/A.scala")
      val sibling = root.resolve("app/src/main/scala/B.scala")
      val result = ModuleCheck.Result(
        config = config(Nil, None).copy(notes = List("something to know")),
        requested = List(file),
        changedSiblings = List(sibling),
        diagnostics = List(diagnostic("error", file, 4, 15, "  val x: Int = \"s\"", "Found: String\nRequired: Int")),
        fatalWarnings = false,
        compilerVersion = "3.9.0",
        totalMillis = 12,
        compilerMillis = 10,
        reusedCompiler = true,
      )
      val text = ModuleCheck.render(result, root)
      assertTrue(
        text.linesIterator.toList.take(3) == List(
          "[error] check app/compile (Scala 3.9.0): 1 error, 0 warnings",
          "elapsed: 12 ms (compiler 10 ms, warm compiler)",
          "checked: app/src/main/scala/A.scala (+ 1 changed source since last compile: app/src/main/scala/B.scala)",
        ),
        text.contains("app/src/main/scala/A.scala:4:15: error [E007]: Found: String\nRequired: Int\n  " +
          "  val x: Int = \"s\"\n                 ^"),
        text.endsWith("(note: something to know)"),
      )
    },
    test("warnings alone pass, unless fatal") {
      val root = Paths.get("/r")
      val warning = diagnostic("warning", root.resolve("A.scala"), 1, 1, "x", "careful")
      def result(fatal: Boolean) = ModuleCheck.Result(
        config(Nil, None), Nil, Nil, List(warning), fatal, "3.9.0", 1, 1, reusedCompiler = false,
      )
      assertTrue(
        !result(fatal = false).failed,
        result(fatal = true).failed,
        ModuleCheck.render(result(fatal = true), root).startsWith(
          "[error] check app/compile (Scala 3.9.0): 0 errors, 1 warning (fatal: -Werror)"
        ),
      )
    },
  )
