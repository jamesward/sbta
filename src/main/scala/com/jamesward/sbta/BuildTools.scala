package com.jamesward.sbta

import com.jamesward.ziohttp.mcp.*
import com.jamesward.ziohttp.mcp.McpInput.given
import com.jamesward.ziohttp.mcp.McpOutput.given
import com.jamesward.ziohttp.mcp.McpError.given
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.{ DeriveSchema, Schema }
import zio.schema.annotation.description
import zio.schema.codec.JsonCodec as SchemaJsonCodec
import zio.schema.codec.json.schemaJson

/**
 * The structured build tools: hand-written MCP tools with typed input and output
 * schemas for the build operations agents use most. The work is done by
 * [[SbtTools]], as a client of the build's sbt server, reached through the
 * string-only `buildTool` callback: arguments cross as string lists, results come
 * back as a JSON object (or an error message, which never starts with `{`). Each
 * result is decoded into its output case class here, so `structuredContent` always
 * matches the advertised `outputSchema`.
 */
object BuildTools:

  type Run = (String, Map[String, List[String]]) => String

  // ---- inputs ----

  private final val ProjectDoc =
    "Project id (see the `projects` tool); a project of another build (a source dependency) works by its id too, " +
      "or as `{<build>}id` when the id isn't unique. Default: the session's current project; " +
      "an aggregating project also runs this for the projects it aggregates."

  enum CompileConfiguration:
    case compile, test
  object CompileConfiguration:
    given Schema[CompileConfiguration] = DeriveSchema.gen[CompileConfiguration]

  final case class CompileArgs(
      @description(ProjectDoc) project: Option[String] = None,
      @description("`compile` (main sources, the default) or `test` (test sources, and the main sources they depend on).")
      configuration: Option[CompileConfiguration] = None,
  )
  object CompileArgs { given Schema[CompileArgs] = DeriveSchema.gen[CompileArgs] }

  final case class TestArgs(
      @description(ProjectDoc) project: Option[String] = None,
      @description(
        "Test classes to run: fully-qualified names or `*` globs (e.g. `com.example.FooSpec`, `*Integration*`). " +
          "Runs exactly these, whether or not they changed. See `test-names` for the available names."
      )
      tests: Option[List[String]] = None,
      @description(
        "Run every test (sbt `testFull`). By default, without `tests`, sbt 2 runs only the tests that failed " +
          "before, were not run yet, or whose code changed, so passing tests may not be re-run."
      )
      full: Option[Boolean] = None,
      @description("Arguments passed to the test framework (what follows `--` in `testOnly`), e.g. a ScalaTest `-z` filter.")
      frameworkArgs: Option[List[String]] = None,
  )
  object TestArgs { given Schema[TestArgs] = DeriveSchema.gen[TestArgs] }

  final case class RunArgs(
      @description("Project id (see the `projects` tool). Default: the session's current project (run does not aggregate).")
      project: Option[String] = None,
      @description("Fully-qualified main class. Default: the project's main class; see `main-classes`.")
      mainClass: Option[String] = None,
      @description("Program arguments, passed verbatim (each element is one argument).")
      args: Option[List[String]] = None,
  )
  object RunArgs { given Schema[RunArgs] = DeriveSchema.gen[RunArgs] }

  final case class ProjectArgs(@description(ProjectDoc) project: Option[String] = None)
  object ProjectArgs { given Schema[ProjectArgs] = DeriveSchema.gen[ProjectArgs] }

  enum DependencyConfiguration:
    case compile, runtime, test, provided
  object DependencyConfiguration:
    given Schema[DependencyConfiguration] = DeriveSchema.gen[DependencyConfiguration]

  final case class DependenciesArgs(
      @description("Project id (see the `projects` tool). Default: the session's current project.")
      project: Option[String] = None,
      @description("Which classpath's resolved dependencies to report (default `compile`).")
      configuration: Option[DependencyConfiguration] = None,
  )
  object DependenciesArgs { given Schema[DependenciesArgs] = DeriveSchema.gen[DependenciesArgs] }

  final case class ShowArgs(
      @description(
        "An sbt key, optionally scoped like on the sbt shell: `scalaVersion`, `Test / fork`, " +
          "`core / Compile / sourceDirectories`, `core / Test / fullClasspath`."
      )
      key: String
  )
  object ShowArgs { given Schema[ShowArgs] = DeriveSchema.gen[ShowArgs] }

  // ---- outputs ----

  final case class Problem(
      @description("`error`, `warning` or `info`.") severity: String,
      @description("Source file, relative to the build root when inside it.") file: Option[String],
      @description("1-based line.") line: Option[Int],
      @description("0-based column, as sbt prints it (`file:line:column`).") column: Option[Int],
      message: String,
  )
  object Problem { given Schema[Problem] = DeriveSchema.gen[Problem] }

  final case class ActionResult(
      @description("The sbt command line that ran.") command: String,
      success: Boolean,
      elapsedMs: Long,
      errorCount: Int,
      warningCount: Int,
      @description("Compiler errors and warnings reported while the command ran (at most 500).") problems: List[Problem],
      @description("Captured sbt console output (the last 20000 characters).") log: String,
      logTruncated: Boolean,
      @description("Why the command failed, when `success` is false.") error: Option[String],
      @description("`run` only: the program's stdout and stderr lines (the last 20000 characters).") output: Option[String],
  )
  object ActionResult { given Schema[ActionResult] = DeriveSchema.gen[ActionResult] }

  final case class TestSummary(total: Int, passed: Int, failed: Int, errors: Int, skipped: Int, suites: Int)
  object TestSummary { given Schema[TestSummary] = DeriveSchema.gen[TestSummary] }

  final case class TestFailure(
      @description("Fully-qualified test class.") suite: String,
      @description("The test within the suite, when the framework reports one.") test: Option[String],
      @description("`failed` (assertion) or `error` (unexpected exception).") status: String,
      @description("The framework's failure message.") message: Option[String],
      durationMs: Option[Long],
  )
  object TestFailure { given Schema[TestFailure] = DeriveSchema.gen[TestFailure] }

  final case class SuiteError(suite: String, message: String)
  object SuiteError { given Schema[SuiteError] = DeriveSchema.gen[SuiteError] }

  final case class TestResult(
      @description("The sbt command line that ran.") command: String,
      success: Boolean,
      elapsedMs: Long,
      errorCount: Int,
      warningCount: Int,
      @description("Compiler errors and warnings reported while the command ran (at most 500).") problems: List[Problem],
      @description("Captured sbt console output (the last 20000 characters).") log: String,
      logTruncated: Boolean,
      @description("Why the command failed, when `success` is false.") error: Option[String],
      @description("Counts over the test cases that ran (0 total: nothing needed to run, or no tests matched).")
      summary: TestSummary,
      failures: List[TestFailure],
      @description("Suites that failed outside a test case (e.g. a constructor or setup error).") suiteErrors: List[SuiteError],
  )
  object TestResult { given Schema[TestResult] = DeriveSchema.gen[TestResult] }

  final case class PublishedModule(project: String, organization: String, name: String, version: String)
  object PublishedModule { given Schema[PublishedModule] = DeriveSchema.gen[PublishedModule] }

  final case class PublishLocalResult(
      @description("The sbt command line that ran.") command: String,
      success: Boolean,
      elapsedMs: Long,
      errorCount: Int,
      warningCount: Int,
      @description("Compiler errors and warnings reported while the command ran (at most 500).") problems: List[Problem],
      @description("Captured sbt console output (the last 20000 characters).") log: String,
      logTruncated: Boolean,
      @description("Why the command failed, when `success` is false.") error: Option[String],
      @description("Modules published to the local repository (dependency coordinates).") published: List[PublishedModule],
  )
  object PublishLocalResult { given Schema[PublishLocalResult] = DeriveSchema.gen[PublishLocalResult] }

  final case class ProjectDependency(project: String, @description("e.g. `compile->compile;test->test`.") configuration: Option[String])
  object ProjectDependency { given Schema[ProjectDependency] = DeriveSchema.gen[ProjectDependency] }

  final case class ProjectInfo(
      id: String,
      @description("Base directory, relative to the build root.") base: String,
      @description("The session's current project (the default for the other tools).") current: Boolean,
      @description("The root project of the build sbt was started in.") root: Boolean,
      @description("Build URI, only for projects of another build (a source dependency).") build: Option[String],
      name: Option[String],
      organization: Option[String],
      version: Option[String],
      scalaVersion: Option[String],
      @description("Projects this one aggregates: tasks run on it also run on them.") aggregates: List[String],
      @description("Projects on this one's classpath (`dependsOn`).") dependsOn: List[ProjectDependency],
  )
  object ProjectInfo { given Schema[ProjectInfo] = DeriveSchema.gen[ProjectInfo] }

  final case class ProjectsResult(currentProject: String, projects: List[ProjectInfo])
  object ProjectsResult { given Schema[ProjectsResult] = DeriveSchema.gen[ProjectsResult] }

  final case class ProjectTests(project: String, tests: List[String])
  object ProjectTests { given Schema[ProjectTests] = DeriveSchema.gen[ProjectTests] }

  final case class TestNamesResult(projects: List[ProjectTests])
  object TestNamesResult { given Schema[TestNamesResult] = DeriveSchema.gen[TestNamesResult] }

  final case class ProjectMainClasses(
      project: String,
      @description("Every main class found in the compiled sources.") mainClasses: List[String],
      @description("The class `run` uses: the configured `mainClass`, or the only discovered one.") defaultMainClass: Option[String],
  )
  object ProjectMainClasses { given Schema[ProjectMainClasses] = DeriveSchema.gen[ProjectMainClasses] }

  final case class MainClassesResult(projects: List[ProjectMainClasses])
  object MainClassesResult { given Schema[MainClassesResult] = DeriveSchema.gen[MainClassesResult] }

  final case class DeclaredModule(
      organization: String,
      name: String,
      revision: String,
      @description("The configurations it is declared for, e.g. `test`; absent means compile.") configurations: Option[String],
  )
  object DeclaredModule { given Schema[DeclaredModule] = DeriveSchema.gen[DeclaredModule] }

  final case class ResolvedModule(
      organization: String,
      @description("Artifact name, including any Scala version suffix.") name: String,
      revision: String,
      @description("Declared by this project (otherwise transitive).") direct: Boolean,
  )
  object ResolvedModule { given Schema[ResolvedModule] = DeriveSchema.gen[ResolvedModule] }

  final case class EvictedModule(organization: String, name: String, revision: String)
  object EvictedModule { given Schema[EvictedModule] = DeriveSchema.gen[EvictedModule] }

  final case class DependenciesResult(
      project: String,
      configuration: String,
      @description("`libraryDependencies` as declared in the build.") declared: List[DeclaredModule],
      @description("Every module on the configuration's resolved classpath.") resolved: List[ResolvedModule],
      @description("Versions that lost a conflict to a newer one.") evicted: List[EvictedModule],
  )
  object DependenciesResult { given Schema[DependenciesResult] = DeriveSchema.gen[DependenciesResult] }

  final case class ShowResult(
      @description("The resolved key, relative to the current project.") key: String,
      @description("`setting` or `task`.") kind: String,
      @description("A setting's type as sbt reports it; `unknown` for a task.") valueType: String,
      @description("The value as text (at most 20000 characters): a setting's JSON or string, a task's `show` output.") value: String,
      @description("A setting's value as JSON, when sbt can write its type as JSON. Never set for a task.")
      json: Option[Json],
  )
  object ShowResult { given Schema[ShowResult] = DeriveSchema.gen[ShowResult] }

  // ---- tools ----

  def all(run: Run, reportEvery: Duration = ReportEvery): List[McpToolHandler] =
    def tool[In: McpInput, Out: Schema](definition: McpTool, run: Run, name: String)(toArgs: In => Map[String, List[String]]) =
      BuildTools.tool[In, Out](definition, run, name, reportEvery)(toArgs)
    def noArgTool[Out: Schema](definition: McpTool, run: Run, name: String) =
      BuildTools.noArgTool[Out](definition, run, name, reportEvery)
    List(
      tool[CompileArgs, ActionResult](
        McpTool("compile")
          .description(
            "Compile a project (and what it aggregates) and return the compiler's errors and warnings as data " +
              "(file, line, column, message), plus status, timing and the captured log. Incremental: only changed " +
              "sources are recompiled, so warnings from unchanged files are not reported again. Runs on sbt's command " +
              "loop, after any running command."
          )
          .annotations(readOnly = OptBool.False, destructive = OptBool.False, idempotent = OptBool.True, openWorld = OptBool.False),
        run,
        "compile",
      )(a => opt("project", a.project) ++ opt("configuration", a.configuration.map(_.toString))),
      tool[TestArgs, TestResult](
        McpTool("test")
          .description(
            "Run tests and return the results as data: pass/fail counts, each failed test's suite, name and " +
              "message, compiler diagnostics if the tests didn't compile, and the captured log. Without arguments " +
              "it runs sbt 2's incremental `test` (only failed, new or affected tests); set `full` to run all, or " +
              "`tests` to run specific classes."
          )
          .annotations(readOnly = OptBool.False, destructive = OptBool.False, idempotent = OptBool.False, openWorld = OptBool.True),
        run,
        "test",
      )(a =>
        opt("project", a.project) ++ list("tests", a.tests) ++ opt("full", a.full.map(_.toString)) ++
          list("frameworkArgs", a.frameworkArgs)
      ),
      tool[RunArgs, ActionResult](
        McpTool("run")
          .description(
            "Run a project's main class in a forked JVM with program arguments, and return the program's " +
              "output (stdout and stderr), status, compiler diagnostics and the captured log. Uses the project's " +
              "`run` settings (main class, classpath, javaOptions, envVars, working directory), with stdin " +
              "disconnected. It blocks sbt until the program exits, so don't use it for servers."
          )
          .annotations(readOnly = OptBool.False, destructive = OptBool.True, idempotent = OptBool.False, openWorld = OptBool.True),
        run,
        "run",
      )(a => opt("project", a.project) ++ opt("mainClass", a.mainClass) ++ a.args.map(v => "args" -> v).toMap),
      tool[ProjectArgs, ActionResult](
        McpTool("clean")
          .description("Delete a project's build outputs (and those of the projects it aggregates), forcing a full recompile next time.")
          .annotations(readOnly = OptBool.False, destructive = OptBool.True, idempotent = OptBool.True, openWorld = OptBool.False),
        run,
        "clean",
      )(a => opt("project", a.project)),
      tool[ProjectArgs, PublishLocalResult](
        McpTool("publish-local")
          .description(
            "Publish a project (and what it aggregates) to the local Ivy repository, so other local builds can " +
              "depend on it. Returns the published modules' coordinates, plus status, diagnostics and the log."
          )
          .annotations(readOnly = OptBool.False, destructive = OptBool.True, idempotent = OptBool.True, openWorld = OptBool.False),
        run,
        "publish-local",
      )(a => opt("project", a.project)),
      noArgTool[ProjectsResult](
        McpTool("projects")
          .description(
            "List the build's projects: id, base directory, name/organization/version, Scala version, which " +
              "project is current, and how they aggregate and depend on each other. Reads the loaded build only " +
              "(nothing runs), and answers even while sbt is busy."
          )
          .annotations(readOnly = OptBool.True, idempotent = OptBool.True, openWorld = OptBool.False),
        run,
        "projects",
      ),
      tool[ProjectArgs, TestNamesResult](
        McpTool("test-names")
          .description(
            "List the test classes defined in a project (and the projects it aggregates), for use with the `test` " +
              "tool's `tests`. Compiles the test sources first if needed."
          )
          .annotations(readOnly = OptBool.True, idempotent = OptBool.True, openWorld = OptBool.False),
        run,
        "test-names",
      )(a => opt("project", a.project)),
      tool[ProjectArgs, MainClassesResult](
        McpTool("main-classes")
          .description(
            "List the main classes of a project (and the projects it aggregates) and the one `run` would use, for " +
              "the `run` tool's `mainClass`. Compiles the main sources first if needed."
          )
          .annotations(readOnly = OptBool.True, idempotent = OptBool.True, openWorld = OptBool.False),
        run,
        "main-classes",
      )(a => opt("project", a.project)),
      tool[DependenciesArgs, DependenciesResult](
        McpTool("dependencies")
          .description(
            "Report a project's library dependencies: as declared (`libraryDependencies`), as resolved for a " +
              "configuration's classpath (with direct/transitive), and the versions evicted by conflicts. " +
              "Resolves dependencies first if needed (may download)."
          )
          .annotations(readOnly = OptBool.True, idempotent = OptBool.True, openWorld = OptBool.True),
        run,
        "dependencies",
      )(a => opt("project", a.project) ++ opt("configuration", a.configuration.map(_.toString))),
      tool[ShowArgs, ShowResult](
        McpTool("show")
          .description(
            "Return the value of an sbt setting, or run a task and return its result. A setting is read without " +
              "running anything, as JSON when sbt can write its type as JSON, and only for exactly the resolved key. " +
              "A task (or a setting sbt can't write as JSON) runs as sbt's `show <key>`: its result is that command's " +
              "printed output, as text only, and an aggregating project also prints the projects it aggregates. " +
              "Running a task runs its side effects. Input tasks (that take arguments) need `sbt-task`."
          )
          .annotations(readOnly = OptBool.False, destructive = OptBool.True, idempotent = OptBool.False, openWorld = OptBool.True),
        run,
        "show",
      )(a => Map("key" -> List(a.key))),
    )

  private def opt(name: String, value: Option[String]): Map[String, List[String]] =
    value.map(v => name -> List(v)).toMap

  private def list(name: String, value: Option[List[String]]): Map[String, List[String]] =
    value.map(v => name -> v).toMap

  /** A call still running after this continues as an MCP task, for a client that handles tasks. */
  val TaskAfter: Duration = 10.seconds

  /** How often a running call reports what sbt is doing (a task's `statusMessage`). */
  val ReportEvery: Duration = 5.seconds

  /**
   * How often a client is asked to poll a task: every second at first, then 20% less
   * often each poll, up to every 30 s, so a long `testFull` isn't polled every second.
   */
  val PollSchedule: Schedule[Any, Any, Any] = Schedule.exponential(1.second, 1.2) || Schedule.spaced(30.seconds)

  /**
   * `tool`, answered as an MCP task once it has run for `after`, for a client that
   * declared the Tasks extension: the client follows it in the background, and the
   * agent goes on meanwhile. A quicker call, and every call of other clients, is
   * answered as before.
   */
  def asTask[R](tool: McpToolHandlerR[R], after: Duration = TaskAfter): McpToolHandlerR[R] =
    val policy = TaskPolicy(TaskExecution.WhenSlow(after), pollInterval = 1.second, pollSchedule = Some(PollSchedule))
    new McpToolHandlerR[R]:
      def name: ToolName                = tool.name
      def definition: ToolDefinition    = tool.definition
      override def requiredScopes       = tool.requiredScopes
      override def taskPolicy           = policy
      def call(args: Option[Json.Obj])  = tool.call(args)
      override def callWithContext(args: Option[Json.Obj], ctx: McpToolContext) = tool.callWithContext(args, ctx)

  private def tool[In: McpInput, Out: Schema](definition: McpTool, run: Run, name: String, reportEvery: Duration)(
      toArgs: In => Map[String, List[String]]
  ): McpToolHandler =
    definition.handleWithContext[Any, String, In, Validated[Out]] { (in: In, ctx: McpToolContext) =>
      call[Out](run, name, toArgs(in), ctx, reportEvery)
    }

  private def noArgTool[Out: Schema](definition: McpTool, run: Run, name: String, reportEvery: Duration): McpToolHandler =
    definition.handleWithContext[Any, String, Validated[Out]]((ctx: McpToolContext) => call[Out](run, name, Map.empty, ctx, reportEvery))

  /**
   * A result object decoded into `Out` and re-encoded, so it has exactly the fields of
   * `Out`'s schema (a missing list becomes `[]`). Absent optional fields are dropped
   * rather than written as `null` (zio-schema's encoding), which the advertised
   * `outputSchema` (e.g. `"error": {"type": "string"}`) doesn't allow.
   */
  final case class Validated[Out](json: Json.Obj)

  given [Out](using schema: Schema[Out]): McpOutput[Validated[Out]] with
    val outputSchema: Option[Json.Obj] = Some(JsonSchemaGen.fromSchema(schema))
    def toResult(output: Validated[Out]): CallToolResult =
      CallToolResult(content = Chunk(ToolContent.text(output.json.toJson)), structuredContent = Some(output.json))

  private def dropNulls(json: Json): Json = json match
    case Json.Obj(fields) => Json.Obj(fields.collect { case (k, v) if v.asNull.isEmpty => k -> dropNulls(v) })
    case Json.Arr(items)  => Json.Arr(items.map(dropNulls))
    case other            => other

  private def call[Out](run: Run, name: String, args: Map[String, List[String]], ctx: McpToolContext, reportEvery: Duration)(using
      schema: Schema[Out]
  ): IO[String, Validated[Out]] =
    // Interruptible: a cancelled call (`notifications/cancelled`) or task interrupts the waiting thread.
    Activity.reporting(ctx, reportEvery)(run(name, args))
      .mapError(e => s"$name: ${Option(e.getMessage).getOrElse(e.toString)}")
      .flatMap { text =>
        if text.startsWith("{") then
          ZIO.fromEither(
            for
              value   <- SchemaJsonCodec.jsonDecoder(schema).decodeJson(text)
              encoded <- SchemaJsonCodec.jsonEncoder(schema).encodeJson(value, None).toString.fromJson[Json]
              obj     <- dropNulls(encoded).asObject.toRight("not a JSON object")
            yield Validated[Out](obj)
          ).mapError(e => s"$name: unexpected result ($e): $text")
        else ZIO.fail(text)
      }
end BuildTools
