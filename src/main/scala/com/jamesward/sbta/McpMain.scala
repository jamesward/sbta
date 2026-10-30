package com.jamesward.sbta

import com.jamesward.sbta.BuildTools
import com.jamesward.ziohttp.mcp.*
import com.jamesward.ziohttp.mcp.McpInput.given
import com.jamesward.ziohttp.mcp.McpOutput.given
import com.jamesward.ziohttp.mcp.McpError.given
import com.jamesward.ziohttp.mcp.client.{ McpClient, McpClientConfig }
import zio.*
import zio.http.Client
import zio.json.ast.Json
import zio.schema.{ DeriveSchema, Schema }
import zio.schema.annotation.description

import java.io.{ FileOutputStream, PrintStream }
import java.nio.file.{ Files, Path }
import java.time.LocalTime
import java.time.format.DateTimeFormatter
import scala.util.control.NonFatal

/**
 * `sbta --mcp`: an MCP server on stdio for the build in the current directory. It
 * answers `initialize` and `tools/list` itself, so clients connect right away, and
 * reaches the build through sbt's own server (see [[SbtServer]]), starting it when
 * needed. Cancelling a tool call cancels the sbt command it is running.
 */
object McpMain:

  /** `--setup <clients>` or `--setup=<clients>`, if given. */
  private def setupClients(args: List[String]): Option[Either[String, List[Setup.Client]]] =
    args.indexWhere(a => a == "--setup" || a.startsWith("--setup=")) match
      case -1 => None
      case i =>
        // `--setup kiro,claude` or `--setup kiro claude` (cmd.exe splits arguments at commas, too).
        val following = args.drop(i + 1).takeWhile(!_.startsWith("--"))
        val attached  = if args(i).startsWith("--setup=") then List(args(i).stripPrefix("--setup=")) else Nil
        val value     = Option((attached ++ following).mkString(",")).filter(_.nonEmpty)
        Some(value.toRight(s"--setup needs the MCP client(s): ${Setup.Client.values.map(_.id).mkString(", ")}")
          .flatMap(Setup.Client.parse))

  final case class SbtTaskArgs(
      @description("An sbt command line, as typed at the sbt shell: `reload`, `scalafmtAll`, `clean; compile`, `core / Test / console`…")
      command: String
  )
  object SbtTaskArgs { given Schema[SbtTaskArgs] = DeriveSchema.gen[SbtTaskArgs] }

  final case class ListTasksArgs(
      @description("Include tasks without a description.") all: Option[Boolean] = None,
      @description("Return detailed help (`help <task>`) for one task instead of the list.") task: Option[String] = None,
  )
  object ListTasksArgs { given Schema[ListTasksArgs] = DeriveSchema.gen[ListTasksArgs] }

  def run(args: List[String]): Unit =
    setupClients(args).foreach { clients =>
      val base = args.collectFirst { case a if a.startsWith("--project-dir=") => Path.of(a.stripPrefix("--project-dir=")) }
        .getOrElse(Path.of("")).toAbsolutePath.normalize
      clients match
        case Left(error) =>
          java.lang.System.err.println(s"sbta: $error")
          sys.exit(2)
        case Right(cs) =>
          try
            Setup.run(base, cs).foreach(line => println(s"sbta: $line"))
            println(
              s"""sbta: the MCP server `${Setup.ServerName}` runs `./sbta --mcp`; restart or reconnect the MCP client to load it.
                 |sbta: add this to the project's AGENTS.md (create it if missing; merge with an existing section of the same name):
                 |
                 |${Setup.agentsGuidance}""".stripMargin
            )
            sys.exit(0)
          catch
            case e: IllegalArgumentException =>
              java.lang.System.err.println(s"sbta: ${e.getMessage}")
              sys.exit(1)
    }
    if SbtServer.windows then
      // sbt's server is a named pipe on Windows; sbta speaks to its Unix domain socket only.
      java.lang.System.err.println("sbta: --mcp is not supported on Windows yet (sbt's server there is a named pipe)")
      sys.exit(2)
    var base    = Path.of("").toAbsolutePath
    var start   = StartMode.Eager
    var docsUrl = Option("https://www.javadocs.dev/mcp")
    var resultNotes = true
    args.foreach {
      case a if a.startsWith("--setup") => ()
      case "--no-update-notice"                => resultNotes = false
      case a if a.startsWith("--project-dir=") => base = Path.of(a.stripPrefix("--project-dir=")).toAbsolutePath.normalize
      case "--start=eager"                     => start = StartMode.Eager
      case "--start=lazy"                      => start = StartMode.Lazy
      case "--start=never"                     => start = StartMode.Never
      case a if a.startsWith("--docs-url=")    =>
        val url = a.stripPrefix("--docs-url=").trim
        docsUrl = if url.isEmpty || url == "none" then None else Some(url)
      case other =>
        java.lang.System.err.println(s"sbta: unknown --mcp option $other")
        sys.exit(2)
    }

    // stdout carries MCP messages only: diagnostics go to stderr and to project/target/sbta/mcp.log.
    val logFile = openLog(base)
    def log(message: String): Unit =
      val line = s"${LocalTime.now.format(DateTimeFormatter.ofPattern("HH:mm:ss"))} sbta: $message"
      java.lang.System.err.println(line)
      logFile.foreach { out => out.synchronized { out.println(line); out.flush() } }

    log(s"MCP server ${SbtaBuildInfo.version} for $base (start=${start.toString.toLowerCase}, java ${java.lang.System.getProperty("java.version")})")
    val server = SbtServer(base, start, log)
    val tools  = SbtTools(server, log)
    val code   = CodeTools(server, log)

    if start == StartMode.Eager then
      val booting = Thread(() => { try server.ensureRunning() catch case NonFatal(e) => log(s"starting sbt failed: ${e.getMessage}"); () })
      booting.setDaemon(true)
      booting.start()

    val mcp: McpServer[Client] =
      val updates = UpdateNotes(base, resultNotes, log, buildNote = () => server.brokenBuildNote)
      val core = McpServer("sbta", SbtaBuildInfo.version)
        .instructions(InstructionsSource(_ => ZIO.succeed(Some(Instructions + updates.instructionsNote.fold("")(" " + _)))))
      val handlers =
        List(sbtTaskTool(tools), listTasksTool(tools), globSearchTool(code), inspectTool(code), locationTool(code), checkTool(code)) ++
          BuildTools.all(tools.run) :+ updateTool(updates)
      val withBuildTools = handlers.map(updates.withNote).foldLeft(core)(_.tool(_))
      docsUrl match
        case Some(url) => withBuildTools.toolSource(updates.withNote(ProxyToolSource(url, log)))
        case None      => withBuildTools

    val runtime = Unsafe.unsafe(implicit u => Runtime.unsafe.fromLayer(Runtime.removeDefaultLoggers))
    Unsafe.unsafe { implicit u =>
      runtime.unsafe.run(mcp.serveStdio.provide(Client.default)).getOrThrowFiberFailure()
    }
    log("stdin closed; exiting (sbt keeps running)")
    sys.exit(0)

  private def openLog(base: Path): Option[PrintStream] =
    try
      val dir = base.resolve("project").resolve("target").resolve("sbta")
      Files.createDirectories(dir)
      Some(PrintStream(FileOutputStream(dir.resolve("mcp.log").toFile, true), true, "UTF-8"))
    catch case NonFatal(_) => None

  private val Instructions =
    "Tools for driving this Scala/sbt build through its sbt server. Structured build tools return data: " +
      "`compile`, `test`, `run`, `clean`, `publish-local`, `projects`, `test-names`, `main-classes`, " +
      "`dependencies` and `show`. `sbt-task` runs any other sbt command, and `list-tasks` lists the tasks. " +
      "Search/inspect Scala 3 symbols read from TASTy (`glob-search`, `inspect`, `symbol-location`), and `check` " +
      "Scala 3 sources quickly without compiling. Commands run on sbt's command loop " +
      "one at a time (shared with any other sbt client); cancelling a call cancels its command. " +
      "Documentation tools are proxied from javadocs.dev. `update-sbta` updates the build's sbta scripts."

  /**
   * The notes appended (as more text items, never in `structuredContent`) to every tool
   * result: that the build files don't load (`buildNote`, always), and that a newer sbta
   * release is out (unless `resultNotes` is off; also in the instructions). The update
   * note compares the version the build's `./sbta` pins (so it stops once `update-sbta`
   * has run) with the newest release from the daily check, which is refreshed in the
   * background when the server starts.
   */
  private[sbta] final class UpdateNotes(
      base: Path,
      resultNotes: Boolean,
      log: String => Unit,
      cache: Path = Updates.cacheFile,
      env: String => Option[String] = sys.env.get,
      buildNote: () => Option[String] = () => None,
  ):
    @volatile private var checked = Updates.read(cache)
    if !Updates.checkDisabled(env) then
      Updates.refreshInBackground(cache) { c =>
        checked = c
        c.flatMap(_.latest).foreach(v => log(s"newest sbta release: $v"))
      }

    def current: String = Updates.pinnedVersion(base).getOrElse(SbtaBuildInfo.version)

    def note: Option[String] =
      if Updates.noticesDisabled(env) then None
      else Updates.newer(current, checked).map(Updates.mcpNote(current, _))

    def instructionsNote: Option[String] = note

    def addNote(result: CallToolResult): CallToolResult =
      // A broken build is always noted (unless the result says so already); the update note can be turned off.
      val build = buildNote().filterNot(_ => result.content.exists {
        case ToolContent.Text(t, _) => t.contains("the build failed to load")
        case _                      => false
      })
      val notes = build.toList ++ (if resultNotes then note.toList else Nil)
      if notes.isEmpty then result else result.copy(content = result.content ++ notes.map(ToolContent.text))

    def withNote[R](tool: McpToolHandlerR[R]): McpToolHandlerR[R] =
      new McpToolHandlerR[R]:
        def name: ToolName                = tool.name
        def definition: ToolDefinition    = tool.definition
        override def requiredScopes       = tool.requiredScopes
        override def taskPolicy           = tool.taskPolicy
        def call(args: Option[Json.Obj])  = tool.call(args).map(addNote)
        override def callWithContext(args: Option[Json.Obj], ctx: McpToolContext) =
          tool.callWithContext(args, ctx).map(addNote)

    def withNote[R](source: McpToolSource[R]): McpToolSource[R] =
      new McpToolSource[R]:
        def listTools(ctx: McpToolContext) = source.listTools(ctx)
        def callTool(name: ToolName, args: Option[Json.Obj], ctx: McpToolContext) =
          source.callTool(name, args, ctx).map(addNote)

    def update(version: Option[String]): Either[String, List[String]] =
      Updates.update(
        base,
        version,
        Updates.releasesBase(env),
        () => Updates.latest(),
        deferBat = false,
      )

  final case class UpdateArgs(
      @description("The release to update to, e.g. `1.2.3`. Default: the newest release.") version: Option[String] = None
  )
  object UpdateArgs { given Schema[UpdateArgs] = DeriveSchema.gen[UpdateArgs] }

  private def updateTool(updates: UpdateNotes): McpToolHandler =
    McpTool("update-sbta")
      .description(
        "Update this build's sbta launcher scripts (`sbta` and `sbta.bat`, in the build root) to the newest sbta " +
          "release (or `version`), checked against the release's SHA256SUMS. Only when the user agrees: it changes " +
          "files to commit. The running MCP server keeps its version until the MCP client restarts it."
      )
      .annotations(destructive = OptBool.True, openWorld = OptBool.True)
      .handle[Any, Throwable, UpdateArgs, String] { (args: UpdateArgs) =>
        ZIO.attemptBlockingInterrupt(updates.update(args.version)).flatMap {
          case Right(lines) => ZIO.succeed(lines.mkString("\n"))
          case Left(error)  => ZIO.fail(RuntimeException(error))
        }
      }

  final case class GlobSearchArgs(
      @description("An unqualified name or glob (`*Service`, `Foo*`); `*` (or empty) with `inPackage` lists the package.") query: String,
      @description("Restrict to a package, e.g. `com.example` (recommended).") inPackage: Option[String] = None,
  )
  object GlobSearchArgs { given Schema[GlobSearchArgs] = DeriveSchema.gen[GlobSearchArgs] }

  final case class SymbolArgs(@description("A fully-qualified name, e.g. `com.example.Service`.") symbol: String)
  object SymbolArgs { given Schema[SymbolArgs] = DeriveSchema.gen[SymbolArgs] }

  final case class CheckArgs(
      @description("Source paths, absolute or relative to the build root, all of one module (project + Compile/Test).") files: List[String],
      @description("Unsaved content to check instead of the file on disk (exactly one file).") content: Option[String] = None,
      @description("`files` (default): the files, plus the module's sources changed since its last compile; `module`: every source of the module.")
      scope: Option[String] = None,
  )
  object CheckArgs { given Schema[CheckArgs] = DeriveSchema.gen[CheckArgs] }

  private def globSearchTool(code: CodeTools): McpToolHandler =
    McpTool("glob-search")
      .description(
        "Search Scala 3 symbols by unqualified name in this build's projects (read from their TASTy). Dependency " +
          "jars are used to resolve types and can be inspected exactly with `inspect`/`symbol-location`, but are not " +
          "enumerated here. Set `inPackage` to restrict to a package; `query`=\"*\" (or empty) with `inPackage` LISTS " +
          "ALL symbols in it, e.g. {\"query\":\"*\",\"inPackage\":\"com.example\"}. Returns each match's kind and " +
          "fully-qualified name. The project is compiled first (unless sbt is busy) so new symbols appear."
      )
      .annotations(readOnly = OptBool.True)
      .handle[Any, Throwable, GlobSearchArgs, String] { (args: GlobSearchArgs) =>
        ZIO.attemptBlockingInterrupt(code.globSearch(args.query, args.inPackage))
      }

  private def inspectTool(code: CodeTools): McpToolHandler =
    McpTool("inspect")
      .description(
        "Inspect a Scala 3 symbol by fully-qualified name: its members and (approximate) signatures. Works for " +
          "classes, objects and traits of this build and its dependencies."
      )
      .annotations(readOnly = OptBool.True)
      .handle[Any, Throwable, SymbolArgs, String] { (args: SymbolArgs) =>
        ZIO.attemptBlockingInterrupt(code.inspect(args.symbol))
      }

  private def locationTool(code: CodeTools): McpToolHandler =
    McpTool("symbol-location")
      .description(
        "Return the source location (`path:line`) of a Scala 3 symbol (a class, object, trait, or a top-level/static " +
          "term), read from its defining tree in TASTy."
      )
      .annotations(readOnly = OptBool.True)
      .handle[Any, Throwable, SymbolArgs, String] { (args: SymbolArgs) =>
        ZIO.attemptBlockingInterrupt(code.location(args.symbol))
      }

  private def checkTool(code: CodeTools): McpToolHandler =
    McpTool("check")
      .description(
        "Fast syntax + type check of Scala 3 source files WITHOUT compiling: no class files, and it doesn't wait " +
          "behind a running build or `~` watch. It runs the module's own Scala compiler, kept warm in sbta, through " +
          "every checking phase (parser, typer, macro/inline expansion, override and abstract-member checks, " +
          "pattern-match exhaustivity, unused/deprecation warnings) with the module's classpath and scalacOptions, " +
          "stopping before bytecode generation. Use it after each edit; run `compile`/`test` before finishing. " +
          "Sources of the module changed since its last compile are checked along with the files; `scope`: \"module\" " +
          "re-checks every source of the module (after changing an API). Other modules are seen as of their last " +
          "compile. Returns `[ok]`/`[error]`, counts, and diagnostics as `path:line:column: severity: message`."
      )
      .annotations(readOnly = OptBool.True)
      .handle[Any, Throwable, CheckArgs, String] { (args: CheckArgs) =>
        ZIO.attemptBlockingInterrupt(code.check(args.files, args.content, args.scope))
      }

  private def sbtTaskTool(tools: SbtTools): McpToolHandler =
    McpTool("sbt-task")
      .description(
        "Run any sbt command line in this build and return its status (`[ok] <cmd>` or `[error] <cmd>: …`), the " +
          "elapsed time and the captured console output (compiler errors and warnings, test output and failures). " +
          "It runs on sbt's command loop like a command typed at the sbt shell, after any command already running. " +
          "Prefer the structured tools (`compile`, `test`, `run`, …) where one fits. Use this for anything else, " +
          "e.g. `reload`, `set …`, `scalafmtAll`, or chains like `clean; compile`."
      )
      .annotations(destructive = OptBool.True, openWorld = OptBool.True)
      .handle[Any, Throwable, SbtTaskArgs, String] { (args: SbtTaskArgs) =>
        ZIO.attemptBlockingInterrupt(tools.sbtTask(args.command))
      }

  private def listTasksTool(tools: SbtTools): McpToolHandler =
    McpTool("list-tasks")
      .description(
        "List the build's tasks with their descriptions (sbt's `tasks`; `all` for every task, `tasks -V`), or with " +
          "`task`, the detailed help for one task (`help <task>`)."
      )
      .annotations(readOnly = OptBool.True)
      .handle[Any, Throwable, ListTasksArgs, String] { (args: ListTasksArgs) =>
        ZIO.attemptBlockingInterrupt(tools.listTasks(args.all.contains(true), args.task))
      }

  /**
   * Proxies an upstream MCP server (javadocs.dev by default): its tools are merged into
   * `tools/list`, and a call to any other tool name is forwarded. An unreachable upstream
   * gives an empty list or an `isError` result, never a failure of this server.
   */
  private final class ProxyToolSource(url: String, log: String => Unit) extends McpToolSource[Client]:
    private val clientInfo = Implementation("sbta-docs-proxy", SbtaBuildInfo.version)

    def listTools(ctx: McpToolContext): ZIO[Client, Nothing, Chunk[ToolDefinition]] =
      ZIO.scoped(McpClient.connect(McpClientConfig(url, clientInfo = clientInfo)).flatMap(_.listTools))
        .catchAll(e => ZIO.succeed(log(s"docs proxy ($url) listTools failed: $e")).as(Chunk.empty))

    def callTool(name: ToolName, args: Option[Json.Obj], ctx: McpToolContext): ZIO[Client, Nothing, CallToolResult] =
      ZIO.scoped(McpClient.connect(McpClientConfig(url, clientInfo = clientInfo)).flatMap(_.callTool(name.value, args.getOrElse(Json.Obj()))))
        .retry(Schedule.exponential(500.millis) && Schedule.recurs(2))
        .catchAll(e =>
          ZIO.succeed(log(s"docs proxy ($url) callTool '${name.value}' failed: $e")).as(
            CallToolResult(content = Chunk(ToolContent.text(s"proxy to $url failed for '${name.value}': $e")), isError = Some(true))
          )
        )
