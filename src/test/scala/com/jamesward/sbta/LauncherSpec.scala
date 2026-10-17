package com.jamesward.sbta

import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path }
import java.util.concurrent.TimeUnit

/**
 * sbta as a launcher, end to end, on an sbt 1 and an sbt 2 build: the assembled jar
 * (`-Dsbta.jar`) and the generated `sbta` script (`-Dsbta.scripts`), as a user runs
 * them. Without `--mcp`, everything must behave like sbt itself. Needs sbt and Scala
 * from Maven Central (or the local caches) and takes a few minutes.
 */
object LauncherSpec extends ZIOSpecDefault:

  final case class EchoArgs(text: String)
  object EchoArgs { given zio.schema.Schema[EchoArgs] = zio.schema.DeriveSchema.gen[EchoArgs] }

  private val jar     = Path.of(sys.props("sbta.jar"))
  private val scripts = Path.of(sys.props("sbta.scripts"))
  private val version = sys.props("sbta.version")

  final case class Ran(exitCode: Int, output: String)

  private def run(dir: Path, command: List[String], input: String = "", env: Map[String, String] = Map.empty, timeoutSeconds: Long = 300): Ran =
    val out     = Files.createTempFile("sbta-launcher", ".out")
    val builder = ProcessBuilder(command*).directory(dir.toFile).redirectErrorStream(true).redirectOutput(out.toFile)
    // Set by sbt for the processes it forks (this test JVM): sbt in a child would take its
    // terminal from it instead of reading stdin.
    builder.environment().remove("SBT_TERMINAL_PROPS")
    env.foreach((k, v) => builder.environment().put(k, v))
    val process = builder.start()
    val stdin   = process.getOutputStream
    stdin.write(input.getBytes(StandardCharsets.UTF_8))
    stdin.close()
    val finished = process.waitFor(timeoutSeconds, TimeUnit.SECONDS)
    if !finished then
      process.descendants().forEach(_.destroyForcibly())
      process.destroyForcibly()
    val text = Files.readString(out)
    Files.deleteIfExists(out)
    // sbt colours its output on CI (e.g. `\e[0m[\e[0minfo\e[0m]`): compare plain text.
    Ran(if finished then process.exitValue() else -1, Text.stripAnsi(text))

  private def runJar(dir: Path, args: List[String], input: String = ""): Ran =
    run(dir, List(Path.of(java.lang.System.getProperty("java.home"), "bin", "java").toString, "-jar", jar.toString) ++ args, input)

  /** root aggregates mid, which aggregates leaf; app depends on leaf. */
  private def build(sbtVersion: String): Path =
    val dir = Files.createTempDirectory(s"sbta-launcher-$sbtVersion-")
    Files.createDirectories(dir.resolve("project"))
    Files.writeString(dir.resolve("project/build.properties"), s"sbt.version=$sbtVersion\n")
    Files.writeString(
      dir.resolve("build.sbt"),
      """ThisBuild / scalaVersion := "3.3.7"
        |lazy val leaf = project
        |lazy val app  = project.dependsOn(leaf)
        |lazy val mid  = project.aggregate(leaf)
        |lazy val root = (project in file(".")).aggregate(mid, app)
        |""".stripMargin,
    )
    Files.createDirectories(dir.resolve("leaf/src/main/scala"))
    Files.writeString(dir.resolve("leaf/src/main/scala/Leaf.scala"), "package leaf\nobject Leaf { def x = 1 }\n")
    Files.createDirectories(dir.resolve("app/src/main/scala"))
    Files.writeString(dir.resolve("app/src/main/scala/App.scala"), "object App { def main(args: Array[String]): Unit = println(s\"hello ${leaf.Leaf.x} ${args.mkString}\") }\n")
    Files.copy(scripts.resolve("sbta"), dir.resolve("sbta"))
    Files.copy(scripts.resolve("sbta.bat"), dir.resolve("sbta.bat"))
    dir.resolve("sbta").toFile.setExecutable(true)
    dir

  /** A Maven repository holding this sbta jar, for the script to download from. */
  private lazy val repo: Path =
    val dir = Files.createTempDirectory("sbta-repo")
    val at  = dir.resolve(s"com/jamesward/sbta/$version")
    Files.createDirectories(at)
    Files.copy(jar, at.resolve(s"sbta-$version.jar"))
    val sha1 = java.security.MessageDigest.getInstance("SHA-1").digest(Files.readAllBytes(jar)).map(b => f"$b%02x").mkString
    Files.writeString(at.resolve(s"sbta-$version.jar.sha1"), sha1)
    dir

  private val windows = java.lang.System.getProperty("os.name").toLowerCase.contains("win")

  private val localScriptsSource = Path.of(sys.props("sbta.localScripts"))

  /** The local-mode scripts in `dir/local/`, with no jar next to them. */
  private def localScripts(dir: Path): Path =
    val local = dir.resolve("local")
    Files.createDirectories(local)
    List("sbta", "sbta.bat").foreach(f => Files.copy(localScriptsSource.resolve(f), local.resolve(f), java.nio.file.StandardCopyOption.REPLACE_EXISTING))
    Files.deleteIfExists(local.resolve("sbta-local.jar"))
    local

  private def localCommand(local: Path): List[String] =
    if windows then List("cmd", "/c", local.resolve("sbta.bat").toString) else List("bash", local.resolve("sbta").toString)

  /** The launcher script as a user runs it: `sbta.bat` on Windows, `sbta` elsewhere. */
  private def scriptCommand(dir: Path): List[String] =
    if windows then List("cmd", "/c", dir.resolve("sbta.bat").toString) else List("bash", dir.resolve("sbta").toString)

  private def script(dir: Path, args: List[String], input: String = "", timeoutSeconds: Long = 300): Ran =
    run(dir, scriptCommand(dir) ++ args, input, Map("SBTA_LAUNCH_REPO" -> repo.toUri.toString), timeoutSeconds)

  private def suiteFor(sbtVersion: String) =
    val dir = build(sbtVersion)
    suite(s"sbt $sbtVersion")(
      test("a command runs as with sbt, and its exit code is sbt's") {
        val ok  = runJar(dir, List("show leaf/name"))
        val bad = runJar(dir, List("nosuchcommand"))
        assertTrue(ok.exitCode == 0, ok.output.contains("[info] leaf"), bad.exitCode == 1, bad.output.contains("Not a valid command"))
      },
      test("no arguments starts sbt's shell") {
        val shell = runJar(dir, Nil, input = "leaf/name\nexit\n")
        assertTrue(shell.exitCode == 0, shell.output.contains("[info] leaf"))
      },
      test("the script downloads sbta and runs sbt with it") {
        val cached = Path.of(java.lang.System.getProperty("user.home"), ".cache/sbt/boot/sbta", version)
        deleteRecursively(cached)
        val ran = script(dir, List("show leaf/name"))
        assertTrue(ran.exitCode == 0, ran.output.contains("leaf"), Files.isRegularFile(cached.resolve(s"sbta-$version.jar")))
      },
      // (sbt 2's sbtn joins its arguments with spaces, so several commands go in one, with `;`.)
      test("the script passes a command line with spaces and `;` through to sbt") {
        val ran = script(dir, List("show leaf / name; show app / name"))
        assertTrue(ran.exitCode == 0, ran.output.contains("leaf"), ran.output.contains("app"))
      },
      test("the script's --mcp --setup writes the MCP client config (on every OS) and prints the AGENTS.md guidance") {
        val ran    = script(dir, List("--mcp", "--setup", "kiro,claude"), timeoutSeconds = 120)
        val config = Files.readString(dir.resolve(".kiro/settings/mcp.json"))
        assertTrue(
          ran.exitCode == 0,
          config.contains("\"./sbta\""),
          Files.isRegularFile(dir.resolve(".mcp.json")),
          ran.output.contains("## Build, Test & Dev Workflow"),
        )
      },
      test("the script's --mcp says it isn't supported on Windows (sbt's server is a named pipe there)") {
        val ran = script(dir, List("--mcp"), timeoutSeconds = 60)
        assertTrue(ran.exitCode != 0, ran.output.contains("not supported on Windows"))
      } @@ TestAspect.ifProp("os.name")(_.toLowerCase.contains("win")),
      test("the script's --mcp is an MCP server on stdout, starting sbt with the script, and reports aggregation") {
        val requests = List(
          """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"spec","version":"1"}}}""",
          """{"jsonrpc":"2.0","method":"notifications/initialized"}""",
          """{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"projects","arguments":{}}}""",
        )
        val mcp = McpProcess.start(dir, scriptCommand(dir) ++ List("--mcp", "--docs-url=none"), Map("SBTA_LAUNCH_REPO" -> repo.toUri.toString))
        try
          requests.foreach(mcp.send)
          val init     = mcp.response(1, 60)
          val projects = mcp.response(2, 300)
          val aggregates = projects.flatMap(path(_, "result", "structuredContent", "projects")).collect { case Json.Arr(ps) => ps.toList }
            .getOrElse(Nil).flatMap { p =>
              for
                case Json.Str(id) <- path(p, "id")
                case Json.Arr(as) <- path(p, "aggregates")
              yield id -> as.toList.collect { case Json.Str(a) => a }
            }.toMap
          assertTrue(
            init.flatMap(path(_, "result", "serverInfo", "name")).contains(Json.Str("sbta")),
            aggregates == Map("root" -> List("mid", "app"), "mid" -> List("leaf"), "leaf" -> Nil, "app" -> Nil),
            mcp.nonJson.isEmpty,
          )
        finally mcp.close()
      } @@ TestAspect.ifProp("os.name")(!_.toLowerCase.contains("win")),
      test("this build's own (local-mode) script is plain sbt until its jar is built") {
        val local = localScripts(dir)
        val ran   = run(dir, localCommand(local) ++ List("show leaf/name"))
        val mcp   = run(dir, localCommand(local) ++ List("--mcp"), timeoutSeconds = 60)
        assertTrue(ran.exitCode == 0, ran.output.contains("leaf"), mcp.exitCode != 0, mcp.output.contains("isn't built yet"))
      },
      test("this build's own (local-mode) script runs its built jar") {
        val local = localScripts(dir)
        Files.copy(jar, local.resolve("sbta-local.jar"), java.nio.file.StandardCopyOption.REPLACE_EXISTING)
        val init = """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"spec","version":"1"}}}"""
        val args = localCommand(local) ++ List("--mcp", "--start=never", "--docs-url=none")
        val sbtaAnswered =
          // On Windows sbta answers --mcp itself, saying it isn't supported there: still the built jar.
          if windows then run(dir, args, timeoutSeconds = 60).output.contains("not supported on Windows")
          else
            val mcp = McpProcess.start(dir, args, Map.empty)
            try
              mcp.send(init)
              mcp.response(1, 60).flatMap(path(_, "result", "serverInfo", "name")).contains(Json.Str("sbta"))
            finally mcp.close()
        assertTrue(sbtaAnswered)
      },
      everyTool(dir),
    ) @@ TestAspect.sequential @@ TestAspect.afterAll(ZIO.succeed {
      script(dir, List("--client", "shutdown"), timeoutSeconds = 60)
      deleteRecursively(dir)
    })

  /** Every tool, through the shrunk jar's MCP server, with a local upstream for the docs proxy. */
  private def everyTool(dir: Path) =
    test("every MCP tool works from the shrunk jar, including the HTTP client of the docs proxy") {
      import com.jamesward.ziohttp.mcp.*
      import com.jamesward.ziohttp.mcp.McpInput.given
      import com.jamesward.ziohttp.mcp.McpOutput.given
      import zio.http.Server
      val upstream = McpServer("upstream", "1").tool(
        McpTool("echo").description("Echo").handle[Any, Nothing, EchoArgs, String]((a: EchoArgs) => ZIO.succeed(s"echo: ${a.text}"))
      ).mountedAt("/")
      val calls = List(
        "projects"        -> "{}",
        "compile"         -> """{"project":"app"}""",
        "test-names"      -> "{}",
        "main-classes"    -> """{"project":"app"}""",
        "check"           -> """{"files":["app/src/main/scala/App.scala"]}""",
        "glob-search"     -> """{"query":"*","inPackage":"leaf"}""",
        "inspect"         -> """{"symbol":"leaf.Leaf"}""",
        "symbol-location" -> """{"symbol":"leaf.Leaf"}""",
        "dependencies"    -> """{"project":"app"}""",
        "show"            -> """{"key":"app / name"}""",
        "show"            -> """{"key":"app / Compile / mainClass"}""",
        "test"            -> """{"project":"app"}""",
        "run"             -> """{"project":"app","args":["x"]}""",
        "publish-local"   -> """{"project":"leaf"}""",
        "sbt-task"        -> """{"command":"leaf/name"}""",
        "list-tasks"      -> "{}",
        "echo"            -> """{"text":"hi"}""",
        "clean"           -> """{"project":"leaf"}""",
      )
      ZIO.scoped {
        for
          server <- Server.defaultWithPort(0).build
          port   <- Server.install(upstream.statelessRoutes).provideEnvironment(server)
          result <- ZIO.attemptBlocking {
                      val mcp = McpProcess.start(
                        dir,
                        List(Path.of(java.lang.System.getProperty("java.home"), "bin", "java").toString, "-jar", jar.toString, "--mcp", s"--docs-url=http://127.0.0.1:$port/"),
                        // sbta starts sbt with the build's ./sbta, which downloads this version of sbta.
                        Map("SBTA_LAUNCH_REPO" -> repo.toUri.toString),
                      )
                      try
                        mcp.send("""{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"spec","version":"1"}}}""")
                        mcp.send("""{"jsonrpc":"2.0","method":"notifications/initialized"}""")
                        mcp.response(1, 60)
                        mcp.send("""{"jsonrpc":"2.0","id":2,"method":"tools/list"}""")
                        val tools = mcp.response(2, 60).flatMap(path(_, "result", "tools")).collect { case Json.Arr(ts) => ts.toList }
                          .getOrElse(Nil).flatMap(path(_, "name")).collect { case Json.Str(n) => n }
                        val results = calls.zipWithIndex.map { case ((tool, args), i) =>
                          val id = 10 + i
                          mcp.send(s"""{"jsonrpc":"2.0","id":$id,"method":"tools/call","params":{"name":"$tool","arguments":$args}}""")
                          tool -> mcp.response(id, 300)
                        }
                        (tools, results)
                      finally mcp.close()
                    }
        yield
          val (tools, results) = result
          val broken = """NoClassDefFoundError|ClassNotFoundException|NoSuchMethod|NoSuchField|ExceptionInInitializerError""".r
          val failures = results.collect {
            case (tool, None)                                                     => s"$tool: no response"
            case (tool, Some(r)) if path(r, "error").isDefined                    => s"$tool: ${r.toJson}"
            case (tool, Some(r)) if path(r, "result", "isError").contains(Json.Bool(true)) => s"$tool: ${r.toJson.take(500)}"
            case (tool, Some(r)) if broken.findFirstIn(r.toJson).isDefined        => s"$tool: ${r.toJson.take(500)}"
          }
          val text = results.toMap.view.mapValues(_.map(_.toJson).getOrElse("")).toMap
          failures.foreach(f => println(s"tool failed: $f"))
          assertTrue(
            failures.isEmpty,
            tools.contains("echo"), // the upstream's tools, listed through the HTTP client
            text("echo").contains("echo: hi"),
            text("run").contains("hello 1 x"),
            text("glob-search").contains("leaf.Leaf"),
          )
      }
    } @@ TestAspect.ifProp("os.name")(!_.toLowerCase.contains("win"))

  private def path(json: Json, fields: String*): Option[Json] =
    fields.foldLeft(Option(json))((j, f) => j.flatMap(_.asObject).flatMap(_.get(f)))

  private def deleteRecursively(dir: Path): Unit =
    if Files.exists(dir) then
      scala.util.Using.resource(Files.walk(dir))(_.sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p)))

  def spec = suite("sbta as a launcher")(suiteFor("1.12.11"), suiteFor("2.0.10")) @@ TestAspect.sequential @@
    TestAspect.withLiveClock @@ TestAspect.afterAll(ZIO.succeed(deleteRecursively(repo)))

  /** A newline-delimited JSON-RPC subprocess (an MCP stdio server). */
  final class McpProcess private (process: Process):
    private val responses = java.util.concurrent.ConcurrentHashMap[String, Json]()
    @volatile var nonJson: List[String] = Nil
    private val reader = Thread { () =>
      val in = java.io.BufferedReader(java.io.InputStreamReader(process.getInputStream, StandardCharsets.UTF_8))
      Iterator.continually(in.readLine()).takeWhile(_ != null).filter(_.trim.nonEmpty).foreach { line =>
        line.fromJson[Json] match
          case Right(json) => path(json, "id").foreach(id => responses.put(id.toJson, json))
          case Left(_)     => nonJson = line :: nonJson
      }
    }
    reader.setDaemon(true)
    reader.start()

    def send(line: String): Unit =
      process.getOutputStream.write((line + "\n").getBytes(StandardCharsets.UTF_8))
      process.getOutputStream.flush()

    def response(id: Int, timeoutSeconds: Long): Option[Json] =
      val deadline = java.lang.System.nanoTime() + TimeUnit.SECONDS.toNanos(timeoutSeconds)
      while !responses.containsKey(id.toString) && java.lang.System.nanoTime() < deadline do Thread.sleep(100)
      Option(responses.get(id.toString))

    def close(): Unit =
      process.getOutputStream.close()
      if !process.waitFor(20, TimeUnit.SECONDS) then process.destroyForcibly()

  object McpProcess:
    def start(dir: Path, command: List[String], env: Map[String, String]): McpProcess =
      val builder = ProcessBuilder(command*).directory(dir.toFile).redirectError(ProcessBuilder.Redirect.DISCARD)
      builder.environment().remove("SBT_TERMINAL_PROPS")
      env.foreach((k, v) => builder.environment().put(k, v))
      McpProcess(builder.start())
