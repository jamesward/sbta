package com.jamesward.sbta

import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

import java.io.{ BufferedInputStream, EOFException, InputStream, OutputStream }
import java.net.{ StandardProtocolFamily, UnixDomainSocketAddress }
import java.nio.channels.{ Channels, ServerSocketChannel, SocketChannel }
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path }

object SbtaSpec extends ZIOSpecDefault:

  // ---- a fake sbt server: LSP framing on a Unix domain socket ----

  private def readMessage(in: InputStream): Json =
    def line(): String =
      val b = StringBuilder()
      var c = in.read()
      while c != -1 && c != '\n' do { if c != '\r' then b.append(c.toChar); c = in.read() }
      if c == -1 then throw EOFException()
      b.toString
    var length = -1
    var l      = line()
    while l.nonEmpty do
      if l.toLowerCase.startsWith("content-length:") then length = l.drop(15).trim.toInt
      l = line()
    String(in.readNBytes(length), StandardCharsets.UTF_8).fromJson[Json].toOption.get

  private def write(out: OutputStream, message: String): Unit = out.synchronized {
    val body = message.getBytes(StandardCharsets.UTF_8)
    out.write(s"Content-Length: ${body.length}\r\n\r\n".getBytes(StandardCharsets.US_ASCII))
    out.write(body)
    out.flush()
  }

  private def field(m: Json, name: String): Option[Json] = m.asObject.flatMap(_.get(name))
  private def str(m: Json, name: String): String         = field(m, name).collect { case Json.Str(s) => s }.getOrElse("")

  /** Serves one connection: what a real sbt does for initialize, attach, exec and cancel. Returns the socket and the requests seen. */
  private def fakeSbt(dir: Path, dropFirstAttach: Boolean = false): (Path, java.util.concurrent.ConcurrentLinkedQueue[String]) =
    val socket = dir.resolve("sock")
    val seen   = java.util.concurrent.ConcurrentLinkedQueue[String]()
    val server = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
    server.bind(UnixDomainSocketAddress.of(socket))
    val thread = Thread { () =>
      val channel = server.accept()
      val in      = BufferedInputStream(Channels.newInputStream(channel))
      val out     = Channels.newOutputStream(channel)
      var running: Option[String] = None
      var dropAttach              = dropFirstAttach
      try
        while true do
          val m      = readMessage(in)
          val method = str(m, "method")
          val id     = field(m, "id").map(_.toJson).getOrElse("null")
          if method.nonEmpty then seen.add(method)
          method match
            case "initialize" => write(out, s"""{"jsonrpc":"2.0","id":$id,"result":{"capabilities":{}}}""")
            case "sbt/attach" if dropAttach =>
              // As sbt 1 when the attach is read before the new channel is registered: no answer.
              dropAttach = false
            case "sbt/attach" =>
              // sbt asks the attached client about its terminal before answering.
              write(out, """{"jsonrpc":"2.0","id":"q1","method":"sbt/terminalPropertiesQuery","params":""}""")
              val answer = readMessage(in)
              seen.add("answered:" + field(answer, "result").flatMap(r => field(r, "isAnsiSupported")).map(_.toJson).getOrElse("?"))
              write(out, s"""{"jsonrpc":"2.0","id":$id,"result":""}""")
            case "sbt/exec" =>
              val command = str(field(m, "params").get, "commandLine")
              val bytes   = s"[info] running $command\n\u001b[31mred\u001b[0m\n".getBytes(StandardCharsets.UTF_8).map(_.toInt).mkString(",")
              write(out, s"""{"jsonrpc":"2.0","method":"sbt/systemOut","params":[$bytes]}""")
              write(out,
                s"""{"jsonrpc":"2.0","method":"build/publishDiagnostics","params":{"textDocument":{"uri":"${dir.resolve("A.scala").toUri}"},
                   |"buildTarget":{"uri":"x"},"diagnostics":[{"range":{"start":{"line":2,"character":4},"end":{"line":2,"character":6}},
                   |"severity":1,"message":"Found: \\u001b[32mString\\u001b[0m"}],"reset":true}}""".stripMargin.replace("\n", ""))
              command match
                case "fail" => write(out, s"""{"jsonrpc":"2.0","id":$id,"error":{"code":-32603,"message":"(Compile / compile) Compilation failed"}}""")
                case "slow" => running = Some(id) // answered when cancelled
                case "reload-broken" =>
                  // As sbt's doLoadFailed: the error and the prompt, then a read of the channel's stdin.
                  val prompt = s"[error] build.sbt:3: error: not found: noSuchSetting\n[warn] ${Session.LoadFailedPrompt}\n"
                    .getBytes(StandardCharsets.UTF_8).map(_.toInt).mkString(",")
                  write(out, s"""{"jsonrpc":"2.0","method":"sbt/systemOut","params":[$prompt]}""")
                  write(out, """{"jsonrpc":"2.0","method":"sbt/readSystemIn","params":""}""")
                  running = Some(id)
                case _      => write(out, s"""{"jsonrpc":"2.0","id":$id,"result":{"status":"Done","exitCode":0}}""")
            case "sbt/systemIn" =>
              val byte = field(m, "params").collect { case Json.Num(n) => n.intValue.toChar }.getOrElse('?')
              seen.add(s"systemIn:$byte")
              // (i)gnore: sbt keeps the previous build and the command completes.
              if byte == 'i' then running.foreach(r => write(out, s"""{"jsonrpc":"2.0","id":$r,"result":{"status":"Done","exitCode":0}}"""))
            case "sbt/cancelRequest" =>
              write(out, s"""{"jsonrpc":"2.0","id":$id,"result":{"status":"Task cancelled"}}""")
              running.foreach(r => write(out, s"""{"jsonrpc":"2.0","id":$r,"error":{"code":-33000,"message":"slow"}}"""))
            case _ => ()
      catch case _: Exception => ()
      finally server.close()
    }
    thread.setDaemon(true)
    thread.start()
    (socket, seen)

  private def withBuild[A](f: Path => A): A =
    val dir = Files.createTempDirectory("sbta-spec")
    try f(dir)
    finally
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))

  private def session(dir: Path, socket: Path): Session =
    Session.open(SbtServer(dir, StartMode.Never, _ => ()), socket, _ => ())

  def spec = suite("sbta")(
    test("a session attaches as a plain terminal and collects a command's output and diagnostics") {
      withBuild { dir =>
        val (socket, seen) = fakeSbt(dir)
        val s = session(dir, socket)
        try
          val r = s.exec("compile")
          assertTrue(
            r.success,
            r.output == "[info] running compile\nred\n",
            r.diagnostics == List(Diagnostic("error", Some("A.scala"), Some(3), Some(4), "Found: String")),
            seen.contains("answered:false"),
            seen.contains("sbt/attach"),
          )
        finally s.close()
      }
    },
    test("an sbt/attach that sbt drops (read before it registered the channel) is sent again") {
      withBuild { dir =>
        val (socket, seen) = fakeSbt(dir, dropFirstAttach = true)
        val s = session(dir, socket)
        try
          val r = s.exec("compile")
          assertTrue(r.success, seen.toArray.count(_ == "sbt/attach") == 2, seen.contains("answered:false"))
        finally s.close()
      }
    },
    test("a failed command reports sbt's message") {
      withBuild { dir =>
        val (socket, _) = fakeSbt(dir)
        val s = session(dir, socket)
        try
          val r = s.exec("fail")
          assertTrue(!r.success, r.error.contains("(Compile / compile) Compilation failed"))
        finally s.close()
      }
    },
    test("a failed build load: sbt's retry/quit prompt is answered with (i)gnore, and the command fails with the load error") {
      withBuild { dir =>
        val (socket, seen) = fakeSbt(dir)
        val server = SbtServer(dir, StartMode.Never, _ => ())
        val s = Session.open(server, socket, _ => ())
        try
          val r      = s.exec("reload-broken")
          val noted  = server.brokenBuildNote
          val next   = s.exec("compile")
          val still  = server.brokenBuildNote
          val reload = s.exec("reload")
          assertTrue(
            seen.contains("systemIn:i"),
            !r.success,
            r.error.exists(e => e.contains("the build failed to load") && e.contains("not found: noSuchSetting")),
            r.output.contains("not found: noSuchSetting"),
            !r.output.contains("(r)etry"),
            next.success, // the flag is per command
            noted.exists(_.contains("not found: noSuchSetting")),
            still == noted, // noted on every result until the build loads
            reload.success,
            server.brokenBuildNote.isEmpty,
          )
        finally s.close()
      }
    },
    test("interrupting a running command asks sbt to cancel it") {
      ZIO.succeed(Files.createTempDirectory("sbta-spec")).flatMap { dir =>
        val (socket, seen) = fakeSbt(dir)
        val s = session(dir, socket)
        for
          fiber <- ZIO.attemptBlockingInterrupt(s.exec("slow")).fork
          // Polled, not timed: on a loaded machine (CI) the fake server can lag behind.
          _     <- ZIO.succeed(seen.contains("sbt/exec")).repeatUntil(identity).delay(50.millis).timeout(10.seconds)
          _     <- fiber.interrupt
          _     <- ZIO.succeed(seen.contains("sbt/cancelRequest")).delay(50.millis).repeatUntil(identity).timeout(10.seconds)
          _     <- ZIO.succeed(s.close())
        yield assertTrue(seen.contains("sbt/cancelRequest"))
      }
    } @@ TestAspect.withLiveClock,
    test("repeated interrupts (as ZIO's attemptBlockingInterrupt sends) don't close the connection before the cancel is sent") {
      withBuild { dir =>
        val (socket, seen) = fakeSbt(dir)
        val s = session(dir, socket)
        try
          val caller = Thread(() => { try s.exec("slow") catch case _: Exception => (); () })
          caller.start()
          val deadline = java.lang.System.nanoTime() + 10_000_000_000L
          while !seen.contains("sbt/exec") && java.lang.System.nanoTime() < deadline do Thread.sleep(5)
          // Keep interrupting until the call returns, like ZIO's interruptThread.
          while caller.isAlive && java.lang.System.nanoTime() < deadline do
            caller.interrupt()
            Thread.sleep(0, 100_000)
          caller.join(1000)
          while !seen.contains("sbt/cancelRequest") && java.lang.System.nanoTime() < deadline do Thread.sleep(5)
          assertTrue(seen.contains("sbt/cancelRequest"))
        finally s.close()
      }
    },
    test("JUnit XML: failures keep their line breaks, without sbt's fork wrappers or colour codes") {
      withBuild { dir =>
        val file = dir.resolve("TEST-demo.FailSpec.xml")
        Files.writeString(
          file,
          """<?xml version='1.0' encoding='UTF-8'?>
            |<testsuite name="demo.FailSpec" tests="3" errors="0" failures="1" skipped="1">
            |  <testcase classname="demo.FailSpec" name="good" time="0.015"></testcase>
            |  <testcase classname="demo.FailSpec" name="bad" time="0.037">
            |    <failure message="flattened" type="x">sbt.internal.worker1.ForkTestMain$ForkError: sbt.internal.worker1.PersistedException: munit.ComparisonFailException: FailSpec.scala:4
            |[7m4:  test("bad")[0m
            |the answer is wrong
            |	at munit.FunSuite.assertEquals(FunSuite.scala:11)</failure>
            |  </testcase>
            |  <testcase classname="demo.FailSpec" name="later" time="0"><skipped/></testcase>
            |</testsuite>""".stripMargin,
        )
        val cases = TestReports.parse(file)
        assertTrue(
          cases.map(_.status) == List("passed", "failed", "skipped"),
          cases(1).message.contains("munit.ComparisonFailException: FailSpec.scala:4\n4:  test(\"bad\")\nthe answer is wrong"),
          cases(1).durationMs.contains(37L),
        )
      }
    },
    test("reports are found under target directories, newer than the run's start") {
      withBuild { dir =>
        val reports = dir.resolve("target/out/jvm/scala-3.3.7/app/test-reports")
        Files.createDirectories(reports)
        val xml = """<testsuite name="S"><testcase classname="S" name="t" time="0.001"/></testsuite>"""
        Files.writeString(reports.resolve("TEST-S.xml"), xml)
        val old = reports.resolve("TEST-Old.xml")
        Files.writeString(old, xml)
        Files.setLastModifiedTime(old, java.nio.file.attribute.FileTime.fromMillis(1000))
        val found = TestReports.since(dir, java.lang.System.currentTimeMillis() - 60_000)
        assertTrue(found.suites == 1, found.cases.map(_.suite) == List("S"))
      }
    },
    test("the committed ./sbta and ./sbta.bat are what `sbtaInstall` writes (run it after changing the scripts)") {
      val expected = Path.of(sys.props("sbta.installExpected"))
      val base     = Path.of(sys.props("sbta.base"))
      def same(name: String) = Files.mismatch(expected.resolve(name), base.resolve(name)) == -1L
      assertTrue(same("sbta"), same("sbta.bat"))
    },
    test("--setup adds the sbta server to each client's config, keeping what's there, and is idempotent") {
      withBuild { dir =>
        Files.writeString(dir.resolve(".mcp.json"), """{"mcpServers":{"other":{"command":"x"}},"extra":1}""")
        val first  = Setup.run(dir, List(Setup.Client.Claude, Setup.Client.Kiro, Setup.Client.Cursor))
        val second = Setup.run(dir, List(Setup.Client.Claude, Setup.Client.Kiro, Setup.Client.Cursor))
        def json(p: String) = Files.readString(dir.resolve(p)).fromJson[Json].toOption.get
        def server(p: String, name: String) = field(field(json(p), "mcpServers").get, name)
        // Read before the directory is deleted (assertTrue evaluates lazily).
        val other    = server(".mcp.json", "other")
        val extra    = field(json(".mcp.json"), "extra")
        val claude   = server(".mcp.json", "sbta")
        val kiro     = server(".kiro/settings/mcp.json", "sbta")
        val cursor   = server(".cursor/mcp.json", "sbta")
        val approved = field(json(".claude/settings.json"), "enabledMcpjsonServers")
        assertTrue(
          other.isDefined,
          extra.contains(Json.Num(1)),
          claude.contains(Setup.entry),
          kiro.contains(Setup.entry),
          cursor.contains(Setup.entry),
          approved.contains(Json.Arr(Json.Str("sbta"))),
          first.forall(_.endsWith("updated")),
          second.forall(_.endsWith("already up to date")),
          Setup.Client.parse("kiro, claude").contains(List(Setup.Client.Kiro, Setup.Client.Claude)),
          Setup.Client.parse("vim").isLeft,
        )
      }
    },
    test("keys in sbt 1's syntax for `sbt/setting`") {
      assertTrue(
        Session.legacyKey("app/Test/fork").contains("app/test:fork"),
        Session.legacyKey("Compile/sourceDirectories").contains("compile:sourceDirectories"),
        Session.legacyKey("app/name").isEmpty,
        Session.legacyKey("name").isEmpty,
      )
    },
    test("sbt's socket URI and ANSI stripping") {
      assertTrue(
        SbtServer.socketOf("local:///home/u/.sbt/1.0/server/abc/sock").contains(Path.of("/home/u/.sbt/1.0/server/abc/sock")),
        SbtServer.socketOf("tcp://127.0.0.1:5000").isEmpty,
        Text.stripAnsi("\u001b[31m-- Error\u001b[0m [1m=> Obtained[0m\r\n") == "-- Error => Obtained\n",
      )
    },
    updatesSuite,
    test("a project of another build (RootProject/ProjectRef) is referred to with its build") {
      val ps = List(
        (true, "file:/b/root/", "root"),
        (true, "file:/b/root/", "core"),
        (false, "file:/b/zio-bedrock/", "zio-bedrock"),
        (false, "file:/b/x/", "dup"),
        (false, "file:/b/y/", "dup"),
        (false, "file:/b/other/", "core"),
      )
      assertTrue(
        SbtTools.projectRef("core", ps) == "core", // the root build's wins
        SbtTools.projectRef("zio-bedrock", ps) == "{file:/b/zio-bedrock/}zio-bedrock",
        SbtTools.projectRef("dup", ps) == "dup", // ambiguous: left for sbt to report
        SbtTools.projectRef("nope", ps) == "nope",
      )
    },
  )

  // ---- updates ----

  private def v(s: String) = Updates.Version.parse(s).get

  private def releasesJson(tags: (String, Boolean, Boolean)*): String =
    tags.map((tag, draft, pre) => s"""{"tag_name":"$tag","draft":$draft,"prerelease":$pre}""").mkString("[", ",", "]")

  /** A release script pinning `version`, as `declare sbta_version=` in the real one. */
  private def releaseScript(version: String) = s"#!/usr/bin/env bash\ndeclare sbta_version=\"$version\"\n"

  /** `<dir>/v<version>/{sbta,sbta.bat,SHA256SUMS}`, as a release has them; returns its `file:` URL. */
  private def releases(dir: Path, version: String, corruptBat: Boolean = false): String =
    val at = dir.resolve(s"v$version")
    Files.createDirectories(at)
    val sbta = releaseScript(version).getBytes(StandardCharsets.UTF_8)
    val bat  = s"@echo off\r\nset sbta_version=$version\r\n".getBytes(StandardCharsets.UTF_8)
    Files.write(at.resolve("sbta"), sbta)
    Files.write(at.resolve("sbta.bat"), if corruptBat then "tampered".getBytes(StandardCharsets.UTF_8) else bat)
    Files.writeString(at.resolve("SHA256SUMS"), s"${Updates.sha256(sbta)}  sbta\n${Updates.sha256(bat)}  sbta.bat\n")
    dir.toUri.toString

  private val noCheck: String => Option[String] = Map("SBTA_NO_UPDATE_CHECK" -> "1").get

  private def updatesSuite = suite("updates")(
    test("semver: only releases, newest by precedence (0.10.0 after 0.9.9, a release after its pre-releases)") {
      // The precedence example of the semver spec, shuffled.
      val spec   = List("1.0.0-alpha", "1.0.0-alpha.1", "1.0.0-alpha.beta", "1.0.0-beta", "1.0.0-beta.2", "1.0.0-beta.11", "1.0.0-rc.1", "1.0.0")
      val sorted = (List("0.10.0", "0.9.9") ++ scala.util.Random(1).shuffle(spec)).map(v).sorted.map(_.toString)
      assertTrue(
        sorted == "0.9.9" :: "0.10.0" :: spec,
        Updates.Version.parse("v1.2.3").contains(Updates.Version(1, 2, 3, None)),
        Updates.Version.parse("1.2").isEmpty,
        Updates.Version.parse("0.0.2-3-c76eefc9-20261018").exists(!_.isRelease), // a dynver development build
        Updates.latestRelease(
          releasesJson(("v0.9.0", false, false), ("v0.10.0", false, false), ("v0.11.0", true, false), ("v0.12.0", false, true),
            ("v0.13.0-RC1", false, false), ("nightly", false, false))
        ).contains(v("0.10.0")),
        Updates.latestRelease("not json").isEmpty,
      )
    },
    test("the check runs at most once a day, and a failed one keeps what the last found") {
      withBuild { dir =>
        val file  = dir.resolve("latest.json")
        var calls = 0
        def fetch(json: String) = () => { calls += 1; json }
        val day   = Updates.CheckInterval
        val first = Updates.refresh(file, 1000, fetch(releasesJson(("v0.1.0", false, false))))
        val same  = Updates.refresh(file, 1000 + day - 1, fetch(releasesJson(("v0.2.0", false, false))))
        val failed = Updates.refresh(file, 1000 + day, () => { calls += 1; throw java.io.IOException("offline") })
        val later  = Updates.refresh(file, 1000 + 2 * day, fetch(releasesJson(("v0.2.0", false, false))))
        val stored = Updates.read(file)
        assertTrue(
          first.flatMap(_.latest).contains("0.1.0"),
          same.flatMap(_.latest).contains("0.1.0"),
          failed.contains(Updates.Checked(1000 + day, Some("0.1.0"))),
          later.flatMap(_.latest).contains("0.2.0"),
          calls == 3,
          stored == later,
        )
      }
    },
    test("a notice only for a newer release than a release build, and switches to turn it off") {
      val checked = Some(Updates.Checked(0, Some("0.3.0")))
      assertTrue(
        Updates.newer("0.2.9", checked).contains(v("0.3.0")),
        Updates.newer("0.3.0", checked).isEmpty,
        Updates.newer("0.4.0", checked).isEmpty,
        Updates.newer("0.2.9-4-abcd1234", checked).isEmpty, // development builds
        Updates.newer("0.2.9", None).isEmpty,
        Updates.notice("0.2.9", v("0.3.0")).contains("./sbta --update"),
        Updates.checkDisabled(Map("CI" -> "true").get),
        Updates.checkDisabled(Map("SBTA_NO_UPDATE_CHECK" -> "1").get),
        !Updates.checkDisabled(Map.empty[String, String].get),
        Updates.noticesDisabled(Map("SBTA_NO_UPDATE_NOTICE" -> "1").get),
      )
    },
    test("--update replaces sbta and sbta.bat with a release's, checked against its SHA256SUMS") {
      withBuild { dir =>
        val repo = releases(dir.resolve("repo"), "0.3.0")
        Files.writeString(dir.resolve("sbta"), releaseScript("0.2.0"))
        Files.writeString(dir.resolve("sbta.bat"), "old")
        val result = Updates.update(dir, None, repo, () => Some(v("0.3.0")), deferBat = false)
        val again  = Updates.update(dir, Some("0.3.0"), repo, () => None, deferBat = false)
        val pinned = Updates.pinnedVersion(dir)
        val bat    = Files.readString(dir.resolve("sbta.bat"))
        val executable = SbtServer.windows || Files.isExecutable(dir.resolve("sbta"))
        assertTrue(
          result.exists(_.head == "updated sbta and sbta.bat from 0.2.0 to 0.3.0"),
          pinned.contains("0.3.0"),
          executable,
          bat.contains("set sbta_version=0.3.0"),
          again == Right(List("sbta and sbta.bat are already 0.3.0")),
        )
      }
    },
    test("--update changes nothing when a file doesn't match SHA256SUMS, and refuses a non-release script") {
      withBuild { dir =>
        val repo = releases(dir.resolve("repo"), "0.3.0", corruptBat = true)
        Files.writeString(dir.resolve("sbta"), releaseScript("0.2.0"))
        val tampered = Updates.update(dir, Some("0.3.0"), repo, () => None, deferBat = false)
        val pinned   = Updates.pinnedVersion(dir)
        val missing  = Updates.update(dir, Some("9.9.9"), repo, () => None, deferBat = false)
        Files.writeString(dir.resolve("sbta"), "#!/usr/bin/env bash\ndeclare sbta_local_jar=\"x.jar\"\n")
        val local = Updates.update(dir, Some("0.3.0"), repo, () => None, deferBat = false)
        assertTrue(
          tampered.left.exists(_.contains("sbta.bat of 0.3.0 doesn't match its SHA256SUMS")),
          pinned.contains("0.2.0"),
          missing.isLeft,
          local.left.exists(_.contains("no release sbta script")),
        )
      }
    },
    test("--update from a running sbta.bat leaves the new one as sbta.bat.new, for the script to move into place") {
      withBuild { dir =>
        val repo = releases(dir.resolve("repo"), "0.3.0")
        Files.writeString(dir.resolve("sbta"), releaseScript("0.2.0"))
        Files.writeString(dir.resolve("sbta.bat"), "old")
        val result = Updates.update(dir, Some("0.3.0"), repo, () => None, deferBat = true)
        val bat    = Files.readString(dir.resolve("sbta.bat"))
        val next   = Files.readString(dir.resolve("sbta.bat.new"))
        assertTrue(result.isRight, bat == "old", next.contains("0.3.0"))
      }
    },
    test("MCP: the note is added to every tool result (not structuredContent), unless turned off, until the build is updated") {
      import com.jamesward.ziohttp.mcp.*
      withBuild { dir =>
        Files.writeString(dir.resolve("sbta"), releaseScript("0.2.0"))
        val cache = dir.resolve("latest.json")
        Files.writeString(cache, """{"checkedAt":0,"latest":"0.3.0"}""")
        val on     = McpMain.UpdateNotes(dir, resultNotes = true, _ => (), cache, noCheck)
        val off    = McpMain.UpdateNotes(dir, resultNotes = false, _ => (), cache, noCheck)
        val muted  = McpMain.UpdateNotes(dir, resultNotes = true, _ => (), cache, Map("SBTA_NO_UPDATE_CHECK" -> "1", "SBTA_NO_UPDATE_NOTICE" -> "1").get)
        val result = CallToolResult(content = zio.Chunk(ToolContent.text("ok")), structuredContent = Some(Json.Obj("a" -> Json.Num(1))))
        val noted  = on.addNote(result)
        val instructions = on.instructionsNote
        val (offResult, offInstructions, mutedResult) = (off.addNote(result), off.instructionsNote, muted.addNote(result))
        Files.writeString(dir.resolve("sbta"), releaseScript("0.3.0"))
        val afterUpdate = on.addNote(result)
        assertTrue(
          noted.content.size == 2,
          noted.content.last == ToolContent.text(Updates.mcpNote("0.2.0", v("0.3.0"))),
          noted.structuredContent == result.structuredContent,
          instructions.exists(_.contains("update-sbta")),
          offResult == result,
          offInstructions.isDefined,
          mutedResult == result,
          afterUpdate == result,
        )
      }
    },
    test("MCP: a broken build is noted on every tool result, also with the update note off, unless the result says so") {
      import com.jamesward.ziohttp.mcp.*
      withBuild { dir =>
        var broken: Option[String] = Some("The build files don't load: x")
        val notes  = McpMain.UpdateNotes(dir, resultNotes = false, _ => (), dir.resolve("latest.json"), noCheck, () => broken)
        val result = CallToolResult(content = zio.Chunk(ToolContent.text("ok")))
        val failed = CallToolResult(content = zio.Chunk(ToolContent.text("[error] reload: the build failed to load; …")))
        val noted       = notes.addNote(result)
        val notRepeated = notes.addNote(failed)
        broken = None
        val fixed = notes.addNote(result)
        assertTrue(
          noted.content.last == ToolContent.text("The build files don't load: x"),
          notRepeated == failed,
          fixed == result,
        )
      }
    },
  )
