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
  private def fakeSbt(dir: Path): (Path, java.util.concurrent.ConcurrentLinkedQueue[String]) =
    val socket = dir.resolve("sock")
    val seen   = java.util.concurrent.ConcurrentLinkedQueue[String]()
    val server = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
    server.bind(UnixDomainSocketAddress.of(socket))
    val thread = Thread { () =>
      val channel = server.accept()
      val in      = BufferedInputStream(Channels.newInputStream(channel))
      val out     = Channels.newOutputStream(channel)
      var running: Option[String] = None
      try
        while true do
          val m      = readMessage(in)
          val method = str(m, "method")
          val id     = field(m, "id").map(_.toJson).getOrElse("null")
          if method.nonEmpty then seen.add(method)
          method match
            case "initialize" => write(out, s"""{"jsonrpc":"2.0","id":$id,"result":{"capabilities":{}}}""")
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
                case _      => write(out, s"""{"jsonrpc":"2.0","id":$id,"result":{"status":"Done","exitCode":0}}""")
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
  )
