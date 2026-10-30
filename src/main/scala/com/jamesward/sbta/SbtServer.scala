package com.jamesward.sbta

import zio.json.*
import zio.json.ast.Json

import java.io.{ File, IOException }
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path }
import java.util.concurrent.{ CompletableFuture, TimeUnit, TimeoutException }
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/** How `sbta --mcp` starts the build's sbt server when it isn't running. */
enum StartMode derives CanEqual:
  case Eager, Lazy, Never

/**
 * The build's sbt server: found through `project/target/active.json`, which sbt
 * writes while its server runs (the same file `sbtn` uses), and started with
 * `sbt --client` when it isn't running. That is sbt's own background server, shared
 * with later `sbt`/`sbtn`/`sbta` commands, and it keeps running after `sbta` exits.
 */
final class SbtServer(val base: Path, start: StartMode, log: String => Unit):

  private val activeFile = base.resolve("project").resolve("target").resolve("active.json")
  val runtimeDir: Path   = base.resolve("project").resolve("target").resolve("sbta")
  val bootLog: Path       = runtimeDir.resolve("boot.log")

  /** How long a call waits for sbt to start (a first start may download a lot). */
  val StartWait = java.time.Duration.ofMinutes(15)

  /** Which `active.json` this is (sbt writes it anew when its server starts), or None without one. */
  private[sbta] def activeStamp(): Option[String] =
    try
      val attrs = Files.readAttributes(activeFile, classOf[java.nio.file.attribute.BasicFileAttributes])
      Some(s"${attrs.lastModifiedTime.toMillis}/${attrs.fileKey}")
    catch case NonFatal(_) => None

  /** The last failed start: when, the build files then, and why. Retried after 30 s, or once the build files change. */
  @volatile private var lastFailure: Option[(Long, Vector[String], String)] = None

  /**
   * Changes when anything that can change the loaded build does: the build definition
   * files, and sbt's server itself (`active.json` is rewritten when it restarts).
   */
  def buildFingerprint(): Vector[String] =
    def list(dir: Path): List[Path] =
      try scala.util.Using.resource(Files.list(dir))(_.iterator.asScala.filter(Files.isRegularFile(_)).toList)
      catch case NonFatal(_) => Nil
    val root    = base.toAbsolutePath.normalize
    val project = root.resolve("project")
    val candidates =
      list(root).filter(_.toString.endsWith(".sbt")) ++
        list(project).filter(p => p.toString.endsWith(".sbt") || p.toString.endsWith(".scala") || p.getFileName.toString == "build.properties") ++
        List(activeFile)
    candidates.sorted.map(p => s"$p\u0000${try Files.getLastModifiedTime(p).toMillis catch case NonFatal(_) => -1L}").toVector

  /**
   * The build files as sbt last loaded them (as far as sbta knows): the fingerprint of
   * this channel's last (re)load, or, for a server sbta hasn't reloaded, its start.
   */
  @volatile private var loadedFingerprint: Option[Vector[String]] = None

  private[sbta] def loaded(fingerprint: Vector[String]): Unit = loadedFingerprint = Some(fingerprint)

  /** The last load's error, while the build files don't load (and the server that failed to load them runs). */
  @volatile private var broken: Option[(Option[String], String)] = None

  private def startedAt: Option[String] = buildFingerprint().find(_.startsWith(activeFile.toString + "\u0000"))

  private[sbta] def loadFailed(errors: String): Unit = broken = Some(startedAt -> errors)
  private[sbta] def loadSucceeded(): Unit = broken = None

  /**
   * For every tool result while the build files don't load: sbt keeps running the build
   * it had loaded, so results don't reflect the build-file changes.
   */
  def brokenBuildNote: Option[String] =
    broken.filter(_._1 == startedAt).map { (_, errors) =>
      "The build files don't load, so sbt is still using the build it loaded before they changed " +
        "(results don't reflect the build-file changes). Fix them, then run `reload` (sbt-task)." +
        (if errors.isEmpty then "" else s" The load error:\n$errors")
    }

  /**
   * Whether to `reload` from this channel before `commandLine` (or a BSP request). sbt 1,
   * with `onChangedBuildSource := ReloadOnSourceChanges`, reloads changed build files
   * itself before a command, but as a command of no channel (sbt 2 keeps the
   * command's): a failed load then asks "(r)etry, (q)uit, (l)ast, or (i)gnore?" on the
   * server's own stdin, which nobody can answer, and sbt hangs. Reloading first, from
   * this channel, makes sbt ask here instead (see [[Session]]), and leaves sbt nothing
   * to reload. sbt 1 can't report `onChangedBuildSource` without running a command
   * (which would set off its reload), so on sbt 1 changed build files are always
   * reloaded first, also when the build only warns about them. sbt 2 is left alone.
   */
  private[sbta] def reloadFirst(commandLine: String): Boolean =
    if SbtServer.reloads(commandLine) then false
    else
      val current  = buildFingerprint()
      val active   = activeFile.toString + "\u0000"
      def started(fp: Vector[String]) = fp.find(_.startsWith(active))
      def millis(entry: String) = entry.split('\u0000').lift(1).flatMap(_.toLongOption)
      val baseline = loadedFingerprint.filter(fp => started(fp) == started(current)).getOrElse {
        // Not reloaded by sbta since this server started (`active.json` is written then).
        val since = started(current).flatMap(millis).getOrElse(Long.MaxValue)
        val changedSinceStart = current.filterNot(_.startsWith(active)).exists(e => millis(e).exists(_ > since))
        if changedSinceStart then Vector.empty else current
      }
      if baseline == current then
        loadedFingerprint = Some(current)
        false
      else
        // sbt 1 can't say what `onChangedBuildSource` is without running a command (it has
        // no JSON for it), and any command would trigger its reload: reload either way.
        val sbt1 = SbtServer.sbtVersion(base).forall(_.startsWith("1."))
        if !sbt1 then loadedFingerprint = Some(current) // sbt 2 reloads from the command's channel
        else log("build files changed: reloading from sbta's channel first (sbt 1 would reload them from no channel)")
        sbt1

  /** The live server's socket, or None (not running, or `active.json` is stale). */
  def socket(): Option[Path] = activeSocket().filter(SbtServer.accepts)

  /** The server's socket, starting sbt first when it isn't running (and starting is allowed). */
  def ensureRunning(): Path = synchronized {
    socket().getOrElse {
      if start == StartMode.Never then
        throw IOException(s"sbt is not running in $base (and sbta was started with --start=never). Start sbt in the build, then retry.")
      lastFailure match
        case Some((at, files, failure)) if System.currentTimeMillis() - at < 30_000 && files == buildFingerprint() =>
          throw IOException(failure)
        case _ => ()
      boot()
    }
  }

  /**
   * Run `f` on a fresh connection to the server: one sbt channel per call, closed afterwards.
   * It connects to the socket `active.json` names without probing it first: a probe is a
   * channel too, and on sbt 1 a channel's disconnect runs a command that reloads changed
   * build files (with `ReloadOnSourceChanges`) as a command of no channel, whose failed
   * load then asks "(r)etry, (q)uit…" on the server's own stdin and hangs sbt.
   */
  def session[A](f: Session => A): A =
    val direct =
      activeSocket().flatMap { path =>
        try Some(Session.open(this, path, log))
        catch
          case e: IOException =>
            log(s"connecting to sbt failed (${e.getMessage}); checking that it runs")
            None
      }
    val session = direct.getOrElse(Session.open(this, ensureRunning(), log))
    try f(session)
    finally session.close()

  /**
   * The sbt server JVMs running now (`--detach-stdio`, as sbtn starts them); on Linux only
   * this build's, by working directory (elsewhere other builds' servers are included).
   */
  private[sbta] def serverPids(): Set[Long] =
    val root = try base.toRealPath() catch case NonFatal(_) => base.toAbsolutePath.normalize
    def cwd(p: ProcessHandle) = try Some(Path.of(s"/proc/${p.pid}/cwd").toRealPath()) catch case NonFatal(_) => None
    try
      ProcessHandle.allProcesses().iterator.asScala.filter { p =>
        val info = p.info
        (info.arguments().orElse(Array.empty[String]).exists(_.contains("--detach-stdio")) ||
          info.commandLine().orElse("").contains("--detach-stdio")) &&
        cwd(p).forall(_ == root)
      }.map(_.pid).toSet
    catch case NonFatal(_) => Set.empty

  /** The socket `active.json` names, unchecked. */
  private[sbta] def activeSocketPath(): Option[Path] = activeSocket()

  private def activeSocket(): Option[Path] =
    try
      if !Files.isRegularFile(activeFile) then None
      else
        Files.readString(activeFile, StandardCharsets.UTF_8).fromJson[Json].toOption
          .flatMap(_.asObject).flatMap(_.get("uri")).collect { case Json.Str(uri) => uri }
          .flatMap(SbtServer.socketOf)
    catch case NonFatal(_) => None

  private def boot(): Path =
    Files.createDirectories(runtimeDir)
    val version = SbtServer.sbtVersion(base)
    val from    = version.fold("no sbt.version in project/build.properties")(v => s"sbt.version=$v from project/build.properties")
    log(s"starting sbt in $base ($from; output: $bootLog)")
    // Each start's output after its own header, so a failure shows this start's lines only.
    try Files.writeString(bootLog, s"${SbtServer.BootHeader} ${java.time.Instant.now()} ($from)\n", StandardCharsets.UTF_8,
        java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.APPEND)
    catch case NonFatal(_) => ()
    val started = System.nanoTime()
    var code    = runSbt("--client")
    if code != 0 && socket().isEmpty then
      log(s"`sbt --client` exited with $code; trying the JVM client")
      code = runSbt("--jvm-client")
    // `--client` returns once the server runs; the socket can lag a little behind it. A
    // client that failed (e.g. sbt couldn't get the build's `sbt.version`) started nothing.
    val deadline = if code == 0 then started + StartWait.toNanos else System.nanoTime() + 5_000_000_000L
    var found    = socket()
    while found.isEmpty && System.nanoTime() < deadline do
      Thread.sleep(250)
      found = socket()
    found match
      case Some(path) =>
        lastFailure = None
        log(s"sbt is running (${(System.nanoTime() - started) / 1_000_000} ms)")
        path
      case None =>
        val failure =
          s"sbt did not start in $base ($from, exit code $code). sbta tries again on the next call once the build files " +
            s"(project/build.properties included) change, or after 30 s. Last lines of $bootLog:\n${SbtServer.tailSince(bootLog, SbtServer.BootHeader, 25)}"
        lastFailure = Some((System.currentTimeMillis(), buildFingerprint(), failure))
        log(failure)
        throw IOException(failure)

  /** `sbt --client <a cheap command>`: connects to the build's server, starting it if needed, then exits. */
  private def runSbt(clientFlag: String): Int =
    val command = SbtServer.sbtCommand(base) ++ List(clientFlag, "sbtVersion")
    log(s"running: ${command.mkString(" ")}")
    val builder = ProcessBuilder(command*)
      .directory(base.toFile)
      .redirectErrorStream(true)
      .redirectOutput(ProcessBuilder.Redirect.appendTo(bootLog.toFile))
      .redirectInput(ProcessBuilder.Redirect.from(File(if SbtServer.windows then "NUL" else "/dev/null")))
    // sbt sets this for the processes it forks; an sbt started with it takes its terminal from it.
    builder.environment().remove("SBT_TERMINAL_PROPS")
    val process = builder.start()
    if process.waitFor(StartWait.toMinutes, TimeUnit.MINUTES) then process.exitValue()
    else
      process.destroyForcibly()
      -1

object SbtServer:

  /** The build's sbt version, from `project/build.properties`. */
  def sbtVersion(base: Path): Option[String] =
    try
      val props = java.util.Properties()
      scala.util.Using.resource(Files.newBufferedReader(base.resolve("project").resolve("build.properties")))(props.load)
      Option(props.getProperty("sbt.version")).map(_.trim).filter(_.nonEmpty)
    catch case NonFatal(_) => None

  /** Whether a command line (re)loads the build itself, so sbt doesn't check the build files first (as its `needCheck`). */
  def reloads(commandLine: String): Boolean =
    commandLine.split(';').map(_.trim.takeWhile(!_.isWhitespace)).exists(c =>
      c == "reload" || c == "reboot" || c == "exit" || c == "shutdown" || c.startsWith("sbtReboot")
    )

  val windows: Boolean = System.getProperty("os.name", "").toLowerCase.contains("win")

  /**
   * The build's sbt runner: its own `sbta` or `sbt` script when it has one (checked
   * in, like `./sbt`), otherwise `sbta` or `sbt` from the PATH.
   */
  def sbtCommand(base: Path): List[String] =
    if windows then
      List("sbta.bat", "sbt.bat").map(base.resolve).find(Files.isRegularFile(_)) match
        case Some(script) => List("cmd", "/c", script.toString)
        case None         => List("cmd", "/c", if onPath("sbta.bat") then "sbta" else "sbt")
    else
      List("sbta", "sbt").map(base.resolve).find(Files.isRegularFile(_)) match
        case Some(script) => List("bash", script.toString)
        case None         => List(if onPath("sbta") then "sbta" else "sbt")

  private def onPath(name: String): Boolean =
    Option(System.getenv("PATH")).toList.flatMap(_.split(java.io.File.pathSeparator)).exists { dir =>
      try Files.isExecutable(Path.of(dir).resolve(name)) catch case NonFatal(_) => false
    }

  /** `local:///path/to/sock` (sbt's Unix domain socket). Windows named pipes and TCP aren't supported yet. */
  def socketOf(uri: String): Option[Path] =
    try
      val parsed = URI.create(uri)
      if parsed.getScheme == "local" && !windows then Some(Path.of(parsed.getPath)) else None
    catch case NonFatal(_) => None

  /** Whether something accepts connections on the socket (sbt removes it on a clean exit, not when killed). */
  def accepts(socket: Path): Boolean =
    try
      val channel = java.nio.channels.SocketChannel.open(java.net.StandardProtocolFamily.UNIX)
      try { channel.connect(java.net.UnixDomainSocketAddress.of(socket)); true }
      finally channel.close()
    catch case NonFatal(_) => false

  /** Starts each sbt start's output in the boot log. */
  val BootHeader = "--- sbta: starting sbt"

  /** The last `lines` lines of `file` after its last line starting with `header` (or of all of it). */
  def tailSince(file: Path, header: String, lines: Int): String =
    try
      val all  = Files.readAllLines(file, StandardCharsets.UTF_8).asScala.toVector
      val from = all.lastIndexWhere(_.startsWith(header)) + 1
      val mine = all.drop(from)
      if mine.isEmpty then "(no output)" else mine.takeRight(lines).mkString("\n")
    catch case NonFatal(_) => "(no output)"


/** A compiler diagnostic from BSP `build/publishDiagnostics`. */
final case class Diagnostic(severity: String, file: Option[String], line: Option[Int], column: Option[Int], message: String)
    derives CanEqual

/** A JVM sbt 2 asked the client to start (`sbt/clientJob`, for `run`/`runMain` over the network). */
final case class ClientJob(params: Json)

/** What one `sbt/exec` did. */
final case class ExecResult(
    commandLine: String,
    success: Boolean,
    error: Option[String],
    elapsedMs: Long,
    output: String,
    outputTruncated: Boolean,
    diagnostics: List[Diagnostic],
    clientJobs: List[ClientJob],
)

/**
 * One sbt channel: initialized without the startup analysis, and attached as a
 * non-interactive terminal so the console output of the commands it sends
 * (including forked tests and programs) comes back as `sbt/systemOut` bytes. sbt
 * sends a command's output to the channel that sent it, so each call gets its own.
 */
final class Session private (server: SbtServer, log: String => Unit) extends AutoCloseable:

  @volatile private var connection: SbtConnection = null
  /** Completed when sbt closes this channel to reboot (see [[Session.rebootOf]]). */
  @volatile private var rebooting = CompletableFuture[(String, String)]()
  /** The server JVMs when sbt said it was rebooting: it reboots in the same JVM, which exits if that fails. */
  @volatile private var serversAtReboot = Set.empty[Long]

  /** Connect to the server on `socket`: `initialize` and `sbt/attach`. */
  private def connect(socket: Path): Unit =
    rebooting = CompletableFuture()
    val c = SbtConnection.open(socket, (method, params) => handle(method, params))
    connection = c
    try handshake(c)
    catch
      case e: Throwable =>
        c.close()
        throw e

  private def handshake(c: SbtConnection): Unit =
    val init = Json.Obj("initializationOptions" -> Json.Obj("skipAnalysis" -> Json.Bool(true)))
    // Both at once, as sbt's own client does: with `sbt/attach` sent only after the
    // `initialize` answer, sbt 1 asks a failed build load's "(r)etry, (q)uit…" on the
    // server's own stdin instead of this channel's, and hangs.
    val initialized = c.request("initialize", init)
    val attach      = Json.Obj("interactive" -> Json.Bool(false))
    var attached    = c.request("sbt/attach", attach)
    def failed(e: java.util.concurrent.ExecutionException) = e.getCause match
      case io: IOException => io
      case other           => IOException(s"the connection handshake failed: $other", other)
    try initialized._2.get(30, TimeUnit.SECONDS)
    catch
      case _: TimeoutException => throw IOException("sbt did not answer the connection handshake within 30 s")
      case e: java.util.concurrent.ExecutionException => throw failed(e)
    // sbt (1.12 at least) can drop an `sbt/attach` that arrives before it has registered
    // the new channel: `NetworkChannel` starts reading in its constructor, and
    // `channelForName` finds no channel yet, so nothing answers. Ask again, until one is
    // answered (asking twice only replaces the channel's terminal).
    val deadline = System.nanoTime() + 30_000_000_000L
    var answered = Option.empty[Json]
    while answered.isEmpty do
      try answered = Some(attached._2.get(1, TimeUnit.SECONDS))
      catch
        case _: TimeoutException =>
          if System.nanoTime() > deadline then throw IOException("sbt did not answer the connection handshake within 30 s")
          log(s"sbt didn't answer sbt/attach ${attached._1} within 1 s; asking again")
          attached = c.request("sbt/attach", attach)
        case e: java.util.concurrent.ExecutionException => throw failed(e)

  /**
   * After sbt closed the channel to reboot: wait for its new server (the same JVM loads
   * sbt again, downloading a changed `sbt.version` first, so this can take minutes), and
   * connect to it. `activeBefore` is `active.json`'s timestamp before the reboot: the old
   * server must not be mistaken for the new one.
   */
  private def reconnectAfterReboot(activeBefore: Option[String]): Unit =
    connection.close()
    val started  = System.nanoTime()
    val deadline = started + server.StartWait.toNanos
    var last     = Option.empty[String]
    var done     = false
    var diedAt   = Option.empty[Long]
    while !done do
      // A reboot that fails (e.g. sbt can't get the `sbt.version` it should switch to) ends the JVM.
      if diedAt.isEmpty && serversAtReboot.exists(pid => !ProcessHandle.of(pid).map(_.isAlive).orElse(false)) then
        diedAt = Some(System.nanoTime())
      if diedAt.exists(at => System.nanoTime() - at > 3_000_000_000L) && server.activeStamp().forall(activeBefore.contains) then
        log("sbt's server exited while rebooting: starting it again")
        val socket =
          try server.ensureRunning()
          catch
            case e: IOException => throw IOException(s"sbt's server exited while rebooting (the reboot failed). ${e.getMessage}", e)
        connect(socket)
        done = true
      else if System.nanoTime() > deadline then
        throw IOException(s"sbt rebooted, and its server didn't come back within ${server.StartWait.toMinutes} minutes" +
          last.fold("")(e => s" (last connection error: $e)") + s". See ${server.bootLog} and `./sbta` in the build.")
      else if !server.activeStamp().exists(stamp => !activeBefore.contains(stamp)) then Thread.sleep(250)
      else
        server.activeSocketPath() match
          case None         => Thread.sleep(250)
          case Some(socket) =>
            try
              connect(socket)
              done = true
            catch
              case e: IOException =>
                last = Some(e.getMessage)
                Thread.sleep(500)
    log(s"sbt rebooted: reconnected after ${(System.nanoTime() - started) / 1_000_000} ms")

  private val MaxOutput = 200_000

  private val output       = StringBuilder()
  private var truncated    = false
  private val diagnostics  = scala.collection.mutable.LinkedHashMap[String, List[Diagnostic]]()
  private val clientJobs   = scala.collection.mutable.ListBuffer[ClientJob]()
  private var bspReady     = false
  @volatile var onQueued: String => Unit = _ => ()

  private[sbta] def handle(method: String, params: Json): Unit = method match
    case "sbt/systemOut" | "sbt/systemErr" =>
      params match
        case Json.Arr(bytes) =>
          val raw = bytes.collect { case Json.Num(n) => n.byteValue }.toArray
          append(String(raw, StandardCharsets.UTF_8))
        case _ => ()
    case "build/publishDiagnostics" => synchronized(addDiagnostics(params))
    case "shutdown" =>
      Session.rebootOf(params) match
        case Some(reboot) =>
          log(s"sbt is rebooting (the exec it ran: ${reboot._1}; commands left: \"${reboot._2}\")")
          serversAtReboot = server.serverPids()
          rebooting.complete(reboot)
          ()
        case None => ()
    case "sbt/clientJob"            => synchronized(clientJobs += ClientJob(params))
    case "sbt/exec" =>
      val fields = params.asObject.map(_.fields.toMap).getOrElse(Map.empty)
      if fields.get("status").contains(Json.Str("Queued")) then
        onQueued(fields.get("message").collect { case Json.Str(m) => m }.getOrElse("queued"))
    // sbt reads this channel's stdin (it flushes the command's output first, so a prompt is
    // already in `output`). Nobody types into sbta's channel: a failed build load's
    // "(r)etry, (q)uit, (l)ast, or (i)gnore?" is answered with (i)gnore, which keeps the
    // build sbt had loaded before (retry would fail again, quit would stop the shared
    // server), and the command is reported as failed. Other reads stay unanswered.
    case "sbt/readSystemIn" =>
      val prompt = synchronized(output.lastIndexOf(Session.LoadFailedPrompt) >= 0 && !loadFailed)
      if prompt then
        synchronized { loadFailed = true }
        log("the build failed to load: answering sbt's prompt with (i)gnore")
        try connection.notify("sbt/systemIn", Json.Num('i'.toByte))
        catch case NonFatal(e) => log(s"answering sbt's prompt failed: ${e.getMessage}")
      else log("sbt is waiting for input on stdin, which sbta doesn't provide (cancel the call to stop it)")
    case _ => ()

  /** The command's build load failed, and sbt's retry/quit prompt was answered (see `sbt/readSystemIn`). */
  private var loadFailed = false

  @volatile private var lastOutputAt = 0L

  /** Wait until no console output has arrived for 100 ms (at most 1 s). */
  private def awaitQuietOutput(): Unit =
    val answered = System.nanoTime()
    val deadline = answered + 1_000_000_000L
    // Quiet since the response, or since the last output that followed it.
    def quietFor = System.nanoTime() - math.max(answered, lastOutputAt)
    while quietFor < 100_000_000L && System.nanoTime() < deadline do Thread.sleep(20)

  private def append(text: String): Unit = synchronized {
    lastOutputAt = System.nanoTime()
    output.append(text)
    if output.length > MaxOutput then
      output.delete(0, output.length - MaxOutput)
      truncated = true
  }

  private def addDiagnostics(params: Json): Unit =
    val fields = params.asObject.map(_.fields.toMap).getOrElse(Map.empty)
    val uri = fields.get("textDocument").flatMap(_.asObject).flatMap(_.get("uri")).collect { case Json.Str(u) => u }
    val reset = fields.get("reset").contains(Json.Bool(true))
    val file  = uri.flatMap(u => try Some(Path.of(URI.create(u))) catch case NonFatal(_) => None).map(relative)
    val items = fields.get("diagnostics").collect { case Json.Arr(ds) => ds.toList }.getOrElse(Nil).flatMap(_.asObject).map { d =>
      val m        = d.fields.toMap
      val start    = m.get("range").flatMap(_.asObject).flatMap(_.get("start")).flatMap(_.asObject).map(_.fields.toMap)
      val line     = start.flatMap(_.get("line")).collect { case Json.Num(n) => n.intValue + 1 }
      val column   = start.flatMap(_.get("character")).collect { case Json.Num(n) => n.intValue }
      val severity = m.get("severity").collect { case Json.Num(n) => n.intValue } match
        case Some(1) => "error"
        case Some(2) => "warning"
        case _       => "info"
      val message = m.get("message").collect { case Json.Str(s) => Text.stripAnsi(s) }.getOrElse("")
      Diagnostic(severity, file, line, column, message)
    }
    val key = uri.getOrElse("")
    // `reset` replaces the file's diagnostics (a fresh report); otherwise they accumulate.
    diagnostics(key) = if reset then items else (diagnostics.getOrElse(key, Nil) ++ items).distinct

  private def relative(path: Path): String =
    val root = server.base.toAbsolutePath.normalize
    val abs  = path.toAbsolutePath.normalize
    if abs.startsWith(root) then root.relativize(abs).toString else abs.toString

  /** Discard what earlier commands on this session reported. */
  def reset(): Unit = synchronized {
    output.clear()
    truncated = false
    diagnostics.clear()
    clientJobs.clear()
    loadFailed = false
  }

  /** Run a command line, as typed at the sbt shell; blocks until sbt has run it (cancelled on interrupt). */
  def exec(commandLine: String): ExecResult =
    if !server.reloadFirst(commandLine) then run(commandLine)
    else
      val reload = run("reload")
      if !reload.success then
        reload.copy(commandLine = commandLine, error = reload.error.map(e => s"`$commandLine` didn't run: $e"))
      else
        val r = run(commandLine)
        r.copy(output = reload.output + "\n" + r.output, elapsedMs = reload.elapsedMs + r.elapsedMs)

  /** A BSP request is a command on sbt 1 too: reload changed build files from this channel first (see [[SbtServer.reloadFirst]]). */
  private def reloadIfChanged(): Unit =
    if server.reloadFirst("") then
      val r = run("reload")
      if !r.success then throw RpcError(-32603, r.error.getOrElse("the build failed to load"))

  private def run(commandLine: String): ExecResult =
    val fingerprint  = server.buildFingerprint()
    val activeBefore = server.activeStamp()
    reset()
    val started      = System.nanoTime()
    val (id, future) = connection.request("sbt/exec", Json.Obj("commandLine" -> Json.Str(commandLine)))
    var rebooted     = Option.empty[Session.Rebooted]
    val error =
      try
        await(id, future)
        None
      catch
        case r: Session.Rebooted =>
          rebooted = Some(r)
          None
        // -33000 with an empty message: the command failed (sbt reports why in the output, e.g. a
        // parse error or a failed task), or a command before it on the line did.
        case e: RpcError if e.code == -33000 && e.message.isEmpty => Some(Session.GenericFailure)
        // -33000 with the command line itself: the command was cancelled or not run.
        case e: RpcError if e.code == -33000 => Some(s"not completed: ${e.message} (cancelled, or an earlier part failed)")
        case e: RpcError                     => Some(e.message)
    rebooted match
      case Some(r) => afterReboot(commandLine, r, activeBefore, started)
      case None    => finish(commandLine, fingerprint, started, error)

  /**
   * sbt closed the channel to reboot (`reboot`, e.g. to switch to a changed
   * `sbt.version`): reconnect to its new server, then run the rest of the command line
   * there, as sbtn does.
   */
  private def afterReboot(commandLine: String, r: Session.Rebooted, activeBefore: Option[String], started: Long): ExecResult =
    val before = synchronized(Text.stripAnsi(output.toString))
    reconnectAfterReboot(activeBefore)
    // The new server loaded the build files as they are now.
    server.loaded(server.buildFingerprint())
    val note = s"[info] sbt rebooted; sbta reconnected to its new server (${(System.nanoTime() - started) / 1_000_000} ms)\n"
    val head = before + (if before.isEmpty || before.endsWith("\n") then "" else "\n") + note
    if r.remaining.trim.isEmpty then
      reset()
      ExecResult(commandLine, true, None, (System.nanoTime() - started) / 1_000_000, head, false, Nil, Nil)
    else
      val rest = run(r.remaining)
      rest.copy(commandLine = commandLine, output = head + rest.output, elapsedMs = (System.nanoTime() - started) / 1_000_000)

  private def finish(commandLine: String, fingerprint: Vector[String], started: Long, error: Option[String]): ExecResult =
    // Output written just before the response can arrive just after it (sbt batches console writes).
    awaitQuietOutput()
    // A (re)load from this channel: sbt has loaded the build files as they were when it started.
    if SbtServer.reloads(commandLine) then server.loaded(fingerprint)
    synchronized {
      // The prompt was for a terminal; say what sbta answered instead.
      val text = Text.stripAnsi(output.toString).replace(Session.LoadFailedPrompt, Session.LoadFailedAnswer)
      val errors = Session.loadErrors(text)
      // sbt 2 reloads changed build files for a command (`loadp`) without asking: a load
      // that started but didn't get to setting the current project failed the command.
      val failedLoad = loadFailed || (error.isDefined && Session.loadIncomplete(text))
      val failure =
        if failedLoad then
          Some(
            "the build failed to load; sbt kept the build it had loaded before. Fix the build files and `reload` again" +
              (if errors.isEmpty then " (the error is in the log)" else s". The load error:\n$errors")
          )
        // sbt's answer doesn't say why: say what its output does.
        else if error.contains(Session.GenericFailure) then Some(Session.failureSummary(text).getOrElse(Session.GenericFailure))
        else error
      // Remember a broken build (for the note on later results) until a load succeeds.
      if failedLoad then server.loadFailed(errors)
      else if failure.isEmpty && (SbtServer.reloads(commandLine) || text.contains(Session.LoadedMarker)) then server.loadSucceeded()
      ExecResult(
        commandLine,
        failure.isEmpty,
        failure,
        (System.nanoTime() - started) / 1_000_000,
        text,
        truncated,
        diagnostics.values.flatten.toList,
        clientJobs.toList,
      )
    }

  /** A setting's value as JSON with its type, read from the loaded build without running anything. */
  def setting(key: String): Either[String, (Json, String)] =
    val compact = key.trim.replaceAll("\\s*/\\s*", "/")
    query(compact).left.flatMap { error =>
      // sbt 1's `sbt/setting` parses only the old syntax for a configuration axis: `p/test:key`.
      Session.legacyKey(compact).fold(Left(error))(legacy => query(legacy).left.map(_ => error))
    }

  private def query(key: String): Either[String, (Json, String)] =
    val (id, future) = connection.request("sbt/setting", Json.Obj("setting" -> Json.Str(key)))
    try
      val result = await(id, future).asObject.map(_.fields.toMap).getOrElse(Map.empty)
      Right(result.getOrElse("value", Json.Null) -> result.get("contentType").collect { case Json.Str(t) => t }.getOrElse(""))
    catch case e: RpcError => Left(e.message)

  /** A Build Server Protocol request on this channel (initialized on first use). */
  def bsp(method: String, params: Json): Json =
    reloadIfChanged()
    bspInit()
    val (id, future) = connection.request(method, params)
    await(id, future)

  /**
   * Like [[bsp]], but None as soon as sbt reports that the request is queued behind
   * another command (sbt 2 says so; sbt 1 doesn't), or after `maxWaitMillis`. The
   * session must then be closed: sbt drops a disconnected channel's queued requests.
   */
  def bspUnlessBusy(method: String, params: Json, maxWaitMillis: Long = Long.MaxValue): Option[Json] =
    reloadIfChanged()
    bspInit()
    val queued = CompletableFuture[Json]()
    // "waiting for: shell" is sbt's idle prompt command, about to give way.
    onQueued = message => if !message.endsWith("waiting for: shell") then { queued.complete(Json.Null); () }
    val (id, future) = connection.request(method, params)
    try
      try CompletableFuture.anyOf(future, queued).get(maxWaitMillis, TimeUnit.MILLISECONDS)
      catch case _: TimeoutException => queued.complete(Json.Null)
      // A command just finishing also reports a queued request: give it a moment.
      if !future.isDone then
        try future.get(1, TimeUnit.SECONDS)
        catch case _: TimeoutException | _: java.util.concurrent.ExecutionException => ()
      if future.isDone then Some(await(id, future)) else None
    catch
      case e: InterruptedException =>
        cancel(id)
        throw e
      case e: java.util.concurrent.ExecutionException => throw e.getCause
    finally onQueued = _ => ()

  private def bspInit(): Unit =
    if !bspReady then
      val root = server.base.toAbsolutePath.normalize.toUri.toString
      val (id, future) = connection.request(
        "build/initialize",
        Json.Obj(
          "displayName"  -> Json.Str("sbta"),
          "version"      -> Json.Str(SbtaBuildInfo.version),
          "bspVersion"   -> Json.Str("2.1.0"),
          "rootUri"      -> Json.Str(root),
          "capabilities" -> Json.Obj("languageIds" -> Json.Arr(Json.Str("scala"), Json.Str("java"))),
        ),
      )
      await(id, future)
      connection.notify("build/initialized", Json.Obj())
      bspReady = true

  /** Collected diagnostics (e.g. after a BSP compile). */
  def collectedDiagnostics: List[Diagnostic] = synchronized(diagnostics.values.flatten.toList)
  def collectedOutput: String                = synchronized(Text.stripAnsi(output.toString))

  /**
   * Wait for a response. When the waiting thread is interrupted (the MCP client
   * cancelled the call), ask sbt to cancel the running command, then give up.
   */
  private def await(id: String, future: CompletableFuture[Json]): Json =
    val reboot = rebooting
    def rebootedIfSo(): Unit =
      if reboot.isDone && !(future.isDone && !future.isCompletedExceptionally) then
        val (execId, remaining) = reboot.get()
        throw Session.Rebooted(execId, remaining)
    try
      try CompletableFuture.anyOf(future, reboot).get()
      catch case _: java.util.concurrent.ExecutionException => () // the request failed: see below
      // A reboot closes the channel too (after its `shutdown` notification, so `reboot` is
      // complete by then): report the reboot, not the closed connection.
      rebootedIfSo()
      future.get()
    catch
      case _: InterruptedException =>
        cancel(id)
        throw InterruptedException("cancelled")
      case e: java.util.concurrent.ExecutionException =>
        rebootedIfSo()
        throw e.getCause

  /** Ask sbt to cancel request `id`, waiting (at most 5 s, even if interrupted again) for its answer. */
  private def cancel(id: String): Unit =
    log(s"cancelling sbt request $id")
    val cancel   = connection.request("sbt/cancelRequest", Json.Obj("id" -> Json.Str(id)))._2
    val deadline = System.nanoTime() + 5_000_000_000L
    var outcome  = Option.empty[String]
    // The caller may be interrupted again while waiting: keep waiting (bounded) for sbt's answer.
    while outcome.isEmpty && System.nanoTime() < deadline do
      try outcome = Some(cancel.get(math.max(1L, deadline - System.nanoTime()), TimeUnit.NANOSECONDS).toJson)
      catch
        case _: InterruptedException                    => ()
        case e: java.util.concurrent.ExecutionException => outcome = Some(s"failed: ${e.getCause.getMessage}")
        case _: TimeoutException                        => outcome = Some("no answer within 5 s")
    log(s"cancel $id: ${outcome.getOrElse("no answer within 5 s")}")

  def close(): Unit = connection.close()

object Session:


  /** What sbt 1 and 2 ask (`BuiltinCommands.doLoadFailed`) when loading the build fails. */
  val LoadFailedPrompt = "Project loading failed: (r)etry, (q)uit, (l)ast, or (i)gnore? (default: r)"
  val LoadFailedAnswer = "Project loading failed (sbta answered sbt's prompt with (i)gnore: sbt keeps the build it had loaded)"
  /** sbt's answer to a failed command (`-33000`, no message): a parse error or a failed task, said only in the output. */
  val GenericFailure = "the command failed (see the log for sbt's error)"

  private val TaskFailed = """^\[error\] (\([^)]+\) .+)$""".r
  private val ErrorLine  = """^\[error\] ?(.*)$""".r

  /**
   * Why a command failed, from its output: the tasks that failed, as sbt summarizes them
   * (`[error] (Test / testFull) sbt.TestsFailedException: Tests unsuccessful`), or else
   * the error that starts sbt's message for a command it couldn't parse.
   */
  def failureSummary(output: String): Option[String] =
    val errors = output.linesIterator.collect { case ErrorLine(e) => e.stripTrailing }.toVector
    val tasks  = output.linesIterator.collect { case TaskFailed(t) => t.trim }.toVector.distinct
    def timing(e: String) = e.startsWith("Total time:") || e.startsWith("elapsed time:")
    if tasks.nonEmpty then
      Some(tasks.take(5).mkString("; ") + (if tasks.size > 5 then s"; and ${tasks.size - 5} more" else ""))
    // No task failed: sbt couldn't parse the command, and its first error line says why.
    else errors.map(_.trim).find(e => e.nonEmpty && !timing(e)).map(_.take(500))

  /** What sbt prints when it (re)loads the build. */
  val LoadingMarker = "loading project definition from"
  /** What sbt prints when a load has succeeded. */
  val LoadedMarker = "set current project to"

  /** The output's last load started and didn't finish. */
  def loadIncomplete(output: String): Boolean =
    val started = output.lastIndexOf(LoadingMarker)
    started >= 0 && output.indexOf(LoadedMarker, started) < 0

  /**
   * The load error in a failed load's output: what sbt printed after starting to load,
   * without its `[info]` lines and the prompt (at most 20 lines).
   */
  def loadErrors(output: String): String =
    val lines = output.linesIterator.toList
    val from  = lines.lastIndexWhere(_.contains(LoadingMarker))
    lines.drop(from + 1)
      .filterNot(l => l.startsWith("[info]") || l.contains(LoadFailedAnswer) || l.contains("Use 'last' for the full log") || l.trim.isEmpty)
      .take(20).mkString("\n").take(3000)

  /** `p/Test/key` → `p/test:key`, `Test/key` → `test:key` (a capitalized middle part is a configuration). */
  def legacyKey(key: String): Option[String] =
    def config(c: String) = Option.when(c.headOption.exists(_.isUpper))(c.head.toLower.toString + c.tail)
    key.split('/').toList match
      case List(p, c, k) => config(c).map(cfg => s"$p/$cfg:$k")
      case List(c, k)    => config(c).map(cfg => s"$cfg:$k")
      case _             => None
  def open(server: SbtServer, socket: Path, log: String => Unit): Session =
    val session = Session(server, log)
    session.connect(socket)
    session

  /**
   * The `shutdown` notification's reboot part: sbt tells a channel it's closing it to
   * reboot (`[logShutdown, [execId, remainingCommands]]`; `null` for a plain shutdown),
   * for the client to reconnect and run what's left of the command line, as sbtn does.
   */
  def rebootOf(params: Json): Option[(String, String)] = params match
    case Json.Arr(items) =>
      items.lift(1).collect { case Json.Arr(pair) =>
        (pair.lift(0), pair.lift(1)) match
          case (Some(Json.Str(id)), Some(Json.Str(rest))) => Some((id, rest))
          case _                                          => None
      }.flatten
    case _ => None

  /** sbt closed the channel to reboot: the exec id it was running and the commands left after `reboot`. */
  final case class Rebooted(execId: String, remaining: String) extends IOException("sbt is rebooting")

object Text:
  private val Ansi = "\u001b\\[[0-9;?]*[ -/]*[@-~]".r
  // SGR codes whose ESC was dropped (XML can't hold it): `[7m`, `[0m`, `[91m`…
  private val BareSgr = "\\[[0-9;]{1,8}m".r

  def stripAnsi(s: String): String = BareSgr.replaceAllIn(Ansi.replaceAllIn(s, ""), "").replace("\r\n", "\n")

  /** The last `max` characters of `s`, and whether it was cut. */
  def lastChars(s: String, max: Int): (String, Boolean) =
    if s.length <= max then (s, false) else (s.takeRight(max), true)
