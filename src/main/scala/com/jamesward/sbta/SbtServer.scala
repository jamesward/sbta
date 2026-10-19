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
enum StartMode:
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
  private val bootLog    = runtimeDir.resolve("boot.log")

  /** How long a call waits for sbt to start (a first start may download a lot). */
  private val StartWait = java.time.Duration.ofMinutes(15)

  @volatile private var lastFailure: Option[(Long, String)] = None

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
        case Some((at, failure)) if System.currentTimeMillis() - at < 30_000 => throw IOException(failure)
        case _                                                              => ()
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

  /** The socket `active.json` names, unchecked. */
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
    log(s"starting sbt in $base (output: $bootLog)")
    val started = System.nanoTime()
    var code    = runSbt("--client")
    if code != 0 && socket().isEmpty then
      log(s"`sbt --client` exited with $code; trying the JVM client")
      code = runSbt("--jvm-client")
    val deadline = started + StartWait.toNanos
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
          s"sbt did not start in $base (exit code $code). Last lines of $bootLog:\n${SbtServer.tail(bootLog, 25)}"
        lastFailure = Some(System.currentTimeMillis() -> failure)
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

  def tail(file: Path, lines: Int): String =
    try
      val all = Files.readAllLines(file, StandardCharsets.UTF_8)
      String.join("\n", all.subList(math.max(0, all.size - lines), all.size))
    catch case NonFatal(_) => "(no output)"

/** A compiler diagnostic from BSP `build/publishDiagnostics`. */
final case class Diagnostic(severity: String, file: Option[String], line: Option[Int], column: Option[Int], message: String)

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
final class Session private (server: SbtServer, connection: SbtConnection, log: String => Unit) extends AutoCloseable:

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
    val fingerprint = server.buildFingerprint()
    reset()
    val started      = System.nanoTime()
    val (id, future) = connection.request("sbt/exec", Json.Obj("commandLine" -> Json.Str(commandLine)))
    val error =
      try
        await(id, future)
        None
      catch
        // -33000 with an empty message: sbt couldn't parse the command (the reason is in the log).
        case e: RpcError if e.code == -33000 && e.message.isEmpty => Some("sbt could not parse the command (see the log)")
        // -33000 with the command line itself: the command was cancelled or not run.
        case e: RpcError if e.code == -33000 => Some(s"not completed: ${e.message} (cancelled, or an earlier part failed)")
        case e: RpcError                     => Some(e.message)
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
    try future.get()
    catch
      case _: InterruptedException =>
        cancel(id)
        throw InterruptedException("cancelled")
      case e: java.util.concurrent.ExecutionException => throw e.getCause

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
    var session: Session = null
    val connection = SbtConnection.open(socket, (method, params) => if session != null then session.handle(method, params))
    session = Session(server, connection, log)
    val init = Json.Obj("initializationOptions" -> Json.Obj("skipAnalysis" -> Json.Bool(true)))
    // Both at once, as sbt's own client does: with `sbt/attach` sent only after the
    // `initialize` answer, sbt 1 asks a failed build load's "(r)etry, (q)uit…" on the
    // server's own stdin instead of this channel's, and hangs.
    val initialized = connection.request("initialize", init)
    val attached    = connection.request("sbt/attach", Json.Obj("interactive" -> Json.Bool(false)))
    session.await0(initialized)
    session.await0(attached)
    session

  extension (s: Session)
    private def await0(request: (String, CompletableFuture[Json])): Json =
      try request._2.get(30, TimeUnit.SECONDS)
      catch
        case _: TimeoutException => throw IOException("sbt did not answer the connection handshake within 30 s")
        case e: java.util.concurrent.ExecutionException => throw e.getCause

object Text:
  private val Ansi = "\u001b\\[[0-9;?]*[ -/]*[@-~]".r
  // SGR codes whose ESC was dropped (XML can't hold it): `[7m`, `[0m`, `[91m`…
  private val BareSgr = "\\[[0-9;]{1,8}m".r

  def stripAnsi(s: String): String = BareSgr.replaceAllIn(Ansi.replaceAllIn(s, ""), "").replace("\r\n", "\n")

  /** The last `max` characters of `s`, and whether it was cut. */
  def lastChars(s: String, max: Int): (String, Boolean) =
    if s.length <= max then (s, false) else (s.takeRight(max), true)
