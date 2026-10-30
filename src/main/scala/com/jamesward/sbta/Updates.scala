package com.jamesward.sbta

import zio.json.*
import zio.json.ast.Json

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path, StandardCopyOption }
import java.security.MessageDigest
import scala.util.control.NonFatal

/**
 * Newer sbta releases: a check of the GitHub releases at most once a day (per user,
 * cached in `~/.cache/sbt/boot/sbta/latest.json`, in the background, never delaying
 * a command), the notice that a newer release is out, and `--update`, which replaces
 * the build's `sbta` and `sbta.bat` with a release's (checked against its
 * `SHA256SUMS`). The release scripts pin their version, so an update is a change to
 * the build that someone commits.
 *
 * Only releases count (`X.Y.Z`, no pre-release or build suffix), newest by semver, and
 * only a release of sbta is compared: a development build (sbt-dynver's
 * `0.0.2-5-abcd1234`) is never told to update.
 *
 * `SBTA_NO_UPDATE_CHECK` (any value) or `CI=true` turns the check off;
 * `SBTA_NO_UPDATE_NOTICE` turns the notices off.
 */
object Updates:

  /** A semver version: `major.minor.patch`, with an optional pre-release (`-RC1`) and build (`+abc`). */
  final case class Version(major: Int, minor: Int, patch: Int, preRelease: Option[String]):
    def isRelease: Boolean = preRelease.isEmpty
    override def toString: String = s"$major.$minor.$patch${preRelease.fold("")("-" + _)}"

  object Version:
    private val Pattern = """v?(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-([0-9A-Za-z.-]+))?(?:\+[0-9A-Za-z.-]+)?""".r

    def parse(text: String): Option[Version] = text.trim match
      case Pattern(major, minor, patch, pre) =>
        for
          ma <- major.toIntOption
          mi <- minor.toIntOption
          pa <- patch.toIntOption
        yield Version(ma, mi, pa, Option(pre))
      case _ => None

    /** Semver precedence: a pre-release is before its release; pre-release fields compare numerically or as text. */
    given Ordering[Version] = (a: Version, b: Version) =>
      val core = Ordering[(Int, Int, Int)].compare((a.major, a.minor, a.patch), (b.major, b.minor, b.patch))
      if core != 0 then core
      else
        (a.preRelease, b.preRelease) match
          case (None, None)       => 0
          case (None, Some(_))    => 1
          case (Some(_), None)    => -1
          case (Some(x), Some(y)) => comparePre(x.split('.').toList, y.split('.').toList)

    private def comparePre(a: List[String], b: List[String]): Int = (a, b) match
      case (Nil, Nil)    => 0
      case (Nil, _)      => -1
      case (_, Nil)      => 1
      case (x :: xs, y :: ys) =>
        val c = (x.toIntOption, y.toIntOption) match
          case (Some(i), Some(j)) => i.compare(j)
          case (Some(_), None)    => -1
          case (None, Some(_))    => 1
          case (None, None)       => x.compare(y)
        if c != 0 then c else comparePre(xs, ys)

  /** The newest release (by semver) in a GitHub `releases` API response, ignoring drafts, pre-releases and non-semver tags. */
  def latestRelease(releasesJson: String): Option[Version] =
    releasesJson.fromJson[Json].toOption.collect { case Json.Arr(items) => items.toList }.getOrElse(Nil).flatMap { item =>
      val fields = item.asObject.map(_.fields.toMap).getOrElse(Map.empty)
      def flag(name: String) = fields.get(name).contains(Json.Bool(true))
      if flag("draft") || flag("prerelease") then None
      else fields.get("tag_name").collect { case Json.Str(tag) => tag }.flatMap(Version.parse).filter(_.isRelease)
    }.maxOption

  // ---- the daily check ----

  /** What the last check found: when it ran (epoch millis, also when it failed) and the newest release it saw. */
  final case class Checked(checkedAt: Long, latest: Option[String])
  object Checked:
    given JsonCodec[Checked] = DeriveJsonCodec.gen[Checked]

  val CheckInterval: Long = 24L * 60 * 60 * 1000

  val ReleasesApi  = "https://api.github.com/repos/jamesward/sbta/releases?per_page=100"
  val Releases     = "https://github.com/jamesward/sbta/releases/download"
  /** The `sbta` script of the release GitHub marks latest (a redirect to the asset, not the API). */
  val LatestScript = "https://github.com/jamesward/sbta/releases/latest/download/sbta"

  private val PinPattern = """(?m)^declare sbta_version="([^"]+)"""".r

  /** The release version a release `sbta` script pins. */
  def scriptVersion(script: String): Option[Version] =
    PinPattern.findFirstMatchIn(script).flatMap(m => Version.parse(m.group(1))).filter(_.isRelease)

  /**
   * The newest release: by semver from the releases API, or else the version that
   * GitHub's latest release's `sbta` script pins. Some sandboxes (e.g. cloud agent
   * sessions) block api.github.com and `/releases/latest` but let release downloads
   * through. On failure, what each source said.
   */
  def latest(get: String => String = fetch): Either[String, Version] =
    def attempt(url: String)(parse: String => Option[Version]): Either[String, Version] =
      try parse(get(url)).toRight(s"$url: no release found")
      catch case NonFatal(e) => Left(s"$url: ${Option(e.getMessage).getOrElse(e.toString)}")
    attempt(ReleasesApi)(latestRelease).left.flatMap { api =>
      attempt(LatestScript)(scriptVersion).left.map(script => s"$api; $script")
    }

  def cacheFile: Path =
    Path.of(System.getProperty("user.home")).resolve(".cache").resolve("sbt").resolve("boot").resolve("sbta").resolve("latest.json")

  def read(file: Path): Option[Checked] =
    try if Files.isRegularFile(file) then Files.readString(file).fromJson[Checked].toOption else None
    catch case NonFatal(_) => None

  private def write(file: Path, checked: Checked): Unit =
    try
      Files.createDirectories(file.getParent)
      val temp = Files.createTempFile(file.getParent, "latest", ".tmp")
      try
        Files.writeString(temp, checked.toJson)
        Files.move(temp, file, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
      finally Files.deleteIfExists(temp)
    catch case NonFatal(_) => ()

  /** The check is off: `SBTA_NO_UPDATE_CHECK` is set, or on CI. */
  def checkDisabled(env: String => Option[String] = sys.env.get): Boolean =
    env("SBTA_NO_UPDATE_CHECK").exists(_.nonEmpty) || env("CI").exists(_.equalsIgnoreCase("true"))

  def noticesDisabled(env: String => Option[String] = sys.env.get): Boolean =
    env("SBTA_NO_UPDATE_NOTICE").exists(_.nonEmpty)

  /**
   * Check for the newest release unless the last check (successful or not) is less
   * than a day old, and record the result. `find` looks it up (see [[latest]]).
   */
  def refresh(file: Path, now: Long, find: () => Either[String, Version]): Option[Checked] =
    val previous = read(file)
    if previous.exists(p => now - p.checkedAt < CheckInterval && now >= p.checkedAt) then previous
    else
      val found   = try find().toOption catch case NonFatal(_) => None
      // A failed check keeps what the last one found, and waits a day too.
      val checked = Checked(now, found.map(_.toString).orElse(previous.flatMap(_.latest)))
      write(file, checked)
      Some(checked)

  /** [[refresh]] of `file` against GitHub, in a daemon thread; `done` gets the result. Nothing when the check is off. */
  def refreshInBackground(file: Path = cacheFile)(done: Option[Checked] => Unit = _ => ()): Unit =
    if !checkDisabled() then
      val thread = Thread(() => done(refresh(file, System.currentTimeMillis(), () => latest())), "sbta-update-check")
      thread.setDaemon(true)
      thread.start()

  /** The newest release, when it's newer than `current` (a release itself). */
  def newer(current: String, checked: Option[Checked]): Option[Version] =
    for
      cur    <- Version.parse(current).filter(_.isRelease)
      latest <- checked.flatMap(_.latest).flatMap(Version.parse)
      if summon[Ordering[Version]].gt(latest, cur)
    yield latest

  /** The one-line notice for a terminal. */
  def notice(current: String, latest: Version): String =
    s"sbta $latest is available (this build uses $current): run ./sbta --update, then commit sbta and sbta.bat"

  /** The note an MCP client sees: on every tool result, and in the server's instructions. */
  def mcpNote(current: String, latest: Version): String =
    s"sbta $latest is available (this build uses $current). Tell the user; to update, call the `update-sbta` tool " +
      s"(or run ./sbta --update), then commit sbta and sbta.bat and restart the MCP client."

  /** The version a release `sbta` script pins (`declare sbta_version="…"`), if `dir` has one. */
  def pinnedVersion(dir: Path): Option[String] =
    try
      val script = dir.resolve("sbta")
      if Files.isRegularFile(script) then PinPattern.findFirstMatchIn(Files.readString(script)).map(_.group(1)) else None
    catch case NonFatal(_) => None

  // ---- --update ----

  /**
   * Replace `dir`'s `sbta` and `sbta.bat` with release `version`'s (the newest, if
   * not given), from `releases` (`<releases>/v<version>/…`), each checked against
   * the release's `SHA256SUMS`. With `deferBat`, `sbta.bat` is written as
   * `sbta.bat.new`, for the running `sbta.bat` to move into place when it's done
   * (cmd.exe reads a batch file while running it). Returns what it did, or why not.
   */
  def update(
      dir: Path,
      version: Option[String],
      releases: String,
      latest: () => Either[String, Version],
      deferBat: Boolean,
      get: String => Array[Byte] = fetchBytes,
  ): Either[String, List[String]] =
    pinnedVersion(dir) match
      case None =>
        Left(s"$dir has no release sbta script (one with `declare sbta_version=`) to update")
      case Some(pinned) =>
        val target = version match
          case Some(v) => Version.parse(v).toRight(s"not a version: $v")
          case None    =>
            latest().left.map(why => s"couldn't find the newest sbta release ($why); try --update <version>")
        target.flatMap { v =>
          if Version.parse(pinned).contains(v) then Right(List(s"sbta and sbta.bat are already $v"))
          else
            val base = s"${releases.stripSuffix("/")}/v$v"
            try
              val sums = parseSums(String(get(s"$base/SHA256SUMS"), StandardCharsets.UTF_8))
              val files = List("sbta", "sbta.bat").map { name =>
                val bytes    = get(s"$base/$name")
                val expected = sums.getOrElse(name, throw IllegalStateException(s"SHA256SUMS of $v has no $name"))
                if sha256(bytes) != expected then throw IllegalStateException(s"$name of $v doesn't match its SHA256SUMS")
                name -> bytes
              }
              val written = files.map { (name, bytes) =>
                val file = if name == "sbta.bat" && deferBat then dir.resolve("sbta.bat.new") else dir.resolve(name)
                replace(file, bytes, executable = name == "sbta")
                name
              }
              Right(
                s"updated ${written.mkString(" and ")} from $pinned to $v" ::
                  (if deferBat then List("sbta.bat is replaced when this command finishes") else Nil) :::
                  List("commit sbta and sbta.bat; the next ./sbta downloads sbta " + v + ", and an MCP client needs a restart to use it")
              )
            catch case NonFatal(e) => Left(s"update to $v failed: ${Option(e.getMessage).getOrElse(e.toString)}")
        }

  /** `SHA256SUMS` lines (`<hex>  <name>`, as sha256sum writes) by file name. */
  def parseSums(text: String): Map[String, String] =
    text.linesIterator.map(_.trim).filter(_.nonEmpty).flatMap { line =>
      line.split("\\s+", 2) match
        case Array(hash, name) => Some(name.stripPrefix("*") -> hash.toLowerCase)
        case _                 => None
    }.toMap

  def sha256(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"$b%02x").mkString

  /** A new file moved into place (a running bash keeps reading the script it opened). */
  private def replace(file: Path, bytes: Array[Byte], executable: Boolean): Unit =
    val temp = Files.createTempFile(file.toAbsolutePath.getParent, file.getFileName.toString, ".tmp")
    try
      Files.write(temp, bytes)
      if executable then temp.toFile.setExecutable(true, false)
      Files.move(temp, file, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
    finally Files.deleteIfExists(temp)

  // ---- HTTP (and file: URLs, for mirrors and tests) ----

  def fetchBytes(url: String): Array[Byte] =
    val connection = URI.create(url).toURL.openConnection()
    connection.setConnectTimeout(5000)
    connection.setReadTimeout(10000)
    connection.setRequestProperty("User-Agent", s"sbta/${SbtaBuildInfo.version}")
    connection.setRequestProperty("Accept", "application/vnd.github+json")
    val in = connection.getInputStream
    try in.readAllBytes() finally in.close()

  def fetch(url: String): String = String(fetchBytes(url), StandardCharsets.UTF_8)

  /** The releases to download from: `$SBTA_RELEASES_URL` (a mirror), or GitHub's. */
  def releasesBase(env: String => Option[String] = sys.env.get): String =
    env("SBTA_RELEASES_URL").filter(_.nonEmpty).getOrElse(Releases)

  // ---- entry points ----

  /** Before sbt runs (not `--mcp`): the notice on stderr, from the last check, and a check in the background. */
  def beforeLaunch(args: Seq[String]): Unit =
    // The release `sbta` script shows the notice itself (sbt 2's sbtn never runs this JVM
    // for a command), and the scripts' own helper calls (`--rt-ext-dir`) aren't sessions.
    val helper = args.exists(a => a == "--rt-ext-dir" || a == "--export-rt")
    if !helper && !noticesDisabled() && !sys.env.contains("SBTA_UPDATE_NOTICE_SHOWN") then
      newer(SbtaBuildInfo.version, read(cacheFile)).foreach(v => System.err.println(s"[info] ${notice(SbtaBuildInfo.version, v)}"))
    if !helper then refreshInBackground()()

  /**
   * `sbta --update [<version>]` (or `--update=<version>`): update the scripts in
   * `$SBTA_SCRIPT_DIR` (set by the scripts to their own directory), or the current
   * directory. Exits.
   */
  def cli(args: List[String]): Nothing =
    val i       = args.indexWhere(a => a == "--update" || a.startsWith("--update="))
    val version = args(i) match
      case a if a.startsWith("--update=") => Some(a.stripPrefix("--update=")).filter(_.nonEmpty)
      case _                              => args.lift(i + 1).filterNot(_.startsWith("-"))
    val fromScript = sys.env.get("SBTA_SCRIPT_DIR").filter(_.nonEmpty)
    val dir        = fromScript.map(Path.of(_)).getOrElse(Path.of("")).toAbsolutePath.normalize
    val result = update(
      dir,
      version,
      releasesBase(),
      () => latest(),
      deferBat = SbtServer.windows && fromScript.isDefined,
    )
    result match
      case Right(lines) =>
        lines.foreach(l => println(s"sbta: $l"))
        sys.exit(0)
      case Left(error) =>
        System.err.println(s"sbta: $error")
        sys.exit(1)
