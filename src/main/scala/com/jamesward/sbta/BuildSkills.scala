package com.jamesward.sbta

import com.jamesward.ziohttp.mcp.*
import zio.*
import zio.json.*

import java.io.IOException
import java.nio.file.{ Files, Path }

/**
 * The build's agent skills, served over MCP's Skills extension (`skills/list`,
 * `skills/get`, `resources/directory/read`, and `resources/read` for their files): the
 * SkillsJars of sbt-skillsjars' `skillsJarsFiles`, read through [[SbtBridge]]. That is
 * every project's `skillsJarsConfigurations` (`Skills` unless the project says
 * otherwise), in every build, one jar per module at its highest revision. A SkillsJar on
 * a program's classpath is served only when its project scans that configuration too.
 *
 * The skills are read on first use and again once the build files or sbt's server
 * change. Without sbt-skillsjars (or with one too old for `skillsJarsFiles`) there are none.
 */
final class BuildSkills(server: SbtServer, log: String => Unit):
  import BuildSkills.*

  private val state = Unsafe.unsafe(implicit u => Ref.Synchronized.unsafe.make(Option.empty[Loaded]))

  /** The skills of the build as it is now. */
  def current: IO[McpSkillsSourceError, McpSkillsJars] =
    state.modifyZIO { cached =>
      ZIO.succeedBlocking(server.buildFingerprint()).flatMap { fingerprint =>
        cached match
          case Some(loaded) if loaded.fingerprint == fingerprint => ZIO.succeed(loaded.skills -> cached)
          // Fingerprinted after loading: loading can start sbt's server, which is part of the fingerprint.
          case _ =>
            load.flatMap(skills => ZIO.succeedBlocking(server.buildFingerprint()).map(after => skills -> Some(Loaded(after, skills))))
      }
    }

  private def load: IO[McpSkillsSourceError, McpSkillsJars] =
    ZIO.attemptBlockingInterrupt(skillsJars())
      .tapError(e => ZIO.succeed(log(s"reading the build's SkillsJars failed: ${e.getMessage}")))
      .mapError(e => McpSkillsSourceError.Internal(s"reading the build's SkillsJars failed: ${e.getMessage}"))
      .flatMap(jars =>
        McpSkillsJars.load(jars)
          .tap(skills => ZIO.succeed(log(s"serving ${skills.entries.size} skill(s) from ${jars.size} SkillsJar(s)")))
          .mapError(e => McpSkillsSourceError.Internal(s"loading the build's SkillsJars failed: ${e.getMessage}"))
      )

  /** `skillsJarsFiles`' jars (none when the build has no such task). */
  private def skillsJars(): Chunk[Path] =
    server.session(s => SbtBridge.eval(server, s, FilesTask)) match
      case Left(BridgeError.NoSuchKey(_)) =>
        log(s"the build has no `$FilesTask` (sbt-skillsjars $MinimumPlugin or later): no skills to serve")
        Chunk.empty
      case Left(error) => throw IOException(s"`$FilesTask`: ${error.message}")
      case Right(value) =>
        val files = value.as[List[String]].fold(e => throw IOException(s"`$FilesTask` isn't a list of files ($e): ${value.toJson.take(200)}"), identity)
        val (present, missing) = files.map(Path.of(_)).partition(Files.isRegularFile(_))
        missing.foreach(jar => log(s"SkillsJars: no jar at $jar; skipped"))
        Chunk.fromIterable(present)

  val source: McpSkillsSource[Any] = new McpSkillsSource[Any]:
    def list(params: McpSkillsListParams, ctx: McpRequestContext) = current.flatMap(_.source.list(params, ctx))
    def get(uri: McpSkillUri, ctx: McpRequestContext)            = current.flatMap(_.source.get(uri, ctx))

  val directory: McpSkillsDirectorySource[Any] = new McpSkillsDirectorySource[Any]:
    def read(params: ResourceDirectoryReadParams, ctx: McpRequestContext) = current.flatMap(_.directory.read(params, ctx))

  /** Each skill's files through `resources/read`, and each `SKILL.md` in `resources/list`. */
  val resources: McpResourceSource[Any] = new McpResourceSource[Any]:
    def listResources(ctx: McpToolContext) =
      current.foldZIO(_ => ZIO.succeed(Chunk.empty), _.resources.listResources(ctx))
    def listResourceTemplates(ctx: McpToolContext) = ZIO.succeed(Chunk.empty)
    def readResource(uri: String, ctx: McpToolContext) =
      current.mapError(e => ToolError(message(e))).flatMap(_.resources.readResource(uri, ctx))

  def extensions: McpExtensions[Any] = McpSkills.withDirectory(source, directory)

object BuildSkills:

  /** sbt-skillsjars' task listing the build's SkillsJars, and the first release with it. */
  val FilesTask     = "skillsJarsFiles"
  val MinimumPlugin = "0.1.0"

  private final case class Loaded(fingerprint: Vector[String], skills: McpSkillsJars)

  private def message(error: McpSkillsSourceError): String = error match
    case McpSkillsSourceError.InvalidParams(m) => m
    case McpSkillsSourceError.Internal(m)      => m

  /**
   * The skills part of the server's instructions: for a client that declared the Skills
   * extension, that the build's skills come over MCP; for any other, how to extract them.
   */
  def instructions(ctx: McpRequestContext): String =
    if ctx.extensionCapabilities.contains(McpSkills.Id) then
      "The agent skills the build declares (SkillsJars, with sbt-skillsjars) are served through the MCP Skills extension."
    else
      "If the build declares agent skills (SkillsJars, with sbt-skillsjars), extract them with `sbt-task`: " +
        "`extractSkillsJars <your agent's skills directory>` (e.g. `.kiro/skills` or `.claude/skills`), then read the relevant ones."
