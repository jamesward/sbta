package com.jamesward.sbta

import zio.json.*
import zio.json.ast.Json

import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path }

/**
 * `sbta --mcp --setup <client>[,<client>…]`: register this build's MCP server
 * (`./sbta --mcp`) in the project's MCP client configuration, and print the guidance
 * for the project's AGENTS.md. Existing configuration is kept: only the `sbta` server
 * entry is added or replaced, so running it again changes nothing.
 */
object Setup:

  val ServerName = "sbta"

  /** The server entry (Kiro, Claude Code and Cursor share this shape). */
  val entry: Json.Obj = Json.Obj(
    "type"    -> Json.Str("stdio"),
    "command" -> Json.Str("./sbta"),
    "args"    -> Json.Arr(Json.Str("--mcp")),
    // Commands can run long (a first compile, a test suite): wait as long as sbta does.
    "timeout" -> Json.Num(1800000),
  )

  enum Client(val id: String, val label: String, val config: String) derives CanEqual:
    case Kiro   extends Client("kiro", "Kiro", ".kiro/settings/mcp.json")
    case Claude extends Client("claude", "Claude Code", ".mcp.json")
    case Cursor extends Client("cursor", "Cursor", ".cursor/mcp.json")

  object Client:
    def parse(ids: String): Either[String, List[Client]] =
      val requested = ids.split(',').map(_.trim.toLowerCase).filter(_.nonEmpty).toList
      val unknown   = requested.filterNot(id => values.exists(_.id == id))
      if requested.isEmpty || unknown.nonEmpty then
        Left(s"unknown MCP client ${unknown.mkString(", ")}: expected ${values.map(_.id).mkString(", ")} (comma-separated)")
      else Right(requested.distinct.map(id => values.find(_.id == id).get))

  /** Write the configuration for `clients` under `base`; returns what was done, one line per file. */
  def run(base: Path, clients: List[Client]): List[String] =
    clients.flatMap { client =>
      val config = base.resolve(client.config)
      val written = upsert(config) { root =>
        val servers = root.get("mcpServers").flatMap(_.asObject).getOrElse(Json.Obj())
        root.upserted("mcpServers", servers.upserted(ServerName, entry))
      }
      val claudeSettings =
        if client != Client.Claude then Nil
        else
          // Claude Code asks before starting a project's MCP server unless it's approved here.
          val settings = base.resolve(".claude/settings.json")
          List(settings -> upsert(settings) { root =>
            val enabled = root.get("enabledMcpjsonServers").collect { case Json.Arr(xs) => xs.toList }.getOrElse(Nil)
            if enabled.contains(Json.Str(ServerName)) then root
            else root.upserted("enabledMcpjsonServers", Json.Arr((enabled :+ Json.Str(ServerName))*))
          })
      ((config -> written) :: claudeSettings).map { (file, changed) =>
        s"${client.label}: ${base.relativize(file)} ${if changed then "updated" else "already up to date"}"
      }
    }

  /** Rewrite the JSON object in `file` (created if missing) with `f`. Returns whether it changed. */
  private def upsert(file: Path)(f: Json.Obj => Json.Obj): Boolean =
    val before =
      if Files.isRegularFile(file) then
        Files.readString(file, StandardCharsets.UTF_8).fromJson[Json] match
          case Right(obj: Json.Obj) => Some(obj)
          case Right(_)             => throw IllegalArgumentException(s"$file isn't a JSON object")
          case Left(error)          => throw IllegalArgumentException(s"$file isn't valid JSON ($error); fix or remove it")
      else None
    val after = f(before.getOrElse(Json.Obj()))
    if before.contains(after) then false
    else
      Option(file.getParent).foreach(Files.createDirectories(_))
      Files.writeString(file, after.toJsonPretty + "\n", StandardCharsets.UTF_8)
      true

  extension (obj: Json.Obj)
    /** `obj` with `key` set to `value`, in place if present, else appended. */
    private def upserted(key: String, value: Json): Json.Obj =
      if obj.fields.exists(_._1 == key) then Json.Obj(obj.fields.map((k, v) => if k == key then k -> value else k -> v)*)
      else Json.Obj((obj.fields :+ (key -> value))*)

  /** The guidance for the project's AGENTS.md. */
  val agentsGuidance: String =
    s"""## Build, Test & Dev Workflow
       |
       |- Use the MCP server `$ServerName` (this build's `./sbta --mcp`) for all sbt work: build, test and run
       |  with `compile`, `test` (`tests` for specific classes, `full` for all), `run`, `clean` and
       |  `publish-local`; inspect the build with `projects`, `test-names`, `main-classes`, `dependencies` and
       |  `show`. Use `sbt-task` for any other command (e.g. `reload`) and `list-tasks` to find tasks. Don't run
       |  `sbt` or `./sbta` in a shell while the MCP tools are available; if the server is unavailable, say so and
       |  fall back to `./sbta <command>`.
       |- After editing Scala 3 sources, check them with `check` (pass the edited files: a fast type check
       |  without compiling; `"scope":"module"` after changing an API), then run `compile` and `test` before
       |  declaring the work done.
       |- For Scala symbols use `glob-search` (find or list), `inspect` (members and signatures) and
       |  `symbol-location` (source location) rather than text search or guessing APIs. JavaDoc/ScalaDoc
       |  lookups are the proxied javadocs.dev tools of the same server.
       |""".stripMargin
