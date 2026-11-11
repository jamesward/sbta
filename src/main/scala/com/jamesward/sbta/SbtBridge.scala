package com.jamesward.sbta

import zio.json.*
import zio.json.ast.Json

import java.nio.file.{ Files, Path, StandardCopyOption }
import java.security.MessageDigest

/** Why the bridge didn't give a value. */
enum BridgeError(val message: String) derives CanEqual:
  /** The key doesn't exist (in the current project) or isn't defined. */
  case NoSuchKey(detail: String) extends BridgeError(detail)
  /** The key exists, but reading or running it failed. */
  case Failed(detail: String) extends BridgeError(detail)
  /** The bridge couldn't be loaded into this server (or didn't answer). */
  case Unavailable(detail: String) extends BridgeError(detail)

/**
 * Values from the build's running sbt server as data, without a plugin: sbta loads its
 * bridge (`sbt-bridge/`, a jar per sbt line embedded in sbta) into the server with sbt's
 * own `apply -cp <jar> sbt.sbtabridge.SbtaBridge`, which adds the command
 * `sbtaEval <id> <key>`. That reads a setting or runs a task and answers this channel with
 * a `sbta/result` notification holding the value as JSON. A server without the bridge
 * (new, or restarted) gets it on first use: sbt reports `sbtaEval` as not a valid command,
 * the bridge is applied, and the call is made again. It stays through `reload`.
 */
object SbtBridge:

  val Eval         = "sbtaEval"
  val ResultMethod = "sbta/result"
  private val Bridge = "sbt.sbtabridge.SbtaBridge"

  /** The value of `key` (a setting, or a task's result, which runs it), as sbt's shell would resolve the key. */
  def eval(server: SbtServer, session: Session, key: String): Either[BridgeError, Json] =
    attempt(session, key) match
      case Attempt.Done(result) => result
      case Attempt.NotInstalled =>
        install(server, session).flatMap { _ =>
          attempt(session, key) match
            case Attempt.Done(result) => result
            case Attempt.NotInstalled => Left(BridgeError.Unavailable(s"sbt still has no `$Eval` after applying sbta's bridge"))
        }

  private enum Attempt:
    case NotInstalled
    case Done(result: Either[BridgeError, Json])

  private def attempt(session: Session, key: String): Attempt =
    val id = java.util.UUID.randomUUID().toString
    val r  = session.exec(s"$Eval $id $key")
    session.takeBridgeResult(id) match
      case Some(text) => Attempt.Done(reply(text))
      case None if r.output.contains(s"Not a valid command: $Eval") => Attempt.NotInstalled
      case None =>
        Attempt.Done(Left(BridgeError.Unavailable(
          s"`${r.commandLine}` gave no result${r.error.fold("")(e => s": $e")}\n${Text.lastChars(r.output, 2000)._1}"
        )))

  /** The bridge's answer: `{"value": …}`, `{"noSuchKey": "…"}` or `{"error": "…"}`. */
  def reply(text: String): Either[BridgeError, Json] =
    text.fromJson[Json.Obj] match
      case Left(e) => Left(BridgeError.Unavailable(s"the bridge's answer isn't JSON ($e): ${text.take(200)}"))
      case Right(obj) =>
        def str(field: String) = obj.get(field).collect { case Json.Str(s) => s }
        (obj.get("value"), str("noSuchKey"), str("error")) match
          case (Some(value), _, _) => Right(value)
          case (_, Some(m), _)     => Left(BridgeError.NoSuchKey(m))
          case (_, _, Some(m))     => Left(BridgeError.Failed(m))
          case _                   => Left(BridgeError.Unavailable(s"unexpected answer from the bridge: ${text.take(200)}"))

  private def install(server: SbtServer, session: Session): Either[BridgeError, Unit] =
    val jar  = bridgeJar(server)
    val path = jar.toAbsolutePath.toString
    val r    = session.exec(s"apply -cp ${if path.exists(c => c.isWhitespace || c == '"') then "\"" + path.replace("\"", "\\\"") + "\"" else path} $Bridge")
    if r.success then Right(())
    else Left(BridgeError.Unavailable(s"loading sbta's bridge into sbt failed: ${r.error.getOrElse("")}\n${Text.lastChars(r.output, 2000)._1}"))

  /** The embedded bridge for the build's sbt line, written under `project/target/sbta/bridge/` (by content, so versions don't clash). */
  private def bridgeJar(server: SbtServer): Path =
    // No `sbt.version`: sbta's launcher (sbt 2) picks the version.
    val line     = if SbtServer.sbtVersion(server.base).exists(_.startsWith("1.")) then "sbt1" else "sbt2"
    val resource = s"/sbta-bridge/$line.jar"
    val bytes = scala.util.Using.resource(
      Option(getClass.getResourceAsStream(resource)).getOrElse(throw IllegalStateException(s"sbta is missing $resource"))
    )(_.readAllBytes())
    val digest = MessageDigest.getInstance("SHA-256").digest(bytes).take(8).map(b => f"$b%02x").mkString
    val dir    = server.runtimeDir.resolve("bridge")
    val jar    = dir.resolve(s"$line-$digest.jar")
    if !Files.isRegularFile(jar) then
      Files.createDirectories(dir)
      val temp = Files.createTempFile(dir, line, ".tmp")
      Files.write(temp, bytes)
      Files.move(temp, jar, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
    jar
