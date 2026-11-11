package sbt
package sbtabridge

import sbt.internal.server.NetworkChannel
import sbt.internal.util.{ AttributeKey, Attributed }
import sbt.internal.util.complete.Parser
import sjsonnew.BasicJsonProtocol._
import sjsonnew.shaded.scalajson.ast.unsafe._
import sjsonnew.support.scalajson.unsafe.{ CompactPrinter, Converter }

/**
 * sbta's bridge into a running sbt server, loaded into it with sbt's own
 * `apply -cp <this jar> sbt.sbtabridge.SbtaBridge` (no plugin, no change to the build).
 * It adds the command `sbtaEval <id> <key>`, which reads a setting or runs a task and
 * answers the channel that sent it with the notification
 * `sbta/result {"id": <id>, "result": <JSON text>}`, the text being
 * `{"value": <the value as JSON>}`, `{"noSuchKey": <message>}` or `{"error": <message>}`.
 *
 * Compiled once for sbt 1 (Scala 2.12, against the oldest sbt 1 sbta supports) and once
 * for sbt 2 (Scala 3); [[Versioned]] holds what differs. It lives in package `sbt` to reach
 * sbt's server internals (`StandardMain.exchange`, `NetworkChannel.jsonRpcNotify`).
 */
object SbtaBridge extends (State => State) {
  val Eval         = "sbtaEval"
  val ResultMethod = "sbta/result"

  def apply(s: State): State =
    if (s.definedCommands.exists(_.nameOption.contains(Eval))) s
    else s.copy(definedCommands = s.definedCommands :+ eval)

  private def eval: Command = Command.args(Eval, "<id> <key>") { (state, args) =>
    args.toList match {
      case id :: key if key.nonEmpty =>
        val result = evaluate(state, key.mkString(" "))
        StandardMain.exchange.channels.collectFirst {
          case c: NetworkChannel if state.source.exists(_.channelName == c.name) => c
        }.foreach(_.jsonRpcNotify(ResultMethod, Map("id" -> id, "result" -> CompactPrinter(result))))
        state
      case _ =>
        state.log.error(s"usage: $Eval <id> <key>")
        state.fail
    }
  }

  private def evaluate(state: State, keyText: String): JValue = {
    val extracted = Versioned.extract(state)
    Parser.parse(keyText, sbt.internal.Act.scopedKeyParser(extracted)) match {
      case Left(message) => obj("noSuchKey" -> JString(message))
      case Right(key) =>
        try
          Versioned.lookup(extracted, key) match {
            case None                   => obj("noSuchKey" -> JString(s"$keyText is not defined"))
            case Some(_: InputTask[_])  => obj("error" -> JString(s"$keyText is an input task: run it as a command"))
            case Some(_: Task[_]) =>
              // The parsed key is untyped; a key holding a Task is a task key.
              val task = Scoped.scopedTask(key.scope, key.key.asInstanceOf[AttributeKey[Task[Any]]])
              obj("value" -> toJson(extracted.runTask(task, state)._2, Versioned.toPath(extracted), 0))
            case Some(value) => obj("value" -> toJson(value, Versioned.toPath(extracted), 0))
          }
        catch { case e: Throwable => obj("error" -> JString(message(e))) }
    }
  }

  private def obj(fields: (String, JValue)*): JValue = JObject(fields.map { case (k, v) => JField(k, v) }.toArray)

  private def message(e: Throwable): String = {
    val causes = e match {
      case incomplete: Incomplete => Incomplete.allExceptions(incomplete).toList
      case other                  => List(other)
    }
    causes
      .map(c => Iterator.iterate[Throwable](c)(_.getCause).takeWhile(_ != null).toList.last)
      .map(c => Option(c.getMessage).filter(_.nonEmpty).getOrElse(c.getClass.getName))
      .distinct
      .mkString("; ")
  }

  /**
   * The value as JSON: sbt's own JSON for an update report; strings, numbers, booleans,
   * files (absolute paths), options, collections and string-keyed maps as themselves; a
   * module or configuration as sbt writes it; anything else as its `toString`.
   */
  private def toJson(value: Any, toPath: xsbti.VirtualFileRef => java.nio.file.Path, depth: Int): JValue =
    if (depth > 16) JString(String.valueOf(value))
    else {
      def nested(v: Any) = toJson(v, toPath, depth + 1)
      value match {
        case null                                   => JNull
        case ()                                     => JNull
        case r: sbt.librarymanagement.UpdateReport =>
          implicit val format: sjsonnew.JsonFormat[sbt.librarymanagement.UpdateReport] =
            sbt.librarymanagement.LibraryManagementCodec.UpdateReportFormat
          Converter.toJsonUnsafe(r)
        case s: String                              => JString(s)
        case b: Boolean                             => JBoolean(b)
        case n: Int                                 => JNumber(n.toString)
        case n: Long                                => JNumber(n.toString)
        case n: Short                               => JNumber(n.toString)
        case n: Byte                                => JNumber(n.toString)
        case d: Double if !d.isNaN && !d.isInfinite => JNumber(d.toString)
        case f: Float if !f.isNaN && !f.isInfinite  => JNumber(f.toString)
        case f: java.io.File                        => JString(f.getAbsolutePath)
        case p: java.nio.file.Path                  => JString(p.toAbsolutePath.toString)
        case v: xsbti.VirtualFileRef                => JString(toPath(v).toAbsolutePath.toString)
        case u: java.net.URI                        => JString(u.toString)
        case u: java.net.URL                        => JString(u.toString)
        case c: sbt.librarymanagement.Configuration => JString(c.name)
        case a: Attributed[_]                       => nested(a.data)
        case o: Option[_]                           => o.fold[JValue](JNull)(nested)
        case m: scala.collection.Map[_, _] if m.keys.forall(_.isInstanceOf[String]) =>
          JObject(m.toSeq.map { case (k, v) => JField(k.toString, nested(v)) }.toArray)
        case it: Iterable[_]                        => JArray(it.iterator.map(nested).toArray)
        case arr: Array[_]                          => JArray(arr.iterator.map(nested).toArray)
        case other                                  => JString(String.valueOf(other))
      }
    }
}
