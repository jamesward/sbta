package com.jamesward.sbta

import java.util.concurrent.atomic.AtomicReference
import scala.util.control.NonFatal

/** Why the MCP server process ends, as its last log line says. */
enum ExitReason derives CanEqual:
  case StdinClosed
  case Signal(name: String, number: Int)
  case Failed(detail: String)

  def message: String = this match
    case StdinClosed          => "stdin closed (sbt keeps running)"
    case Signal(name, number) => s"SIG$name ($number)"
    case Failed(detail)       => s"the MCP server failed: $detail"

/**
 * Logs every way `sbta --mcp` ends, so a server that a client (or anything else) stopped
 * leaves a line: stdin closing, a failure, SIGTERM, SIGINT or SIGHUP (each logged by
 * name, then the JVM exits with 128 + its number, as it would without a handler), and
 * any other shutdown (a `System.exit` elsewhere). SIGKILL can't be caught, so a server
 * whose log just stops was killed that way, or crashed (see `hs_err_pid<pid>.log`).
 */
final class McpExit(log: String => Unit):

  private val reason = AtomicReference[Option[ExitReason]](None)

  def install(): Unit =
    Runtime.getRuntime.addShutdownHook(Thread(() => log(s"exiting: ${reason.get.fold("JVM shutdown (System.exit or an unhandled signal)")(_.message)}"), "sbta-exit-log"))
    List("TERM", "INT", "HUP").foreach { name =>
      try
        sun.misc.Signal.handle(sun.misc.Signal(name), (signal: sun.misc.Signal) => exit(ExitReason.Signal(name, signal.getNumber), 128 + signal.getNumber))
        ()
      catch
        // Ignored by the parent (`nohup` ignores HUP) or reserved by the JVM (`-Xrs`): the default stays.
        case NonFatal(e) => log(s"not logging SIG$name: ${e.getMessage}")
    }

  /** Exits the JVM with `code`, recording `why` for the shutdown hook's line. */
  def exit(why: ExitReason, code: Int): Nothing =
    reason.compareAndSet(None, Some(why))
    sys.exit(code)
