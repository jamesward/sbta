package com.jamesward.sbta

import com.jamesward.ziohttp.mcp.McpToolContext
import zio.*

/**
 * What one tool call's sbt work is doing: the command line sbt runs and the last
 * line of its output. The [[Session]]s the call opens update it, and
 * [[Activity.reporting]] reports it as the call's progress, which becomes the
 * task's `statusMessage` when the call runs as an MCP task.
 *
 * A call's sessions are opened on the thread that does the call's work, so they
 * find its activity on that thread ([[Activity.current]]) without passing it through
 * every tool's code.
 */
final class Activity:
  private val started = java.lang.System.nanoTime()
  @volatile private var command  = Option.empty[String]
  @volatile private var lastLine = Option.empty[String]

  def running(commandLine: String): Unit = command = Some(commandLine)

  /** Note the last non-blank line of `output`, if it has one. */
  def output(text: String): Unit =
    Text.stripAnsi(text).split("[\r\n]").iterator.map(_.trim).filter(_.nonEmpty).toSeq.lastOption
      .foreach(line => lastLine = Some(line.take(Activity.MaxLine)))

  def elapsed: Duration = Duration.fromNanos(java.lang.System.nanoTime() - started)

  /** E.g. "`Test / testFull` (2m 10s): [info] Compiling 3 Scala sources…". */
  def status: String =
    val head = command.fold("sbt")(c => s"`$c`")
    s"$head (${Activity.format(elapsed)})" + lastLine.fold("")(l => s": $l")

object Activity:
  private val MaxLine = 200

  private val threadActivity = ThreadLocal.withInitial[Option[Activity]](() => None)

  /** The activity of the call whose work runs on this thread, if any. */
  def current: Option[Activity] = threadActivity.get

  /** Run `body` with `activity` as this thread's. */
  def during[A](activity: Activity)(body: => A): A =
    val previous = threadActivity.get
    threadActivity.set(Some(activity))
    try body
    finally threadActivity.set(previous)

  /**
   * Run the blocking `body` (interruptibly: a cancelled call or task interrupts it)
   * with a fresh activity, reported through `ctx` every `every`: as `notifications/progress`
   * for a request that asked for progress, and as the task's `statusMessage` when the
   * call runs as a task.
   */
  def reporting[A](ctx: McpToolContext, every: Duration)(body: => A): Task[A] =
    ZIO.succeed(Activity()).flatMap { activity =>
      val report = ZIO.suspendSucceed(ctx.progress(activity.elapsed.toMillis / 1000.0, None, Some(activity.status)))
      ZIO.scoped(
        (ZIO.sleep(every) *> report).forever.forkScoped *>
          ZIO.attemptBlockingInterrupt(during(activity)(body))
      )
    }

  private[sbta] def format(d: Duration): String =
    val seconds = d.toSeconds
    if seconds < 60 then s"${seconds}s" else s"${seconds / 60}m ${seconds % 60}s"
