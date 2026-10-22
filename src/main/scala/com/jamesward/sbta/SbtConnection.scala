package com.jamesward.sbta

import zio.json.*
import zio.json.ast.Json

import java.io.{ BufferedInputStream, EOFException, IOException, InputStream, OutputStream }
import java.net.{ StandardProtocolFamily, UnixDomainSocketAddress }
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.concurrent.{ CompletableFuture, ConcurrentHashMap }
import java.util.concurrent.atomic.{ AtomicBoolean, AtomicLong }
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/** A JSON-RPC error response from sbt. */
final case class RpcError(code: Long, message: String) extends Exception(s"$message ($code)")

/**
 * One connection to an sbt server (sbt 1.4+ and 2.x): JSON-RPC with LSP framing
 * (`Content-Length` headers) over sbt's user-only Unix domain socket. sbt sends
 * notifications (logs, BSP diagnostics, console bytes), answers our requests by id,
 * and asks a few questions of its own (the attached terminal's size and
 * capabilities), which are answered here as a plain, non-ANSI terminal.
 */
final class SbtConnection private (
    channel: SocketChannel,
    onNotification: (String, Json) => Unit,
) extends AutoCloseable:

  private val input   = BufferedInputStream(SbtConnection.ChannelInput(channel), 1 << 16)
  private val output  = SbtConnection.ChannelOutput(channel)
  private val pending = ConcurrentHashMap[String, CompletableFuture[Json]]()
  private val ids     = AtomicLong(1)
  private val closed  = AtomicBoolean(false)

  /** The one thread that writes to the channel (see [[write]]); it keeps messages whole and in order. */
  private val writer = java.util.concurrent.Executors.newSingleThreadExecutor { r =>
    val t = Thread(r, "sbta-sbt-writer")
    t.setDaemon(true)
    t
  }
  private val reader = Thread(() => readLoop(), "sbta-sbt-reader")
  reader.setDaemon(true)
  reader.start()

  /** Send a request; the future completes with its `result`, or fails with an [[RpcError]] or IOException. */
  def request(method: String, params: Json): (String, CompletableFuture[Json]) =
    val id     = s"sbta-${ids.getAndIncrement()}"
    val future = CompletableFuture[Json]()
    pending.put(id, future)
    try write(Json.Obj("jsonrpc" -> Json.Str("2.0"), "id" -> Json.Str(id), "method" -> Json.Str(method), "params" -> params))
    catch
      case error: IOException =>
        pending.remove(id)
        future.completeExceptionally(error)
    (id, future)

  def notify(method: String, params: Json): Unit =
    write(Json.Obj("jsonrpc" -> Json.Str("2.0"), "method" -> Json.Str(method), "params" -> params))

  def isOpen: Boolean = !closed.get

  def close(): Unit =
    if closed.compareAndSet(false, true) then
      try channel.close() catch case NonFatal(_) => ()
      writer.shutdown()
      failPending(IOException("the connection to sbt was closed"))

  private def write(message: Json): Unit =
    val body = message.toJson.getBytes(StandardCharsets.UTF_8)
    // On the writer thread, never the caller's: a SocketChannel is an InterruptibleChannel,
    // so a write from a thread being interrupted (a cancelled call: ZIO's
    // attemptBlockingInterrupt interrupts until the call returns) would close the
    // connection, and with it the `sbt/cancelRequest` meant to cancel the command.
    val done =
      try
        CompletableFuture.runAsync(
          () =>
            output.write(s"Content-Length: ${body.length}\r\n\r\n".getBytes(StandardCharsets.US_ASCII))
            output.write(body),
          writer,
        )
      catch case _: java.util.concurrent.RejectedExecutionException => throw IOException("the connection to sbt was closed")
    // Wait for the write even if interrupted (it's quick, and the caller needs to know it was
    // sent), then keep the interrupt for the caller.
    var interrupted = false
    var result      = Option.empty[Throwable]
    var waiting     = true
    while waiting do
      try { done.get(); waiting = false }
      catch
        case _: InterruptedException                       => interrupted = true
        case e: java.util.concurrent.ExecutionException    => result = Some(e.getCause); waiting = false
        case e: java.util.concurrent.CancellationException => result = Some(IOException("the connection to sbt was closed")); waiting = false
    if interrupted then Thread.currentThread.interrupt()
    result.foreach {
      case e: IOException => throw e
      case e              => throw IOException(s"writing to sbt failed: $e", e)
    }

  private def readLoop(): Unit =
    val reason =
      try
        while true do dispatch(readMessage())
        IOException("unreachable")
      catch
        case _: EOFException  => IOException("sbt closed the connection (the server stopped, or was restarted)")
        case error: Throwable => IOException(s"lost the connection to sbt: $error")
    closed.set(true)
    try channel.close() catch case NonFatal(_) => ()
    writer.shutdown()
    failPending(reason)

  private def failPending(error: Throwable): Unit =
    pending.asScala.keys.toList.foreach(id => Option(pending.remove(id)).foreach(_.completeExceptionally(error)))

  private def readMessage(): Json =
    var length = -1
    var line   = readLine()
    while line.nonEmpty do
      if line.toLowerCase.startsWith("content-length:") then length = line.drop("content-length:".length).trim.toInt
      line = readLine()
    if length < 0 then readMessage() // a stray blank line
    else
      val text = String(input.readNBytes(length), StandardCharsets.UTF_8)
      text.fromJson[Json].getOrElse(Json.Obj())

  private def readLine(): String =
    val b = StringBuilder()
    var c = input.read()
    while c != -1 && c != '\n' do
      if c != '\r' then b.append(c.toChar)
      c = input.read()
    if c == -1 then throw EOFException()
    b.toString

  private def dispatch(message: Json): Unit =
    val fields = message.asObject.map(_.fields.toMap).getOrElse(Map.empty)
    (fields.get("method"), fields.get("id")) match
      case (Some(Json.Str(method)), Some(id)) => answer(id, method)
      case (Some(Json.Str(method)), None)     =>
        try onNotification(method, fields.getOrElse("params", Json.Null))
        catch case NonFatal(_) => ()
      case (None, Some(id)) =>
        Option(pending.remove(SbtConnection.idKey(id))).foreach { future =>
          fields.get("error").flatMap(_.asObject) match
            case Some(error) =>
              val map  = error.fields.toMap
              val code = map.get("code").collect { case Json.Num(n) => n.longValue }.getOrElse(0L)
              val msg  = map.get("message").collect { case Json.Str(s) => s }.getOrElse("")
              future.completeExceptionally(RpcError(code, msg))
            case None => future.complete(fields.getOrElse("result", Json.Null))
        }
      case _ => ()

  /** sbt asks an attached client about its terminal: answer as a dumb terminal, so output has no ANSI codes. */
  private def answer(id: Json, method: String): Unit =
    val result = method match
      case "sbt/terminalPropertiesQuery" =>
        Json.Obj(
          "width"               -> Json.Num(200),
          "height"              -> Json.Num(50),
          "isAnsiSupported"     -> Json.Bool(false),
          "isColorEnabled"      -> Json.Bool(false),
          "isSupershellEnabled" -> Json.Bool(false),
          "isEchoEnabled"       -> Json.Bool(false),
        )
      case "sbt/terminalGetSize"      => Json.Obj("width" -> Json.Num(200), "height" -> Json.Num(50))
      case "sbt/getTerminalAttributes" =>
        Json.Obj("iflag" -> Json.Str(""), "oflag" -> Json.Str(""), "cflag" -> Json.Str(""), "lflag" -> Json.Str(""), "cchars" -> Json.Str(""))
      case _ => Json.Obj()
    try write(Json.Obj("jsonrpc" -> Json.Str("2.0"), "id" -> id, "result" -> result))
    catch case NonFatal(_) => ()

object SbtConnection:

  /** Connect to the sbt server listening on `socket`. */
  def open(socket: Path, onNotification: (String, Json) => Unit): SbtConnection =
    val channel = SocketChannel.open(StandardProtocolFamily.UNIX)
    try
      channel.connect(UnixDomainSocketAddress.of(socket))
      SbtConnection(channel, onNotification)
    catch
      case error: Throwable =>
        channel.close()
        throw error

  private def idKey(id: Json): String = id match
    case Json.Str(s) => s
    case other       => other.toJson

  /**
   * Streams over the channel's own `read`/`write`, so a read blocked waiting for sbt
   * never stalls a write (`Channels.newInputStream`/`newOutputStream` share a lock on
   * some JDKs).
   */
  private[sbta] final class ChannelInput(channel: SocketChannel) extends InputStream:
    override def read(): Int =
      val one = new Array[Byte](1)
      if read(one, 0, 1) < 0 then -1 else one(0) & 0xff
    override def read(b: Array[Byte], off: Int, len: Int): Int =
      if len == 0 then 0 else channel.read(ByteBuffer.wrap(b, off, len))

  private[sbta] final class ChannelOutput(channel: SocketChannel) extends OutputStream:
    override def write(b: Int): Unit = write(Array(b.toByte), 0, 1)
    override def write(b: Array[Byte], off: Int, len: Int): Unit =
      val buffer = ByteBuffer.wrap(b, off, len)
      while buffer.hasRemaining do channel.write(buffer)
