package com.jamesward.sbta

import java.nio.file.{ Files, Path }
import scala.util.control.NonFatal

/**
 * `sbta`: an sbt launcher.
 *
 *   sbta [sbt arguments]      sbt, exactly: its own launcher runs in this JVM with the
 *                             arguments (the shell with none, batch commands, …)
 *   sbta --mcp [options]      serve MCP over stdio for the build in the current
 *                             directory, as a client of the build's sbt server
 *
 * MCP options:
 *   --project-dir=<dir>       the build's root directory (default: the current directory)
 *   --start=eager|lazy|never  start sbt right away (default), on the first call that
 *                             needs it, or never (only use an sbt started some other way)
 *   --docs-url=<url>|none     the MCP server whose documentation tools are proxied
 *                             (default https://www.javadocs.dev/mcp)
 */
object Main:

  def main(args: Array[String]): Unit =
    if args.contains("--mcp") then McpMain.run(args.filterNot(_ == "--mcp").toList)
    else if args.toList == List("--sbta-version") then println(s"sbta ${SbtaBuildInfo.version} (sbt launcher ${Launcher.version})")
    else Launcher.run(args)
