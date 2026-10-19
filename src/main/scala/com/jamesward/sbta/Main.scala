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
 *   sbta --update [<version>] replace the build's sbta and sbta.bat with the newest
 *                             release's (or <version>'s); see [[Updates]]
 *
 * MCP options:
 *   --project-dir=<dir>       the build's root directory (default: the current directory)
 *   --start=eager|lazy|never  start sbt right away (default), on the first call that
 *                             needs it, or never (only use an sbt started some other way)
 *   --docs-url=<url>|none     the MCP server whose documentation tools are proxied
 *                             (default https://www.javadocs.dev/mcp)
 *   --no-update-notice        don't add the "newer sbta release" note to tool results
 */
object Main:

  def main(args: Array[String]): Unit =
    if args.contains("--mcp") then McpMain.run(args.filterNot(_ == "--mcp").toList)
    else if args.toList == List("--sbta-version") then println(s"sbta ${SbtaBuildInfo.version} (sbt launcher ${Launcher.version})")
    else if args.exists(a => a == "--update" || a.startsWith("--update=")) then Updates.cli(args.toList)
    else
      Updates.beforeLaunch(args.toSeq)
      Launcher.run(args)
