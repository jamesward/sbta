import sbt.*

/**
 * The `sbta` and `sbta.bat` launcher scripts: sbt's own runner scripts
 * (`src/scripts/sbt` and `sbt.bat`, verbatim from the sbt distribution of the release
 * the embedded launcher comes from; `src/scripts/update.sh <version>` replaces them),
 * with only what makes them run sbta instead of sbt's launcher. Two modes:
 *
 *  - [[SbtaScripts.Release]]: what users commit to their builds. The jar is
 *    `com.jamesward:sbta:<version>` from Maven Central (or `$SBTA_LAUNCH_REPO`, else
 *    `$SBT_LAUNCH_REPO`), cached in `~/.cache/sbt/boot/sbta/<version>/`; the Windows
 *    script downloads it too (sbt.bat expects an installed sbt, with its launcher).
 *  - [[SbtaScripts.Local]]: this build's own `./sbta` and `./sbta.bat` (`sbtaInstall`).
 *    The jar is the one this build made, at a path relative to the script. Until it
 *    exists (a fresh clone, or after `clean`), the script is plain sbt: it downloads
 *    and runs sbt's launcher, which is how sbta is built in the first place.
 *
 * In both, `--mcp` runs in the JVM (never through sbtn, sbt 2's default client), and
 * sbt.bat uses sbt's JVM client when no sbtn is installed next to it. Each edit must
 * apply exactly once, so a changed upstream script fails the build instead of producing
 * a half-patched launcher.
 */
object SbtaScripts {

  val sbtaScripts = taskKey[Seq[File]]("Generate the release sbta and sbta.bat launcher scripts.")

  sealed trait Mode
  final case class Release(version: String) extends Mode
  /** `jar`: the sbta jar, relative to the scripts' directory, with `/` separators. */
  final case class Local(jar: String) extends Mode

  def generate(sources: File, out: File, mode: Mode, sbtVersion: String): Seq[File] = {
    // Line endings as on Unix, however the checkout wrote them (git on Windows may use CRLF).
    val unixSource    = IO.read(sources / "sbt").replace("\r\n", "\n")
    val windowsSource = IO.read(sources / "sbt.bat").replace("\r\n", "\n")
    // The scripts and the embedded sbt launcher come from the same sbt release.
    Seq("sbt" -> s"declare init_sbt_version=$sbtVersion\n", "sbt.bat" -> s"set init_sbt_version=$sbtVersion\n").foreach {
      case (name, expected) =>
        val text = if (name == "sbt") unixSource else windowsSource
        if (!text.contains(expected))
          sys.error(s"sbta scripts: $name is not sbt $sbtVersion's (run src/scripts/update.sh $sbtVersion)")
    }
    val unix    = patch("sbt", unixSource, unixEdits(mode))
    val windows = patch("sbt.bat", windowsSource, windowsEdits(mode))
    val sbta    = out / "sbta"
    val bat     = out / "sbta.bat"
    IO.write(sbta, unix)
    sbta.setExecutable(true, false)
    // cmd.exe mis-parses labels and `goto` in LF-only batch files.
    IO.write(bat, windows.replace("\n", "\r\n"))
    Seq(sbta, bat)
  }

  private def patch(name: String, text: String, edits: Seq[(String, String)]): String =
    edits.foldLeft(text) { case (current, (from, to)) =>
      val count = current.sliding(from.length).count(_ == from)
      if (count != 1) sys.error(s"sbta scripts: expected exactly one occurrence in $name of:\n$from\n(found $count)")
      current.replace(from, to)
    }

  // ---- sbt (bash) ----

  private def unixEdits(mode: Mode): Seq[(String, String)] = {
    val common = Seq(
      "jps -v | grep sbt-launch | cut -f1 -d ' '" -> "jps -v | grep -E 'sbt-launch|sbta' | cut -f1 -d ' '",
      // Without --fail, curl saves a 404 page as the jar (and with no shasum to catch it, keeps it).
      "      curl --silent -L \"$url\" --output \"$jar\"" -> "      curl --fail --silent -L \"$url\" --output \"$jar\"",
      "  --client            run sbtn (native client), and start sbt server in the background\n" ->
        "  --client            run sbtn (native client), and start sbt server in the background\n  --mcp               serve MCP over stdio for this build (sbta), starting sbt when needed\n",
    )
    val specific = mode match {
      case Release(version) =>
        Seq(
          "declare -a residual_args\n" ->
            s"# sbta: the launcher jar this script runs (instead of sbt-launch)\ndeclare sbta_version=\"$version\"\ndeclare -a residual_args\n",
          "echo \"$(cygwinpath \"${sbt_home}/bin/sbt-launch.jar\")\"" -> "echo \"$(cygwinpath \"${sbt_home}/bin/sbta.jar\")\"",
          "  local repo_base=\"$SBT_LAUNCH_REPO\"" -> "  local repo_base=\"${SBTA_LAUNCH_REPO:-$SBT_LAUNCH_REPO}\"",
          "echo \"$repo_base/org/scala-sbt/sbt-launch/$1/sbt-launch-$1.jar\"" -> "echo \"$repo_base/com/jamesward/sbta/$1/sbta-$1.jar\"",
          """  if [[ "$launcher_sv" == "" ]]; then
    if [[ "$init_sbt_version" != "_to_be_replaced" ]]; then
      launcher_sv="$init_sbt_version"
    else
      launcher_sv="$builtin_sbt_version"
    fi
  fi""" -> """  [[ "$launcher_sv" == "" ]] && launcher_sv="$sbta_version"""",
          "download_jar=\"${user_home:-$HOME}/.cache/sbt/boot/sbt-launch/$launcher_sv/sbt-launch-$launcher_sv.jar\"" ->
            "download_jar=\"${user_home:-$HOME}/.cache/sbt/boot/sbta/$launcher_sv/sbta-$launcher_sv.jar\"",
          "    dlog \"downloading sbt launcher $launcher_sv\"" -> "    dlog \"downloading sbta $launcher_sv\"",
          "                        bsp) use_sbtn=0 && addResidual \"-bsp\" && shift ;;" ->
            "                        bsp) use_sbtn=0 && addResidual \"-bsp\" && shift ;;\n                      --mcp) use_sbtn=0 && addResidual \"--mcp\" && shift ;;",
        )
      case Local(jar) =>
        Seq(
          "declare -a residual_args\n" ->
            s"""# sbta: this build's own sbta jar, relative to this script (written by `sbtaInstall`).
               |# Until it exists, this script is plain sbt and runs sbt's launcher.
               |declare sbta_local_jar="$jar"
               |declare -a residual_args
               |""".stripMargin,
          "  echo \"$(cygwinpath \"${sbt_home}/bin/sbt-launch.jar\")\"" ->
            """  if [[ -f "${sbt_bin_dir}/$sbta_local_jar" ]]; then
    echo "$(cygwinpath "${sbt_bin_dir}/$sbta_local_jar")"
  else
    echo "$(cygwinpath "${sbt_home}/bin/sbt-launch.jar")"
  fi""",
          "                        bsp) use_sbtn=0 && addResidual \"-bsp\" && shift ;;" ->
            """                        bsp) use_sbtn=0 && addResidual "-bsp" && shift ;;
                      --mcp) [[ -f "${sbt_bin_dir}/$sbta_local_jar" ]] || {
                               echoerr "sbta: $sbta_local_jar isn't built yet: run ./sbta sbtaInstall"
                               exit 1
                             }
                             use_sbtn=0 && addResidual "--mcp" && shift ;;""",
        )
    }
    specific ++ common
  }

  // ---- sbt.bat ----

  private def windowsEdits(mode: Mode): Seq[(String, String)] = {
    val header = mode match {
      case Release(version) =>
        s"rem sbta: the launcher jar this script runs (instead of sbt-launch)\nset sbta_version=$version\nset sbt_default_mem="
      case Local(jar) =>
        s"rem sbta: this build's own sbta jar, relative to this script (written by `sbtaInstall`).\n" +
          s"rem Until it exists, this script is plain sbt and runs sbt's launcher.\nset sbta_local_jar=${jar.replace('/', '\\')}\nset sbt_default_mem="
    }
    val mcpCheck = mode match {
      case Release(_) => ""
      case Local(_) =>
        """  if not exist "!SBT_BIN_DIR!!sbta_local_jar!" (
    >&2 echo sbta: !sbta_local_jar! isn't built yet: run sbta.bat sbtaInstall
    goto error
  )
"""
    }
    val jarStep = mode match {
      case Release(_) =>
        """:sbta_jar
rem sbta: next to this script's sbt home, or downloaded to the user's sbt boot cache
set "sbt_main=com.jamesward.sbta.Main"
if exist "!SBT_HOME!\bin\sbta.jar" (
  set "sbt_jar=!SBT_HOME!\bin\sbta.jar"
  exit /B 0
)
set "sbt_jar=%UserProfile%\.cache\sbt\boot\sbta\!sbta_version!\sbta-!sbta_version!.jar"
set "sbta_repo=https://repo1.maven.org/maven2"
if defined SBT_LAUNCH_REPO set "sbta_repo=%SBT_LAUNCH_REPO%"
if defined SBTA_LAUNCH_REPO set "sbta_repo=%SBTA_LAUNCH_REPO%"
set "sbta_url=!sbta_repo!/com/jamesward/sbta/!sbta_version!/sbta-!sbta_version!.jar"
goto sbta_download
"""
      case Local(_) =>
        """:sbta_jar
rem sbta: this build's own jar, or (until it's built) sbt's launcher, downloaded
if exist "!SBT_BIN_DIR!!sbta_local_jar!" (
  set "sbt_jar=!SBT_BIN_DIR!!sbta_local_jar!"
  set "sbt_main=com.jamesward.sbta.Main"
  exit /B 0
)
set "sbt_main=xsbt.boot.Boot"
set "sbt_jar=%UserProfile%\.cache\sbt\boot\sbt-launch\!init_sbt_version!\sbt-launch-!init_sbt_version!.jar"
set "sbta_repo=https://repo1.maven.org/maven2"
if defined SBT_LAUNCH_REPO set "sbta_repo=%SBT_LAUNCH_REPO%"
set "sbta_url=!sbta_repo!/org/scala-sbt/sbt-launch/!init_sbt_version!/sbt-launch-!init_sbt_version!.jar"
goto sbta_download
"""
    }
    Seq(
      "set sbt_default_mem=" -> header,
      "if \"%~0\" == \"--client\" set _client_arg=true\n" ->
        s"""rem sbta: --mcp runs in the JVM (like --server), never through sbtn
if "%~0" == "--mcp" (
$mcpCheck  set sbt_args_client=0
  set "SBT_ARGS=!SBT_ARGS! --mcp"
  goto args_loop
)

if "%~0" == "--client" set _client_arg=true
""",
      "if !run_native_client! equ 1 if not defined sbt_args_print_version (" ->
        """rem sbta: sbt.bat runs the sbtn installed next to it; a script committed to a build
rem has none, so use sbt's JVM client (`--client` to the launcher), like --jvm-client.
if !run_native_client! equ 1 if not exist "!SBT_BIN_DIR!sbtn-x86_64-pc-win32.exe" (
  set run_native_client=
  set SBT_ARGS=--client !SBT_ARGS!
)

if !run_native_client! equ 1 if not defined sbt_args_print_version (""",
      """  set "sbt_jar=!SBT_HOME!\bin\sbt-launch.jar"""" -> "  call :sbta_jar\n  if ERRORLEVEL 1 goto error",
      "-cp \"!sbt_jar!\" xsbt.boot.Boot " -> "-cp \"!sbt_jar!\" !sbt_main! ",
      "  echo xsbt.boot.Boot\n" -> "  echo !sbt_main!\n",
      "findstr \"xsbt.boot.Boot\"" -> "findstr \"xsbt.boot.Boot com.jamesward.sbta.Main\"",
      "\n:copyrt\n" ->
        ("\n" + jarStep + """
:sbta_download
if exist "!sbt_jar!" exit /B 0
for %%D in ("!sbt_jar!") do if not exist "%%~dpD" mkdir "%%~dpD"
>&2 echo [info] downloading !sbta_url!
curl.exe -fsSL -o "!sbt_jar!.temp" "!sbta_url!" 2>nul || powershell -NoProfile -Command "Invoke-WebRequest -UseBasicParsing -Uri '!sbta_url!' -OutFile '!sbt_jar!.temp'"
if not exist "!sbt_jar!.temp" (
  >&2 echo [error] failed to download !sbta_url!
  exit /B 1
)
move /Y "!sbt_jar!.temp" "!sbt_jar!" >nul
exit /B 0

:copyrt
"""),
    )
  }
}
