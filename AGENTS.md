# AGENTS.md: sbta

`sbta` is an sbt launcher (`./sbta <args>` is sbt) whose `--mcp` serves MCP over stdio
as a client of the build's sbt server. See `README.md` for use and `DEV.md` for how it
works.

## Skills

- Follow the `zen-of-projects` Skill; this file records only project-specific facts and exceptions.
- For Scala 3 and ZIO code (writing or reviewing it), follow `zen-of-scala`.
- For design, domain modeling, effects and testing strategy, follow `zen-of-james`.
- Get them with `./sbta extractSkillsJars` (into the gitignored `.kiro/skills/`), and read the relevant ones before working.

## MCP

- Use the MCP server `sbta` (this build's own `./sbta --mcp`; Kiro: `.kiro/settings/mcp.json`, Claude Code: `.mcp.json`, approved in `.claude/settings.json`) for sbt work: `compile`, `test`, `check`, the symbol tools and `sbt-task`. Its tools may be deferred: load them with ToolSearch (search for `sbta`). If it's unavailable (not installed yet), say so and use `./sbta` directly.

## Build, Test & Dev Workflow

- `./sbta testFull`: the full validation, everything CI runs on Linux. The launcher specs (`LauncherSpec`)
  start sbt 1.12.15 and 2.0.12 builds and take a few minutes; `./sbta "testOnly *SbtaSpec"`
  is the fast unit suite. No tests are gated, paid or need Docker.
- The shrunk jar (`sbtaShrink`) is what's tested and published. A new reflective call
  that ProGuard can't follow fails `ReflectionSpec`: keep its targets in `proguard.pro`
  (or confirm they're outside the jar) and record why in `reflection.txt`.
- Upgrading sbt: `src/scripts/update.sh <version>`, set `SbtLaunchVersion` in
  `build.sbt`, fix any script edit `sbtaScripts` reports as no longer applying, and run
  `./sbta sbtaInstall` (a spec checks the committed `./sbta` and `sbta.bat` match it).
- `sbta.bat` is only tested on Windows CI (`test-windows`, required for a release).
- Releasing: push a `v<version>` tag; `.github/workflows/release.yml` runs the tests on
  Linux and Windows, then publishes the GitHub release (`sbtaDist`'s files).

## Maintenance routine

- `.factory/MAINTENANCE.md`, weekly, following the `zen-of-projects` Skill.

## Exceptions to the Skills

- The build's launcher is its own `./sbta` / `sbta.bat` (no `./sbt`), written by
  `./sbta sbtaInstall`, not the latest release: it runs the jar this build made
  (`target/sbta-local/sbta.jar`), and until that exists (a fresh clone, or after removing
  `target/`) it is plain sbt, running sbt's launcher, and its `--mcp` runs sbta from this
  build's classes instead (`sbtaDevClasspath` compiles them and writes
  `target/sbta-dev/classpath`; about a minute in a fresh clone), so the MCP server works
  without `./sbta sbtaInstall`. Restart the MCP client to pick up sbta changes.
- The sbta jar targets Java 17 bytecode (`-release 17`): it runs in the JVM of the
  build it launches, and sbt builds run on JDK 17 and newer. Development and CI use Java 25.
- No `-language:strictEquality` yet: it reports 16 comparisons in the main sources
  (enums without `CanEqual`, `java.nio.file.Path`, `Json`) that need migrating first.
