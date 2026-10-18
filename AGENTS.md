# AGENTS.md: sbta

`sbta` is an sbt launcher (`./sbta <args>` is sbt) whose `--mcp` serves MCP over stdio
as a client of the build's sbt server. See `README.md` for use and `DEV.md` for how it
works.

## Agent tooling

- Follow the `zen-of-projects` Skill (extract with `./sbta extractSkillsJars` into the gitignored `.kiro/skills/`); this file records only project-specific facts and exceptions.
- Maintenance routine: `.factory/MAINTENANCE.md` (weekly), following the `zen-of-projects` Skill.

## Exceptions to zen-of-projects

- The build's launcher is its own `./sbta` / `sbta.bat` (no `./sbt`), written by
  `./sbta sbtaInstall`: it runs the jar this build made (`target/sbta-local/sbta.jar`),
  and until that exists (a fresh clone, or after removing `target/`) it is plain sbt,
  running sbt's launcher, and its `--mcp` runs sbta from this build's classes instead
  (`sbtaDevClasspath` compiles them and writes `target/sbta-dev/classpath`; about a minute
  in a fresh clone), so the MCP server works without `./sbta sbtaInstall`. Restart the MCP
  client to pick up sbta changes.

## Build, Test & Dev Workflow

- Use the MCP server `sbta` (this build's own `./sbta --mcp`; Kiro: `.kiro/settings/mcp.json`, Claude Code: `.mcp.json`, approved in `.claude/settings.json`) for sbt work: `compile`, `test`, `check`, the symbol tools and `sbt-task`. If it's unavailable (not installed yet), say so and use `./sbta` directly.

- `./sbta testFull`: everything CI runs on Linux. The launcher specs (`LauncherSpec`)
  start sbt 1.12.11 and 2.0.10 builds and take a few minutes; `./sbta "testOnly *SbtaSpec"`
  is the fast unit suite.
- The shrunk jar (`sbtaShrink`) is what's tested and published. A new reflective call
  that ProGuard can't follow fails `ReflectionSpec`: keep its targets in `proguard.pro`
  (or confirm they're outside the jar) and record why in `reflection.txt`.
- Upgrading sbt: `src/scripts/update.sh <version>`, set `SbtLaunchVersion` in
  `build.sbt`, fix any script edit `sbtaScripts` reports as no longer applying, and run
  `./sbta sbtaInstall` (a spec checks the committed `./sbta` and `sbta.bat` match it).
- `sbta.bat` is only tested on Windows CI (`test-windows`, required for a release).
