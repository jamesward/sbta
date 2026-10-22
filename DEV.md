# sbta: development

## Build

```
./sbta testFull      # all tests (unit, the reflection check, the launcher specs on sbt 1 and 2 builds)
./sbta assembly      # target/out/jvm/u/sbta/sbta-assembly.jar
./sbta sbtaShrink    # target/out/jvm/u/sbta/proguard/sbta.jar: the published jar
./sbta sbtaScripts   # target/out/jvm/u/sbta/sbta-scripts/{sbta,sbta.bat}: the release scripts
./sbta sbtaDist      # target/out/jvm/u/sbta/sbta-dist: the GitHub release files
./sbta sbtaInstall   # this build's own ./sbta and ./sbta.bat, running the jar built here
```

This build runs on its own launcher, in local mode: `./sbta` and `sbta.bat` run
`target/sbta-local/sbta.jar` (copied there by `sbtaInstall`, as a new file, so a
running sbta keeps its jar), and until it exists they are plain sbt, downloading sbt's
launcher. That's how a fresh clone (and CI, on Linux and Windows) builds. The release
scripts download the jar of their version from its GitHub release instead; both come from `SbtaScripts`
(`SbtaScripts.Release` and `SbtaScripts.Local`).

```
```

Projects: `sbta` (the root: launcher and MCP server, Scala 3.10, JDK 17 bytecode) and
the isolated runtimes it embeds as jars and loads in their own classloaders:
`check-runtime` (the `check` compiler bridge, compiled against Scala 3.3.1), and
`symbol-runtime` (the TASTy symbol index on TASTy Query 1.8, Scala 3.8) with the
TASTy Query 1.9 and 1.10 readers (`reader-3.9`, `reader-3.10`). TASTy Query follows
the TASTy minor version and each release is compiled with that Scala, so a build's
symbols are read by the reader for its Scala version, with the build's own Scala
library. sbta was split out of [sbt-mcp](https://github.com/jamesward/sbt-mcp), where
these engines started.

## How it works


- **Launcher.** Without `--mcp`, `Launcher` runs sbt's launcher (`xsbt.boot.Boot`,
  sbt-launch `SbtLaunchVersion`, embedded as a nested jar because it bundles Scala
  2.12) in sbta's JVM, from sbt's boot cache path
  (`~/.cache/sbt/boot/sbt-launch/<v>/`), with `java.class.path` pointed at it (sbt
  looks for its launcher there). So `java -jar sbta.jar <args>` is
  `java -jar sbt-launch.jar <args>`.
- **Scripts.** `src/scripts/sbt` and `sbt.bat` are the runner scripts of the
  sbt release the embedded launcher comes from (`SbtLaunchVersion`, now 2.0.11),
  verbatim from its distribution zip. `project/SbtaScripts.scala` (`sbtaScripts`)
  checks they're that release, then applies the few edits (jar URL and cache path,
  sbta's main class in the .bat, `--mcp` without sbtn, the JVM client when no sbtn is
  installed next to `sbta.bat`, a download step in the .bat), and fails if an edit no
  longer applies exactly once. Upgrading sbt: `src/scripts/update.sh <version>`,
  set `SbtLaunchVersion`, fix any edit that fails. In this build's own (local-mode)
  script, `--mcp` without a built jar runs `sbtaDevClasspath` (compile, then write the
  runtime classpath with real paths to `target/sbta-dev/classpath`) through the sbt client,
  with its output on stderr, then execs `java -cp <that> com.jamesward.sbta.Main --mcp`: a
  fresh clone (a cloud routine, say) gets the MCP server in about a minute instead of
  `sbtaInstall`'s four. `sbt runMain` won't do: its banner and `[info] running` lines go to
  stdout, which MCP's stdio transport needs for JSON only; `bgRunMain` has no stdin.
- **Releasing.** Push a `vX.Y.Z` tag. The version comes from it (sbt-dynver). There
  is no Maven repository: `release.yml` runs the tests (`test.yml`, Linux and Windows),
  then attaches `sbtaDist`'s files to the GitHub release: `sbta-<version>.jar` (the
  shrunk jar), its `.sha1` (which the scripts check, as sbt's do for sbt-launch),
  `sbta`, `sbta.bat` and `SHA256SUMS`. The release scripts download the jar from
  `https://github.com/jamesward/sbta/releases/download/v<version>/`
  (`$SBTA_RELEASES_URL` replaces that). 0.0.1 is also on Maven Central, from before this.
- **CI.** `test.yml` (pushes, PRs, and called by `release.yml`) runs `testFull` on
  Linux, and `LauncherSpec` on Windows with the build's own `sbta.bat` (required: no
  release without it).
- **Jar size: 33 MB.** The assembly (51 MB, after excluding zio-http's
  `netty-pkitesting` and Bouncy Castle: test-certificate generation, ~10 MB; sbta
  never serves TLS) is shrunk by ProGuard (`sbtaShrink`, `project/SbtaShrink.scala`,
  rules in `proguard.pro`): unused classes removed, nothing renamed or optimized.
  That jar is the one tested, published and released. Two rules keep what's reached
  only by name: all of Netty (its internals use computed-name reflection and JNI;
  ~1.5 MB) and every Scala 3 lazy val's field (`x$lzy1`), which the class's static
  initializer looks up with `findVarHandle` even when the lazy val is unused (without
  that rule, zio-json fails to initialize).
- **No unseen reflection** (`ReflectionSpec`). It finds every reflective call in the
  shrunk jar's bytecode (`Class.forName`, `getDeclared*`, `MethodHandles.Lookup.find*`,
  field updaters, `ServiceLoader`, `Proxy`…). With constant names, the target is
  checked: still in the shrunk jar if it was in the assembly. A call with a computed
  name must be listed, with its reason, in `reflection.txt` (today: Netty, kept
  whole, and sbta's own lookups into its embedded jars). `META-INF/services` providers
  are checked too. Then `LauncherSpec` calls every MCP tool through the shrunk jar on
  sbt 1 and 2 builds, with a local upstream MCP server for the docs proxy's HTTP client.
  The remaining risk is reflection driven by data (e.g. a class name read from a
  file), which bytecode can't show and only the end-to-end tests exercise.
- **Tests.** `SbtaSpec` uses a fake sbt server. `LauncherSpec` runs the assembled jar
  and generated script against sbt 1.12.11 and 2.0.11 builds (commands and exit codes,
  the shell, the script's download, `--mcp` with aggregation): a few minutes.
  Processes started from an sbt-forked JVM inherit `SBT_TERMINAL_PROPS`, which makes a
  child sbt ignore stdin; the spec and `SbtServer` remove it.
- **Connecting.** `project/target/active.json` → `local://` Unix socket; LSP framing
  (`SbtConnection`). Each tool call opens its own connection (`Session`):
  `initialize` with `skipAnalysis`, then `sbt/attach {interactive:false}`, answering
  sbt's terminal queries as a non-ANSI terminal. sbt sends a command's console
  output (`sbt/systemOut` bytes, including forked tests and programs) to the channel
  that sent it, so a call's output is its own.
- **Commands** go through `sbt/exec` (queued on sbt's command loop, `Queued`
  notifications while waiting). Response: a result (done), `-32603` (failed, with
  sbt's message), `-33000` with an empty message (couldn't parse) or with the
  command line (cancelled). An interrupted call sends `sbt/cancelRequest {id}`.
- **A build that fails to load** makes sbt ask "(r)etry, (q)uit, (l)ast, or (i)gnore?"
  and read stdin. For a command of sbta's channel sbt reads that channel
  (`sbt/readSystemIn`): sbta answers `i` (keep the build loaded before; `r` would fail
  again, `q` would stop the shared server) and reports the command as failed, with the
  prompt line replaced. That needs the command to be sbta's: sbt 1 reloads changed
  build files before a command (with `ReloadOnSourceChanges`; sbt 2 by default) as a
  command of no channel, asking on the server's own stdin, which no one can answer. So
  on sbt 1, when the build files changed since sbt last loaded them (by sbta's
  fingerprint), a `reload` from sbta's channel goes first, before commands and BSP
  requests. Opening a channel must not set off sbt 1's reload either: sbta connects to
  the socket in `active.json` without probing it first (a probe's disconnect runs a
  command), and sends `initialize` and `sbt/attach` together, as sbt's own client does.
  Not covered: sbt starting (`sbt --client`) on a build that doesn't load.
  Until a load succeeds again (a `reload`, or sbt's own reload, printing "set current
  project to"), every tool result gets a note with the last load error: sbt keeps
  running the build it had, so results don't reflect the build-file changes. sbt 2's
  own reload before a command (`loadp`) fails the command without asking; sbta tells
  it from a load that started ("loading project definition from") and didn't finish.
- **Data.** Diagnostics from BSP `build/publishDiagnostics` (sent for `sbt/exec`
  compiles too; ANSI stripped; republished for unchanged files). Test results from
  the JUnit XML sbt writes by default (`test-reports/TEST-*.xml`, newer than the
  call). Structure from BSP `workspace/buildTargets`, `scalaTestClasses`,
  `scalaMainClasses`; settings from `sbt/setting` (needs a project axis; sbt 2 has no
  JSON writer for `Seq`, so `show` falls back to `show <key>`'s log).
- **run on sbt 2** answers with `sbt/clientJob` (classpath, JVM, options): sbta starts
  that JVM itself, stdin closed, and kills it on cancel. sbt 1 runs it in-process.
- **`dependencies`** runs `<project> / update`, then reads the `UpdateReport` sbt
  caches as JSON (`update/update_cache_<n>/output` under the project's `target`,
  from `sbt/setting`); declared modules from `sbt/setting libraryDependencies`
  (sbt 1) or `show`'s log (sbt 2). The cache file is sbt-internal: keep an eye on it.
- **`check` and the symbol tools** use the engines (`IsolatedCheck`,
  `ModuleCheck`, `IsolatedSymbolIndex`, `SymbolIndexState`, with the embedded runtimes, `codeToolAssets` in `build.sbt`). `CodeTools` feeds
  them from BSP (`buildTargets` data: Scala version and compiler jars;
  `scalacOptions`: classpath, options, class directory; `sources`). That module data
  is cached in `project/target/sbta/modules.json` until a build definition file or
  `active.json` changes, and while sbt is busy (sbt 2 reports the request `Queued`)
  the cached copy is used, so `check` never waits behind a build. "Changed since the
  last compile" is a source newer than the module's newest class file (no Zinc
  stamps). The symbol tools compile the root build's main targets first (BSP
  `compile`, given up after 10 s or when queued) and index their class directories
  plus classpath; the index is keyed by a digest of their TASTy files (sbt 2 rewrites
  unchanged class files) and is primed after a rebuild, since a cold first search can
  exceed the search's 10 s budget.
- **Aggregation** (not in BSP) comes from `show <p> / name`: sbt prints a
  `<q> / name` block for each project `q` that `p` aggregates (transitively); direct
  aggregates are the ones not reached through another. One command per root-build
  project, cached until the build definition changes.
- **Not covered yet:** Windows named pipes (`--mcp`).
- **Updates** (`Updates`). The release scripts pin their version (`sbta_version`). The
  check: GitHub's releases API, newest semver release (no drafts, pre-releases or
  suffixed tags), at most once a day per user (`~/.cache/sbt/boot/sbta/latest.json`,
  `{checkedAt, latest}`; a failed check waits a day too), in a daemon thread whenever
  the jar starts (launcher, sbt server, `--mcp`). Only a release build compares: a
  dynver development version never gets a notice. Notices: the release `sbta` script
  prints it from the cache (`sbtaUpdateNotice`, since sbt 2's sbtn never starts the jar
  for a command) and sets `SBTA_UPDATE_NOTICE_SHOWN` so the jar doesn't repeat it; the
  jar prints it otherwise (`sbta.bat`, `java -jar`). `--mcp` (`McpMain.UpdateNotes`)
  compares the version the build's `./sbta` pins (so it stops once updated) and adds the
  note to the instructions and, as an extra text item (never `structuredContent`), to
  every tool result, proxied ones too. `--update` / `update-sbta` downloads `sbta`,
  `sbta.bat` and `SHA256SUMS` from `$SBTA_RELEASES_URL` or GitHub and moves each verified
  file into place as a new file (a running bash keeps the script it opened). cmd.exe
  reads a batch file while running it, so from `sbta.bat` the new one is written as
  `sbta.bat.new`, which the script moves over itself on its last line. The scripts route
  `--update` to the JVM like `--mcp`, with `SBTA_SCRIPT_DIR` set to their directory.
  Still to do: `sbta.bat` doesn't check the downloaded jar's `.sha1` yet (`sbta` does,
  when `shasum` is installed).

## `check`

- **`check` is a warm, compile-free checker using the project's own compiler.**
  `IsolatedCheckBridge` (`check-runtime/`) is compiled once against the
  oldest supported compiler (Scala 3.3.1, `Provided`) and embedded as a jar. At
  runtime `IsolatedCheck` loads it in a platform-parented `URLClassLoader` together
  with the module's compiler jars (from BSP), so each module is checked by exactly the
  compiler version that compiles it; the project classpath is passed as `-classpath`
  (macros load through the compiler's own macro classloader, as in a batch compile).
  The bridge only touches the long-stable driver surface (`Driver.setup`,
  `Compiler.newRun`, `Run.compileSources`, `StoreReporter`, `SourceFile.virtual`).
  - **Phases:** the full `Compiler` pipeline with `-Ystop-before:genBCode`, so every
    diagnostic a compile can produce short of bytecode emission is reported
    (typer, PostTyper, inlining/macros, RefChecks, init checks, pattern matching,
    unused checks, erasure clashes, `@tailrec`). Output goes to a private scratch
    directory; options that write elsewhere or rewrite sources (`-d`, `-rewrite`,
    `-Xsemanticdb`, `-Ypickle-write`, …) are dropped (`IsolatedCheck.sanitizeOptions`).
  - **Reporter parity with sbt:** `UniqueMessagePositions` + `HideNonSensicalMessages`
    (what zinc's `DelegatingReporter` inherits), positions via `pos.nonInlined`, and
    `-explain` text appended — so a check reports the same diagnostics, at the same
    line/column, as `compile`.
  - **Fresh context, warm compiler:** every check builds a new root compiler context
    (non-interactive, byte-for-byte batch-compiler behavior), so nothing from one
    check (e.g. an unsaved overlay's definitions) can leak into the next, and a
    compile's new classes are always seen. What is kept warm is the classloader per
    compiler version (`ModuleCheck` caches up to 4, LRU): the JIT-compiled compiler and
    its cached classpath archives. Reusing one symbol table across checks in
    `Mode.Interactive` (as the REPL / IDE presentation compiler do) was measured about
    1.5x faster but was rejected: symbols entered from source survive into later runs,
    and the interactive parser is more lenient (e.g. a trailing infix operator), both
    of which break parity with `compile`.
  - **Measured** (`zz`-style bench on a ZIO module, JIT warm): full-fidelity check of
    one file ~280-450 ms vs ~550-1000 ms for sbt's incremental compile of the same edit;
    typer-only (`-Dsbta.check.stopBefore=posttyper`) ~200 ms fresh / ~80 ms with a
    reused context. The phases from inlining to erasure are most of the difference.
    The first check after sbt starts pays ~3 s to load and JIT the compiler.
