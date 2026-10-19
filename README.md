# sbta

An sbt launcher with an MCP server. It needs no plugin and works with sbt 1.4+ and 2.x
builds.

## Agent setup

Paste this into your coding agent, in the root of your sbt project:

```text
Set up sbta (https://github.com/jamesward/sbta) in this sbt project, so you can drive the
build through its MCP server:

1. Download the launcher scripts into the project root and make the Unix one executable:
     curl -fsSL -o sbta https://github.com/jamesward/sbta/releases/latest/download/sbta
     curl -fsSL -o sbta.bat https://github.com/jamesward/sbta/releases/latest/download/sbta.bat
     chmod +x sbta
   (`./sbta` is sbt: an existing `./sbt` can stay, and works as before.)
2. Register the MCP server for the agent you are, choosing kiro, claude or cursor
   (separate several with commas or spaces):
     ./sbta --mcp --setup <client>
   It adds an `sbta` server (`./sbta --mcp`) to the project's MCP config and keeps
   any other servers there.
3. Add the "Build, Test & Dev Workflow" guidance it prints to the project's AGENTS.md
   (create the file if missing; merge it with an existing section of that name).
4. Tell me which files you added or changed, that they should be committed, and that
   I need to restart or reconnect you (the MCP client) to load the `sbta` server.
   The MCP server needs macOS or Linux (not Windows yet).
```

## Usage

- `./sbta <args>` is sbt: the same options, `.sbtopts`/`.jvmopts`, sbtn for sbt 2, and
  sbt's own launcher (embedded, running in the sbta JVM). `./sbta` alone is sbt's shell.
- `./sbta --mcp` is an MCP stdio server for the build, a client of sbt's own server
  (like `sbtn`): it starts sbt (`./sbta --client`) when it isn't running, and sbt keeps
  running after it, shared with your own `./sbta` commands.

## Install

The `sbta` and `sbta.bat` scripts are sbt's own `sbt` and `sbt.bat`, changed only to
download and run the sbta jar of their version (from its
[GitHub release](https://github.com/jamesward/sbta/releases), checked against its
`.sha1`) instead of sbt's launcher, and to run `--mcp` in the JVM. Download them from a
release and commit them to the build, like `./sbt`. The jar needs JDK 17+.
`$SBTA_RELEASES_URL` replaces `https://github.com/jamesward/sbta/releases/download`
(for a mirror with the same `v<version>/sbta-<version>.jar` layout).

## Updates

The scripts pin their sbta version, so updating is a change to commit:
`./sbta --update` replaces `sbta` and `sbta.bat` with the newest release's (or
`./sbta --update <version>`), checked against the release's `SHA256SUMS`.

sbta checks GitHub for the newest release (semver, releases only) at most once a day,
in the background, and says when there's a newer one: on stderr when you run `./sbta`,
and over MCP in the server's instructions and as a note on every tool result, for the
agent to pass on (it can update with the `update-sbta` tool). `--no-update-notice`
(an `--mcp` option) drops the note from tool results; `SBTA_NO_UPDATE_NOTICE=1` turns
the notices off, and `SBTA_NO_UPDATE_CHECK=1` (or `CI=true`) the check.

## MCP

`./sbta --mcp --setup kiro|claude|cursor` writes this (as the agent setup does), into
`.kiro/settings/mcp.json`, `.mcp.json` (and approves it in `.claude/settings.json`), or
`.cursor/mcp.json`:

```json
{
  "mcpServers": {
    "sbta": {
      "type": "stdio",
      "command": "./sbta",
      "args": ["--mcp"],
      "timeout": 1800000
    }
  }
}
```

Clients start project servers in the project's root directory; elsewhere, add
`--project-dir=<build root>`.

| Tool | What it does |
|---|---|
| `compile` | Compile (`configuration`: `compile`/`test`); errors and warnings as `{severity, file, line, column, message}` |
| `test` | Run tests (`tests` for classes or globs, `full`, `frameworkArgs`); counts and each failure's suite, test and message |
| `run` | Run a main class with `args`; returns the program's output |
| `clean`, `publish-local` | Delete build outputs; publish locally (returns the coordinates) |
| `projects` | Projects: ids, base directories, versions, Scala version, aggregation, `dependsOn` |
| `test-names`, `main-classes` | Test classes and main classes per project |
| `dependencies` | Declared, resolved (direct/transitive) and evicted library dependencies |
| `show` | A setting's value (as JSON when sbt can write it), or a task's result |
| `sbt-task`, `list-tasks` | Run any sbt command line; list the tasks |
| `check` | Type-check Scala 3 files without compiling, with the module's own compiler (warm, in sbta), never waiting behind sbt |
| `glob-search`, `inspect`, `symbol-location` | Scala 3 symbols from TASTy (3.3–3.10): search, members, source location |
| `update-sbta` | Update the build's `sbta` and `sbta.bat` to the newest (or a given) release |

Plus the [javadocs.dev](https://www.javadocs.dev/mcp) tools, proxied. Commands run on
sbt's command loop one at a time, shared with any other sbt client; cancelling a call
cancels its command.

Options: `--project-dir=<dir>`, `--start=eager|lazy|never` (start sbt right away, on
the first call that needs it, or never), `--docs-url=<url>|none`, `--setup <clients>`,
`--no-update-notice`. Logs:
`project/target/sbta/mcp.log` and `project/target/sbta/boot.log`.

`--mcp` needs Unix domain sockets: not on Windows yet (sbt's server is a named pipe
there). Everything else works on Windows with `sbta.bat`.

## Security

sbta connects only to sbt's own server socket, which only your user can open. The
tools run arbitrary build code (`sbt-task`, `run`, `show`). The docs proxy makes
outbound requests to javadocs.dev; `--docs-url=none` turns it off. The daily update
check calls the GitHub API (`api.github.com`); `SBTA_NO_UPDATE_CHECK=1` turns it off.

## Development

See [DEV.md](DEV.md).

## License

Apache-2.0
