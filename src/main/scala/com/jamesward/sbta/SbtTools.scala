package com.jamesward.sbta

import zio.json.*
import zio.json.ast.Json

import java.io.File
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{ FileVisitResult, Files, Path, SimpleFileVisitor }
import java.nio.file.attribute.BasicFileAttributes
import java.util.concurrent.TimeUnit
import javax.xml.parsers.DocumentBuilderFactory
import scala.util.control.NonFatal

/**
 * The tools' work, as a client of the build's sbt server: commands through
 * `sbt/exec` (as typed at the sbt shell, on sbt's command loop, after any running
 * command), build structure through BSP, settings through `sbt/setting`. Results
 * are JSON objects in the shape of [[com.jamesward.sbta.BuildTools]]'s output
 * schemas, or an error message (never starting with `{`).
 */
final class SbtTools(server: SbtServer, log: String => Unit):

  private val MaxLog      = 20_000
  private val MaxProblems = 500

  // ---- sbt-task / list-tasks (text) ----

  def sbtTask(commandLine: String): String =
    server.session { s =>
      val r    = s.exec(commandLine)
      val jobs = runClientJobs(r)
      val status = if r.success && jobs.forall(_.exitCode == 0) then s"[ok] $commandLine" else s"[error] $commandLine: ${r.error.getOrElse("the program failed")}"
      val body   = (r.output + jobs.map(_.output).mkString).trim
      val (text, cut) = Text.lastChars(body, MaxLog)
      s"$status\n(elapsed ${r.elapsedMs} ms)\n" + (if text.isEmpty then "captured output: empty" else (if cut || r.outputTruncated then "…\n" else "") + text)
    }

  def listTasks(all: Boolean, task: Option[String]): String =
    val command = task.map(_.trim).filter(_.nonEmpty) match
      case Some(t) => s"help $t"
      case None    => if all then "tasks -V" else "tasks"
    server.session { s =>
      val r = s.exec(command)
      val text = logLines(r.output).mkString("\n").trim
      if r.success then text else s"[error] $command: ${r.error.getOrElse("")}\n$text"
    }

  // ---- structured tools (BuildTools.Run) ----

  def run(tool: String, args: Map[String, List[String]]): String =
    def one(name: String): Option[String] = args.get(name).flatMap(_.headOption).map(_.trim).filter(_.nonEmpty)
    try
      tool match
        case "compile" =>
          val config = if one("configuration").contains("test") then "Test / " else ""
          action(s"${scope(one("project"))}${config}compile")
        case "test"          => test(one("project"), args.getOrElse("tests", Nil), one("full").contains("true"), args.getOrElse("frameworkArgs", Nil))
        case "run"           => runMain(one("project"), one("mainClass"), args.getOrElse("args", Nil))
        case "clean"         => action(s"${scope(one("project"))}clean")
        case "publish-local" => publishLocal(one("project"))
        case "projects"      => projects()
        case "test-names"    => testNames(one("project"))
        case "main-classes"  => mainClasses(one("project"))
        case "show"          => show(one("key").getOrElse(""))
        case "dependencies"  => dependencies(one("project"), one("configuration").getOrElse("compile"))
        case other           => s"unknown tool $other"
    catch
      case e: InterruptedException => throw e
      case NonFatal(e) => s"$tool: ${Option(e.getMessage).getOrElse(e.toString)}"

  private def scope(project: Option[String]): String = project.fold("")(p => s"$p / ")

  private def action(command: String): String =
    server.session(s => actionJson(s.exec(command)).toJson)

  private def actionJson(r: ExecResult, extra: (String, Json)*): Json.Obj =
    val (text, cut) = Text.lastChars(r.output, MaxLog)
    val problems    = r.diagnostics.take(MaxProblems)
    Json.Obj(
      (Seq[(String, Json)](
        "command"      -> Json.Str(r.commandLine),
        "success"      -> Json.Bool(r.success),
        "elapsedMs"    -> Json.Num(r.elapsedMs),
        "errorCount"   -> Json.Num(r.diagnostics.count(_.severity == "error")),
        "warningCount" -> Json.Num(r.diagnostics.count(_.severity == "warning")),
        "problems"     -> Json.Arr(problems.map(problemJson)*),
        "log"          -> Json.Str(text),
        "logTruncated" -> Json.Bool(cut || r.outputTruncated),
      ) ++ r.error.map(e => "error" -> Json.Str(e)) ++ extra)*
    )

  private def problemJson(d: Diagnostic): Json =
    Json.Obj(
      (Seq[(String, Json)]("severity" -> Json.Str(d.severity)) ++
        d.file.map(f => "file" -> Json.Str(f)) ++
        d.line.map(l => "line" -> Json.Num(l)) ++
        d.column.map(c => "column" -> Json.Num(c)) :+
        ("message" -> Json.Str(d.message)))*
    )

  // ---- test ----

  private def test(project: Option[String], tests: List[String], full: Boolean, frameworkArgs: List[String]): String =
    server.session { s =>
      val p = scope(project)
      val fargs = if frameworkArgs.isEmpty then "" else " -- " + frameworkArgs.map(quote).mkString(" ")
      val command =
        if tests.nonEmpty || frameworkArgs.nonEmpty then s"${p}testOnly ${tests.map(quote).mkString(" ")}$fargs".replace("testOnly  --", "testOnly --")
        else if full && sbtMajor(s) >= 2 then s"${p}testFull"
        else s"${p}test"
      val startedAt = System.currentTimeMillis() - 1000
      val r         = s.exec(command)
      val reports   = TestReports.since(server.base, startedAt)
      val failures  = reports.cases.filter(c => c.status == "failed" || c.status == "error")
      val summary = Json.Obj(
        "total"   -> Json.Num(reports.cases.size),
        "passed"  -> Json.Num(reports.cases.count(_.status == "passed")),
        "failed"  -> Json.Num(reports.cases.count(_.status == "failed")),
        "errors"  -> Json.Num(reports.cases.count(_.status == "error")),
        "skipped" -> Json.Num(reports.cases.count(_.status == "skipped")),
        "suites"  -> Json.Num(reports.suites),
      )
      actionJson(
        r,
        "summary"     -> summary,
        "failures"    -> Json.Arr(failures.map(_.toJson)*),
        "suiteErrors" -> Json.Arr(),
      ).toJson
    }

  /** The build's sbt major version (1 or 2). */
  private def sbtMajor(s: Session): Int =
    s.setting("sbtVersion").toOption.collect { case (Json.Str(v), _) => v.takeWhile(_ != '.').toIntOption.getOrElse(1) }.getOrElse(1)

  // ---- run ----

  final case class JobResult(exitCode: Int, output: String)

  private def runMain(project: Option[String], mainClass: Option[String], args: List[String]): String =
    server.session { s =>
      val p       = scope(project)
      val argText = args.map(quote).mkString(" ")
      val command = mainClass.fold(s"${p}run $argText")(m => s"${p}runMain $m $argText").trim
      val r       = s.exec(command)
      val jobs    = runClientJobs(r)
      // sbt 1 runs the program itself: its output is in the log, among sbt's own lines.
      val programOutput =
        if r.clientJobs.nonEmpty then jobs.map(_.output).mkString
        else r.output.linesIterator.filterNot(SbtTools.isLogLine).mkString("\n")
      val failedJob = jobs.find(_.exitCode != 0)
      val result    = r.copy(success = r.success && failedJob.isEmpty, error = r.error.orElse(failedJob.map(j => s"the program exited with ${j.exitCode}")))
      actionJson(result, "output" -> Json.Str(Text.lastChars(programOutput, MaxLog)._1)).toJson
    }

  /**
   * sbt 2 hands `run`/`runMain` from a network client back to the client to start
   * (`sbt/clientJob`, with the JVM, classpath, options and arguments): run it here,
   * stdin disconnected, and capture its output. Interrupting the waiting thread kills it.
   */
  private def runClientJobs(r: ExecResult): List[JobResult] =
    r.clientJobs.flatMap { job =>
      val info = job.params.asObject.flatMap(_.get("runInfo")).flatMap(_.asObject).map(_.fields.toMap).getOrElse(Map.empty)
      if !info.get("jvm").contains(Json.Bool(true)) then None
      else
        val jvm  = info.get("jvmRunInfo").flatMap(_.asObject).map(_.fields.toMap).getOrElse(Map.empty)
        def str(k: String)  = jvm.get(k).collect { case Json.Str(v) => v }
        def strs(k: String) = jvm.get(k).collect { case Json.Arr(vs) => vs.toList.collect { case Json.Str(v) => v } }.getOrElse(Nil)
        val classpath = jvm.get("classpath").collect { case Json.Arr(vs) => vs.toList }.getOrElse(Nil).flatMap(_.asObject)
          .flatMap(_.get("path")).collect { case Json.Str(u) => Path.of(URI.create(u)).toString }
        val javaHome = str("javaHome").map(u => Path.of(URI.create(u))).getOrElse(Path.of(System.getProperty("java.home")))
        val java     = javaHome.resolve("bin").resolve(if SbtServer.windows then "java.exe" else "java").toString
        val command  = List(java) ++ strs("jvmOptions") ++ List("-cp", classpath.mkString(File.pathSeparator)) ++
          str("mainClass").toList ++ strs("args")
        val builder = ProcessBuilder(command*).redirectErrorStream(true)
          .redirectInput(ProcessBuilder.Redirect.from(File(if SbtServer.windows then "NUL" else "/dev/null")))
        str("workingDirectory").foreach(u => builder.directory(Path.of(URI.create(u)).toFile))
        jvm.get("environmentVariables").flatMap(_.asObject).foreach(_.fields.foreach {
          case (k, Json.Str(v)) => builder.environment().put(k, v)
          case _                => ()
        })
        log(s"running ${str("mainClass").getOrElse("?")} for sbt (client job)")
        val process = builder.start()
        try
          val out  = String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
          val code = process.waitFor()
          Some(JobResult(code, out))
        catch
          case e: InterruptedException =>
            process.descendants().forEach(_.destroyForcibly())
            process.destroyForcibly()
            throw e
        finally if process.isAlive then process.destroyForcibly()
    }

  // ---- publish-local ----

  private val Published = """published\s+\S+\s+to\s+(\S+)""".r

  private def publishLocal(project: Option[String]): String =
    server.session { s =>
      val r = s.exec(s"${scope(project)}publishLocal")
      // e.g. `published app_3 to /home/u/.ivy2/local/org/app_3/0.1.0/jars/app_3.jar`: org/module/version
      val modules = Published.findAllMatchIn(r.output).flatMap { m =>
        val parts = m.group(1).replace('\\', '/').split('/').toList
        val local = parts.indexOf("local")
        if local >= 0 && parts.length > local + 3 then Some((parts(local + 1), parts(local + 2), parts(local + 3))) else None
      }.toList.distinct
      val published = modules.map { case (org, name, version) =>
        Json.Obj("project" -> Json.Str(project.getOrElse(name)), "organization" -> Json.Str(org), "name" -> Json.Str(name), "version" -> Json.Str(version))
      }
      actionJson(r, "published" -> Json.Arr(published*)).toJson
    }

  // ---- build structure (BSP) ----

  /** A BSP build target: `file:/build/#project/Config`. */
  final case class Target(json: Json.Obj, uri: String, build: Path, project: String, config: String):
    def field(name: String): Option[Json] = json.get(name)

  private def targets(s: Session): List[Target] =
    val result = s.bsp("workspace/buildTargets", Json.Obj())
    result.asObject.flatMap(_.get("targets")).collect { case Json.Arr(ts) => ts.toList }.getOrElse(Nil).flatMap(_.asObject).flatMap { t =>
      t.get("id").flatMap(_.asObject).flatMap(_.get("uri")).collect { case Json.Str(u) => u }.flatMap { uri =>
        try
          val parsed   = URI.create(uri)
          val fragment = Option(parsed.getFragment).getOrElse("")
          val build    = Path.of(URI(parsed.getScheme, parsed.getSchemeSpecificPart, null)).toAbsolutePath.normalize
          fragment.split('/') match
            case Array(project, config) => Some(Target(t, uri, build, project, config)) // meta-builds have no config
            case _                      => None
        catch case NonFatal(_) => None
      }
    }

  private def rootBuild: Path = server.base.toAbsolutePath.normalize

  private def projects(): String =
    server.session { s =>
      val all     = targets(s)
      val current = currentProject(s)
      val byProject = all.groupBy(t => (t.build, t.project)).toList.sortBy { case ((b, p), _) => (b != rootBuild, b.toString, p) }
      val idOf    = all.map(t => t.uri -> t.project).toMap
      val aggregates = aggregation(s, all.filter(_.build == rootBuild).map(_.project).distinct, current)
      val infos = byProject.map { case ((build, project), ts) =>
        val compile  = ts.find(_.config == "Compile").getOrElse(ts.head)
        val inRoot   = build == rootBuild
        val baseDir  = compile.field("baseDirectory").collect { case Json.Str(u) => Path.of(URI.create(u)).toAbsolutePath.normalize }
        val scalaVer = compile.field("data").flatMap(_.asObject).flatMap(_.get("scalaVersion")).collect { case Json.Str(v) => v }
        val dependsOn = compile.field("dependencies").collect { case Json.Arr(ds) => ds.toList }.getOrElse(Nil)
          .flatMap(_.asObject).flatMap(_.get("uri")).collect { case Json.Str(u) => u }
          .flatMap(u => idOf.get(u).filter(_ != project)).distinct
        def settingStr(key: String) =
          if !inRoot then None else s.setting(s"$project / $key").toOption.collect { case (Json.Str(v), _) => v }
        Json.Obj(
          (Seq[(String, Json)](
            "id"      -> Json.Str(project),
            "base"    -> Json.Str(baseDir.map(b => if b.startsWith(rootBuild) then { val r = rootBuild.relativize(b).toString; if r.isEmpty then "." else r } else b.toString).getOrElse(".")),
            "current" -> Json.Bool(inRoot && current.contains(project)),
            "root"    -> Json.Bool(inRoot && baseDir.contains(rootBuild)),
          ) ++ (if inRoot then None else Some("build" -> Json.Str(build.toUri.toString))) ++
            settingStr("name").map(v => "name" -> Json.Str(v)) ++
            settingStr("organization").map(v => "organization" -> Json.Str(v)) ++
            settingStr("version").map(v => "version" -> Json.Str(v)) ++
            scalaVer.map(v => "scalaVersion" -> Json.Str(v)) ++
            Seq(
              "aggregates" -> Json.Arr((if inRoot then aggregates.getOrElse(project, Nil) else Nil).map(Json.Str(_))*),
              "dependsOn"  -> Json.Arr(dependsOn.map(d => Json.Obj("project" -> Json.Str(d)))*),
            ))*
        )
      }
      Json.Obj(
        "currentProject" -> Json.Str(current.getOrElse("")),
        "projects"       -> Json.Arr(infos*),
      ).toJson
    }

  @volatile private var aggregationCache: Option[(Vector[String], Map[String, List[String]])] = None

  /**
   * Which projects each project aggregates, directly. BSP doesn't model aggregation,
   * but `show` follows it: `show <p> / name` prints one `<q> / name` block per project
   * `q` that `p` aggregates (transitively, `p` included), and only the value when it
   * aggregates nothing. The direct aggregates are those not reached through another.
   * Cached until the build definition changes.
   */
  private def aggregation(s: Session, projects: List[String], current: Option[String]): Map[String, List[String]] =
    val fingerprint = server.buildFingerprint()
    aggregationCache.collect { case (f, result) if f == fingerprint => result }.getOrElse {
      val header = """^(?:(\S+) / )?name$""".r
      val transitive = projects.map { p =>
        val lines = s.exec(s"show $p / name").output.linesIterator
          .map(line => line.split("""\[info\]\s?""", -1).last).filter(_.trim.nonEmpty).toList
        // Headers are flush left; values under them are indented.
        val owners = lines.filterNot(_.startsWith("\t")).filter(_ => lines.size > 1).collect {
          case header(owner) => Option(owner).orElse(current).getOrElse(p)
        }
        p -> owners.filter(_ != p).distinct
      }.toMap
      val direct = transitive.map { case (p, reached) =>
        val viaOthers = reached.flatMap(q => transitive.getOrElse(q, Nil)).toSet
        p -> reached.filterNot(viaOthers)
      }
      aggregationCache = Some(fingerprint -> direct)
      direct
    }

  /** The session's current project: the one `projects` marks with `*`. */
  private def currentProject(s: Session): Option[String] =
    val r = s.exec("projects")
    logLines(r.output).map(_.trim).collectFirst { case line if line.startsWith("* ") => line.drop(2).trim }

  /** The root build's targets of one configuration, for `project` (or every project when it's None). */
  private def rootTargets(s: Session, project: Option[String], config: String): List[Target] =
    val ts = targets(s).filter(t => t.build == rootBuild && t.config == config)
    project match
      case Some(p) =>
        val matching = ts.filter(_.project == p)
        if matching.isEmpty then throw IllegalArgumentException(s"no project `$p` (see the `projects` tool)") else matching
      case None => ts

  private def targetIds(ts: List[Target]): Json =
    Json.Obj("targets" -> Json.Arr(ts.map(t => Json.Obj("uri" -> Json.Str(t.uri)))*))

  private def itemsByTarget(result: Json): List[(String, Json.Obj)] =
    result.asObject.flatMap(_.get("items")).collect { case Json.Arr(is) => is.toList }.getOrElse(Nil).flatMap(_.asObject).flatMap { item =>
      item.get("target").flatMap(_.asObject).flatMap(_.get("uri")).collect { case Json.Str(u) => u -> item }
    }

  private def testNames(project: Option[String]): String =
    server.session { s =>
      val ts     = rootTargets(s, project, "Test")
      val byUri  = ts.map(t => t.uri -> t.project).toMap
      val items  = itemsByTarget(s.bsp("buildTarget/scalaTestClasses", targetIds(ts)))
      val grouped = items.groupMapReduce((uri, _) => byUri.getOrElse(uri, uri)) { (_, item) =>
        item.get("classes").collect { case Json.Arr(cs) => cs.toList.collect { case Json.Str(c) => c } }.getOrElse(Nil)
      }(_ ++ _)
      Json.Obj("projects" -> Json.Arr(grouped.toList.sortBy(_._1).map { case (p, tests) =>
        Json.Obj("project" -> Json.Str(p), "tests" -> Json.Arr(tests.distinct.sorted.map(Json.Str(_))*))
      }*)).toJson
    }

  private def mainClasses(project: Option[String]): String =
    server.session { s =>
      val ts    = rootTargets(s, project, "Compile")
      val byUri = ts.map(t => t.uri -> t.project).toMap
      val items = itemsByTarget(s.bsp("buildTarget/scalaMainClasses", targetIds(ts)))
      val grouped = items.groupMapReduce((uri, _) => byUri.getOrElse(uri, uri)) { (_, item) =>
        item.get("classes").collect { case Json.Arr(cs) => cs.toList }.getOrElse(Nil).flatMap(_.asObject).flatMap(_.get("class")).collect { case Json.Str(c) => c }
      }(_ ++ _)
      Json.Obj("projects" -> Json.Arr(grouped.toList.sortBy(_._1).map { case (p, classes) =>
        val sorted = classes.distinct.sorted
        // `Compile / mainClass` is a task, which `sbt/setting` can't read: report the only one when unambiguous.
        val default = if sorted.size == 1 then Some(sorted.head) else None
        Json.Obj(
          (Seq[(String, Json)]("project" -> Json.Str(p), "mainClasses" -> Json.Arr(sorted.map(Json.Str(_))*)) ++
            default.map(d => "defaultMainClass" -> Json.Str(d)))*
        )
      }*)).toJson
    }

  // ---- dependencies ----

  /**
   * sbt's `update` report for a project: `update` runs first (it resolves only when
   * the dependencies changed), and its result is read from the JSON cache sbt keeps
   * of it, `update/update_cache_<n>/output` under the project's `target` (the
   * serialized `UpdateReport`, the same data the sbt shell's `update` returns).
   */
  private def dependencies(project: Option[String], configuration: String): String =
    val configurations = List("compile", "runtime", "test", "provided")
    if !configurations.contains(configuration) then
      s"dependencies: unknown configuration '$configuration' (expected ${configurations.mkString(", ")})"
    else
      server.session { s =>
        project.orElse(currentProject(s)) match
          case None => "dependencies: could not determine the current project; pass `project`"
          case Some(p) =>
            val updated = s.exec(s"$p / update")
            if !updated.success then
              s"dependencies: `$p / update` failed: ${updated.error.getOrElse("")}\n${Text.lastChars(updated.output, 4000)._1}"
            else
              val report = targetOf(s, p).flatMap(updateCache).flatMap(file =>
                try zio.json.ast.Json.decoder.decodeJson(java.nio.file.Files.readString(file)).toOption
                catch case NonFatal(_) => None
              )
              report match
                case None => s"dependencies: could not read the update report of $p (under its `target`)"
                case Some(json) =>
                  val declared = declaredDependencies(s, p)
                  def isDirect(org: String, name: String) =
                    declared.exists(d => d.org == org && (d.name == name || name.startsWith(d.name + "_")))
                  val config = arrOf(json, "configurations").find(c =>
                    c.asObject.flatMap(_.get("configuration")).flatMap(_.asObject).flatMap(_.get("name")).contains(Json.Str(configuration))
                  )
                  val modules = config.toList.flatMap(arrOf(_, "modules"))
                  val details = config.toList.flatMap(arrOf(_, "details")).flatMap(arrOf(_, "modules"))
                  def coordinates(report: Json): Option[(String, String, String)] =
                    report.asObject.flatMap(_.get("module")).flatMap(_.asObject).map(_.fields.toMap).map { m =>
                      def f(k: String) = m.get(k).collect { case Json.Str(v) => v }.getOrElse("")
                      (f("organization"), f("name"), f("revision"))
                    }
                  def evicted(report: Json) = report.asObject.flatMap(_.get("evicted")).contains(Json.Bool(true))
                  def module(c: (String, String, String), extra: (String, Json)*) =
                    Json.Obj((Seq[(String, Json)]("organization" -> Json.Str(c._1), "name" -> Json.Str(c._2), "revision" -> Json.Str(c._3)) ++ extra)*)
                  val resolved = modules.filterNot(evicted).flatMap(coordinates).distinct
                    .map(c => module(c, "direct" -> Json.Bool(isDirect(c._1, c._2))))
                  val evictedModules = (modules ++ details).filter(evicted).flatMap(coordinates).distinct.map(module(_))
                  Json.Obj(
                    "project"       -> Json.Str(p),
                    "configuration" -> Json.Str(configuration),
                    "declared"      -> Json.Arr(declared.map(d => module((d.org, d.name, d.revision), d.configurations.map(c => "configurations" -> Json.Str(c)).toSeq*))*),
                    "resolved"      -> Json.Arr(resolved*),
                    "evicted"       -> Json.Arr(evictedModules*),
                  ).toJson
      }

  private final case class Declared(org: String, name: String, revision: String, configurations: Option[String])

  /** `libraryDependencies`: as JSON (sbt 1), or from `show`'s `* org:name:revision[:configurations]` lines (sbt 2). */
  private def declaredDependencies(s: Session, project: String): List[Declared] =
    s.setting(s"$project/libraryDependencies") match
      case Right((Json.Arr(items), _)) =>
        items.toList.flatMap(_.asObject).map(_.fields.toMap).map { m =>
          def f(k: String) = m.get(k).collect { case Json.Str(v) => v }
          Declared(f("organization").getOrElse(""), f("name").getOrElse(""), f("revision").getOrElse(""), f("configurations"))
        }
      case _ =>
        // `show` aggregates: an aggregating project prints a `<project> / libraryDependencies`
        // block per project (its own block's header is bare when it is the current project).
        // A line is read from its last `[info]`: console bytes of an earlier command can
        // arrive late, ahead of this one's first line.
        val lines  = s.exec(s"show $project / libraryDependencies").output.linesIterator
          .map(line => line.split("""\[info\]\s?""", -1).last.trim).toList
        val header = """^(?:(\S+) / )?libraryDependencies$""".r
        val blocks = lines.foldLeft(List.empty[(Option[String], List[String])]) {
          case (acc, header(owner)) => (Option(owner).orElse(Some("")), Nil) :: acc
          case (Nil, line)          => List(None -> List(line))
          case ((owner, items) :: rest, line) => (owner, line :: items) :: rest
        }.map((owner, items) => owner -> items.reverse)
        val own =
          if blocks.forall(_._1.isEmpty) then blocks.flatMap(_._2)
          else blocks.collectFirst { case (Some(`project`), items) => items }
            .orElse(blocks.collectFirst { case (Some(""), items) => items }).getOrElse(Nil)
        own.filter(_.startsWith("* ")).flatMap { line =>
          line.drop(2).trim.split(':').toList match
            case org :: name :: revision :: rest => Some(Declared(org, name, revision, rest.headOption.filter(_.nonEmpty)))
            case _                               => None
        }

  /** A project's `target` directory (a path in sbt 1, a `{first: uri}` file reference in sbt 2). */
  private def targetOf(s: Session, project: String): Option[Path] =
    s.setting(s"$project/target").toOption.flatMap {
      case (Json.Str(v), _) => Some(v)
      case (other, _)       => other.asObject.flatMap(_.get("first")).collect { case Json.Str(v) => v }
    }.flatMap { v =>
      try Some(if v.startsWith("file:") then Path.of(URI.create(v)) else Path.of(v)) catch case NonFatal(_) => None
    }

  /** The newest `update/update_cache_<n>/output` under `target`. */
  private def updateCache(target: Path): Option[Path] =
    try
      scala.util.Using.resource(Files.find(target, 6, (p, attrs) =>
        attrs.isRegularFile && p.getFileName.toString == "output" &&
          Option(p.getParent).exists(_.getFileName.toString.startsWith("update_cache_")) &&
          Option(p.getParent.getParent).exists(_.getFileName.toString == "update")
      )) { found =>
        import scala.jdk.CollectionConverters.*
        found.iterator.asScala.toList.maxByOption(p => Files.getLastModifiedTime(p).toMillis)
      }
    catch case NonFatal(_) => None

  private def arrOf(json: Json, field: String): List[Json] =
    json.asObject.flatMap(_.get(field)).collect { case Json.Arr(xs) => xs.toList }.getOrElse(Nil)

  // ---- show ----

  private def show(key: String): String =
    if key.isEmpty then "show: a `key` is required"
    else server.session { s =>
      // `sbt/setting` needs a project axis: give an unscoped key the current project's.
      val lookup = s.setting(key) match
        case Left(e) if e.startsWith("Not a valid project ID") || e.startsWith("Expected project ID") =>
          currentProject(s).map(p => s.setting(s"$p / ${key.trim}")).getOrElse(Left(e))
        case other => other
      lookup match
        case Right((value, contentType)) =>
          Json.Obj(
            "key"       -> Json.Str(key),
            "kind"      -> Json.Str("setting"),
            "valueType" -> Json.Str(contentType.stripPrefix("class ").stripPrefix("interface ")),
            "value"     -> Json.Str(Text.lastChars(value match { case Json.Str(v) => v; case other => other.toJsonPretty }, MaxLog)._1),
            "json"      -> value,
          ).toJson
        case Left(settingError) =>
          // A task, or a setting sbt can't write as JSON: run `show` and take its value from the log.
          val kind = if settingError.contains("JsonWriter") then "setting" else "task"
          val r    = s.exec(s"show $key")
          if !r.success then s"show $key: ${r.error.getOrElse("failed")}\n${Text.lastChars(r.output, 4000)._1}"
          else
            val value = logLines(r.output).filterNot(_.startsWith("[")).mkString("\n").trim
            Json.Obj(
              "key"       -> Json.Str(key),
              "kind"      -> Json.Str(kind),
              "valueType" -> Json.Str("unknown"),
              "value"     -> Json.Str(Text.lastChars(value, MaxLog)._1),
            ).toJson
    }

  // ---- helpers ----

  /** sbt's `[info] …` lines, without the level prefix (other lines, e.g. `[success]`, keep theirs). */
  private def logLines(output: String): List[String] =
    output.linesIterator.map(l => if l.startsWith("[info] ") then l.drop(7) else l).filterNot(_.startsWith("[success]")).toList

  /** Quote an argument for sbt's input-task parser when it has spaces or quotes. */
  private def quote(arg: String): String =
    if arg.nonEmpty && !arg.exists(c => c.isWhitespace || c == '"') then arg
    else "\"" + arg.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

object SbtTools:
  private val LogLine = """^\[(info|warn|error|success|debug)\]""".r
  def isLogLine(line: String): Boolean = LogLine.findPrefixOf(line).isDefined

/** Test results from the JUnit XML reports sbt writes (`testReportsDirectory`, on by default). */
object TestReports:

  final case class Case(suite: String, test: String, status: String, message: Option[String], durationMs: Option[Long]):
    def toJson: Json =
      Json.Obj(
        (Seq[(String, Json)]("suite" -> Json.Str(suite), "test" -> Json.Str(test), "status" -> Json.Str(status)) ++
          message.map(m => "message" -> Json.Str(m)) ++ durationMs.map(d => "durationMs" -> Json.Num(d)))*
      )

  final case class Reports(suites: Int, cases: List[Case])

  /** Directories never holding reports; skipped so the walk stays fast on large builds. */
  private val Skip = Set(".git", ".bsp", ".idea", ".metals", ".bloop", "node_modules", "src", "classes", "test-classes",
    "streams", "zinc", "bg-jobs", "resolution-cache", "sync", "cache", "out-of-date")

  /** The reports written at or after `sinceMillis` anywhere under the build root. */
  def since(base: Path, sinceMillis: Long): Reports =
    val files = scala.collection.mutable.ListBuffer[Path]()
    try
      Files.walkFileTree(
        base,
        java.util.EnumSet.noneOf(classOf[java.nio.file.FileVisitOption]),
        12,
        new SimpleFileVisitor[Path]:
          override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult =
            val name = Option(dir.getFileName).map(_.toString).getOrElse("")
            if dir != base && Skip(name) then FileVisitResult.SKIP_SUBTREE else FileVisitResult.CONTINUE
          override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult =
            val name = file.getFileName.toString
            if name.startsWith("TEST-") && name.endsWith(".xml") && file.getParent.getFileName.toString == "test-reports" &&
              attrs.lastModifiedTime.toMillis >= sinceMillis
            then files += file
            FileVisitResult.CONTINUE
          override def visitFileFailed(file: Path, exc: java.io.IOException): FileVisitResult = FileVisitResult.CONTINUE
      )
    catch case NonFatal(_) => ()
    val parsed = files.toList.flatMap(parse)
    Reports(files.size, parsed)

  /** sbt's wrappers around a forked test's exception (`sbt.ForkMain$ForkError: …`, sbt 2 `…ForkTestMain$ForkError: …PersistedException: …`). */
  private val Wrappers = """^(?:sbt\.[\w.$]*(?:ForkError|PersistedException):\s*)+""".r

  /**
   * The failure's message, with line breaks (attribute values lose them, so the
   * element's text, the full stack trace, is read up to its first frame).
   */
  private def failureMessage(f: org.w3c.dom.Element): String =
    val text  = Option(f.getTextContent).filter(_.trim.nonEmpty).getOrElse(Option(f.getAttribute("message")).getOrElse(""))
    val lines = Text.stripAnsi(text).linesIterator.toList
    val head  = lines.takeWhile(l => !l.trim.startsWith("at ")).mkString("\n").trim
    Wrappers.replaceFirstIn(head, "")

  def parse(file: Path): List[Case] =
    try
      val factory = DocumentBuilderFactory.newInstance()
      factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
      val doc   = factory.newDocumentBuilder().parse(file.toFile)
      val cases = doc.getElementsByTagName("testcase")
      (0 until cases.getLength).toList.map { i =>
        val e       = cases.item(i).asInstanceOf[org.w3c.dom.Element]
        def child(tag: String) = Option(e.getElementsByTagName(tag).item(0)).map(_.asInstanceOf[org.w3c.dom.Element])
        val failure = child("failure").map("failed" -> _).orElse(child("error").map("error" -> _))
        val status  = failure.map(_._1).getOrElse(if child("skipped").isDefined then "skipped" else "passed")
        val message = failure.map { case (_, f) => failureMessage(f) }.filter(_.nonEmpty)
        val duration = Option(e.getAttribute("time")).flatMap(_.toDoubleOption).map(s => (s * 1000).round)
        Case(e.getAttribute("classname"), e.getAttribute("name"), status, message, duration)
      }
    catch case NonFatal(_) => Nil
