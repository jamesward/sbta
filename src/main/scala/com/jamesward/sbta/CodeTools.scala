package com.jamesward.sbta

import com.jamesward.sbta.{ ModuleCheck, SbtaCheck, SymbolIndexState }
import zio.json.ast.Json

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path }
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

/**
 * `check` and the symbol tools (`glob-search`, `inspect`, `symbol-location`): the
 * plugin's engines (a warm, isolated copy of the module's own Scala compiler; TASTy
 * Query readers), run in sbta's JVM and fed from BSP: each module's compiler jars,
 * classpath, options, class and source directories.
 *
 * That module data comes from requests sbt queues on its command loop, so it is
 * cached (until a build definition file changes, or sbt restarts) and, while sbt is
 * busy, the cached copy is used: a check never waits behind a running build.
 */
final class CodeTools(server: SbtServer, log: String => Unit):

  /** One BSP build target (a project's Compile or Test configuration) and what `check` needs from it. */
  final case class Module(
      uri: String,
      build: Path,
      project: String,
      configuration: String,
      scalaVersion: String,
      compilerJars: List[Path],
      classpath: List[Path],
      classDirectory: Option[Path],
      options: List[String],
      sourceDirectories: List[Path],
      sourceFiles: List[Path],
  ):
    def label = s"$project/$configuration"

  private final case class Model(fingerprint: Vector[String], modules: List[Module])

  /** Kept on disk too (`project/target/sbta/modules.json`), so a new sbta process doesn't wait behind a busy sbt. */
  private val cacheFile = server.runtimeDir.resolve("modules.json")

  @volatile private var cached: Option[Model] = readCache()

  private def writeCache(model: Model): Unit =
    try
      def strs(xs: List[Path]) = Json.Arr(xs.map(p => Json.Str(p.toString))*)
      val json = Json.Obj(
        "fingerprint" -> Json.Arr(model.fingerprint.map(Json.Str(_))*),
        "modules" -> Json.Arr(model.modules.map { m =>
          Json.Obj(
            "uri" -> Json.Str(m.uri), "build" -> Json.Str(m.build.toString), "project" -> Json.Str(m.project),
            "configuration" -> Json.Str(m.configuration), "scalaVersion" -> Json.Str(m.scalaVersion),
            "compilerJars" -> strs(m.compilerJars), "classpath" -> strs(m.classpath),
            "classDirectory" -> m.classDirectory.fold(Json.Null)(p => Json.Str(p.toString)),
            "options" -> Json.Arr(m.options.map(Json.Str(_))*),
            "sourceDirectories" -> strs(m.sourceDirectories), "sourceFiles" -> strs(m.sourceFiles),
          )
        }*),
      )
      Files.createDirectories(cacheFile.getParent)
      val temp = Files.createTempFile(cacheFile.getParent, "modules", ".tmp")
      Files.writeString(temp, json.toString, StandardCharsets.UTF_8)
      Files.move(temp, cacheFile, java.nio.file.StandardCopyOption.REPLACE_EXISTING, java.nio.file.StandardCopyOption.ATOMIC_MOVE)
    catch case NonFatal(e) => log(s"could not write $cacheFile: $e")

  private def readCache(): Option[Model] =
    try
      if !Files.isRegularFile(cacheFile) then None
      else
        Json.decoder.decodeJson(Files.readString(cacheFile, StandardCharsets.UTF_8)).toOption.flatMap(_.asObject).map(_.fields.toMap).map { root =>
          def strList(j: Option[Json]) = j.collect { case Json.Arr(xs) => xs.toList.collect { case Json.Str(v) => v } }.getOrElse(Nil)
          val modules = root.get("modules").collect { case Json.Arr(xs) => xs.toList }.getOrElse(Nil).flatMap(_.asObject).map(_.fields.toMap).map { m =>
            def str(k: String) = m.get(k).collect { case Json.Str(v) => v }.getOrElse("")
            Module(
              str("uri"), Path.of(str("build")), str("project"), str("configuration"), str("scalaVersion"),
              strList(m.get("compilerJars")).map(Path.of(_)), strList(m.get("classpath")).map(Path.of(_)),
              m.get("classDirectory").collect { case Json.Str(v) => Path.of(v) }, strList(m.get("options")),
              strList(m.get("sourceDirectories")).map(Path.of(_)), strList(m.get("sourceFiles")).map(Path.of(_)),
            )
          }
          Model(strList(root.get("fingerprint")).toVector, modules)
        }
    catch case NonFatal(_) => None

  private def root: Path = server.base.toAbsolutePath.normalize

  // ---- the module model (BSP) ----

  /**
   * The modules, with a note when they may be out of date. Uses the cache when the
   * build definition is unchanged; otherwise asks sbt, unless it is busy and a cached
   * copy exists.
   */
  private def model(): (List[Module], Option[String]) =
    val current = server.buildFingerprint()
    cached match
      case Some(m) if m.fingerprint == current => (m.modules, None)
      case previous =>
        server.session { s =>
          val loaded =
            if previous.isDefined then loadModules(s, unlessBusy = true)
            else loadModules(s, unlessBusy = false)
          loaded match
            case Some(modules) =>
              val model = Model(current, modules)
              cached = Some(model)
              writeCache(model)
              (modules, None)
            case None =>
              (previous.get.modules, Some("sbt is busy: using the module settings read before the build definition changed"))
        }

  private def loadModules(s: Session, unlessBusy: Boolean): Option[List[Module]] =
    def ask(method: String, params: Json): Option[Json] =
      if unlessBusy then s.bspUnlessBusy(method, params) else Some(s.bsp(method, params))
    for
      targetsResult <- ask("workspace/buildTargets", Json.Obj())
      targets = arr(targetsResult, "targets").flatMap(_.asObject).flatMap { t =>
                  val fields = t.fields.toMap
                  for
                    uri  <- fields.get("id").flatMap(obj(_, "uri")).collect { case Json.Str(u) => u }
                    data <- fields.get("data").flatMap(_.asObject).map(_.fields.toMap)
                    (build, project, config) <- parseTarget(uri)
                  yield (uri, build, project, config, data)
                }
      ids = Json.Obj("targets" -> Json.Arr(targets.map(t => Json.Obj("uri" -> Json.Str(t._1)))*))
      options <- ask("buildTarget/scalacOptions", ids)
      sources <- ask("buildTarget/sources", ids)
    yield
      val optionsByUri = items(options)
      val sourcesByUri = items(sources)
      targets.map { case (uri, build, project, config, data) =>
        val opt = optionsByUri.getOrElse(uri, Map.empty)
        val src = sourcesByUri.get(uri).flatMap(_.get("sources")).collect { case Json.Arr(xs) => xs.toList }.getOrElse(Nil)
          .flatMap(_.asObject).map(_.fields.toMap)
        def srcPaths(kind: Int) = src.filter(_.get("kind").collect { case Json.Num(n) => n.intValue }.contains(kind))
          .flatMap(_.get("uri")).collect { case Json.Str(u) => path(u) }.flatten
        Module(
          uri = uri,
          build = build,
          project = project,
          configuration = config,
          scalaVersion = data.get("scalaVersion").collect { case Json.Str(v) => v }.getOrElse(""),
          compilerJars = data.get("jars").collect { case Json.Arr(js) => js.toList.collect { case Json.Str(u) => u } }.getOrElse(Nil).flatMap(path),
          classpath = opt.get("classpath").collect { case Json.Arr(cs) => cs.toList.collect { case Json.Str(u) => u } }.getOrElse(Nil).flatMap(path),
          classDirectory = opt.get("classDirectory").collect { case Json.Str(u) => u }.flatMap(path),
          options = opt.get("options").collect { case Json.Arr(os) => os.toList.collect { case Json.Str(o) => o } }.getOrElse(Nil),
          sourceDirectories = srcPaths(2),
          sourceFiles = srcPaths(1),
        )
      }

  private def parseTarget(uri: String): Option[(Path, String, String)] =
    try
      val parsed = URI.create(uri)
      val build  = Path.of(URI(parsed.getScheme, parsed.getSchemeSpecificPart, null)).toAbsolutePath.normalize
      Option(parsed.getFragment).map(_.split('/')).collect { case Array(project, config) => (build, project, config) }
    catch case NonFatal(_) => None

  private def path(uri: String): Option[Path] =
    try Some(Path.of(URI.create(uri)).toAbsolutePath.normalize) catch case NonFatal(_) => None

  private def obj(json: Json, field: String): Option[Json] = json.asObject.flatMap(_.get(field))
  private def arr(json: Json, field: String): List[Json] =
    obj(json, field).collect { case Json.Arr(xs) => xs.toList }.getOrElse(Nil)
  private def items(result: Json): Map[String, Map[String, Json]] =
    arr(result, "items").flatMap(_.asObject).map(_.fields.toMap).flatMap { item =>
      item.get("target").flatMap(obj(_, "uri")).collect { case Json.Str(u) => u -> item }
    }.toMap

  // ---- check ----

  def check(files: List[String], content: Option[String], scope: Option[String]): String =
    val paths = files.map(_.trim).filter(_.nonEmpty).map(f => root.resolve(f).toAbsolutePath.normalize).distinct
    def fail(message: String) = s"check: $message"
    if paths.isEmpty then fail("provide at least one file in `files`")
    else if content.isDefined && paths.size != 1 then fail("`content` can only be used with exactly one file")
    else
      paths.find(p => !p.toString.endsWith(".scala")).map(p => fail(s"only .scala files can be checked: $p"))
        .orElse(if content.isEmpty then paths.find(p => !Files.isRegularFile(p)).map(p => fail(s"file not found: $p")) else None)
        .getOrElse {
          val (modules, note) = model()
          val owners = paths.map(p => p -> ownerOf(modules, p))
          owners.collectFirst { case (p, None) => fail(s"$p is not in a source directory of any project") }.getOrElse {
            val distinct = owners.flatMap(_._2).distinct
            if distinct.size != 1 then
              fail(s"files belong to different modules (${distinct.map(_.label).mkString(", ")}); check each module separately")
            else
              val module = distinct.head
              if !SbtaCheck.isSupportedScala(module.scalaVersion) then
                fail(s"${module.project} uses Scala ${module.scalaVersion}; check supports Scala 3.3 and later")
              else
                val overlays = content.map(text => Map(paths.head -> text)).getOrElse(Map.empty)
                SbtaCheck.run(moduleConfig(module, note), paths, overlays, scope, root)
          }
        }

  /** The module whose source directories contain `path`, by the most specific directory. */
  private def ownerOf(modules: List[Module], path: Path): Option[Module] =
    modules.flatMap { m =>
      (m.sourceDirectories.filter(path.startsWith).map(_.getNameCount) ++
        m.sourceFiles.filter(_ == path).map(_.getNameCount + 1)).maxOption.map(_ -> m)
    }.sortBy(-_._1).headOption.map(_._2)

  private def moduleConfig(m: Module, note: Option[String]): ModuleCheck.ModuleConfig =
    val sources = (m.sourceDirectories.flatMap(sourcesUnder) ++ m.sourceFiles).distinct
    // The module's last compile: its newest class file. Sources changed since then are
    // re-checked from source too (their compiled classes are stale).
    val lastCompile = m.classDirectory.flatMap(newestFile)
    ModuleCheck.ModuleConfig(
      projectId = m.project,
      configuration = m.configuration.toLowerCase,
      scalaVersion = m.scalaVersion,
      compilerJars = m.compilerJars,
      classpath = (m.classDirectory.toList ++ m.classpath).distinct,
      scalacOptions = m.options,
      sources = sources,
      recordedStamps = lastCompile.map(_ => sources.map(_ -> "compiled").toMap),
      currentStamp = (source, previous) =>
        lastCompile.map(compiled => if modified(source) > compiled then "changed" else previous),
      notes = note.toList,
    )

  private def sourcesUnder(dir: Path): List[Path] =
    if !Files.isDirectory(dir) then Nil
    else
      try Using.resource(Files.walk(dir))(_.iterator.asScala.filter { p =>
        val name = p.getFileName.toString
        (name.endsWith(".scala") || name.endsWith(".java")) && Files.isRegularFile(p)
      }.map(_.toAbsolutePath.normalize).toList)
      catch case NonFatal(_) => Nil

  private def newestFile(dir: Path): Option[Long] =
    if !Files.isDirectory(dir) then None
    else
      try Using.resource(Files.walk(dir))(_.iterator.asScala.filter(Files.isRegularFile(_)).map(modified).maxOption)
      catch case NonFatal(_) => None

  private def modified(p: Path): Long =
    try Files.getLastModifiedTime(p).toMillis catch case NonFatal(_) => Long.MaxValue

  // ---- symbols ----

  /**
   * Bring the symbol index up to date: compile the root build's main sources (unless
   * sbt is busy, in which case the last compile is used), and point the index at
   * their class directories and classpath. Returns a note for the result, if any.
   */
  private def refreshSymbols(): Option[String] =
    val (modules, modelNote) = model()
    val main = modules.filter(m => m.build == root && m.configuration == "Compile")
    val compileNote =
      if main.isEmpty then None
      else
        try
          server.session { s =>
            val ids = Json.Obj("targets" -> Json.Arr(main.map(m => Json.Obj("uri" -> Json.Str(m.uri)))*))
            // sbt 1 doesn't report a queued request: bound the wait instead.
            s.bspUnlessBusy("buildTarget/compile", ids, maxWaitMillis = 10_000) match
              case None => Some("sbt is busy (or the compile takes longer than 10 s): symbols are as of the last compile")
              case Some(result) =>
                val status = obj(result, "statusCode").collect { case Json.Num(n) => n.intValue }
                Option.when(status.exists(_ != 1))(Failed)
          }
        catch
          case e: InterruptedException => throw e
          case e: RpcError             => Some(Failed)
    val classDirs = main.flatMap(_.classDirectory)
    val entries   = (classDirs ++ main.flatMap(_.classpath)).distinct.filter(Files.exists(_))
    val version   = main.map(_.scalaVersion).filter(_.nonEmpty).maxByOption(versionKey).getOrElse("")
    val stamp     = classDirs.map(d => s"$d\u0000${tastyDigest(d)}")
    val fingerprint = (entries.map(_.toString) ++ stamp).toVector
    SymbolIndexState.update("sbta", entries, fingerprint, version, classDirs.filter(Files.exists(_)))
    if !indexed.contains(fingerprint) then
      prime()
      indexed = Some(fingerprint)
    (modelNote.toList ++ compileNote.toList).reduceOption(_ + "; " + _)

  @volatile private var indexed: Option[Vector[String]] = None

  /**
   * A digest of a class directory's TASTy files. Content, not timestamps: sbt 2 can
   * rewrite unchanged class files (restoring them from its cache), and rebuilding the
   * index takes seconds.
   */
  private def tastyDigest(dir: Path): String =
    if !Files.isDirectory(dir) then "missing"
    else
      try
        Using.resource(Files.walk(dir)) { paths =>
          val crc = java.util.zip.CRC32()
          paths.iterator.asScala.filter(_.getFileName.toString.endsWith(".tasty")).toList.sorted.foreach { p =>
            crc.update(dir.relativize(p).toString.getBytes(StandardCharsets.UTF_8))
            crc.update(Files.readAllBytes(p))
          }
          java.lang.Long.toHexString(crc.getValue)
        }
      catch case NonFatal(_) => "unreadable"

  /**
   * A new index reads the build's classes on its first search, which can take longer
   * than a search's 10 s budget (then it finds nothing). Read them now, until a search
   * fits in the budget (at most three rounds).
   */
  private def prime(): Unit =
    val started = System.nanoTime()
    var rounds  = 0
    var done    = false
    while !done && rounds < 3 do
      val t = System.nanoTime()
      SymbolIndexState.globSearchResult("\u0000", None, 1)
      rounds += 1
      done = System.nanoTime() - t < 8_000_000_000L
    log(s"symbol index ready in ${(System.nanoTime() - started) / 1_000_000} ms ($rounds rounds)")

  private val Failed = "the project does not currently compile — symbols reflect the last successful compile"

  private def versionKey(version: String): (Int, Int, Int) =
    val n = version.split("[.-]").iterator.take(3).map(_.toIntOption.getOrElse(0)).toVector.padTo(3, 0)
    (n(0), n(1), n(2))

  def globSearch(query: String, inPackage: Option[String]): String =
    val wildcard = { val q = query.trim; q.isEmpty || q == "*" }
    if wildcard && inPackage.forall(_.trim.isEmpty) then
      "to list all symbols, provide `inPackage` (e.g. {\"query\":\"*\",\"inPackage\":\"com.example\"})"
    else
      val note = refreshSymbols()
      val body = SymbolIndexState.globSearchResult(query, inPackage.map(_.trim).filter(_.nonEmpty)) match
        case SymbolIndexState.QueryResult.Unavailable(message) => message
        case SymbolIndexState.QueryResult.Available(hits) =>
          if hits.isEmpty then s"no symbols matching '$query'" else hits.map(h => s"${h.kind} ${h.fqn}").mkString("\n")
      withNote(body, note)

  def inspect(symbol: String): String =
    val note = refreshSymbols()
    val body = SymbolIndexState.inspectResult(symbol.trim) match
      case SymbolIndexState.QueryResult.Unavailable(message) => message
      case SymbolIndexState.QueryResult.Available(result)    => result.getOrElse(s"symbol not found: $symbol")
    withNote(body, note)

  def location(symbol: String): String =
    val note = refreshSymbols()
    val body = SymbolIndexState.locationResult(symbol.trim) match
      case SymbolIndexState.QueryResult.Unavailable(message) => message
      case SymbolIndexState.QueryResult.Available(result)    => result.map(relativeLocation).getOrElse(s"no source location for $symbol")
    withNote(body, note)

  /** `path:line` with the path relative to the build root when it is inside it. */
  private def relativeLocation(location: String): String =
    val separator = location.lastIndexOf(':')
    if separator <= 0 then location
    else
      val (file, line) = location.splitAt(separator)
      try
        val p = Path.of(file).toAbsolutePath.normalize
        if p.startsWith(root) then root.relativize(p).toString + line else location
      catch case NonFatal(_) => location

  private def withNote(body: String, note: Option[String]): String = note.fold(body)(n => s"$body\n(note: $n)")
