// sbta: an sbt launcher. `sbta <args>` is sbt (its own launcher, embedded, runs in sbta's
// JVM); `sbta --mcp` is an MCP stdio server for the build, a client of sbt's own server
// (sbt server protocol + BSP on its user-only socket), for sbt 1.4+ and 2.x builds.
// Shipped as one runnable jar (assembly, shrunk by ProGuard), JDK 17+, and the `sbta` /
// `sbta.bat` scripts (sbt's runner scripts, downloading this jar).

ThisBuild / organization := "com.jamesward"
ThisBuild / homepage     := Some(uri("https://github.com/jamesward/sbta"))
ThisBuild / licenses     := List("Apache-2.0" -> uri("https://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / developers   := List(Developer("jamesward", "James Ward", "james@jamesward.com", uri("https://jamesward.com")))
ThisBuild / versionScheme := Some("semver-spec")

val zioVersion = "2.1.27"

// The sbt launcher sbta runs in-process for everything but `--mcp`, embedded as a jar
// (it bundles its own Scala 2.12 classes, so it can't be merged into sbta's). The
// runner scripts in src/scripts are this release's (see src/scripts/update.sh).
val SbtLaunchVersion = "2.0.11"
lazy val SbtLaunch    = config("sbt-launch").hide
lazy val Proguard     = config("proguard").hide
lazy val sbtaShrink   = taskKey[File]("The sbta jar: the assembly, shrunk by ProGuard (proguard.pro).")
lazy val sbtaDist     = taskKey[Seq[File]]("The release files (jar, scripts, SHA256SUMS) in target/sbta-dist.")
lazy val sbtaInstall  = taskKey[Seq[File]]("Build sbta and write this build's own ./sbta and ./sbta.bat, which run it.")
lazy val sbtaDevClasspath = taskKey[File]("Compile sbta and write its runtime classpath (target/sbta-dev/classpath), for ./sbta --mcp without a built jar.")
lazy val sbtaLocalTestScripts = taskKey[Seq[File]]("Local-mode scripts for the launcher specs.")
lazy val sbtaInstallExpected  = taskKey[Seq[File]]("What sbtaInstall writes, to check the committed scripts against.")

// ---- isolated code-tool runtimes, embedded as resources and loaded in their own classloaders ----

// The `check` compiler bridge: compiled once against the OLDEST supported compiler
// (Scala 3.3.1, Provided: never embedded) and linked at runtime against the build's own
// scala3-compiler jars, in an isolated classloader.
lazy val checkRuntime = (project in file("check-runtime"))
  .settings(
    publish / skip := true,
    scalaVersion := "3.3.1",
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % Provided,
  )

// The TASTy symbol index, on TASTy Query 1.8 (for Scala 3.3–3.8 builds). TASTy Query
// follows the TASTy minor version and each release is compiled with that Scala, so the
// 3.9 and 3.10 readers are separate jars (below), loaded with the build's own Scala library.
lazy val symbolRuntime = (project in file("symbol-runtime"))
  .settings(
    publish / skip := true,
    scalaVersion := "3.8.4",
    libraryDependencies ++= Seq(
      "ch.epfl.scala" %% "tasty-query"  % "1.8.0",
      "dev.zio"       %% "zio-test"     % zioVersion % Test,
      "dev.zio"       %% "zio-test-sbt" % zioVersion % Test,
    ),
  )

lazy val reader39 = (project in file("reader-3.9"))
  .settings(
    publish / skip := true,
    scalaVersion := "3.9.0",
    libraryDependencies := Seq("ch.epfl.scala" %% "tasty-query" % "1.9.0"),
  )

lazy val reader310 = (project in file("reader-3.10"))
  .settings(
    publish / skip := true,
    scalaVersion := "3.10.0",
    libraryDependencies := Seq("ch.epfl.scala" %% "tasty-query" % "1.10.0"),
  )

/** The code-tool runtimes as resources: the `check` bridge, the symbol runtime, and every TASTy Query reader. */
def codeToolAssets: Def.Initialize[Task[Seq[File]]] = Def.task {
  val converter = fileConverter.value
  val out       = (Compile / resourceManaged).value
  def readerJar(report: UpdateReport, version: String): File =
    report.matching(artifactFilter(name = "tasty-query_3", extension = "jar")).headOption
      .getOrElse(sys.error(s"tasty-query $version was not resolved"))
  val checkJar  = converter.toPath((checkRuntime / Compile / packageBin).value).toFile
  val symbolJar = converter.toPath((symbolRuntime / Compile / packageBin).value).toFile
  val reader18 = (symbolRuntime / Runtime / dependencyClasspath).value
    .map(entry => converter.toPath(entry.data).toFile)
    .find(_.getName.startsWith("tasty-query_3-1.8.0"))
    .getOrElse(sys.error("tasty-query 1.8.0 was not resolved"))
  val files = Seq(
    checkJar                                         -> out / "sbta-check" / "check-runtime.jar",
    symbolJar                                        -> out / "sbta-symbol" / "symbol-runtime.jar",
    reader18                                         -> out / "sbta-symbol" / "tasty-query-1.8.0.jar",
    readerJar((reader39 / Compile / update).value, "1.9.0")   -> out / "sbta-readers" / "tasty-query-1.9.0.jar",
    readerJar((reader310 / Compile / update).value, "1.10.0") -> out / "sbta-readers" / "tasty-query-1.10.0.jar",
  )
  files.foreach { case (source, target) => IO.copyFile(source, target) }
  files.map(_._2)
}

// ---- sbta ----

ThisBuild / dynverSeparator := "-"

lazy val sbta = (project in file("."))
  .settings(
    name := "sbta",
    description := "sbta: an sbt launcher whose --mcp serves MCP over stdio as a client of the build's sbt server",
    scalaVersion := "3.10.0",
    scalacOptions ++= Seq("-release", "17", "-Werror"),
    javacOptions ++= Seq("-source", "17", "-target", "17"),
    libraryDependencies ++= Seq(
      // zio-http pulls in netty-pkitesting (test-certificate generation) and with it
      // Bouncy Castle (~11 MB): sbta never serves TLS, so leave them out of the jar.
      ("com.jamesward" %% "zio-http-mcp" % "0.10.0")
        .exclude("io.netty", "netty-pkitesting")
        .exclude("org.bouncycastle", "bcprov-jdk18on")
        .exclude("org.bouncycastle", "bcpkix-jdk18on")
        .exclude("org.bouncycastle", "bcutil-jdk18on"),
      "dev.zio"       %% "zio-test"     % zioVersion % Test,
      "dev.zio"       %% "zio-test-sbt" % zioVersion % Test,
      "org.ow2.asm"    % "asm"          % "9.11"     % Test,
      ("org.scala-sbt" % "sbt-launch"   % SbtLaunchVersion % SbtLaunch).intransitive(),
      "com.guardsquare" % "proguard-base" % "7.10.0" % Proguard,
      "com.jamesward" % "skills" % "0.0.17" % Skills,
    ),
    ivyConfigurations ++= Seq(SbtLaunch, Proguard),
    crossPaths := false,
    Compile / resourceGenerators += codeToolAssets.taskValue,
    Compile / resourceGenerators += Def.task {
      val jar = update.value.select(configurationFilter(SbtLaunch.name), moduleFilter(), artifactFilter(name = "sbt-launch", extension = "jar"))
        .headOption.getOrElse(sys.error(s"sbt-launch $SbtLaunchVersion was not resolved"))
      val out = (Compile / resourceManaged).value / "sbta-launcher"
      IO.copyFile(jar, out / "sbt-launch.jar")
      IO.write(out / "version", SbtLaunchVersion)
      Seq(out / "sbt-launch.jar", out / "version")
    }.taskValue,
    Compile / sourceGenerators += Def.task {
      val output = (Compile / sourceManaged).value / "com" / "jamesward" / "sbta" / "SbtaBuildInfo.scala"
      val escapedVersion = version.value.replace("\\", "\\\\").replace("\"", "\\\"")
      IO.write(
        output,
        s"""package com.jamesward.sbta
           |
           |object SbtaBuildInfo:
           |  final val version: String = "$escapedVersion"
           |""".stripMargin,
      )
      Seq(output)
    }.taskValue,
    Compile / mainClass := Some("com.jamesward.sbta.Main"),
    // Specs and fixtures have main methods too.
    Test / mainClass := None,

    // ---- the jar ----
    assembly / mainClass := Some("com.jamesward.sbta.Main"),
    assembly / assemblyJarName := "sbta-assembly.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "versions", _, "module-info.class") => MergeStrategy.discard
      case PathList("META-INF", "versions", _, "OSGI-INF", _*)      => MergeStrategy.discard
      case "module-info.class"                                       => MergeStrategy.discard
      case PathList("META-INF", "io.netty.versions.properties")       => MergeStrategy.first
      case PathList("META-INF", "native-image", _*)                  => MergeStrategy.discard
      case x =>
        val previous = (assembly / assemblyMergeStrategy).value
        previous(x)
    },
    // Scala 3.10's scala-library ships `scala.annotation.unroll` itself.
    assembly / assemblyExcludedJars := {
      val converter = fileConverter.value
      (assembly / fullClasspath).value.filter(entry => converter.toPath(entry.data).getFileName.toString.startsWith("unroll-annotation_"))
    },
    sbtaShrink := Def.uncached {
      val converter = fileConverter.value
      val jdk = SbtaShrink.jdkJar(target.value / "proguard" / s"jdk-${sys.props("java.version")}.jar")
      SbtaShrink.run(
        update.value.select(configurationFilter(Proguard.name)),
        baseDirectory.value / "proguard.pro",
        converter.toPath(assembly.value).toFile,
        target.value / "proguard" / "sbta.jar",
        jdk,
        file(sys.props("java.home")),
        streams.value.log,
      )
    },

    // Released as GitHub release files only (sbtaDist), not to a Maven repository.
    publish / skip := true,

    // ---- scripts and release files ----
    Compile / SbtaScripts.sbtaScripts := Def.uncached(SbtaScripts.generate(
      baseDirectory.value / "src" / "scripts",
      (Compile / target).value / "sbta-scripts",
      SbtaScripts.Release(version.value),
      SbtLaunchVersion,
    )),
    // This build's own ./sbta and ./sbta.bat: they run the jar built here (copied to
    // target/sbta-local/sbta.jar), or, until it's built, sbt's launcher.
    // Real paths (sbt 2's `export` prints virtual ones), joined for `java -cp`.
    sbtaDevClasspath := Def.uncached {
      val converter = fileConverter.value
      val paths     = (Runtime / fullClasspath).value.map(a => converter.toPath(a.data).toAbsolutePath.toString)
      val out       = baseDirectory.value / SbtaScripts.DevClasspath
      IO.write(out, paths.mkString(java.io.File.pathSeparator))
      out
    },
    sbtaInstall := Def.uncached {
      val base = baseDirectory.value
      val jar  = base / "target" / "sbta-local" / "sbta.jar"
      IO.createDirectory(jar.getParentFile)
      // A new file moved into place: a running sbta keeps the jar it started with.
      val temp = java.io.File.createTempFile("sbta", ".jar.tmp", jar.getParentFile)
      IO.copyFile(sbtaShrink.value, temp)
      java.nio.file.Files.move(temp.toPath, jar.toPath, java.nio.file.StandardCopyOption.REPLACE_EXISTING, java.nio.file.StandardCopyOption.ATOMIC_MOVE)
      val relative = "target/sbta-local/sbta.jar"
      val scripts  = SbtaScripts.generate(base / "src" / "scripts", base, SbtaScripts.Local(relative), SbtLaunchVersion)
      streams.value.log.info(s"sbta: wrote ${scripts.map(_.getName).mkString(" and ")}, running $relative")
      scripts
    },
    // What `sbtaInstall` would write, for the spec that checks the committed ./sbta and ./sbta.bat.
    sbtaInstallExpected := Def.uncached(SbtaScripts.generate(
      baseDirectory.value / "src" / "scripts",
      target.value / "sbta-install-expected",
      SbtaScripts.Local("target/sbta-local/sbta.jar"),
      SbtLaunchVersion,
    )),
    // The same local-mode scripts, for LauncherSpec (pointing at a jar it places itself).
    sbtaLocalTestScripts := Def.uncached(SbtaScripts.generate(
      baseDirectory.value / "src" / "scripts",
      target.value / "sbta-local-test-scripts",
      SbtaScripts.Local("sbta-local.jar"),
      SbtLaunchVersion,
    )),
    sbtaDist := Def.uncached {
      val out = target.value / "sbta-dist"
      IO.delete(out)
      val jar = out / s"sbta-${version.value}.jar"
      IO.copyFile(sbtaShrink.value, jar)
      val scripts = (Compile / SbtaScripts.sbtaScripts).value.map { f =>
        val copy = out / f.getName
        IO.copyFile(f, copy)
        copy.setExecutable(f.canExecute, false)
        copy
      }
      val files = jar +: scripts
      def digest(algorithm: String, f: File) =
        java.security.MessageDigest.getInstance(algorithm).digest(IO.readBytes(f)).map(b => f"$b%02x").mkString
      // The scripts check the jar they download against this (as sbt's do for sbt-launch).
      val sha1 = out / s"${jar.getName}.sha1"
      IO.write(sha1, digest("SHA-1", jar))
      val sumsFile = out / "SHA256SUMS"
      IO.writeLines(sumsFile, files.map(f => s"${digest("SHA-256", f)}  ${f.getName}"))
      files :+ sha1 :+ sumsFile
    },

    // ---- tests: the launcher specs run the shrunk jar and the generated scripts ----
    Test / fork := true,
    Test / javaOptions ++= Seq(
      s"-Dsbta.jar=${sbtaShrink.value}",
      s"-Dsbta.assembly=${fileConverter.value.toPath(assembly.value)}",
      s"-Dsbta.reflection=${baseDirectory.value / "reflection.txt"}",
      s"-Dsbta.scripts=${(Compile / SbtaScripts.sbtaScripts).value.head.getParentFile}",
      s"-Dsbta.localScripts=${sbtaLocalTestScripts.value.head.getParentFile}",
      s"-Dsbta.installExpected=${sbtaInstallExpected.value.head.getParentFile}",
      s"-Dsbta.base=${baseDirectory.value}",
      s"-Dsbta.version=${version.value}",
    ),

    // SkillsJars: extract agent Skills with `./sbt extractSkillsJars`
    skillsJarsOutputDir := Some(file(".kiro/skills")),
  )
  .aggregate(symbolRuntime)
