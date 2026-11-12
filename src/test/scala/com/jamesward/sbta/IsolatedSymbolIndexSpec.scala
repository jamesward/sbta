package com.jamesward.sbta

import java.nio.file.{ Path, Paths }

import zio.*
import zio.test.*

object IsolatedSymbolIndexSpec extends ZIOSpecDefault:

  private def codePath(clazz: Class[?]): Path =
    Paths.get(clazz.getProtectionDomain.getCodeSource.getLocation.toURI)

  def spec = suite("IsolatedSymbolIndexSpec")(
    test("loads the bridge and TASTy Query outside sbt's Scala classloader") {
      ZIO.attempt {
        val entries = List(
          codePath(classOf[isolatedfixture.Probe]),
          codePath(classOf[scala.Option[?]]),
          codePath(classOf[scala.deriving.Mirror]),
        ).distinct
        val searchEntries = List(codePath(classOf[isolatedfixture.Probe]))
        val session = IsolatedSymbolIndex.open(entries, searchEntries, scala.util.Properties.versionNumberString)
        try
          // A cold search reads the classpath first and has a 10 s budget, which a loaded
          // machine (the launcher specs run alongside) can exceed: warm the index up.
          Iterator.range(0, 3).map(_ => session.globSearch("Option", Some("scala"), 1)).find(_.nonEmpty)
          val hits              = session.globSearch("Probe", Some("isolatedfixture"), 100)
          val limitedHits       = session.globSearch("*", Some("isolatedfixture"), 1)
          val dependencyHits    = session.globSearch("Option", Some("scala"), 100)
          val inspect           = session.inspect("isolatedfixture.Probe")
          val dependencyInspect = session.inspect("scala.Option")
          val dependencyLocation = session.location("scala.Option")
          val opaqueType        = session.inspect("isolatedfixture.Registry.Version")
          val alias             = session.inspect("isolatedfixture.Registry.Id")
          val nested            = session.inspect("isolatedfixture.Registry.Nested")
          val term              = session.inspect("isolatedfixture.Registry.default")
          val opaqueLocation    = session.location("isolatedfixture.Registry.Version")
          val probe             = session.inspect("isolatedfixture.Probe")
          val codec             = session.inspect("isolatedfixture.Codec")
          val codecCompanion    = session.inspect("isolatedfixture.Codec$")
          val pkg               = session.inspect("isolatedfixture")
          val pkgObject         = session.inspect("isolatedfixture.package")
          // The package isn't in this build's own entries (only Probe's directory is): a dependency's.
          val dependencyPackage = session.globSearch("*", Some("scala.deriving"), 100)
          val globbed           = session.globSearch("*Cod?c*", Some("isolatedfixture"), 100)
          val anchored          = session.globSearch("Prob*", Some("isolatedfixture"), 100)
          assertTrue(
            session.isIsolated,
            session.readerVersion.startsWith("1.10"), // the reader for this spec's Scala (3.10)
            session.runtimeScalaVersion == scala.util.Properties.versionNumberString,
            hits.exists(_.fqn.contains("isolatedfixture.Probe")),
            limitedHits.size <= 1,
            // Nothing of the build's own in `scala`: its symbols come from the classpath.
            dependencyHits.exists(_.fqn == "scala.Option"),
            dependencyPackage.exists(_.fqn == "scala.deriving.Mirror"),
            inspect.exists(_.contains("ping")),
            dependencyInspect.exists(_.contains("Option")),
            dependencyLocation.nonEmpty,
            // An opaque type in an object, with its companion's members.
            opaqueType.exists(_.startsWith("opaque type Version — isolatedfixture.Registry.Version")),
            opaqueType.exists(_.contains("companion object Version:")),
            opaqueType.exists(_.contains("def parse(s: String): Version")),
            opaqueType.exists(_.contains("extension (v: Version) def value: String")),
            alias.exists(_.startsWith("type Id = Long")),
            nested.exists(_.contains("def answer: Int")),
            term.exists(_.startsWith("val default: Version")),
            // Scala signatures, not TASTy's types.
            probe.exists(_.contains("def ping: String")),
            probe.exists(_.contains("def each[A](f: A => Unit)(using codec: Codec[A]): (A, Int)")),
            // A trait with its companion's givens, and the companion alone as `Codec$`.
            codec.exists(_.startsWith("trait Codec — isolatedfixture.Codec")),
            codec.exists(_.contains("def read(s: String): A")),
            codec.exists(_.contains("companion object Codec:")),
            codecCompanion.exists(_.startsWith("object Codec — isolatedfixture.Codec")),
            codecCompanion.exists(_.contains("given intCodec: Codec[Int]")),
            codecCompanion.exists(_.contains("given listCodec[A](using a: Codec[A]): Codec[List[A]]")),
            !codecCompanion.exists(_.contains("def read")),
            // A package: its types, then its top-level definitions.
            pkg.exists(_.startsWith("package isolatedfixture")),
            pkg.exists(_.contains("  class Probe")),
            pkg.exists(_.contains("  object Registry")),
            pkg.exists(_.contains("  trait Codec")),
            pkg.exists(_.contains("def greet(name: String): String")),
            pkg.exists(_.contains("given defaultProbe: Probe")),
            pkgObject == pkg,
            // A glob matches whole names (ignoring an object's `$`); a plain query, any containing it.
            globbed.exists(_.fqn == "isolatedfixture.Codec"),
            globbed.exists(_.fqn == "isolatedfixture.Codec$.intCodec"),
            !globbed.exists(_.fqn.endsWith(".read")),
            anchored.exists(_.fqn == "isolatedfixture.Probe"),
            !anchored.exists(_.fqn == "isolatedfixture.defaultProbe"),
            opaqueLocation.exists(_.contains("Probe.scala:")),
          )
        finally session.close()
      }
    },
    test("reader failures remain distinct from symbol misses") {
      ZIO.attempt {
        val entries = List(codePath(classOf[isolatedfixture.Probe]))
        SymbolIndexState.update("unsupported", entries, Vector("unsupported"), "3.11.0")
        val result = try SymbolIndexState.inspectResult("isolatedfixture.Probe")
        finally SymbolIndexState.shutdown()
        assertTrue(result match {
          case SymbolIndexState.QueryResult.Unavailable(message) =>
            message.contains("No embedded TASTy reader supports Scala 3.11")
          case _ => false
        })
      }
    },
    test("shutdown clears process-global isolated sessions") {
      ZIO.attempt {
        SymbolIndexState.shutdown()
        assertTrue(!SymbolIndexState.isReady, SymbolIndexState.activeReaderVersion.isEmpty)
      }
    },
  ) @@ TestAspect.sequential
end IsolatedSymbolIndexSpec
