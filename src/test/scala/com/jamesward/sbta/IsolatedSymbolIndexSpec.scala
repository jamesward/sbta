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
          val hits              = session.globSearch("Probe", Some("isolatedfixture"), 100)
          val limitedHits       = session.globSearch("*", Some("isolatedfixture"), 1)
          val dependencyHits    = session.globSearch("Option", Some("scala"), 100)
          val inspect           = session.inspect("isolatedfixture.Probe")
          val dependencyInspect = session.inspect("scala.Option")
          val dependencyLocation = session.location("scala.Option")
          assertTrue(
            session.isIsolated,
            session.readerVersion.startsWith("1.10"), // the reader for this spec's Scala (3.10)
            session.runtimeScalaVersion == scala.util.Properties.versionNumberString,
            hits.exists(_.fqn.contains("isolatedfixture.Probe")),
            limitedHits.size <= 1,
            dependencyHits.isEmpty,
            inspect.exists(_.contains("ping")),
            dependencyInspect.exists(_.contains("Option")),
            dependencyLocation.nonEmpty,
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
