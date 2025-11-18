package scalus.testing.conformance

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Tag
import scalus.testing.conformance.CardanoLedgerVectorImporter.*
import scalus.testing.conformance.ConformanceTestRunner.*

import java.io.{FileInputStream, FileOutputStream}
import java.nio.file.{Files, Path, Paths}
import scala.util.Using
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream

/** Cardano Ledger Conformance Test Suite
  *
  * Runs conformance tests from cardano-ledger test vectors
  * to validate Scalus ledger implementation against reference implementation.
  */
class CardanoLedgerConformanceTest extends AnyFunSuite {

  // Extract vectors.tar.gz to a temporary directory
  private lazy val conformanceVectorsPath: Path = {
    val vectorsTarGz = getClass.getClassLoader.getResource("vectors.tar.gz")
    if (vectorsTarGz == null) {
      null
    } else {
      val tempDir = Files.createTempDirectory("cardano-ledger-vectors")

      Using.resource(new FileInputStream(Paths.get(vectorsTarGz.toURI).toFile)) { fis =>
        Using.resource(new GzipCompressorInputStream(fis)) { gzis =>
          Using.resource(new TarArchiveInputStream(gzis)) { tis =>
            var entry = tis.getNextTarEntry
            while (entry != null) {
              val outputFile = tempDir.resolve(entry.getName).toFile
              if (entry.isDirectory) {
                outputFile.mkdirs()
              } else {
                outputFile.getParentFile.mkdirs()
                Using.resource(new FileOutputStream(outputFile)) { fos =>
                  val buffer = new Array[Byte](8192)
                  var len = tis.read(buffer)
                  while (len != -1) {
                    fos.write(buffer, 0, len)
                    len = tis.read(buffer)
                  }
                }
              }
              entry = tis.getNextTarEntry
            }
          }
        }
      }

      tempDir.resolve("eras")
    }
  }

  // Skip tests if vectors directory doesn't exist
  val vectorsExist: Boolean = conformanceVectorsPath != null && Files.exists(conformanceVectorsPath)

  if (!vectorsExist) {
    println(
      s"⚠ Conformance test vectors not found at $conformanceVectorsPath - skipping conformance tests"
    )
  }

  val TestTag = Tag("conformance")

  test("load and analyze conformance test vectors", TestTag) {
    assume(vectorsExist, "Conformance test vectors directory not found")

    println(s"\nLoading conformance test vectors from: $conformanceVectorsPath")
    val vectors = loadAllVectors(conformanceVectorsPath)

    println(s"Loaded ${vectors.size} test vectors")
    assert(vectors.nonEmpty, "Should load at least some test vectors")

    val stats = computeStats(vectors)
    println(stats.report)

    // Verify we have a good mix of success and failure cases
    assert(stats.successVectors > 0, "Should have some success test cases")
    assert(stats.failureVectors > 0, "Should have some failure test cases")

    // Verify we have tests across different categories
    assert(stats.categoryCounts.nonEmpty, "Should have test categories")
    println(s"\nFound ${stats.categoryCounts.size} test categories")

    // Verify we have proper tagging
    assert(stats.tagCounts.nonEmpty, "Should have test tags")
    println(s"Found ${stats.tagCounts.size} different tags")
  }

  test("verify CBOR structure of test vectors", TestTag) {
    assume(vectorsExist, "Conformance test vectors directory not found")

    val vectors = loadAllVectors(conformanceVectorsPath)
    assume(vectors.nonEmpty, "No test vectors loaded")

    // Try to hex-decode the first 10 transaction CBOR strings
    var successCount = 0
    var failureCount = 0

    vectors.take(10).foreach { case (path, vector) =>
      try {
        import scalus.testing.conformance.CborParser

        // Parse the transaction CBOR using CborParser
        val transaction = CborParser.parseTransaction(vector.cbor).get

        successCount += 1
        println(s"✓ Successfully parsed transaction for ${path.getFileName}")
        println(s"  Inputs: ${transaction.body.value.inputs.toSeq.size}")
        println(s"  Outputs: ${transaction.body.value.outputs.size}")
        println(s"  Fee: ${transaction.body.value.fee}")
      } catch {
        case e: Exception =>
          failureCount += 1
          println(s"✗ Failed to parse CBOR for ${path.getFileName}: ${e.getMessage}")
      }
    }

    println(
      s"\nCBOR validation: $successCount succeeded, $failureCount failed out of 10 attempts"
    )
    assert(successCount > 0, "Should have valid CBOR hex in test vectors")
  }

  test("run conformance tests - basic structural validation", TestTag) {
    assume(vectorsExist, "Conformance test vectors directory not found")

    // Run a small sample of tests with basic validation
    val results = runConformanceTests(
      vectorsPath = conformanceVectorsPath,
      validators = List.empty, // No validators for basic structural check
      maxTests = Some(50), // Limit to first 50 tests
      filterTags = Set.empty
    )

    println(s"\n=== Basic Conformance Test Results ===")
    println(s"Total:  ${results.totalTests}")
    println(s"Passed: ${results.passedTests}")
    println(s"Failed: ${results.failedTests}")
    println(s"Rate:   ${results.passRate}%")

    // We should be able to at least parse most transactions
    assert(results.totalTests > 0, "Should run some tests")
  }

  test("run conformance tests - filtered by Conway era", TestTag) {
    assume(vectorsExist, "Conformance test vectors directory not found")

    val results = runConformanceTests(
      vectorsPath = conformanceVectorsPath,
      validators = List.empty,
      maxTests = Some(20),
      filterTags = Set("conway")
    )

    println(s"\n=== Conway Era Tests ===")
    println(s"Tests run: ${results.totalTests}")
    assert(results.totalTests > 0, "Should find Conway era tests")
  }

  test("run conformance tests - filtered by Plutus", TestTag) {
    assume(vectorsExist, "Conformance test vectors directory not found")

    val results = runConformanceTests(
      vectorsPath = conformanceVectorsPath,
      validators = List.empty,
      maxTests = Some(20),
      filterTags = Set("plutus")
    )

    println(s"\n=== Plutus Tests ===")
    println(s"Tests run: ${results.totalTests}")
    // May or may not find Plutus tests depending on vectors
  }

  test("run conformance tests with validators - placeholder", TestTag) {
    assume(vectorsExist, "Conformance test vectors directory not found")

    // TODO: Add proper context creation once we have proper ProtocolParams conversion
    // For now, just run basic tests
    val results = runConformanceTests(
      vectorsPath = conformanceVectorsPath,
      validators = List.empty,
      maxTests = Some(10)
    )

    println(s"\n=== Placeholder Validator Tests ===")
    println(s"Total:  ${results.totalTests}")
    println(s"Passed: ${results.passedTests}")

    assert(results.totalTests > 0, "Should run some tests")
  }

  test("export conformance test statistics", TestTag) {
    assume(vectorsExist, "Conformance test vectors directory not found")

    val vectors = loadAllVectors(conformanceVectorsPath)
    val stats = computeStats(vectors)

    // Verify comprehensive statistics
    println("\n=== Statistics Export ===")
    println(s"Total vectors: ${stats.totalVectors}")
    println(s"Success: ${stats.successVectors}")
    println(s"Failure: ${stats.failureVectors}")
    println(s"Categories: ${stats.categoryCounts.size}")
    println(s"Tags: ${stats.tagCounts.size}")

    // Verify we captured key era tags
    val eras = Set("conway", "babbage", "alonzo", "allegra")
    val foundEras = eras.intersect(stats.tagCounts.keySet)
    println(s"Eras found: ${foundEras.mkString(", ")}")

    // Verify we captured feature tags
    val features = Set("plutus", "metadata", "collateral", "staking", "governance")
    val foundFeatures = features.intersect(stats.tagCounts.keySet)
    println(s"Features found: ${foundFeatures.mkString(", ")}")

    assert(stats.totalVectors > 0, "Should have vectors")
    assert(foundEras.nonEmpty, "Should find at least one era tag")
  }
}
