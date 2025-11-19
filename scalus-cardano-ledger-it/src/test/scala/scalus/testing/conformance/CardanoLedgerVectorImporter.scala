package scalus.testing.conformance

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** Importer for Cardano Ledger conformance test vectors
  *
  * Supports importing test vectors from the cardano-ledger dump format
  * as described in https://github.com/SundaeSwap-finance/cardano-ledger-conformance-tests
  */
object CardanoLedgerVectorImporter {

  /** Raw test vector from cardano-ledger dump */
  case class RawTestVector(
      /** Transaction CBOR (hex-encoded) */
      cbor: String,
      /** Old (before) ledger state CBOR (hex-encoded) */
      oldLedgerState: String,
      /** New (after) ledger state CBOR (hex-encoded) - only present if success=true */
      newLedgerState: Option[String] = None,
      /** Whether the transaction should succeed */
      success: Boolean,
      /** Test category/name */
      testState: String
  )

  given JsonValueCodec[RawTestVector] = JsonCodecMaker.make

  /** Load a single test vector from a file */
  def loadRawVector(path: Path): Try[RawTestVector] = Try {
    val json = Files.readString(path)
    readFromString[RawTestVector](json)
  }

  /** Load all test vectors from a directory tree */
  def loadAllVectors(rootPath: Path): List[(Path, RawTestVector)] = {
    if (!Files.exists(rootPath)) {
      println(s"Warning: Test vector directory not found: $rootPath")
      return List.empty
    }

    Files
      .walk(rootPath)
      .iterator()
      .asScala
      .filter(p => Files.isRegularFile(p) && !p.getFileName.toString.startsWith("."))
      .filter(p => !p.toString.contains("pparams-by-hash")) // Skip protocol params files
      .filter(p => !p.getFileName.toString.endsWith(".md")) // Skip markdown files
      .filter(p => !p.getFileName.toString.endsWith(".tar.gz")) // Skip tar.gz files
      .flatMap { path =>
        loadRawVector(path) match {
          case scala.util.Success(vector) => Some((path, vector))
          case scala.util.Failure(e) =>
            // Skip files that aren't valid JSON vectors
            None
        }
      }
      .toList
  }

  /** Convert raw vector to TransactionTestCase
    *
    * Note: This is a simplified conversion. Full conversion would require
    * parsing the CBOR ledger state to extract all details.
    */
  def toTransactionTestCase(
      id: String,
      vector: RawTestVector,
      genesis: ConformanceTestSchema.GenesisConfig
  ): ConformanceTestSchema.TransactionTestCase = {
    import ConformanceTestSchema.*

    // Extract test category and description from testState
    val parts = vector.testState.split('.')
    val category = parts.dropRight(1).mkString(".")
    val description = parts.lastOption.getOrElse(vector.testState)

    // Determine tags from test category
    val tags = extractTags(vector.testState)

    // Create minimal initial state
    // In a full implementation, this would parse oldLedgerState CBOR
    val initialState = InitialLedgerState(
      utxos = List.empty // Would be parsed from oldLedgerState
    )

    // Create expected state
    val expectedState = ExpectedLedgerState(
      utxos = None, // Would be parsed from newLedgerState if present
      utxoChanges = None,
      expectedErrors = if (!vector.success) {
        Some(List(ExpectedError(errorType = "validation_failure")))
      } else None
    )

    TransactionTestCase(
      id = id,
      description = description,
      tags = tags,
      genesis = genesis,
      initialState = initialState,
      transaction = vector.cbor,
      expectedState = expectedState,
      shouldSucceed = vector.success,
      context = None
    )
  }

  /** Extract tags from test state name */
  def extractTags(testState: String): List[String] = {
    val tags = scala.collection.mutable.ListBuffer[String]()

    // Add era tags
    if (testState.contains("Conway")) tags += "conway"
    if (testState.contains("Babbage")) tags += "babbage"
    if (testState.contains("Alonzo")) tags += "alonzo"
    if (testState.contains("Allegra")) tags += "allegra"

    // Add feature tags
    if (testState.contains("Plutus")) {
      tags += "plutus"
      if (testState.contains("PlutusV1")) tags += "plutus-v1"
      if (testState.contains("PlutusV2")) tags += "plutus-v2"
      if (testState.contains("PlutusV3")) tags += "plutus-v3"
    }

    if (testState.contains("Metadata")) tags += "metadata"
    if (testState.contains("Collateral")) tags += "collateral"
    if (testState.contains("ExUnits") || testState.contains("execution units"))
      tags += "ex-units"
    if (testState.contains("UTXO")) tags += "utxo"
    if (testState.contains("UTXOW")) tags += "utxow"
    if (testState.contains("UTXOS")) tags += "utxos"
    if (testState.contains("Stake")) tags += "staking"
    if (testState.contains("Pool")) tags += "pools"
    if (testState.contains("DRep") || testState.contains("Gov"))
      tags += "governance"
    if (testState.contains("Treasury")) tags += "treasury"
    if (testState.contains("Withdrawal")) tags += "withdrawals"
    if (testState.contains("Certificate")) tags += "certificates"

    // Add success/failure tag
    tags += (if (testState.toLowerCase.contains("invalid") ||
                   testState.toLowerCase.contains("insufficient") ||
                   testState.toLowerCase.contains("too many") ||
                   testState.toLowerCase.contains("fails"))
              "negative-test"
            else "positive-test")

    tags.toList.distinct
  }

  /** Statistics about loaded test vectors */
  case class VectorStats(
      totalVectors: Int,
      successVectors: Int,
      failureVectors: Int,
      categoryCounts: Map[String, Int],
      tagCounts: Map[String, Int]
  ) {
    def report: String = {
      val sb = new StringBuilder
      sb.append(s"\n=== Test Vector Statistics ===\n")
      sb.append(s"Total vectors: $totalVectors\n")
      sb.append(s"Success vectors: $successVectors\n")
      sb.append(s"Failure vectors: $failureVectors\n")
      sb.append(s"\n=== Top Categories ===\n")
      categoryCounts.toSeq.sortBy(-_._2).take(10).foreach { case (cat, count) =>
        sb.append(f"  $cat%-60s: $count%5d\n")
      }
      sb.append(s"\n=== Tag Distribution ===\n")
      tagCounts.toSeq.sortBy(-_._2).foreach { case (tag, count) =>
        sb.append(f"  $tag%-20s: $count%5d\n")
      }
      sb.toString
    }
  }

  /** Compute statistics for loaded vectors */
  def computeStats(vectors: List[(Path, RawTestVector)]): VectorStats = {
    val categories = scala.collection.mutable.Map[String, Int]()
    val tags = scala.collection.mutable.Map[String, Int]()
    var successCount = 0
    var failureCount = 0

    vectors.foreach { case (_, vector) =>
      // Count success/failure
      if (vector.success) successCount += 1
      else failureCount += 1

      // Count categories
      val category = vector.testState.split('.').dropRight(1).mkString(".")
      categories(category) = categories.getOrElse(category, 0) + 1

      // Count tags
      extractTags(vector.testState).foreach { tag =>
        tags(tag) = tags.getOrElse(tag, 0) + 1
      }
    }

    VectorStats(
      totalVectors = vectors.size,
      successVectors = successCount,
      failureVectors = failureCount,
      categoryCounts = categories.toMap,
      tagCounts = tags.toMap
    )
  }

  /** Example: Load and analyze test vectors */
  def analyzeVectors(vectorsPath: Path): Unit = {
    println(s"Loading test vectors from: $vectorsPath")
    val vectors = loadAllVectors(vectorsPath)
    val stats = computeStats(vectors)
    println(stats.report)
  }
}
