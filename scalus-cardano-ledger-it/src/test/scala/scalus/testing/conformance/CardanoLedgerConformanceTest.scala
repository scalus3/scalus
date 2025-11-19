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
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.cardano.address.Address
import scalus.utils.Hex

/** Helper object for converting parsed ledger state to Scalus types */
object LedgerStateConverter {
    import ConformanceTestSchema.*

    /** Convert parsed UTxO entries to Scalus Utxos map */
    def convertUtxos(entries: List[UtxoEntry]): Map[TransactionInput, TransactionOutput] = {
        import scalus.builtin.ByteString

        entries.flatMap { entry =>
            try {
                val txHash = TransactionHash.fromHex(entry.txHash)
                val txInput = TransactionInput(txHash, entry.outputIndex)

                // Parse address
                val addressBytes = ByteString.fromHex(entry.address)
                val address = Address.fromByteString(addressBytes)

                // Parse value (coin + assets)
                val coin = Coin(entry.value.toLong)
                val value = Value(coin, MultiAsset.empty) // TODO: parse assets from entry.assets

                // Parse datum option
                val datumOption = entry.datum.flatMap { d =>
                    // TODO: parse datum from d.datum hex
                    None
                }

                // Create output
                val txOutput = TransactionOutput(address, value, datumOption, None)
                Some((txInput, txOutput))
            } catch {
                case _: Exception => None
            }
        }.toMap
    }

    /** Convert parsed CertState data to Scalus CertState */
    def convertCertState(
        dreps: Option[List[DRepState]],
        stakePools: Option[List[StakePoolState]],
        accounts: Option[List[AccountState]]
    ): CertState = {
        // Convert DReps
        val votingState = VotingState(
          dreps = dreps.map { drepList =>
              drepList.flatMap { drep =>
                  drep.drepId.flatMap { id =>
                      try {
                          import io.bullet.borer.Cbor
                          val credBytes = Hex.hexToBytes(id)
                          val cred = Cbor.decode(credBytes).to[Credential].value
                          val drepState = scalus.cardano.ledger.DRepState(
                            expiry = drep.expiryEpoch.getOrElse(0).toLong,
                            anchor = None, // TODO: create Anchor type from drep.anchor
                            deposit = Coin(drep.deposit.getOrElse("0").toLong),
                            delegates = Set.empty
                          )
                          Some((cred, drepState))
                      } catch {
                          case _: Exception => None
                      }
                  }
              }.toMap
          }.getOrElse(Map.empty)
        )

        // Pool state (simplified for now)
        val poolsState = PoolsState()

        // Convert delegation state from accounts
        val delegationState = DelegationState(
          rewards = accounts.map { accountList =>
              accountList.flatMap { account =>
                  try {
                      import io.bullet.borer.Cbor
                      val credBytes = Hex.hexToBytes(account.stakeAddress)
                      val cred = Cbor.decode(credBytes).to[Credential].value
                      val reward = Coin(account.rewards.getOrElse("0").toLong)
                      Some((cred, reward))
                  } catch {
                      case _: Exception => None
                  }
              }.toMap
          }.getOrElse(Map.empty),
          deposits = Map.empty,
          stakePools = Map.empty,
          dreps = Map.empty
        )

        CertState(votingState, poolsState, delegationState)
    }
}

/** Cardano Ledger Conformance Test Suite
  *
  * Runs conformance tests from cardano-ledger test vectors to validate Scalus ledger implementation
  * against reference implementation.
  */
class CardanoLedgerConformanceTest extends AnyFunSuite {

    // Extract vectors.tar.gz to a temporary directory
    private lazy val conformanceVectorsPath: Path = {
        val vectorsTarGz = getClass.getClassLoader.getResource("vectors.tar.gz")
        if vectorsTarGz == null then {
            null
        } else {
            val tempDir = Files.createTempDirectory("cardano-ledger-vectors")

            Using.resource(new FileInputStream(Paths.get(vectorsTarGz.toURI).toFile)) { fis =>
                Using.resource(new GzipCompressorInputStream(fis)) { gzis =>
                    Using.resource(new TarArchiveInputStream(gzis)) { tis =>
                        var entry = tis.getNextTarEntry
                        while entry != null do {
                            val outputFile = tempDir.resolve(entry.getName).toFile
                            if entry.isDirectory then {
                                outputFile.mkdirs()
                            } else {
                                outputFile.getParentFile.mkdirs()
                                Using.resource(new FileOutputStream(outputFile)) { fos =>
                                    val buffer = new Array[Byte](8192)
                                    var len = tis.read(buffer)
                                    while len != -1 do {
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
    val vectorsExist: Boolean =
        conformanceVectorsPath != null && Files.exists(conformanceVectorsPath)

    if !vectorsExist then {
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

    test("MetadataValidator Conway.Imp.AllegraImpSpec.UTXOW.InvalidMetadata", TestTag) {
        assume(vectorsExist, "Conformance test vectors directory not found")
        val vectorName = "Conway.Imp.AllegraImpSpec.UTXOW.InvalidMetadata"
        val vectorPath = conformanceVectorsPath.resolve("conway/impl/dump").resolve(vectorName)
        for case (path, vector) <- loadAllVectors(vectorPath) do {
            val transaction = Transaction.fromCbor(Hex.hexToBytes(vector.cbor))
            val ledgerState = CborParser.parseLedgerState(vector.oldLedgerState).get
            println(pprint(ledgerState))

            // Convert parsed ledger state to Context and State
            val utxos = LedgerStateConverter.convertUtxos(ledgerState.utxos)
            val certState = LedgerStateConverter.convertCertState(
              ledgerState.dreps,
              ledgerState.stakePools,
              ledgerState.accounts
            )

            val state = State(
              utxos = utxos,
              certState = certState,
              deposited = Coin(ledgerState.deposited.getOrElse("0").toLong),
              fees = Coin(ledgerState.fees.getOrElse("0").toLong),
              govState = (),
              stakeDistribution = Map.empty,
              donation = ledgerState.donation.map(d => Coin(d.toLong)).getOrElse(Coin.zero)
            )
            val context = Context()

            val validation = MetadataValidator.validate(context, state, transaction)
            println(pprint(validation))
            assert(vector.success === validation.isRight)
        }
    }
    test("LedgerState", TestTag) {
        assume(vectorsExist, "Conformance test vectors directory not found")

        println(s"\n=== Searching for vectors with actual ledger state data ===")

        val allVectors = loadAllVectors(conformanceVectorsPath)
        println(s"Total vectors: ${allVectors.size}")

        // Analyze vectors to find ones with data
        val vectorsWithData = allVectors.flatMap { case (path, vector) =>
            val oldStateHex = vector.oldLedgerState
            // Show CBOR structure info
            val cborBytes = Hex.hexToBytes(oldStateHex)

            val result = CborParser.parseLedgerState(oldStateHex)
            result.toOption.map { state =>
                val score =
                  state.utxos.size * 100 +
                  state.stakePools.map(_.size).getOrElse(0) * 50 +
                  state.dreps.map(_.size).getOrElse(0) * 50 +
                  state.accounts.map(_.size).getOrElse(0) * 30 +
                  (if (state.deposited.exists(_ != "0")) 10 else 0) +
                  (if (state.fees.exists(_ != "0")) 10 else 0)

                (path, vector, state, score, cborBytes.length)
            }
        }.filter(_._4 > 0).sortBy(-_._4).take(10)

        println(s"\nFound ${vectorsWithData.size} vectors with data")

        vectorsWithData.foreach { case (path, vector, state, score, cborLen) =>
            println(s"\n  Score: $score, CBOR size: $cborLen bytes")
            println(s"  Path: ${path.getParent.getFileName}/${path.getFileName}")
            println(s"  UTxOs: ${state.utxos.size}, Pools: ${state.stakePools.map(_.size).getOrElse(0)}, " +
                   s"DReps: ${state.dreps.map(_.size).getOrElse(0)}, Accounts: ${state.accounts.map(_.size).getOrElse(0)}")
        }

        // Show the best one in detail
        vectorsWithData.headOption match {
            case Some((path, vector, oldState, score, _)) =>
                println(s"\n=== Showing vector with most data (score: $score) ===")
                println(s"Path: ${path.getParent.getFileName}/${path.getFileName}")
                println(s"Test name: ${vector.testState}")
                println(s"Expected to succeed: ${vector.success}")
                println(s"Old ledger state CBOR length: ${vector.oldLedgerState.length / 2} bytes")
                println(s"Old ledger state CBOR prefix: ${vector.oldLedgerState.take(100)}...")

                println(s"\n=== OLD Ledger State (before transaction) ===")
                pprint.pprintln(oldState, height = 100)

                vector.newLedgerState.foreach { newStateHex =>
                    println(s"\nNew ledger state CBOR length: ${newStateHex.length / 2} bytes")
                    CborParser.parseLedgerState(newStateHex).toOption.foreach { newState =>
                        println(s"\n=== NEW Ledger State (after transaction) ===")
                        pprint.pprintln(newState, height = 100)
                    }
                }
            case None =>
                // If no vectors with data, show the first one with debugging
                println("\n⚠ No vectors found with significant data")
                println("Showing first vector for debugging:")

                allVectors.headOption.foreach { case (path, vector) =>
                    println(s"\nPath: ${path.getParent.getFileName}/${path.getFileName}")
                    println(s"Old ledger state CBOR length: ${vector.oldLedgerState.length / 2} bytes")
                    println(s"Old ledger state CBOR hex: ${vector.oldLedgerState.take(200)}")

                    val result = CborParser.parseLedgerState(vector.oldLedgerState)
                    result match {
                        case scala.util.Success(state) =>
                            println("\nParsing succeeded but returned empty state:")
                            pprint.pprintln(state, height = 50)
                        case scala.util.Failure(e) =>
                            println(s"\nParsing failed: ${e.getMessage}")
                            e.printStackTrace()
                    }
                }
        }
    }
}
