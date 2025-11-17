package scalus.testing.conformance.amaru.rules

import scalus.testing.conformance.amaru.ConformanceTestBase
import java.nio.file.Files

/** Conformance tests for block validation rules
  *
  * Tests CBOR parsing of real blocks from Amaru test fixtures.
  * Full validator integration will be added as ledger rules are completed.
  */
class BlockRulesConformanceTest extends ConformanceTestBase:

    test("Block rules - load and parse block CBOR from preprod test fixtures") {
        requireLedgerRulesData()

        val blockTests = getAvailableBlockTests("preprod")

        if blockTests.isEmpty then cancel("No block test cases available")

        blockTests.foreach { blockNumber =>
            val blockPath = dataDir.resolve(s"blocks/preprod/$blockNumber")
            val validCborPath = blockPath.resolve("valid.cbor")

            if Files.exists(validCborPath) then
                val blockCbor = loadCbor(validCborPath)
                info(s"Block $blockNumber: ${blockCbor.length} bytes")
                assert(blockCbor.length > 0, "Block CBOR should not be empty")
            else info(s"Block $blockNumber: no valid.cbor file found")
        }

        succeed
    }
