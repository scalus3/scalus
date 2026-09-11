package scalus.examples.cape.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{CardanoInfo, ExUnits, RefScriptFee}
import scalus.examples.cape.{CapeHarness, CapeTestSuite}
import scalus.testing.kit.ScalusTest

/** CAPE test harness for the HTLC (Hashed Time-Locked Contract) benchmark.
  *
  * Loads the v3.0.0 `cape-tests.json` fixtures via the shared `CapeTestSuite` loader and runs all
  * cases against the compiled `HtlcValidator`.
  */
class HtlcCapeTest extends AnyFunSuite with ScalusTest {
    private given CardanoInfo = CardanoInfo.mainnet
    private val program = HtlcContract.program
    private val suite = CapeTestSuite.load("/cape/htlc/cape-tests.json")
    private val protocolParams = summon[CardanoInfo].protocolParams
    private val scriptSize = program.cborByteString.size
    // Price it as the transaction's only reference script, using the repository's parameter snapshot.
    private val referenceScriptFee = RefScriptFee.fee(scriptSize, protocolParams).value

    test(s"Script size: $scriptSize bytes") {
        assert(scriptSize == 569)
    }

    private val expectedBudgets: Map[String, ExUnits] = Map(
      "claim_well_before_timeout" -> ExUnits(memory = 44945, steps = 18406297),
      "claim_just_before_timeout" -> ExUnits(memory = 44945, steps = 18406297),
      "refund_just_after_timeout" -> ExUnits(memory = 41812, steps = 16987475),
      "refund_well_after_timeout" -> ExUnits(memory = 41812, steps = 16987475)
    )

    for c <- suite.cases do
        test(s"CAPE: ${c.name}") {
            CapeHarness.run(program, c).foreach { budget =>
                val executionFee = budget.fee(protocolParams.executionUnitPrices).value
                val combinedFee = referenceScriptFee + executionFee
                info(
                  s"${c.name}: $budget; fees (lovelace): reference-script=$referenceScriptFee, " +
                      s"execution=$executionFee, combined=$combinedFee"
                )
                expectedBudgets.get(c.name).foreach(exp => assert(budget == exp))
            }
        }
}
