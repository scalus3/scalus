package scalus.examples.cape.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{CardanoInfo, ExUnits}
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

    test(s"Script size: ${program.cborByteString.length} bytes") {
        assert(program.cborByteString.length == 582)
    }

    private val expectedBudgets: Map[String, ExUnits] = Map(
      "claim_well_before_timeout" -> ExUnits(memory = 49304, steps = 19732160),
      "claim_just_before_timeout" -> ExUnits(memory = 49304, steps = 19732160),
      "refund_just_after_timeout" -> ExUnits(memory = 46371, steps = 18345338),
      "refund_well_after_timeout" -> ExUnits(memory = 46371, steps = 18345338)
    )

    for c <- suite.cases do
        test(s"CAPE: ${c.name}") {
            CapeHarness.run(program, c).foreach { budget =>
                info(s"${c.name}: $budget")
                expectedBudgets.get(c.name).foreach(exp => assert(budget == exp))
            }
        }
}
