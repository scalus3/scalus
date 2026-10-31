package scalus.examples.cape.ecd

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{CardanoInfo, ExUnits}
import scalus.examples.cape.{CapeHarness, CapeTestSuite}
import scalus.testing.kit.ScalusTest
import scalus.uplc.DefaultFun

class EcdCapeTest extends AnyFunSuite with ScalusTest {
    private given CardanoInfo = CardanoInfo.mainnet
    private val program = EcdContract.program
    private val suite = CapeTestSuite.load("/cape/ecd/cape-tests.json")

    // The spec recurses on `a `mod` b`, and CAPE rules that `rem` changes the recursive arguments
    // for negative inputs: ecd(-12, 8) must recurse into ecd(8, 4), not ecd(8, -4). The results
    // agree either way, so only the builtin used shows the difference.
    test("recursion uses modInteger, as the prescribed algorithm does") {
        val used = program.term.collectBuiltins
        assert(used.contains(DefaultFun.ModInteger))
        assert(!used.contains(DefaultFun.RemainderInteger))
    }

    test(s"Script size: ${program.cborByteString.length} bytes") {
        assert(program.cborByteString.length == 46)
    }

    private val expectedBudgets: Map[String, ExUnits] = Map(
      "ecd_0_12" -> ExUnits(memory = 4404, steps = 970397),
      "ecd_12_0" -> ExUnits(memory = 2602, steps = 497723),
      "ecd_7_7" -> ExUnits(memory = 4404, steps = 970397),
      "ecd_6_9" -> ExUnits(memory = 8008, steps = 1915745),
      "ecd_12_8" -> ExUnits(memory = 6206, steps = 1443071),
      "ecd_15_25" -> ExUnits(memory = 9810, steps = 2388419),
      "ecd_17_19" -> ExUnits(memory = 9810, steps = 2388419),
      "ecd_13_29" -> ExUnits(memory = 9810, steps = 2388419),
      "ecd_48_18" -> ExUnits(memory = 8008, steps = 1915745),
      "ecd_100_75" -> ExUnits(memory = 6206, steps = 1443071),
      "ecd_1071_462" -> ExUnits(memory = 8008, steps = 1915745),
      "ecd_2520_1890" -> ExUnits(memory = 6206, steps = 1443071),
      "ecd_negative_12_8" -> ExUnits(memory = 6206, steps = 1443071),
      "ecd_12_negative_8" -> ExUnits(memory = 6608, steps = 1608279)
    )

    for c <- suite.cases do
        test(s"CAPE: ${c.name}") {
            CapeHarness.run(program, c).foreach { budget =>
                info(s"${c.name}: $budget")
                expectedBudgets.get(c.name).foreach(exp => assert(budget == exp))
            }
        }
}
