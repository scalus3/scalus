package scalus.examples.cape.factorial

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{CardanoInfo, Coin, ExUnits}
import scalus.testing.kit.ScalusTest
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.*

/** CAPE test harness for the Factorial benchmarks.
  *
  * Parses cape-tests.json and runs all test cases against both base (naive recursion compiled from
  * Scala) and open (hand-crafted UPLC) implementations.
  */
class FactorialCapeTest extends AnyFunSuite with ScalusTest {
    private given CardanoInfo = CardanoInfo.mainnet
    private val baseProgram = FactorialContract.baseProgram
    private val openProgram = FactorialContract.openProgram

    private val testsJson: ujson.Value = {
        val stream = getClass.getResourceAsStream("/cape/factorial/cape-tests.json")
        assert(stream != null, "cape-tests.json not found in test resources")
        ujson.read(stream)
    }

    private val tests: Seq[ujson.Value] = testsJson("tests").arr.toSeq

    test(s"Base script size: ${baseProgram.cborByteString.length} bytes") {
        assert(baseProgram.cborByteString.length == 52)
    }

    test(s"Open script size: ${openProgram.cborByteString.length} bytes") {
        assert(openProgram.cborByteString.length == 40)
    }

    for testCase <- tests do {
        val testName = testCase("name").str
        val input = parseUplcInteger(testCase("inputs").arr.head("value").str)
        val expected = parseUplcInteger(testCase("expected")("content").str)

        test(s"CAPE base: $testName") {
            val result = (baseProgram $ input.asTerm).term.evaluateDebug
            result match {
                case Result.Success(term, budget, _, _) =>
                    assert(
                      term == asTerm(expected),
                      s"factorial($input) expected $expected"
                    )
                    val actual = ExUnits(memory = budget.memory, steps = budget.steps)
                    val fee = actual.fee
                    expectedBaseBudgets.get(testName).foreach { exp =>
                        assert(actual == exp, s"$testName budget: expected $exp but got $actual")
                    }
                    expectedBaseFees.get(testName).foreach { exp =>
                        assert(fee == exp, s"$testName fee: expected $exp but got $fee")
                    }
                case Result.Failure(err, _, _, _) =>
                    fail(s"$testName: Evaluation failed: $err")
            }
        }

        test(s"CAPE open: $testName") {
            val result = (openProgram $ input.asTerm).term.evaluateDebug
            result match {
                case Result.Success(term, budget, _, _) =>
                    assert(
                      term == asTerm(expected),
                      s"factorial($input) expected $expected"
                    )
                    val actual = ExUnits(memory = budget.memory, steps = budget.steps)
                    val fee = actual.fee
                    expectedOpenBudgets.get(testName).foreach { exp =>
                        assert(actual == exp, s"$testName budget: expected $exp but got $actual")
                    }
                    expectedOpenFees.get(testName).foreach { exp =>
                        assert(fee == exp, s"$testName fee: expected $exp but got $fee")
                    }
                case Result.Failure(err, _, _, _) =>
                    fail(s"$testName: Evaluation failed: $err")
            }
        }
    }

    /** Parse an integer from UPLC constant format like "(con integer 10)" */
    private def parseUplcInteger(s: String): BigInt = {
        val pattern = """\(con integer (-?\d+)\)""".r
        s match {
            case pattern(n) => BigInt(n)
            case _          => throw RuntimeException(s"Cannot parse UPLC integer: $s")
        }
    }

    private val expectedBaseBudgets: Map[String, ExUnits] = Map(
      "factorial_0" -> ExUnits(memory = 2602, steps = 519986),
      "factorial_1" -> ExUnits(memory = 5808, steps = 1344033),
      "factorial_2" -> ExUnits(memory = 9014, steps = 2168080),
      "factorial_3" -> ExUnits(memory = 12220, steps = 2992127),
      "factorial_4" -> ExUnits(memory = 15426, steps = 3816174),
      "factorial_5" -> ExUnits(memory = 18632, steps = 4640221),
      "factorial_8" -> ExUnits(memory = 28250, steps = 7112362),
      "factorial_10" -> ExUnits(memory = 34662, steps = 8760456),
      "factorial_12" -> ExUnits(memory = 41074, steps = 10408550),
      "factorial_negative" -> ExUnits(memory = 2602, steps = 519986)
    )

    private val expectedBaseFees: Map[String, Coin] = Map(
      "factorial_0" -> Coin(188),
      "factorial_1" -> Coin(433),
      "factorial_2" -> Coin(677),
      "factorial_3" -> Coin(921),
      "factorial_4" -> Coin(1166),
      "factorial_5" -> Coin(1410),
      "factorial_8" -> Coin(2143),
      "factorial_10" -> Coin(2632),
      "factorial_12" -> Coin(3121),
      "factorial_negative" -> Coin(188)
    )

    private val expectedOpenBudgets: Map[String, ExUnits] = Map(
      "factorial_0" -> ExUnits(memory = 2302, steps = 471986),
      "factorial_1" -> ExUnits(memory = 4908, steps = 1200033),
      "factorial_2" -> ExUnits(memory = 7514, steps = 1928080),
      "factorial_3" -> ExUnits(memory = 10120, steps = 2656127),
      "factorial_4" -> ExUnits(memory = 12726, steps = 3384174),
      "factorial_5" -> ExUnits(memory = 15332, steps = 4112221),
      "factorial_8" -> ExUnits(memory = 23150, steps = 6296362),
      "factorial_10" -> ExUnits(memory = 28362, steps = 7752456),
      "factorial_12" -> ExUnits(memory = 33574, steps = 9208550),
      "factorial_negative" -> ExUnits(memory = 2302, steps = 471986)
    )

    private val expectedOpenFees: Map[String, Coin] = Map(
      "factorial_0" -> Coin(167),
      "factorial_1" -> Coin(370),
      "factorial_2" -> Coin(573),
      "factorial_3" -> Coin(776),
      "factorial_4" -> Coin(979),
      "factorial_5" -> Coin(1182),
      "factorial_8" -> Coin(1790),
      "factorial_10" -> Coin(2196),
      "factorial_12" -> Coin(2602),
      "factorial_negative" -> Coin(167)
    )
}
