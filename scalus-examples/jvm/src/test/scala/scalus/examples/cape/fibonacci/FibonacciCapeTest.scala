package scalus.examples.cape.fibonacci

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{CardanoInfo, Coin, ExUnits}
import scalus.testing.kit.ScalusTest
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.*

/** CAPE test harness for the Fibonacci benchmarks.
  *
  * Parses cape-tests.json and runs all test cases against both base (naive recursion compiled from
  * Scala) and open (hand-crafted UPLC) implementations.
  */
class FibonacciCapeTest extends AnyFunSuite with ScalusTest {
    private given CardanoInfo = CardanoInfo.mainnet
    private val baseProgram = FibonacciContract.baseProgram
    private val openProgram = FibonacciContract.openProgram

    private val testsJson: ujson.Value = {
        val stream = getClass.getResourceAsStream("/cape/fibonacci/cape-tests.json")
        assert(stream != null, "cape-tests.json not found in test resources")
        ujson.read(stream)
    }

    private val tests: Seq[ujson.Value] = testsJson("tests").arr.toSeq

    test(s"Base script size: ${baseProgram.cborByteString.length} bytes") {
        assert(baseProgram.cborByteString.length == 60)
    }

    test(s"Open script size: ${openProgram.cborByteString.length} bytes") {
        assert(openProgram.cborByteString.length == 47)
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
                      s"fibonacci($input) expected $expected"
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
                      s"fibonacci($input) expected $expected"
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
      "fibonacci_0" -> ExUnits(memory = 2902, steps = 567986),
      "fibonacci_1" -> ExUnits(memory = 2902, steps = 567986),
      "fibonacci_2" -> ExUnits(memory = 8712, steps = 2039382),
      "fibonacci_3" -> ExUnits(memory = 14522, steps = 3510778),
      "fibonacci_5" -> ExUnits(memory = 43572, steps = 10867758),
      "fibonacci_8" -> ExUnits(memory = 194632, steps = 49124054),
      "fibonacci_10" -> ExUnits(memory = 514182, steps = 130050834),
      "fibonacci_15" -> ExUnits(memory = 5731562, steps = 1451364442),
      "fibonacci_20" -> ExUnits(memory = 63593352, steps = 16104997206L),
      "fibonacci_25" -> ExUnits(memory = 705290422, steps = 178616271218L),
      "fibonacci_negative" -> ExUnits(memory = 2902, steps = 567986)
    )

    private val expectedBaseFees: Map[String, Coin] = Map(
      "fibonacci_0" -> Coin(209),
      "fibonacci_1" -> Coin(209),
      "fibonacci_2" -> Coin(650),
      "fibonacci_3" -> Coin(1092),
      "fibonacci_5" -> Coin(3298),
      "fibonacci_8" -> Coin(14773),
      "fibonacci_10" -> Coin(39045),
      "fibonacci_15" -> Coin(435355),
      "fibonacci_20" -> Coin(4830507),
      "fibonacci_25" -> Coin(53573491),
      "fibonacci_negative" -> Coin(209)
    )

    private val expectedOpenBudgets: Map[String, ExUnits] = Map(
      "fibonacci_0" -> ExUnits(memory = 2302, steps = 471986),
      "fibonacci_1" -> ExUnits(memory = 2302, steps = 471986),
      "fibonacci_2" -> ExUnits(memory = 7112, steps = 1783382),
      "fibonacci_3" -> ExUnits(memory = 11922, steps = 3094778),
      "fibonacci_5" -> ExUnits(memory = 35972, steps = 9651758),
      "fibonacci_8" -> ExUnits(memory = 161032, steps = 43748054),
      "fibonacci_10" -> ExUnits(memory = 425582, steps = 115874834),
      "fibonacci_15" -> ExUnits(memory = 4744962, steps = 1293508442),
      "fibonacci_20" -> ExUnits(memory = 52647752, steps = 14353701206L),
      "fibonacci_25" -> ExUnits(memory = 583897822, steps = 159193455218L),
      "fibonacci_negative" -> ExUnits(memory = 2302, steps = 471986)
    )

    private val expectedOpenFees: Map[String, Coin] = Map(
      "fibonacci_0" -> Coin(167),
      "fibonacci_1" -> Coin(167),
      "fibonacci_2" -> Coin(539),
      "fibonacci_3" -> Coin(912),
      "fibonacci_5" -> Coin(2772),
      "fibonacci_8" -> Coin(12446),
      "fibonacci_10" -> Coin(32911),
      "fibonacci_15" -> Coin(367047),
      "fibonacci_20" -> Coin(4072678),
      "fibonacci_25" -> Coin(45168753),
      "fibonacci_negative" -> Coin(167)
    )
}
