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
        assert(baseProgram.cborByteString.length == 52)
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
      "fibonacci_0" -> ExUnits(memory = 2001, steps = 347937),
      "fibonacci_1" -> ExUnits(memory = 2001, steps = 347937),
      "fibonacci_2" -> ExUnits(memory = 6809, steps = 1_507235),
      "fibonacci_3" -> ExUnits(memory = 11617, steps = 2_666533),
      "fibonacci_5" -> ExUnits(memory = 35657, steps = 8_463023),
      "fibonacci_8" -> ExUnits(memory = 160665, steps = 38_604771),
      "fibonacci_10" -> ExUnits(memory = 425105, steps = 102_366161),
      "fibonacci_15" -> ExUnits(memory = 4_742689, steps = 1143_415765),
      "fibonacci_20" -> ExUnits(memory = 52_625561, steps = 12688_864547L),
      "fibonacci_25" -> ExUnits(memory = 583_654737, steps = 140729_850753L),
      "fibonacci_negative" -> ExUnits(memory = 2001, steps = 347937)
    )

    private val expectedBaseFees: Map[String, Coin] = Map(
      "fibonacci_0" -> Coin(141),
      "fibonacci_1" -> Coin(141),
      "fibonacci_2" -> Coin(502),
      "fibonacci_3" -> Coin(863),
      "fibonacci_5" -> Coin(2668),
      "fibonacci_8" -> Coin(12054),
      "fibonacci_10" -> Coin(31910),
      "fibonacci_15" -> Coin(356094),
      "fibonacci_20" -> Coin(3951363),
      "fibonacci_25" -> Coin(43823501),
      "fibonacci_negative" -> Coin(141)
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
