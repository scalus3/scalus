package scalus.examples.cape.factorial

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{CardanoInfo, Coin, ExUnits}
import scalus.examples.cape.{CapeHarness, CapeTestSuite}
import scalus.testing.kit.ScalusTest
import scalus.uplc.{Constant, Program, Term}
import scalus.uplc.Term.asTerm
import scalus.uplc.transform.CaseConstrApply
import scalus.uplc.eval.Result

/** CAPE test harness for the Factorial benchmarks.
  *
  * Loads the v3.0.0 `cape-tests.json` fixtures via the shared `CapeTestSuite` loader and runs all
  * cases against both base (naive recursion compiled from Scala) and open (hand-crafted UPLC)
  * implementations.
  *
  * The open program (`FactorialOpen.term`) is correct for every integer input, not just the
  * fixture's `-5..12` range: `0!..12!` is a memoized base case (PV11 case-on-builtin-integer,
  * `FactorialOpen.termB`), and `x >= 13` falls back to genuine self-application recursion (`x *
  * factorial(x - 1)`) bottoming out on that table. The "correctness beyond the fixture" tests below
  * assert this directly, against a `BigInt` factorial computed in Scala (never a hand-typed
  * literal).
  */
class FactorialCapeTest extends AnyFunSuite with ScalusTest {
    private given CardanoInfo = CardanoInfo.mainnet
    private val baseProgram = FactorialContract.baseProgram
    private val openProgram = FactorialContract.openProgram

    private val baseSuite = CapeTestSuite.load("/cape/factorial_naive_recursion/cape-tests.json")
    private val openSuite = CapeTestSuite.load("/cape/factorial/cape-tests.json")

    test(s"Base script size: ${baseProgram.cborByteString.length} bytes") {
        assert(baseProgram.cborByteString.length == 34)
    }

    test(s"Open script size: ${openProgram.cborByteString.length} bytes") {
        assert(openProgram.cborByteString.length == 91)
    }

    for c <- baseSuite.cases do
        test(s"CAPE base: ${c.name}") {
            CapeHarness.run(baseProgram, c).foreach { budget =>
                info(s"${c.name}: $budget")
                val fee = budget.fee
                expectedBaseBudgets.get(c.name).foreach { exp =>
                    assert(budget == exp, s"${c.name} budget: expected $exp but got $budget")
                }
                expectedBaseFees.get(c.name).foreach { exp =>
                    assert(fee == exp, s"${c.name} fee: expected $exp but got $fee")
                }
            }
        }

    for c <- openSuite.cases do
        test(s"CAPE open: ${c.name}") {
            CapeHarness.run(openProgram, c).foreach { budget =>
                info(s"${c.name}: $budget")
                val fee = budget.fee
                expectedOpenBudgets.get(c.name).foreach { exp =>
                    assert(budget == exp, s"${c.name} budget: expected $exp but got $budget")
                }
                expectedOpenFees.get(c.name).foreach { exp =>
                    assert(fee == exp, s"${c.name} fee: expected $exp but got $fee")
                }
            }
        }

    private val expectedBaseBudgets: Map[String, ExUnits] = Map(
      "factorial_0" -> ExUnits(memory = 1701, steps = 299937),
      "factorial_1" -> ExUnits(memory = 3706, steps = 855935),
      "factorial_2" -> ExUnits(memory = 5711, steps = 1_411933),
      "factorial_3" -> ExUnits(memory = 7716, steps = 1_967931),
      "factorial_4" -> ExUnits(memory = 9721, steps = 2_523929),
      "factorial_5" -> ExUnits(memory = 11726, steps = 3_079927),
      "factorial_8" -> ExUnits(memory = 17741, steps = 4_747921),
      "factorial_10" -> ExUnits(memory = 21751, steps = 5_859917),
      "factorial_12" -> ExUnits(memory = 25761, steps = 6_971913),
      "factorial_negative" -> ExUnits(memory = 1701, steps = 299937)
    )

    private val expectedBaseFees: Map[String, Coin] = Map(
      "factorial_0" -> Coin(120),
      "factorial_1" -> Coin(276),
      "factorial_2" -> Coin(432),
      "factorial_3" -> Coin(588),
      "factorial_4" -> Coin(743),
      "factorial_5" -> Coin(899),
      "factorial_8" -> Coin(1366),
      "factorial_10" -> Coin(1678),
      "factorial_12" -> Coin(1990),
      "factorial_negative" -> Coin(120)
    )

    // FactorialOpen.term (adopted `termB`, PV11 case-on-builtin-integer): 0!..12! is an O(1)
    // memoized base case, so every non-negative fixture case costs the same; `x < 0` is even
    // cheaper (matches the old direct-constant path, no table case at all).
    private val expectedOpenBudgets: Map[String, ExUnits] = Map(
      "factorial_0" -> ExUnits(memory = 2502, steps = 471774),
      "factorial_1" -> ExUnits(memory = 2502, steps = 471774),
      "factorial_2" -> ExUnits(memory = 2502, steps = 471774),
      "factorial_3" -> ExUnits(memory = 2502, steps = 471774),
      "factorial_4" -> ExUnits(memory = 2502, steps = 471774),
      "factorial_5" -> ExUnits(memory = 2502, steps = 471774),
      "factorial_8" -> ExUnits(memory = 2502, steps = 471774),
      "factorial_10" -> ExUnits(memory = 2502, steps = 471774),
      "factorial_12" -> ExUnits(memory = 2502, steps = 471774),
      "factorial_negative" -> ExUnits(memory = 1701, steps = 299937)
    )

    private val expectedOpenFees: Map[String, Coin] = Map(
      "factorial_0" -> Coin(179),
      "factorial_1" -> Coin(179),
      "factorial_2" -> Coin(179),
      "factorial_3" -> Coin(179),
      "factorial_4" -> Coin(179),
      "factorial_5" -> Coin(179),
      "factorial_8" -> Coin(179),
      "factorial_10" -> Coin(179),
      "factorial_12" -> Coin(179),
      "factorial_negative" -> Coin(120)
    )

    // ---- Correctness gate: the open program must compute correctly for EVERY integer input, not
    // just the 10 fixture cases (-5..12). `scalaFactorial` is a genuine Scala BigInt computation
    // (never a hand-typed literal), so these tests catch a wrong/stale table or a broken recursive
    // fallback the same way a hardcoded-expected-value test could not.

    private def scalaFactorial(n: BigInt): BigInt = {
        @scala.annotation.tailrec
        def loop(i: BigInt, acc: BigInt): BigInt = if i > n then acc else loop(i + 1, acc * i)
        if n <= 0 then 1 else loop(1, 1)
    }

    // Evaluate at the Term level, not Program.evaluateDebug -- the latter enforces CIP-117's
    // Unit-only return value for Plutus V3 scripts, but factorial (open) returns its computed
    // Integer value directly (same reasoning as CapeHarness.run; see its comment).
    private def evalOpen(x: BigInt): BigInt =
        (openProgram $ x.asTerm).term.evaluateDebug match {
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) => v
            case Result.Success(term, _, _, _) =>
                fail(s"factorial($x): expected an integer result, got $term")
            case Result.Failure(err, _, _, logs) =>
                fail(s"factorial($x): expected success, got $err; logs: ${logs.mkString(", ")}")
        }

    for x <- Seq(13, 15, 20, 25) do
        test(s"CAPE open correctness beyond the fixture: factorial($x)") {
            assert(
              evalOpen(x) == scalaFactorial(x),
              s"factorial($x) must equal Scala's BigInt factorial"
            )
        }

    for x <- Seq(-100, -1, 0, 1) do
        test(s"CAPE open correctness: factorial($x)") {
            assert(
              evalOpen(x) == scalaFactorial(x),
              s"factorial($x) must equal Scala's BigInt factorial"
            )
        }

    // `termA` is the reference PV9 variant: not on any shipped path, so nothing else exercises it,
    // and its byte table is derived from `factorials` with nothing hand-syncing the two encodings.
    // Evaluating it across the table range and past it catches a wrong entry, width, endianness, or
    // range guard -- which would otherwise silently invalidate its documented measurements.
    private lazy val openProgramA: Program = CaseConstrApply(FactorialOpen.termA).plutusV3

    private def evalOpenA(x: BigInt): BigInt =
        (openProgramA $ x.asTerm).term.evaluateDebug match {
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) => v
            case Result.Success(term, _, _, _) =>
                fail(s"termA factorial($x): expected an integer result, got $term")
            case Result.Failure(err, _, _, logs) =>
                fail(
                  s"termA factorial($x): expected success, got $err; logs: ${logs.mkString(", ")}"
                )
        }

    test("reference variant termA computes factorial across and beyond its table") {
        for x <- (-5 to 12) ++ Seq(13, 20, 25) do
            assert(
              evalOpenA(x) == scalaFactorial(x),
              s"termA factorial($x) must equal Scala's BigInt factorial"
            )
    }

    // One Scala definition, three execution paths: `FactorialBase.factorial` runs on the JVM (where
    // it generates the open submission's memoized table), compiles to UPLC as the
    // factorial_naive_recursion submission, and its values back the open submission's table. All
    // three must agree with each other and with an independent BigInt computation -- so a
    // divergence between what Scalus runs off-chain and what it compiles on-chain fails here.

    private def evalBase(x: BigInt): BigInt =
        (baseProgram $ x.asTerm).term.evaluateDebug match {
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) => v
            case Result.Success(term, _, _, _) =>
                fail(s"naive factorial($x): expected an integer result, got $term")
            case Result.Failure(err, _, _, logs) =>
                fail(
                  s"naive factorial($x): expected success, got $err; logs: ${logs.mkString(", ")}"
                )
        }

    test("JVM, compiled naive UPLC, and the memoized open submission all agree") {
        for x <- -3 to 15 do
            val jvm = FactorialBase.factorial(x)
            assert(
              jvm == scalaFactorial(x),
              s"JVM factorial($x) must equal an independent BigInt computation"
            )
            assert(evalBase(x) == jvm, s"compiled naive UPLC must match the JVM at factorial($x)")
            assert(evalOpen(x) == jvm, s"the open submission must match the JVM at factorial($x)")
    }

    // What the memoization buys, in the benchmark's own units, at the largest fixture input.
    test("memoized open submission costs less than the compiled naive recursion") {
        val naive = (baseProgram $ 12.asTerm).term.evaluateDebug.budget
        val memoized = (openProgram $ 12.asTerm).term.evaluateDebug.budget
        info(s"factorial(12): naive $naive vs memoized $memoized")
        assert(memoized.steps < naive.steps && memoized.memory < naive.memory)
    }
}
