package scalus.examples.cape.fibonacci

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{CardanoInfo, Coin, ExUnits}
import scalus.examples.cape.{CapeHarness, CapeTestSuite}
import scalus.testing.kit.ScalusTest
import scalus.uplc.{Constant, Term}
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.Result

/** CAPE test harness for the Fibonacci benchmarks.
  *
  * Loads the v3.0.0 `cape-tests.json` fixtures via the shared `CapeTestSuite` loader and runs all
  * cases against both base (naive recursion compiled from Scala) and open (hand-crafted UPLC)
  * implementations.
  *
  * The open program (`FibonacciOpen.term`) is correct for every integer input, not just the
  * fixture's `0..25`/`-1` range: `fib(0)..fib(25)` is a memoized base case (PV11
  * case-on-builtin-integer), and `n >= 26` falls back to a linear accumulator loop (`go`) bottoming
  * out on that table. The "correctness beyond the fixture" tests below assert this directly,
  * against an independent iterative Scala fibonacci (never a hand-typed literal, and never
  * `FibonacciBase.fibonacci` itself, since that's the function under differential test).
  */
class FibonacciCapeTest extends AnyFunSuite with ScalusTest {
    private given CardanoInfo = CardanoInfo.mainnet
    private val baseProgram = FibonacciContract.baseProgram
    private val openProgram = FibonacciContract.openProgram

    private val baseSuite = CapeTestSuite.load("/cape/fibonacci_naive_recursion/cape-tests.json")
    private val openSuite = CapeTestSuite.load("/cape/fibonacci/cape-tests.json")

    test(s"Base script size: ${baseProgram.cborByteString.length} bytes") {
        assert(baseProgram.cborByteString.length == 42)
    }

    test(s"Open script size: ${openProgram.cborByteString.length} bytes") {
        assert(openProgram.cborByteString.length == 156)
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
      "fibonacci_0" -> ExUnits(memory = 1701, steps = 299937),
      "fibonacci_1" -> ExUnits(memory = 1701, steps = 299937),
      "fibonacci_2" -> ExUnits(memory = 5309, steps = 1_267235),
      "fibonacci_3" -> ExUnits(memory = 8917, steps = 2_234533),
      "fibonacci_5" -> ExUnits(memory = 26957, steps = 7_071023),
      "fibonacci_8" -> ExUnits(memory = 120765, steps = 32_220771),
      "fibonacci_10" -> ExUnits(memory = 319205, steps = 85_422161),
      "fibonacci_15" -> ExUnits(memory = 3_559189, steps = 954_055765),
      "fibonacci_20" -> ExUnits(memory = 39_491261, steps = 10587_376547L),
      "fibonacci_25" -> ExUnits(memory = 437_984037, steps = 117422_538753L),
      "fibonacci_negative" -> ExUnits(memory = 1701, steps = 299937)
    )

    private val expectedBaseFees: Map[String, Coin] = Map(
      "fibonacci_0" -> Coin(120),
      "fibonacci_1" -> Coin(120),
      "fibonacci_2" -> Coin(398),
      "fibonacci_3" -> Coin(676),
      "fibonacci_5" -> Coin(2066),
      "fibonacci_8" -> Coin(9292),
      "fibonacci_10" -> Coin(24578),
      "fibonacci_15" -> Coin(274153),
      "fibonacci_20" -> Coin(3041996),
      "fibonacci_25" -> Coin(33737844),
      "fibonacci_negative" -> Coin(120)
    )

    // FibonacciOpen.term: fib(0)..fib(25) is an O(1) memoized base case, so every non-negative
    // fixture case costs the same (one extra guard vs. the pre-fallback design); `n < 0` is even
    // cheaper (skips the case-on-integer table entirely).
    private val expectedOpenBudgets: Map[String, ExUnits] = Map(
      "fibonacci_0" -> ExUnits(memory = 1902, steps = 375774),
      "fibonacci_1" -> ExUnits(memory = 1902, steps = 375774),
      "fibonacci_2" -> ExUnits(memory = 1902, steps = 375774),
      "fibonacci_3" -> ExUnits(memory = 1902, steps = 375774),
      "fibonacci_5" -> ExUnits(memory = 1902, steps = 375774),
      "fibonacci_8" -> ExUnits(memory = 1902, steps = 375774),
      "fibonacci_10" -> ExUnits(memory = 1902, steps = 375774),
      "fibonacci_15" -> ExUnits(memory = 1902, steps = 375774),
      "fibonacci_20" -> ExUnits(memory = 1902, steps = 375774),
      "fibonacci_25" -> ExUnits(memory = 1902, steps = 375774),
      "fibonacci_negative" -> ExUnits(memory = 1101, steps = 203937)
    )

    private val expectedOpenFees: Map[String, Coin] = Map(
      "fibonacci_0" -> Coin(137),
      "fibonacci_1" -> Coin(137),
      "fibonacci_2" -> Coin(137),
      "fibonacci_3" -> Coin(137),
      "fibonacci_5" -> Coin(137),
      "fibonacci_8" -> Coin(137),
      "fibonacci_10" -> Coin(137),
      "fibonacci_15" -> Coin(137),
      "fibonacci_20" -> Coin(137),
      "fibonacci_25" -> Coin(137),
      "fibonacci_negative" -> Coin(79)
    )

    // ---- Correctness gate: the open program must compute correctly for EVERY integer input, not
    // just the 11 fixture cases (0..25, -1). `scalaFibonacci` is an independent iterative Scala
    // computation (never a hand-typed literal, and never FibonacciBase.fibonacci itself -- that's
    // the function under differential test below), so these tests catch a wrong/stale table or a
    // broken accumulator fallback the same way a hardcoded-expected-value test could not.

    private def scalaFibonacci(n: BigInt): BigInt = {
        if n <= 1 then n
        else
            @scala.annotation.tailrec
            def loop(k: BigInt, a: BigInt, b: BigInt): BigInt =
                if k == 0 then b else loop(k - 1, b, a + b)
            loop(n - 1, 0, 1)
    }

    // Evaluate at the Term level, not Program.evaluateDebug -- the latter enforces CIP-117's
    // Unit-only return value for Plutus V3 scripts, but fibonacci (open) returns its computed
    // Integer value directly (same reasoning as CapeHarness.run; see its comment).
    private def evalOpen(n: BigInt): BigInt =
        (openProgram $ n.asTerm).term.evaluateDebug match {
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) => v
            case Result.Success(term, _, _, _) =>
                fail(s"fibonacci($n): expected an integer result, got $term")
            case Result.Failure(err, _, _, logs) =>
                fail(s"fibonacci($n): expected success, got $err; logs: ${logs.mkString(", ")}")
        }

    for n <- Seq(26, 30, 40, 60) do
        test(s"CAPE open correctness beyond the fixture: fibonacci($n)") {
            assert(
              evalOpen(n) == scalaFibonacci(n),
              s"fibonacci($n) must equal an independent iterative Scala fibonacci"
            )
        }

    for n <- Seq(-100, -25, -2, -1, 0, 1) do
        test(s"CAPE open correctness: fibonacci($n)") {
            assert(
              evalOpen(n) == scalaFibonacci(n),
              s"fibonacci($n) must equal an independent iterative Scala fibonacci"
            )
        }

    // One Scala definition, three execution paths: `FibonacciBase.fibonacci` runs on the JVM
    // (where it generates the open submission's memoized table), compiles to UPLC as the
    // fibonacci_naive_recursion submission, and its values back the open submission's table. All
    // three must agree with each other and with an independent iterative computation over a range
    // where naive O(fib(n)) double recursion is still affordable to evaluate on-chain -- naive
    // fibonacci makes ~2*fib(n+2) calls, so this loop's cumulative cost across 0..25 (~2*fib(27) ~=
    // 392,836 calls total) stays well within a fast JVM CEK run; going past the fixture's own
    // largest case (25) would only test the naive baseline further from its comfort zone without
    // exercising anything `FibonacciOpen` doesn't already cover in the "beyond the fixture" tests
    // above (which use the cheap memoized/accumulator open program, not naive recursion).

    private def evalBase(n: BigInt): BigInt =
        (baseProgram $ n.asTerm).term.evaluateDebug match {
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) => v
            case Result.Success(term, _, _, _) =>
                fail(s"naive fibonacci($n): expected an integer result, got $term")
            case Result.Failure(err, _, _, logs) =>
                fail(
                  s"naive fibonacci($n): expected success, got $err; logs: ${logs.mkString(", ")}"
                )
        }

    test("JVM, compiled naive UPLC, and the memoized open submission all agree") {
        for n <- -3 to 25 do
            val jvm = FibonacciBase.fibonacci(n)
            assert(
              jvm == scalaFibonacci(n),
              s"JVM fibonacci($n) must equal an independent iterative computation"
            )
            assert(evalBase(n) == jvm, s"compiled naive UPLC must match the JVM at fibonacci($n)")
            assert(evalOpen(n) == jvm, s"the open submission must match the JVM at fibonacci($n)")
    }

    // What the memoization + linear fallback buys, in the benchmark's own units, at the largest
    // fixture input.
    test("memoized open submission costs less than the compiled naive recursion") {
        val naive = (baseProgram $ 25.asTerm).term.evaluateDebug.budget
        val memoized = (openProgram $ 25.asTerm).term.evaluateDebug.budget
        info(s"fibonacci(25): naive $naive vs memoized $memoized")
        assert(memoized.steps < naive.steps && memoized.memory < naive.memory)
    }
}
