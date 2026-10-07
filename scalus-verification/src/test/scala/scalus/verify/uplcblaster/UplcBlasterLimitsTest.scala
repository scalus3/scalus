package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
// Wildcard, not a selective import: `List.filter` and friends are extension methods that a
// selective import would leave out of scope. This shadows scala.List and scala.Option here.
import scalus.cardano.onchain.plutus.prelude.*
import scalus.uplc.builtin.Data
import scalus.verify.*
import scalus.verify.Props.*

import scala.concurrent.duration.*

/** Statements that are natural to write and that [[UplcBlaster]] cannot settle: their programs
  * loop, and how long the loop runs depends on a quantified value.
  *
  * None of them is provable at any budget, because some input needs more steps than the budget;
  * that takes induction. What differs is the cost of finding that out: seconds for a loop that only
  * walks, and no result at all once each round of the loop makes a choice, or leaves arithmetic to
  * the solver. The timings are in `docs/design/verification-details/uplc-blaster.md`.
  *
  * A test here that starts to fail because Lean finishes is good news: the statement then needs a
  * larger budget, or is no longer a limit.
  */
class UplcBlasterLimitsTest extends AnyFunSuite with LeanProofs {

    /** The time a statement that Lean can finish gets. They take seconds. */
    private val finishes = 2.minutes

    /** The time before a statement that Lean cannot finish is given up. They gave no result in
      * minutes where this was written.
      */
    private val givenUp = 15.seconds

    private val length = FunctionDef.named("length", (xs: List[BigInt]) => xs.length)

    private val positives =
        FunctionDef.named("positives", (xs: List[BigInt]) => xs.filter(_ > BigInt(0)).length)

    private val gcd = FunctionDef(Math.gcd)

    private def lengthIsNotNegative =
        forAll[Data](d => whenReturns(length, d.to[List[BigInt]])(n => n >= BigInt(0)))

    private def filterDoesNotLengthen = forAll[Data](d =>
        whenReturns(positives, d.to[List[BigInt]])(n => n <= d.to[List[BigInt]].length)
    )

    private def gcdIsNotNegative =
        forAll[BigInt, BigInt]((x, y) => call(gcd, (x, y))(r => r >= BigInt(0)))

    test("a loop that only walks a list is inconclusive in seconds, at any budget", Unfinished) {
        // One path per length of the list. Lean's counterexample is a list too long for the
        // budget, on which the statement holds.
        for budget <- Seq(100, 800) do
            val reason = inconclusive(lengthIsNotNegative, budget, finishes, length)
            assert(reason.contains("spurious"), reason)
    }

    test("no budget is found for a loop over a whole list", Unfinished) {
        // Every counterexample is a list too long for the budget, and asks for the steps of
        // that list. The next budget gives a longer one, and so on to a time limit.
        val sought =
            inconclusive(lengthIsNotNegative, UplcBlaster(Budget.Auto, lean, 40.seconds), length)
        assert(sought.contains("The tactic sought the budget, and tried 100"), sought)
    }

    test("a loop that makes a choice per element is not finished by Lean", Unfinished) {
        // Each element is kept or dropped, so the paths double with it. At a small budget the
        // loop is cut after a few elements.
        val small = inconclusive(filterDoesNotLengthen, 100, finishes, positives)
        assert(small.contains("spurious"), small)
        // At budget 400 Lean still finishes, in 20 s. At 800 it does not: the time goes to
        // Blaster's symbolic run of each leaf, before the solver is asked.
        val large = inconclusive(filterDoesNotLengthen, 800, givenUp, positives)
        assert(large.contains("did not finish"), large)
        assert(large.contains("symbolically"), large)
        assert(large.contains("had not come to the solver"), large)
    }

    test("a recursion over integers is not finished by the solver", Unfinished) {
        val small = inconclusive(gcdIsNotNegative, 200, finishes, gcd)
        assert(small.contains("spurious"), small)
        // Lean is done in seconds; Z3 is then left with nested remainders, and takes gigabytes.
        val large = inconclusive(gcdIsNotNegative, 400, givenUp, gcd)
        assert(large.contains("did not finish"), large)
        assert(large.contains("Blaster had started the solver"), large)
    }
}
