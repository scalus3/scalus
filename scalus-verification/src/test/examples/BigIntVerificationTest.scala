package scalus.verify.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.Compile
import scalus.verify.Props.*
import scalus.verify.uplcblaster.{Budget, LeanProofsSuite, UplcBlaster}

import scala.concurrent.duration.*

@Compile
object RoundedMagnitude {

    /** Half the magnitude, rounded up: -5 -> 3, 0 -> 0, 4 -> 2. */
    def half(x: BigInt): BigInt = {
        val magnitude = if x < 0 then -x else x
        (magnitude + 1) / 2
    }
}

/** A small example for reports: every check concerns the compiled UPLC of the same function. Run
  * with: sbt 'scalusVerification/testOnly *BigIntVerificationTest'
  */
class BigIntVerificationTest extends AnyFunSuite with LeanProofsSuite {
    import proofs.*
    import RoundedMagnitude.half

    // The statements, declared by name in the suite's verifier. Declaring one runs nothing; a test
    // runs it with `verify`, and asserts what it expects of the result.

    private val doubledIsNearTheMagnitude = verifier.statement(
      "half.doubled_is_near_the_magnitude",
      forAll[BigInt] { x =>
          val magnitude = if x < 0 then -x else x
          val r = half(x)
          magnitude <= r * 2 && r * 2 <= magnitude + 1
      }
    )

    /** A negative control, declared with `refute`: it must be refuted. */
    private val doublingRecoversANonnegativeInput = verifier.refute(
      "half.doubling_recovers_a_nonnegative_input",
      forAll[BigInt](x => (x >= 0) ==>: (half(x) * 2 == x))
    )

    private val neverNegative =
        verifier.statement("half.never_negative", forAll[BigInt](x => half(x) >= 0))

    /** The same property, for a check given too few steps. */
    private val neverNegativeInFewSteps =
        verifier.statement("half.never_negative.few_steps", forAll[BigInt](x => half(x) >= 0))

    test("proof: doubling the result is between the magnitude and magnitude + 1") {
        val result = verify(doubledIsNearTheMagnitude, budget = 160)
        assert(result.meets(doubledIsNearTheMagnitude), explain(doubledIsNearTheMagnitude, result))
    }

    test("counterexample: doubling the result does not always recover a nonnegative input") {
        val result = verify(doublingRecoversANonnegativeInput, budget = 160)
        assert(
          result.meets(doublingRecoversANonnegativeInput),
          explain(doublingRecoversANonnegativeInput, result)
        )
        val counterexample = counterexampleOf(result)
        // The solver chooses the value; check its meaning, rather than expecting a fixed one.
        val x = integer(counterexample.values.head)
        assert(x >= 0 && x % 2 == 1, counterexample)
        assert(half(x) * 2 != x)
        info(s"Counterexample: x = $x, half(x) = ${half(x)}")
    }

    test("no solutions: no integer input produces a negative result") {
        // Prove the universal statement, equivalently: not exists x. half(x) < 0.
        val result = verify(neverNegative, budget = 100)
        assert(result.meets(neverNegative), explain(neverNegative, result))
    }

    test("inconclusive: too few evaluation steps cannot establish the same true property") {
        val result = verifyWithoutKeeping(
          neverNegativeInFewSteps,
          UplcBlaster(Budget.LeanSteps(3), lean, 30.seconds)
        )
        assert(result.isInconclusive, explain(neverNegativeInFewSteps, result))
        val reason = result.why.getOrElse("")
        // A candidate from the bounded model passes CEK replay: it is not a real counterexample.
        assert(reason.contains("spurious"), reason)
        info(reason)
    }
}
