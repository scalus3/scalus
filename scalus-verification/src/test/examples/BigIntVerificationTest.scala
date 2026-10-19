package scalus.verify.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.Compile
import scalus.verify.Props.*
import scalus.verify.uplcblaster.LeanProofsSuite

import scala.concurrent.duration.*

@Compile
object RoundedMagnitude {

    /** Half the magnitude, rounded up: -5 -> 3, 0 -> 0, 4 -> 2. */
    def half(x: BigInt): BigInt = {
        val magnitude = if x < BigInt(0) then -x else x
        (magnitude + BigInt(1)) / BigInt(2)
    }
}

/** A small example for reports: every check concerns the compiled UPLC of the same function. Run
  * with: sbt 'scalusVerification/testOnly *BigIntVerificationTest'
  */
class BigIntVerificationTest extends AnyFunSuite with LeanProofsSuite {
    import proofs.*
    import RoundedMagnitude.half

    private def nonnegative = forAll[BigInt](x => half(x) >= BigInt(0))

    test("proof: doubling the result is between the magnitude and magnitude + 1") {
        proven(
          forAll[BigInt] { x =>
              val magnitude = if x < BigInt(0) then -x else x
              val r = half(x)
              magnitude <= BigInt(2) * r && BigInt(2) * r <= magnitude + BigInt(1)
          },
          160
        )
    }

    test("counterexample: doubling the result does not always recover a nonnegative input") {
        val counterexample = refuted(
          forAll[BigInt](x => (x >= BigInt(0)) ==> (BigInt(2) * half(x) == x)),
          160
        )
        // The solver chooses the value; check its meaning, rather than expecting a fixed one.
        val x = integer(counterexample.values.head)
        assert(x >= 0 && x % 2 == 1, counterexample)
        assert(BigInt(2) * half(x) != x)
        info(s"Counterexample: x = $x, half(x) = ${half(x)}")
    }

    test("no solutions: no integer input produces a negative result") {
        // Prove the universal statement, equivalently: not exists x. half(x) < 0.
        proven(nonnegative, 100)
    }

    test("inconclusive: too few evaluation steps cannot establish the same true property") {
        val reason = inconclusive(nonnegative, 3, 30.seconds)
        // A candidate from the bounded model passes CEK replay: it is not a real counterexample.
        assert(reason.contains("spurious"), reason)
        info(reason)
    }
}
