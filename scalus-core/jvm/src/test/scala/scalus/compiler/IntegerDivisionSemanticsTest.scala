package scalus.compiler

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}
import scalus.uplc.*
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.PlutusVM
import scalus.toUplc

import scala.language.implicitConversions

/** Audit finding E1 (docs/internal/UPLC_CORRECTNESS_AUDIT.md): `BigInt./` must have Scala semantics
  * on-chain.
  *
  * Scala's `BigInt./` truncates toward zero (= Plutus `quotientInteger`); Plutus `divideInteger`
  * floors toward negative infinity. The two differ whenever the operands have opposite signs. `%`
  * maps to `remainderInteger`, which already matches Scala — so `/` and `%` together must satisfy
  * the Euclidean identity `(a/b)*b + a%b == a` on-chain, like they do on the JVM.
  */
class IntegerDivisionSemanticsTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    private given PlutusVM = PlutusVM.makePlutusV2VM()

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    private lazy val divUplc = compile { (a: BigInt, b: BigInt) => a / b }.toUplc()
    private lazy val modUplc = compile { (a: BigInt, b: BigInt) => a % b }.toUplc()
    private lazy val euclidUplc = compile { (a: BigInt, b: BigInt) =>
        (a / b) * b + a % b
    }.toUplc()

    // All four sign combinations, exact division, and the audit's counterexample (-20, 3)
    private val cases: Seq[(BigInt, BigInt)] = Seq(
      (BigInt(7), BigInt(2)),
      (BigInt(-7), BigInt(2)), // floored: -4, Scala: -3
      (BigInt(7), BigInt(-2)), // floored: -4, Scala: -3
      (BigInt(-7), BigInt(-2)),
      (BigInt(-20), BigInt(3)), // audit E1: floored -7 vs Scala -6
      (BigInt(20), BigInt(-3)),
      (BigInt(-20), BigInt(4)), // exact division: both conventions agree
      (BigInt(0), BigInt(5))
    )

    test("BigInt./ matches Scala (truncated) division on-chain") {
        for (a, b) <- cases do
            assert(
              (divUplc $ a.asTerm $ b.asTerm).evaluate == (a / b).asTerm,
              s"$a / $b: expected ${a / b}"
            )
    }

    test("BigInt.% matches Scala remainder on-chain") {
        for (a, b) <- cases do
            assert(
              (modUplc $ a.asTerm $ b.asTerm).evaluate == (a % b).asTerm,
              s"$a % $b: expected ${a % b}"
            )
    }

    test("(a/b)*b + a%b == a holds on-chain (Euclidean identity)") {
        for (a, b) <- cases do
            assert(
              (euclidUplc $ a.asTerm $ b.asTerm).evaluate == a.asTerm,
              s"identity broken for ($a, $b)"
            )
    }

    test("BigInt./ and % match Scala for arbitrary operands (property)") {
        val operands = Gen.chooseNum(-1000L, 1000L).map(BigInt(_))
        forAll(operands, operands) { (a, b) =>
            whenever(b != 0) {
                assert((divUplc $ a.asTerm $ b.asTerm).evaluate == (a / b).asTerm)
                assert((modUplc $ a.asTerm $ b.asTerm).evaluate == (a % b).asTerm)
            }
        }
    }
}
