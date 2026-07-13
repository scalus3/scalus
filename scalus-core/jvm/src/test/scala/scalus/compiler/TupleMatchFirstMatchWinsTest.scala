package scalus.compiler

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.compile
import scalus.uplc.*
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.PlutusVM

import scala.language.implicitConversions

/** A tuple pattern over sum-type components produces a row with a TypeSelector pattern in every
  * such column. `eliminateTypeSelectorsInRaw` used to expand that row into one partially-unrolled
  * copy per column; the copy whose selector column stayed un-unrolled fell into the default group
  * and lost the constraint once the unrolled column was specialized away. Consequence: a later
  * wildcard case could be shadowed by an earlier case whose first-column constraint had already
  * failed — silent first-match-wins violation.
  */
class TupleMatchFirstMatchWinsTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    test("tuple match with constructor patterns in both columns follows first-match-wins") {
        import scalus.cardano.onchain.plutus.prelude.*
        val sir = compile { (sx: BigInt, sy: BigInt) =>
            val x: These[BigInt, BigInt] =
                if sx == BigInt(0) then These.This(BigInt(7)) else These.That(BigInt(8))
            val y: These[BigInt, BigInt] =
                if sy == BigInt(0) then These.This(BigInt(9)) else These.That(BigInt(100))
            (x, y) match
                case (These.This(_), These.This(v)) => v
                case (_, These.That(b))             => b
                case _                              => BigInt(0)
        }
        val uplc = sir.toUplc()
        assert((uplc $ 0.asTerm $ 0.asTerm).evaluate == 9.asTerm)
        assert((uplc $ 0.asTerm $ 1.asTerm).evaluate == 100.asTerm)
        assert((uplc $ 1.asTerm $ 1.asTerm).evaluate == 100.asTerm)
        // (That(8), This(9)): rows 1 and 2 fail, the wildcard row must win —
        // the broken expansion returned 9 here (row 1's action without its
        // first-column constraint)
        assert((uplc $ 1.asTerm $ 0.asTerm).evaluate == 0.asTerm)
    }
}
