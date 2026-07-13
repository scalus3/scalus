package scalus.compiler

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.compile
import scalus.uplc.*
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.PlutusVM

import scala.language.implicitConversions

/** Audit findings M2/M3 (compiler plugin audit) on ByReference embedding of pattern-match actions
  * and guards.
  *
  * M2: the Inline/ByReference embedding heuristic multiplied the action size by the action's
  * *index* `i` in `parsedActions` instead of its usage *count*, so the action of the first case
  * (index 0) was always inlined regardless of size and reuse, duplicating large shared actions
  * (e.g. from or-patterns) in the generated SIR.
  *
  * M3: a ByReference action/guard was let-bound as its raw body under a function type (no lambda
  * wrapping), so applying the reference at a leaf crashed lowering with "Cannot upcast T to Unit ->
  * T"; and the generated applications used the action's original binder names instead of the leaf's
  * column bindings.
  */
class PatternActionEmbeddingTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    test("big shared or-pattern action at case index 0 is embedded once, not duplicated") {
        import scalus.cardano.onchain.plutus.prelude.*
        val sir = compile { (x: These[BigInt, BigInt]) =>
            x match
                case These.This(_) | These.That(_) =>
                    BigInt(31337) + BigInt(1) + BigInt(2) + BigInt(3) + BigInt(4) + BigInt(5)
                case These.These(a, b) => a + b
        }
        val rendered = sir.pretty.render(120)
        val occurrences = "31337".r.findAllIn(rendered).size
        assert(
          occurrences == 1,
          s"expected the shared action to appear once (by reference), found it $occurrences times"
        )
    }

    test("big shared or-pattern action without binders lowers to UPLC and evaluates") {
        import scalus.cardano.onchain.plutus.prelude.*
        val sir = compile { (sel: BigInt) =>
            val v: These[BigInt, BigInt] =
                if sel == BigInt(0) then These.This(BigInt(7))
                else if sel == BigInt(1) then These.That(BigInt(8))
                else These.These(BigInt(2), BigInt(3))
            v match
                case These.This(_) | These.That(_) =>
                    BigInt(31337) + BigInt(1) + BigInt(2) + BigInt(3) + BigInt(4) + BigInt(5)
                case These.These(a, b) => a + b
        }
        assert(sir.pretty.render(120).contains("caseAction"), "expected a ByReference action let")
        val uplc = sir.toUplc()
        assert((uplc $ 0.asTerm).evaluate == 31352.asTerm)
        assert((uplc $ 1.asTerm).evaluate == 31352.asTerm)
        assert((uplc $ 2.asTerm).evaluate == 5.asTerm)
    }

    test("big shared action with binders reached from different leaves evaluates correctly") {
        import scalus.cardano.onchain.plutus.prelude.*
        // The second case's wildcard row is duplicated into the This-group and the default
        // group of the first column, so its action (with binder b) is used from >= 2 leaves.
        val sir = compile { (sx: BigInt, sy: BigInt) =>
            val x: These[BigInt, BigInt] =
                if sx == BigInt(0) then These.This(BigInt(7)) else These.That(BigInt(8))
            val y: These[BigInt, BigInt] =
                if sy == BigInt(0) then These.This(BigInt(9)) else These.That(BigInt(100))
            (x, y) match
                case (These.This(_), These.This(v)) => v
                case (_, These.That(b)) => b + BigInt(31337) + BigInt(1) + BigInt(2) + BigInt(3)
                case _                  => BigInt(0)
        }
        val rendered = sir.pretty.render(120)
        val occurrences = "31337".r.findAllIn(rendered).size
        assert(
          occurrences == 1,
          s"expected the shared action to appear once (by reference), found it $occurrences times"
        )
        val uplc = sir.toUplc()
        assert((uplc $ 0.asTerm $ 0.asTerm).evaluate == 9.asTerm)
        assert((uplc $ 0.asTerm $ 1.asTerm).evaluate == 31443.asTerm)
        assert((uplc $ 1.asTerm $ 1.asTerm).evaluate == 31443.asTerm)
        assert((uplc $ 1.asTerm $ 0.asTerm).evaluate == 0.asTerm)
    }

    test("big or-pattern action with binders is applied with the leaf's column bindings") {
        import scalus.cardano.onchain.plutus.prelude.*
        // The or-pattern in the first column duplicates the row, so the action (with
        // binder b in the second column) is used from two leaves in different subtrees,
        // each with its own column names for b.
        val sir = compile { (sx: BigInt, sy: BigInt) =>
            val x: These[BigInt, BigInt] =
                if sx == BigInt(0) then These.This(BigInt(7)) else These.That(BigInt(8))
            val y: These[BigInt, BigInt] =
                if sy == BigInt(0) then These.This(BigInt(9)) else These.That(BigInt(100))
            (x, y) match
                case (These.This(_) | These.That(_), These.That(b)) =>
                    b + BigInt(31337) + BigInt(1) + BigInt(2) + BigInt(3)
                case _ => BigInt(0)
        }
        val rendered = sir.pretty.render(120)
        assert(rendered.contains("caseAction"), "expected a ByReference action let")
        val occurrences = "31337".r.findAllIn(rendered).size
        assert(
          occurrences == 1,
          s"expected the shared action to appear once (by reference), found it $occurrences times"
        )
        val uplc = sir.toUplc()
        assert((uplc $ 0.asTerm $ 1.asTerm).evaluate == 31443.asTerm)
        assert((uplc $ 1.asTerm $ 1.asTerm).evaluate == 31443.asTerm)
        assert((uplc $ 0.asTerm $ 0.asTerm).evaluate == 0.asTerm)
        assert((uplc $ 1.asTerm $ 0.asTerm).evaluate == 0.asTerm)
    }

    test("big or-pattern guard with binders is applied with the leaf's column bindings") {
        import scalus.cardano.onchain.plutus.prelude.*
        val sir = compile { (sx: BigInt, sy: BigInt) =>
            val x: These[BigInt, BigInt] =
                if sx == BigInt(0) then These.This(BigInt(7)) else These.That(BigInt(8))
            val y: These[BigInt, BigInt] =
                if sy == BigInt(0) then These.This(BigInt(9))
                else if sy == BigInt(1) then These.That(BigInt(100))
                else These.That(BigInt(1))
            (x, y) match
                case (These.This(_) | These.That(_), These.That(b))
                    if b + BigInt(31337) + BigInt(1) + BigInt(2) > BigInt(31400) =>
                    b
                case _ => BigInt(0)
        }
        val rendered = sir.pretty.render(120)
        assert(rendered.contains("caseGuard"), "expected a ByReference guard let")
        val uplc = sir.toUplc()
        // guard true, through both or-pattern leaves
        assert((uplc $ 0.asTerm $ 1.asTerm).evaluate == 100.asTerm)
        assert((uplc $ 1.asTerm $ 1.asTerm).evaluate == 100.asTerm)
        // guard false: falls through to the wildcard case
        assert((uplc $ 0.asTerm $ 2.asTerm).evaluate == 0.asTerm)
        assert((uplc $ 1.asTerm $ 2.asTerm).evaluate == 0.asTerm)
        assert((uplc $ 0.asTerm $ 0.asTerm).evaluate == 0.asTerm)
    }
}
