package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.Options
import scalus.cardano.onchain.plutus.prelude.{Rational, RationalEq}
import scalus.uplc.PlutusV3
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.PlutusVM

/** `Rational` has no `Eq`: its equality is non-structural cross-multiplication. Value equality is
  * via `RationalEq.equals`; a reduced (lowest-terms) form via `.normalize`. `===` / `==` on
  * `Rational` is a compile error. See `docs/local/claude/compiler/v3-eq-eliminating.md`.
  */
class RationalEqLoweringTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()
    private given Options = Options.release.copy(noWarn = true)

    test("RationalEq.equals is cross-multiplication on-chain (1/2 == 2/4)") {
        val compiled = PlutusV3.compile {
            RationalEq.equals(Rational(BigInt(1), BigInt(2)), Rational(BigInt(2), BigInt(4)))
        }
        assert(compiled.program.term.evaluate == true.asTerm)
    }

    test("RationalEq.equals distinguishes non-equal fractions (1/2 != 1/3)") {
        val compiled = PlutusV3.compile {
            RationalEq.equals(Rational(BigInt(1), BigInt(2)), Rational(BigInt(1), BigInt(3)))
        }
        assert(compiled.program.term.evaluate == false.asTerm)
    }

    test("normalize reduces to lowest terms on-chain (2/4 -> 1/2)") {
        val numOk = PlutusV3.compile {
            Rational(BigInt(2), BigInt(4)).normalize.numerator == BigInt(1)
        }
        val denOk = PlutusV3.compile {
            Rational(BigInt(2), BigInt(4)).normalize.denominator == BigInt(2)
        }
        assert(numOk.program.term.evaluate == true.asTerm)
        assert(denOk.program.term.evaluate == true.asTerm)
    }

    test("normalize carries the sign on the numerator (1/-2 -> -1/2)") {
        val numOk = PlutusV3.compile {
            Rational(BigInt(1), BigInt(-2)).normalize.numerator == BigInt(-1)
        }
        val denOk = PlutusV3.compile {
            Rational(BigInt(1), BigInt(-2)).normalize.denominator == BigInt(2)
        }
        assert(numOk.program.term.evaluate == true.asTerm)
        assert(denOk.program.term.evaluate == true.asTerm)
    }

    // Note: `Rational === Rational` / `Rational == Rational` is a compile error (the
    // `compiletime.error` given for `===`, the plugin's `compileEquality` case for `==`). It can't
    // be asserted with `assertDoesNotCompile` because that only typechecks and doesn't run inline
    // expansion where `compiletime.error` fires. The enforcement is exercised by real compilation
    // (e.g. the converted call sites in v3/Contexts and the pricebet example).
}
