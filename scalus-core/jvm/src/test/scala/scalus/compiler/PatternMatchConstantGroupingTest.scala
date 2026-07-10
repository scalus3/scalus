package scalus.compiler

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}
import scalus.uplc.*
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.PlutusVM
import scalus.toUplc

import scala.language.implicitConversions

/** Regression tests for audit finding M1 (docs/local/audits/M1.md): equal literal constants in
  * different cases must be grouped into one decision-tree branch, otherwise guard fallthrough and
  * multi-column constant rows select the wrong branch.
  */
class PatternMatchConstantGroupingTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV2VM()

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    test("guard fallthrough on repeated string constant selects the next same-constant case") {
        val compiled = compile { (x: String, flag: Boolean) =>
            x match
                case "a" if flag => BigInt(1)
                case "a"         => BigInt(2)
                case _           => BigInt(3)
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)

        assert((uplc $ "a".asTerm $ true.asTerm).evaluate == 1.asTerm)
        assert((uplc $ "a".asTerm $ false.asTerm).evaluate == 2.asTerm)
        assert((uplc $ "b".asTerm $ true.asTerm).evaluate == 3.asTerm)
    }

    test("multi-column constant rows sharing a first constant match the right row") {
        val compiled = compile { (x: (String, String)) =>
            x match
                case ("a", "x") => BigInt(1)
                case ("a", "y") => BigInt(2)
                case _          => BigInt(0)
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)

        val argAx = compile { ("a", "x") }.toUplc()
        val argAy = compile { ("a", "y") }.toUplc()
        val argAz = compile { ("a", "z") }.toUplc()
        val argBx = compile { ("b", "x") }.toUplc()
        assert((uplc $ argAx).evaluate == 1.asTerm)
        assert((uplc $ argAy).evaluate == 2.asTerm)
        assert((uplc $ argAz).evaluate == 0.asTerm)
        assert((uplc $ argBx).evaluate == 0.asTerm)
    }

    test("all-constant boolean match with guard falls through to the next same-constant case") {
        val compiled = compile { (x: Boolean, g: Boolean) =>
            x match
                case true if g => BigInt(1)
                case true      => BigInt(2)
                case false     => BigInt(3)
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)

        assert((uplc $ true.asTerm $ true.asTerm).evaluate == 1.asTerm)
        assert((uplc $ true.asTerm $ false.asTerm).evaluate == 2.asTerm)
        assert((uplc $ false.asTerm $ true.asTerm).evaluate == 3.asTerm)
    }
}
