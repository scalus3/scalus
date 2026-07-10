package scalus.compiler

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}
import scalus.uplc.*
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.Builtins
import scalus.uplc.eval.PlutusVM
import scalus.toUplc

import scala.language.implicitConversions

/** Audit finding X1 (docs/internal/UPLC_CORRECTNESS_AUDIT.md): a match whose decision tree never
  * inspects the scrutinee must still evaluate it, matching Scala's strict scrutinee semantics. The
  * scrutinee must be evaluated exactly once.
  */
class MatchScrutineeStrictnessTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV2VM()

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("wildcard-only match evaluates erroring scrutinee (V3 backend)") {
        val compiled = compile { (x: BigInt) =>
            (BigInt(1) / x) match
                case _ => BigInt(42)
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val zero = compile { BigInt(0) }.toUplc()
        val one = compile { BigInt(1) }.toUplc()
        assert((uplc $ one).evaluate == 42.asTerm)
        // Scala: 1/0 throws before the match — the script must fail
        assert((uplc $ zero).evaluateDebug.isFailure)
    }

    test("wildcard-only match emits scrutinee trace exactly once (V3 backend)") {
        val compiled = compile { (x: BigInt) =>
            Builtins.trace("scrutinee-evaluated")(x + BigInt(1)) match
                case _ => BigInt(42)
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val one = compile { BigInt(1) }.toUplc()
        val result = (uplc $ one).evaluateDebug
        assert(result.isSuccess)
        assert(result.logs.count(_.contains("scrutinee-evaluated")) == 1)
    }

    test("used scrutinee is evaluated exactly once, not per use (V3 backend)") {
        val compiled = compile { (x: BigInt) =>
            Builtins.trace("scrutinee-evaluated")(x + BigInt(1)) match
                case y if y > BigInt(10) => y + y
                case y                   => y
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val big = compile { BigInt(100) }.toUplc()
        val small = compile { BigInt(1) }.toUplc()
        val rBig = (uplc $ big).evaluateDebug
        assert(rBig.isSuccess)
        assert(rBig.logs.count(_.contains("scrutinee-evaluated")) == 1)
        val rSmall = (uplc $ small).evaluateDebug
        assert(rSmall.isSuccess)
        assert(rSmall.logs.count(_.contains("scrutinee-evaluated")) == 1)
    }

    test("tuple match with all-wildcard components evaluates erroring scrutinee (V3 backend)") {
        val compiled = compile { (x: BigInt) =>
            (BigInt(1) / x, BigInt(2)) match
                case (_, _) => BigInt(42)
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val zero = compile { BigInt(0) }.toUplc()
        val one = compile { BigInt(1) }.toUplc()
        assert((uplc $ one).evaluate == 42.asTerm)
        assert((uplc $ zero).evaluateDebug.isFailure)
    }

    test("wildcard-only match evaluates erroring scrutinee (SumOfProducts backend)") {
        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SumOfProductsLowering,
          generateErrorTraces = true,
          optimizeUplc = false,
          debug = false
        )
        val compiled = compile { (x: BigInt) =>
            (BigInt(1) / x) match
                case _ => BigInt(42)
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val zero = compile { BigInt(0) }.toUplc()
        val one = compile { BigInt(1) }.toUplc()
        assert((uplc $ one).evaluate == 42.asTerm)
        assert((uplc $ zero).evaluateDebug.isFailure)
    }

    test("wildcard-only match evaluates erroring scrutinee with optimizeUplc = true") {
        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = true,
          optimizeUplc = true,
          debug = false
        )
        val compiled = compile { (x: BigInt) =>
            (BigInt(1) / x) match
                case _ => BigInt(42)
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val zero = compile { BigInt(0) }.toUplc()
        assert((uplc $ zero).evaluateDebug.isFailure)
    }
}
