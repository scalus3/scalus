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

/** Audit finding L3 (docs/internal/UPLC_CORRECTNESS_AUDIT.md): when a function-typed value needs
  * representation alignment, the synthesized eta-expansion `λx. arg (convert x)` must not embed a
  * non-trivial `arg` term inside the wrapper lambda — UPLC is call-by-value and Scala evaluates the
  * expression exactly once, so re-evaluating `arg` per invocation duplicates its effects (traces,
  * errors, budget) and defers them from the application site to the first call.
  */
class RepresentationAlignmentStrictnessTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV2VM()

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("aligned higher-order argument is evaluated exactly once, not per call") {
        val compiled = compile { (x: BigInt) =>
            def mk(d: BigInt): BigInt => BigInt =
                Builtins.trace("mk-evaluated")((y: BigInt) => y + d)
            def callTwice[A](f: A => A, a: A): A = f(f(a))
            callTwice(mk(x), x)
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val ten = compile { BigInt(10) }.toUplc()
        assert((uplc $ ten).evaluate == 30.asTerm)
        val result = (uplc $ ten).evaluateDebug
        assert(result.isSuccess)
        assert(result.logs.count(_.contains("mk-evaluated")) == 1)
    }

    test("aligned higher-order argument evaluates its errors at the application site") {
        val compiled = compile { (x: BigInt) =>
            def mk(d: BigInt): BigInt => BigInt =
                if d == BigInt(0) then throw new RuntimeException("boom")
                else (y: BigInt) => y + d
            // f is never called: Scala still evaluates mk(x) eagerly at the call.
            def ignoreF[A](f: A => A, a: A): A = a
            ignoreF(mk(x), x)
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val zero = compile { BigInt(0) }.toUplc()
        val one = compile { BigInt(1) }.toUplc()
        assert((uplc $ one).evaluateDebug.isSuccess)
        // Scala: mk(0) throws before ignoreF — the script must fail
        assert((uplc $ zero).evaluateDebug.isFailure)
    }
}
