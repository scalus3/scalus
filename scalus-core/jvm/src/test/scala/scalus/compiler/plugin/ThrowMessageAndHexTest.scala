package scalus.compiler.plugin

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.{compile, Options}
import scalus.compiler.sir.TargetLoweringBackend
import scalus.toUplc
import scalus.uplc.*
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.PlutusVM

import scala.language.implicitConversions

/** Custom exception whose first constructor argument is NOT a String (audit finding E10). */
class CodeException(code: BigInt) extends RuntimeException

/** Audit findings E9 and E10.
  *
  * E9: `ByteString.fromHex(<invalid literal>)` must report a positioned compile error, not crash
  * the compiler with an uncaught exception (the `hex"…"` interpolator path already does this).
  *
  * E10: `throw new Exc(arg, …)` used the first constructor argument as the error message regardless
  * of its type, producing an ill-typed (non-String) message SIR. It must only use `arg` when it is
  * a String; otherwise fall back to the expression text.
  */
class ThrowMessageAndHexTest extends AnyFunSuite with SnippetCompilation {

    private given PlutusVM = PlutusVM.makePlutusV2VM()

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    // E9
    test("ByteString.fromHex with an invalid hex literal is a positioned error, not a crash") {
        val errors = compileSnippet(
          """import scalus.compiler.Compile
            |import scalus.uplc.builtin.ByteString
            |@Compile
            |object M {
            |    def bs: ByteString = ByteString.fromHex("nothex!")
            |}
            |""".stripMargin
        )
        assert(
          errors.exists(_.contains("not a valid hex string")),
          s"expected a positioned hex error, got: $errors"
        )
    }

    // E10
    test("throw with a non-String first constructor argument lowers cleanly") {
        val compiled = compile { (x: BigInt) =>
            if x == BigInt(0) then throw new CodeException(x) else x
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        assert((uplc $ 1.asTerm).evaluate == 1.asTerm) // x != 0 → x
        assert((uplc $ 0.asTerm).evaluateDebug.isFailure) // x == 0 → throw → error
    }
}
