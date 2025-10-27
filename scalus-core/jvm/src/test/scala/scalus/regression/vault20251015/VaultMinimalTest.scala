package scalus.regression.vault20251015

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile

import scala.util.{Failure, Success, Try}

/** Regression test for Vault validator compilation
  *
  * This test verifies that a Validator that only implements the `spend` method can be compiled and
  * executed successfully with the inheritance via inline override feature.
  *
  * Issue: The compiler plugin adds RuntimeException stubs for unimplemented validator methods
  * (mint, reward, certify, vote, propose). This test ensures that:
  *   1. The validator compiles without errors
  *   2. The SIR can be lowered to UPLC
  */
class VaultMinimalTest extends AnyFunSuite:

    inline given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("VaultMinimal compiles and lowers to UPLC successfully") {
        val loweringResult = Try {
            val sir = compile(VaultMinimal.validate)
            val uplc = sir.toUplc()
            uplc
        }

        loweringResult match
            case Success(uplc) =>
                // Success! The validator compiled and lowered
                assert(uplc != null, "UPLC should not be null")
            case Failure(exception) =>
                fail(
                  s"VaultMinimal lowering failed (regression): ${exception.getClass.getSimpleName}: ${exception.getMessage}"
                )
    }
