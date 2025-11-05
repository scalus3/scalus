package scalus.testing.regression.gr20251006

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Data
import scala.util.{Failure, Success, Try}

/** Regression test for Groth16 compilation issue
  *
  * This test verifies that the Groth16 implementation compiles and lowers to UPLC successfully.
  * Previously, it was failing with a type unification error during lowering, specifically in the
  * `derive` function which has nested pattern matching on List[ByteString] and List[BigInt] with
  * recursion.
  *
  * The error was: "Cannot unify result type of apply: Proxy(X) -> BLS12_381_G1_Element ->
  * BLS12_381_G1_Element and scalus.prelude.List[Int] -> BLS12_381_G1_Element ->
  * BLS12_381_G1_Element"
  */
class Groth16MinimalTest extends AnyFunSuite:

    given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    test("Groth16 minimal wrapper compiles and lowers to UPLC successfully") {
        // This is the minimal reproduction that triggers the bug
        val loweringResult = Try {
            val sir = compile((data: Data) =>
                Groth16Minimal.minimalWrapper(data.to[Groth16Minimal.MinimalData])
            )

            // Print SIR before lowering
            // println("=== SIR before lowering ===")
            // println(sir.pretty.render(120))
            // println("=== End SIR ===")

            val uplc = sir.toUplc()
            uplc
        }

        loweringResult match
            case Success(uplc) =>
                // Success! The bug is fixed
                assert(uplc != null, "UPLC should not be null")
            case Failure(exception) =>
                // Print the error details for debugging
                println(s"=== Groth16 lowering failed ===")
                println(s"Error type: ${exception.getClass.getSimpleName}")
                println(s"Error message: ${exception.getMessage}")
                exception.printStackTrace()
                fail(
                  s"Groth16 lowering failed (regression): ${exception.getClass.getSimpleName}: ${exception.getMessage}"
                )
    }
