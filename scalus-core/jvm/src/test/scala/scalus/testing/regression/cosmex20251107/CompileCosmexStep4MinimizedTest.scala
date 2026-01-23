package scalus.testing.regression.cosmex20251107

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.ByteString
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}

class CompileCosmexStep4MinimizedTest extends AnyFunSuite {

    test("step 4 minimized - 96 lines but triggers different error (LoweringException)") {
        val key =
            ByteString.fromHex("aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899")

        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering
        )
        val compiledValidator = compile(minimized.CosmexContract.validate)

        val uplc = compiledValidator.toUplcOptimized().plutusV3

        assert(uplc != null, "Validator should compile successfully once bug is fixed")
    }
}
