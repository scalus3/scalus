package scalus.testing.regression.cosmex20251107

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.ByteString

class CompileCosmexStep4MinimizedTest extends AnyFunSuite {

    test("step 4 minimized - 96 lines but triggers different error (LoweringException)") {
        val key = ByteString.fromHex("aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899")
        
        given Compiler.Options = Compiler.Options(
          targetLoweringBackend = Compiler.TargetLoweringBackend.SirToUplcV3Lowering
        )
        val compiledValidator = Compiler.compile(minimized.CosmexContract.validate)
        val uplc = compiledValidator.toUplcOptimized().plutusV3
        
        assert(uplc != null, "Validator should compile successfully once bug is fixed")
    }
}
