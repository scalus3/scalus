package scalus.testing.regression.cosmex20251107

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.ByteString
import scalus.ledger.api.v3.*

class CompileCosmexStep4Test extends AnyFunSuite {

    test("step 4 - Action enum with Close case containing SignedSnapshot") {
        val key = ByteString.fromHex("aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899")
        
        given Compiler.Options = Compiler.Options(
          targetLoweringBackend = Compiler.TargetLoweringBackend.SirToUplcV3Lowering
        )
        val compiledValidator = Compiler.compile(step4.CosmexContract.validate)
        val uplc = compiledValidator.toUplcOptimized().plutusV3
        
        assert(uplc != null, "Validator should compile successfully once bug is fixed")
    }
}
