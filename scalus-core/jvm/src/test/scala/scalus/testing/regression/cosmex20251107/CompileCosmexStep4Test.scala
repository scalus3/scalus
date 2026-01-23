package scalus.testing.regression.cosmex20251107

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.ByteString
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}
import scalus.cardano.onchain.plutus.v3.*

class CompileCosmexStep4Test extends AnyFunSuite {

    test("step 4 - Action enum with Close case containing SignedSnapshot") {
        val key =
            ByteString.fromHex("aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899")

        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering
        )
        val compiledValidator = compile(step4.CosmexContract.validate)

        // println("=== SIR OUTPUT ===")
        // println(compiledValidator.show)
        // println("=== END SIR OUTPUT ===")

        val uplc = compiledValidator.toUplcOptimized().plutusV3

        assert(uplc != null, "Validator should compile successfully once bug is fixed")
    }
}
