package scalus.testing.regression.cosmex20251107

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.ByteString
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}
import scalus.cardano.onchain.plutus.v3.*

/** Regression test for cosmex shadowing bug.
  *
  * This test should:
  *   - FAIL when the bug exists (compilation throws CaclulateApplyTypeException)
  *   - PASS when the bug is fixed (compilation succeeds)
  *
  * Current status: FAILS because the bug still exists.
  */
class CompileCosmexFullTest extends AnyFunSuite {

    test("compile full cosmex contract - should succeed when bug is fixed") {
        val key =
            ByteString.fromHex("aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899")

        val exchangeParams = full.ExchangeParams(
          exchangePkh = PubKeyHash(key),
          contestationPeriodInMilliseconds = BigInt(5000),
          exchangePubKey = key
        )

        // This WILL throw CaclulateApplyTypeException while the bug exists
        // causing this test to FAIL
        // Once the bug is fixed, this will compile successfully and test will PASS
        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering
        )
        val compiledValidator = compile(full.CosmexContract.validate)
        val uplc = compiledValidator.toUplcOptimized().plutusV3

        assert(uplc != null, "Validator should compile successfully once bug is fixed")
    }
}
