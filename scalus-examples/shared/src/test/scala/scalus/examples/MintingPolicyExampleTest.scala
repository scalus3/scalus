package scalus
package examples

import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data
import scalus.compiler.compile
import scalus.cardano.onchain.plutus.v1.*
import scalus.cardano.onchain.plutus.v2
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.Option.*
import scalus.testing.assertions.Expected
import scalus.testing.kit.BaseValidatorTest
import scalus.uplc.TermDSL.given
import scalus.uplc.*

import scala.language.implicitConversions

class MintingPolicyExampleTest extends BaseValidatorTest {

    private def scriptContextV1(
        txInfoInputs: scalus.cardano.onchain.plutus.prelude.List[TxInInfo],
        value: Value
    ) =
        ScriptContext(
          TxInfo(
            inputs = txInfoInputs,
            outputs = scalus.cardano.onchain.plutus.prelude.List.Nil,
            fee = Value.lovelace(BigInt("188021")),
            mint = value,
            dcert = scalus.cardano.onchain.plutus.prelude.List.Nil,
            withdrawals = scalus.cardano.onchain.plutus.prelude.List.Nil,
            validRange = Interval.always,
            signatories = scalus.cardano.onchain.plutus.prelude.List.Nil,
            data = scalus.cardano.onchain.plutus.prelude.List.Nil,
            id = TxId(hex"1e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a7bf61fe9")
          ),
          ScriptPurpose.Minting(hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235")
        )

    private def scriptContextV2(
        txInfoInputs: scalus.cardano.onchain.plutus.prelude.List[v2.TxInInfo],
        value: Value
    ) =
        v2.ScriptContext(
          v2.TxInfo(
            inputs = txInfoInputs,
            referenceInputs = scalus.cardano.onchain.plutus.prelude.List.Nil,
            outputs = scalus.cardano.onchain.plutus.prelude.List.Nil,
            fee = Value.lovelace(BigInt("188021")),
            mint = value,
            dcert = scalus.cardano.onchain.plutus.prelude.List.Nil,
            withdrawals = SortedMap.empty,
            validRange = Interval.always,
            signatories = scalus.cardano.onchain.plutus.prelude.List.Nil,
            redeemers = SortedMap.empty,
            data = SortedMap.empty,
            id = TxId(hex"1e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a7bf61fe9")
          ),
          ScriptPurpose.Minting(hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235")
        )

    def withScriptContextV1(
        validator: Term,
        txInfoInputs: scalus.cardano.onchain.plutus.prelude.List[TxInInfo],
        value: Value
    ) =
        import Data.toData
        Program.plutusV1(validator $ () $ scriptContextV1(txInfoInputs, value).toData)

    def withScriptContextV2(
        validator: Term,
        txInfoInputs: scalus.cardano.onchain.plutus.prelude.List[TxInInfo],
        value: Value
    ) =
        import Data.toData
        val txInfoInputsV2 = txInfoInputs.map { case TxInInfo(txOutRef, txOut) =>
            val txOutV2 =
                v2.TxOut(txOut.address, txOut.value, v2.OutputDatum.NoOutputDatum, None)
            v2.TxInInfo(txOutRef, txOutV2)
        }
        Program.plutusV2(validator $ () $ scriptContextV2(txInfoInputsV2, value).toData)

    private def performMintingPolicyValidatorChecks(
        validator: Term
    )(
        withScriptContext: (
            Term,
            scalus.cardano.onchain.plutus.prelude.List[TxInInfo],
            Value
        ) => Program
    ) = {
        // The minting policy script should succeed when the TxOutRef is spent and the minted tokens are correct
        assertEvalResult(Expected.success(()))(
          withScriptContext(
            validator,
            List(TxInInfo(hoskyMintTxOutRef, hoskyMintTxOut)),
            Value(
              hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235",
              hex"484f534b59",
              BigInt("1000000000000000")
            )
          )
        )
        // Successful burn, the minted tokens are negative and TxOutRef is not spent
        assertEvalResult(Expected.success(()))(
          withScriptContext(
            validator,
            List.empty,
            Value(
              hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235",
              hex"484f534b59",
              BigInt(-100)
            )
          )
        )

        assertEvalResult(Expected.failure)(
          withScriptContext(
            validator,
            List(TxInInfo(hoskyMintTxOutRef, hoskyMintTxOut)),
            Value(hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235", hex"484f534b59", 2)
          )
        )

        assertEvalResult(Expected.failure)(
          withScriptContext(
            validator,
            List(TxInInfo(hoskyMintTxOutRef, hoskyMintTxOut)),
            Value(hex"cc", hex"484f534b59", BigInt("1000000000000000"))
          )
        )

        assertEvalResult(Expected.failure)(
          withScriptContext(
            validator,
            List(TxInInfo(hoskyMintTxOutRef, hoskyMintTxOut)),
            Value(
              hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235",
              hex"deadbeef",
              BigInt("1000000000000000")
            )
          )
        )

        assertEvalResult(Expected.failure)(
          withScriptContext(
            validator,
            List.empty,
            Value(
              hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235",
              hex"484f534b59",
              BigInt("1000000000000000")
            )
          )
        )

        assertEvalResult(Expected.failure)(
          withScriptContext(
            validator,
            List(TxInInfo(hoskyMintTxOutRef, hoskyMintTxOut)),
            Value(
              hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235",
              hex"484f534b59",
              BigInt("1000000000000000")
            ) +
                Value(
                  hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235",
                  hex"deadbeef",
                  BigInt("1000000000000000")
                )
          )
        )
    }

    val evaledTokens =
        val tokensSIR =
            compile(SortedMap.singleton(hex"484f534b59", BigInt("1000000000000000")))
        tokensSIR.toUplc().evaluate

    test("Minting Policy Validator") {
        // Import the V3 backend configuration from MintingPolicy
        import MintingPolicy.given scalus.Compiler.Options

        // Compile evaledTokens with the same V3 backend
        val evaledTokensV3 =
            val tokensSIR =
                compile(SortedMap.singleton(hex"484f534b59", BigInt("1000000000000000")))
            tokensSIR.toUplc().evaluate

        val sir = MintingPolicy.compiledMintingPolicyScript

        val validator = sir.toUplc(
          generateErrorTraces = true,
          optimizeUplc = false
        )
        val appliedValidator =
            validator $ hoskyMintTxOutRef.id.hash $ hoskyMintTxOutRef.idx $ evaledTokensV3
        val flatSize = Program.plutusV1(appliedValidator).flatEncoded.length
        assert(flatSize == 1232)
        performMintingPolicyValidatorChecks(appliedValidator)(withScriptContextV1)

    }

    test("Minting Policy Validator V2") {
        val validator =
            MintingPolicyV2.compiledMintingPolicyScriptV2.toUplc(
              generateErrorTraces = true,
              optimizeUplc = false
            )
        val appliedValidator =
            validator $ hoskyMintTxOutRef.id.hash $ hoskyMintTxOutRef.idx $ evaledTokens
        val flatSize = Program.plutusV2(appliedValidator).flatEncoded.length
        assert(flatSize == 1234)
        performMintingPolicyValidatorChecks(appliedValidator)(withScriptContextV2)
    }

    test("Minting Policy Validator Optimized") {
        // Import SimpleSirToUplcLowering backend from MintingPolicySimpleBackend
        import MintingPolicySimpleBackend.given scalus.Compiler.Options

        // Compile evaledTokens with the same SimpleSirToUplcLowering backend
        val evaledTokensSimple =
            val tokensSIR =
                compile(SortedMap.singleton(hex"484f534b59", BigInt("1000000000000000")))
            tokensSIR.toUplc().evaluate

        // println(MintingPolicySimpleBackend.compiledOptimizedMintingPolicyScript.pretty.render(100))
        val validator =
            MintingPolicySimpleBackend.compiledOptimizedMintingPolicyScript.toUplc(
              generateErrorTraces = true,
              optimizeUplc = false
            )
        // println(s"Validator UPLC:\n${validator.pretty.render(100)}")
        val appliedValidator =
            validator $ hoskyMintTxOutRef.id.hash $ hoskyMintTxOutRef.idx $ evaledTokensSimple
        // println(s"Applied Validator UPLC:\n${appliedValidator.pretty.render(100)}")
        val flatSize = Program.plutusV1(appliedValidator).flatEncoded.length
        // println(s"Flat size: $flatSize")
        // assert(flatSize == 846)
        //    Re-enable when lazy let will be supported on SimpleSirToUplcLowering (issue #125)
        //    In addition to that, we should optimize redurant beta-reductions in UPLC
        //    (i.e.  (App (lam x x) y)  => y )
        //    Not sure, if it exists in current UPLC optimization passes, but in this test
        //    it produces larger code size than non-optimized version.
        //    (TODO: alows to disable/enable specific UPLC optimizations from Compiler.Options)
        assert(flatSize == 857)
        performMintingPolicyValidatorChecks(appliedValidator)(withScriptContextV1)
    }
}
