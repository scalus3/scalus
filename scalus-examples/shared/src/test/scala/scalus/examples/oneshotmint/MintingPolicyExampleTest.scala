package scalus
package examples
package oneshotmint

import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data
import scalus.compiler.compile
import scalus.cardano.onchain.plutus.v1.*
import scalus.cardano.onchain.plutus.v2
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.Option.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.testing.kit.ScalusTest
import scalus.uplc.eval.PlutusVM
import scalus.uplc.TermDSL.given
import scalus.uplc.*

import scala.language.implicitConversions

class MintingPolicyExampleTest extends AnyFunSuite, ScalusTest {

    // Plutus V2 scripts: evaluated on the V2 machine, as BaseValidatorTest did.
    override protected def plutusVM: PlutusVM = PlutusVM.makePlutusV2VM()

    private val hoskyMintTxOutRef = TxOutRef(
      TxId(hex"1ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982"),
      0
    )

    private val hoskyMintTxOut = TxOut(
      address = Address(
        Credential.PubKeyCredential(
          PubKeyHash(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6")
        ),
        None
      ),
      Value.lovelace(BigInt("10000000")),
      None
    )

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
            redeemers = AssocMap.empty,
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
        assertSuccess(
          withScriptContext(
            validator,
            List(TxInInfo(hoskyMintTxOutRef, hoskyMintTxOut)),
            Value(
              hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235",
              hex"484f534b59",
              BigInt("1000000000000000")
            )
          ).evaluateDebug
        )
        // Successful burn, the minted tokens are negative and TxOutRef is not spent
        assertSuccess(
          withScriptContext(
            validator,
            List.empty,
            Value(
              hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235",
              hex"484f534b59",
              BigInt(-100)
            )
          ).evaluateDebug
        )

        assertFailure(
          withScriptContext(
            validator,
            List(TxInInfo(hoskyMintTxOutRef, hoskyMintTxOut)),
            Value(hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235", hex"484f534b59", 2)
          ).evaluateDebug
        )

        assertFailure(
          withScriptContext(
            validator,
            List(TxInInfo(hoskyMintTxOutRef, hoskyMintTxOut)),
            Value(hex"cc", hex"484f534b59", BigInt("1000000000000000"))
          ).evaluateDebug
        )

        assertFailure(
          withScriptContext(
            validator,
            List(TxInInfo(hoskyMintTxOutRef, hoskyMintTxOut)),
            Value(
              hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235",
              hex"deadbeef",
              BigInt("1000000000000000")
            )
          ).evaluateDebug
        )

        assertFailure(
          withScriptContext(
            validator,
            List.empty,
            Value(
              hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235",
              hex"484f534b59",
              BigInt("1000000000000000")
            )
          ).evaluateDebug
        )

        assertFailure(
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
          ).evaluateDebug
        )
    }

    val evaledTokens =
        val tokensSIR =
            compile(SortedMap.singleton(hex"484f534b59", BigInt("1000000000000000")))
        tokensSIR.toUplc().evaluate

    test("Minting Policy Validator") {
        val validator = MintingPolicy.compiledMintingPolicyScript.toUplc(
          generateErrorTraces = true,
          optimizeUplc = true
        )
        val appliedValidator =
            validator $ hoskyMintTxOutRef.id.hash $ hoskyMintTxOutRef.idx $ evaledTokens
        val flatSize = Program.plutusV1(appliedValidator).flatEncoded.length
        assert(flatSize == 649)
        performMintingPolicyValidatorChecks(appliedValidator)(withScriptContextV1)
    }

    test("Minting Policy Validator V2") {
        val validator =
            MintingPolicyV2.compiledMintingPolicyScriptV2.toUplc(
              generateErrorTraces = true,
              optimizeUplc = true
            )
        val appliedValidator =
            validator $ hoskyMintTxOutRef.id.hash $ hoskyMintTxOutRef.idx $ evaledTokens
        val flatSize = Program.plutusV2(appliedValidator).flatEncoded.length
        assert(flatSize == 649)
        performMintingPolicyValidatorChecks(appliedValidator)(withScriptContextV2)
    }

    test("Minting Policy Validator Optimized") {
        // Import the ScottEncodingLowering backend from MintingPolicySimpleBackend
        import MintingPolicySimpleBackend.given scalus.compiler.Options

        // Compile the tokens argument with the same ScottEncodingLowering backend so its
        // runtime representation matches the validator's
        val evaledTokensScott =
            val tokensSIR =
                compile(SortedMap.singleton(hex"484f534b59", BigInt("1000000000000000")))
            tokensSIR.toUplc().evaluate

        val validator =
            MintingPolicySimpleBackend.compiledOptimizedMintingPolicyScript.toUplc(
              generateErrorTraces = true,
              optimizeUplc = true
            )
        val appliedValidator =
            validator $ hoskyMintTxOutRef.id.hash $ hoskyMintTxOutRef.idx $ evaledTokensScott
        val flatSize = Program.plutusV1(appliedValidator).flatEncoded.length
        // 687 (was 672 with the Z combinator): the self-application recursion
        // encoding adds one Var per recursive call site; execution is cheaper.
        assert(flatSize == 653)
        performMintingPolicyValidatorChecks(appliedValidator)(withScriptContextV1)
    }
}
