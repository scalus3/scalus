package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data}
import scalus.compiler.{compile, Options}
import scalus.compiler.sir.TargetLoweringBackend
import scalus.cardano.onchain.plutus.v1.{PubKeyHash, TxId, TxOutRef}
import scalus.cardano.onchain.plutus.v2.{Interval, ScriptContext, ScriptPurpose, TxInfo, Value}
import scalus.cardano.onchain.plutus.prelude.List
import scalus.uplc.*
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.*

/** Tests that validators compile and evaluate correctly with nativeListElements=true.
  *
  * Re-lowers existing compiled validators with nativeListElements enabled and verifies they produce
  * the same results as standard compilation.
  */
class NativeListElementsTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    private val standardOpts = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true
    )

    private val nativeOpts = standardOpts.copy(nativeListElements = true)

    /** Compile SIR with both standard and native options, apply args, and verify both succeed or
      * both fail with consistent results.
      */
    private def assertBothModes(
        sir: scalus.compiler.sir.SIR,
        args: Seq[Term],
        expectSuccess: Boolean
    ): Unit = {
        Seq(standardOpts -> "standard", nativeOpts -> "native").foreach { pair =>
            val opts = pair._1
            val label = pair._2
            val term = sir.toUplc(using opts)()
            val applied = args.foldLeft(term)((t, arg) => Term.Apply(t, arg))
            applied.evaluateDebug match
                case Result.Success(_, budget, _, _) =>
                    assert(
                      expectSuccess,
                      s"[$label] Expected failure but got success with budget=$budget"
                    )
                    info(s"[$label] OK, budget=$budget")
                case Result.Failure(ex, budget, _, _) =>
                    assert(
                      !expectSuccess,
                      s"[$label] Expected success but got failure: $ex"
                    )
                    info(s"[$label] Failed as expected, budget=$budget")
        }
    }

    private val hoskyMintTxOutRef = TxOutRef(
      TxId(hex"1e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a7bf61fe9"),
      0
    )

    private def scriptContext(
        signatories: scalus.cardano.onchain.plutus.prelude.List[PubKeyHash]
    ) =
        ScriptContext(
          TxInfo(
            inputs = List.Nil,
            referenceInputs = List.Nil,
            outputs = List.Nil,
            fee = Value.lovelace(BigInt("188021")),
            mint = Value.lovelace(BigInt("188021")),
            dcert = List.Nil,
            withdrawals = scalus.cardano.onchain.plutus.prelude.SortedMap.empty,
            validRange = Interval.always,
            signatories = signatories,
            redeemers = scalus.cardano.onchain.plutus.prelude.SortedMap.empty,
            data = scalus.cardano.onchain.plutus.prelude.SortedMap.empty,
            id = TxId(hex"1e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a7bf61fe9")
          ),
          ScriptPurpose.Spending(hoskyMintTxOutRef)
        )

    test("PreimageValidator with nativeListElements") {
        val sir = compile(PreimageValidator.preimageValidator)

        val pkh = PubKeyHash(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6")
        val hash = hex"36c71edaf0affadf6dd9c5f8df3dc90ec0fc01bf9f8cf0f18268db24b2a3da49"
        val preimage = ByteString.fromArray("Scalus rocks!".getBytes("UTF-8"))
        val ctx = scriptContext(List(pkh))

        // Should succeed with correct preimage and signatory
        assertBothModes(
          sir,
          Seq(
            (hash, pkh).toData.asTerm,
            preimage.toData.asTerm,
            ctx.toData.asTerm
          ),
          expectSuccess = true
        )

        // Should fail with wrong hash
        assertBothModes(
          sir,
          Seq(
            (hex"000000": ByteString, pkh).toData.asTerm,
            preimage.toData.asTerm,
            ctx.toData.asTerm
          ),
          expectSuccess = false
        )
    }

}
