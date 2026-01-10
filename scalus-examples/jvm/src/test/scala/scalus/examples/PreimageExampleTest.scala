package scalus
package examples

import scalus.builtin.ByteString.*
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}
import scalus.ledger.api.v1
import scalus.ledger.api.v1.{PubKeyHash, TxId}
import scalus.ledger.api.v2.*
import scalus.prelude.List
import scalus.uplc.*
import scalus.uplc.Term.*

class PreimageExampleTest extends BaseValidatorTest {

    private def scriptContext(signatories: scalus.prelude.List[PubKeyHash]) =
        ScriptContext(
          TxInfo(
            inputs = scalus.prelude.List.Nil,
            referenceInputs = scalus.prelude.List.Nil,
            outputs = scalus.prelude.List.Nil,
            fee = Value.lovelace(BigInt("188021")),
            mint = Value.lovelace(BigInt("188021")),
            dcert = scalus.prelude.List.Nil,
            withdrawals = scalus.prelude.SortedMap.empty,
            validRange = Interval.always,
            signatories = signatories,
            redeemers = scalus.prelude.SortedMap.empty,
            data = scalus.prelude.SortedMap.empty,
            id = TxId(hex"1e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a7bf61fe9")
          ),
          ScriptPurpose.Spending(hoskyMintTxOutRef)
        )

    private def performChecks(validator: Program) = {
        def appliedScript(
            preimage: ByteString,
            pubKeyHash: PubKeyHash,
            hash: ByteString,
            signatories: scalus.prelude.List[PubKeyHash]
        ) =
            val datum = (hash, pubKeyHash).toData
            val redeemer = preimage.toData
            val ctx = scriptContext(signatories)
            validator $ datum $ redeemer $ ctx.toData

        assertSameResult(Expected.Success(Const(Constant.Unit)))(
          appliedScript(
            preimage = ByteString.fromArray("Scalus rocks!".getBytes("UTF-8")),
            pubKeyHash =
                PubKeyHash(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"),
            hash = hex"36c71edaf0affadf6dd9c5f8df3dc90ec0fc01bf9f8cf0f18268db24b2a3da49",
            signatories =
                List(PubKeyHash(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"))
          )
        )

        assertSameResult(Expected.Failure("Wrong preimage"))(
          appliedScript(
            preimage = ByteString.fromArray("Scalus rocks!".getBytes("UTF-8")),
            pubKeyHash =
                PubKeyHash(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"),
            hash = hex"000000",
            signatories =
                List(PubKeyHash(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"))
          )
        )

        assertSameResult(Expected.Failure("No valid signature"))(
          appliedScript(
            preimage = ByteString.fromArray("Scalus rocks!".getBytes("UTF-8")),
            pubKeyHash =
                PubKeyHash(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"),
            hash = hex"36c71edaf0affadf6dd9c5f8df3dc90ec0fc01bf9f8cf0f18268db24b2a3da49",
            signatories = List(PubKeyHash(hex"000000"))
          )
        )

    }

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    test("Preimage Validator") {
        // compile to Scalus Intermediate Representation, SIR
        val compiled = compile(PreimageValidator.preimageValidator)
        // println(compiled.showHighlighted)
        // convert SIR to UPLC
        val validator = compiled.toUplc(generateErrorTraces = true).plutusV2
        // println(validator.showHighlighted)
        val flatSize = validator.flatEncoded.length
        // V3 backend with lambda barriers, optimizeUplc = true
        assert(flatSize == 285)
        // V3 backend, optimizeUplc = false
        // assert(flatSize == 380)
        // SimpleBackend
        // assert(flatSize == 2230)

        performChecks(validator)
    }

    test("Optimized Preimage Validator") {
        val optV = OptimizedPreimage.compiledOptimizedPreimageValidator
        val uplc = optV.toUplc()
        val program = uplc.plutusV3
        val flatSize = program.flatEncoded.length
//        assert(flatSize == 179)
        performChecks(program)
    }
}
