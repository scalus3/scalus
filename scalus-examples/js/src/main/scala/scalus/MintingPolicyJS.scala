package scalus

import scalus.uplc.builtin.{ByteString, Data}
import scalus.compiler.compile
import scalus.compiler.sir.SIR
import scalus.examples.MintingPolicy
import scalus.cardano.onchain.plutus.prelude.SortedMap
import scalus.uplc.Term
import scalus.uplc.TermDSL.given
import scalus.uplc.eval.PlutusVM

import scala.language.implicitConversions
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("MintingPolicyJS")
object MintingPolicyJS:

    private val validatorScript =
        MintingPolicy.compiledOptimizedMintingPolicyScript
            .toUplc(generateErrorTraces = true)
            .plutusV1

    val alwaysok: SIR = compile((redeemer: Data, ctx: Data) => ())
    val alwaysokTerm: Term = alwaysok.toUplc()

    @JSExport
    def getPlutusScriptCborFromTxOutRef(
        txIdHex: String,
        txOutIdx: Int,
        tokenNameHex: String,
        amount: Int
    ): String = {
        given PlutusVM = PlutusVM.makePlutusV1VM()
        val tokensSIR = compile((tokenNameHex: ByteString, amount: BigInt) =>
            SortedMap.singleton(tokenNameHex, amount)
        )
        val evaledTokens = tokensSIR.toUplc().evaluate
        val txId = ByteString.fromHex(txIdHex)
        val tokens = evaledTokens $ ByteString.fromHex(tokenNameHex) $ amount
        // val appliedValidator = alwaysokTerm
        val appliedValidator = validatorScript $ txId $ txOutIdx $ tokens

        appliedValidator.doubleCborHex
    }
