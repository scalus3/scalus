package scalus.examples

import scalus.Compile
import scalus.uplc.builtin
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data
import scalus.cardano.onchain.plutus.prelude.log

@Compile
object PubKeyValidator {

    def validator(datum: Unit, redeemer: Unit, ctx: Data) = {
        val txinfo = unConstrData(unConstrData(ctx).snd.head).snd
        val signatories = unListData(txinfo.tail.tail.tail.tail.tail.tail.tail.head)

        def findSignatureOrFail(sigs: scalus.uplc.builtin.BuiltinList[Data]): Unit =
            if signatories.isEmpty then throw new RuntimeException("Signature not found")
            else if unBData(signatories.head) == hex"deadbeef"
            then ()
            else findSignatureOrFail(signatories.tail)

        findSignatureOrFail(signatories)
    }

    inline def validatorV2(inline pubKey: ByteString)(datum: Unit, redeemer: Unit, ctx: Data) = {
        import scalus.cardano.onchain.plutus.v2.ScriptContext
        val signatories = ctx.field[ScriptContext](_.txInfo.signatories).toList
        log("got signatories")

        def findSignatureOrFail(sigs: scalus.uplc.builtin.BuiltinList[Data]): Unit =
            log("check sigs")
            if signatories.isEmpty then throw new RuntimeException("Signature not found")
            else if signatories.head.toByteString == pubKey
            then log("found!")
            else trace("nope")(findSignatureOrFail(signatories.tail))

        findSignatureOrFail(signatories)
    }

    inline def validatorV3(inline pubKey: ByteString)(ctx: Data) = {
        import scalus.cardano.onchain.plutus.v3.ScriptContext
        val signatories = ctx.field[ScriptContext](_.txInfo.signatories).toList
        log("got signatories")

        def findSignatureOrFail(sigs: scalus.uplc.builtin.BuiltinList[Data]): Unit =
            log("check sigs")
            if signatories.isEmpty then throw new RuntimeException("Signature not found")
            else if signatories.head.toByteString == pubKey
            then log("found!")
            else trace("nope")(findSignatureOrFail(signatories.tail))

        findSignatureOrFail(signatories)
    }
}
