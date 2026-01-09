package scalus.examples

import scalus.*
import scalus.builtin.Builtins.*
import scalus.builtin.{ByteString, Data}
import scalus.cardano.blueprint.{Application, Blueprint}
import scalus.compiler.sir.{SIR, TargetLoweringBackend}
import scalus.compiler.{compile, Options}
import scalus.ledger.api.v2.*
import scalus.prelude.*
import scalus.uplc.*

@Compile
object PreimageValidator {
    def preimageValidator(datum: Data, redeemer: Data, ctxData: Data): Unit = {
        // deserialize from Data
        val (hash, pkh) = datum.to[(ByteString, ByteString)]
        val preimage = redeemer.toByteString
        val ctx = ctxData.to[ScriptContext]
        // check that the transaction is signed by the public key hash
        ctx.txInfo.signatories.find(_.hash == pkh).orFail("Not signed")
        // check that the preimage hashes to the hash
        require(sha2_256(preimage) == hash, "Wrong preimage")
    }
}

@Compile
object PreimageValidatorV3 extends Validator {
    import scalus.ledger.api.v3.*
    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = { // deserialize from Data
        val (hash, pkh) = datum.getOrFail("Expected datum").to[(ByteString, ByteString)]
        val preimage = redeemer.toByteString
        // check that the transaction is signed by the public key hash
        tx.signatories.find(_.hash == pkh).orFail("Not signed")
        // check that the preimage hashes to the hash
        require(sha2_256(preimage) == hash, "Wrong preimage")
    }
}

@Compile
object OptimizedPreimageValidator {

    /** Validates that the preimage is correct for the given hash and public key hash. The public
      * key hash must be a signatory of the transaction.
      */
    def preimageValidator(datum: Data, redeemer: Data, ctxData: Data): Unit = {
        // datum is a pair of 2 bytestrings: sha2_256(preimage) and public key hash
        val pair = datum.toConstr.snd
        // get the hash
        inline def hash = pair.head.toByteString
        // get the public key hash
        val pkh = pair.tail.head
        // get the preimage
        inline def preimage = redeemer.toByteString
        def checkSignatories(sigs: builtin.BuiltinList[Data]): Unit =
            if trace("sig.head")(sigs.head) == pkh then trace("signed")(())
            else checkSignatories(sigs.tail)
        // get the signatories of the transaction
        inline def sigs = ctxData.field[ScriptContext](_.txInfo.signatories).toList
        checkSignatories(sigs)
        sha2_256(preimage) == hash || (throw new RuntimeException("Wrong"))
    }
}

object OptimizedPreimage {

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
    )

    val compiledOptimizedPreimageValidator: SIR = compile(
      OptimizedPreimageValidator.preimageValidator
    )
    val validator: Term = compiledOptimizedPreimageValidator.toUplc(generateErrorTraces = true)
    val programV1: Program = validator.plutusV1
    // val cbor = Cbor.encode(flatEncoded).toByteArray
    // val cborHex = Utils.bytesToHex(Cbor.encode(flatEncoded).toByteArray)
    val doubleCborHex: String = programV1.doubleCborHex
}

object PreimageValidatorContract:
    def application: Application = Application
        .ofSingleValidator[(ByteString, ByteString), ByteString](
          "Preimage validator",
          "Hash preimage verification with signature validation",
          "1.0.0",
          PreimageValidatorV3.validate
        )

    def blueprint: Blueprint = application.blueprint

end PreimageValidatorContract
