package scalus.testing.regression.mintingpolicy20251123

import scalus.*
import scalus.builtin.{ByteString, Data}
import scalus.builtin.Data.toData
import scalus.ledger.api.v3.*
import scalus.prelude.List
import scalus.uplc.Program

/** Minimal reproducer for IndexOutOfBoundsException: 41 during DeBruijn conversion
  *
  * This simple minting policy that validates a UTxO is consumed triggers a DeBruijn
  * index error when compiled with SirToUplcV3Lowering backend and then applied with
  * a Data parameter.
  *
  * The issue occurs in:
  * - scalus.uplc.DeBruijn$.go$1(DeBruijn.scala:57)
  * - During evaluation when converting the applied program to DeBruijn form
  *
  * Expected behavior:
  * - Should compile and apply the Data parameter successfully
  * - The resulting UPLC program should be evaluatable
  *
  * Actual behavior:
  * - IndexOutOfBoundsException: 41 when trying to evaluate the script
  * - The index suggests there are unbound variables in the generated UPLC
  */
@Compile
object SimpleMintingPolicy {
    import scalus.prelude.*

    /** Minting policy validator that checks if a specific UTxO is being spent
      *
      * @param utxoRefData The UTxO reference that must be spent (as Data)
      * @return A minting policy function that takes redeemer and context
      */
    def validate(utxoRefData: Data) = (redeemer: Data, ctxData: Data) => {
        val utxoRef = utxoRefData.to[TxOutRef]
        val ctx = ctxData.to[ScriptContext]
        import ctx.{txInfo}

        // Check that the specified UTxO is being consumed in this transaction
        val hasUtxo = txInfo.inputs.exists { input =>
            (input.outRef.id.hash == utxoRef.id.hash) &&
            (input.outRef.idx == utxoRef.idx)
        }

        // The policy succeeds only if the required UTxO is consumed
        hasUtxo
    }

    @Ignore
    given Compiler.Options = Compiler.Options(
      targetLoweringBackend = Compiler.TargetLoweringBackend.SirToUplcV3Lowering
    )

    @Ignore
    val compiledValidator = Compiler.compile(validate)
}

object SimpleMintingPolicyContract {
    /** Compile the minting policy and apply it to a specific UTxO reference
      *
      * @param utxoRef The UTxO that must be consumed to mint tokens
      * @return The compiled Plutus script program with the UTxO applied
      */
    def compileAndApply(utxoRef: TxOutRef): Program = {
        val program = SimpleMintingPolicy.compiledValidator.toUplcOptimized().plutusV3
        val utxoData = utxoRef.toData
        val uplcProgram = program $ utxoData
        uplcProgram
    }
}
