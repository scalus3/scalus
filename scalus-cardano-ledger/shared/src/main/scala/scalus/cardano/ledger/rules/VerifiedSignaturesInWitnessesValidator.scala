package scalus.cardano.ledger
package rules

import scalus.uplc.builtin.{platform, ByteString, PlatformSpecific}

// It's Shelley.validateVerifiedWits in cardano-ledger
object VerifiedSignaturesInWitnessesValidator extends STS.Validator {
    override final type Error = TransactionException.InvalidSignaturesInWitnessesException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val utxo = state.utxos

        val invalidVkeyWitnessesSet = invalidVkeyWitnesses(event)
        val invalidBootstrapWitnessesSet = invalidBootstrapWitnesses(event)

        if invalidVkeyWitnessesSet.nonEmpty || invalidBootstrapWitnessesSet.nonEmpty
        then
            failure(
              TransactionException.InvalidSignaturesInWitnessesException(
                transactionId,
                invalidVkeyWitnessesSet,
                invalidBootstrapWitnessesSet
              )
            )
        else success
    }

    private def invalidVkeyWitnesses(
        event: Event
    ): Set[VKeyWitness] = {
        val transactionId = event.id
        val vkeyWitnesses = event.witnessSet.vkeyWitnesses

        vkeyWitnesses.toSet.filterNot(vkeyWitness =>
            verifyWitnessSignature(
              platform,
              transactionId,
              vkeyWitness.vkey,
              vkeyWitness.signature
            )
        )
    }

    private def invalidBootstrapWitnesses(
        event: Event
    ): Set[BootstrapWitness] = {
        val transactionId = event.id
        val bootstrapWitnesses = event.witnessSet.bootstrapWitnesses

        bootstrapWitnesses.toSet.filterNot(bootstrapWitness =>
            verifyWitnessSignature(
              platform,
              transactionId,
              bootstrapWitness.publicKey,
              bootstrapWitness.signature
            )
        )
    }

    /** The platform is a parameter so a test can stand in for the native library. */
    private[rules] def verifyWitnessSignature(
        ps: PlatformSpecific,
        transactionId: TransactionHash,
        key: ByteString,
        signature: ByteString
    ): Boolean = {
        // Only a wrong key or signature length (a `require` on every platform) is an invalid
        // signature. Anything else, such as a missing native library, must not look like one.
        try ps.verifyEd25519Signature(key, transactionId, signature)
        catch case _: IllegalArgumentException => false
    }
}
