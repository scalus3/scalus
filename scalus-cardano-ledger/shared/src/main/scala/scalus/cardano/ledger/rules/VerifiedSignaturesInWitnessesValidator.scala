package scalus.cardano.ledger
package rules

import scalus.builtin.{platform, ByteString}
import scala.util.control.NonFatal

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
            return failure(
              TransactionException.InvalidSignaturesInWitnessesException(
                transactionId,
                invalidVkeyWitnessesSet,
                invalidBootstrapWitnessesSet
              )
            )

        success
    }

    private def invalidVkeyWitnesses(
        event: Event
    ): Set[VKeyWitness] = {
        val transactionId = event.id
        val vkeyWitnesses = event.witnessSet.vkeyWitnesses

        vkeyWitnesses.toSortedSet.filterNot(vkeyWitness =>
            verifyWitnessSignature(transactionId, vkeyWitness.vkey, vkeyWitness.signature)
        )
    }

    private def invalidBootstrapWitnesses(
        event: Event
    ): Set[BootstrapWitness] = {
        val transactionId = event.id
        val bootstrapWitnesses = event.witnessSet.bootstrapWitnesses

        bootstrapWitnesses.toSortedSet.filterNot(bootstrapWitness =>
            verifyWitnessSignature(
              transactionId,
              bootstrapWitness.publicKey,
              bootstrapWitness.signature
            )
        )
    }

    private def verifyWitnessSignature(
        transactionId: TransactionHash,
        key: ByteString,
        signature: ByteString
    ): Boolean = {
        try platform.verifyEd25519Signature(key, transactionId, signature)
        catch case NonFatal(exception) => false
    }
}
