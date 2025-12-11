package scalus.cardano.ledger
package utils

import monocle.syntax.all.*
import monocle.*

import scala.annotation.tailrec

object MinCoinSizedTransactionOutput {

    /** Compute the minimum ada required for the given sized transaction output.
      *
      * @param sizedTransactionOutput
      * @param protocolParams
      * @return
      */
    def computeMinAda(
        sizedTransactionOutput: Sized[TransactionOutput],
        protocolParams: ProtocolParams
    ): Coin = {
        val utxoCostPerByte = protocolParams.utxoCostPerByte
        val size = sizedTransactionOutput.size
        val minAda = Coin((constantOverhead + size) * utxoCostPerByte)
        minAda
    }

    /** Recursively calculate the minimum ada that the given utxo must have in order to satisfy the
      * minAda requirement.
      *
      * @param sizedTransactionOutput
      * @param protocolParams
      * @return
      */
    @tailrec
    def ensureMinAda(
        sizedTransactionOutput: Sized[TransactionOutput],
        protocolParams: ProtocolParams
    ): Coin = {
        val coin = sizedTransactionOutput.value.value.coin
        val minAda = computeMinAda(sizedTransactionOutput, protocolParams)

        if minAda <= coin then minAda
        else
            val nextCandidateOutput = coinLens.replace(minAda)(sizedTransactionOutput)
            MinCoinSizedTransactionOutput.ensureMinAda(nextCandidateOutput, protocolParams)
    }

    private val constantOverhead = 160

    // Cache the lens at object level
    private val coinLens: Lens[Sized[TransactionOutput], Coin] =
        Sized
            .lens[TransactionOutput]()
            .andThen(TransactionOutput.valueLens)
            .refocus(_.coin)
}
