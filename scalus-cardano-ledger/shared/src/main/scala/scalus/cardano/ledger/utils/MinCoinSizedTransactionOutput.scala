package scalus.cardano.ledger
package utils

import monocle.syntax.all.*
import monocle.*
import scalus.|>

import scala.annotation.tailrec

object MinCoinSizedTransactionOutput {

    def extractMinAda(
        sizedTransactionOutput: Sized[TransactionOutput],
        protocolParams: ProtocolParams
    ): Coin = {
        val utxoCostPerByte = protocolParams.utxoCostPerByte
        val Sized(TransactionOutputValue(ValueCoin(coin)), size) = sizedTransactionOutput

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
    def findMinAda(
        sizedTransactionOutput: Sized[TransactionOutput],
        protocolParams: ProtocolParams
    ): Coin = {
        val utxoCostPerByte = protocolParams.utxoCostPerByte
        val Sized(TransactionOutputValue(ValueCoin(coin)), size) = sizedTransactionOutput

        val minAda = Coin((constantOverhead + size) * utxoCostPerByte)

        if minAda <= coin then minAda
        else
            val nextCandidateOutput = sizedTransactionOutput |>
                Sized
                    .lens[TransactionOutput]()
                    .andThen(TransactionOutput.valueLens)
                    .refocus(_.coin)
                    .replace(minAda)
            MinCoinSizedTransactionOutput.findMinAda(nextCandidateOutput, protocolParams)
    }

    private val constantOverhead = 160
}
