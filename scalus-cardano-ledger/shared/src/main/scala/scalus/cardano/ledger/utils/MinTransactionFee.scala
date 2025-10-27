package scalus.cardano.ledger
package utils

import monocle.Focus
import monocle.Focus.refocus
import scalus.serialization.cbor.Cbor
import scalus.|>

import scala.annotation.tailrec

object MinTransactionFee {
    @tailrec
    def apply(
        transaction: Transaction,
        utxo: Utxos,
        protocolParams: ProtocolParams
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Coin
    ] = {
        AllResolvedScripts.allProvidedReferenceScripts(transaction, utxo) match {
            case Left(e) => Left(e)
            case Right(scripts) =>
                val refScriptsFee = RefScriptsFeeCalculator(scripts, protocolParams)
                val transactionSizeFee = calculateTransactionSizeFee(transaction, protocolParams)
                val exUnitsFee = calculateExUnitsFee(transaction, protocolParams)

                val minFee = refScriptsFee + transactionSizeFee + exUnitsFee
                if minFee <= transaction.body.value.fee
                then Right(minFee)
                else {
                    val nextCandidateTransaction = transaction |>
                        Focus[Transaction](_.body)
                            .andThen(KeepRaw.lens[TransactionBody]())
                            .refocus(_.fee)
                            .replace(minFee)
                    MinTransactionFee(nextCandidateTransaction, utxo, protocolParams)
                }
        }
    }

    private object RefScriptsFeeCalculator {
        def apply(scripts: Set[Script], protocolParams: ProtocolParams): Coin = {
            def tierRefScriptFee(
                multiplier: NonNegativeInterval,
                sizeIncrement: Int,
                curTierPrice: NonNegativeInterval,
                n: Int
            ): Coin = {
                @tailrec
                def go(
                    acc: NonNegativeInterval,
                    curTierPrice: NonNegativeInterval,
                    n: Int
                ): Coin = {
                    if n < sizeIncrement then Coin((acc + curTierPrice * n).floor)
                    else
                        go(
                          acc + curTierPrice * sizeIncrement,
                          multiplier * curTierPrice,
                          n - sizeIncrement
                        )
                }

                go(NonNegativeInterval.zero, curTierPrice, n)
            }

            val refScriptsSize = scripts.foldLeft(0) { case (length, script) =>
                val scripLength = script match
                    case _: Script.Native => 0 // Native scripts do not contribute to fees
                    case s: PlutusScript  => s.script.size

                length + scripLength
            }

            val minFeeRefScriptCostPerByte = NonNegativeInterval(
              protocolParams.minFeeRefScriptCostPerByte
            )

            tierRefScriptFee(
              refScriptCostMultiplier,
              refScriptCostStride,
              minFeeRefScriptCostPerByte,
              refScriptsSize
            )
        }

        private val refScriptCostMultiplier = NonNegativeInterval(1.2)
        private val refScriptCostStride = 25600
    }

    private def calculateTransactionSizeFee(
        transaction: Transaction,
        protocolParams: ProtocolParams
    ): Coin = {
        val txFeeFixed = protocolParams.txFeeFixed
        val txFeePerByte = protocolParams.txFeePerByte
        val transactionSize = Cbor.encode(transaction).length

        Coin(transactionSize * txFeePerByte + txFeeFixed)
    }

    private def calculateExUnitsFee(
        transaction: Transaction,
        protocolParams: ProtocolParams
    ): Coin = {
        val executionUnitPrices = protocolParams.executionUnitPrices
        val totalExUnits = calculateTotalExUnits(transaction)

        if totalExUnits == ExUnits.zero then Coin.zero
        else
            Coin(
              (
                executionUnitPrices.priceMemory * totalExUnits.memory +
                    executionUnitPrices.priceSteps * totalExUnits.steps
              ).ceil
            )
    }

    private def calculateTotalExUnits(transaction: Transaction): ExUnits = {
        transaction.witnessSet.redeemers
            .map(_.value.toSeq.foldLeft(ExUnits.zero) { (exUnits, redeemer) =>
                exUnits + redeemer.exUnits
            })
            .getOrElse(ExUnits.zero)
    }
}
