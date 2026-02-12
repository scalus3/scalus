package scalus.cardano.ledger
package utils

import io.bullet.borer.Cbor
import monocle.{Focus, Lens}
import monocle.Focus.refocus

import scala.annotation.tailrec

object MinTransactionFee {
    def computeMinFee(
        transaction: Transaction,
        utxo: Utxos,
        protocolParams: ProtocolParams
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Coin
    ] = {
        AllResolvedScripts.allProvidedReferenceScriptsNonDistinctSeq(transaction, utxo) match {
            case Left(e) => Left(e)
            case Right(scripts) =>
                val refScriptsFee = RefScriptsFeeCalculator(scripts, protocolParams)
                val transactionSizeFee = calculateTransactionSizeFee(transaction, protocolParams)
                val exUnitsFee = calculateExUnitsFee(transaction, protocolParams)

                val minFee = refScriptsFee + transactionSizeFee + exUnitsFee
                Right(minFee)
        }
    }

    def ensureMinFee(
        transaction: Transaction,
        utxo: Utxos,
        protocolParams: ProtocolParams
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Coin
    ] = {
        AllResolvedScripts.allProvidedReferenceScriptsNonDistinctSeq(transaction, utxo) match {
            case Left(e) => Left(e)
            case Right(scripts) =>
                val refScriptsFee = RefScriptsFeeCalculator(scripts, protocolParams)
                val exUnitsFee = calculateExUnitsFee(transaction, protocolParams)

                @tailrec
                def go(transaction: Transaction): Coin = {
                    val fee = transaction.body.value.fee
                    val transactionSizeFee =
                        calculateTransactionSizeFee(transaction, protocolParams)
                    val minFee = refScriptsFee + transactionSizeFee + exUnitsFee
                    if minFee <= fee then minFee
                    else
                        val updatedTransaction = feeLens.replace(minFee)(transaction)
                        go(updatedTransaction)
                }

                Right(go(transaction))
        }
    }

    private object RefScriptsFeeCalculator {
        def apply(scripts: Seq[Script], protocolParams: ProtocolParams): Coin = {
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
                    case s: Script.Native => Cbor.encode(s).toByteArray.length
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
        val transactionSize = transaction.toCborForFeeCalculation.length

        Coin(transactionSize * txFeePerByte + txFeeFixed)
    }

    private def calculateExUnitsFee(
        transaction: Transaction,
        protocolParams: ProtocolParams
    ): Coin = {
        val executionUnitPrices = protocolParams.executionUnitPrices
        val totalExUnits =
            transaction.witnessSet.redeemers.map(_.value.totalExUnits).getOrElse(ExUnits.zero)

        if totalExUnits == ExUnits.zero then Coin.zero
        else totalExUnits.fee(executionUnitPrices)
    }

    // Cache the lens at object level
    private val feeLens: Lens[Transaction, Coin] =
        Focus[Transaction](_.body)
            .andThen(KeepRaw.lens[TransactionBody]())
            .refocus(_.fee)
}
