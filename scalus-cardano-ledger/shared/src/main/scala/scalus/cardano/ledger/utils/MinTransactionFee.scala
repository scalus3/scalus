package scalus.cardano.ledger
package utils

import monocle.{Focus, Lens}
import monocle.Focus.refocus

import scala.annotation.tailrec

object MinTransactionFee {

    /** The minimum fee of `transaction`.
      *
      * @throws java.lang.ArithmeticException
      *   if the minimum fee exceeds the range of Coin
      */
    def computeMinFee(
        transaction: Transaction,
        utxo: Utxos,
        protocolParams: ProtocolParams
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Coin
    ] = computeExactMinFee(transaction, utxo, protocolParams).map(Coin.exact)

    /** The minimum fee of `transaction`, computed exactly as cardano-ledger does with `Integer`.
      * The script fee of redeemers with units far above the protocol limit can exceed the range of
      * Coin.
      */
    private[ledger] def computeExactMinFee(
        transaction: Transaction,
        utxo: Utxos,
        protocolParams: ProtocolParams
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      BigInt
    ] = {
        AllResolvedScripts.allProvidedReferenceScriptsNonDistinctSeq(transaction, utxo) match {
            case Left(e) => Left(e)
            case Right(scripts) =>
                val refScriptsFee = RefScriptsFeeCalculator(scripts, protocolParams)
                val transactionSizeFee = calculateTransactionSizeFee(transaction, protocolParams)
                val exUnitsFee = calculateExUnitsFee(transaction, protocolParams)

                Right(BigInt(refScriptsFee.value) + transactionSizeFee.value + exUnitsFee)
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
                val exUnitsFee = Coin.exact(calculateExUnitsFee(transaction, protocolParams))

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
            val refScriptsSize = scripts.map(rules.TxRefScriptsSizeValidator.scriptSize).sum
            new RefScriptFee(protocolParams).calculate(refScriptsSize)
        }
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

    /** The script fee of all redeemers. Like cardano-ledger `totExUnits`, it sums the units without
      * a bound: `Redeemers.totalExUnits` saturates at `Long.MaxValue`.
      */
    private def calculateExUnitsFee(
        transaction: Transaction,
        protocolParams: ProtocolParams
    ): BigInt = {
        val redeemers = transaction.witnessSet.redeemers.map(_.value.toSeq).getOrElse(Seq.empty)
        val memory = redeemers.foldLeft(BigInt(0))(_ + _.exUnits.memory)
        val steps = redeemers.foldLeft(BigInt(0))(_ + _.exUnits.steps)
        ExUnits.exactFee(protocolParams.executionUnitPrices, memory, steps)
    }

    // Cache the lens at object level
    private val feeLens: Lens[Transaction, Coin] =
        Focus[Transaction](_.body)
            .andThen(KeepRaw.lens[TransactionBody]())
            .refocus(_.fee)
}
