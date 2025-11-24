package scalus.cardano.txbuilder

import monocle.Lens
import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.{MinTransactionFee, TxBalance}
import scalus.cardano.txbuilder
import scalus.cardano.txbuilder.TxBalancingError.Failed

import scala.annotation.tailrec
import scala.util.Try

object LowLevelTxBuilder {
    @deprecated("Use scalus.cardano.txbuilder.ChangeOutputDiffHandler instead", "scalus 0.13.0")
    class ChangeOutputDiffHandler(protocolParams: ProtocolParams, changeOutputIdx: Int)
        extends txbuilder.ChangeOutputDiffHandler(protocolParams, changeOutputIdx)

    /** Balances the transaction using a diff handler to adjust the transaction.
      *
      * Invariants:
      *   - only ADA is adjusted, native tokens must be balanced beforehand
      *   - fees never go below the initial fee
      */
    def balanceFeeAndChange(
        initial: Transaction,
        changeOutputIdx: Int,
        protocolParams: ProtocolParams,
        resolvedUtxo: Utxos,
        evaluator: PlutusScriptEvaluator,
    ): Either[TxBalancingError, Transaction] = {
        balanceFeeAndChange(
          initial,
          txbuilder
              .ChangeOutputDiffHandler(protocolParams, changeOutputIdx)
              .changeOutputDiffHandler,
          protocolParams,
          resolvedUtxo,
          evaluator
        )
    }

    /** Balances the transaction using a diff handler to adjust the transaction.
      *
      * Invariants:
      *   - only ADA is adjusted, native tokens must be balanced beforehand
      *   - fees never go below the initial fee
      */
    def balanceFeeAndChange(
        initial: Transaction,
        diffHandler: (Long, Transaction) => Either[TxBalancingError, Transaction],
        protocolParams: ProtocolParams,
        resolvedUtxo: Utxos,
        evaluator: PlutusScriptEvaluator,
    ): Either[TxBalancingError, Transaction] = {
        var iteration = 0

        @tailrec def loop(tx: Transaction): Either[TxBalancingError, Transaction] = {
            iteration += 1
            if iteration > 20 then return Left(TxBalancingError.CantBalance(0))
            val providedTxFee = tx.body.value.fee

            val eTrialTx = for {
                txWithExUnits <- computeScriptsWitness(resolvedUtxo, evaluator, protocolParams)(tx)
                minFee <- MinTransactionFee(txWithExUnits, resolvedUtxo, protocolParams).left.map(
                  TxBalancingError.Failed(_)
                )
                // Don't go below initial fee
                fee = Coin(math.max(minFee.value, initial.body.value.fee.value))
                txWithFees = setFee(fee)(txWithExUnits)
                diff = calculateChangeLovelace(txWithFees, resolvedUtxo, protocolParams)
                // try to balance it
                balanced <- diffHandler(diff, txWithFees)
            } yield balanced
            eTrialTx match {
                case Left(e)                         => Left(e)
                case Right(trialTx) if tx == trialTx => Right(tx)
                case Right(trialTx)                  => loop(trialTx)
            }
        }
        loop(initial)
    }

    private def computeScriptsWitness(
        utxos: Utxos,
        evaluator: PlutusScriptEvaluator,
        protocolParams: ProtocolParams
    )(tx: Transaction): Either[TxBalancingError, Transaction] = Try {
        val redeemers = evaluator.evalPlutusScripts(tx, utxos)
        setupRedeemers(protocolParams, tx, utxos, redeemers)
    }.toEither.left.map(t =>
        t match
            case psee: PlutusScriptEvaluationException => TxBalancingError.EvaluationFailed(psee)
            case other                                 => TxBalancingError.Failed(other)
    )

    private def setupRedeemers(
        protocolParams: ProtocolParams,
        tx: Transaction,
        utxos: Utxos,
        redeemers: Seq[Redeemer]
    ) = {
        val txWithRedeemers =
            if redeemers.nonEmpty then
                val rawRedeemers = KeepRaw(Redeemers.from(redeemers))
                tx.copy(witnessSet = tx.witnessSet.copy(redeemers = Some(rawRedeemers)))
            else tx

        val scriptDataHash =
            ScriptDataHashGenerator
                .computeScriptDataHash(
                  txWithRedeemers,
                  utxos,
                  protocolParams,
                )
                .toOption
                .get

        if scriptDataHash.nonEmpty then
            txWithRedeemers.copy(body =
                KeepRaw(tx.body.value.copy(scriptDataHash = scriptDataHash))
            )
        else txWithRedeemers
    }
}

// Transaction balancing error types
enum TxBalancingError {
    // Now it's only Plutus, but may become `Plutus... | SthElse...` in the future
    case EvaluationFailed(cause: PlutusScriptEvaluationException)
    // TODO: this constructor gets all other errors - rename?
    case Failed(cause: Throwable)
    case CantBalance(lastDiff: Long)
    case InsufficientFunds(diff: Long, minRequired: Long)
}

extension (t: TransactionOutput) {
    def valueLens: Lens[TransactionOutput, Value] =
        Lens[TransactionOutput, Value](_.value)(v => txout => txout.withValue(v))

    def withValue(amount: Value): TransactionOutput = t match {
        case shelley: TransactionOutput.Shelley =>
            shelley.copy(value = amount)
        case babbage: TransactionOutput.Babbage =>
            babbage.copy(value = amount)
    }
}

def modifyBody(tx: Transaction, f: TransactionBody => TransactionBody): Transaction = {
    val newBody = f(tx.body.value)
    tx.copy(body = KeepRaw(newBody))
}

def modifyWs(tx: Transaction, f: TransactionWitnessSet => TransactionWitnessSet): Transaction = {
    val newWs = f(tx.witnessSet)
    tx.copy(witnessSet = newWs)
}

def setFee(amount: Coin)(tx: Transaction) = modifyBody(tx, _.copy(fee = amount))

def calculateChangeLovelace(tx: Transaction, utxo: Utxos, params: ProtocolParams): Long = {
    val produced = TxBalance.produced(tx)
    val consumed = TxBalance.consumed(tx, CertState.empty, utxo, params).toTry.get
    consumed.coin.value - produced.coin.value
}

def calculateChangeValue(tx: Transaction, utxo: Utxos, params: ProtocolParams): Value = {
    val produced = TxBalance.produced(tx)
    val consumed = TxBalance.consumed(tx, CertState.empty, utxo, params).toTry.get
    consumed - produced
}
