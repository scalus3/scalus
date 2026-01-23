package scalus.cardano.txbuilder

import scalus.cardano.ledger.*

/** @deprecated
  *   This object is deprecated. Use the functions and types from their new locations:
  *   - `balanceFeeAndChange` and `balanceFeeAndChangeWithTokens` are now in `TransactionBuilder`
  *     object
  *   - `TxBalancingError` is now in its own file `scalus.cardano.txbuilder.TxBalancingError`
  *   - `modifyBody`, `modifyWs`, `setFee`, `calculateChangeValue` are now in `TransactionBuilder`
  *     object
  *   - `withValue` extension is now in `TransactionOutput` companion object in
  *     `scalus.cardano.ledger`
  */
@deprecated("Use TransactionBuilder methods instead", "scalus 0.13.0")
object LowLevelTxBuilder {
    @deprecated("Use Change.changeOutputDiffHandler instead", "scalus 0.13.0")
    class ChangeOutputDiffHandler(protocolParams: ProtocolParams, changeOutputIdx: Int)
        extends scalus.cardano.txbuilder.ChangeOutputDiffHandler(protocolParams, changeOutputIdx)

    @deprecated(
      "Use TransactionBuilder.balanceFeeAndChange instead",
      "0.13.0"
    )
    def balanceFeeAndChange(
        initial: Transaction,
        changeOutputIdx: Int,
        protocolParams: ProtocolParams,
        resolvedUtxo: Utxos,
        evaluator: PlutusScriptEvaluator,
    ): Either[TxBalancingError, Transaction] =
        TransactionBuilder.balanceFeeAndChange(
          initial,
          changeOutputIdx,
          protocolParams,
          resolvedUtxo,
          evaluator
        )

    @deprecated(
      "Use TransactionBuilder.balanceFeeAndChangeWithTokens instead",
      "0.13.0"
    )
    def balanceFeeAndChangeWithTokens(
        initial: Transaction,
        diffHandler: (Value, Transaction) => Either[TxBalancingError, Transaction],
        protocolParams: ProtocolParams,
        resolvedUtxo: Utxos,
        evaluator: PlutusScriptEvaluator,
    ): Either[TxBalancingError, Transaction] =
        TransactionBuilder.balanceFeeAndChangeWithTokens(
          initial,
          diffHandler,
          protocolParams,
          resolvedUtxo,
          evaluator
        )
}

@deprecated("Use Transaction.withBody instead", "0.13.0")
def modifyBody(tx: Transaction, f: TransactionBody => TransactionBody): Transaction =
    tx.withBody(f)

@deprecated("Use Transaction.withWitness instead", "0.13.0")
def modifyWs(
    tx: Transaction,
    f: TransactionWitnessSet => TransactionWitnessSet
): Transaction =
    tx.withWitness(f)

@deprecated("Use tx.withBody(_.copy(fee = amount)) instead", "0.13.0")
def setFee(amount: Coin)(tx: Transaction): Transaction =
    tx.withBody(_.copy(fee = amount))

@deprecated("Use TransactionBuilder.calculateChangeValue instead", "scalus 0.13.0")
def calculateChangeValue(tx: Transaction, utxo: Utxos, params: ProtocolParams): Value =
    TransactionBuilder.calculateChangeValue(tx, utxo, params)
