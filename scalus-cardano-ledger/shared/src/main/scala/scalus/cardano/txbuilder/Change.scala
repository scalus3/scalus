package scalus.cardano.txbuilder

import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput

/** Utilities for handling change outputs in transaction balancing. */
object Change {

    /** Handles the diff by adjusting an existing change output at the specified index.
      *
      * This method is intended to be used as a diff handler function with
      * [[TransactionBuilder.balanceFeeAndChangeWithTokens]]. It adjusts an existing change output
      * rather than creating a new one.
      *
      * @param diff
      *   The value difference to apply (positive to add, negative to remove)
      * @param tx
      *   The transaction to modify
      * @param protocolParams
      *   Protocol parameters for minAda calculations
      * @param changeOutputIdx
      *   The index of the change output to modify
      * @return
      *   Either a [[TxBalancingError]] or the modified transaction
      * @throws IllegalArgumentException
      *   if changeOutputIdx is out of bounds
      */
    def changeOutputDiffHandler(
        diff: Value,
        tx: Transaction,
        protocolParams: ProtocolParams,
        changeOutputIdx: Int
    ): Either[TxBalancingError, Transaction] = {
        val numOutputs = tx.body.value.outputs.size
        require(
          changeOutputIdx < numOutputs,
          s"Change output index $changeOutputIdx is out of bounds for outputs of size $numOutputs"
        )
        val changeOut = tx.body.value.outputs(changeOutputIdx)
        val currentValue = changeOut.value.value

        val updatedCoinValue = currentValue.coin.value + diff.coin.value
        val updatedAssets = currentValue.assets + diff.assets
        val newValue = Value(Coin(updatedCoinValue), updatedAssets)

        val newChangeOut = Sized(changeOut.value.withValue(newValue))
        val minAda = MinCoinSizedTransactionOutput.ensureMinAda(newChangeOut, protocolParams)

        if updatedCoinValue < minAda.value then {
            return Left(
              TxBalancingError.InsufficientFunds(diff, minAda.value - updatedCoinValue)
            )
        }

        Right(
          tx.withBody(
            _.copy(outputs = tx.body.value.outputs.updated(changeOutputIdx, newChangeOut))
          )
        )
    }
}
