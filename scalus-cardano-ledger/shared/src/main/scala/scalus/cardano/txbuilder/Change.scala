package scalus.cardano.txbuilder

import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput
import scalus.cardano.txbuilder.TransactionBuilder.{ensureMinAda, modifyBody}

/** Helper class that creates a diff handler function for an existing change output.
  *
  * @deprecated
  *   Use [[Change.changeOutputDiffHandler]] instead
  */
@deprecated("Use Change.changeOutputDiffHandler instead", "0.13.0")
class ChangeOutputDiffHandler(protocolParams: ProtocolParams, changeOutputIdx: Int) {

    /** Handles the diff by adjusting the change output at the specified index.
      *
      * @param diff
      *   The value difference to apply (positive to add, negative to remove)
      * @param tx
      *   The transaction to modify
      * @return
      *   Either a [[TxBalancingError]] or the modified transaction
      */
    def changeOutputDiffHandler(
        diff: Value,
        tx: Transaction
    ): Either[TxBalancingError, Transaction] =
        Change.changeOutputDiffHandler(diff, tx, protocolParams, changeOutputIdx)
}

/** Utilities for handling change outputs in transaction balancing. */
object Change {

    /** Handles the change output logic for transaction balancing.
      *
      * This is the main entry point for adjusting change outputs during transaction balancing. It
      * decides whether to create, update, or remove change outputs based on the value difference.
      *
      *   - If diff is zero and no tokens, may remove an empty change output
      *   - If diff is positive or has positive tokens, adds to or creates a change output
      *   - If diff is negative, removes from the existing change output
      *
      * @param diff
      *   The value difference to handle (inputs - outputs - fee)
      * @param tx
      *   The transaction to modify
      * @param changeAddress
      *   The address to use for the change output
      * @param protocolParams
      *   Protocol parameters for minAda calculations
      * @return
      *   Either a [[TxBalancingError]] or the modified transaction
      */
    @deprecated("use changeOutputDiffHandler instead", "0.14.2")
    def handleChange(
        diff: Value,
        tx: Transaction,
        changeAddress: Address,
        protocolParams: ProtocolParams
    ): Either[TxBalancingError, Transaction] = {
        val body = tx.body.value
        val changeOutputIdx = findChangeOutput(tx, changeAddress)

        val adaDiff = diff.coin.value
        val tokensDiff = diff.assets

        // Check if there are leftover tokens
        val hasPositiveTokens = tokensDiff.assets.exists { case (_, assetMap) =>
            assetMap.exists { case (_, amount) => amount > 0 }
        }

        if adaDiff == 0 && tokensDiff.isEmpty then {
            if changeOutputIdx >= 0 && body.outputs(changeOutputIdx).value.value.isZero
            then {
                Right(
                  modifyBody(
                    tx,
                    b => b.copy(outputs = b.outputs.patch(changeOutputIdx, Nil, 1))
                  )
                )
            } else {
                Right(tx)
            }
        } else if adaDiff > 0 || hasPositiveTokens then {
            if changeOutputIdx >= 0 then {
                updateChangeOutput(tx, changeOutputIdx, diff, protocolParams)
            } else {
                createChangeOutput(tx, changeAddress, diff, protocolParams)
            }
        } else {
            if changeOutputIdx >= 0 then {
                removeFromChangeOutput(tx, changeOutputIdx, diff, protocolParams)
            } else {
                Left(TxBalancingError.InsufficientFunds(diff, -adaDiff))
            }
        }
    }

    /** Finds the index of the change output in a transaction.
      *
      * Currently uses a simple address match. This may need to be more sophisticated in the future
      * to handle cases with multiple outputs to the same address.
      *
      * @param tx
      *   The transaction to search
      * @param changeAddress
      *   The address to look for
      * @return
      *   The index of the first output matching the change address, or -1 if not found
      */
    @deprecated("will be removed", "0.14.2")
    def findChangeOutput(tx: Transaction, changeAddress: Address): Int =
        tx.body.value.outputs.indexWhere(_.value.address == changeAddress)

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
          modifyBody(
            tx,
            _.copy(outputs = tx.body.value.outputs.updated(changeOutputIdx, newChangeOut))
          )
        )
    }

    /** Updates an existing change output by adding additional value to it.
      *
      * @param tx
      *   The transaction to modify
      * @param changeOutputIdx
      *   The index of the change output to update
      * @param additionalValue
      *   The value to add to the change output
      * @param protocolParams
      *   Protocol parameters for minAda calculations
      * @return
      *   Either a [[TxBalancingError]] or the modified transaction
      */
    private def updateChangeOutput(
        tx: Transaction,
        changeOutputIdx: Int,
        additionalValue: Value,
        protocolParams: ProtocolParams
    ): Either[TxBalancingError, Transaction] = {
        val currentOutput = tx.body.value.outputs(changeOutputIdx).value
        val currentValue = currentOutput.value

        val newCoinValue = currentValue.coin.value + additionalValue.coin.value
        val newAssets = currentValue.assets + additionalValue.assets
        val newValue = Value(Coin(newCoinValue), newAssets)

        val updatedOutput = currentOutput.withValue(newValue)
        val finalOutput = Sized(ensureMinAda(updatedOutput, protocolParams))

        Right(
          modifyBody(tx, b => b.copy(outputs = b.outputs.updated(changeOutputIdx, finalOutput)))
        )
    }

    /** Creates a new change output with the specified value.
      *
      * @param tx
      *   The transaction to modify
      * @param changeAddress
      *   The address for the new change output
      * @param value
      *   The value for the change output
      * @param protocolParams
      *   Protocol parameters for minAda calculations
      * @return
      *   Either a [[TxBalancingError]] or the modified transaction with the new output appended
      */
    private def createChangeOutput(
        tx: Transaction,
        changeAddress: Address,
        value: Value,
        protocolParams: ProtocolParams
    ): Either[TxBalancingError, Transaction] = {
        val changeOutput = TransactionOutput(changeAddress, value)
        val finalOutput = Sized(ensureMinAda(changeOutput, protocolParams))
        Right(modifyBody(tx, b => b.copy(outputs = b.outputs.toSeq :+ finalOutput)))
    }

    /** Removes value from an existing change output.
      *
      * This is used when the transaction has excess outputs and needs to reduce the change. Fails
      * if removing would leave the change output below minAda or with insufficient tokens.
      *
      * @param tx
      *   The transaction to modify
      * @param changeOutputIdx
      *   The index of the change output to reduce
      * @param valueToRemove
      *   The value to remove (should have negative amounts)
      * @param protocolParams
      *   Protocol parameters for minAda calculations
      * @return
      *   Either a [[TxBalancingError]] if insufficient funds, or the modified transaction
      */
    private def removeFromChangeOutput(
        tx: Transaction,
        changeOutputIdx: Int,
        valueToRemove: Value,
        protocolParams: ProtocolParams
    ): Either[TxBalancingError, Transaction] = {
        val currentOutput = tx.body.value.outputs(changeOutputIdx).value
        val currentValue = currentOutput.value

        val adaToRemove = -valueToRemove.coin.value // Negative, so negate it
        val tokensToRemove = valueToRemove.assets

        // Check if we have enough tokens
        val insufficientTokens = tokensToRemove.assets.exists { case (policyId, assets) =>
            assets.exists { case (assetName, amountToRemove) =>
                val currentAmount = currentValue.assets.assets
                    .get(policyId)
                    .flatMap(_.get(assetName))
                    .getOrElse(0L)
                // If trying to remove more tokens than we have (negative diff means we need to remove)
                amountToRemove < 0 && currentAmount < -amountToRemove
            }
        }

        if insufficientTokens then {
            Left(TxBalancingError.InsufficientFunds(valueToRemove, adaToRemove))
        } else {
            // Subtract tokens (valueToRemove has negative amounts for subtraction)
            val newAssets = currentValue.assets + tokensToRemove
            val newCoinValue = currentValue.coin.value + valueToRemove.coin.value
            val newValue = Value(Coin(newCoinValue), newAssets)
            val updatedOutput = currentOutput.withValue(newValue)

            // Use ensureMinAda to check if output meets minAda requirement
            val finalOutput = ensureMinAda(updatedOutput, protocolParams)
            if finalOutput.value.coin > updatedOutput.value.coin then {
                // ensureMinAda had to bump the coin, meaning we're below minAda
                val minAdaNeeded = finalOutput.value.coin.value - updatedOutput.value.coin.value
                Left(TxBalancingError.InsufficientFunds(valueToRemove, minAdaNeeded))
            } else {
                Right(
                  modifyBody(
                    tx,
                    b => b.copy(outputs = b.outputs.updated(changeOutputIdx, Sized(finalOutput)))
                  )
                )
            }
        }
    }
}
