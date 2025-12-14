package scalus.cardano.txbuilder

import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput
import scalus.cardano.txbuilder.TransactionBuilder.{ensureMinAda, modifyBody}

class ChangeOutputDiffHandler(protocolParams: ProtocolParams, changeOutputIdx: Int) {
    def changeOutputDiffHandler(
        diff: Value,
        tx: Transaction
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
              TxBalancingError.InsufficientFunds(diff.coin.value, minAda.value - updatedCoinValue)
            )
        }

        Right(
          modifyBody(
            tx,
            _.copy(outputs = tx.body.value.outputs.updated(changeOutputIdx, newChangeOut))
          )
        )
    }
}

object Change {

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
                Left(TxBalancingError.InsufficientFunds(adaDiff, -adaDiff))
            }
        }
    }

    // FIXME: needs to be more sophisticated
    def findChangeOutput(tx: Transaction, changeAddress: Address) =
        tx.body.value.outputs.indexWhere(_.value.address == changeAddress)

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
            Left(TxBalancingError.InsufficientFunds(valueToRemove.coin.value, adaToRemove))
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
                Left(TxBalancingError.CantBalance(valueToRemove.coin.value))
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
