package scalus.cardano.txbuilder

import monocle.Focus.focus
import monocle.Lens
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput

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
        val minAda = MinCoinSizedTransactionOutput(newChangeOut, protocolParams)

        if updatedCoinValue < minAda.value then {
            return Left(
              TxBalancingError.InsufficientFunds(diff.coin.value, minAda.value - updatedCoinValue)
            )
        }

        val tb = tx.body.value
            .focus(_.outputs.index(changeOutputIdx))
            .replace(newChangeOut)
        val t = tx.copy(body = KeepRaw(tb))
        Right(t)
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

        val updatedOutput = currentOutput match {
            case s: TransactionOutput.Shelley => s.copy(value = newValue)
            case b: TransactionOutput.Babbage => b.copy(value = newValue)
        }

        // Ensure minAda
        val sizedOutput = Sized(updatedOutput)
        val minAda = MinCoinSizedTransactionOutput(sizedOutput, protocolParams)

        if newCoinValue < minAda.value then {
            // Bump to minAda if needed
            val bumpedValue = newValue.copy(coin = minAda)
            val bumpedOutput = updatedOutput match {
                case s: TransactionOutput.Shelley => s.copy(value = bumpedValue)
                case b: TransactionOutput.Babbage => b.copy(value = bumpedValue)
            }
            Right(
              modifyBody(
                tx,
                b => b.copy(outputs = b.outputs.updated(changeOutputIdx, Sized(bumpedOutput)))
              )
            )
        } else {
            Right(
              modifyBody(
                tx,
                b => b.copy(outputs = b.outputs.updated(changeOutputIdx, sizedOutput))
              )
            )
        }
    }

    private def createChangeOutput(
        tx: Transaction,
        changeAddress: Address,
        value: Value,
        protocolParams: ProtocolParams
    ): Either[TxBalancingError, Transaction] = {
        val changeOutput: TransactionOutput = TransactionOutput.Babbage(
          address = changeAddress,
          value = value,
          datumOption = None,
          scriptRef = None
        )

        // Ensure min ADA
        val sizedOutput = Sized(changeOutput)
        val minAda = MinCoinSizedTransactionOutput(sizedOutput, protocolParams)

        if value.coin.value < minAda.value then {
            // Bump to min ADA
            val bumpedValue = value.copy(coin = minAda)
            val bumpedOutput = changeOutput match {
                case b: TransactionOutput.Babbage => b.copy(value = bumpedValue)
                case s: TransactionOutput.Shelley => s.copy(value = bumpedValue)
            }
            Right(
              modifyBody(
                tx,
                b => b.copy(outputs = b.outputs.toSeq :+ Sized(bumpedOutput))
              )
            )
        } else {
            Right(
              modifyBody(
                tx,
                b => b.copy(outputs = b.outputs.toSeq :+ sizedOutput)
              )
            )
        }
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

        val newCoinValue = currentValue.coin.value + valueToRemove.coin.value

        // Check if we have enough tokens
        // For each token type to remove, check if we have it
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
            val newValue = Value(Coin(newCoinValue), newAssets)

            // Check if output would still meet minAda requirement
            val updatedOutput = currentOutput match {
                case s: TransactionOutput.Shelley => s.copy(value = newValue)
                case b: TransactionOutput.Babbage => b.copy(value = newValue)
            }

            val sizedOutput = Sized(updatedOutput)
            val minAda = MinCoinSizedTransactionOutput(sizedOutput, protocolParams).value

            if newCoinValue < minAda then {
                // Can't satisfy - change output would be below min ADA
                Left(TxBalancingError.CantBalance(valueToRemove.coin.value))
            } else {
                Right(
                  modifyBody(
                    tx,
                    b => b.copy(outputs = b.outputs.updated(changeOutputIdx, sizedOutput))
                  )
                )
            }
        }
    }
}
