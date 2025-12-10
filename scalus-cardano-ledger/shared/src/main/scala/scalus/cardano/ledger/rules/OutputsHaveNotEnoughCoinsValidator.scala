package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput

// It's Babbage.validateOutputTooSmallUTxO in cardano-ledger
object OutputsHaveNotEnoughCoinsValidator extends STS.Validator {
    override final type Error = TransactionException.OutputsHaveNotEnoughCoinsException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val protocolParams = context.env.params
        val outputs = event.body.value.outputs
        val collateralReturnOutput = event.body.value.collateralReturnOutput.toIndexedSeq

        val invalidOutputs = findInvalidOutputs(outputs, protocolParams)

        val invalidCollateralReturnOutput =
            findInvalidOutputs(collateralReturnOutput, protocolParams).headOption

        if invalidOutputs.nonEmpty || invalidCollateralReturnOutput.nonEmpty then
            failure(
              TransactionException.OutputsHaveNotEnoughCoinsException(
                transactionId,
                invalidOutputs,
                invalidCollateralReturnOutput
              )
            )
        else success
    }

    private def findInvalidOutputs(
        outputs: IndexedSeq[Sized[TransactionOutput]],
        protocolParams: ProtocolParams
    ): IndexedSeq[(TransactionOutput, Coin, MultiAsset)] = {
        (
          for
              sizedOutput @ SizedValue(output) <- outputs.view
              minAda = MinCoinSizedTransactionOutput.extractMinAda(sizedOutput, protocolParams)
              negativeAssets = output.value.assets.negativeAssets
              if output.value.coin < minAda || negativeAssets.nonEmpty
          yield (output, minAda, negativeAssets)
        ).toIndexedSeq
    }
}
