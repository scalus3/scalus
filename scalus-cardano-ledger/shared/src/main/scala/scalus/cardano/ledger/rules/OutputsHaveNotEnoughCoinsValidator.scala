package scalus.cardano.ledger
package rules

// It's Babbage.validateOutputTooSmallUTxO in cardano-ledger
object OutputsHaveNotEnoughCoinsValidator extends STS.Validator {
    override final type Error = TransactionException.OutputsHaveNotEnoughCoinsException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val utxoCostPerByte = context.env.params.utxoCostPerByte
        val outputs = event.body.value.outputs
        val collateralReturnOutput = event.body.value.collateralReturnOutput.toIndexedSeq

        val invalidOutputs = findInvalidOutputs(outputs, utxoCostPerByte)

        val invalidCollateralReturnOutput =
            findInvalidOutputs(collateralReturnOutput, utxoCostPerByte).headOption

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
        utxoCostPerByte: Long
    ): IndexedSeq[(TransactionOutput, Coin, MultiAsset)] = {
        (
          for
              Sized(output, size) <- outputs.view
              minAda = Coin((constantOverhead + size) * utxoCostPerByte)
              negativeAssets = output.value.assets.negativeAssets
              if output.value.coin < minAda || negativeAssets.nonEmpty
          yield (output, minAda, negativeAssets)
        ).toIndexedSeq
    }

    private val constantOverhead = 160
}
