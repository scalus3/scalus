package scalus.cardano.ledger
package rules

import scalus.serialization.cbor.Cbor

// It's Babbage.validateOutputTooBigUTxO in cardano-ledger
object OutputsHaveTooBigValueStorageSizeValidator extends STS.Validator {
    override final type Error = TransactionException.OutputsHaveTooBigValueStorageSizeException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val maxValueSize = context.env.params.maxValueSize
        val outputs = event.body.value.outputs
        val collateralReturnOutput = event.body.value.collateralReturnOutput

        val invalidOutputs = findInvalidOutputs(outputs, maxValueSize)

        val invalidCollateralReturnOutput = findInvalidOutputs(
          collateralReturnOutput.map(IndexedSeq.apply(_)).getOrElse(IndexedSeq.empty),
          maxValueSize
        ).headOption

        if invalidOutputs.nonEmpty || invalidCollateralReturnOutput.nonEmpty then
            return failure(
              TransactionException.OutputsHaveTooBigValueStorageSizeException(
                transactionId,
                maxValueSize,
                invalidOutputs,
                invalidCollateralReturnOutput
              )
            )

        success
    }

    private def findInvalidOutputs(
        outputs: IndexedSeq[Sized[TransactionOutput]],
        maxValueSize: Long,
    ): IndexedSeq[(TransactionOutput, Int)] = {
        for
            SizedValue(output) <- outputs
            // TODO maybe make serialization depending on the protocol version
            // serSize = fromIntegral $ BSL.length $ serialize (pvMajor protVer) v
            outputValueSerializationSize = Cbor.encode(output.value).length
            if outputValueSerializationSize > maxValueSize
        yield (output, outputValueSerializationSize)
    }
}
