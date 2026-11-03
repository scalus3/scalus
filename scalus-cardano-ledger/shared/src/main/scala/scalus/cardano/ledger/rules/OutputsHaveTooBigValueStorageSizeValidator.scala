package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.Value as LedgerValue
import scalus.serialization.cbor.Cbor

// It's Babbage.validateOutputTooBigUTxO in cardano-ledger
object OutputsHaveTooBigValueStorageSizeValidator extends STS.Validator {
    override final type Error = TransactionException.OutputsHaveTooBigValueStorageSizeException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val maxValueSize = context.env.params.maxValueSize
        val outputs = event.body.value.outputs
        val collateralReturnOutput = event.body.value.collateralReturnOutput.toIndexedSeq

        val invalidOutputs = findInvalidOutputs(outputs, maxValueSize)

        val invalidCollateralReturnOutput =
            findInvalidOutputs(collateralReturnOutput, maxValueSize).headOption

        if invalidOutputs.nonEmpty || invalidCollateralReturnOutput.nonEmpty then
            failure(
              TransactionException.OutputsHaveTooBigValueStorageSizeException(
                transactionId,
                maxValueSize,
                invalidOutputs,
                invalidCollateralReturnOutput
              )
            )
        else success
    }

    private def findInvalidOutputs(
        outputs: IndexedSeq[Sized[TransactionOutput]],
        maxValueSize: Long
    ): IndexedSeq[(TransactionOutput, Int)] = {
        (
          for
              SizedValue(output) <- outputs.view
              outputValueSerializationSize = ledgerSerializedSize(output.value)
              if outputValueSerializationSize > maxValueSize
          yield (output, outputValueSerializationSize)
        ).toIndexedSeq
    }

    /** The byte length of `serialize (pvMajor protVer) v`, the size the ledger compares with
      * `maxValueSize` (eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Rules/Utxo.hs:444-445).
      *
      * The size does not depend on the PV in Conway (spec [D101]): the `MaryValue` encoder writes a
      * bare coin when there is no multi-asset, else `[coin, multiAsset]`, at every version
      * (eras/mary/impl/src/Cardano/Ledger/Mary/Value.hs:334-342), and so does the Scalus encoder.
      * The ledger's Map encoder differs in one way (spec [D115]): from version 2 on, a map of more
      * than 23 entries has an indefinite length, a 2-byte head with its break
      * (libs/cardano-ledger-binary/src/Cardano/Ledger/Binary/Encoding/Encoder.hs:395-425). Scalus
      * writes a definite head, which is 3 bytes from 256 entries on and 5 bytes from 65 536 on, so
      * the extra bytes of each such map come off the Scalus size. For example, one policy of 256
      * assets is one byte smaller in the ledger.
      */
    private def ledgerSerializedSize(value: LedgerValue): Int = {
        val maps = value.assets.assets
        val sizes =
            if maps.isEmpty then Iterator.empty
            else Iterator(maps.size) ++ maps.valuesIterator.map(_.size)
        Cbor.encode(value).length - sizes.map(extraHeadBytes).sum
    }

    /** The bytes a definite map head of `entries` takes beyond the ledger's head. */
    private def extraHeadBytes(entries: Int): Int =
        if entries < 256 then 0 // 1 byte up to 23 in both; 2 bytes from 24 on in both
        else if entries < 65536 then 1
        else 3
}
