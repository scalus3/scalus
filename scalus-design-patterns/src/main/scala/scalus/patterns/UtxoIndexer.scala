package scalus.patterns

import scalus.*
import scalus.ledger.api.v3.{TxInInfo, TxInfo, TxOutRef}
import scalus.ledger.api.v2.TxOut
import scalus.prelude.*

/** Indexer pattern for matching inputs to outputs by index. Indices are computed off-chain using
  * SpendWithDelayedRedeemer after the transaction is built.
  *
  * @see
  *   [[https://github.com/Anastasia-Labs/aiken-design-patterns/tree/main/lib/aiken-design-patterns]]
  */
@Compile
object UtxoIndexer {

    /** Validates a one-to-one relationship between an input and output at specified indices.
      *
      * @param ownRef
      *   The output reference being spent
      * @param inputIdx
      *   The index of the input in tx.inputs
      * @param outputIdx
      *   The index of the output in tx.outputs
      * @param tx
      *   The transaction info
      * @param validator
      *   Custom validation function for the input-output pair
      */
    inline def oneToOne(
        ownRef: TxOutRef,
        inputIdx: BigInt,
        outputIdx: BigInt,
        tx: TxInfo,
        validator: (TxInInfo, TxOut) => Boolean
    ): Unit = {
        val input = tx.inputs.at(inputIdx)
        require(input.outRef === ownRef, InputIndexMismatch)

        val output = tx.outputs.at(outputIdx)
        require(validator(input, output), ValidatorFailed)
    }

    inline val InputIndexMismatch = "Input index does not match ownRef"
    inline val ValidatorFailed = "Validator failed for input-output pair"
}
