package scalus.testing.budget

import scalus.cardano.ledger.{CardanoInfo, ExUnits, RefScriptFee, Transaction}

/** One pinned measurement of a budget (spec "Budget pins", `[BP-1]` to `[BP-9]`).
  *
  * A pin is exact: a test fails when any field differs, also when the new value is better. So a
  * regression and an improvement both show up, and the table diff records the improvement.
  */
enum Pin {

    /** One script evaluation with runtime arguments.
      *
      * `size` is the size in bytes of the evaluated script as the ledger counts it (the script
      * without its arguments). `fee` is the combined fee in lovelace at mainnet parameters: the
      * execution fee of `memory` and `steps`, plus the reference-script fee of `size`. The model is
      * one evaluation per transaction, with the script as a reference script.
      */
    case Eval(memory: Long, steps: Long, size: Int, fee: Long)

    /** One balanced transaction under test. `memory` and `steps` are the sum over all redeemers,
      * `size` is the transaction's size in bytes as the fee rule counts it
      * (`toCborForFeeCalculation`), and `fee` is its fee in lovelace, as the balancer set it: the
      * size fee, the execution fee and the reference-script fee.
      */
    case Tx(memory: Long, steps: Long, size: Int, fee: Long)

    /** The size of a script in bytes (`[BP-5]`). */
    case Size(bytes: Int)
}

object Pin {

    private def params = CardanoInfo.mainnet.protocolParams

    /** The execution fee of a budget at mainnet prices, rounded up. */
    def executionFee(memory: Long, steps: Long): Long =
        ExUnits(memory, steps).fee(params.executionUnitPrices).value

    /** The Conway tiered reference-script fee of `size` bytes at mainnet parameters. */
    def referenceScriptFee(size: Int): Long =
        RefScriptFee.fee(size, params.minFeeRefScriptCostPerByte).value

    /** The size fee of a transaction of `size` bytes at mainnet parameters. */
    def sizeFee(size: Int): Long = params.txFeePerByte * size + params.txFeeFixed

    /** The pin of a balanced transaction. */
    def tx(transaction: Transaction): Pin.Tx = {
        val budget = transaction.witnessSet.redeemers
            .map(_.value.totalExUnits)
            .getOrElse(ExUnits.zero)
        Pin.Tx(
          budget.memory,
          budget.steps,
          transaction.toCborForFeeCalculation.length,
          transaction.body.value.fee.value
        )
    }

    /** The pin of one evaluation: the budget, the script size and the combined fee.
      *
      * The parameters come from the checked-in mainnet protocol parameters. A change of that
      * snapshot moves every fee pin, in one update run.
      */
    def eval(budget: ExUnits, size: Int): Pin.Eval =
        Pin.Eval(
          budget.memory,
          budget.steps,
          size,
          executionFee(budget.memory, budget.steps) + referenceScriptFee(size)
        )
}
