package scalus.uplc.transform

import scalus.cardano.ledger.CardanoInfo
import scalus.uplc.Term
import scalus.uplc.eval.CekMachineCosts
import scalus.uplc.transform.CommonSubexpressionElimination.{termBits, TermTagBits, VarBits}

/** Shared profitability estimate for retaining or introducing a let binding.
  *
  * Prices one execution of the binding and the first reference-script fee tier. A one-node value
  * adds Apply + LamAbs + Var costs: n value evaluations become n lookups plus one retained value.
  * Larger expressions receive no credit for avoided computation. This is a heuristic, not a
  * prediction of total fees: execution frequency, byte alignment and index growth are unknown.
  */
private[transform] object SharingCost {
    private val params = CardanoInfo.mainnet.protocolParams
    private val costs = CekMachineCosts.defaultMachineCosts
    private val overhead = costs.applyCost + costs.lamCost + costs.varCost
    private val bindingFee =
        overhead.memory * params.executionUnitPrices.priceMemory.toDouble +
            overhead.steps * params.executionUnitPrices.priceSteps.toDouble
    private val pricePerBit = params.minFeeRefScriptCostPerByte.toDouble / 8

    def savingBits(term: Term, uses: Int): Long =
        (uses.toLong - 1) * termBits(term) - uses.toLong * VarBits - 2 * TermTagBits

    /** Positive means keep/share the binding; zero or negative favors inlining when safe. */
    def savingLovelace(term: Term, uses: Int): Double =
        savingBits(term, uses) * pricePerBit - bindingFee
}
