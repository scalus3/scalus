package scalus.uplc.transform

import scalus.cardano.ledger.{CardanoInfo, ExUnits}
import scalus.uplc.Term
import scalus.uplc.eval.CekMachineCosts
import scalus.uplc.transform.CommonSubexpressionElimination.{cachedTermBits, termBits, TermTagBits, VarBits}

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
    private val bindingFee = lovelace(overhead)
    val lovelacePerBit: Double = params.minFeeRefScriptCostPerByte.toDouble / 8

    /** Keep fractional fees for optimizer estimates; ledger fees round only the final total. */
    def lovelace(cost: ExUnits): Double =
        cost.memory * params.executionUnitPrices.priceMemory.toDouble +
            cost.steps * params.executionUnitPrices.priceSteps.toDouble

    def savingBits(term: Term, uses: Int): Long = savingBits(termBits(term), uses)

    def savingBits(bits: Int, uses: Int): Long =
        (uses.toLong - 1) * bits - uses.toLong * VarBits - 2 * TermTagBits

    /** Positive means keep/share the binding; zero or negative favors inlining when safe. */
    def savingLovelace(term: Term, uses: Int): Double =
        cachedTermBits()(term).fold(Double.NegativeInfinity)(savingLovelace(_, uses))

    def savingLovelace(bits: Int, uses: Int): Double =
        savingBits(bits, uses) * lovelacePerBit - bindingFee
}
