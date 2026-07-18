package scalus.cardano.txbuilder

import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput

/** Encapsulates UTXO selection state during transaction balancing.
  *
  * Separates available pool from already-selected UTXOs, enabling:
  *   - Greedy selection from remaining UTXOs
  *   - Reusing input UTXOs for collateral (without double-counting)
  *   - Clear tracking of what's selected for inputs vs collateral
  *
  * @param available
  *   all UTXOs available for selection (never changes)
  * @param selectedForInputs
  *   UTXOs selected as transaction inputs
  * @param selectedForCollateral
  *   UTXOs selected as collateral (may overlap with inputs)
  */
private[txbuilder] class UtxoPool(
    private val available: Utxos,
    private val selectedForInputs: Utxos,
    private val selectedForCollateral: Utxos
) {

    /** UTXOs not yet selected for inputs. */
    def remainingForInputs: Utxos = available -- selectedForInputs.keySet

    /** All UTXOs selected as transaction inputs. */
    def inputs: Utxos = selectedForInputs

    /** All UTXOs selected as collateral. */
    def collateral: Utxos = selectedForCollateral

    /** Returns a new pool with additional UTXOs selected for inputs. */
    def withInputs(additional: Utxos): UtxoPool =
        new UtxoPool(available, selectedForInputs ++ additional, selectedForCollateral)

    /** Returns a new pool with additional UTXOs selected for collateral. */
    def withCollateral(additional: Utxos): UtxoPool =
        new UtxoPool(available, selectedForInputs, selectedForCollateral ++ additional)

    /** Total ADA available across all UTXOs (for error messages). */
    def totalAvailableAda: Coin =
        Coin(available.values.foldLeft(0L)((acc, o) => acc + o.value.coin.value))

    /** Total of specific token available across all UTXOs (for error messages). */
    def totalAvailableTokens(policyId: PolicyId, assetName: AssetName): Long =
        available.values
            .flatMap(_.value.assets.assets.get(policyId).flatMap(_.get(assetName)))
            .sum

    /** Select UTXOs to cover the required value.
      *
      * Selection strategy:
      *   - First selects UTXOs containing required tokens (these are mandatory)
      *   - Then for remaining ADA: finds smallest sufficient single UTxO, or multiple largest-first
      *
      * UTXOs carrying a reference script are only used as a last resort: spending one destroys the
      * deployed script for everyone who references it, and also incurs the Conway tiered ref-script
      * fee.
      *
      * @param required
      *   the value to cover
      * @return
      *   selected UTXOs from the remaining pool
      */
    def selectForValue(required: Value): Utxos = {
        val remaining = remainingForInputs
        var selected = Map.empty[TransactionInput, TransactionOutput]
        var remainingAda = required.coin.value
        var remainingTokens = required.assets

        // First, select UTXOs that have required tokens (mandatory - tokens are constrained).
        // Plain UTXOs are considered before ref-script ones.
        val tokenCandidates = remaining.toSeq.sortBy { case (_, output) =>
            output.scriptRef.isDefined
        }
        tokenCandidates.foreach { case (input, output) =>
            if !selected.contains(input) then {
                val hasNeededTokens = remainingTokens.assets.exists { case (policy, assets) =>
                    output.value.assets.assets.get(policy).exists { outputAssets =>
                        assets.exists { case (name, amount) =>
                            amount > 0 && outputAssets.getOrElse(name, 0L) > 0
                        }
                    }
                }
                if hasNeededTokens then {
                    selected = selected + (input -> output)
                    remainingAda -= output.value.coin.value
                    remainingTokens = remainingTokens - output.value.assets
                }
            }
        }

        // Selects UTXOs for ADA from candidates (sorted by ADA ascending):
        // smallest sufficient single UTxO, or multiple largest-first
        def selectForAda(sortedCandidates: Seq[(TransactionInput, TransactionOutput)]): Unit = {
            val smallestSufficient = sortedCandidates.find { case (_, output) =>
                output.value.coin.value >= remainingAda
            }

            smallestSufficient match {
                case Some((input, output)) =>
                    // Found a single UTxO that covers the requirement
                    selected = selected + (input -> output)
                    remainingAda -= output.value.coin.value

                case None =>
                    // No single UTxO covers it, select multiple starting with largest
                    sortedCandidates.reverse.foreach { case (input, output) =>
                        if remainingAda > 0 then {
                            selected = selected + (input -> output)
                            remainingAda -= output.value.coin.value
                        }
                    }
            }
        }

        // Then, select UTXOs for ADA if still needed: plain UTXOs first,
        // ref-script UTXOs only if plain ones cannot cover the requirement
        if remainingAda > 0 then {
            val (refScriptUtxos, plainUtxos) = remaining
                .filterNot { case (input, _) => selected.contains(input) }
                .partition { case (_, output) => output.scriptRef.isDefined }

            selectForAda(plainUtxos.toSeq.sortBy { case (_, output) => output.value.coin.value })
            if remainingAda > 0 && refScriptUtxos.nonEmpty then
                selectForAda(refScriptUtxos.toSeq.sortBy { case (_, output) =>
                    output.value.coin.value
                })
        }

        selected
    }

    /** Select UTXOs for collateral, preferring ADA-only UTXOs.
      *
      * Can reuse UTXOs already selected for inputs - same UTXO can be in both inputs and
      * collateralInputs (collateral only consumed if scripts fail).
      *
      * Selection strategy:
      *   1. First tries to find an "optimal" ADA-only UTxO where the excess is below minAda
      *      threshold, so no collateralReturn output is needed
      *   2. Otherwise finds smallest sufficient single UTxO
      *   3. If no single UTxO covers it, selects multiple starting with largest
      *   4. Falls back to UTxOs with tokens if ADA-only UTxOs are insufficient
      *   5. UTXOs carrying a reference script are the absolute last resort: if the scripts fail
      *      on-chain, collateral is consumed and the deployed script is destroyed
      *
      * @param requiredAmount
      *   the amount of ADA required for collateral
      * @param protocolParams
      *   protocol parameters for computing minAda
      * @return
      *   selected UTXOs for collateral
      */
    def selectForCollateral(requiredAmount: Coin, protocolParams: ProtocolParams): Utxos = {
        val required = requiredAmount.value

        // All UTxOs are candidates (can reuse inputs for collateral),
        // but ref-script UTXOs are deferred to the last-resort strategy 5
        val (refScriptUtxos, plainUtxos) = available.partition { case (_, output) =>
            output.scriptRef.isDefined
        }
        val adaOnlyUtxos = plainUtxos.filter { case (_, output) =>
            output.value.assets.isEmpty
        }

        // Sort ADA-only UTxOs by ADA amount ascending
        val adaOnlySorted = adaOnlyUtxos.toSeq.sortBy { case (_, output) =>
            output.value.coin.value
        }

        // Strategy 1: Try to find an optimal UTxO (covers requirement, excess < minAda for return)
        // The minAda for return depends on the UTxO's address (collateral return goes to same address)
        val optimalUtxo = adaOnlySorted.find { case (_, output) =>
            val ada = output.value.coin.value
            // Compute minAda for a potential return output to this address
            val minimalReturnOutput = TransactionOutput(output.address, Value.zero)
            val minAdaForReturn =
                MinCoinSizedTransactionOutput.ensureMinAda(
                  Sized(minimalReturnOutput),
                  protocolParams
                )
            ada >= required && (ada - required) < minAdaForReturn.value
        }

        if optimalUtxo.isDefined then {
            return Map(optimalUtxo.get)
        }

        // Strategy 2: Find smallest sufficient single UTxO
        val smallestSufficient = adaOnlySorted.find { case (_, output) =>
            output.value.coin.value >= required
        }

        if smallestSufficient.isDefined then {
            return Map(smallestSufficient.get)
        }

        // Strategy 3: No single ADA-only UTxO covers it, select multiple starting with largest
        var selected = Map.empty[TransactionInput, TransactionOutput]
        var accumulated = 0L

        adaOnlySorted.reverse.foreach { case (input, output) =>
            if accumulated < required then {
                selected = selected + (input -> output)
                accumulated += output.value.coin.value
            }
        }

        // If ADA-only UTxOs are sufficient, return them
        if accumulated >= required then {
            return selected
        }

        // Strategy 4: Need to use UTxOs with tokens as well
        // Sort token UTxOs by ADA ascending to find smallest sufficient first
        val tokenUtxosSorted = plainUtxos
            .filter { case (_, output) => output.value.assets.nonEmpty }
            .toSeq
            .sortBy { case (_, output) => output.value.coin.value } // ascending

        // Compute remaining needed after ADA-only selection
        val remainingRequired = required - accumulated

        // Try to find smallest sufficient single token UTxO
        val smallestSufficientToken = tokenUtxosSorted.find { case (_, output) =>
            output.value.coin.value >= remainingRequired
        }

        if smallestSufficientToken.isDefined then {
            return selected + smallestSufficientToken.get
        }

        // Otherwise, select multiple starting with largest
        tokenUtxosSorted.reverse.foreach { case (input, output) =>
            if accumulated < required then {
                selected = selected + (input -> output)
                accumulated += output.value.coin.value
            }
        }

        if accumulated >= required then {
            return selected
        }

        // Strategy 5: Last resort - UTxOs carrying reference scripts
        val refScriptSorted = refScriptUtxos.toSeq
            .sortBy { case (_, output) => output.value.coin.value } // ascending

        val remainingAfterPlain = required - accumulated
        val smallestSufficientRefScript = refScriptSorted.find { case (_, output) =>
            output.value.coin.value >= remainingAfterPlain
        }

        if smallestSufficientRefScript.isDefined then {
            return selected + smallestSufficientRefScript.get
        }

        refScriptSorted.reverse.foreach { case (input, output) =>
            if accumulated < required then {
                selected = selected + (input -> output)
                accumulated += output.value.coin.value
            }
        }

        selected
    }
}

private[txbuilder] object UtxoPool {

    /** Creates a pool with no initial selections. */
    def apply(available: Utxos): UtxoPool =
        new UtxoPool(available, Map.empty, Map.empty)
}
