package scalus.cardano.txbuilder

import scalus.cardano.ledger.*

/** Encapsulates UTXO selection state during transaction balancing.
  *
  * Separates available pool from already-selected UTXOs, enabling: - Greedy selection from
  * remaining UTXOs - Reusing input UTXOs for collateral (without double-counting) - Clear tracking
  * of what's selected for inputs vs collateral
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

    /** Select UTXOs greedily to cover the required value.
      *
      * First covers tokens (since they're constrained), then ADA.
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

        // First, select UTXOs that have required tokens
        remaining.foreach { case (input, output) =>
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

        // Then, select UTXOs for ADA if still needed
        if remainingAda > 0 then {
            remaining.foreach { case (input, output) =>
                if !selected.contains(input) && remainingAda > 0 then {
                    selected = selected + (input -> output)
                    remainingAda -= output.value.coin.value
                }
            }
        }

        selected
    }

    /** Select UTXOs for collateral, preferring ADA-only UTXOs.
      *
      * Can reuse UTXOs already selected for inputs - same UTXO can be in both inputs and
      * collateralInputs (collateral only consumed if scripts fail).
      *
      * @param requiredAmount
      *   the amount of ADA required for collateral
      * @return
      *   selected UTXOs for collateral
      */
    def selectForCollateral(requiredAmount: Coin): Utxos = {
        // Combine remaining and already-selected for inputs (can reuse for collateral)
        val allCandidates = remainingForInputs ++ selectedForInputs

        // Prefer ADA-only UTXOs (simpler, no tokens to return)
        val adaOnlyUtxos = allCandidates.filter { case (_, output) =>
            output.value.assets.isEmpty
        }

        var selected = Map.empty[TransactionInput, TransactionOutput]
        var accumulated = 0L

        // First try ADA-only
        adaOnlyUtxos.foreach { case (input, output) =>
            if accumulated < requiredAmount.value then {
                selected = selected + (input -> output)
                accumulated += output.value.coin.value
            }
        }

        // If not enough, use any UTXOs (tokens will be returned via collateral return output)
        if accumulated < requiredAmount.value then {
            allCandidates.foreach { case (input, output) =>
                if !selected.contains(input) && accumulated < requiredAmount.value then {
                    selected = selected + (input -> output)
                    accumulated += output.value.coin.value
                }
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
