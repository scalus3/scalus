package scalus.patterns

import scalus.*
import scalus.builtin.Data
import scalus.ledger.api.v3.{Credential, Redeemer, ScriptPurpose, TxInfo, ValidatorHash}
import scalus.prelude.*

/** Merkelized Validators Pattern
  *
  * This pattern allows offloading expensive computation to a stake validator that runs once per
  * transaction, while spending validators remain minimal. The stake validator verifies that
  * pre-computed values (provided in the redeemer) are correct, and spending validators simply check
  * that the stake validator executed.
  *
  * The pattern works as follows:
  *   1. Off-chain code computes expensive function: `output = f(input)`
  *   2. Off-chain includes both `input` and `output` in stake validator's redeemer
  *   3. Stake validator verifies: `f(input) == output` (runs once)
  *   4. Spending validator checks stake validator ran (runs per UTxO, minimal)
  *   5. Spending validator can read verified `output` from `txInfoRedeemers`
  *
  * Benefits:
  *   - When spending N UTxOs: O(N) instead of O(N²) for iteration-heavy logic
  *   - Spending validator stays small and cheap
  *   - Heavy computation logic can be optimized in the stake validator
  *
  * @see
  *   [[https://github.com/Anastasia-Labs/design-patterns/blob/main/merkelized-validators/merkelized-validators.md]]
  */
@Compile
object MerkelizedValidator {

    /** Retrieve the redeemer used by a stake validator from txInfoRedeemers.
      *
      * This allows the spending validator to access the verified output state without re-computing
      * the expensive function.
      *
      * @param stakeValidatorHash
      *   The hash of the stake validator
      * @param txInfo
      *   The transaction info containing redeemers
      * @return
      *   The redeemer data used by the stake validator
      */
    def getStakeRedeemer(stakeValidatorHash: ValidatorHash, txInfo: TxInfo): Redeemer =
        val scriptCredential = Credential.ScriptCredential(stakeValidatorHash)
        val scriptPurpose = ScriptPurpose.Rewarding(scriptCredential)
        txInfo.redeemers.getOrFail(scriptPurpose, MissingStakeRedeemer)

    /** Verify that a stake validator executed and return its redeemer.
      *
      * Combines the withdrawal check with redeemer retrieval for convenience.
      *
      * @param stakeValidatorHash
      *   The hash of the stake validator
      * @param txInfo
      *   The transaction info
      * @return
      *   The redeemer data used by the stake validator
      */
    def verifyAndGetRedeemer(stakeValidatorHash: ValidatorHash, txInfo: TxInfo): Redeemer =
        val scriptCredential = Credential.ScriptCredential(stakeValidatorHash)
        // Verify withdrawal exists (stake validator executed)
        txInfo.withdrawals.getOrFail(scriptCredential, MissingWithdrawal)
        // Return the redeemer
        val scriptPurpose = ScriptPurpose.Rewarding(scriptCredential)
        txInfo.redeemers.getOrFail(scriptPurpose, MissingStakeRedeemer)

    inline val MissingStakeRedeemer = "Stake validator redeemer not found in txInfoRedeemers"
    inline val MissingWithdrawal = "Stake validator withdrawal not found"
}
