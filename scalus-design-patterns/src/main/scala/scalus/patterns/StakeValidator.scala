package scalus.patterns

import scalus.*
import scalus.prelude.*
import scalus.ledger.api.v3.{Credential, Lovelace, Redeemer, ScriptPurpose, TxInfo, ValidatorHash}

/** This pattern allows for delegating some computations to a given staking script. The primary
  * application for this is the so-called "withdraw zero trick," which is most effective for
  * validators that need to go over multiple inputs. With a minimal spending logic (which is
  * executed for each UTxO), and an arbitrary withdrawal logic (which is executed only once), a much
  * more optimized script can be implemented.
  *
  * @see
  *   [[https://github.com/Anastasia-Labs/design-patterns/tree/main/stake-validator]]
  */

@Compile
object StakeValidator {

    /** Helper function for implementing validation for spending UTxOs, essentially delegating their
      * requirements to the given withdrawal validator. In simpler terms, it says: As long as there
      * is a reward withdrawal of the given script in transaction, this UTxO can be spent. Allows
      * you to validate based on both the withdrawal's redeemer (mostly useful for ensuring specific
      * endpoints are invoked), and the withdrawal Lovelace count.
      */
    def spend(
        withdrawalScriptHash: ValidatorHash,
        withdrawalRedeemerValidator: (Redeemer, Lovelace) => Boolean,
        txInfo: TxInfo
    ): Unit =
        val scriptCredential = Credential.ScriptCredential(withdrawalScriptHash)
        val scriptPurpose = ScriptPurpose.Rewarding(scriptCredential)

        val redeemer = txInfo.redeemers.getOrFail(scriptPurpose, MissingRedeemer)
        val withdrawalAmount = txInfo.withdrawals.getOrFail(scriptCredential, MissingWithdrawal)

        require(
          withdrawalRedeemerValidator(redeemer, withdrawalAmount),
          WithdrawalRedeemerValidatorFailed
        )

    /** A more minimal version of [`spend`](#spend), where only the `withdrawals` field is
      * traversed, and no other validations are performed.
      */
    def spendMinimal(withdrawalScriptHash: ValidatorHash, txInfo: TxInfo): Unit =
        val scriptCredential = Credential.ScriptCredential(withdrawalScriptHash)
        txInfo.withdrawals.getOrFail(scriptCredential, MissingWithdrawal)

    /** Function to be used under your withdrawal endpoint. The only convenience this function
      * provides is that it'll provide you with the `ScriptHash` of your withdrawal script, so that
      * you don't have to unwrap it yourself.
      */
    def withdraw[T](
        withdrawalValidator: (T, ValidatorHash, TxInfo) => Boolean,
        redeemer: T,
        credential: Credential,
        txInfo: TxInfo
    ): Unit =
        val validatorHash = credential.scriptOption.getOrFail(PubKeyCredentialNotSupported)
        require(withdrawalValidator(redeemer, validatorHash, txInfo), WithdrawalValidatorFailed)

    inline val MissingRedeemer = "There isn't a redeemer for the script purpose"
    inline val MissingWithdrawal = "There isn't a withdrawal for the script credential"
    inline val WithdrawalRedeemerValidatorFailed = "Withdrawal redeemer validator failed"
    inline val WithdrawalValidatorFailed = "Withdrawal validator failed"
    inline val PubKeyCredentialNotSupported = "PubKeyCredential not supported"

}
