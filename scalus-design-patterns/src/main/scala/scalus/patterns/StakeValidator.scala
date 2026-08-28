package scalus.patterns

import scalus.compiler.Compile

import scalus.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v3.{Credential, Lovelace, Redeemer, ScriptPurpose, TxInfo, ValidatorHash}

/** This pattern allows for delegating some computations to a given staking script. The primary
  * application for this is the so-called "withdraw zero trick," which is most effective for
  * validators that need to go over multiple inputs. With a minimal spending logic (which is
  * executed for each UTxO), and an arbitrary withdrawal logic (which is executed only once), a much
  * more optimized script can be implemented.
  *
  * @see
  *   [[scalus.examples.StakeValidatorPaymentSplitterExample]]
  * @see
  *   [[scalus.examples.MultiPoolDexExample]]
  * @see
  *   [[https://github.com/Anastasia-Labs/design-patterns/tree/main/stake-validator]]
  */

@Compile
object StakeValidator {

    /** Helper function for implementing validation for spending UTxOs, essentially delegating their
      * requirements to the given withdrawal validator. In simpler terms, it says: As long as there
      * is a reward withdrawal of the given script in transaction, this UTxO can be spent. Allows
      * you to validate based on both the withdrawal's redeemer (mostly useful for ensuring specific
      * endpoints are invoked), and the withdrawal Lovelace count. The validator returns `Unit` and
      * fails with its own message.
      */
    def spend(
        withdrawalScriptHash: ValidatorHash,
        withdrawalRedeemerValidator: (Redeemer, Lovelace) => Unit,
        txInfo: TxInfo
    ): Unit =
        val scriptCredential = Credential.ScriptCredential(withdrawalScriptHash)
        val scriptPurpose = ScriptPurpose.Rewarding(scriptCredential)

        val redeemer = txInfo.redeemers.getOrFail(scriptPurpose, MissingRedeemer)
        val withdrawalAmount = txInfo.withdrawals.getOrFail(scriptCredential, MissingWithdrawal)

        withdrawalRedeemerValidator(redeemer, withdrawalAmount)

    /** A more minimal version of [`spend`](#spend), where only the `withdrawals` field is
      * traversed, and no other validations are performed.
      *
      * This proves that the withdrawal script RAN, not which redeemer it ran with. If the
      * withdrawal script has more than one branch, any branch satisfies this check; use [`spend`]
      * with a redeemer validator to pin the endpoint.
      */
    def spendMinimal(withdrawalScriptHash: ValidatorHash, txInfo: TxInfo): Unit =
        val scriptCredential = Credential.ScriptCredential(withdrawalScriptHash)
        txInfo.withdrawals.getOrFail(scriptCredential, MissingWithdrawal)

    /** Function to be used under your withdrawal endpoint. The only convenience this function
      * provides is that it'll provide you with the `ScriptHash` of your withdrawal script, so that
      * you don't have to unwrap it yourself.
      */
    def withdraw[T](
        withdrawalValidator: (T, ValidatorHash, TxInfo) => Unit,
        redeemer: T,
        credential: Credential,
        txInfo: TxInfo
    ): Unit =
        val validatorHash = credential.scriptHashOrFail(PubKeyCredentialNotSupported)
        withdrawalValidator(redeemer, validatorHash, txInfo)

    inline val MissingRedeemer = "There isn't a redeemer for the script purpose"
    inline val MissingWithdrawal = "There isn't a withdrawal for the script credential"
    inline val PubKeyCredentialNotSupported = "PubKeyCredential not supported"

}
