package scalus.patterns

import scalus.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v3.*

/** This design pattern couples the spend and minting endpoints of a validator, in order to have
  * minimal spend costs, in exchange for a single execution of the minting endpoint. In other words,
  * spend logic only ensures the minting endpoint executes. It does so by looking at the mint field
  * and making sure **only** a non-zero amount of its asset (i.e. with a policy identical to the
  * validator's hash, where its name comes from `expected_mint_name`) are getting minted/burnt. The
  * arbitrary logic is passed to the minting policy so that it can be executed a single time for a
  * given transaction.
  *
  * @see
  *   [[https://github.com/Anastasia-Labs/design-patterns/blob/main/transaction-level-validator-minting-policy/TRANSACTION-LEVEL-VALIDATION-MINTING-POLICY.md]]
  */
@Compile
object TransactionLevelMinterValidator {

    /** Function to be used under the spending endpoint of your validator. It looks at both the
      * redeemers, and minted tokens to allow you validate both its redeemer, and its tokens getting
      * minted/burnt.
      */
    def spend(
        minterScriptHash: ValidatorHash,
        minterRedeemerValidator: Redeemer => Boolean,
        minterTokensValidator: SortedMap[TokenName, BigInt] => Boolean,
        txInfo: TxInfo
    ): Unit =
        val scriptPurpose = ScriptPurpose.Minting(minterScriptHash)
        val tokens = txInfo.mint.toSortedMap.get(minterScriptHash).getOrElse(SortedMap.empty)

        val redeemer: Redeemer = txInfo.redeemers.getOrFail(scriptPurpose, MissingRedeemer)
        require(minterRedeemerValidator(redeemer), MinterRedeemerValidatorFailed)
        require(minterTokensValidator(tokens), MinterTokensValidatorFailed)

    /** A minimal version of [`spend`](#spend), where the only validation is presence of at least
      * one minting/burning action with the given policy ID.
      */
    def spendMinimal(minterScriptHash: ValidatorHash, txInfo: TxInfo): Unit =
        txInfo.mint.toSortedMap.getOrFail(minterScriptHash, MissingMint)

    inline val MissingRedeemer = "There isn't a redeemer for the script purpose"
    inline val MinterRedeemerValidatorFailed = "Minter redeemer validator failed"
    inline val MinterTokensValidatorFailed = "Minter tokens validator failed"
    inline val MissingMint = "There isn't a mint for the minter script hash"
}
