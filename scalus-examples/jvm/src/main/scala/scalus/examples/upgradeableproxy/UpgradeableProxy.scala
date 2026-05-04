package scalus.examples.upgradeableproxy

import scalus.compiler.Compile

import scalus.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v1.{Credential, PubKeyHash}
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.uplc.builtin.{Data, FromData, ToData}
import scalus.cardano.onchain.plutus.v3.Validator

/** Upgradeable proxy pattern for Cardano smart contracts.
  *
  * The pattern works by having a spend validator ensure that a stake validator has been called by
  * checking that
  *   - a withdrawal has been made;
  *   - the withdrawal has been made from a known trusted script address.
  *
  * A combination of these conditions ensure that the validator has been called, thus allowing a
  * forced script composition.
  *
  * This also ensures that the logic can be changed (upgraded), by updating the known trusted script
  * address, that is stored in the datum. In this illustration, a proxy has an owner, which can
  * update the datum. In a real application, a more sophisticated approach should be preferred, to
  * ensure trustlessness.
  */

case class ProxyDatum(logicHash: ValidatorHash, owner: PubKeyHash) derives FromData, ToData

enum ProxyRedeemer derives FromData, ToData:
    case Call

    case Upgrade(newLogicHash: ValidatorHash)

/** Spending validator for the upgradeable proxy.
  *
  * Stores the active logic script hash in the datum. Two actions:
  *
  *   - `Call` - verifies the logic stake script was withdrawn and the datum is unchanged.
  *   - `Upgrade` - owner replaces the logic script hash; value must be preserved.
  */
@Compile
object ProxyValidator extends Validator {

    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val d = datum.getOrFail(MissingDatum).to[ProxyDatum]
        val r = redeemer.to[ProxyRedeemer]
        val ownInput = tx.findOwnInputOrFail(ownRef)

        val continuationOutput =
            tx.outputs
                .filter(out => out.address === ownInput.resolved.address)
                .headOption
                .getOrFail(MissingContinuation)

        val continuationDatum = continuationOutput.datum match
            case OutputDatum.OutputDatum(d) => d.to[ProxyDatum]
            case _                          => fail(ContinuationMustHaveInlineDatum)

        require(
          continuationOutput.value === ownInput.resolved.value,
          ValueMustBePreserved
        )

        r match
            case ProxyRedeemer.Call =>
                // Ensure the logic stake validator was called
                val logicCredential = Credential.ScriptCredential(d.logicHash)
                tx.withdrawals.getOrFail(logicCredential, LogicNotInvoked)

                // Ensure the proxy UTxO continues with the same datum (state preserved)
                require(continuationDatum.logicHash === d.logicHash, LogicHashChanged)
                require(continuationDatum.owner === d.owner, OwnerChanged)

            case ProxyRedeemer.Upgrade(newLogicHash) =>
                // Only the owner can upgrade the logic
                require(tx.isSignedBy(d.owner), NotSignedByOwner)

                // Continuation output must carry the updated datum
                require(continuationDatum.logicHash === newLogicHash, LogicHashMismatch)
                require(continuationDatum.owner === d.owner, OwnerChanged)
    }

    inline val MissingDatum = "Proxy datum must be present"
    inline val LogicNotInvoked = "Logic stake validator must be invoked in this transaction"
    inline val MissingContinuation = "Proxy continuation output not found"
    inline val ContinuationMustHaveInlineDatum = "Continuation output must have an inline datum"
    inline val LogicHashChanged = "Logic hash must not change on Call"
    inline val OwnerChanged = "Owner must not change"
    inline val NotSignedByOwner = "Transaction must be signed by the proxy owner"
    inline val LogicHashMismatch = "Continuation datum logic hash does not match upgrade target"
    inline val ValueMustBePreserved = "Proxy value must be preserved"
}
