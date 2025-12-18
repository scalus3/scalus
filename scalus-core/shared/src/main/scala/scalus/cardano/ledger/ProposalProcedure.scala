package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*

/** Represents a proposal procedure in the Cardano blockchain governance system.
  *
  * A proposal procedure consists of a deposit, reward account, governance action, and an anchor
  * with additional metadata.
  *
  * @param deposit
  *   The deposit amount for the proposal
  * @param rewardAccount
  *   The reward account for returning the deposit
  * @param govAction
  *   The governance action being proposed
  * @param anchor
  *   The anchor with additional metadata
  */
case class ProposalProcedure(
    deposit: Coin,
    rewardAccount: RewardAccount,
    govAction: GovAction,
    anchor: Anchor
) derives Codec

object ProposalProcedure {

    /** Ordering matches Haskell's derived Ord: compare fields in declaration order (deposit,
      * rewardAccount, govAction, anchor)
      */
    given Ordering[ProposalProcedure] =
        Ordering.by(p => (p.deposit.value, p.rewardAccount, p.govAction, p.anchor))
}
