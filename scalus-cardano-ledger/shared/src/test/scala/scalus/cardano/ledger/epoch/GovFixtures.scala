package scalus.cardano.ledger.epoch

import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*
import scalus.uplc.builtin.ByteString

/** The ids, accounts and thresholds that the RATIFY, ENACT and epoch-boundary tests share. */
object GovFixtures {

    /** The gov action id of tx `i`, index 0. */
    def actionId(i: Int): GovActionId =
        GovActionId(TransactionHash.fromByteString(ByteString.fromHex(f"$i%02x" * 32)), 0)

    /** The mainnet reward account of the stake key hash `i`. */
    def rewardAccount(i: Int): RewardAccount =
        RewardAccount(
          StakeAddress(Network.Mainnet, StakePayload.Stake(StakeKeyHash.fromHex(f"$i%02x" * 28)))
        )

    /** The credential of [[rewardAccount]] `i`. */
    def credential(i: Int): Credential = rewardAccount(i).address.credential

    /** Every SPO threshold at 0. */
    val zeroPools: PoolVotingThresholds = {
        val z = UnitInterval.zero
        PoolVotingThresholds(z, z, z, z, z)
    }

    /** Every DRep threshold at 0. */
    val zeroDReps: DRepVotingThresholds = {
        val z = UnitInterval.zero
        DRepVotingThresholds(z, z, z, z, z, z, z, z, z, z)
    }
}
