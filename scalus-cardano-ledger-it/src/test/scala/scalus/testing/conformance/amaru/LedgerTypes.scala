package scalus.testing.conformance.amaru

import scalus.builtin.ByteString
import LedgerTypes.DRep.{KeyHash, ScriptHash}

import scala.collection.immutable.SortedMap

/** Core types for ledger validation, matching Amaru's data structures */
object LedgerTypes:

    type Epoch = Int
    type Lovelace = BigInt
    type Slot = Long

    /** Stake credential (hash of staking key or script) */
    case class StakeCredential(bytes: ByteString):
        override def toString: String = bytes.toHex
    given Ordering[StakeCredential] = Ordering.by(_.bytes)

    /** Pool ID (hash of pool cold key) */
    case class PoolId(bytes: ByteString):
        override def toString: String = bytes.toHex
    given Ordering[PoolId] = Ordering.by(_.bytes)

    /** DRep (Delegated Representative) identifier */
    enum DRep:
        case KeyHash(bytes: ByteString)
        case ScriptHash(bytes: ByteString)
        case AlwaysAbstain
        case AlwaysNoConfidence
    given Ordering[DRep] = Ordering.by[DRep, Int](_.ordinal).orElseBy {
        case KeyHash(bytes)    => bytes
        case ScriptHash(bytes) => bytes
        case _                 => ByteString.empty
    }

    /** Certificate pointer - identifies when a certificate was registered */
    case class CertificatePointer(
        slot: Slot,
        txIndex: Int,
        certIndex: Int
    )

    /** Anchor for metadata (URL + content hash) */
    case class Anchor(
        url: String,
        hash: ByteString
    )

    /** Safe rational number for precise calculations */
    case class SafeRatio(numerator: BigInt, denominator: BigInt):
        require(denominator > 0, "Denominator must be positive")

        def +(other: SafeRatio): SafeRatio =
            SafeRatio(
              numerator * other.denominator + other.numerator * denominator,
              denominator * other.denominator
            ).normalize

        def *(other: SafeRatio): SafeRatio =
            SafeRatio(numerator * other.numerator, denominator * other.denominator).normalize

        def *(value: BigInt): SafeRatio =
            SafeRatio(numerator * value, denominator).normalize

        def min(other: SafeRatio): SafeRatio =
            if this.toDouble <= other.toDouble then this else other

        def normalize: SafeRatio =
            val gcd = numerator.gcd(denominator)
            SafeRatio(numerator / gcd, denominator / gcd)

        def toDouble: Double = numerator.toDouble / denominator.toDouble

        override def toString: String = s"$numerator/$denominator"

    object SafeRatio:
        def zero: SafeRatio = SafeRatio(0, 1)
        def one: SafeRatio = SafeRatio(1, 1)
        def apply(num: BigInt, denom: BigInt): SafeRatio = new SafeRatio(num, denom)

    /** Treasury and reserve pots */
    case class Pots(
        treasury: Lovelace,
        reserves: Lovelace,
        fees: Lovelace
    )

    /** Pool parameters */
    case class PoolParams(
        pledge: Lovelace,
        cost: Lovelace,
        margin: SafeRatio,
        owners: Set[StakeCredential],
        rewardAccount: StakeCredential
    )

    /** Pool state in stake distribution */
    case class PoolState(
        params: PoolParams,
        stake: Lovelace,
        votingStake: Lovelace,
        blockCount: Int,
        retiring: Option[Epoch]
    )

    /** Account state in stake distribution */
    case class AccountState(
        lovelace: Lovelace,
        pool: Option[PoolId],
        drep: Option[DRep]
    )

    /** DRep state in governance */
    case class DRepState(
        validUntil: Option[Epoch],
        metadata: Option[Anchor],
        stake: Lovelace,
        registeredAt: CertificatePointer,
        previousDeregistration: Option[CertificatePointer]
    ):
        def isActive(epoch: Epoch): Boolean =
            validUntil.isEmpty || validUntil.exists(_ > epoch)

    /** Complete stake distribution for an epoch */
    case class StakeDistribution(
        epoch: Epoch,
        activeStake: Lovelace,
        poolsVotingStake: Lovelace,
        drepsVotingStake: Lovelace,
        accounts: SortedMap[StakeCredential, AccountState],
        pools: SortedMap[PoolId, PoolState],
        dreps: SortedMap[DRep, DRepState]
    )

    /** Pool rewards breakdown */
    case class PoolRewards(
        pot: Lovelace, // Total rewards for the pool
        leader: Lovelace, // Rewards for pool leaders/owners
        members: SortedMap[StakeCredential, Lovelace] // Rewards for delegators
    )

    /** Complete rewards summary for an epoch */
    case class RewardsSummary(
        epoch: Epoch,
        efficiency: SafeRatio,
        incentives: Lovelace,
        totalRewards: Lovelace,
        treasuryTax: Lovelace,
        availableRewards: Lovelace,
        effectiveRewards: Lovelace,
        pots: Pots,
        pools: SortedMap[PoolId, PoolRewards],
        accounts: SortedMap[StakeCredential, Lovelace]
    )

    /** Protocol parameters needed for calculations */
    case class ProtocolParams(
        minPoolCost: Lovelace,
        monetaryExpansionRate: SafeRatio,
        treasuryExpansionRate: SafeRatio,
        activeSlotCoeff: SafeRatio,
        epochLength: Int,
        optimalPoolCount: Int, // n_opt parameter
        poolPledgeInfluence: SafeRatio, // a0 parameter
        poolDeposit: Lovelace,
        drepActivity: Int, // DRep activity window in epochs
        minFeeRefScriptCostPerByte: SafeRatio
    )

    /** Helper to floor SafeRatio multiplication to Lovelace */
    def floorToLovelace(ratio: SafeRatio, value: BigInt): Lovelace =
        (ratio.numerator * value) / ratio.denominator

    /** Helper to create safe ratio from two integers */
    def safeRatio(num: BigInt, denom: BigInt): SafeRatio =
        if denom == 0 then SafeRatio.zero else SafeRatio(num, denom)
