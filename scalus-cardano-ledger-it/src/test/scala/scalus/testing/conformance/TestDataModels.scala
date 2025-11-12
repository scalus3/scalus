package scalus.testing.conformance

import io.circe.Decoder
import io.circe.generic.semiauto.*

/** Models for parsing Amaru test data JSON files */
object TestDataModels:

    /** Helper type for nested lovelace amounts: {"ada": {"lovelace": amount}} */
    case class Lovelace(lovelace: BigInt)
    case class Ada(ada: Lovelace)

    /** Configuration for a network's test data */
    case class Config(
        points: List[Point],
        snapshots: List[Int],
        additionalStakeAddresses: Option[List[String]]
    )

    case class Point(
        epoch: Int,
        slot: Long,
        id: String
    )

    /** Stake pool information for an epoch */
    case class PoolData(
                           id: String,
                           vrfVerificationKeyHash: Option[String],
                           pledge: Ada,
                           cost: Ada,
                           margin: String, // Format: "numerator/denominator"
                           rewardAccount: String,
                           owners: List[String],
                           relays: List[Relay],
                           metadata: Option[PoolMetadata],
                           stake: Option[Ada]
    )

    case class Relay(
        `type`: String,
        hostname: Option[String],
        port: Option[Int],
        ipv4: Option[String],
        ipv6: Option[String]
    )

    case class PoolMetadata(
        url: String,
        hash: String
    )

    /** Rewards provenance data for an epoch */
    case class RewardsProvenance(
                                    totalStake: Ada,
                                    activeStake: Ada,
                                    fees: Ada,
                                    incentives: Ada,
                                    treasuryTax: Ada,
                                    totalRewards: Ada,
                                    efficiency: String, // Format: "numerator/denominator"
                                    stakePools: Map[String, StakePoolRewards]
    )

    case class StakePoolRewards(
                                   relativeStake: String, // Format: "numerator/denominator"
                                   blocksMade: Int,
                                   totalRewards: Ada,
                                   leaderReward: Ada,
                                   delegators: List[Delegator]
    )

    case class Delegator(
        from: String, // "verificationKey" or "script"
        credential: String,
        stake: Ada
    )

    /** DRep (Delegated Representative) data for governance */
    case class DRepData(
                           `type`: String, // "registered", "abstain", "no_confidence"
                           from: Option[String], // "verificationKey" or "script" (only for registered)
                           id: Option[String], // ID (only for registered DReps)
                           mandate: Option[Mandate],
                           deposit: Option[Ada], // Only for registered DReps
                           stake: Ada,
                           metadata: Option[Anchor],
                           delegators: List[DRepDelegator]
    )

    case class Mandate(
        epoch: Int
    )

    case class DRepDelegator(
        from: String,
        credential: String
    )

    case class Anchor(
        url: String,
        hash: String
    )

    /** Treasury and reserve pot balances */
    case class PotsData(
                           treasury: Ada,
                           reserves: Ada
    )

    // Circe decoders
    given Decoder[Lovelace] = deriveDecoder[Lovelace]
    given Decoder[Ada] = deriveDecoder[Ada]

    given Decoder[Config] = deriveDecoder[Config]
    given Decoder[Point] = deriveDecoder[Point]

    given Decoder[PoolData] = deriveDecoder[PoolData]
    given Decoder[Relay] = deriveDecoder[Relay]
    given Decoder[PoolMetadata] = deriveDecoder[PoolMetadata]

    given Decoder[Delegator] = deriveDecoder[Delegator]
    given Decoder[StakePoolRewards] = deriveDecoder[StakePoolRewards]
    given Decoder[RewardsProvenance] = deriveDecoder[RewardsProvenance]

    given Decoder[Mandate] = deriveDecoder[Mandate]
    given Decoder[DRepDelegator] = deriveDecoder[DRepDelegator]
    given Decoder[DRepData] = deriveDecoder[DRepData]
    given Decoder[Anchor] = deriveDecoder[Anchor]

    given Decoder[PotsData] = deriveDecoder[PotsData]

    /** Helper to parse rational from string "num/denom" */
    def parseRational(s: String): (BigInt, BigInt) =
        val parts = s.split("/")
        if parts.length == 2 then
            (BigInt(parts(0)), BigInt(parts(1)))
        else
            (BigInt(parts(0)), BigInt(1))

    /** Helper to extract lovelace amount */
    def extractLovelace(wrapper: Ada): BigInt = wrapper.ada.lovelace
