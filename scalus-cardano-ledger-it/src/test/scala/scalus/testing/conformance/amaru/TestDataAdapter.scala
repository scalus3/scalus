package scalus.testing.conformance.amaru

import scalus.builtin.ByteString
import scalus.cardano.address.Address
import scalus.cardano.ledger.Bech32
import LedgerTypes.*

import scala.math.Ordering.Implicits.seqOrdering
import scala.util.Try

/** Adapter to convert Amaru test data into LedgerSnapshot
  *
  * This bridges the gap between Amaru's JSON test data format and Scalus's ledger validation
  * types.
  */
object TestDataAdapter:
    import TestDataModels.*

    /** Convert Amaru test data to LedgerSnapshot
      *
      * @param network
      *   Network name (preprod, preview, etc.)
      * @param epoch
      *   Epoch to load data for
      * @param base
      *   Base test infrastructure for loading data
      * @return
      *   LedgerSnapshot ready for validation
      */
    def loadSnapshot(
        network: String,
        epoch: Int,
        base: ConformanceTestBase
    ): LedgerSnapshot =
        // Load data from Amaru test files
        val pools = base.loadPools(network, epoch)
        val dreps = base.loadDReps(network, epoch)
        val rewardsProvenance = base.loadRewardsProvenance(network, epoch + 1)
        val pots = base.loadPots(network, epoch + 3)

        // Convert to internal types
        val poolData = convertPools(pools)
        val drepData = convertDReps(dreps)
        val accountData = extractAccounts(pools, dreps)
        val utxoData = extractUtxos(pools)
        val potData = convertPots(pots)
        val blockCounts = extractBlockCounts(rewardsProvenance)

        // Compute total active stake from rewards provenance
        val activeStake = extractLovelace(rewardsProvenance.activeStake)

        // Create protocol parameters (hardcoded for now - should be loaded from config)
        val params = createProtocolParams(epoch)

        LedgerSnapshot.fromTestData(
          epochNum = epoch,
          params = params,
          poolData = poolData,
          accountData = accountData,
          drepData = drepData,
          utxoData = utxoData,
          potData = potData,
          blocks = blockCounts,
          activeStake = activeStake
        )

    /** Convert Amaru pool data to internal format */
    private def convertPools(
        pools: Map[String, PoolData]
    ): Map[PoolId, PoolParams] =
        pools.map { case (poolIdStr, pool) =>
            // Extract pool ID from bech32 or hex
            val poolId = PoolId(parseBech32OrHex(poolIdStr))

            // Parse margin rational
            val (marginNum, marginDenom) = parseRational(pool.margin)

            val params = PoolParams(
              pledge = extractLovelace(pool.pledge),
              cost = extractLovelace(pool.cost),
              margin = SafeRatio(marginNum, marginDenom),
              owners = pool.owners.map(hex => StakeCredential(ByteString.fromHex(hex))).toSet,
              rewardAccount = parseStakeAddress(pool.rewardAccount)
            )
            poolId -> params
        }

    /** Convert Amaru DRep data to internal format */
    private def convertDReps(dreps: List[DRepData]): Map[DRep, DRepState] =
        dreps.flatMap { drep =>
            parseDRep(drep).map { drepId =>
                val state = DRepState(
                  validUntil = drep.mandate.map(_.epoch),
                  metadata = drep.metadata.map(a =>
                      LedgerTypes.Anchor(a.url, ByteString.fromHex(a.hash))
                  ),
                  stake = extractLovelace(drep.stake),
                  registeredAt = CertificatePointer(0, 0, 0), // TODO: Extract from data if available
                  previousDeregistration = None
                )
                drepId -> state
            }
        }.toMap

    /** Parse DRep identifier from string */
    private def parseDRep(drep: DRepData): Option[DRep] =
        drep.`type` match
            case "abstain"       => Some(DRep.AlwaysAbstain)
            case "no_confidence" => Some(DRep.AlwaysNoConfidence)
            case "registered" =>
                (drep.id, drep.from) match
                    case (Some(id), Some(from)) =>
                        from match
                            case "script"               => Some(DRep.ScriptHash(ByteString.fromHex(id)))
                            case "verificationKey" | _ => Some(DRep.KeyHash(ByteString.fromHex(id)))
                    case _ => None
            case _ => None

    /** Extract account data from pools and DReps */
    private def extractAccounts(
        pools: Map[String, PoolData],
        dreps: List[DRepData]
    ): Map[StakeCredential, (Option[PoolId], Option[DRep])] =
        // For each pool owner, create an account delegated to that pool
        pools.flatMap { case (poolIdStr, pool) =>
            val poolId = PoolId(parseBech32OrHex(poolIdStr))
            pool.owners.map { ownerHex =>
                val credential = StakeCredential(ByteString.fromHex(ownerHex))
                credential -> (Some(poolId), None: Option[DRep])
            }
        }.toMap

    /** Extract UTxO data from pool stakes */
    private def extractUtxos(
        pools: Map[String, PoolData]
    ): Map[StakeCredential, BigInt] =
        pools.flatMap { case (poolIdStr, pool) =>
            // Use stake if available, otherwise use pledge
            val totalStake = pool.stake.map(extractLovelace).getOrElse(extractLovelace(pool.pledge))
            pool.owners.map { ownerHex =>
                val credential = StakeCredential(ByteString.fromHex(ownerHex))
                // Simplified: distribute stake/pledge among owners
                (credential, totalStake / pool.owners.size)
            }
        }.toMap

    /** Convert Amaru pots data to internal format */
    private def convertPots(pots: PotsData): Pots =
        Pots(
          treasury = extractLovelace(pots.treasury),
          reserves = extractLovelace(pots.reserves),
          fees = BigInt(0) // Fees not present in pots file
        )

    /** Extract block counts from rewards provenance */
    private def extractBlockCounts(provenance: RewardsProvenance): Map[PoolId, Int] =
        provenance.stakePools.map { case (poolIdStr, poolRewards) =>
            val poolId = PoolId(parseBech32OrHex(poolIdStr))
            poolId -> poolRewards.blocksMade
        }

    /** Create protocol parameters for an epoch */
    private def createProtocolParams(epoch: Int): ProtocolParams =
        ProtocolParams(
          minPoolCost = BigInt(340000000),          // 340 ADA
          monetaryExpansionRate = SafeRatio(3, 1000), // 0.003
          treasuryExpansionRate = SafeRatio(2, 10), // 0.2
          activeSlotCoeff = SafeRatio(5, 100),      // 0.05
          epochLength = 432000,                      // 5 days
          optimalPoolCount = 500,
          poolPledgeInfluence = SafeRatio(3, 10), // 0.3 (a0)
          poolDeposit = BigInt(500000000),        // 500 ADA
          drepActivity = 20,                      // 20 epochs
          minFeeRefScriptCostPerByte = SafeRatio(15, 1)
        )

    /** Helper to parse bech32 or hex string to ByteString */
    private def parseBech32OrHex(s: String): ByteString =
        if s.startsWith("pool") then
            // Pool addresses: decode bech32 and use the data bytes directly
            Try(Bech32.decode(s).data).map(ByteString.fromArray).getOrElse(
              ByteString.fromHex(s)
            )
        else if s.startsWith("stake") then
            // Stake addresses: decode using Address and extract the hash
            Try {
                Address.fromBech32(s) match
                    case stake: scalus.cardano.address.StakeAddress =>
                        stake.payload.asHash
                    case other =>
                        // Fallback to raw bytes if not a stake address
                        other.toBytes
            }.getOrElse(ByteString.fromHex(s))
        else
            ByteString.fromHex(s)

    /** Parse stake address to credential */
    private def parseStakeAddress(addr: String): StakeCredential =
        if addr.startsWith("stake") then
            Try {
                Address.fromBech32(addr) match
                    case stake: scalus.cardano.address.StakeAddress =>
                        StakeCredential(stake.payload.asHash)
                    case _ =>
                        StakeCredential(ByteString.fromHex(addr))
            }.getOrElse(StakeCredential(ByteString.fromHex(addr)))
        else
            StakeCredential(ByteString.fromHex(addr))

    /** Helper to create ordered map from StakeCredential */
    private given Ordering[StakeCredential] = Ordering.by(_.bytes.bytes.toList)

    /** Helper to create ordered map from PoolId */
    private given Ordering[PoolId] = Ordering.by(_.bytes.bytes.toList)

    /** Helper to create ordered map from DRep */
    private given Ordering[DRep] = Ordering.by {
        case DRep.KeyHash(bytes)     => (0, bytes.bytes.toList)
        case DRep.ScriptHash(bytes)  => (1, bytes.bytes.toList)
        case DRep.AlwaysAbstain      => (2, List.empty)
        case DRep.AlwaysNoConfidence => (3, List.empty)
    }
