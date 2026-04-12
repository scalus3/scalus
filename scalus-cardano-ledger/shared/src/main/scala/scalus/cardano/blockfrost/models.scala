package scalus.cardano.blockfrost

import scalus.cardano.ledger.Coin
import upickle.default.*
import upickle.implicits.key

/** String-encoded quantity from Blockfrost API. Used for non-ADA numeric fields that Blockfrost
  * returns as JSON strings (asset quantities, execution unit budgets, voting power).
  *
  * For ADA/lovelace amounts, use `Coin` instead.
  */
opaque type Quantity = Long

object Quantity {
    def apply(value: Long): Quantity = value

    extension (q: Quantity)
        def value: Long = q
        def toLong: Long = q

    given ReadWriter[Quantity] = readwriter[ujson.Value].bimap[Quantity](
      q => ujson.Str(q.toString),
      {
          case ujson.Str(s) => s.toLong
          case ujson.Num(n) => n.toLong
          case v => throw upickle.core.Abort(s"Expected string or number for Quantity, got: $v")
      }
    )
}

/** Blockfrost/MiniBF API response models.
  *
  * All case classes derive upickle `ReadWriter` for JSON serialization. Field names use
  * `@key("snake_case")` annotations where the Blockfrost JSON key differs from the Scala field
  * name.
  *
  * `Coin` is used for lovelace/ADA amounts. `Quantity` is used for other string-encoded numeric
  * fields (asset quantities, execution unit budgets, voting power).
  */

// ── Addresses ───────────────────────────────────────────────────────────────

case class AddressInfo(
    address: String,
    amount: Seq[AssetAmount],
    @key("stake_address") stakeAddress: Option[String],
    @key("type") addressType: String,
    script: Boolean
) derives ReadWriter

case class AssetAmount(
    unit: String,
    quantity: Quantity
) derives ReadWriter

case class AddressTransaction(
    @key("tx_hash") txHash: String,
    @key("tx_index") txIndex: Int,
    @key("block_height") blockHeight: Long,
    @key("block_time") blockTime: Long
) derives ReadWriter

// ── Blocks ──────────────────────────────────────────────────────────────────

case class BlockInfo(
    time: Long,
    height: Option[Long],
    hash: String,
    slot: Option[Long],
    epoch: Option[Long],
    @key("epoch_slot") epochSlot: Option[Long],
    @key("slot_leader") slotLeader: String,
    size: Long,
    @key("tx_count") txCount: Int,
    @key("previous_block") previousBlock: Option[String],
    @key("next_block") nextBlock: Option[String],
    confirmations: Long
) derives ReadWriter

// ── Transactions ────────────────────────────────────────────────────────────

case class TransactionInfo(
    hash: String,
    block: String,
    @key("block_height") blockHeight: Long,
    @key("block_time") blockTime: Long,
    slot: Long,
    index: Int,
    fees: Coin,
    size: Long,
    @key("invalid_before") invalidBefore: Option[String],
    @key("invalid_hereafter") invalidHereafter: Option[String],
    @key("utxo_count") utxoCount: Int,
    @key("redeemer_count") redeemerCount: Int,
    @key("output_amount") outputAmount: Seq[AssetAmount],
    @key("valid_contract") validContract: Boolean
) derives ReadWriter

case class TransactionRedeemer(
    @key("tx_index") txIndex: Int,
    purpose: String,
    @key("script_hash") scriptHash: String,
    @key("redeemer_data_hash") redeemerDataHash: String,
    @key("unit_mem") unitMem: Quantity,
    @key("unit_steps") unitSteps: Quantity,
    fee: Coin
) derives ReadWriter

case class TransactionMetadataEntry(
    label: String,
    @key("json_metadata") jsonMetadata: ujson.Value
) derives ReadWriter

case class TransactionMetadataCborEntry(
    label: String,
    @key("cbor_metadata") cborMetadata: Option[String],
    metadata: Option[String]
) derives ReadWriter

case class TransactionWithdrawal(
    address: String,
    amount: Coin
) derives ReadWriter

case class TransactionDelegation(
    index: Int,
    @key("cert_index") certIndex: Int,
    address: String,
    @key("pool_id") poolId: String,
    @key("active_epoch") activeEpoch: Long
) derives ReadWriter

case class TransactionStake(
    @key("cert_index") certIndex: Int,
    address: String,
    registration: Boolean
) derives ReadWriter

case class TransactionMir(
    pot: String,
    @key("cert_index") certIndex: Int,
    address: String,
    amount: Coin
) derives ReadWriter

case class TransactionPoolUpdate(
    @key("cert_index") certIndex: Int,
    @key("pool_id") poolId: String,
    @key("vrf_key") vrfKey: String,
    pledge: Coin,
    @key("margin_cost") marginCost: Double,
    @key("fixed_cost") fixedCost: Coin,
    @key("reward_account") rewardAccount: String,
    owners: Seq[String],
    metadata: Option[PoolMetadataRef],
    relays: Seq[PoolRelay],
    @key("active_epoch") activeEpoch: Long
) derives ReadWriter

case class PoolMetadataRef(
    url: Option[String],
    hash: Option[String]
) derives ReadWriter

case class PoolRelay(
    ipv4: Option[String],
    ipv6: Option[String],
    dns: Option[String],
    @key("dns_srv") dnsSrv: Option[String],
    port: Option[Int]
) derives ReadWriter

case class TransactionPoolRetire(
    @key("cert_index") certIndex: Int,
    @key("pool_id") poolId: String,
    @key("retiring_epoch") retiringEpoch: Long
) derives ReadWriter

// ── Scripts ─────────────────────────────────────────────────────────────────

case class ScriptInfo(
    @key("script_hash") scriptHash: String,
    @key("type") scriptType: String,
    @key("serialised_size") serialisedSize: Option[Long]
) derives ReadWriter

case class DatumCborResponse(
    cbor: String
) derives ReadWriter

case class ScriptCborResponse(
    cbor: String
) derives ReadWriter

// ── Accounts ────────────────────────────────────────────────────────────────

case class AccountInfo(
    @key("stake_address") stakeAddress: String,
    active: Boolean,
    @key("active_epoch") activeEpoch: Option[Long],
    @key("controlled_amount") controlledAmount: Coin,
    @key("rewards_sum") rewardsSum: Coin,
    @key("withdrawals_sum") withdrawalsSum: Coin,
    @key("reserves_sum") reservesSum: Coin,
    @key("treasury_sum") treasurySum: Coin,
    @key("withdrawable_amount") withdrawableAmount: Coin,
    @key("pool_id") poolId: Option[String]
) derives ReadWriter

case class AccountRegistration(
    @key("tx_hash") txHash: String,
    action: String
) derives ReadWriter

case class AccountDelegation(
    @key("active_epoch") activeEpoch: Long,
    @key("tx_hash") txHash: String,
    amount: Coin,
    @key("pool_id") poolId: String
) derives ReadWriter

case class AccountAddress(
    address: String
) derives ReadWriter

case class AccountReward(
    epoch: Long,
    amount: Coin,
    @key("pool_id") poolId: String,
    @key("type") rewardType: String
) derives ReadWriter

// ── Assets ──────────────────────────────────────────────────────────────────

case class AssetInfo(
    asset: String,
    @key("policy_id") policyId: String,
    @key("asset_name") assetName: Option[String],
    fingerprint: String,
    quantity: Quantity,
    @key("initial_mint_tx_hash") initialMintTxHash: String,
    @key("mint_or_burn_count") mintOrBurnCount: Int,
    @key("onchain_metadata") onchainMetadata: Option[ujson.Value],
    @key("onchain_metadata_standard") onchainMetadataStandard: Option[String],
    metadata: Option[AssetMetadata]
) derives ReadWriter

case class AssetMetadata(
    name: String,
    description: String,
    ticker: Option[String],
    url: Option[String],
    logo: Option[String],
    decimals: Option[Int]
) derives ReadWriter

case class AssetAddress(
    address: String,
    quantity: Quantity
) derives ReadWriter

case class AssetTransaction(
    @key("tx_hash") txHash: String,
    @key("tx_index") txIndex: Int,
    @key("block_height") blockHeight: Long,
    @key("block_time") blockTime: Long
) derives ReadWriter

// ── Genesis & Network ───────────────────────────────────────────────────────

case class GenesisInfo(
    @key("active_slots_coefficient") activeSlotsCoefficient: Double,
    @key("update_quorum") updateQuorum: Int,
    @key("max_lovelace_supply") maxLovelaceSupply: Coin,
    @key("network_magic") networkMagic: Int,
    @key("epoch_length") epochLength: Long,
    @key("system_start") systemStart: Long,
    @key("slots_per_kes_period") slotsPerKesPeriod: Long,
    @key("slot_length") slotLength: Long,
    @key("max_kes_evolutions") maxKesEvolutions: Long,
    @key("security_param") securityParam: Long
) derives ReadWriter

case class NetworkInfo(
    supply: NetworkSupply,
    stake: NetworkStake
) derives ReadWriter

case class NetworkSupply(
    max: Coin,
    total: Coin,
    circulating: Coin,
    locked: Coin,
    treasury: Coin,
    reserves: Coin
) derives ReadWriter

case class NetworkStake(
    live: Coin,
    active: Coin
) derives ReadWriter

case class EraInfo(
    start: EraBound,
    end: Option[EraBound],
    parameters: EraParameters
) derives ReadWriter

case class EraBound(
    time: Double,
    slot: Long,
    epoch: Long
) derives ReadWriter

case class EraParameters(
    @key("epoch_length") epochLength: Long,
    @key("slot_length") slotLength: Double,
    @key("safe_zone") safeZone: Long
) derives ReadWriter

// ── Metadata ────────────────────────────────────────────────────────────────

case class TxMetadataJson(
    @key("tx_hash") txHash: String,
    @key("json_metadata") jsonMetadata: ujson.Value
) derives ReadWriter

case class TxMetadataCbor(
    @key("tx_hash") txHash: String,
    @key("cbor_metadata") cborMetadata: Option[String],
    metadata: Option[String]
) derives ReadWriter

// ── Pools ───────────────────────────────────────────────────────────────────

case class PoolExtended(
    @key("pool_id") poolId: String,
    hex: String,
    @key("active_stake") activeStake: Option[Coin] = None,
    @key("live_stake") liveStake: Option[Coin] = None,
    @key("live_size") liveSize: Option[Double] = None,
    @key("live_saturation") liveSaturation: Option[Double] = None,
    @key("live_delegators") liveDelegators: Option[Long] = None,
    @key("active_size") activeSize: Option[Double] = None,
    blocks: Option[Long] = None,
    registration: Option[Seq[String]] = None,
    retirement: Option[Seq[String]] = None
) derives ReadWriter

case class PoolDelegator(
    address: String,
    @key("live_stake") liveStake: Coin
) derives ReadWriter

case class PoolHistoryEntry(
    epoch: Long,
    blocks: Long,
    @key("active_stake") activeStake: Coin,
    @key("active_size") activeSize: Double,
    @key("delegators_count") delegatorsCount: Long,
    rewards: Coin,
    fees: Coin
) derives ReadWriter

// ── Governance ──────────────────────────────────────────────────────────────

case class DrepInfo(
    @key("drep_id") drepId: String,
    hex: String,
    amount: Coin,
    active: Boolean,
    @key("active_epoch") activeEpoch: Option[Long],
    @key("has_script") hasScript: Boolean
) derives ReadWriter

// ── Health ──────────────────────────────────────────────────────────────────

case class ApiRootInfo(
    url: String,
    version: String
) derives ReadWriter

case class HealthStatus(
    @key("is_healthy") isHealthy: Boolean
) derives ReadWriter

case class HealthClock(
    @key("server_time") serverTime: Long
) derives ReadWriter

// ── Block Addresses ─────────────────────────────────────────────────────────

case class BlockAddress(
    address: String,
    transactions: Seq[BlockAddressTx]
) derives ReadWriter

case class BlockAddressTx(
    @key("tx_hash") txHash: String
) derives ReadWriter
