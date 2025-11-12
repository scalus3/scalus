package scalus.testing.conformance.ledger

import scalus.builtin.ByteString
import scala.collection.immutable.SortedMap

/** Trait for accessing ledger state data at a specific point in time
  *
  * This provides a read-only view of the ledger state needed for:
  * - Computing stake distribution
  * - Computing rewards
  * - Validating transactions
  *
  * Implementations can be backed by:
  * - Actual blockchain data
  * - Test snapshots from Amaru
  * - In-memory test data
  */
trait LedgerSnapshot:
    import LedgerTypes.*

    /** Get the epoch this snapshot represents */
    def epoch: Epoch

    /** Get protocol parameters for this epoch */
    def protocolParams: ProtocolParams

    /** Get all registered stake pools */
    def pools: Iterator[(PoolId, PoolParams)]

    /** Get all stake accounts with their delegations */
    def accounts: Iterator[(StakeCredential, (Option[PoolId], Option[DRep]))]

    /** Get all registered DReps with their state */
    def dreps: Iterator[(DRep, DRepState)]

    /** Get all UTxOs to scan for stake */
    def utxos: Iterator[(StakeCredential, Lovelace)]

    /** Get current pot balances */
    def pots: Pots

    /** Get number of blocks produced by each pool in the previous epoch */
    def blockCounts: Map[PoolId, Int]

    /** Get total active stake for voting */
    def totalActiveStake: Lovelace

/** Companion object with helper methods */
object LedgerSnapshot:
    import LedgerTypes.*

    /** Create a test snapshot from explicit data */
    def fromTestData(
        epochNum: Epoch,
        params: ProtocolParams,
        poolData: Map[PoolId, PoolParams],
        accountData: Map[StakeCredential, (Option[PoolId], Option[DRep])],
        drepData: Map[DRep, DRepState],
        utxoData: Map[StakeCredential, Lovelace],
        potData: Pots,
        blocks: Map[PoolId, Int],
        activeStake: Lovelace
    ): LedgerSnapshot = new LedgerSnapshot:
        def epoch: Epoch = epochNum
        def protocolParams: ProtocolParams = params
        def pools: Iterator[(PoolId, PoolParams)] = poolData.iterator
        def accounts: Iterator[(StakeCredential, (Option[PoolId], Option[DRep]))] =
            accountData.iterator
        def dreps: Iterator[(DRep, DRepState)] = drepData.iterator
        def utxos: Iterator[(StakeCredential, Lovelace)] = utxoData.iterator
        def pots: Pots = potData
        def blockCounts: Map[PoolId, Int] = blocks
        def totalActiveStake: Lovelace = activeStake
