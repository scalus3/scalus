package scalus.cardano.node.emulator

import scalus.cardano.ledger.{PoolKeyHash, UnitInterval}
import scalus.cardano.ledger.epoch.PoolPerformance

/** When the Emulator forges a block, spec section 5. A Java enum, interop rule 8. */
enum BlockProduction extends Enum[BlockProduction] {

    /** Every submit forges a block at once, so a valid tx is visible right after `submit`. */
    case Instant

    /** Only an explicit call forges a block. A submitted tx waits in the mempool until then. */
    case Manual
}

/** What awaiting a tx does while the tx is still pending, spec section 5. A Java enum, interop rule
  * 8.
  */
enum AwaitTxPolicy extends Enum[AwaitTxPolicy] {

    /** Forge blocks until the tx leaves the mempool. */
    case ProduceBlock

    /** Report the status as it is, and forge nothing. */
    case WaitOnly
}

/** What happens to the txs of a block that leaves the selected chain, spec section 11.2. A Java
  * enum, interop rule 8.
  */
enum OrphanTxPolicy extends Enum[OrphanTxPolicy] {

    /** Put them back in the mempool, before the txs already there. */
    case ReturnToMempool

    /** Drop them. */
    case Drop
}

/** Which pool the Emulator's blocks count for, ledger-rules spec [ES-3]. */
enum BlockIssuer {

    /** No pool: a block counts for none, spec [ES-5]. */
    case Unattributed

    /** The pool `poolId`, registered or not: each block counts in its blocks made, spec [ES-4]. */
    case Pool(poolId: PoolKeyHash)
}

object BlockIssuer {

    // Java names a case of a Scala 3 enum only through the module class, so these forward as
    // static methods, `BlockIssuer.pool(id)`, as ChainView does, ledger-rules spec [ES-1a].

    /** [[Unattributed]], for Java. */
    def unattributed: BlockIssuer = Unattributed

    /** [[Pool]], for Java. */
    def pool(poolId: PoolKeyHash): BlockIssuer = Pool(poolId)
}

/** How the Emulator behaves as a chain, spec section 5.
  *
  * The constructor is private, so Scala 3 makes `apply` and `copy` private too: Java and JS cannot
  * use their default parameters, spec [CF-1]. Start from [[ChainConfig.instant]] or
  * [[ChainConfig.manual]] and change a field with its `with` method, spec [CF-3].
  *
  * @param securityParam
  *   k, the deepest rollback the Emulator allows. At least 1.
  * @param poolPerformance
  *   how the reward calculation rates the pools, ledger-rules spec [ES-1]
  * @param activeSlotsCoeff
  *   `f`, the share of slots with a block, which sets the expected blocks of an epoch under
  *   `Measured`, ledger-rules spec [ES-2a], [RW-14]. Above 0.
  * @param blockIssuer
  *   the pool each forged block counts for, ledger-rules spec [ES-3]
  */
final case class ChainConfig private (
    blockProduction: BlockProduction,
    awaitTx: AwaitTxPolicy,
    orphanTxs: OrphanTxPolicy,
    securityParam: Long,
    poolPerformance: PoolPerformance,
    activeSlotsCoeff: UnitInterval,
    blockIssuer: BlockIssuer
) {
    require(securityParam >= 1, s"securityParam must be at least 1, got $securityParam")
    require(
      activeSlotsCoeff.numerator > 0,
      s"activeSlotsCoeff must be above 0, got ${activeSlotsCoeff.numerator}/${activeSlotsCoeff.denominator}"
    )

    def withBlockProduction(blockProduction: BlockProduction): ChainConfig =
        copy(blockProduction = blockProduction)

    def withAwaitTx(awaitTx: AwaitTxPolicy): ChainConfig = copy(awaitTx = awaitTx)

    def withOrphanTxs(orphanTxs: OrphanTxPolicy): ChainConfig = copy(orphanTxs = orphanTxs)

    def withSecurityParam(securityParam: Long): ChainConfig = copy(securityParam = securityParam)

    def withPoolPerformance(poolPerformance: PoolPerformance): ChainConfig =
        copy(poolPerformance = poolPerformance)

    def withActiveSlotsCoeff(activeSlotsCoeff: UnitInterval): ChainConfig =
        copy(activeSlotsCoeff = activeSlotsCoeff)

    def withBlockIssuer(blockIssuer: BlockIssuer): ChainConfig = copy(blockIssuer = blockIssuer)
}

object ChainConfig {

    /** The default: every submit forges a block, spec [CF-2], [CF-4]. Every pool is rated `Ideal`,
      * `f` is 1/20, and no block counts for a pool, ledger-rules spec [ES-1] to [ES-3].
      */
    val instant: ChainConfig = new ChainConfig(
      BlockProduction.Instant,
      AwaitTxPolicy.ProduceBlock,
      OrphanTxPolicy.ReturnToMempool,
      2160L,
      PoolPerformance.Ideal,
      UnitInterval(1, 20),
      BlockIssuer.Unattributed
    )

    /** Only an explicit call forges a block, spec [CF-2]. */
    val manual: ChainConfig = instant.withBlockProduction(BlockProduction.Manual)
}
