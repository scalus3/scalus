package scalus.cardano.node

/** When the Emulator forges a block, spec section 5. */
enum BlockProduction {

    /** Every submit forges a block at once, so a valid tx is visible right after `submit`. */
    case Instant

    /** Only an explicit call forges a block. A submitted tx waits in the mempool until then. */
    case Manual
}

/** What awaiting a tx does while the tx is still pending, spec section 5. */
enum AwaitTxPolicy {

    /** Forge blocks until the tx leaves the mempool. */
    case ProduceBlock

    /** Report the status as it is, and forge nothing. */
    case WaitOnly
}

/** What happens to the txs of a block that leaves the selected chain, spec section 11.2. */
enum OrphanTxPolicy {

    /** Put them back in the mempool, before the txs already there. */
    case ReturnToMempool

    /** Drop them. */
    case Drop
}

/** How the Emulator behaves as a chain, spec section 5.
  *
  * The constructor is private, so Scala 3 makes `apply` and `copy` private too: Java and JS cannot
  * use their default parameters, spec [CF-1]. Start from [[ChainConfig.instant]] or
  * [[ChainConfig.manual]] and change a field with its `with` method, spec [CF-3].
  *
  * @param securityParam
  *   k, the deepest rollback the Emulator allows. At least 1.
  */
final case class ChainConfig private (
    blockProduction: BlockProduction,
    awaitTx: AwaitTxPolicy,
    orphanTxs: OrphanTxPolicy,
    securityParam: Long
) {
    require(securityParam >= 1, s"securityParam must be at least 1, got $securityParam")

    def withBlockProduction(blockProduction: BlockProduction): ChainConfig =
        copy(blockProduction = blockProduction)

    def withAwaitTx(awaitTx: AwaitTxPolicy): ChainConfig = copy(awaitTx = awaitTx)

    def withOrphanTxs(orphanTxs: OrphanTxPolicy): ChainConfig = copy(orphanTxs = orphanTxs)

    def withSecurityParam(securityParam: Long): ChainConfig = copy(securityParam = securityParam)
}

object ChainConfig {

    /** The default: every submit forges a block, spec [CF-2], [CF-4]. */
    val instant: ChainConfig = new ChainConfig(
      BlockProduction.Instant,
      AwaitTxPolicy.ProduceBlock,
      OrphanTxPolicy.ReturnToMempool,
      2160L
    )

    /** Only an explicit call forges a block, spec [CF-2]. */
    val manual: ChainConfig = instant.withBlockProduction(BlockProduction.Manual)
}
