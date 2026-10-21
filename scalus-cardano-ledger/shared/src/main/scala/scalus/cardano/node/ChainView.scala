package scalus.cardano.node

/** Which ledger state a query of the Emulator reads, spec section 9. */
enum ChainView {

    /** The tip snapshot: what Blockfrost, Koios and Kupo show, spec [VW-1]. */
    case Tip

    /** The snapshot after the last mempool tx, or the tip snapshot if the mempool is empty, spec
      * [VW-4].
      */
    case Mempool

    /** The snapshot of the selected-chain block of depth `depth`: 1 is the tip, the tip height plus
      * 1 is genesis, spec [VW-5], [VW-8].
      */
    case Stable(depth: Int)
}

/** Why a [[ChainView]] cannot be read, spec [VW-6], [VW-7]. */
enum ChainViewError {

    /** `depth` is below 1, or above the tip height plus 1, spec [VW-6]. */
    case DepthOutOfRange(depth: Int, tipHeight: Long)

    /** The block of depth `depth` is deeper than k, so its snapshot was dropped, spec [VW-7]. */
    case SnapshotDropped(depth: Int)
}
