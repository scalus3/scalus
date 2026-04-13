package scalus.cardano.node.stream

import scalus.cardano.ledger.{BlockHash, SlotNo}

/** A position on the chain, identified by slot and block header hash.
  *
  * Emitted events carry a `ChainPoint` so subscribers can correlate across
  * streams, deduplicate on replay, and checkpoint their own progress.
  */
case class ChainPoint(slot: SlotNo, blockHash: BlockHash)

/** Where a subscription should start reading from. */
enum StartFrom {

    /** Begin from the genesis origin — replay the whole chain. Only
      * practical with a persistent `ChainStore`.
      */
    case Origin

    /** Begin from the current tip at subscription time; no historical
      * events are delivered. This is the default for live subscriptions.
      */
    case Tip

    /** Begin from a specific chain point. Useful for resuming after a
      * subscriber restart from a checkpoint it persisted itself.
      */
    case At(point: ChainPoint)
}
