package scalus.cardano.node.stream

import scalus.cardano.ledger.{BlockHash, SlotNo}
import scalus.uplc.builtin.ByteString

/** Block height — count of blocks since genesis. Carried in [[ChainTip]] so subscribers can compute
  * confirmations locally (`tip.blockNo - tx.blockNo`). Not exposed on
  * [[UtxoEvent]]/[[TransactionEvent]]/[[BlockEvent]] where the block-identity information
  * ([[ChainPoint]]) is sufficient.
  */
type BlockNo = Long

/** A position on the chain, identified by slot and block header hash.
  *
  * Events carry a `ChainPoint` so subscribers can correlate across streams, deduplicate on replay,
  * and checkpoint their own progress. Note: block *height* lives in [[ChainTip]], not here — two
  * blocks at the same slot have different hashes but (in the rollback-buffer sense) distinguishable
  * identity via the hash.
  */
case class ChainPoint(slot: SlotNo, blockHash: BlockHash)

object ChainPoint {

    /** Sentinel "before any block". Used by the engine when it seeds a subscription from a backup
      * snapshot and has not yet observed a real tip: seeded [[UtxoEvent.Created]] events carry this
      * point rather than faking one, and subscribers can recognise it with
      * `point == ChainPoint.origin`.
      */
    val origin: ChainPoint =
        ChainPoint(0L, BlockHash.fromByteString(ByteString.fromArray(new Array[Byte](32))))
}

/** Rich chain-tip value: position plus height.
  *
  * Emitted by `subscribeTip()` and returned by `Engine.currentTip`. `blockNo` is what enables local
  * confirmation math — a subscriber tracking its own tx can compute depth without calling the
  * backup.
  */
case class ChainTip(point: ChainPoint, blockNo: BlockNo) {
    def slot: SlotNo = point.slot
    def blockHash: BlockHash = point.blockHash
}

object ChainTip {

    /** Sentinel matching [[ChainPoint.origin]]. Height 0. */
    val origin: ChainTip = ChainTip(ChainPoint.origin, 0L)
}

/** Where a subscription should start reading from. */
enum StartFrom {

    /** Begin from the genesis origin — replay the whole chain. Only practical with a persistent
      * `ChainStore`.
      */
    case Origin

    /** Begin from the current tip at subscription time; no historical events are delivered. This is
      * the default for live subscriptions.
      */
    case Tip

    /** Begin from a specific chain point. Useful for resuming after a subscriber restart from a
      * checkpoint it persisted itself.
      */
    case At(point: ChainPoint)
}
