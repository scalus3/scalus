package scalus.cardano.node.stream

import scalus.cardano.ledger.{BlockHash, SlotNo}
import scalus.uplc.builtin.ByteString

/** Block height — the count of blocks since genesis.
  *
  * Carried on [[ChainTip]] so subscribers can compute confirmations locally (`tip.blockNo -
  * event.blockNo`) rather than asking the provider. Not carried on the event types, where block
  * *identity* ([[ChainPoint]]) is what matters.
  */
type BlockNo = Long

/** A position on the chain, identified by slot and block header hash.
  *
  * Every event carries one, so subscribers can correlate across streams, deduplicate on replay, and
  * checkpoint their own progress — a persisted `ChainPoint` is what `StartFrom.At` resumes from.
  */
case class ChainPoint(slot: SlotNo, blockHash: BlockHash)

object ChainPoint {

    /** Sentinel "before any block". Used when a subscription is seeded from a snapshot before any
      * real tip has been observed: the synthetic [[UtxoEvent.Created]] events carry this rather
      * than a faked point, and subscribers can recognise it with `point == ChainPoint.origin`.
      */
    val origin: ChainPoint =
        ChainPoint(0L, BlockHash.fromByteString(ByteString.fromArray(new Array[Byte](32))))
}

/** Chain tip: position plus height.
  *
  * Height is what enables local confirmation arithmetic, so a subscriber tracking its own
  * transaction can compute depth without another round trip.
  */
case class ChainTip(point: ChainPoint, blockNo: BlockNo) {
    def slot: SlotNo = point.slot
    def blockHash: BlockHash = point.blockHash
}

object ChainTip {

    /** Sentinel matching [[ChainPoint.origin]], height 0. */
    val origin: ChainTip = ChainTip(ChainPoint.origin, 0L)
}

/** Where a subscription starts reading.
  *
  * Support is provider- *and* query-dependent — see [[StreamCapabilities]] and
  * [[SubscriptionSupport]]. A provider that cannot serve the requested start point throws
  * [[scalus.cardano.infra.UnsupportedSubscriptionException]] from `subscribe`.
  */
enum StartFrom {

    /** Replay the whole chain from genesis. Requires `ReplaySupport.FullHistory`. */
    case Origin

    /** Start at the tip as of subscription time; no historical events. The default. */
    case Tip

    /** Resume from a point the subscriber checkpointed itself. Serviceable wherever the provider
      * can replay the subscription's query — on Blockfrost that is address- and asset-scoped
      * queries, which is the common case.
      */
    case At(point: ChainPoint)
}
