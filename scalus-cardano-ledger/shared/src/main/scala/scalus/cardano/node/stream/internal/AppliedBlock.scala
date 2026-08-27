package scalus.cardano.node.stream.internal

import scalus.cardano.ledger.{Block, Transaction, Utxos}
import scalus.cardano.node.UtxoSource
import scalus.cardano.node.stream.{BlockNo, ChainPoint}

/** One applied transaction, with its UTxO effects already resolved.
  *
  * `spent` carries the *resolved* consumed outputs, not just the inputs: a subscriber watching an
  * address needs to know which of its UTxOs disappeared, and the input alone does not say. Every
  * provider can resolve this — the emulator from its ledger, a chain-following provider from the
  * block it just fetched — so resolving once at the source beats every subscriber doing it.
  */
case class AppliedTransaction(tx: Transaction, created: Utxos, spent: Utxos) {
    def txHash: scalus.cardano.ledger.TransactionHash = tx.id
}

/** What the provider actually examined when it produced an [[AppliedBlock]].
  *
  * A provider that holds the whole block can answer any subscription from it. A metered provider
  * cannot afford to: Blockfrost's cheap path is per-address and per-asset endpoints, so what it
  * learns about a block is only ever "what these sources did in it", and it must say so.
  *
  * The distinction is load-bearing rather than informational. The hub delivers a block to a
  * subscription and advances that subscription's watermark past it in the same step, so handing it
  * a block that did not cover a subscription would tell that subscriber "nothing here for you" — as
  * an [[scalus.cardano.node.stream.UtxoEvent.Idle]], no less — and then make the real events for
  * that height undeliverable, because the watermark has moved on. Silently.
  */
enum BlockCoverage {

    /** The provider holds the whole block and can answer any query from it. */
    case Complete

    /** The provider examined only these sources. A subscription is covered when its query can be
      * answered from them — see [[BlockCoverage.covers]].
      */
    case Sources(sources: Set[UtxoSource])
}

object BlockCoverage {

    /** Whether a block with this coverage is authoritative for a query.
      *
      * The recursion mirrors `SubscriptionSupport.isIndexed`, and for the same reasons: a union is
      * covered only if every arm is, because the events we would miss are exactly the ones the
      * uncovered arm would have found; an intersection needs just one covered arm, because that
      * arm's results contain every candidate and the other arm post-filters data already in hand.
      */
    def covers(coverage: BlockCoverage, source: UtxoSource): Boolean = coverage match
        case Complete        => true
        case Sources(probed) => coveredBy(source, probed)

    private def coveredBy(source: UtxoSource, probed: Set[UtxoSource]): Boolean =
        probed.contains(source) || (source match
            case UtxoSource.Or(l, r)  => coveredBy(l, probed) && coveredBy(r, probed)
            case UtxoSource.And(l, r) => coveredBy(l, probed) || coveredBy(r, probed)
            case _                    => false)
}

/** A block as the subscription hub consumes it.
  *
  * `block` is the raw ledger block when the provider has one. Providers that synthesise blocks — an
  * emulator applying one transaction at a time — have nothing truthful to put here, which is why it
  * is optional and why such providers declare that they do not serve `Block` subscriptions rather
  * than fabricating a header.
  *
  * ## The contract for partial coverage
  *
  * A provider producing anything other than [[BlockCoverage.Complete]] owes the hub two things.
  *
  *   - **One block per height, carrying the union of every source probed at that height** — not one
  *     block per watcher. A query spanning two sources is covered only by a block that probed both
  *     (a union is as covered as its worst arm), so splitting a height across per-watcher blocks
  *     would leave such a subscription permanently uncovered and silent.
  *   - **Every height examined, whether or not it matched anything.** An empty `txs` with the right
  *     coverage is how a provider says "I looked here for these sources and there was nothing",
  *     which is both what makes `Idle` truthful and what keeps watermarks tracking the tip. A
  *     provider that reported only the heights it found matches in would let its subscriptions fall
  *     arbitrarily far behind the retention window and lose events to pruning.
  *
  * Heights are applied in ascending order and each is applied once — `SubscriptionHub.applyBlock`
  * takes the block's height as the new tip unconditionally, so it has always required this.
  *
  * **Not yet supported: backfill.** A provider that runs out of request budget mid-height and wants
  * to come back for the sources it skipped would have to re-report a height it has already applied,
  * and there is no representation for that: `recent` is ordered by height and a subscription's
  * progress is a single watermark. Such a provider must instead defer the *whole* height until it
  * can cover everything it has subscriptions for. Lifting this needs per-height observation sets
  * and is a deliberate design step, not an implementation detail — see the M2 plan.
  */
case class AppliedBlock(
    point: ChainPoint,
    blockNo: BlockNo,
    txs: Seq[AppliedTransaction],
    block: Option[Block] = None,
    coverage: BlockCoverage = BlockCoverage.Complete
)
