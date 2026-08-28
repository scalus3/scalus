package scalus.cardano.node.stream.internal

import scalus.cardano.node.stream.{ChainTip, ScalusAsyncSource}

/** One thing that happened to the chain, as a backend reports it.
  *
  * Deliberately normalised rather than backend-shaped. If a follower emitted its own wire types the
  * driver below would need a branch per backend and the sharing would evaporate — the whole point
  * is that everything downstream of this type is written once. Backend-shaped payloads are still
  * worth keeping where they earn their place: recorded at the HTTP/WebSocket boundary, as test
  * fixtures.
  */
private[stream] enum ChainEvent {

    /** The chain advanced. See [[AppliedBlock]] for what a partial observation must state. */
    case RollForward(block: AppliedBlock)

    /** The chain switched to a different fork, and the follower can reconcile it. A follower that
      * detects a reorg it *cannot* reconcile fails its source instead — see
      * [[scalus.cardano.infra.ResyncRequiredException]].
      */
    case RollBackward(to: ChainTip)
}

/** Where a provider's chain events come from: the only thing a backend implements.
  *
  * Everything a subscription's correctness depends on — fan-out, confirmation gating, rollback
  * delivery, watermarks — lives in [[SubscriptionHub]] and is shared. A backend supplies a sequence
  * of [[ChainEvent]]s and nothing else, so Blockfrost polling, an Ogmios chain-sync stream and a
  * UTxORPC feed differ only here.
  *
  * Sources are the seam for testing, too: a follower is a function from a fake backend to an event
  * sequence, assertable without a hub, a subscription or a clock.
  */
private[stream] trait ChainFollower {

    /** Begin producing events.
      *
      * Separate from construction so that a follower is not off polling a chain — spending a
      * metered quota — before whoever built it has finished wiring up a consumer. [[HubDriver]]
      * calls this as it starts pumping, so the two halves of the lifecycle cannot be started out of
      * order. Idempotent, and a closed follower stays closed.
      */
    def start(): Unit

    /** The follower's event stream. Single-consumer — [[HubDriver]] is the consumer.
      *
      * A failed pull ends the follower: the driver propagates the cause to every subscription
      * rather than retrying, because a follower that cannot say what happened to the chain cannot
      * let subscribers keep believing their view is current.
      */
    def events: ScalusAsyncSource[ChainEvent]

    /** Which sources the follower should observe from now on, and **the point from which that takes
      * effect**.
      *
      * A metered follower polls per source, so it must be told what anyone is actually subscribed
      * to. One that reads whole blocks ignores `sources` and returns [[ChainPoint.origin]]: every
      * block it produces has `BlockCoverage.Complete`, so "everything after the origin covers you"
      * is simply true, and reporting its current position instead would understate what it covers
      * and push the caller onto a snapshot it does not need.
      *
      * **The argument is the complete set, and calls must be serialised.** `sources` replaces what
      * was being watched rather than adding to it, so a caller passes the union of every live
      * subscription's sources every time. Two concurrent callers each passing only their own would
      * leave one of them silently unwatched while holding a position that promises otherwise —
      * which is the very failure this return value exists to prevent, reintroduced one layer up.
      *
      * The return value is what makes a subscription's start point exact rather than approximate.
      * Registering a subscription and telling the follower to watch its sources are two steps, and
      * a block processed between them is covered by neither: the subscriber would be registered
      * from the tip, silently miss that block — nobody having fetched it on its behalf — and the
      * hub would then advance it past that point when the next covered block arrived. So a follower
      * returns the last position whose source set was already fixed; everything after it is
      * guaranteed to have been assembled with `sources` included, and the caller closes the
      * remaining gap with a snapshot taken afterwards.
      *
      * **A [[ChainPoint]], not a height.** Heights are only unique within a fork, and this seam is
      * meant for followers that emit [[ChainEvent.RollBackward]]. After a reorg, "block 100" on the
      * abandoned fork and on the new one are different blocks, and a caller told "everything above
      * 100 covers you" would silently assign the replacement 100 to a snapshot describing the fork
      * that no longer exists. A point names the block.
      *
      * A follower that rolls back therefore **must lower this to the rollback target** as it emits
      * the `RollBackward`, since the blocks it replays afterwards are new blocks that no previous
      * `watch` can have accounted for.
      */
    def watch(sources: Set[scalus.cardano.node.UtxoSource]): scalus.cardano.node.stream.ChainPoint

    def close(): Unit
}
