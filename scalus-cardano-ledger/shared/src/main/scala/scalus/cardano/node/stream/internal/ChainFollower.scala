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

    /** The follower's event stream. Single-consumer — [[HubDriver]] is the consumer.
      *
      * A failed pull ends the follower: the driver propagates the cause to every subscription
      * rather than retrying, because a follower that cannot say what happened to the chain cannot
      * let subscribers keep believing their view is current.
      */
    def events: ScalusAsyncSource[ChainEvent]

    /** Which sources the follower should observe from now on, and **the height from which that
      * takes effect**.
      *
      * A metered follower polls per source, so it must be told what anyone is actually subscribed
      * to; one that reads whole blocks ignores this and reports its current height.
      *
      * The return value is what makes a subscription's start point exact rather than approximate.
      * Registering a subscription and telling the follower to watch its sources are two steps, and
      * a block processed between them is covered by neither: the subscriber would be registered
      * from the tip, silently miss that block — nobody having fetched it on its behalf — and the
      * hub would then advance it past that height when the next covered block arrived.
      *
      * So a follower returns the last height whose source set was already fixed. Every block above
      * it is guaranteed to have been assembled with `sources` included, which lets the caller
      * register the subscription at exactly that height instead of at whatever the tip happened to
      * be. The guarantee becomes structural rather than a matter of how the two calls interleave.
      */
    def watch(sources: Set[scalus.cardano.node.UtxoSource]): scalus.cardano.node.stream.BlockNo

    def close(): Unit
}
