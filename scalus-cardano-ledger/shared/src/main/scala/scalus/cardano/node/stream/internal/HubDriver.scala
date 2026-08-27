package scalus.cardano.node.stream.internal

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

/** Pumps a [[ChainFollower]]'s events into a [[SubscriptionHub]].
  *
  * The half of a streaming provider that is the same whatever the backend is: pull an event, apply
  * it, pull again. Written once so that Blockfrost, Ogmios and UTxORPC cannot each get the
  * lifecycle subtly different.
  *
  * Not a thread. The loop is a `Future` continuation chain, so it runs wherever the follower
  * completes its pulls and needs nothing that does not exist on Scala.js.
  */
private[stream] final class HubDriver(
    hub: SubscriptionHub,
    follower: ChainFollower
)(using ec: ExecutionContext) {

    @volatile private var stopped = false

    /** Begin pumping. Idempotent in the sense that a stopped driver stays stopped. */
    def start(): Unit = if !stopped then pump()

    private def pump(): Unit =
        follower.events.pull().onComplete {
            case Success(Some(event)) =>
                // Applying can throw — a rollback deeper than the horizon, a capability the
                // provider declared it does not have. That is a failure of the whole feed, not of
                // one event, so it travels the same path as a follower failure.
                try
                    apply(event)
                    if !stopped then pump()
                catch
                    case t: Throwable => fail(t)
            // The follower ended cleanly: no more chain events are coming, so subscriptions are
            // complete rather than broken.
            case Success(None) => if !stopped then hub.closeAll()
            case Failure(t)    => fail(t)
        }

    private def apply(event: ChainEvent): Unit = event match
        case ChainEvent.RollForward(block) => hub.applyBlock(block)
        case ChainEvent.RollBackward(to)   => hub.rollbackTo(to)

    /** A follower that cannot say what happened to the chain leaves every subscriber holding a view
      * it has no way to know is stale. Failing them all is the only honest option — the same
      * reasoning as a bounded buffer overflowing rather than dropping.
      */
    private def fail(t: Throwable): Unit = if !stopped then {
        stopped = true
        hub.failAll(t)
    }

    def close(): Unit = {
        stopped = true
        follower.close()
    }
}
