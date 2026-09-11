package scalus.cardano.node.stream.internal

import scala.concurrent.{ExecutionContext, Future}
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
    @volatile private var started = false

    /** Begin pumping.
      *
      * Idempotent: a second call is ignored, and a stopped driver stays stopped. Two pumps would
      * both pull from a source documented as single-consumer — `Mailbox.pull` hands concurrent
      * callers the same promise — so a single event would be applied twice, appending the same
      * block to `recent` and re-emitting its deltas.
      */
    def start(): Unit = {
        val begin = synchronized {
            if started || stopped then false
            else { started = true; true }
        }
        if begin then {
            // Before the first pull, so nothing is produced into a source nobody is reading yet.
            follower.start()
            pump()
        }
    }

    private def pump(): Unit =
        follower.events.pull().onComplete {
            case Success(Some(event)) =>
                // Applying can throw — a rollback deeper than the horizon, a capability the
                // provider declared it does not have. That is a failure of the whole feed, not of
                // one event, so it travels the same path as a follower failure.
                val continue =
                    try
                        apply(event)
                        true
                    catch
                        case t: Throwable =>
                            fail(t)
                            false
                // Re-dispatched rather than called here. A buffered backlog completes every pull
                // synchronously, and under an inline executor a direct recursive call would nest
                // one frame per drained event until the stack gave out — turning a queue that
                // merely got long into a dead feed.
                if continue && !stopped then Future.unit.foreach(_ => pump())
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
    private def fail(t: Throwable): Unit = {
        val first = synchronized {
            if stopped then false
            else { stopped = true; true }
        }
        if first then {
            // Stop the follower too. Nothing will read its events again, and a metered one left
            // running keeps spending quota on a feed with no consumer.
            follower.close()
            hub.failAll(t)
        }
    }

    /** Stop pumping and end every subscription.
      *
      * Closing the hub here rather than relying on the follower's end-of-stream is deliberate: the
      * pump is stopped by this call, so nothing is left to observe that end, and subscribers would
      * be left parked on promises that never complete.
      */
    def close(): Unit = {
        val first = synchronized {
            if stopped then false
            else { stopped = true; true }
        }
        follower.close()
        if first then hub.closeAll()
    }
}
