package scalus.cardano.node.stream

import scalus.cardano.node.stream.engine.AppliedBlock

import scala.concurrent.Future

/** Test-only mixin that lets tests drive the [[BaseStreamProvider]] without a real chain-sync
  * pipeline.
  *
  * Two layers of push:
  *
  *   - `pushTip` / `pushUtxo` fan raw events straight to the subscriber sinks, bypassing the
  *     rollback buffer and per-query indexes. Useful for exercising adapter plumbing in isolation,
  *     which is what the M1 tests do.
  *   - `applyBlock` / `rollbackTo` drive the engine proper: the rollback buffer, tx-hash index, and
  *     each per-subscription UTxO index are all updated; subscriber events flow out of the real
  *     fan-out logic.
  *
  * Each push returns a `Future[Unit]` that completes once every registered sink has accepted the
  * event; tests should await this before issuing the next push so ordering is deterministic.
  */
trait SyntheticEventSource[F[_], C[_]] extends BaseStreamProvider[F, C] {

    /** Raw tip fan-out — does not update the engine's tip state. Equivalent to the M1 `pushTip`
      * helper.
      */
    def pushTip(tip: ChainTip): Future[Unit] =
        engine.testFanoutTip(tip)

    /** Convenience overload: build a [[ChainTip]] from a point and height 0. Useful for M1-style
      * tip tests that don't care about blockNo.
      */
    def pushTip(point: ChainPoint): Future[Unit] =
        engine.testFanoutTip(ChainTip(point, 0L))

    /** Raw UTxO-event fan-out — delivered verbatim to every active UTxO subscriber, ignoring
      * queries. Equivalent to the M1 `pushUtxo` helper.
      */
    def pushUtxo(event: UtxoEvent): Future[Unit] =
        engine.testFanoutUtxoEvent(event)

    /** Full-path block application: updates the rollback buffer, per-query indexes, tx-hash index,
      * and tip cell; emits `Created` / `Spent` events to matching subscribers.
      */
    def applyBlock(block: AppliedBlock): Future[Unit] =
        engine.onRollForward(block)

    /** Full-path rollback: reverses the volatile tail back to `to`, emits `RolledBack(to)` to every
      * subscriber and a new tip event.
      */
    def rollbackTo(to: ChainPoint): Future[Unit] =
        engine.onRollBackward(to)

    /** Complete every active subscription. */
    def closeAllSubs(): Future[Unit] = engine.testCloseAll()

    /** Fail every active subscription with `t`. */
    def failAllSubs(t: Throwable): Future[Unit] = engine.testFailAll(t)

    /** Total number of active subscribers across every supported event kind. Does not include
      * tx/block queries since they are not wired until a later milestone.
      */
    def activeSubscriptionCount: Int = engine.testActiveSubscriptionCount
}
