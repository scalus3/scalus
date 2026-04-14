package scalus.cardano.node.stream

import scalus.cardano.infra.ScalusAsyncSink

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.{ExecutionContext, Future}

/** Test-only mixin that exposes the registry maintained by
  * [[BaseStreamProvider]] so tests can drive subscriptions end-to-end
  * without a real engine.
  *
  * Push methods fan out to every active subscriber regardless of the
  * subscription's query — query-aware matching arrives with the
  * rollback buffer + per-query index. Tests that want selectivity
  * subscribe with a single query and rely on consumer-side `collect` /
  * `filter`.
  *
  * Each push method returns a `Future[Unit]` that completes once every
  * registered sink has accepted the event. Tests should await this
  * future before issuing the next push so that ordering is
  * deterministic — without awaiting, the underlying adapter runtimes
  * (cats-effect dispatcher, ox virtual threads) may schedule offers
  * concurrently and reorder the queue.
  */
trait SyntheticEventSource[F[_], C[_]] extends BaseStreamProvider[F, C] {

    /** ExecutionContext used to flatten the per-sink offer futures into
      * a single result future. The default is `global`; tests can
      * override if they want to keep allocation off the common pool.
      */
    protected def syntheticEC: ExecutionContext = ExecutionContext.global

    def pushUtxo(event: UtxoEvent): Future[Unit] =
        fanOut(utxoSubs, _.sink.offer(event))

    def pushTx(event: TransactionEvent): Future[Unit] =
        fanOut(txSubs, _.sink.offer(event))

    def pushBlock(event: BlockEvent): Future[Unit] =
        fanOut(blockSubs, _.sink.offer(event))

    def pushTip(point: ChainPoint): Future[Unit] =
        fanOut(tipSubs, _.offer(point))

    /** Close every active subscription cleanly. */
    def closeAllSubs(): Future[Unit] =
        drainAll(_.sink.complete(), _.sink.complete(), _.sink.complete(), _.complete())

    /** Fail every active subscription with `t`. */
    def failAllSubs(t: Throwable): Future[Unit] =
        drainAll(_.sink.fail(t), _.sink.fail(t), _.sink.fail(t), _.fail(t))

    /** Total number of active subscribers across every event kind.
      * Useful for tests that want to verify cancellation and close
      * paths actually shrink the registry.
      */
    def activeSubscriptionCount: Int =
        utxoSubs.get().size + txSubs.get().size + blockSubs.get().size + tipSubs.get().size

    private def fanOut[V](
        subs: AtomicReference[Map[SubId, V]],
        deliver: V => Future[Unit]
    ): Future[Unit] = {
        val map = subs.get()
        if map.isEmpty then Future.unit
        else if map.sizeIs == 1 then deliver(map.head._2)
        else sequenceUnit(map.values.iterator.map(deliver).toList)
    }

    private def drainAll(
        utxo: UtxoSub => Future[Unit],
        tx: TxSub => Future[Unit],
        block: BlockSub => Future[Unit],
        tip: ScalusAsyncSink[ChainPoint] => Future[Unit]
    ): Future[Unit] = {
        val drained =
            utxoSubs.getAndSet(Map.empty).values.iterator.map(utxo) ++
                txSubs.getAndSet(Map.empty).values.iterator.map(tx) ++
                blockSubs.getAndSet(Map.empty).values.iterator.map(block) ++
                tipSubs.getAndSet(Map.empty).values.iterator.map(tip)
        sequenceUnit(drained.toList)
    }

    private def sequenceUnit(fs: List[Future[Unit]]): Future[Unit] = fs match {
        case Nil      => Future.unit
        case f :: Nil => f
        case _ => Future.sequence(fs)(using implicitly, syntheticEC).map(_ => ())(syntheticEC)
    }
}
