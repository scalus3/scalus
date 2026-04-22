package scalus.cardano.node.stream.engine.replay

import scalus.cardano.node.stream.ChainPoint
import scalus.cardano.node.stream.engine.AppliedBlock

import scala.concurrent.{ExecutionContext, Future, Promise}

/** Test helper — a controllable [[AsyncReplaySource]] whose prefetch blocks on an internal
  * [[Promise]]. Lets unit tests pose "engine has buffered live events during prefetch" scenarios by
  * calling [[complete]] only after injecting the live events.
  */
final class AsyncReplaySourceStub(blocks: Seq[AppliedBlock]) extends AsyncReplaySource {

    private val gate = Promise[Unit]()

    /** Unblocks the pending (or next) [[prefetch]] call. Each test uses exactly one prefetch. */
    def complete(): Unit = {
        val _ = gate.trySuccess(())
    }

    /** Fails the pending (or next) [[prefetch]] call. */
    def fail(t: Throwable): Unit = {
        val _ = gate.tryFailure(t)
    }

    def prefetch(
        from: ChainPoint,
        to: ChainPoint
    )(using ExecutionContext): Future[Either[ReplayError, Seq[AppliedBlock]]] = {
        if from == to then Future.successful(Right(Seq.empty))
        else
            gate.future.map { _ =>
                val filtered =
                    blocks.filter(b => b.point.slot > from.slot && b.point.slot <= to.slot)
                Right(filtered)
            }
    }
}

/** Async source that always responds `Right(blocks)` without gating — for ordering tests that don't
  * need a "live events during prefetch" race.
  */
final class ImmediateAsyncReplaySource(blocks: Seq[AppliedBlock]) extends AsyncReplaySource {

    def prefetch(
        from: ChainPoint,
        to: ChainPoint
    )(using ExecutionContext): Future[Either[ReplayError, Seq[AppliedBlock]]] =
        Future.successful(
          Right(blocks.filter(b => b.point.slot > from.slot && b.point.slot <= to.slot))
        )
}

/** Async source that responds `Left(ReplaySourceExhausted)` so the cascade can move on. */
final class ExhaustedAsyncReplaySource extends AsyncReplaySource {

    def prefetch(
        from: ChainPoint,
        to: ChainPoint
    )(using ExecutionContext): Future[Either[ReplayError, Seq[AppliedBlock]]] =
        Future.successful(Left(ReplayError.ReplaySourceExhausted(from)))
}

/** Async source that responds with a non-exhaustion `ReplayError` — the engine must fail the
  * subscription with this cause and NOT cascade to the next source.
  */
final class InterruptedAsyncReplaySource(detail: String = "test interrupt")
    extends AsyncReplaySource {

    def prefetch(
        from: ChainPoint,
        to: ChainPoint
    )(using ExecutionContext): Future[Either[ReplayError, Seq[AppliedBlock]]] =
        Future.successful(
          Left(ReplayError.ReplayInterrupted(detail, new RuntimeException(detail)))
        )
}
