package scalus.cardano.infra

import scalus.cardano.node.stream.BufferPolicy

/** Uniform async-channel contract that adapter modules implement so that
  * the data node can push events into a subscriber's stream of choice
  * (fs2, ox, pekko, …) without knowing which library is in play.
  *
  * Each adapter provides a `given ScalusAsyncStream[S]` that allocates a
  * bounded async channel:
  *
  *   - the producer-side [[ScalusAsyncSink]] is handed to the data-node
  *     engine, which pushes events into it;
  *   - the consumer-side `S[A]` is handed to the application, which
  *     consumes events through the native API of its chosen stream
  *     library.
  *
  * The interface is library-agnostic: adapters back the channel with
  * native primitives (e.g. `Queue[IO, A]` for fs2, `ox.channels.Channel`
  * for ox), but the surface exposed here is the same.
  */
trait ScalusAsyncStream[S[_]] {

    /** Allocate a bounded async channel.
      *
      * @param bufferPolicy
      *   capacity bound and overflow behaviour for the consumer-side
      *   buffer.
      * @param onCancel
      *   callback invoked exactly once when the consumer-facing stream
      *   terminates (clean completion, error, or downstream cancel).
      *   The data-node engine uses this to free its per-subscription
      *   bookkeeping. Adapters wire this through their library's native
      *   finalisation hook (fs2 `onFinalize`, ox flow cleanup, …).
      * @return
      *   the producer-side sink and the consumer-side stream. The two
      *   share the channel: writes to the sink are observable as
      *   elements of the stream.
      */
    def channel[A](
        bufferPolicy: BufferPolicy,
        onCancel: () => Unit
    ): (ScalusAsyncSink[A], S[A])
}
