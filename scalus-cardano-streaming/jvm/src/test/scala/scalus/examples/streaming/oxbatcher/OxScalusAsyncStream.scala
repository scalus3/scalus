package scalus.examples.streaming.oxbatcher

import ox.flow.Flow
import scalus.cardano.infra.ScalusAsyncStream

/** ox `Flow` instance of `ScalusAsyncStream`, plus an identity effect
  * type so the snapshot side of the streaming provider stays in
  * direct-style.
  *
  * `ScalusAsyncStream` is currently producer-only and exposes no
  * methods, so the instance is trivial. Will be replaced by the real
  * ox adapter once producer methods land on the typeclass.
  */
object OxScalusAsyncStream {

    /** Identity effect: ox is direct-style, so `provider.submit(tx)`
      * returns `Either[SubmitError, TxHash]` without an outer wrapper.
      */
    type Id[A] = A

    given ScalusAsyncStream[Flow] = new ScalusAsyncStream[Flow] {}
}
