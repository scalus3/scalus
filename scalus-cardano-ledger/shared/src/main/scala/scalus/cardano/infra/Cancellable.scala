package scalus.cardano.infra

/** Handle that undoes a registration. */
trait Cancellable {
    def cancel(): Unit
}

object Cancellable {

    /** Pre-allocated no-op handle. Returned by [[CancelToken.onCancel]] when the token is already
      * cancelled at registration time (nothing to deregister) and by [[CancelToken.never]].
      */
    val noop: Cancellable = () => ()
}
