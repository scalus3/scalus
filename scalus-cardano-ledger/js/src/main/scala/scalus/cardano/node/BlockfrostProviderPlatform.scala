package scalus.cardano.node

import sttp.client4.Backend
import sttp.client4.fetch.FetchBackend

import scala.concurrent.{Future, Promise}
import scala.scalajs.js.timers

/** JavaScript-specific platform support for BlockfrostProvider. */
object BlockfrostProviderPlatform {

    /** Creates the default HTTP backend for JavaScript (browser/Node.js). */
    def defaultBackend: Backend[Future] = FetchBackend()

    /** Non-blocking delay that completes after the given number of milliseconds. */
    def delayFuture(ms: Long): Future[Unit] = {
        val promise = Promise[Unit]()
        timers.setTimeout(ms.toDouble) {
            promise.success(())
        }
        promise.future
    }
}
