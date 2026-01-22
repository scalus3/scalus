package scalus.cardano.node

import sttp.client4.Backend
import sttp.client4.fetch.FetchBackend

import scala.concurrent.Future

/** JavaScript-specific platform support for BlockfrostProvider. */
object BlockfrostProviderPlatform {

    /** Creates the default HTTP backend for JavaScript (browser/Node.js). */
    def defaultBackend: Backend[Future] = FetchBackend()
}
