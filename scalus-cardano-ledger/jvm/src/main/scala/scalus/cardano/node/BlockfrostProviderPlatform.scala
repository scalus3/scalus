package scalus.cardano.node

import sttp.client4.{Backend, DefaultFutureBackend}

import scala.concurrent.Future

/** JVM-specific platform support for BlockfrostProvider. */
object BlockfrostProviderPlatform {

    /** Creates the default HTTP backend for JVM. */
    def defaultBackend: Backend[Future] = DefaultFutureBackend()
}
