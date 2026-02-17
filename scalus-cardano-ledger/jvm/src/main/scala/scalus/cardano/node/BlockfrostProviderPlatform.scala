package scalus.cardano.node

import sttp.client4.{Backend, DefaultFutureBackend}

import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}
import scala.concurrent.{Future, Promise}

/** JVM-specific platform support for BlockfrostProvider. */
object BlockfrostProviderPlatform {

    /** Creates the default HTTP backend for JVM. */
    def defaultBackend: Backend[Future] = DefaultFutureBackend()

    private lazy val scheduler: ScheduledExecutorService =
        Executors.newScheduledThreadPool(
          1,
          r => {
              val t = new Thread(r, "scalus-delay-scheduler")
              t.setDaemon(true)
              t
          }
        )

    /** Non-blocking delay that completes after the given number of milliseconds. */
    def delayFuture(ms: Long): Future[Unit] = {
        val promise = Promise[Unit]()
        scheduler.schedule(
          (() => promise.success(())): Runnable,
          ms,
          TimeUnit.MILLISECONDS
        )
        promise.future
    }
}
