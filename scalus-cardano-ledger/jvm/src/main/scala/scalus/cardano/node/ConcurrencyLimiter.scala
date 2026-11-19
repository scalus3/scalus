package scalus.cardano.node

import scala.annotation.nowarn
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.{ExecutionContext, Future, Promise}

/** Limits the number of concurrent futures that can be executing. Cross-platform (JVM/JS) and
  * non-blocking.
  *
  * @param maxConcurrent
  *   Maximum number of futures that can execute concurrently
  */
class ConcurrencyLimiter(maxConcurrent: Int)(using ec: ExecutionContext) {
    private val running = new AtomicInteger(0)
    private val queue = new ConcurrentLinkedQueue[() => Unit]()

    /** Execute a future with concurrency limiting.
      *
      * @param f
      *   A by-name parameter that creates the future to execute
      * @return
      *   A future that completes when the original future completes
      */
    // Catches every Throwable on purpose: the slot must be released whatever `f` throws, or the
    // limiter loses a slot for good.
    @nowarn("msg=Catching Throwable")
    def apply[T](f: => Future[T]): Future[T] = {
        val promise = Promise[T]()
        queue.add { () =>
            try {
                f.onComplete { result =>
                    promise.complete(result)
                    running.decrementAndGet()
                    processQueue()
                }
            } catch {
                case e: Throwable =>
                    promise.failure(e)
                    running.decrementAndGet()
                    processQueue()
            }
        }
        processQueue()
        promise.future
    }

    private def processQueue(): Unit = {
        while running.get() < maxConcurrent && !queue.isEmpty do {
            val task = queue.poll()
            if task != null then {
                running.incrementAndGet()
                task()
            }
        }
    }
}
