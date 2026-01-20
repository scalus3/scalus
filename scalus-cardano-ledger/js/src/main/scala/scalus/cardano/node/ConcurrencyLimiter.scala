package scalus.cardano.node

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}

/** Limits the number of concurrent futures that can be executing. Cross-platform (JVM/JS) and
  * non-blocking.
  *
  * @param maxConcurrent
  *   Maximum number of futures that can execute concurrently
  */
class ConcurrencyLimiter(maxConcurrent: Int)(using ec: ExecutionContext) {
    private var running = 0
    private val queue = mutable.Queue[() => Unit]()

    /** Execute a future with concurrency limiting.
      *
      * @param f
      *   A by-name parameter that creates the future to execute
      * @return
      *   A future that completes when the original future completes
      */
    def apply[T](f: => Future[T]): Future[T] = {
        val promise = Promise[T]()
        queue.enqueue { () =>
            try {
                f.onComplete { result =>
                    promise.complete(result)
                    running -= 1
                    processQueue()
                }
            } catch {
                case e: Throwable =>
                    promise.failure(e)
                    running -= 1
                    processQueue()
            }
        }
        processQueue()
        promise.future
    }

    private def processQueue(): Unit = {
        while running < maxConcurrent && queue.nonEmpty do {
            running += 1
            queue.dequeue()()
        }
    }
}
