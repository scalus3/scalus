package scalus.utils

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

/** JVM-specific extension methods for Future.
  *
  * These operations use blocking calls like `Await.result` which are only available on the JVM
  * platform. Import this object to use the `await` extension method on any Future.
  *
  * ==Usage==
  * {{{
  * import scalus.utils.await
  *
  * val result = someFuture.await()           // wait indefinitely
  * val result = someFuture.await(30.seconds) // wait with timeout
  * }}}
  */

extension [T](future: Future[T])
    /** Extension method to block and await the result of a Future.
      *
      * @param timeout
      *   maximum duration to wait (default: infinite)
      * @return
      *   the result of the Future
      * @throws java.util.concurrent.TimeoutException
      *   if the timeout is exceeded
      * @throws Exception
      *   if the Future fails
      */
    def await(timeout: Duration = Duration.Inf): T =
        Await.result(future, timeout)
