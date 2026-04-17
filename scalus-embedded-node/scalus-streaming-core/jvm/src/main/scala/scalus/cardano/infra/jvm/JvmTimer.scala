package scalus.cardano.infra.jvm

import scalus.cardano.infra.{Cancellable, Timer}

import java.util.concurrent.{Executors, ScheduledExecutorService, ScheduledFuture, TimeUnit}
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

/** JVM [[Timer]] backed by a single-thread `ScheduledExecutorService` with daemon threads. One
  * instance is enough for the whole process — the global [[JvmTimer.shared]] is exposed as a
  * convenience; applications that want to bound timer-thread usage can construct their own.
  *
  * Scheduled-action exception policy: every exception thrown by a scheduled action is logged via
  * the configured `logger` and then swallowed so the scheduler keeps accepting further work. The
  * log is the information channel — silent swallowing would be a debuggability hazard, so callers
  * get a full log line with stacktrace.
  */
final class JvmTimer(
    executor: ScheduledExecutorService = JvmTimer.defaultExecutor(),
    logger: scribe.Logger = scribe.Logger[JvmTimer]
) extends Timer {

    def schedule(delay: FiniteDuration)(action: => Unit): Cancellable = {
        val task: Runnable = () =>
            try action
            catch {
                case NonFatal(t) => logger.error("scheduled action threw", t)
            }
        val future: ScheduledFuture[?] =
            executor.schedule(task, delay.toNanos, TimeUnit.NANOSECONDS)
        () =>
            val _ = future.cancel(false)
    }

    /** Tear down the underlying executor. Only call on a [[JvmTimer]] this code owns — don't shut
      * down [[JvmTimer.shared]] from application code.
      */
    def shutdown(): Unit = {
        val _ = executor.shutdownNow()
    }
}

object JvmTimer {

    /** Process-wide shared timer. Daemon-threaded, so it never blocks JVM shutdown. */
    lazy val shared: JvmTimer = new JvmTimer()

    private def defaultExecutor(): ScheduledExecutorService =
        Executors.newSingleThreadScheduledExecutor { r =>
            val t = new Thread(r, "scalus-n2n-timer")
            t.setDaemon(true)
            t
        }
}
