package scalus.cardano.n2n.keepalive

/** Terminal errors raised by [[KeepAliveDriver]]. Both variants are connection-fatal by policy —
  * the driver fires `connectionRoot.cancel(...)` with the error as the cause, and every subscriber
  * sees the connection tear down.
  */
sealed abstract class KeepAliveError(message: String) extends RuntimeException(message)

object KeepAliveError {

    /** Peer echoed a cookie that doesn't match what we sent — wire-protocol violation. */
    final class CookieMismatch(val expected: Int, val received: Int)
        extends KeepAliveError(
          s"keep-alive cookie mismatch: expected $expected, got $received"
        )

    /** A beat-response didn't arrive within the per-beat deadline — peer is unresponsive. */
    final class Timeout extends KeepAliveError("keep-alive beat timed out")
}
