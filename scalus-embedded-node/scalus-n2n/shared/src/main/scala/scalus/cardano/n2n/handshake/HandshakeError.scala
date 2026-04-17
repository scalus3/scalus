package scalus.cardano.n2n.handshake

/** Errors raised by [[HandshakeDriver]]. All are terminal for the handshake future; whether they
  * escalate to the connection root depends on the event — see the handshake section of
  * `docs/local/claude/indexer/n2n-transport.md` for the policy table.
  */
sealed abstract class HandshakeError(message: String, cause: Throwable = null)
    extends RuntimeException(message, cause)

object HandshakeError {

    /** Peer refused with `VersionMismatch` — none of our proposed versions were acceptable, and the
      * peer listed its own supported set.
      */
    final class VersionMismatch(val proposed: Set[Int], val peerSupported: List[Int])
        extends HandshakeError(
          s"handshake version mismatch: we proposed ${proposed.toSeq.sorted.mkString("[", ",", "]")}, " +
              s"peer supports ${peerSupported.mkString("[", ",", "]")}"
        )

    /** Peer refused with a reason specific to a version — either peer-side decode failure or an
      * explicit refusal (e.g. magic mismatch).
      */
    final class Refused(val versionTried: Int, val reason: String)
        extends HandshakeError(s"handshake refused at v$versionTried: $reason")

    /** Bytes from the peer could not be decoded as a valid handshake message — wraps the underlying
      * borer error.
      */
    final class DecodeError(message: String, cause: Throwable)
        extends HandshakeError(s"handshake decode error: $message", cause)

    /** The 30s handshake deadline expired before a reply arrived. Fired via the `handshakeScope`
      * timer; does NOT itself cancel the connection root.
      */
    final class HandshakeTimeoutException extends HandshakeError("handshake timed out")

    /** Peer sent a message we don't expect in the initiator flow (e.g. `MsgProposeVersions`). */
    final class UnexpectedMessage(val received: String)
        extends HandshakeError(s"unexpected handshake message: $received")
}
