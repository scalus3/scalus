package scalus.cardano.n2n

import scalus.cardano.node.stream.DeltaBufferPolicy

import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** Tunables for [[scalus.cardano.n2n.jvm.NodeToNodeClient.connect]]. Defaults are the
  * Ouroboros-network-recommended values — see `docs/local/claude/indexer/n2n-transport.md` § *The
  * public surface of M4*.
  *
  * @param handshakeTimeout
  *   maximum time from socket-open to handshake completion. On expiry the returned connection
  *   future fails with [[handshake.HandshakeError.Timeout]].
  * @param keepAliveInterval
  *   delay between consecutive heartbeats.
  * @param keepAliveTimeout
  *   per-beat deadline — if no response arrives within this many seconds of send, the connection
  *   root fires with [[keepalive.KeepAliveError.Timeout]].
  * @param sduMaxPayload
  *   limit on the payload portion of a single SDU (defaults to the wire maximum).
  * @param mailboxCapacity
  *   per-route mailbox capacity. Overflow causes a
  *   [[scalus.cardano.infra.ScalusBufferOverflowException]] on the state machine's next pull.
  */
final case class ClientConfig(
    handshakeTimeout: FiniteDuration = 30.seconds,
    keepAliveInterval: FiniteDuration = 30.seconds,
    keepAliveTimeout: FiniteDuration = 60.seconds,
    sduMaxPayload: Int = Sdu.MaxPayloadSize,
    mailboxCapacity: DeltaBufferPolicy = DeltaBufferPolicy.Bounded(256)
)

object ClientConfig {
    val default: ClientConfig = ClientConfig()
}
