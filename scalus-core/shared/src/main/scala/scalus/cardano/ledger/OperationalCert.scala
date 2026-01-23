package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import scalus.uplc.builtin.ByteString

/** Represents an operational certificate in Cardano
  *
  * An operational certificate authorizes a node to create blocks
  */
case class OperationalCert(
    /** Hot key verification key (KES) */
    hotVKey: ByteString,

    /** Sequence number */
    sequenceNumber: Long,

    /** KES period */
    kesPeriod: Long,

    /** Sigma (signature) */
    sigma: ByteString
) derives Codec:
    require(hotVKey.size == 32, s"Hot key must be 32 bytes, got ${hotVKey.size}")
    require(sequenceNumber >= 0, s"Sequence number must be non-negative, got $sequenceNumber")
    require(kesPeriod >= 0, s"KES period must be non-negative, got $kesPeriod")
    require(sigma.size == 64, s"Signature must be 64 bytes, got ${sigma.size}")
