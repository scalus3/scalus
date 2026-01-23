package scalus.cardano.ledger

import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.*
import scalus.uplc.builtin.ByteString

/** Represents a VRF certificate in Cardano, consisting of output and proof */
case class VrfCert(
    /** VRF output */
    output: ByteString,

    /** VRF proof (80 bytes) */
    proof: ByteString
) derives Codec:
    require(proof.size == 80, s"VRF proof must be 80 bytes, got ${proof.size}")
