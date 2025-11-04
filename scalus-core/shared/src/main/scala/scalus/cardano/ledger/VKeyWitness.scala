package scalus.cardano.ledger

import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.{Codec, Decoder, Encoder, Writer}
import scalus.builtin.{platform, ByteString, given}

/** Represents a verification key witness in Cardano */
case class VKeyWitness(
    /** Verification key (32 bytes) */
    vkey: ByteString,

    /** Signature (64 bytes) */
    signature: ByteString
) derives Codec:
    require(vkey.size == 32, s"Verification key must be 32 bytes, got ${vkey.size}")
    require(signature.size == 64, s"Signature must be 64 bytes, got ${signature.size}")

    @transient lazy val vkeyHash: AddrKeyHash = Hash(
      platform.blake2b_224(vkey)
    )

object VKeyWitness:
    given Ordering[VKeyWitness] =
        Ordering.by[VKeyWitness, ByteString](_.vkey).orElseBy[ByteString](_.signature)
