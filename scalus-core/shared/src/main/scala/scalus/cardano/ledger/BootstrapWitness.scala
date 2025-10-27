package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import scalus.builtin.{platform, ByteString}

/** Represents a bootstrap witness in Cardano (for Byron-era addresses) */
case class BootstrapWitness(
    /** Public key (32 bytes) */
    publicKey: ByteString,

    /** Signature (64 bytes) */
    signature: ByteString,

    /** Chain code (32 bytes) */
    chainCode: ByteString,

    /** Attributes */
    attributes: ByteString
) derives Codec:
    require(publicKey.size == 32, s"Public key must be 32 bytes, got ${publicKey.size}")
    require(signature.size == 64, s"Signature must be 64 bytes, got ${signature.size}")
    require(chainCode.size == 32, s"Chain code must be 32 bytes, got ${chainCode.size}")

    /** cardano-ledger-core/src/Cardano/Ledger/Keys/Bootstrap.hs bootstrapWitKeyHash
      */
    @transient lazy val addrKeyHash: AddrKeyHash = {
        val prefix = ByteString.fromHex("8300820058")
        val bytes = prefix ++ publicKey ++ chainCode ++ attributes
        Hash(platform.sha3_256(bytes).take(28))
    }

object BootstrapWitness:
    given Ordering[BootstrapWitness] =
        Ordering.by[BootstrapWitness, ByteString](_.addrKeyHash)
