package scalus.cardano.ledger

import io.bullet.borer.NullOptions.given
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.*
import scalus.uplc.builtin.ByteString

/** Represents the body of a block header in Cardano */
case class BlockHeaderBody(
    /** Block number */
    blockNumber: Long,

    /** Slot number */
    slot: Long,

    /** Previous block hash (or nil for genesis) */
    prevHash: Option[BlockHash],

    /** Issuer verification key */
    issuerVkey: ByteString,

    /** VRF verification key */
    vrfVkey: ByteString,

    /** VRF certificate */
    vrfResult: VrfCert,

    /** Block body size in bytes */
    blockBodySize: Long,

    /** Block body hash */
    blockBodyHash: BlockHash,

    /** Operational certificate */
    operationalCert: OperationalCert,

    /** Protocol version */
    protocolVersion: ProtocolVersion
) derives Codec {
    require(blockNumber >= 0, s"Block number must be non-negative, got $blockNumber")
    require(slot >= 0, s"Slot must be non-negative, got $slot")
    require(
      issuerVkey.size == 32,
      s"Issuer verification key must be 32 bytes, got ${issuerVkey.size}"
    )
    require(vrfVkey.size == 32, s"VRF verification key must be 32 bytes, got ${vrfVkey.size}")
    require(blockBodySize >= 0, s"Block body size must be non-negative, got $blockBodySize")
}
