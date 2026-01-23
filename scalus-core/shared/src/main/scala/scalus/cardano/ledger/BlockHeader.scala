package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import scalus.uplc.builtin.ByteString

/** Represents a block header in Cardano */
case class BlockHeader(
    /** Header body with block metadata */
    headerBody: BlockHeaderBody,

    /** Body signature (KES signature, 448 bytes) */
    bodySignature: ByteString
) derives Codec {
    require(
      bodySignature.size == 448,
      s"Body signature must be 448 bytes, got ${bodySignature.size}"
    )

    /** Get block number */
    def blockNumber: Long = headerBody.blockNumber

    /** Get slot number */
    def slot: Long = headerBody.slot

    /** Get previous block hash */
    def prevHash: Option[BlockHash] = headerBody.prevHash
}
