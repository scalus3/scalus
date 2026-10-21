package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import scalus.serialization.cbor.Cbor
import scalus.uplc.builtin.{platform, ByteString}

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

    /** The block hash: blake2b-256 of the header CBOR, as Cardano names a block.
      *
      * It hashes a re-encoding of the decoded header, since `BlockHeader` keeps no original bytes
      * (it is not `KeepRaw`). That gives the hash of the original bytes only when the header was
      * encoded canonically, as the node encodes a Praos header and as this encoder writes it. A
      * header with another encoding of the same values would get a different hash. `BlockHashTest`
      * checks the hash against two mainnet blocks, spec [TE-11].
      */
    @transient lazy val hash: BlockHash =
        BlockHash.fromByteString(platform.blake2b_256(Cbor.encodeToByteString(this)))
}
