package scalus.cardano.node.stream.engine.snapshot.immutabledb

import io.bullet.borer.Cbor
import scalus.cardano.ledger.{Block, KeepRaw, OriginalCborByteArray}
import scalus.utils.Hex.toHex

/** Decoder for the on-disk HardForkCombinator block wrapping that cardano-node writes into
  * `ImmutableDB` `.chunk` files.
  *
  * The upstream Haskell encoding is `encodeNS = [listLen 2, word8 era_index, inner_block]` (see
  * `ouroboros-consensus/.../HardFork/Combinator/Serialisation/Common.hs`, `encodeDiskHfcBlock`).
  * That differs from the N2N chain-sync / block-fetch envelope in two ways:
  *
  *   - On-disk has no outer `tag(24)` indirection — the inner era-specific block bytes are embedded
  *     directly after the era word.
  *   - The era index follows ouroboros-consensus-cardano's current `CardanoEras` list:
  *     `0=Byron, 1=Shelley, 2=Allegra, 3=Mary, 4=Alonzo, 5=Babbage, 6=Conway, 7=Dijkstra, …`. Newer
  *     eras are appended without renumbering older ones.
  *
  * We target Shelley+ only. Byron's block CBOR is a different shape (ouroboros-consensus
  * `ByronBlock` vs. `ShelleyBlock`), unsupported by `scalus-cardano-ledger`'s `Block` decoder.
  * Byron (era 0) chunks surface as [[Error.ByronEra]]; newer Shelley-family eras all share the same
  * `Block` CBOR shape in scalus (intra-era differences live inside `TransactionBody`), so we
  * attempt decode for any era >= 1 and let the ledger decoder report a typed failure if a truly new
  * shape appears.
  */
object HfcDiskBlockDecoder {

    /** One decoded ImmutableDB block with its era tag preserved. `blockRaw.raw` is the inner
      * era-specific block bytes (NOT the outer `[era, inner]` wrapper), so hashing / round-tripping
      * aligns with the cardano-node format for that era.
      */
    final case class DecodedBlock(
        chunkNo: Int,
        slot: Long,
        headerHash: Array[Byte],
        era: Int,
        block: KeepRaw[Block]
    )

    sealed trait Error extends Product with Serializable
    object Error {
        final case class Malformed(detail: String, sampleHex: String) extends Error
        final case class ByronEra(slot: Long) extends Error
        final case class LedgerDecode(era: Int, slot: Long, cause: Throwable) extends Error
    }

    /** Decode one [[ImmutableDb.ImmutableBlock]] into a [[DecodedBlock]]. Peels off the
      * `[era, inner]` CBOR wrapper by hand (the prefix is short and entirely deterministic for era
      * indexes 0..23) to avoid allocating a Borer `Reader` over the whole block body.
      */
    def decode(block: ImmutableDb.ImmutableBlock): Either[Error, DecodedBlock] =
        splitHfcWrapper(block.blockBytes) match {
            case Left(detail) =>
                Left(Error.Malformed(detail, hexPreview(block.blockBytes)))
            case Right((era, innerBytes)) if era == 0 =>
                Left(Error.ByronEra(block.slot))
            case Right((era, innerBytes)) =>
                given OriginalCborByteArray = OriginalCborByteArray(innerBytes)
                try {
                    val raw = Cbor.decode(innerBytes).to[KeepRaw[Block]].value
                    Right(DecodedBlock(block.chunkNo, block.slot, block.headerHash, era, raw))
                } catch {
                    case t: Throwable => Left(Error.LedgerDecode(era, block.slot, t))
                }
        }

    /** Peel `[listLen=2, word8=era, remaining...]`. Returns `(era, innerBytes)` where `innerBytes`
      * is a newly-allocated slice so the caller's `OriginalCborByteArray` anchors onto the inner
      * block's real start offset (offset 0 in the returned array).
      */
    private def splitHfcWrapper(bs: Array[Byte]): Either[String, (Int, Array[Byte])] = {
        if bs.length < 3 then return Left(s"too short for HFC wrapper: ${bs.length} B")
        val head = bs(0) & 0xff
        if head != 0x82 then return Left(f"expected listLen(2) 0x82, got 0x$head%02x")

        // CBOR uint encoding for era — valid era indexes are 0..23 inline (1-byte), but be
        // permissive about a 1-byte-follow 0x18 form in case of future extensions.
        val (era, innerStart) = bs(1) & 0xff match {
            case v if v < 0x18 => (v, 2)
            case 0x18          => (bs(2) & 0xff, 3)
            case other         => return Left(f"unsupported era encoding 0x$other%02x")
        }
        val inner = java.util.Arrays.copyOfRange(bs, innerStart, bs.length)
        Right((era, inner))
    }

    private def hexPreview(bs: Array[Byte], max: Int = 16): String =
        if bs.length <= max then bs.toHex
        else bs.take(max).toHex + "…"
}
