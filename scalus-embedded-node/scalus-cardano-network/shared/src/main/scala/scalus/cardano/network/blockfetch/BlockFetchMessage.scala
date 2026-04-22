package scalus.cardano.network.blockfetch

import io.bullet.borer.{Cbor, Decoder, Encoder, Reader, Tag, Writer}
import scalus.cardano.network.chainsync.Point
import scalus.uplc.builtin.ByteString

/** BlockFetch mini-protocol messages (protocol id 3), per the ouroboros-network CDDL
  * (`block-fetch.cddl`):
  *
  * {{{
  * blockFetchMessage
  *      = msgRequestRange
  *      / msgClientDone
  *      / msgStartBatch
  *      / msgNoBlocks
  *      / msgBlock
  *      / msgBatchDone
  *
  * msgRequestRange = [0, base.point, base.point]
  * msgClientDone   = [1]
  * msgStartBatch   = [2]
  * msgNoBlocks     = [3]
  * msgBlock        = [4, base.block]
  * msgBatchDone    = [5]
  * }}}
  *
  * `base.point` is the same shape as in chain-sync (see
  * [[scalus.cardano.network.chainsync.Point]]).
  *
  * `base.block` uses a different era-wrapping shape from ChainSync's `base.header`, and a different
  * era-numbering convention:
  *
  *   - ChainSync header: `[era_u8, tag24(headerCbor)]` — era outside the tag24 wrapper, using
  *     ouroboros-consensus HardForkCombinator 0-based indexing (Byron=0..Conway=6).
  *   - BlockFetch block: `tag24([era_u16, blockCbor])` — era inside the tag24 wrapper, using the
  *     cardano-ledger spec 1-based indexing (Byron=1..Conway=7). Pallas's
  *     `pallas-traverse/src/era.rs` uses the same 1-based scheme; its
  *     `MultiEraBlock::decode_conway` parses the tag24 payload as `(u16, Block)`.
  *
  * Our codec normalises the 1-based BlockFetch era to the 0-based HardFork scheme the rest of the
  * codebase uses, so downstream code doesn't have to special-case two era numberings.
  * `MsgBlock.era` is therefore 0-based and directly consumable by
  * [[scalus.cardano.network.Era.fromWire]].
  */
sealed trait BlockFetchMessage

object BlockFetchMessage {

    /** Client → server: please stream every block in the inclusive `[start, end]` range. */
    final case class MsgRequestRange(start: Point, end: Point) extends BlockFetchMessage

    /** Client → server: we're done talking to you; close the protocol cleanly. */
    case object MsgClientDone extends BlockFetchMessage

    /** Server → client: about to start streaming blocks for the requested range. */
    case object MsgStartBatch extends BlockFetchMessage

    /** Server → client: the requested range produced no blocks (e.g. peer rolled back between our
      * chain-sync observation and our fetch request).
      */
    case object MsgNoBlocks extends BlockFetchMessage

    /** Server → client: one block from the batch.
      *
      * @param era
      *   HardFork era index (Byron=0, Shelley=1, …, Conway=6). Wire-extracted from inside the
      *   `tag24` wrapper; cross-validate against the ChainSync header's era at the applier.
      * @param blockBytes
      *   raw CBOR bytes of the era-specific block only (the `[era, block]` tuple already
      *   unwrapped). Parsed upstream by [[scalus.cardano.network.BlockEnvelope.decodeBlock]].
      */
    final case class MsgBlock(era: Int, blockBytes: ByteString) extends BlockFetchMessage

    /** Server → client: batch complete, no more blocks to deliver. */
    case object MsgBatchDone extends BlockFetchMessage

    // ----------------------------------------------------------------------------------------
    // CBOR codec — hand-written.
    // ----------------------------------------------------------------------------------------

    given Encoder[BlockFetchMessage] with
        def write(w: Writer, m: BlockFetchMessage): Writer = m match {
            case MsgRequestRange(start, end) =>
                w.writeArrayHeader(3).writeInt(0).write(start).write(end)
            case MsgClientDone        => w.writeArrayHeader(1).writeInt(1)
            case MsgStartBatch        => w.writeArrayHeader(1).writeInt(2)
            case MsgNoBlocks          => w.writeArrayHeader(1).writeInt(3)
            case MsgBlock(era, bytes) =>
                // Outer: [4, tag24(innerBytes)]. innerBytes is itself CBOR: [era_u16, block].
                // Era on the wire is 1-based (Byron=1..Conway=7); we store the 0-based
                // HardFork era in the case class, so re-add 1 before emitting.
                val wireEra = era + 1
                val eraCbor = Cbor.encode(wireEra: Int).toByteArray
                val inner = new Array[Byte](1 + eraCbor.length + bytes.size)
                inner(0) = 0x82.toByte // CBOR array header (2 items, definite length)
                System.arraycopy(eraCbor, 0, inner, 1, eraCbor.length)
                System.arraycopy(bytes.bytes, 0, inner, 1 + eraCbor.length, bytes.size)
                w.writeArrayHeader(2)
                    .writeInt(4)
                    .writeTag(Tag.EmbeddedCBOR)
                    .writeBytes(inner)
            case MsgBatchDone => w.writeArrayHeader(1).writeInt(5)
        }

    given Decoder[BlockFetchMessage] with
        def read(r: Reader): BlockFetchMessage = {
            val arrLen = r.readArrayHeader().toInt
            r.readInt() match {
                case 0 if arrLen == 3 =>
                    MsgRequestRange(r.read[Point](), r.read[Point]())
                case 1 if arrLen == 1 => MsgClientDone
                case 2 if arrLen == 1 => MsgStartBatch
                case 3 if arrLen == 1 => MsgNoBlocks
                case 4 if arrLen == 2 =>
                    r.readTag() match {
                        case Tag.EmbeddedCBOR => ()
                        case other =>
                            r.validationFailure(
                              s"MsgBlock: expected CBOR tag 24 (EmbeddedCBOR), got $other"
                            )
                    }
                    val wrapperBytes = r.readByteArray()
                    val (wireEra, blockBytes) = decodeEraAndBlock(wrapperBytes)
                    // Normalise BlockFetch's 1-based era to the 0-based HardFork index used
                    // by the rest of the pipeline. Byron=1→0 is the minimum valid input.
                    MsgBlock(wireEra - 1, blockBytes)
                case 5 if arrLen == 1 => MsgBatchDone
                case other =>
                    r.validationFailure(s"unexpected blockFetchMessage tag=$other arrLen=$arrLen")
            }
        }

    /** Parse the tag24 payload of a MsgBlock as `[era, block_cbor]` and return the era + the raw
      * bytes of the block sub-value (sliced from `wrapperBytes`, no re-encoding).
      *
      * Implemented by hand rather than through borer because we want to return the block as a raw
      * byte slice without forcing borer to decode or skip over it — era is a small uint, everything
      * after it (up to the end of the wrapper) is the block's serialised CBOR.
      */
    private def decodeEraAndBlock(wrapperBytes: Array[Byte]): (Int, ByteString) = {
        if wrapperBytes.isEmpty || wrapperBytes(0) != 0x82.toByte then
            throw new IllegalArgumentException(
              "MsgBlock inner: expected CBOR array-of-2 header (0x82)"
            )
        val b1 = wrapperBytes(1) & 0xff
        // CBOR uint major type 0: low 5 bits encode either the value directly (0..23) or a
        // width marker for the following bytes.
        val (era, eraLen) = b1 match {
            case b if b <= 0x17 => (b, 1)
            case 0x18 =>
                (wrapperBytes(2) & 0xff, 2)
            case 0x19 =>
                (((wrapperBytes(2) & 0xff) << 8) | (wrapperBytes(3) & 0xff), 3)
            case other =>
                throw new IllegalArgumentException(
                  s"MsgBlock inner: unsupported era CBOR width byte 0x${other.toHexString}"
                )
        }
        val blockStart = 1 + eraLen
        (era, ByteString.unsafeFromArray(wrapperBytes.slice(blockStart, wrapperBytes.length)))
    }
}
