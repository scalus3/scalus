package scalus.cardano.network.blockfetch

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}
import scalus.cardano.network.chainsync.{ChainSyncMessage, Point}
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
  * [[scalus.cardano.network.chainsync.Point]]); `base.block` uses the same HardFork-combinator
  * envelope shape (`[era, tag24(blockCbor)]`) that headers do in chain-sync —
  * [[BlockFetchMessage.MsgBlock]] extracts `(era, blockBytes)` and
  * [[scalus.cardano.network.BlockEnvelope.decodeBlock]] handles era-to-ledger-type dispatch at
  * the applier layer.
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
      *   HardFork era index (Byron = 0, Shelley = 1, …, Conway = 6).
      * @param blockBytes
      *   raw CBOR bytes of the era-specific block (tag24 inner payload). Parsed upstream by
      *   [[scalus.cardano.network.BlockEnvelope.decodeBlock]].
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
            case MsgClientDone => w.writeArrayHeader(1).writeInt(1)
            case MsgStartBatch => w.writeArrayHeader(1).writeInt(2)
            case MsgNoBlocks   => w.writeArrayHeader(1).writeInt(3)
            case MsgBlock(era, bytes) =>
                w.writeArrayHeader(2).writeInt(4)
                ChainSyncMessage.writeHardForkEnvelope(w, era, bytes)
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
                    val (era, bytes) = ChainSyncMessage.readHardForkEnvelope(r)
                    MsgBlock(era, bytes)
                case 5 if arrLen == 1 => MsgBatchDone
                case other =>
                    r.validationFailure(s"unexpected blockFetchMessage tag=$other arrLen=$arrLen")
            }
        }
}
