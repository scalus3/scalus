package scalus.cardano.network.chainsync

import io.bullet.borer.{Decoder, Encoder, Reader, Tag, Writer}
import scalus.cardano.ledger.BlockHash
import scalus.cardano.node.stream.ChainPoint
import scalus.uplc.builtin.ByteString

/** ChainSync mini-protocol messages (protocol id 2), per the ouroboros-network CDDL
  * (`chain-sync.cddl`):
  *
  * {{{
  * chainSyncMessage
  *     = msgRequestNext
  *     / msgAwaitReply
  *     / msgRollForward
  *     / msgRollBackward
  *     / msgFindIntersect
  *     / msgIntersectFound
  *     / msgIntersectNotFound
  *     / chainSyncMsgDone
  *
  * msgRequestNext         = [0]
  * msgAwaitReply          = [1]
  * msgRollForward         = [2, base.header, base.tip]
  * msgRollBackward        = [3, base.point, base.tip]
  * msgFindIntersect       = [4, base.points]
  * msgIntersectFound      = [5, base.point, base.tip]
  * msgIntersectNotFound   = [6, base.tip]
  * chainSyncMsgDone       = [7]
  * }}}
  *
  * `header`, `point`, and `tip` are polymorphic in the base codec. For Cardano N2N, the
  * instantiations used by `ouroboros-consensus-cardano` are:
  *
  *   - `point` — `[]` (origin) or `[slot: word64, hash: bstr]`. See [[Point]].
  *   - `tip` — `[point, blockNo: word64]`. See [[Tip]].
  *   - `header` — HardFork-combinator envelope `[era: word8, tag24(headerCbor)]` where `era` is
  *     the `HardForkBlock` era index (Byron=0, Shelley=1, …, Conway=6) and `tag24` is CBOR tag 24
  *     (embedded CBOR) wrapping the era-specific header bytes. The decoder extracts the envelope
  *     into `(era, headerBytes)`; era-mapping plus ledger-level decoding lives in
  *     [[scalus.cardano.network.BlockEnvelope]] at the applier layer so the transport module
  *     stays free of ledger-dispatch logic.
  */
sealed trait ChainSyncMessage

object ChainSyncMessage {

    /** Client → server: please give me the next chain event. */
    case object MsgRequestNext extends ChainSyncMessage

    /** Server → client: we have no new event right now; the next response on this stream will be a
      * real event once one becomes available.
      */
    case object MsgAwaitReply extends ChainSyncMessage

    /** Server → client: chain advanced by one block.
      *
      * @param era
      *   HardFork era index carried by the envelope. Byron = 0, Shelley = 1, Allegra = 2, Mary =
      *   3, Alonzo = 4, Babbage = 5, Conway = 6.
      * @param headerBytes
      *   raw CBOR bytes of the era-specific block header (unwrapped from the inner tag24).
      *   Decoded upstream by [[scalus.cardano.network.BlockEnvelope.decodeHeader]].
      */
    final case class MsgRollForward(era: Int, headerBytes: ByteString, tip: Tip)
        extends ChainSyncMessage

    /** Server → client: chain rolled back; subscribers must revert their state to `to`. */
    final case class MsgRollBackward(to: Point, tip: Tip) extends ChainSyncMessage

    /** Client → server: find an intersection with this list of candidate points. */
    final case class MsgFindIntersect(points: List[Point]) extends ChainSyncMessage

    /** Server → client: intersection found at `point`; the peer's tip is `tip`. */
    final case class MsgIntersectFound(point: Point, tip: Tip) extends ChainSyncMessage

    /** Server → client: none of the candidate points are on the peer's chain. */
    final case class MsgIntersectNotFound(tip: Tip) extends ChainSyncMessage

    /** Either side: terminate this protocol instance. Best-effort on clean shutdown. */
    case object MsgDone extends ChainSyncMessage

    // ----------------------------------------------------------------------------------------
    // CBOR codec — hand-written, matching the style of `HandshakeMessage` / `KeepAliveMessage`.
    // ----------------------------------------------------------------------------------------

    given Encoder[ChainSyncMessage] with
        def write(w: Writer, m: ChainSyncMessage): Writer = m match {
            case MsgRequestNext => w.writeArrayHeader(1).writeInt(0)
            case MsgAwaitReply  => w.writeArrayHeader(1).writeInt(1)
            case MsgRollForward(era, headerBytes, tip) =>
                w.writeArrayHeader(3).writeInt(2)
                writeHardForkEnvelope(w, era, headerBytes)
                w.write(tip)
            case MsgRollBackward(to, tip) =>
                w.writeArrayHeader(3).writeInt(3).write(to).write(tip)
            case MsgFindIntersect(points) =>
                w.writeArrayHeader(2).writeInt(4).writeArrayHeader(points.size)
                points.foreach(p => w.write(p))
                w
            case MsgIntersectFound(point, tip) =>
                w.writeArrayHeader(3).writeInt(5).write(point).write(tip)
            case MsgIntersectNotFound(tip) =>
                w.writeArrayHeader(2).writeInt(6).write(tip)
            case MsgDone => w.writeArrayHeader(1).writeInt(7)
        }

    given Decoder[ChainSyncMessage] with
        def read(r: Reader): ChainSyncMessage = {
            val arrLen = r.readArrayHeader().toInt
            r.readInt() match {
                case 0 if arrLen == 1 => MsgRequestNext
                case 1 if arrLen == 1 => MsgAwaitReply
                case 2 if arrLen == 3 =>
                    val (era, bytes) = readHardForkEnvelope(r)
                    MsgRollForward(era, bytes, r.read[Tip]())
                case 3 if arrLen == 3 =>
                    MsgRollBackward(r.read[Point](), r.read[Tip]())
                case 4 if arrLen == 2 =>
                    val n = r.readArrayHeader().toInt
                    val buf = List.newBuilder[Point]
                    var i = 0
                    while i < n do {
                        buf += r.read[Point]()
                        i += 1
                    }
                    MsgFindIntersect(buf.result())
                case 5 if arrLen == 3 => MsgIntersectFound(r.read[Point](), r.read[Tip]())
                case 6 if arrLen == 2 => MsgIntersectNotFound(r.read[Tip]())
                case 7 if arrLen == 1 => MsgDone
                case other =>
                    r.validationFailure(s"unexpected chainSyncMessage tag=$other arrLen=$arrLen")
            }
        }

    /** Write the HardForkCombinator envelope: `[era, tag24(bytes)]`. Shared between chain-sync
      * headers and block-fetch bodies.
      */
    private[network] def writeHardForkEnvelope(
        w: Writer,
        era: Int,
        bytes: ByteString
    ): Writer = {
        w.writeArrayHeader(2).writeInt(era).writeTag(Tag.EmbeddedCBOR).writeBytes(bytes.bytes)
    }

    private[network] def readHardForkEnvelope(r: Reader): (Int, ByteString) = {
        val len = r.readArrayHeader().toInt
        if len != 2 then
            r.validationFailure(s"unexpected hard-fork envelope arrLen=$len (expected 2)")
        val era = r.readInt()
        r.readTag() match {
            case Tag.EmbeddedCBOR => ()
            case other            =>
                r.validationFailure(
                  s"expected CBOR tag 24 (EmbeddedCBOR) for hard-fork envelope inner, got $other"
                )
        }
        val bytes = ByteString.fromArray(r.readByteArray())
        (era, bytes)
    }
}

/** Wire-level chain point: either the genesis origin or a (slot, header-hash) pair.
  *
  * Distinct from the domain type [[scalus.cardano.node.stream.ChainPoint]] because the wire
  * representation has an explicit origin case (encoded as `[]`), whereas the domain type uses a
  * sentinel hash. Convert via [[Point.toChainPoint]] / [[Point.fromChainPoint]] at the driver
  * boundary.
  *
  * {{{
  * point = []                               ; origin
  *       / [slot : word64, hash : bstr]     ; block point
  * }}}
  */
sealed trait Point

object Point {

    /** Before any block. Encoded as an empty array. */
    case object Origin extends Point

    /** A specific block on the chain. `hash` is the peer's header-hash identifier (32 bytes for
      * Shelley+).
      */
    final case class BlockPoint(slot: Long, hash: BlockHash) extends Point

    /** Convert to the domain [[ChainPoint]] type; [[Origin]] maps to [[ChainPoint.origin]]. */
    def toChainPoint(p: Point): ChainPoint = p match {
        case Origin              => ChainPoint.origin
        case BlockPoint(slot, h) => ChainPoint(slot, h)
    }

    /** Inverse of [[toChainPoint]]. A domain point that equals [[ChainPoint.origin]] maps back to
      * [[Origin]]; any other point becomes a [[BlockPoint]].
      */
    def fromChainPoint(cp: ChainPoint): Point =
        if cp == ChainPoint.origin then Origin
        else BlockPoint(cp.slot, cp.blockHash)

    given Encoder[Point] with
        def write(w: Writer, p: Point): Writer = p match {
            case Origin => w.writeArrayHeader(0)
            case BlockPoint(slot, hash) =>
                w.writeArrayHeader(2).writeLong(slot).writeBytes(hash.bytes)
        }

    given Decoder[Point] with
        def read(r: Reader): Point = {
            val len = r.readArrayHeader().toInt
            len match {
                case 0 => Origin
                case 2 =>
                    val slot = r.readLong()
                    val hash = BlockHash.fromByteString(ByteString.fromArray(r.readByteArray()))
                    BlockPoint(slot, hash)
                case other =>
                    r.validationFailure(s"unexpected point arrLen=$other (expected 0 or 2)")
            }
        }
}

/** Wire-level chain tip: a [[Point]] plus the peer's current block height.
  *
  * {{{
  * tip = [point, blockNo : word64]
  * }}}
  *
  * Unlike our domain [[scalus.cardano.node.stream.ChainTip]], the wire `tip` at origin uses
  * `blockNo = 0` rather than encoding origin as a special case — the point inside carries the
  * origin distinction.
  */
final case class Tip(point: Point, blockNo: Long)

object Tip {
    val origin: Tip = Tip(Point.Origin, 0L)

    given Encoder[Tip] with
        def write(w: Writer, t: Tip): Writer =
            w.writeArrayHeader(2).write(t.point).writeLong(t.blockNo)

    given Decoder[Tip] with
        def read(r: Reader): Tip = {
            val len = r.readArrayHeader().toInt
            if len != 2 then r.validationFailure(s"unexpected tip arrLen=$len (expected 2)")
            val point = r.read[Point]()
            val blockNo = r.readLong()
            Tip(point, blockNo)
        }
}
