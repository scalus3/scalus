package scalus.cardano.network.handshake

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}
import scalus.cardano.network.NetworkMagic

import scala.collection.immutable.SortedMap

/** Node-to-Node version data per the ouroboros-network CDDL (`node-to-node-version-data-v14.cddl`,
  * `node-to-node-version-data-v16.cddl`).
  *
  * v14 and v15 share the 4-element shape (magic, initiatorOnly, peerSharing, query); v16+ adds
  * `perasSupport` as a trailing fifth element.
  */
sealed trait NodeToNodeVersionData {
    def networkMagic: NetworkMagic
    def initiatorOnlyDiffusionMode: Boolean
    def peerSharing: Int
    def query: Boolean
}

object NodeToNodeVersionData {

    /** Shape used by v14 and v15. */
    final case class V14(
        networkMagic: NetworkMagic,
        initiatorOnlyDiffusionMode: Boolean,
        peerSharing: Int,
        query: Boolean
    ) extends NodeToNodeVersionData {
        require(
          peerSharing == 0 || peerSharing == 1,
          s"peerSharing must be 0 or 1, got $peerSharing"
        )
    }

    /** Shape used by v16 and newer. Adds `perasSupport`. */
    final case class V16(
        networkMagic: NetworkMagic,
        initiatorOnlyDiffusionMode: Boolean,
        peerSharing: Int,
        query: Boolean,
        perasSupport: Boolean
    ) extends NodeToNodeVersionData {
        require(
          peerSharing == 0 || peerSharing == 1,
          s"peerSharing must be 0 or 1, got $peerSharing"
        )
    }
}

/** Versions supported by M4 callers when building proposal tables. */
object VersionNumber {

    /** v14 and v15 use the same data shape, so we group them. */
    val V14: Int = 14
    val V15: Int = 15
    val V16: Int = 16

    /** Which data shape does the given version expect? */
    def shapeFor(version: Int): VersionShape =
        if version == 14 || version == 15 then VersionShape.FourField
        else if version >= 16 then VersionShape.FiveField
        else VersionShape.Unknown
}

enum VersionShape {
    case FourField, FiveField, Unknown
}

/** CBOR map from version number to that version's data shape. Keys are `u32` on the wire; values
  * are per-version arrays. Using [[SortedMap]] so the wire encoding is deterministic for tests and
  * golden vectors.
  */
type VersionTable = SortedMap[Int, NodeToNodeVersionData]

object VersionTable {
    def apply(entries: (Int, NodeToNodeVersionData)*): VersionTable = SortedMap.from(entries)
}

/** Refuse reason, from the CDDL:
  *
  * {{{
  * refuseReasonVersionMismatch      = [0, [*versionNumbers]]
  * refuseReasonHandshakeDecodeError = [1, versionNumbers, tstr]
  * refuseReasonRefused              = [2, versionNumbers, tstr]
  * }}}
  */
sealed trait RefuseReason

object RefuseReason {

    /** Peer does not support any of our proposed versions; carries the peer's own supported list.
      */
    final case class VersionMismatch(peerSupported: List[Int]) extends RefuseReason

    /** Peer tried to decode version data at `version` and the bytes were malformed. */
    final case class HandshakeDecodeError(version: Int, message: String) extends RefuseReason

    /** Peer explicitly refused the connection at `version` — e.g. magic mismatch. */
    final case class Refused(version: Int, message: String) extends RefuseReason
}

/** Top-level handshake message ADT, matching the CDDL:
  *
  * {{{
  * handshakeMessage = msgProposeVersions / msgAcceptVersion / msgRefuse / msgQueryReply
  * msgProposeVersions = [0, versionTable]
  * msgAcceptVersion   = [1, versionNumber, versionData]
  * msgRefuse          = [2, refuseReason]
  * msgQueryReply      = [3, versionTable]
  * }}}
  */
sealed trait HandshakeMessage

object HandshakeMessage {
    final case class MsgProposeVersions(table: VersionTable) extends HandshakeMessage
    final case class MsgAcceptVersion(version: Int, data: NodeToNodeVersionData)
        extends HandshakeMessage
    final case class MsgRefuse(reason: RefuseReason) extends HandshakeMessage
    final case class MsgQueryReply(table: VersionTable) extends HandshakeMessage

    // ----------------------------------------------------------------------------------------
    // CBOR codecs — hand-written because the message is a tag-first variant and because the
    // value shape inside `versionTable` depends on the key.
    // ----------------------------------------------------------------------------------------

    given Encoder[NodeToNodeVersionData] with
        def write(w: Writer, d: NodeToNodeVersionData): Writer = d match {
            case v: NodeToNodeVersionData.V14 =>
                w.writeArrayHeader(4)
                    .writeLong(v.networkMagic.value)
                    .writeBoolean(v.initiatorOnlyDiffusionMode)
                    .writeInt(v.peerSharing)
                    .writeBoolean(v.query)
            case v: NodeToNodeVersionData.V16 =>
                w.writeArrayHeader(5)
                    .writeLong(v.networkMagic.value)
                    .writeBoolean(v.initiatorOnlyDiffusionMode)
                    .writeInt(v.peerSharing)
                    .writeBoolean(v.query)
                    .writeBoolean(v.perasSupport)
        }

    /** Decode depends on the known `version` (the shape is determined by the version number). */
    private def readVersionData(r: Reader, version: Int): NodeToNodeVersionData = {
        val shape = VersionNumber.shapeFor(version)
        val elems = r.readArrayHeader().toInt
        shape match {
            case VersionShape.FourField if elems == 4 =>
                NodeToNodeVersionData.V14(
                  NetworkMagic(r.readLong()),
                  r.readBoolean(),
                  r.readInt(),
                  r.readBoolean()
                )
            case VersionShape.FiveField if elems == 5 =>
                NodeToNodeVersionData.V16(
                  NetworkMagic(r.readLong()),
                  r.readBoolean(),
                  r.readInt(),
                  r.readBoolean(),
                  r.readBoolean()
                )
            case _ =>
                r.validationFailure(
                  s"unexpected versionData shape: version=$version, elems=$elems"
                )
        }
    }

    private def writeVersionTable(w: Writer, table: VersionTable): Writer = {
        w.writeMapHeader(table.size)
        table.foreach { case (version, data) =>
            w.writeLong(version.toLong)
            w.write(data)
        }
        w
    }

    private def readVersionTable(r: Reader): VersionTable = {
        val entries = r.readMapHeader().toInt
        val builder = SortedMap.newBuilder[Int, NodeToNodeVersionData]
        var i = 0
        while i < entries do {
            val version = r.readInt()
            val data = readVersionData(r, version)
            builder += (version -> data)
            i += 1
        }
        builder.result()
    }

    given Encoder[RefuseReason] with
        def write(w: Writer, reason: RefuseReason): Writer = reason match {
            case RefuseReason.VersionMismatch(versions) =>
                w.writeArrayHeader(2).writeInt(0).writeArrayHeader(versions.size)
                versions.foreach(v => w.writeInt(v))
                w
            case RefuseReason.HandshakeDecodeError(version, message) =>
                w.writeArrayHeader(3).writeInt(1).writeInt(version).writeString(message)
            case RefuseReason.Refused(version, message) =>
                w.writeArrayHeader(3).writeInt(2).writeInt(version).writeString(message)
        }

    given Decoder[RefuseReason] with
        def read(r: Reader): RefuseReason = {
            val arrLen = r.readArrayHeader().toInt
            r.readInt() match {
                case 0 if arrLen == 2 =>
                    val n = r.readArrayHeader().toInt
                    val builder = List.newBuilder[Int]
                    var i = 0
                    while i < n do {
                        builder += r.readInt()
                        i += 1
                    }
                    RefuseReason.VersionMismatch(builder.result())
                case 1 if arrLen == 3 =>
                    RefuseReason.HandshakeDecodeError(r.readInt(), r.readString())
                case 2 if arrLen == 3 =>
                    RefuseReason.Refused(r.readInt(), r.readString())
                case other =>
                    r.validationFailure(s"unexpected refuseReason tag=$other arrLen=$arrLen")
            }
        }

    given Encoder[HandshakeMessage] with
        def write(w: Writer, m: HandshakeMessage): Writer = m match {
            case MsgProposeVersions(table) =>
                w.writeArrayHeader(2).writeInt(0)
                writeVersionTable(w, table)
            case MsgAcceptVersion(version, data) =>
                w.writeArrayHeader(3).writeInt(1).writeLong(version.toLong).write(data)
            case MsgRefuse(reason) =>
                w.writeArrayHeader(2).writeInt(2).write(reason)
            case MsgQueryReply(table) =>
                w.writeArrayHeader(2).writeInt(3)
                writeVersionTable(w, table)
        }

    given Decoder[HandshakeMessage] with
        def read(r: Reader): HandshakeMessage = {
            val arrLen = r.readArrayHeader().toInt
            r.readInt() match {
                case 0 if arrLen == 2 => MsgProposeVersions(readVersionTable(r))
                case 1 if arrLen == 3 =>
                    val version = r.readInt()
                    MsgAcceptVersion(version, readVersionData(r, version))
                case 2 if arrLen == 2 => MsgRefuse(summon[Decoder[RefuseReason]].read(r))
                case 3 if arrLen == 2 => MsgQueryReply(readVersionTable(r))
                case other =>
                    r.validationFailure(s"unexpected handshakeMessage tag=$other arrLen=$arrLen")
            }
        }
}
