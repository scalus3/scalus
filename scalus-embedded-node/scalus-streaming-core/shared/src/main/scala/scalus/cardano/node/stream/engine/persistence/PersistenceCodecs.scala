package scalus.cardano.node.stream.engine.persistence

import io.bullet.borer.{Cbor, Decoder, Encoder, Reader, Writer}
import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, BlockHash, PolicyId, TransactionHash, TransactionInput, TransactionOutput}
import scalus.cardano.node.stream.{ChainPoint, ChainTip}
import scalus.cardano.node.stream.engine.{Bucket, UtxoKey}
import scalus.uplc.builtin.ByteString

import scala.collection.mutable

/** CBOR codecs for the persistence types. Hand-written to match the style used elsewhere in the
  * module ([[scalus.cardano.network.chainsync.ChainSyncMessage]]) and to avoid dragging in the
  * derivation machinery for types that already have their own codecs.
  *
  * Notable absences: `ProtocolParams` is not persisted in M6 because the engine never mutates
  * `paramsRef` from the worker in this milestone. The cell resets to `cardanoInfo.protocolParams`
  * on warm restart, which matches live behaviour. See the note in [[JournalRecord]] and M12b for
  * the advanced-persistence follow-up.
  */
object PersistenceCodecs {

    // ------------------------------------------------------------------
    // Primitives not covered by scalus-core codecs.
    // ------------------------------------------------------------------

    given Encoder[ChainPoint] with
        def write(w: Writer, p: ChainPoint): Writer =
            w.writeArrayHeader(2).writeLong(p.slot).writeBytes(p.blockHash.bytes)

    given Decoder[ChainPoint] with
        def read(r: Reader): ChainPoint = {
            val len = r.readArrayHeader().toInt
            if len != 2 then r.validationFailure(s"ChainPoint arrLen=$len (expected 2)")
            val slot = r.readLong()
            val hash = BlockHash.fromByteString(ByteString.fromArray(r.readByteArray()))
            ChainPoint(slot, hash)
        }

    given Encoder[ChainTip] with
        def write(w: Writer, t: ChainTip): Writer =
            w.writeArrayHeader(2).write(t.point).writeLong(t.blockNo)

    given Decoder[ChainTip] with
        def read(r: Reader): ChainTip = {
            val len = r.readArrayHeader().toInt
            if len != 2 then r.validationFailure(s"ChainTip arrLen=$len (expected 2)")
            val point = r.read[ChainPoint]()
            val blockNo = r.readLong()
            ChainTip(point, blockNo)
        }

    // ------------------------------------------------------------------
    // UtxoKey — closed ADT with four concrete variants.
    // Tag map: 0 → Addr, 1 → Asset, 2 → TxOuts, 3 → Inputs.
    // ------------------------------------------------------------------

    given Encoder[UtxoKey] with
        def write(w: Writer, k: UtxoKey): Writer = k match {
            case UtxoKey.Addr(addr) =>
                w.writeArrayHeader(2).writeInt(0).write(addr)
            case UtxoKey.Asset(policyId, assetName) =>
                w.writeArrayHeader(3).writeInt(1).write(policyId).write(assetName)
            case UtxoKey.TxOuts(txId) =>
                w.writeArrayHeader(2).writeInt(2).write(txId)
            case UtxoKey.Inputs(inputs) =>
                w.writeArrayHeader(2).writeInt(3).writeArrayHeader(inputs.size)
                inputs.foreach(i => w.write(i))
                w
        }

    given Decoder[UtxoKey] with
        def read(r: Reader): UtxoKey = {
            val arrLen = r.readArrayHeader().toInt
            r.readInt() match {
                case 0 if arrLen == 2 => UtxoKey.Addr(r.read[Address]())
                case 1 if arrLen == 3 => UtxoKey.Asset(r.read[PolicyId](), r.read[AssetName]())
                case 2 if arrLen == 2 => UtxoKey.TxOuts(r.read[TransactionHash]())
                case 3 if arrLen == 2 =>
                    val n = r.readArrayHeader().toInt
                    val b = Set.newBuilder[TransactionInput]
                    var i = 0
                    while i < n do { b += r.read[TransactionInput](); i += 1 }
                    UtxoKey.Inputs(b.result())
                case other =>
                    r.validationFailure(s"unexpected UtxoKey tag=$other arrLen=$arrLen")
            }
        }

    // ------------------------------------------------------------------
    // Bucket records.
    // ------------------------------------------------------------------

    given Encoder[Bucket.CreatedRec] with
        def write(w: Writer, rec: Bucket.CreatedRec): Writer =
            w.writeArrayHeader(3).write(rec.input).write(rec.output).write(rec.producedBy)

    given Decoder[Bucket.CreatedRec] with
        def read(r: Reader): Bucket.CreatedRec = {
            val len = r.readArrayHeader().toInt
            if len != 3 then r.validationFailure(s"CreatedRec arrLen=$len (expected 3)")
            Bucket.CreatedRec(
              r.read[TransactionInput](),
              r.read[TransactionOutput](),
              r.read[TransactionHash]()
            )
        }

    given Encoder[Bucket.SpentRec] with
        def write(w: Writer, rec: Bucket.SpentRec): Writer =
            w.writeArrayHeader(3).write(rec.input).write(rec.output).write(rec.spentBy)

    given Decoder[Bucket.SpentRec] with
        def read(r: Reader): Bucket.SpentRec = {
            val len = r.readArrayHeader().toInt
            if len != 3 then r.validationFailure(s"SpentRec arrLen=$len (expected 3)")
            Bucket.SpentRec(
              r.read[TransactionInput](),
              r.read[TransactionOutput](),
              r.read[TransactionHash]()
            )
        }

    given Encoder[BucketDelta] with
        def write(w: Writer, d: BucketDelta): Writer = {
            w.writeArrayHeader(2).writeArrayHeader(d.added.size)
            d.added.foreach(w.write)
            w.writeArrayHeader(d.removed.size)
            d.removed.foreach(w.write)
            w
        }

    given Decoder[BucketDelta] with
        def read(r: Reader): BucketDelta = {
            val len = r.readArrayHeader().toInt
            if len != 2 then r.validationFailure(s"BucketDelta arrLen=$len (expected 2)")
            val na = r.readArrayHeader().toInt
            val addedB = Seq.newBuilder[Bucket.CreatedRec]
            var i = 0
            while i < na do { addedB += r.read[Bucket.CreatedRec](); i += 1 }
            val nr = r.readArrayHeader().toInt
            val remB = Seq.newBuilder[Bucket.SpentRec]
            i = 0
            while i < nr do { remB += r.read[Bucket.SpentRec](); i += 1 }
            BucketDelta(addedB.result(), remB.result())
        }

    given Encoder[BucketState] with
        def write(w: Writer, s: BucketState): Writer = {
            w.writeArrayHeader(2).write(s.key).writeArrayHeader(s.current.size)
            s.current.foreach { (in, out) =>
                w.writeArrayHeader(2).write(in).write(out)
            }
            w
        }

    given Decoder[BucketState] with
        def read(r: Reader): BucketState = {
            val len = r.readArrayHeader().toInt
            if len != 2 then r.validationFailure(s"BucketState arrLen=$len (expected 2)")
            val key = r.read[UtxoKey]()
            val n = r.readArrayHeader().toInt
            val m = mutable.Map.empty[TransactionInput, TransactionOutput]
            var i = 0
            while i < n do {
                val pairLen = r.readArrayHeader().toInt
                if pairLen != 2 then r.validationFailure(s"BucketState pair arrLen=$pairLen")
                val in = r.read[TransactionInput]()
                val out = r.read[TransactionOutput]()
                m.update(in, out)
                i += 1
            }
            BucketState(key, m.toMap)
        }

    // ------------------------------------------------------------------
    // AppliedBlockSummary + small helpers for Map[UtxoKey, *].
    // ------------------------------------------------------------------

    private def writeDeltaMap(w: Writer, m: Map[UtxoKey, BucketDelta]): Writer = {
        w.writeArrayHeader(m.size)
        m.foreach { (k, v) => w.writeArrayHeader(2).write(k).write(v) }
        w
    }

    private def readDeltaMap(r: Reader): Map[UtxoKey, BucketDelta] = {
        val n = r.readArrayHeader().toInt
        val b = Map.newBuilder[UtxoKey, BucketDelta]
        var i = 0
        while i < n do {
            val pairLen = r.readArrayHeader().toInt
            if pairLen != 2 then r.validationFailure(s"delta-map pair arrLen=$pairLen")
            b += (r.read[UtxoKey]() -> r.read[BucketDelta]())
            i += 1
        }
        b.result()
    }

    given Encoder[AppliedBlockSummary] with
        def write(w: Writer, s: AppliedBlockSummary): Writer = {
            w.writeArrayHeader(3).write(s.tip).writeArrayHeader(s.txIds.size)
            s.txIds.foreach(h => w.write(h))
            writeDeltaMap(w, s.bucketDeltas)
        }

    given Decoder[AppliedBlockSummary] with
        def read(r: Reader): AppliedBlockSummary = {
            val len = r.readArrayHeader().toInt
            if len != 3 then r.validationFailure(s"AppliedBlockSummary arrLen=$len (expected 3)")
            val tip = r.read[ChainTip]()
            val n = r.readArrayHeader().toInt
            val idsB = Set.newBuilder[TransactionHash]
            var i = 0
            while i < n do { idsB += r.read[TransactionHash](); i += 1 }
            AppliedBlockSummary(tip, idsB.result(), readDeltaMap(r))
        }

    // ------------------------------------------------------------------
    // JournalRecord.
    // Tag map: 0 → Forward, 1 → Backward, 2 → OwnSubmitted, 3 → OwnForgotten.
    // Tag 4 is reserved for the future ParamsChanged variant (see note in JournalRecord.scala).
    // ------------------------------------------------------------------

    given Encoder[JournalRecord] with
        def write(w: Writer, rec: JournalRecord): Writer = rec match {
            case JournalRecord.Forward(tip, ids, deltas) =>
                w.writeArrayHeader(4).writeInt(0).write(tip).writeArrayHeader(ids.size)
                ids.foreach(h => w.write(h))
                writeDeltaMap(w, deltas)
            case JournalRecord.Backward(to) =>
                w.writeArrayHeader(2).writeInt(1).write(to)
            case JournalRecord.OwnSubmitted(h) =>
                w.writeArrayHeader(2).writeInt(2).write(h)
            case JournalRecord.OwnForgotten(h) =>
                w.writeArrayHeader(2).writeInt(3).write(h)
        }

    given Decoder[JournalRecord] with
        def read(r: Reader): JournalRecord = {
            val arrLen = r.readArrayHeader().toInt
            r.readInt() match {
                case 0 if arrLen == 4 =>
                    val tip = r.read[ChainTip]()
                    val n = r.readArrayHeader().toInt
                    val idsB = Set.newBuilder[TransactionHash]
                    var i = 0
                    while i < n do { idsB += r.read[TransactionHash](); i += 1 }
                    JournalRecord.Forward(tip, idsB.result(), readDeltaMap(r))
                case 1 if arrLen == 2 => JournalRecord.Backward(r.read[ChainPoint]())
                case 2 if arrLen == 2 => JournalRecord.OwnSubmitted(r.read[TransactionHash]())
                case 3 if arrLen == 2 => JournalRecord.OwnForgotten(r.read[TransactionHash]())
                case other =>
                    r.validationFailure(s"unexpected JournalRecord tag=$other arrLen=$arrLen")
            }
        }

    // ------------------------------------------------------------------
    // EngineSnapshotFile — top-level snapshot envelope.
    // ------------------------------------------------------------------

    given Encoder[EngineSnapshotFile] with
        def write(w: Writer, s: EngineSnapshotFile): Writer = {
            w.writeArrayHeader(7)
                .writeInt(s.schemaVersion)
                .writeString(s.appId)
                .writeLong(s.networkMagic)
            s.tip match {
                case Some(t) => w.writeArrayHeader(1).write(t)
                case None    => w.writeArrayHeader(0)
            }
            w.writeArrayHeader(s.ownSubmissions.size)
            s.ownSubmissions.foreach(h => w.write(h))
            w.writeArrayHeader(s.volatileTail.size)
            s.volatileTail.foreach(b => w.write(b))
            w.writeArrayHeader(s.buckets.size)
            s.buckets.foreach { (k, v) =>
                w.writeArrayHeader(2).write(k).write(v)
            }
            w
        }

    given Decoder[EngineSnapshotFile] with
        def read(r: Reader): EngineSnapshotFile = {
            val len = r.readArrayHeader().toInt
            if len != 7 then r.validationFailure(s"EngineSnapshotFile arrLen=$len (expected 7)")
            val schemaVersion = r.readInt()
            val appId = r.readString()
            val networkMagic = r.readLong()
            val tipLen = r.readArrayHeader().toInt
            val tip = tipLen match {
                case 0 => None
                case 1 => Some(r.read[ChainTip]())
                case n => r.validationFailure(s"tip option arrLen=$n (expected 0 or 1)")
            }
            val subsN = r.readArrayHeader().toInt
            val subsB = Set.newBuilder[TransactionHash]
            var i = 0
            while i < subsN do { subsB += r.read[TransactionHash](); i += 1 }
            val tailN = r.readArrayHeader().toInt
            val tailB = Seq.newBuilder[AppliedBlockSummary]
            i = 0
            while i < tailN do { tailB += r.read[AppliedBlockSummary](); i += 1 }
            val bucketsN = r.readArrayHeader().toInt
            val bucketsB = Map.newBuilder[UtxoKey, BucketState]
            i = 0
            while i < bucketsN do {
                val pairLen = r.readArrayHeader().toInt
                if pairLen != 2 then r.validationFailure(s"buckets pair arrLen=$pairLen")
                bucketsB += (r.read[UtxoKey]() -> r.read[BucketState]())
                i += 1
            }
            EngineSnapshotFile(
              schemaVersion,
              appId,
              networkMagic,
              tip,
              subsB.result(),
              tailB.result(),
              bucketsB.result()
            )
        }

    // ------------------------------------------------------------------
    // Public helpers — callers typically go through these rather than
    // constructing the low-level `Cbor.encode` / `Cbor.decode` themselves.
    // ------------------------------------------------------------------

    def encodeRecord(rec: JournalRecord): Array[Byte] =
        Cbor.encode(rec).toByteArray

    def decodeRecord(bytes: Array[Byte]): JournalRecord =
        Cbor.decode(bytes).to[JournalRecord].value

    def encodeSnapshot(snap: EngineSnapshotFile): Array[Byte] =
        Cbor.encode(snap).toByteArray

    def decodeSnapshot(bytes: Array[Byte]): EngineSnapshotFile =
        Cbor.decode(bytes).to[EngineSnapshotFile].value
}
