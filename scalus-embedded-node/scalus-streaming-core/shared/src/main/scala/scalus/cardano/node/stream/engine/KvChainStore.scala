package scalus.cardano.node.stream.engine

import io.bullet.borer.{Cbor, Decoder, Encoder, Reader, Writer}
import scalus.cardano.ledger.{BlockHash, TransactionHash, TransactionInput, TransactionOutput}
import scalus.cardano.node.stream.{ChainPoint, ChainTip}
import scalus.cardano.node.stream.engine.kvstore.KvStore
import scalus.cardano.node.stream.engine.replay.ReplayError
import scalus.uplc.builtin.ByteString

import java.nio.{ByteBuffer, ByteOrder}

/** [[ChainStore]] implementation on top of a narrow [[KvStore]] byte interface. The KV layer owns
  * storage (in-memory, RocksDB, …); this class owns the key layout, the block CBOR codec, and the
  * tip marker. See `docs/local/claude/indexer/chain-store-m9.md` § *Key/value layout* for the
  * on-disk schema and rationale.
  *
  * Thread safety: single-writer contract inherited from [[ChainStore]] — engine worker only.
  */
final class KvChainStore(kv: KvStore) extends ChainStore {

    import KvChainStore.*
    import KvChainStore.given

    def blocksBetween(
        from: ChainPoint,
        to: ChainPoint
    ): Either[ReplayError.ReplaySourceExhausted, Iterator[AppliedBlock]] = {
        if from == to then Right(Iterator.empty)
        else {
            val coversFrom = from == ChainPoint.origin || hasBlockAt(from)
            if !coversFrom then Left(ReplayError.ReplaySourceExhausted(from))
            else {
                // The blocks keyspace is sorted by (slot-BE, hash), so a raw range scan over
                // [blocksPrefix(from.slot + 1), blocksPrefix(to.slot + 1)) already yields blocks
                // strictly after `from` up to and including `to`, in chain order.
                val fromExclusive =
                    if from == ChainPoint.origin then blocksPrefix(0L)
                    else blocksPrefix(from.slot + 1)
                val toInclusiveUpper = blocksPrefix(to.slot + 1)
                val it = kv
                    .rangeScan(fromExclusive, toInclusiveUpper)
                    .map((_, v) => Cbor.decode(v.bytes).to[AppliedBlock].value)
                Right(it)
            }
        }
    }

    def appendBlock(block: AppliedBlock): Unit = {
        val blockBytes = ByteString.unsafeFromArray(Cbor.encode(block).toByteArray)
        val writes = Seq(
          KvStore.Put(blockKey(block.point), blockBytes),
          KvStore.Put(
            indexKey(block.point.slot),
            ByteString.unsafeFromArray(block.point.blockHash.bytes)
          ),
          KvStore.Put(tipKey, encodeTip(block.tip))
        )
        kv.batch(writes)
    }

    def rollbackTo(to: ChainPoint): Unit = {
        // Delete every block / index entry whose slot is strictly greater than `to.slot`.
        val from = blocksPrefix(to.slot + 1)
        val until = blocksPrefix(Long.MaxValue)
        val stale = kv.rangeScan(from, until).map(_._1).toList
        val indexStaleFrom = indexKey(to.slot + 1)
        val indexStaleUntil = indexKey(Long.MaxValue)
        val staleIndex = kv.rangeScan(indexStaleFrom, indexStaleUntil).map(_._1).toList

        // New tip = the stored block with the highest slot ≤ to.slot. Computed by reading the
        // tail of the index keyspace up to (and including) `to.slot`. This is correct on sparse
        // chains too — `to` may name a slot that has no block (common on Cardano), in which case
        // we roll back to the nearest preceding stored block rather than emptying the store.
        val survivingTail = kv.rangeScan(indexKey(0L), indexKey(to.slot + 1)).toList
        val newTip: Seq[KvStore.Write] = survivingTail.lastOption match {
            case None =>
                Seq(KvStore.Delete(tipKey))
            case Some((indexKeyBytes, hashBytes)) =>
                val slot = slotFromIndexKey(indexKeyBytes)
                val blockHash = BlockHash.fromByteString(hashBytes)
                val survivingPoint = ChainPoint(slot, blockHash)
                kv.get(blockKey(survivingPoint)) match {
                    case Some(blockBytes) =>
                        val blk = Cbor.decode(blockBytes.bytes).to[AppliedBlock].value
                        Seq(KvStore.Put(tipKey, encodeTip(blk.tip)))
                    case None =>
                        // index referenced a block whose bytes are absent — a crash during an
                        // earlier write left the two keyspaces inconsistent. Fail loud so the
                        // caller sees the corruption rather than getting a silently-empty tip.
                        throw new KvChainStore.StoreCorruption(
                          s"index entry at slot $slot references missing block"
                        )
                }
        }

        val writes = stale.map(KvStore.Delete.apply)
            ++ staleIndex.map(KvStore.Delete.apply)
            ++ newTip
        if writes.nonEmpty then kv.batch(writes)
    }

    def tip: Option[ChainTip] = kv.get(tipKey).map(decodeTip)

    def close(): Unit = kv.close()

    // ------------------------------------------------------------------
    // Internal helpers.
    // ------------------------------------------------------------------

    private def hasBlockAt(point: ChainPoint): Boolean =
        kv.get(indexKey(point.slot)).exists(bs => BlockHash.fromByteString(bs) == point.blockHash)
}

object KvChainStore {

    /** Raised when the store's index and block keyspaces disagree (an index entry references a
      * block whose bytes are missing). Indicates a crash mid-write corrupted the store; the
      * caller's recovery is to discard the store and rebuild from a snapshot / re-sync.
      */
    final class StoreCorruption(detail: String)
        extends RuntimeException(s"chain store corrupted: $detail")

    // ------------------------------------------------------------------
    // Keyspace layout — see chain-store-m9.md § Key/value layout.
    // ------------------------------------------------------------------

    private val BlocksPrefix: Byte = 0x01
    private val IndexPrefix: Byte = 0x02
    private val TipPrefix: Byte = 0x03

    /** Key for a block at `point` — prefix + slot (big-endian u64) + hash (32 bytes). */
    private[engine] def blockKey(point: ChainPoint): ByteString = {
        val slotBytes = longToBE(point.slot)
        val hashBytes = point.blockHash.bytes
        val buf = new Array[Byte](1 + 8 + hashBytes.length)
        buf(0) = BlocksPrefix
        System.arraycopy(slotBytes, 0, buf, 1, 8)
        System.arraycopy(hashBytes, 0, buf, 9, hashBytes.length)
        ByteString.unsafeFromArray(buf)
    }

    /** Prefix for range-scanning every block at `slot` (one entry per canonical-chain position). */
    private[engine] def blocksPrefix(slot: Long): ByteString = {
        val buf = new Array[Byte](1 + 8)
        buf(0) = BlocksPrefix
        System.arraycopy(longToBE(slot), 0, buf, 1, 8)
        ByteString.unsafeFromArray(buf)
    }

    /** Key for the secondary `slot → hash` index. */
    private[engine] def indexKey(slot: Long): ByteString = {
        val buf = new Array[Byte](1 + 8)
        buf(0) = IndexPrefix
        System.arraycopy(longToBE(slot), 0, buf, 1, 8)
        ByteString.unsafeFromArray(buf)
    }

    /** Inverse of [[indexKey]]: read the slot from a 1-byte prefix + big-endian u64. */
    private[engine] def slotFromIndexKey(key: ByteString): Long = {
        val raw = key.bytes
        require(raw.length == 9 && raw(0) == IndexPrefix, s"not an index key: ${raw.toSeq}")
        ByteBuffer.wrap(raw, 1, 8).order(ByteOrder.BIG_ENDIAN).getLong
    }

    /** Singleton key for the tip marker. */
    private[engine] val tipKey: ByteString = ByteString.unsafeFromArray(Array[Byte](TipPrefix))

    private def longToBE(x: Long): Array[Byte] =
        ByteBuffer.allocate(8).order(ByteOrder.BIG_ENDIAN).putLong(x).array()

    // ------------------------------------------------------------------
    // Tip codec — independent of the AppliedBlock CBOR so the tip marker can be read without a
    // full block decode on warm restart.
    // ------------------------------------------------------------------

    private[engine] def encodeTip(t: ChainTip): ByteString = {
        val hashBytes = t.point.blockHash.bytes
        val buf = new Array[Byte](8 + hashBytes.length + 8)
        System.arraycopy(longToBE(t.point.slot), 0, buf, 0, 8)
        System.arraycopy(hashBytes, 0, buf, 8, hashBytes.length)
        System.arraycopy(longToBE(t.blockNo), 0, buf, 8 + hashBytes.length, 8)
        ByteString.unsafeFromArray(buf)
    }

    private[engine] def decodeTip(bs: ByteString): ChainTip = {
        val raw = bs.bytes
        require(raw.length == 8 + 32 + 8, s"tip record length=${raw.length}, expected 48")
        val bb = ByteBuffer.wrap(raw).order(ByteOrder.BIG_ENDIAN)
        val slot = bb.getLong(0)
        val hashBytes = new Array[Byte](32)
        System.arraycopy(raw, 8, hashBytes, 0, 32)
        val blockNo = bb.getLong(8 + 32)
        ChainTip(
          ChainPoint(slot, BlockHash.fromByteString(ByteString.unsafeFromArray(hashBytes))),
          blockNo
        )
    }

    // ------------------------------------------------------------------
    // CBOR codecs for AppliedBlock / AppliedTransaction. Hand-written to match the style used in
    // `PersistenceCodecs` and to avoid auto-derivation pulling in scalacheck-level machinery.
    // ------------------------------------------------------------------

    private[engine] given Encoder[AppliedTransaction] with
        def write(w: Writer, tx: AppliedTransaction): Writer = {
            w.writeArrayHeader(3).write(tx.id).writeArrayHeader(tx.inputs.size)
            tx.inputs.foreach(i => w.write(i))
            w.writeArrayHeader(tx.outputs.size)
            tx.outputs.foreach(o => w.write(o))
            w
        }

    private[engine] given Decoder[AppliedTransaction] with
        def read(r: Reader): AppliedTransaction = {
            val len = r.readArrayHeader().toInt
            if len != 3 then r.validationFailure(s"AppliedTransaction arrLen=$len (expected 3)")
            val id = r.read[TransactionHash]()
            val nIn = r.readArrayHeader().toInt
            val inB = Set.newBuilder[TransactionInput]
            var i = 0
            while i < nIn do { inB += r.read[TransactionInput](); i += 1 }
            val nOut = r.readArrayHeader().toInt
            val outB = IndexedSeq.newBuilder[TransactionOutput]
            i = 0
            while i < nOut do { outB += r.read[TransactionOutput](); i += 1 }
            AppliedTransaction(id, inB.result(), outB.result())
        }

    // ChainTip codec is shared with [[engine.persistence.PersistenceCodecs]] so the two stores
    // cannot drift on the wire format. ChainTip's codec resolves its ChainPoint dependency at
    // its own definition site — we don't need to re-import ChainPoint's codec here.
    import scalus.cardano.node.stream.engine.persistence.PersistenceCodecs.{given Decoder[ChainTip], given Encoder[ChainTip]}

    private[engine] given Encoder[AppliedBlock] with
        def write(w: Writer, b: AppliedBlock): Writer = {
            w.writeArrayHeader(2).write(b.tip).writeArrayHeader(b.transactions.size)
            b.transactions.foreach(tx => w.write(tx))
            w
        }

    private[engine] given Decoder[AppliedBlock] with
        def read(r: Reader): AppliedBlock = {
            val len = r.readArrayHeader().toInt
            if len != 2 then r.validationFailure(s"AppliedBlock arrLen=$len (expected 2)")
            val tip = r.read[ChainTip]()
            val n = r.readArrayHeader().toInt
            val txs = IndexedSeq.newBuilder[AppliedTransaction]
            var i = 0
            while i < n do { txs += r.read[AppliedTransaction](); i += 1 }
            AppliedBlock(tip, txs.result())
        }
}
