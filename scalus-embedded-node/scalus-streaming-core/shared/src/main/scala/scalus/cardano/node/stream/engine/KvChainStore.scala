package scalus.cardano.node.stream.engine

import io.bullet.borer.{Cbor, Decoder, Encoder, Reader, Writer}
import scalus.cardano.ledger.{BlockHash, TransactionHash, TransactionInput, TransactionOutput, Utxos}
import scalus.cardano.node.UtxoQuery
import scalus.cardano.node.stream.{ChainPoint, ChainTip}
import scalus.cardano.node.stream.engine.kvstore.KvStore
import scalus.cardano.node.stream.engine.replay.ReplayError
import scalus.uplc.builtin.ByteString

import java.nio.{ByteBuffer, ByteOrder}
import scala.collection.mutable

/** [[ChainStore]] implementation on top of a narrow [[KvStore]] byte interface. The KV layer owns
  * storage (in-memory, RocksDB, …); this class owns the key layout, the block CBOR codec, and the
  * tip marker. See `docs/local/claude/indexer/chain-store-m9.md` § *Key/value layout* for the
  * on-disk schema and rationale.
  *
  * Thread safety: single-writer contract inherited from [[ChainStore]] — engine worker only.
  */
final class KvChainStore(kv: KvStore) extends ChainStore with ChainStoreUtxoSet {

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
        val writes = mutable.ArrayBuffer.empty[KvStore.Write]
        writes += KvStore.Put(blockKey(block.point), blockBytes)
        writes += KvStore.Put(
          indexKey(block.point.slot),
          ByteString.unsafeFromArray(block.point.blockHash.bytes)
        )
        writes += KvStore.Put(tipKey, encodeTip(block.tip))

        // UTxO-set maintenance: compute deltas, record a reverse-delta for rollback, and update
        // the primary utxo + utxo-by-key keyspaces. A spend of an input we've never seen (common
        // when the ChainStore didn't start from origin) is silently dropped — rollback can't
        // restore an output we never knew about.
        val addedBuffer = mutable.ArrayBuffer.empty[(TransactionInput, TransactionOutput)]
        val removedBuffer = mutable.ArrayBuffer.empty[(TransactionInput, TransactionOutput)]

        block.transactions.foreach { tx =>
            tx.inputs.foreach { input =>
                kv.get(utxoKey(input)).foreach { outBytes =>
                    val output = Cbor.decode(outBytes.bytes).to[TransactionOutput].value
                    removedBuffer += (input -> output)
                    writes += KvStore.Delete(utxoKey(input))
                    utxoKeysOf(input, output).foreach { k =>
                        writes += KvStore.Delete(utxoByKeyKey(k, input))
                    }
                }
            }
            tx.outputs.iterator.zipWithIndex.foreach { (output, idx) =>
                val input = TransactionInput(tx.id, idx)
                val outBytes = ByteString.unsafeFromArray(Cbor.encode(output).toByteArray)
                addedBuffer += (input -> output)
                writes += KvStore.Put(utxoKey(input), outBytes)
                utxoKeysOf(input, output).foreach { k =>
                    writes += KvStore.Put(utxoByKeyKey(k, input), UtxoByKeySentinel)
                }
            }
        }

        val reverseDelta = ReverseDelta(addedBuffer.toVector, removedBuffer.toVector)
        val deltaBytes = ByteString.unsafeFromArray(Cbor.encode(reverseDelta).toByteArray)
        writes += KvStore.Put(deltaKey(block.point), deltaBytes)

        kv.batch(writes.toSeq)
    }

    def rollbackTo(to: ChainPoint): Unit = {
        // Delete every block / index / delta entry whose slot is strictly greater than `to.slot`.
        val staleBlockKeys =
            kv.rangeScan(blocksPrefix(to.slot + 1), blocksPrefix(Long.MaxValue)).map(_._1).toList
        val staleIndexKeys =
            kv.rangeScan(indexKey(to.slot + 1), indexKey(Long.MaxValue)).map(_._1).toList

        // Pull every reverse-delta whose slot > to.slot and invert them into UTxO writes.
        val staleDeltas =
            kv.rangeScan(deltaPrefix(to.slot + 1), deltaPrefix(Long.MaxValue)).toList
        val writes = mutable.ArrayBuffer.empty[KvStore.Write]

        // Invert deltas in reverse order (newest block first) so that adjacent same-input
        // creations / spends across blocks unwind in the correct nesting.
        staleDeltas.reverse.foreach { (deltaK, deltaV) =>
            val rev = Cbor.decode(deltaV.bytes).to[ReverseDelta].value
            // Undo created outputs: delete them from utxo + utxo-by-key.
            rev.added.foreach { (input, output) =>
                writes += KvStore.Delete(utxoKey(input))
                utxoKeysOf(input, output).foreach { k =>
                    writes += KvStore.Delete(utxoByKeyKey(k, input))
                }
            }
            // Re-add previously-spent outputs: restore them to utxo + utxo-by-key.
            rev.removed.foreach { (input, output) =>
                val outBytes = ByteString.unsafeFromArray(Cbor.encode(output).toByteArray)
                writes += KvStore.Put(utxoKey(input), outBytes)
                utxoKeysOf(input, output).foreach { k =>
                    writes += KvStore.Put(utxoByKeyKey(k, input), UtxoByKeySentinel)
                }
            }
            writes += KvStore.Delete(deltaK)
        }

        // New tip = the stored block with the highest slot ≤ to.slot. Computed by reading the
        // tail of the index keyspace up to (and including) `to.slot`. This is correct on sparse
        // chains too — `to` may name a slot that has no block (common on Cardano), in which case
        // we roll back to the nearest preceding stored block rather than emptying the store.
        val survivingTail = kv.rangeScan(indexKey(0L), indexKey(to.slot + 1)).toList
        survivingTail.lastOption match {
            case None =>
                writes += KvStore.Delete(tipKey)
            case Some((indexKeyBytes, hashBytes)) =>
                val slot = slotFromIndexKey(indexKeyBytes)
                val blockHash = BlockHash.fromByteString(hashBytes)
                val survivingPoint = ChainPoint(slot, blockHash)
                kv.get(blockKey(survivingPoint)) match {
                    case Some(blockBytes) =>
                        val blk = Cbor.decode(blockBytes.bytes).to[AppliedBlock].value
                        writes += KvStore.Put(tipKey, encodeTip(blk.tip))
                    case None =>
                        throw new KvChainStore.StoreCorruption(
                          s"index entry at slot $slot references missing block"
                        )
                }
        }

        writes ++= staleBlockKeys.map(KvStore.Delete.apply)
        writes ++= staleIndexKeys.map(KvStore.Delete.apply)

        if writes.nonEmpty then kv.batch(writes.toSeq)
    }

    // ------------------------------------------------------------------
    // ChainStoreUtxoSet
    // ------------------------------------------------------------------

    def findUtxosFromStore(q: UtxoQuery): Option[Utxos] = {
        val keys = QueryDecomposer.keys(q)
        if keys.isEmpty then return Some(Map.empty)

        val result = mutable.Map.empty[TransactionInput, TransactionOutput]
        keys.foreach {
            case UtxoKey.Inputs(inputs) =>
                inputs.foreach { in =>
                    kv.get(utxoKey(in)).foreach { outBytes =>
                        val out = Cbor.decode(outBytes.bytes).to[TransactionOutput].value
                        result.update(in, out)
                    }
                }
            case key =>
                // Secondary-index range scan: [utxoByKeyPrefix(key), utxoByKeyPrefix(key).next).
                val prefix = utxoByKeyPrefix(key)
                val upper = bumpPrefix(prefix)
                kv.rangeScan(prefix, upper).foreach { (entryKey, _) =>
                    val input = inputFromUtxoByKey(entryKey)
                    kv.get(utxoKey(input)).foreach { outBytes =>
                        val out = Cbor.decode(outBytes.bytes).to[TransactionOutput].value
                        result.update(input, out)
                    }
                }
        }

        // Secondary indexes may over-approximate (e.g. an output indexed under Addr was spent
        // but a prior delta didn't fully clean up). Post-filter by the full query predicate.
        Some(result.iterator.filter((i, o) => QueryDecomposer.matches(q, i, o)).toMap)
    }

    def restoreUtxoSet(
        tip: ChainTip,
        utxos: Iterator[(TransactionInput, TransactionOutput)]
    ): Unit = {
        // Clear the existing UTxO + index keyspaces, then stream the new set in batches.
        val oldPrimary = kv
            .rangeScan(Array(UtxoPrefix).asBs, Array((UtxoPrefix + 1).toByte).asBs)
            .map(_._1)
            .toList
        val oldIndex = kv
            .rangeScan(
              Array(UtxoByKeyPrefix).asBs,
              Array((UtxoByKeyPrefix + 1).toByte).asBs
            )
            .map(_._1)
            .toList
        val clears: Seq[KvStore.Write] =
            oldPrimary.map(KvStore.Delete.apply) ++ oldIndex.map(KvStore.Delete.apply)

        val RestoreBatchSize = 1024
        val buf = mutable.ArrayBuffer.empty[KvStore.Write]
        buf ++= clears
        utxos.foreach { (input, output) =>
            val outBytes = ByteString.unsafeFromArray(Cbor.encode(output).toByteArray)
            buf += KvStore.Put(utxoKey(input), outBytes)
            utxoKeysOf(input, output).foreach { k =>
                buf += KvStore.Put(utxoByKeyKey(k, input), UtxoByKeySentinel)
            }
            if buf.size >= RestoreBatchSize then {
                kv.batch(buf.toSeq)
                buf.clear()
            }
        }
        buf += KvStore.Put(tipKey, encodeTip(tip))
        kv.batch(buf.toSeq)
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
    // Keyspace layout — see chain-store-m9.md + snapshot-bootstrap-m10.md
    // for the full schema. Prefix bytes are chosen distinct so range-scans
    // by leading byte cleanly partition the store.
    // ------------------------------------------------------------------

    private val BlocksPrefix: Byte = 0x01
    private val IndexPrefix: Byte = 0x02
    private val TipPrefix: Byte = 0x03
    private val UtxoPrefix: Byte = 0x04
    private val UtxoByKeyPrefix: Byte = 0x05
    private val DeltaPrefix: Byte = 0x06

    /** Sentinel value for utxo-by-key entries. The entries carry their meaning in the key; the
      * value is unused. Using a distinctive byte (0x01) makes accidental raw reads obviously
      * non-empty without costing storage.
      */
    private[engine] val UtxoByKeySentinel: ByteString =
        ByteString.unsafeFromArray(Array[Byte](0x01))

    extension (a: Array[Byte]) private[engine] def asBs: ByteString = ByteString.unsafeFromArray(a)

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

    /** Key for a UTxO entry — prefix + CBOR-encoded TransactionInput. */
    private[engine] def utxoKey(input: TransactionInput): ByteString =
        prefixedCbor(UtxoPrefix, Cbor.encode(input).toByteArray)

    /** Key for the secondary UTxO-by-key index:
      * `<prefix=UtxoByKeyPrefix> <u32-BE len-ukey-cbor> <ukey-cbor> <input-cbor>`.
      *
      * The length-prefix on the ukey-cbor makes prefix range-scans unambiguous: two different
      * UtxoKeys can share a CBOR prefix without the scan over one picking up entries for the other.
      */
    private[engine] def utxoByKeyKey(key: UtxoKey, input: TransactionInput): ByteString = {
        val ukey = {
            import scalus.cardano.node.stream.engine.persistence.PersistenceCodecs.given
            Cbor.encode(key).toByteArray
        }
        val inputBytes = Cbor.encode(input).toByteArray
        val buf = new Array[Byte](1 + 4 + ukey.length + inputBytes.length)
        buf(0) = UtxoByKeyPrefix
        ByteBuffer.wrap(buf, 1, 4).order(ByteOrder.BIG_ENDIAN).putInt(ukey.length)
        System.arraycopy(ukey, 0, buf, 5, ukey.length)
        System.arraycopy(inputBytes, 0, buf, 5 + ukey.length, inputBytes.length)
        ByteString.unsafeFromArray(buf)
    }

    /** Range-scan prefix for all inputs indexed under `key`. */
    private[engine] def utxoByKeyPrefix(key: UtxoKey): ByteString = {
        val ukey = {
            import scalus.cardano.node.stream.engine.persistence.PersistenceCodecs.given
            Cbor.encode(key).toByteArray
        }
        val buf = new Array[Byte](1 + 4 + ukey.length)
        buf(0) = UtxoByKeyPrefix
        ByteBuffer.wrap(buf, 1, 4).order(ByteOrder.BIG_ENDIAN).putInt(ukey.length)
        System.arraycopy(ukey, 0, buf, 5, ukey.length)
        ByteString.unsafeFromArray(buf)
    }

    /** Extract the [[TransactionInput]] from an `utxo-by-key` composite key. */
    private[engine] def inputFromUtxoByKey(key: ByteString): TransactionInput = {
        val raw = key.bytes
        require(raw.length > 5 && raw(0) == UtxoByKeyPrefix, "not a utxo-by-key key")
        val ukeyLen = ByteBuffer.wrap(raw, 1, 4).order(ByteOrder.BIG_ENDIAN).getInt
        val inputStart = 5 + ukeyLen
        val inputBytes = java.util.Arrays.copyOfRange(raw, inputStart, raw.length)
        Cbor.decode(inputBytes).to[TransactionInput].value
    }

    /** Key for a per-block reverse-delta entry. Same `(slot, hash)` shape as `blockKey` so rollback
      * scans are trivially parallel.
      */
    private[engine] def deltaKey(point: ChainPoint): ByteString = {
        val hashBytes = point.blockHash.bytes
        val buf = new Array[Byte](1 + 8 + hashBytes.length)
        buf(0) = DeltaPrefix
        System.arraycopy(longToBE(point.slot), 0, buf, 1, 8)
        System.arraycopy(hashBytes, 0, buf, 9, hashBytes.length)
        ByteString.unsafeFromArray(buf)
    }

    private[engine] def deltaPrefix(slot: Long): ByteString = {
        val buf = new Array[Byte](1 + 8)
        buf(0) = DeltaPrefix
        System.arraycopy(longToBE(slot), 0, buf, 1, 8)
        ByteString.unsafeFromArray(buf)
    }

    /** Return the smallest ByteString strictly greater than `bs` — used as an exclusive upper bound
      * for prefix range-scans. Increments the last byte with carry; if the entire string is 0xff,
      * appends a 0x00 (which is greater since it's longer).
      */
    private[engine] def bumpPrefix(bs: ByteString): ByteString = {
        val raw = bs.bytes
        val buf = java.util.Arrays.copyOf(raw, raw.length)
        var i = buf.length - 1
        while i >= 0 do {
            val v = buf(i) & 0xff
            if v < 0xff then {
                buf(i) = (v + 1).toByte
                return ByteString.unsafeFromArray(buf)
            } else {
                buf(i) = 0
                i -= 1
            }
        }
        // All bytes were 0xff — append a zero byte (longer ⇒ greater in unsigned-lex).
        ByteString.unsafeFromArray(buf ++ Array[Byte](0))
    }

    /** The set of [[UtxoKey]]s an `(input, output)` pair should be indexed under. `UtxoKey.Inputs`
      * is query-only (not an index key) so it's absent from the return set.
      */
    private[engine] def utxoKeysOf(
        input: TransactionInput,
        output: TransactionOutput
    ): Seq[UtxoKey] = {
        val buf = mutable.ArrayBuffer.empty[UtxoKey]
        buf += UtxoKey.Addr(output.address)
        output.value.assets.assets.foreach { (policy, bundle) =>
            bundle.foreach { (name, _) =>
                buf += UtxoKey.Asset(policy, name)
            }
        }
        buf += UtxoKey.TxOuts(input.transactionId)
        buf.toSeq
    }

    /** Per-block reverse-delta: the outputs this block created (to undo on rollback) plus the
      * previously-existing outputs it spent (to restore on rollback).
      */
    private[engine] final case class ReverseDelta(
        added: Vector[(TransactionInput, TransactionOutput)],
        removed: Vector[(TransactionInput, TransactionOutput)]
    )

    private[engine] given Encoder[ReverseDelta] with
        def write(w: Writer, d: ReverseDelta): Writer = {
            w.writeArrayHeader(2).writeArrayHeader(d.added.size)
            d.added.foreach { (i, o) => w.writeArrayHeader(2).write(i).write(o) }
            w.writeArrayHeader(d.removed.size)
            d.removed.foreach { (i, o) => w.writeArrayHeader(2).write(i).write(o) }
            w
        }

    private[engine] given Decoder[ReverseDelta] with
        def read(r: Reader): ReverseDelta = {
            val len = r.readArrayHeader().toInt
            if len != 2 then r.validationFailure(s"ReverseDelta arrLen=$len (expected 2)")
            def readPairs(): Vector[(TransactionInput, TransactionOutput)] = {
                val n = r.readArrayHeader().toInt
                val b = Vector.newBuilder[(TransactionInput, TransactionOutput)]
                var i = 0
                while i < n do {
                    val pairLen = r.readArrayHeader().toInt
                    if pairLen != 2 then r.validationFailure(s"ReverseDelta pair arrLen=$pairLen")
                    b += (r.read[TransactionInput]() -> r.read[TransactionOutput]())
                    i += 1
                }
                b.result()
            }
            ReverseDelta(readPairs(), readPairs())
        }

    /** Build `<prefix-byte><cbor-bytes>` without a length prefix — the caller knows the key is a
      * single CBOR value.
      */
    private def prefixedCbor(prefix: Byte, cbor: Array[Byte]): ByteString = {
        val buf = new Array[Byte](1 + cbor.length)
        buf(0) = prefix
        System.arraycopy(cbor, 0, buf, 1, cbor.length)
        ByteString.unsafeFromArray(buf)
    }

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
