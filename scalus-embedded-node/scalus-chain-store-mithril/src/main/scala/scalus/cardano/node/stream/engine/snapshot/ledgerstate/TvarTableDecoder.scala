package scalus.cardano.node.stream.engine.snapshot.ledgerstate

import io.bullet.borer.{Cbor, Reader}
import scalus.cardano.ledger.{TransactionInput, TransactionOutput}

import java.io.InputStream

/** Streaming CBOR decoder for a UTxO-HD V2 InMemory `tables/tvar` file.
  *
  * ==File shape==
  *
  * The `tables/tvar` file is CBOR-framed (outer shell written by
  * `ouroboros-consensus/.../LedgerDB/V2/InMemory.hs` `valuesMKEncoder`):
  *
  * {{{
  *   list(1) [
  *     map(n) {
  *       bytes(memPack-packed-TxIn) : bytes(memPack-packed-TxOut),
  *       ...
  *     }
  *   ]
  * }}}
  *
  * The map is either finite-length (bulk writer path via `implTakeHandleSnapshot`) or
  * indefinite (streaming writer path via `sinkInMemoryS`). This decoder handles both.
  *
  * Each map key and each map value is a CBOR `bytes` item whose payload is a MemPack-packed
  * record — we hand the bytes to [[MemPackReaders.readTxIn]] / [[MemPackReaders.readTxOut]].
  *
  * ==Memory profile==
  *
  * Mainnet `tables/tvar` is ~1-2 GB uncompressed. The [[stream]] method yields one
  * `(TxIn, TxOut)` pair at a time; nothing beyond the Borer buffer and one pair's MemPack
  * payload is held in memory. Callers MUST drain the iterator or call `close()` on the
  * returned handle to release the underlying InputStream.
  */
object TvarTableDecoder {

    /** Open a tvar file and return a lazy iterator of decoded `(TxIn, TxOut)` pairs along with
      * a `close()` handle the caller is responsible for invoking (typically via a
      * try-finally). The underlying `InputStream` stays open until `close()` runs.
      */
    def stream(input: InputStream): Handle = {
        val reader: Reader = Cbor.reader(input)
        // Outer list header — must be length 1.
        reader.readArrayHeader(1L)
        // Map header: finite length OR indefinite-start.
        val finiteLen: Long =
            if reader.tryReadMapStart() then -1L
            else reader.readMapHeader()

        val iter = new Iterator[(TransactionInput, TransactionOutput)] {
            private var yielded: Long = 0L
            private var cachedHasNext: Option[Boolean] = None

            override def hasNext: Boolean = {
                cachedHasNext match {
                    case Some(b) => b
                    case None =>
                        val b =
                            if finiteLen >= 0 then yielded < finiteLen
                            else !reader.tryReadBreak()
                        cachedHasNext = Some(b)
                        b
                }
            }

            override def next(): (TransactionInput, TransactionOutput) = {
                if !hasNext then throw new NoSuchElementException("tvar stream exhausted")
                cachedHasNext = None
                val keyBytes = reader.readByteArray()
                val valueBytes = reader.readByteArray()
                val txIn = MemPackReaders.readTxIn(MemPack.Reader(keyBytes))
                val txOut = MemPackReaders.readTxOut(MemPack.Reader(valueBytes))
                yielded += 1
                (txIn, txOut)
            }
        }

        new Handle(iter, () => input.close(), finiteLen)
    }

    /** Lazy iterator + close handle. Owns the `InputStream`; callers MUST `close()` eventually
      * (use try-finally). `expectedCount` is the map's advertised length for finite maps, or
      * `-1` for indefinite — callers can use it as a progress baseline.
      */
    final class Handle private[TvarTableDecoder] (
        val iterator: Iterator[(TransactionInput, TransactionOutput)],
        onClose: () => Unit,
        val expectedCount: Long
    ) extends AutoCloseable {
        override def close(): Unit = onClose()
    }
}
