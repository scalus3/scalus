package scalus.cardano.node.stream.engine.snapshot

import io.bullet.borer.Cbor
import scalus.cardano.node.stream.engine.AppliedBlock

import java.io.{DataOutputStream, OutputStream}
import java.nio.{ByteBuffer, ByteOrder}
import java.security.MessageDigest

import ChainStoreSnapshot.{Footer, Header, Record}
import SnapshotCodecs.given

/** Streaming writer for [[ChainStoreSnapshot]]. Caller opens the stream, writes a header, streams
  * any number of body records, then calls `finish()` to emit the footer (with running sha256).
  *
  * Layout on the wire:
  * {{{
  *   u32-BE(len) record-cbor-bytes    // header
  *   u32-BE(len) record-cbor-bytes    // body record 1
  *   ...
  *   u32-BE(len) record-cbor-bytes    // body record N
  *   u32-BE(len) record-cbor-bytes    // footer
  * }}}
  * Length-prefixing keeps the reader trivially streaming without requiring a CBOR-streaming cursor;
  * `u32-BE` is a hard per-record cap of 4 GiB which is orders of magnitude above any single
  * AppliedBlock.
  *
  * Not thread-safe. The writer owns its running hash and buffer.
  */
final class SnapshotWriter private (out: OutputStream) extends AutoCloseable {

    private val dataOut = new DataOutputStream(out)
    private val hasher: MessageDigest = MessageDigest.getInstance("SHA-256")
    private var wroteHeader: Boolean = false
    private var finished: Boolean = false

    /** Write the header. Must be called before any body record. */
    def writeHeader(header: Header): Unit = {
        require(!wroteHeader, "header already written")
        require(!finished, "writer already finished")
        val bytes = Cbor.encode(header).toByteArray
        writeFrame(bytes, updateHash = false)
        wroteHeader = true
    }

    /** Append one body record. Its bytes participate in the running sha256. */
    def writeRecord(record: Record): Unit = {
        require(wroteHeader, "must writeHeader first")
        require(!finished, "writer already finished")
        val bytes = Cbor.encode(record).toByteArray
        writeFrame(bytes, updateHash = true)
    }

    /** Convenience: write an [[AppliedBlock]] as a [[Record.BlockRecord]]. */
    def writeBlock(block: AppliedBlock): Unit = writeRecord(Record.BlockRecord(block))

    /** Convenience: write a single UTxO as a [[Record.UtxoEntry]]. */
    def writeUtxo(
        input: scalus.cardano.ledger.TransactionInput,
        output: scalus.cardano.ledger.TransactionOutput
    ): Unit = writeRecord(Record.UtxoEntry(input, output))

    /** Emit the footer and flush. The footer's sha256 covers every body record's bytes in stream
      * order; header and footer themselves are not hashed. Idempotent — second call is a no-op.
      */
    def finish(): Unit = {
        if finished then return
        require(wroteHeader, "must writeHeader before finish")
        val footer = Footer(hasher.digest(), ChainStoreSnapshot.FooterSentinel)
        val bytes = Cbor.encode(footer).toByteArray
        writeFrame(bytes, updateHash = false)
        dataOut.flush()
        finished = true
    }

    def close(): Unit = {
        if !finished then finish()
        dataOut.close()
    }

    private def writeFrame(bytes: Array[Byte], updateHash: Boolean): Unit = {
        val lenBuf = ByteBuffer.allocate(4).order(ByteOrder.BIG_ENDIAN).putInt(bytes.length).array()
        dataOut.write(lenBuf)
        dataOut.write(bytes)
        if updateHash then hasher.update(bytes)
    }
}

object SnapshotWriter {
    def apply(out: OutputStream): SnapshotWriter = new SnapshotWriter(out)
}
