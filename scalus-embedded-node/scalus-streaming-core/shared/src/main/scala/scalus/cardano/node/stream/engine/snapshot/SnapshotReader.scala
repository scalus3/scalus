package scalus.cardano.node.stream.engine.snapshot

import io.bullet.borer.Cbor

import java.io.{DataInputStream, EOFException, InputStream}
import java.security.MessageDigest
import scala.util.control.NonFatal

import ChainStoreSnapshot.{Footer, Header, Record}
import SnapshotCodecs.given

/** Streaming reader for [[ChainStoreSnapshot]]. Caller opens the stream, reads the header, drives
  * `records()` to completion (Iterator[Record]), then calls `finish()` to validate the footer
  * against the running sha256.
  *
  * Usage:
  * {{{
  *   val reader = SnapshotReader(in)
  *   val header = reader.headerRead()
  *   val it = reader.records()
  *   it.foreach { case Record.BlockRecord(b) => ...; case Record.UtxoEntry(i, o) => ... }
  *   reader.finish()                  // validates footer, throws SnapshotCorrupted on mismatch
  *   reader.close()
  * }}}
  *
  * Not thread-safe.
  */
final class SnapshotReader private (in: InputStream) extends AutoCloseable {

    private val dataIn = new DataInputStream(in)
    private val hasher: MessageDigest = MessageDigest.getInstance("SHA-256")
    private var headerSeen: Boolean = false
    private var footerSeen: Boolean = false
    private var nextFrameBytes: Option[Array[Byte]] = None

    /** Read the header frame. Refuses snapshots at an unsupported schema version. */
    def readHeader(): Header = {
        require(!headerSeen, "header already read")
        val bytes = readFrameNoHash()
        val header =
            try Cbor.decode(bytes).to[Header].value
            catch {
                case NonFatal(t) =>
                    throw SnapshotError.SnapshotCorrupted("cannot decode header", t)
            }
        if header.schemaVersion != ChainStoreSnapshot.SchemaVersion then
            throw SnapshotError.SnapshotSchemaMismatch(
              expected = ChainStoreSnapshot.SchemaVersion,
              actual = header.schemaVersion
            )
        headerSeen = true
        header
    }

    /** Iterator over body records. Terminates when the next frame's payload decodes as a [[Footer]]
      * rather than a [[Record]] — we peek one frame ahead so `finish()` can consume it for footer
      * validation.
      *
      * Must be exhausted (i.e. iterated until hasNext is false) before `finish()`.
      */
    def records(): Iterator[Record] = {
        require(headerSeen, "must readHeader first")
        new Iterator[Record] {
            def hasNext: Boolean = peekIsRecord()

            def next(): Record = {
                val bytes = nextFrameBytes.getOrElse(
                  throw new NoSuchElementException("records iterator exhausted")
                )
                nextFrameBytes = None
                hasher.update(bytes)
                try Cbor.decode(bytes).to[Record].value
                catch {
                    case NonFatal(t) =>
                        throw SnapshotError.SnapshotCorrupted("cannot decode body record", t)
                }
            }

            // Pre-fetch one frame so we can distinguish record-frame from footer-frame.
            private def peekIsRecord(): Boolean = {
                if nextFrameBytes.isEmpty then nextFrameBytes = Some(readFrameNoHash())
                !looksLikeFooter(nextFrameBytes.get)
            }
        }
    }

    /** Validate the footer. Must be called after the `records()` iterator is exhausted. */
    def finish(): Unit = {
        require(headerSeen, "must readHeader first")
        require(!footerSeen, "footer already consumed")
        val bytes = nextFrameBytes match {
            case Some(b) =>
                nextFrameBytes = None
                b
            case None => readFrameNoHash()
        }
        val footer =
            try Cbor.decode(bytes).to[Footer].value
            catch {
                case NonFatal(t) =>
                    throw SnapshotError.SnapshotCorrupted("cannot decode footer", t)
            }
        if footer.sentinel != ChainStoreSnapshot.FooterSentinel then
            throw SnapshotError.SnapshotCorrupted(
              s"footer sentinel mismatch: got 0x${footer.sentinel.toHexString}"
            )
        val computed = hasher.digest()
        if !java.util.Arrays.equals(computed, footer.sha256) then
            throw SnapshotError.SnapshotCorrupted("body sha256 mismatch")
        footerSeen = true
    }

    def close(): Unit = dataIn.close()

    private def readFrameNoHash(): Array[Byte] = {
        val len =
            try dataIn.readInt()
            catch {
                case _: EOFException =>
                    throw SnapshotError.SnapshotCorrupted("unexpected EOF reading frame length")
            }
        if len < 0 then throw SnapshotError.SnapshotCorrupted(s"negative frame length $len")
        val bytes = new Array[Byte](len)
        try dataIn.readFully(bytes)
        catch {
            case _: EOFException =>
                throw SnapshotError.SnapshotCorrupted(s"unexpected EOF reading $len-byte frame")
        }
        bytes
    }

    /** CBOR-peek: a Footer is `[sha256-bytes, sentinel]` — its outer array header is two; a Record
      * is either `[0, block]` (len 2) or `[1, input, output]` (len 3). Both records' second element
      * is an integer tag; the footer's second element is also an integer. We can distinguish by
      * looking at the first element: Footer starts with a `bytes(32)` (a byte-string CBOR major
      * type 2), while every Record starts with an integer tag (major type 0 or 1). One byte's
      * enough.
      */
    private def looksLikeFooter(bytes: Array[Byte]): Boolean = {
        if bytes.length < 2 then
            throw SnapshotError.SnapshotCorrupted(s"frame too short: ${bytes.length} bytes")
        // bytes(0) is CBOR array header (0x82 for len 2, 0x83 for len 3). bytes(1) is the first
        // inner element. Major type is the top 3 bits: 0 = unsigned int, 2 = bytes.
        val innerMajor = (bytes(1) & 0xe0) >>> 5
        innerMajor == 2 // bytes(32) => Footer
    }
}

object SnapshotReader {
    def apply(in: InputStream): SnapshotReader = new SnapshotReader(in)
}
