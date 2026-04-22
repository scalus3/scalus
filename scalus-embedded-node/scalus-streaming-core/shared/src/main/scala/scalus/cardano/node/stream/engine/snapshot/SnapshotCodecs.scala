package scalus.cardano.node.stream.engine.snapshot

import io.bullet.borer.{Cbor, Decoder, Encoder, Reader, Writer}
import scalus.cardano.ledger.{TransactionInput, TransactionOutput}

import ChainStoreSnapshot.{Footer, Header, Record}

/** CBOR codecs for the [[ChainStoreSnapshot]] record types. Hand-written to match the style used by
  * [[scalus.cardano.node.stream.engine.persistence.PersistenceCodecs]]; record-level codecs for
  * [[scalus.cardano.node.stream.engine.AppliedBlock]] are pulled from
  * [[scalus.cardano.node.stream.engine.KvChainStore]] so the two on-disk stores can't drift.
  */
private[snapshot] object SnapshotCodecs {

    import scalus.cardano.node.stream.engine.KvChainStore.given
    import scalus.cardano.node.stream.engine.persistence.PersistenceCodecs.given

    // Record tag map: 0 → BlockRecord, 1 → UtxoEntry. Separate from header/footer — each record
    // carries its own array header so readers don't need a length prefix.

    given Encoder[Record] with
        def write(w: Writer, r: Record): Writer = r match {
            case Record.BlockRecord(block) =>
                w.writeArrayHeader(2).writeInt(0).write(block)
            case Record.UtxoEntry(input, output) =>
                w.writeArrayHeader(3).writeInt(1).write(input).write(output)
        }

    given Decoder[Record] with
        def read(r: Reader): Record = {
            val arrLen = r.readArrayHeader().toInt
            r.readInt() match {
                case 0 if arrLen == 2 =>
                    Record.BlockRecord(r.read[scalus.cardano.node.stream.engine.AppliedBlock]())
                case 1 if arrLen == 3 =>
                    Record.UtxoEntry(r.read[TransactionInput](), r.read[TransactionOutput]())
                case other =>
                    r.validationFailure(s"unexpected Record tag=$other arrLen=$arrLen")
            }
        }

    given Encoder[Header] with
        def write(w: Writer, h: Header): Writer =
            w.writeArrayHeader(6)
                .writeInt(h.schemaVersion)
                .writeLong(h.networkMagic)
                .write(h.tip)
                .writeLong(h.blockCount)
                .writeLong(h.utxoCount)
                .writeInt(h.contentFlags)

    given Decoder[Header] with
        def read(r: Reader): Header = {
            val len = r.readArrayHeader().toInt
            if len != 6 then r.validationFailure(s"Header arrLen=$len (expected 6)")
            Header(
              schemaVersion = r.readInt(),
              networkMagic = r.readLong(),
              tip = r.read[scalus.cardano.node.stream.ChainTip](),
              blockCount = r.readLong(),
              utxoCount = r.readLong(),
              contentFlags = r.readInt()
            )
        }

    given Encoder[Footer] with
        def write(w: Writer, f: Footer): Writer =
            w.writeArrayHeader(2).writeBytes(f.sha256).writeInt(f.sentinel)

    given Decoder[Footer] with
        def read(r: Reader): Footer = {
            val len = r.readArrayHeader().toInt
            if len != 2 then r.validationFailure(s"Footer arrLen=$len (expected 2)")
            val sha = r.readByteArray()
            val sentinel = r.readInt()
            Footer(sha, sentinel)
        }

    def encodeRecord(rec: Record): Array[Byte] =
        Cbor.encode(rec).toByteArray

    def decodeRecord(bytes: Array[Byte]): Record =
        Cbor.decode(bytes).to[Record].value
}
