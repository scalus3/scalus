package scalus.cardano.ledger

import io.bullet.borer.*
import scalus.serialization.cbor.NestedCbor
import scalus.uplc.builtin.ByteString

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

/** Metadata is a map from metadatum labels to metadatum values */
type Metadata = Map[Word64, Metadatum]

/** Represents transaction metadata in Cardano */
sealed trait Metadatum
object Metadatum:
    /** Map metadata */
    final case class Map(entries: immutable.Map[Metadatum, Metadatum]) extends Metadatum

    /** List metadata */
    final case class List(items: IndexedSeq[Metadatum]) extends Metadatum

    /** Integer metadata */
    final case class Int(value: Long) extends Metadatum

    /** Bytes metadata */
    final case class Bytes(value: ByteString) extends Metadatum

    /** Text metadata */
    final case class Text(value: String) extends Metadatum

    /** CBOR encoder for TransactionMetadatum.
      *
      * It does not recurse over the nesting: what is still to write is kept on the heap, last
      * first, so how deep it writes does not depend on the stack of the thread that writes. A list
      * is written as borer writes an `IndexedSeq`, and a map entry by entry, in its order.
      */
    given Encoder[Metadatum] with
        def write(w: Writer, value: Metadatum): Writer = {
            val pending = ArrayBuffer[Metadatum | ArrayEnd.type](value)
            while pending.nonEmpty do
                pending.remove(pending.size - 1) match
                    case ArrayEnd => w.writeArrayClose()

                    case Metadatum.Map(entries) =>
                        w.writeMapHeader(entries.size)
                        entries.toVector.reverseIterator.foreach { (k, v) =>
                            pending += v
                            pending += k
                        }

                    case Metadatum.List(items) =>
                        w.writeArrayOpen(items.size)
                        pending += ArrayEnd
                        items.reverseIterator.foreach(pending += _)

                    case Metadatum.Int(value) =>
                        w.writeLong(value)

                    case Metadatum.Bytes(value) =>
                        w.writeBytes(value.bytes)

                    case Metadatum.Text(value) =>
                        w.writeString(value)
            w
        }

    /** Where a list that the encoder has opened ends. */
    private object ArrayEnd

    /** CBOR decoder for TransactionMetadatum.
      *
      * It does not recurse over the nesting: it keeps the arrays and maps it has opened on the heap
      * ([[NestedCbor]]), so how deep it reads does not depend on the stack of the thread that
      * reads, with one exception: a map hashes its keys, and the hash of a key recurses over it, as
      * that of a case class does. It reads at most
      * [[scalus.serialization.cbor.Cbor.maxMetadataAndScriptNestingLevels]] levels: a deeper
      * metadatum fails to decode.
      *
      * An array is read as borer reads an `IndexedSeq`, into a `Vector`, and a map as borer reads
      * an `immutable.Map`: a later entry of a key replaces an earlier one.
      */
    given Decoder[Metadatum] with

        /** A list being read: the items read so far, and how many are still to read, or -1 for an
          * indefinite array.
          */
        private final class OpenList(private var remaining: scala.Int)
            extends NestedCbor.Open[Metadatum] {
            private val items = IndexedSeq.newBuilder[Metadatum]

            def isComplete(r: Reader): Boolean = NestedCbor.isEnd(r, remaining)

            def add(item: Metadatum): Unit = {
                items += item
                if remaining > 0 then remaining -= 1
            }

            def close(r: Reader): Metadatum = Metadatum.List(items.result())
        }

        /** A map being read: the entries read so far, the key of the entry being read, and how many
          * entries are still to read, or -1 for an indefinite map.
          */
        private final class OpenMap(private var remaining: scala.Int)
            extends NestedCbor.Open[Metadatum] {
            private val entries = immutable.Map.newBuilder[Metadatum, Metadatum]
            private var key: Metadatum = null

            // a break between a key and its value is not the end of the map
            def isComplete(r: Reader): Boolean = (key eq null) && NestedCbor.isEnd(r, remaining)

            def add(item: Metadatum): Unit = {
                if key eq null then key = item
                else
                    entries += key -> item
                    key = null
                    if remaining > 0 then remaining -= 1
            }

            def close(r: Reader): Metadatum = Metadatum.Map(entries.result())
        }

        def read(r: Reader): Metadatum = {
            readItem(r, depth = 0) match
                case metadatum: Metadatum => metadatum
                case open: NestedCbor.Open[Metadatum @unchecked] =>
                    NestedCbor.readNested(open, r)(depth => readItem(r, depth))
        }

        /** Reads a metadatum held in `depth` arrays and maps that holds no other, or opens an array
          * or a map.
          */
        private def readItem(
            r: Reader,
            depth: scala.Int
        ): Metadatum | NestedCbor.Open[Metadatum] = {
            import io.bullet.borer.DataItem as DI
            r.dataItem() match {
                case DI.MapHeader | DI.MapStart =>
                    NestedCbor.checkDepth(r, depth, "metadata")
                    if r.hasMapHeader then OpenMap(NestedCbor.definite(r, r.readMapHeader(), "Map"))
                    else
                        r.readMapStart()
                        OpenMap(-1)

                case DI.ArrayHeader | DI.ArrayStart =>
                    NestedCbor.checkDepth(r, depth, "metadata")
                    if r.hasArrayHeader then
                        OpenList(NestedCbor.definite(r, r.readArrayHeader(), "Iterable"))
                    else
                        r.readArrayStart()
                        OpenList(-1)

                case DI.Int | DI.Long | DI.OverLong =>
                    Metadatum.Int(r.readLong())

                case DI.Bytes | DI.BytesStart =>
                    Metadatum.Bytes(r.read[ByteString]())

                case DI.Text | DI.TextStart =>
                    Metadatum.Text(r.readString())

                case other =>
                    r.validationFailure(s"Unexpected data item for TransactionMetadatum: $other")
            }
        }

/** Represents auxiliary data in a Cardano transaction */
enum AuxiliaryData:
    /** Shelley-era metadata */
    case Metadata(metadata: Map[Word64, Metadatum])

    /** Shelley-MA era combined metadata and scripts */
    case MetadataWithScripts(
        metadata: Map[Word64, Metadatum],
        nativeScripts: IndexedSeq[Timelock]
    )

    /** Alonzo-era and later metadata format with optional components */
    case AlonzoFormat(
        metadata: Option[Map[Word64, Metadatum]] = None,
        nativeScripts: IndexedSeq[Timelock] = IndexedSeq.empty,
        plutusV1Scripts: IndexedSeq[ByteString] = IndexedSeq.empty,
        plutusV2Scripts: IndexedSeq[ByteString] = IndexedSeq.empty,
        plutusV3Scripts: IndexedSeq[ByteString] = IndexedSeq.empty
    )

    def getMetadata: Map[Word64, Metadatum] = this match
        case data: AuxiliaryData.Metadata            => data.metadata
        case data: AuxiliaryData.MetadataWithScripts => data.metadata
        case data: AuxiliaryData.AlonzoFormat        => data.metadata.getOrElse(Map.empty)

    def getNativeScripts: IndexedSeq[Timelock] = this match
        case _: AuxiliaryData.Metadata               => IndexedSeq.empty
        case data: AuxiliaryData.MetadataWithScripts => data.nativeScripts
        case data: AuxiliaryData.AlonzoFormat        => data.nativeScripts

    def getPlutusV1Scripts: IndexedSeq[ByteString] = this match
        case _: AuxiliaryData.Metadata            => IndexedSeq.empty
        case _: AuxiliaryData.MetadataWithScripts => IndexedSeq.empty
        case data: AuxiliaryData.AlonzoFormat     => data.plutusV1Scripts

    def getPlutusV2Scripts: IndexedSeq[ByteString] = this match
        case _: AuxiliaryData.Metadata            => IndexedSeq.empty
        case _: AuxiliaryData.MetadataWithScripts => IndexedSeq.empty
        case data: AuxiliaryData.AlonzoFormat     => data.plutusV2Scripts

    def getPlutusV3Scripts: IndexedSeq[ByteString] = this match
        case _: AuxiliaryData.Metadata            => IndexedSeq.empty
        case _: AuxiliaryData.MetadataWithScripts => IndexedSeq.empty
        case data: AuxiliaryData.AlonzoFormat     => data.plutusV3Scripts

    def getPlutusScripts: IndexedSeq[ByteString] =
        (getPlutusV1Scripts.view ++ getPlutusV2Scripts.view ++ getPlutusV3Scripts.view).toIndexedSeq

    def getAllScripts: IndexedSeq[Timelock | ByteString] =
        (
          getNativeScripts.view ++ getPlutusV1Scripts.view ++ getPlutusV2Scripts.view ++ getPlutusV3Scripts.view
        ).toIndexedSeq

object AuxiliaryData:
    /** CBOR encoder for AuxiliaryData */
    given Encoder[AuxiliaryData] with
        def write(w: Writer, value: AuxiliaryData): Writer = value match
            case AuxiliaryData.Metadata(metadata) =>
                // Metadata is encoded directly
                w.write(metadata)

            case AuxiliaryData.MetadataWithScripts(metadata, nativeScripts) =>
                // Array of [metadata, scripts]
                w.writeArrayHeader(2)
                w.write(metadata)

                // Write scripts array
                w.writeArrayHeader(nativeScripts.size)
                nativeScripts.foreach(nativeScript => w.write(nativeScript))
                w

            case AuxiliaryData.AlonzoFormat(
                  metadata,
                  nativeScripts,
                  plutusV1Scripts,
                  plutusV2Scripts,
                  plutusV3Scripts
                ) =>
                // Calculate map size
                var mapSize = 0
                if metadata.isDefined then mapSize += 1
                if nativeScripts.nonEmpty then mapSize += 1
                if plutusV1Scripts.nonEmpty then mapSize += 1
                if plutusV2Scripts.nonEmpty then mapSize += 1
                if plutusV3Scripts.nonEmpty then mapSize += 1

                // Write the tag 259 first
                w.writeTag(Tag.Other(259))

                // Then write the map with all fields
                w.writeMapHeader(mapSize)

                // Metadata (key 0)
                metadata.foreach { md =>
                    w.writeInt(0)
                    w.write(md)
                }

                // Native scripts (key 1)
                if nativeScripts.nonEmpty then
                    w.writeInt(1)
                    w.writeIndexedSeq(nativeScripts)

                // Plutus V1 scripts (key 2)
                if plutusV1Scripts.nonEmpty then
                    w.writeInt(2)
                    w.writeIndexedSeq(plutusV1Scripts)

                // Plutus V2 scripts (key 3)
                if plutusV2Scripts.nonEmpty then
                    w.writeInt(3)
                    w.writeIndexedSeq(plutusV2Scripts)

                // Plutus V3 scripts (key 4)
                if plutusV3Scripts.nonEmpty then
                    w.writeInt(4)
                    w.writeIndexedSeq(plutusV3Scripts)

                w

    /** CBOR decoder for AuxiliaryData */
    given Decoder[AuxiliaryData] with
        def read(r: Reader): AuxiliaryData =
            import io.bullet.borer.DataItem as DI

            r.dataItem() match
                case DI.Tag if r.tryReadTag(Tag.Other(259)) =>
                    // Alonzo format with tag
                    // We've already consumed the tag, now read the map
                    val size = r.readMapHeader()
                    var metadata: Option[Map[Word64, Metadatum]] =
                        None
                    var nativeScripts = IndexedSeq.empty[Timelock]
                    var plutusV1Scripts = IndexedSeq.empty[ByteString]
                    var plutusV2Scripts = IndexedSeq.empty[ByteString]
                    var plutusV3Scripts = IndexedSeq.empty[ByteString]

                    for _ <- 0L until size do
                        val key = r.readInt()
                        key match
                            case 0 => // Metadata
                                metadata = Some(r.read[scalus.cardano.ledger.Metadata]())

                            case 1 => // Native scripts
                                nativeScripts = r.read[IndexedSeq[Timelock]]()

                            case 2 => // Plutus V1 scripts
                                plutusV1Scripts = r.read[IndexedSeq[ByteString]]()

                            case 3 => // Plutus V2 scripts
                                plutusV2Scripts = r.read[IndexedSeq[ByteString]]()

                            case 4 => // Plutus V3 scripts
                                plutusV3Scripts = r.read[IndexedSeq[ByteString]]()

                            case _ => r.skipDataItem() // Skip unknown fields

                    AuxiliaryData.AlonzoFormat(
                      metadata,
                      nativeScripts,
                      plutusV1Scripts,
                      plutusV2Scripts,
                      plutusV3Scripts
                    )

                case DI.MapHeader | DI.MapStart =>
                    // Simple metadata format
                    AuxiliaryData.Metadata(r.read[scalus.cardano.ledger.Metadata]())

                case DI.ArrayHeader | DI.ArrayStart =>
                    // MetadataWithScripts format
                    val size = r.readArrayHeader()
                    if size != 2 then
                        r.validationFailure(
                          s"Expected 2 elements for MetadataWithScripts, got $size"
                        )

                    val metadata = r.read[scalus.cardano.ledger.Metadata]()

                    val scripts = r.read[IndexedSeq[Timelock]]()
                    AuxiliaryData.MetadataWithScripts(metadata, scripts)

                case di =>
                    r.validationFailure(
                      s"Expected Map, MapStart, Array, or ArrayStart for AuxiliaryData, got ${DataItem
                              .stringify(di)}"
                    )
