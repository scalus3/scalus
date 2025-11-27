package scalus.cardano.ledger

import io.bullet.borer.*
import scalus.builtin.ByteString

import scala.collection.immutable

/** Metadata is a map from metadatum labels to metadatum values */
type Metadata = Map[Word64, Metadatum]

@deprecated("Use Word64 instead", "0.12")
type TransactionMetadatumLabel = Word64
@deprecated("Use Word64 instead", "0.12")
val TransactionMetadatumLabel = Word64

@deprecated("Use Metadatum instead", "0.12")
type TransactionMetadatum = Metadatum
@deprecated("Use Metadatum instead", "0.12")
val TransactionMetadatum = Metadatum

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

    /** CBOR encoder for TransactionMetadatum */
    given Encoder[Metadatum] with
        def write(w: Writer, value: Metadatum): Writer = value match
            case Metadatum.Map(entries) =>
                w.writeMapHeader(entries.size)
                entries.foreach { case (k, v) =>
                    write(w, k)
                    write(w, v)
                }
                w

            case Metadatum.List(items) =>
                w.writeIndexedSeq(items)

            case Metadatum.Int(value) =>
                w.writeLong(value)

            case Metadatum.Bytes(value) =>
                w.writeBytes(value.bytes)

            case Metadatum.Text(value) =>
                w.writeString(value)

    /** CBOR decoder for TransactionMetadatum */
    given Decoder[Metadatum] with
        def read(r: Reader): Metadatum =
            import io.bullet.borer.DataItem as DI

            r.dataItem() match
                case DI.MapHeader | DI.MapStart =>
                    val entries =
                        r.read[immutable.Map[Metadatum, Metadatum]]()
                    Metadatum.Map(entries)

                case DI.ArrayHeader | DI.ArrayStart =>
                    val items = r.read[IndexedSeq[Metadatum]]()
                    Metadatum.List(items)

                case DI.Int | DI.Long | DI.OverLong =>
                    Metadatum.Int(r.readLong())

                case DI.Bytes | DI.BytesStart =>
                    val bytes = r.read[ByteString]()
                    Metadatum.Bytes(bytes)

                case DI.Text | DI.TextStart =>
                    val text = r.readString()
                    Metadatum.Text(text)

                case other =>
                    r.validationFailure(s"Unexpected data item for TransactionMetadatum: $other")

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
