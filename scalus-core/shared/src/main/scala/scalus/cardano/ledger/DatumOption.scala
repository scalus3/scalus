package scalus.cardano.ledger

import io.bullet.borer.Tag.EmbeddedCBOR
import io.bullet.borer.*
import org.typelevel.paiges.Doc
import scalus.uplc.builtin.Data
import scalus.utils.{Pretty, Style}

import scala.compiletime.asMatchable

/** Represents a datum option in Cardano outputs */
sealed trait DatumOption:
    import DatumOption.*

    /** Return true when the semantic content is the same (handles hash vs inline). Two inline
      * datums compare by their `Data`, not their bytes, spec [SC-13h]. A hash compares with the
      * hash of the original bytes of an inline datum, spec [SC-13l].
      */
    def contentEquals(other: DatumOption): Boolean = (this, other) match
        case (Hash(h1), Hash(h2))      => h1 == h2
        case (Inline(d1), Inline(d2))  => d1 == d2
        case (Hash(h), inline: Inline) => h == inline.dataHash
        case (inline: Inline, Hash(h)) => inline.dataHash == h

    /** The datum hash. For an inline datum, the hash of its original bytes, spec [SC-13i]. */
    def dataHash: DataHash = this match
        case Hash(h)        => h
        case inline: Inline => inline.binaryData.dataHash

    def dataHashOption: Option[DataHash] = this match
        case Hash(h)   => Some(h)
        case Inline(_) => None

    def dataOption: Option[Data] = this match
        case Hash(_)   => None
        case Inline(d) => Some(d)

object DatumOption:
    import Doc.*

    /** Reference to a datum by its hash */
    final case class Hash(hash: DataHash) extends DatumOption

    object Hash:
        /** Typed as [[DatumOption]], as the enum case it replaces was. */
        def apply(hash: DataHash): DatumOption = new Hash(hash)

    /** Inline datum value, with the CBOR it arrived in, spec [SC-13b].
      *
      * Haskell memoizes these bytes (`BinaryData`), and so does this: the encoder writes them back,
      * and two inline datums are equal only if their bytes are, spec [SC-13g]. Build one with
      * `Inline(data)`, and read its `Data` with `case Inline(d)`.
      */
    final case class Inline private (binaryData: KeepRaw[Data]) extends DatumOption:
        /** The datum value. */
        def data: Data = binaryData.value

        override def toString: String = s"Inline($data)"

    object Inline:
        /** An inline datum encoded canonically, spec [SC-13e]. Typed as [[DatumOption]], as the
          * enum case it replaces was.
          */
        def apply(data: Data): DatumOption = new Inline(KeepRaw(data))

        /** An inline datum that keeps the bytes `binaryData` holds. */
        def fromBinaryData(binaryData: KeepRaw[Data]): Inline = new Inline(binaryData)

        /** An inline datum decoded from `cbor` that keeps these bytes, a non-minimal encoding
          * included.
          */
        def fromCbor(cbor: Array[Byte]): Inline =
            new Inline(KeepRaw.unsafe(Data.fromCbor(cbor), cbor))

        /** Binds the `Data`, spec [SC-13f]. */
        def unapply(inline: Inline): Some[Data] = Some(inline.data)

    /** Pretty prints DatumOption - shows hash hex or inline data */
    given Pretty[DatumOption] with
        def pretty(a: DatumOption, style: Style): Doc = a match
            case DatumOption.Hash(hash)   => text(hash.toHex)
            case DatumOption.Inline(data) => Pretty[Data].pretty(data, style)

    /** CBOR encoder for DatumOption */
    given Encoder[DatumOption] with
        def write(w: Writer, value: DatumOption): Writer =
            w.writeArrayHeader(2)
            value match
                case DatumOption.Hash(hash) =>
                    w.writeInt(0)
                    w.write(hash)

                case inline: DatumOption.Inline =>
                    w.writeInt(1)
                    // the bytes it was decoded from, spec [SC-13d]
                    w.write(EmbeddedCBOR @@ inline.binaryData.raw)
            w

    /** CBOR decoder for DatumOption */
    given Decoder[DatumOption] with
        def read(r: Reader): DatumOption =
            val size = r.readArrayHeader()
            if size != 2 then r.validationFailure(s"Expected 2 elements for DatumOption, got $size")

            val tag = r.readInt()
            tag match
                case 0 => DatumOption.Hash(r.read[DataHash]())
                case 1 =>
                    val tag = r.readTag()
                    if tag != EmbeddedCBOR then
                        r.validationFailure(s"Expected tag 24 for Data, got $tag")

                    // Read the embedded CBOR bytes
                    val bytes: Array[Byte] = r.readBytes()

                    // The tag-24 payload is exactly the datum's CBOR: keep it, spec [SC-13c]
                    DatumOption.Inline.fromCbor(bytes)
                case other => r.validationFailure(s"Invalid DatumOption tag: $tag")
