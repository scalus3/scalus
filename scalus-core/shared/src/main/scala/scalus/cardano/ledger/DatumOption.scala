package scalus.cardano.ledger

import io.bullet.borer.Tag.EmbeddedCBOR
import io.bullet.borer.*
import org.typelevel.paiges.Doc
import scalus.builtin.Data
import scalus.utils.{Pretty, Style}

/** Represents a datum option in Cardano outputs */
enum DatumOption:
    /** Reference to a datum by its hash */
    case Hash(hash: DataHash)

    /** Inline datum value */
    case Inline(data: Data)

    /** Return true when the semantic content is the same (handles hash vs inline) */
    def contentEquals(other: DatumOption): Boolean = (this, other) match
        case (Hash(h1), Hash(h2))     => h1 == h2
        case (Inline(d1), Inline(d2)) => d1 == d2
        case (Hash(h), Inline(d))     => h == d.dataHash
        case (Inline(d), Hash(h))     => d.dataHash == h

    def dataHash: DataHash = this match
        case Hash(h)   => h
        case Inline(d) => DataHash.fromByteString(d.dataHash)

    def dataHashOption: Option[DataHash] = this match
        case Hash(h)   => Some(h)
        case Inline(_) => None

    def dataOption: Option[Data] = this match
        case Hash(_)   => None
        case Inline(d) => Some(d)

object DatumOption:
    import Doc.*
    import Pretty.inParens

    /** Pretty prints DatumOption as `Hash(...)` or `Inline(...)` */
    given Pretty[DatumOption] with
        def pretty(a: DatumOption, style: Style): Doc = a match
            case DatumOption.Hash(hash)   => text("Hash") + inParens(text(hash.toHex))
            case DatumOption.Inline(data) => text("Inline") + inParens(text(data.toString))

    /** CBOR encoder for DatumOption */
    given Encoder[DatumOption] with
        def write(w: Writer, value: DatumOption): Writer =
            w.writeArrayHeader(2)
            value match
                case DatumOption.Hash(hash) =>
                    w.writeInt(0)
                    w.write(hash)

                case DatumOption.Inline(data) =>
                    w.writeInt(1)
                    val dataCbor = Cbor.encode(data).toByteArray
                    w.write(EmbeddedCBOR @@ dataCbor)
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

                    // Parse the bytes as a Script
                    val data = Cbor.decode(bytes).to[Data].value
                    DatumOption.Inline(data)
                case other => r.validationFailure(s"Invalid DatumOption tag: $tag")
