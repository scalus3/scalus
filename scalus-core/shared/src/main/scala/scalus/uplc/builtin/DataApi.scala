package scalus.uplc.builtin

import io.bullet.borer
import io.bullet.borer.Tag.{NegativeBigNum, Other, PositiveBigNum}
import io.bullet.borer.{ByteAccess, Cbor, DataItem as DI, Decoder, Encoder, Reader, Tag}
import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc.*
import scalus.uplc.builtin.Data.{B, Constr, FromData, I, Map}
import scalus.compiler.fieldAsData
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.serialization.cbor.Cbor as ScalusCbor
import scalus.serialization.flat
import scalus.serialization.flat.{DecoderState, EncoderState, Flat, given}
import scalus.utils.Pretty.{lit, rainbowBracket}
import scalus.utils.{Pretty, Style}
import upickle.default.*
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonReader, JsonValueCodec, JsonWriter, WriterConfig}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}

import java.io.InputStream
import java.math.BigInteger
import scala.collection.mutable.{ArrayBuffer, ArrayBuilder}
import scala.util.Try

private trait DataApi {
    extension (self: Data)
        inline def dataHash: ByteString =
            Builtins.blake2b_256(Builtins.serialiseData(self))

    /** JSON codec for [[Data]]'s standard encoding, the one Cardano tooling uses: `{"int":42}`,
      * `{"bytes":"deadbeef"}`, `{"list":[..]}`, `{"map":[{"k":..,"v":..}]}` and
      * `{"constructor":0,"fields":[..]}`.
      *
      * On jsoniter rather than upickle so that `scalus.js` does not have to carry upickle's
      * derivation machinery: `Data.fromJson` backs the exported `applyDataArgToScript`, so every
      * path through it kept upickle, ujson and upack in the bundle. See
      * `docs/internal/JS_BUNDLE_SIZE.md`.
      *
      * The encoding cannot be derived from [[Data]] directly. jsoniter writes an ADT either with a
      * discriminator field or, without one, as a single-key wrapper object, and this format is
      * neither: `int`, `bytes`, `list` and `map` are single-key wrappers, but `constructor` is a
      * flat two-key object. So [[DataJson]] mirrors the format as something the macro can derive,
      * and this codec maps between the two. Keys are accepted in any order.
      */
    given dataJsonCodec: JsonValueCodec[Data] = new JsonValueCodec[Data] {
        override def nullValue: Data = null

        override def decodeValue(in: JsonReader, default: Data): Data =
            DataJson.toData(DataJson.codec.decodeValue(in, null), in)

        override def encodeValue(data: Data, out: JsonWriter): Unit = DataJson.write(data, out)
    }

    /** JSON ReadWriter for the [[Data]] type.
      *
      * Retained for source compatibility only. `toJson` and `fromJson` go through
      * [[dataJsonCodec]], which keeps upickle out of the Scala.js bundle.
      */
    given DataReadWriter: ReadWriter[Data] = {
        given ReadWriter[Data] = DataReadWriter

        readwriter[ujson.Value].bimap(
          {
              case Data.Constr(constr, args) =>
                  ujson.Obj(
                    "constructor" -> writeJs(constr),
                    "fields" -> ujson.Arr(ArrayBuffer.from(args.toScalaList.map(writeJs)))
                  )
              case Data.Map(values) =>
                  ujson.Obj("map" -> ujson.Arr(ArrayBuffer.from(values.toScalaList.map {
                      case (k, v) =>
                          ujson.Obj("k" -> writeJs(k), "v" -> writeJs(v))
                  })))
              case Data.List(values) =>
                  ujson.Obj("list" -> ujson.Arr(ArrayBuffer.from(values.toScalaList.map(writeJs))))
              case Data.I(value) =>
                  val v = if value.isValidLong then writeJs(value.toLong) else writeJs(value)
                  ujson.Obj("int" -> v)
              case Data.B(value) => ujson.Obj("bytes" -> writeJs(value.toHex))
          },
          json =>
              if json.obj.get("constructor").isDefined then
                  Data.Constr(
                    BigInt(json.obj("constructor").num.toLong),
                    PList.from(json.obj("fields").arr.map(f => read[Data](f)).toList)
                  )
              else if json.obj.get("map").isDefined then
                  Data.Map(
                    PList.from(
                      json
                          .obj("map")
                          .arr
                          .map { obj =>
                              val k = read[Data](obj.obj("k"))
                              val v = read[Data](obj.obj("v"))
                              k -> v
                          }
                          .toList
                    )
                  )
              else if json.obj.get("list").isDefined then
                  Data.List(PList.from(json.obj("list").arr.map(e => read[Data](e)).toList))
              else if json.obj.get("int").isDefined then Data.I(json.obj("int").num.toLong)
              else if json.obj.get("bytes").isDefined then
                  Data.B(ByteString.fromHex(json.obj("bytes").str))
              else throw new Exception("Invalid Data")
        )
    }

    /** CBOR encoder for the [[Data]] type. The encoding and decoding logic is based on the
      * [[https://github.com/IntersectMBO/plutus/blob/507cab8cf6192e5143f12be5b2d3005a2928dc6d/plutus-core/plutus-core/src/PlutusCore/Data.hs#L146 Cardano node implementation]].
      *
      * The reference has no limit on the nesting depth, so the encoder does not recurse: it keeps
      * the arrays and maps it has opened on the heap. Borer's validation still stops at 1000
      * nesting levels; [[scalus.serialization.cbor.Cbor.encode]] writes without it.
      */
    given dataCborEncoder: Encoder[Data] with

        /** A non-empty array being written: the items still to write. */
        private final class OpenArray(var rest: PList[Data])

        /** A non-empty map being written: the entries still to write. */
        private final class OpenMap(var rest: PList[(Data, Data)])

        override def write(writer: borer.Writer, data: Data): borer.Writer = {
            data match
                case I(value) => writeChunkedBigInt(writer, value)
                case B(value) => writeChunkedByteArray(writer, value.bytes)
                case _        => writeNested(writer, data)
            writer
        }

        private def writeChunkedByteArray(writer: borer.Writer, value: Array[Byte]): Unit = {
            if value.length <= 64 then writer.writeBytes(value)
            else writer.writeBytesIterator(value.grouped(64))
        }

        // The byte string of a big number: `x` for tag 2, `-1 - x` for tag 3
        private def bigNumBytes(x: BigInt): Array[Byte] = {
            val bytes = x.toByteArray
            if x.signum < 0 then bytes.mapInPlace(b => (~b.toInt).toByte) // basically, -1 - x
            // in Java first byte CAN be zero (for sign bit), in Haskell it can not
            if bytes.head == 0 then bytes.tail else bytes
        }

        private def writeBigNumTag(writer: borer.Writer, x: BigInt): Unit =
            writer.writeTag(if x.signum < 0 then Tag.NegativeBigNum else Tag.PositiveBigNum)

        private def writeChunkedBigInt(writer: borer.Writer, x: BigInt): Unit = {
            // use the default BigInt serialization, it works same as in the Cardano node
            if x.bitLength <= 64 then writer.write(x)
            else
                // otherwise, chunk the bytes
                writeBigNumTag(writer, x)
                writeChunkedByteArray(writer, bigNumBytes(x))
        }

        private def writeConstrTag(writer: borer.Writer, constr: BigInt): Unit = {
            if 0 <= constr && constr < 7 then writer.writeTag(Other((121 + constr).toLong))
            else if 7 <= constr && constr < 128 then
                writer.writeTag(Other((1280 + (constr - 7)).toLong))
            else
                writer.writeTag(Other(102))
                writer.writeArrayHeader(2)
                writeConstrIndex(writer, constr)
        }

        // The index of a tag-102 `Constr` is a CBOR *unsigned* 64-bit integer, so writing it
        // as a signed Long turned everything from 2^63 up into a negative integer - different
        // bytes, and a different `serialiseData` hash, than the node produces. Borer's BigInt
        // encoder picks the right CBOR major type within 64 bits. Beyond them the reference
        // writes the index with cborg's `encodeInteger`: tag 2 or 3 and ONE definite byte
        // string, not chunked like the integer of `I`, and without the sign byte `00` that
        // borer's encoder adds.
        private def writeConstrIndex(writer: borer.Writer, x: BigInt): Unit = {
            if x.bitLength <= 64 then writer.write(x)
            else
                writeBigNumTag(writer, x)
                writer.writeBytes(bigNumBytes(x))
        }

        /** Writes a list, a map or a `Constr`, and all it holds. What is still to write is kept on
          * the heap, innermost last: the open arrays and maps, and the value of each map entry
          * whose key is being written.
          */
        private def writeNested(writer: borer.Writer, data: Data): Unit = {
            val pending = ArrayBuffer.empty[OpenArray | OpenMap | Data]

            // A list, and the fields of a `Constr`, are an indefinite array when non-empty
            def writeArray(items: PList[Data]): Unit = {
                items match
                    case PList.Nil => writer.writeEmptyArray()
                    case _ =>
                        writer.writeArrayStart()
                        pending += OpenArray(items)
            }

            def writeItem(item: Data): Unit = {
                item match
                    case Constr(constr, args) =>
                        writeConstrTag(writer, constr)
                        writeArray(args)
                    // Cardano stores maps as a list of key-value pairs and allows duplicate keys,
                    // so the map is written entry by entry, not with the `writeMap` of `Writer`
                    case Map(PList.Nil) => writer.writeEmptyMap()
                    case Map(entries) =>
                        writer.writeMapHeader(entries.length.toLong)
                        pending += OpenMap(entries)
                    case Data.List(values) => writeArray(values)
                    case I(value)          => writeChunkedBigInt(writer, value)
                    case B(value)          => writeChunkedByteArray(writer, value.bytes)
            }

            writeItem(data)
            while pending.nonEmpty do
                pending.last match
                    case array: OpenArray =>
                        array.rest match
                            case PList.Cons(head, tail) =>
                                array.rest = tail
                                writeItem(head)
                            case PList.Nil =>
                                pending.dropRightInPlace(1)
                                writer.writeBreak()
                    case map: OpenMap =>
                        map.rest match
                            case PList.Cons((key, value), tail) =>
                                map.rest = tail
                                pending += value // written after the key and all the key holds
                                writeItem(key)
                            case PList.Nil => pending.dropRightInPlace(1)
                    case value: Data =>
                        pending.dropRightInPlace(1)
                        writeItem(value)
        }
    end dataCborEncoder

    /** CBOR decoder for the [[Data]] type. The encoding and decoding logic is based on the
      * [[https://github.com/IntersectMBO/plutus/blob/507cab8cf6192e5143f12be5b2d3005a2928dc6d/plutus-core/plutus-core/src/PlutusCore/Data.hs#L209 Cardano node implementation]].
      *
      * The reference has no limit on the nesting depth, so the decoder does not recurse: it keeps
      * the arrays and maps it has opened on the heap. It reads their items one at a time, as the
      * reference does, so the length in a header allocates nothing before the items are there.
      * Borer's validation still stops at 1000 nesting levels; [[Data.fromCbor]] and
      * [[scalus.serialization.cbor.Cbor.decode]] read without that limit.
      */
    given dataCborDecoder: Decoder[Data] with

        private val twoTo64: BigInt = BigInt(1) << 64

        private val maxCborByteArraySize = 64

        /** An array or a map being read. */
        private sealed abstract class Open {

            /** Whether all its items are read. Reads the break that ends an indefinite one. */
            def isComplete(r: Reader): Boolean

            def add(item: Data): Unit

            /** The value read, once it is complete. */
            def close(r: Reader): Data
        }

        /** A list, or the fields of a `Constr`, being read: the items read so far, last first. */
        private final class OpenArray(
            // the index of the `Constr`, or null for a list
            constr: BigInt | Null,
            // a tag-102 `Constr` in an indefinite array: a break follows the fields
            closingBreak: Boolean,
            // the items still to read, or -1 for an indefinite array
            private var remaining: Long
        ) extends Open {
            private var items: PList[Data] = PList.Nil

            def isComplete(r: Reader): Boolean = isEnd(r, remaining)

            def add(item: Data): Unit = {
                items = PList.Cons(item, items)
                if remaining > 0 then remaining -= 1
            }

            def close(r: Reader): Data = {
                constr match
                    case null => Data.List(items.reverse)
                    case constr: BigInt =>
                        if closingBreak && !r.tryReadBreak() then
                            r.unexpectedDataItem(expected = "Break")
                        Constr(constr, items.reverse)
            }
        }

        /** A map being read: the entries read so far, last first, and the key of the entry being
          * read.
          *
          * Cardano stores a map as a list of key-value pairs and allows duplicate keys, so the
          * entries are kept in order, duplicates and all.
          */
        private final class OpenMap(private var remaining: Long) extends Open {
            private var entries: PList[(Data, Data)] = PList.Nil
            private var key: Data = null

            // a break between a key and its value is not the end of the map
            def isComplete(r: Reader): Boolean = (key eq null) && isEnd(r, remaining)

            def add(item: Data): Unit = {
                if key eq null then key = item
                else
                    entries = PList.Cons((key, item), entries)
                    key = null
                    if remaining > 0 then remaining -= 1
            }

            def close(r: Reader): Data = Map(entries.reverse)
        }

        override def read(r: Reader): Data = {
            readItem(r) match
                case value: Data => value
                case open: Open  => readNested(r, open)
        }

        /** Reads the items of `outermost` and all they hold. What is open is kept on the heap,
          * innermost last.
          */
        private def readNested(r: Reader, outermost: Open): Data = {
            val open = ArrayBuffer[Open](outermost)
            var result: Data = null
            while result eq null do
                val innermost = open.last
                if innermost.isComplete(r) then
                    open.dropRightInPlace(1)
                    val value = innermost.close(r)
                    if open.isEmpty then result = value else open.last.add(value)
                else
                    readItem(r) match
                        case value: Data  => innermost.add(value)
                        case nested: Open => open += nested
            result
        }

        // whether an array or a map with `remaining` items to read, or -1 if indefinite, is read
        private def isEnd(r: Reader, remaining: Long): Boolean = {
            if remaining < 0 then r.tryReadBreak() else remaining == 0
        }

        /** Reads a value that holds no other, or opens an array or a map. */
        private def readItem(r: Reader): Data | Open = {
            r.dataItem() match {
                case DI.Int | DI.Long | DI.OverLong => I(Decoder.forBigInt.read(r))
                case DI.Bytes | DI.BytesStart => B(ByteString.unsafeFromArray(readBoundedBytes(r)))
                case DI.ArrayStart | DI.ArrayHeader => openArray(r, null, closingBreak = false)
                case DI.MapHeader                   => OpenMap(r.readMapHeader())
                case DI.MapStart =>
                    r.readMapStart()
                    OpenMap(-1)
                case DI.Tag => readTagged(r)
                case _      => r.unexpectedDataItem(s"Allowed Data Item")
            }
        }

        /** Reads a big number, or opens the fields of a `Constr`. */
        private def readTagged(r: Reader): Data | Open = {
            val tagStart = r.cursor
            r.readTag() match {
                case Other(102) =>
                    // Exactly two elements, definite- or indefinite-length, as in
                    // `decodeConstrExtended`. The length used to be read and discarded,
                    // which let a three-element array through as a two-element list.
                    val indefinite = r.hasArrayStart
                    if indefinite then r.readArrayStart()
                    else if !r.tryReadArrayHeader(2) then
                        r.unexpectedDataItem(expected = "Array-Header(2)")
                    val i = readConstrIndex(r)
                    openArray(r, i, closingBreak = indefinite)
                case Other(value) if 121 <= value && value < 128 =>
                    openArray(r, BigInt(value - 121), closingBreak = false)
                case Other(value) if 1280 <= value && value < 1401 =>
                    openArray(r, BigInt(value - 1280 + 7), closingBreak = false)
                case tag @ (PositiveBigNum | NegativeBigNum) =>
                    // cborg takes a bignum only from the one-byte heads `c2` and `c3`. Behind a
                    // wider head, such as `d8 02`, tag 2 is a `Constr` tag that the reference does
                    // not know. Borer reads the same tag from both heads, but then the payload
                    // starts more than one byte after it. This needs a reader that parses the
                    // bytes as it goes, as every Scalus entry point does: one that replays items,
                    // from a borer stash or from a DOM, does not move its cursor, so the head is
                    // not seen and not checked.
                    r.dataItem()
                    if r.cursor - tagStart > 1 then
                        r.unexpectedDataItem(
                          expected = "Bignum Tag in a one-byte head",
                          actual = s"$tag in a ${r.cursor - tagStart}-byte head"
                        )
                    val magnitude = BigInteger(1, readBoundedBytes(r))
                    I(if tag == PositiveBigNum then magnitude else magnitude.not)
                case tag =>
                    r.unexpectedDataItem(s"Allowed Data Constr Tag or CBOR BigNum Tag, got $tag")
            }
        }

        /** Opens the array of a list or of the fields of a `Constr`. */
        private def openArray(
            r: Reader,
            constr: BigInt | Null,
            closingBreak: Boolean
        ): OpenArray = {
            if r.hasArrayStart then
                r.readArrayStart()
                OpenArray(constr, closingBreak, -1)
            else if r.hasArrayHeader then OpenArray(constr, closingBreak, r.readArrayHeader())
            else r.unexpectedDataItem(expected = "Array")
        }

        // read sized bytes bounded by 64 bytes
        private def readBoundedSizedBytes(r: Reader): Array[Byte] = {
            val bytes = r.readSizedBytes()(using ByteAccess.ForByteArray)
            if bytes.length > maxCborByteArraySize then
                r.overflow(s"Bytes chunk ${bytes.length} must be <= 64 bytes")
            bytes
        }

        // read bytes (sized or indefinite) where chunks are bounded by 64 bytes
        private def readBoundedBytes(r: Reader): Array[Byte] = {
            r.dataItem() match
                case DI.Bytes => readBoundedSizedBytes(r)
                case DI.BytesStart =>
                    r.readBytesStart()
                    // Copies each chunk as one block; a generic ArrayBuffer[Byte] copies byte by
                    // byte.
                    val acc = new ArrayBuilder.ofByte
                    while !r.tryReadBreak() do acc.addAll(readBoundedSizedBytes(r))
                    acc.result()
                case _ => r.unexpectedDataItem(expected = "Bytes or BytesStart")
        }

        // The constructor index of the tag-102 form is a CBOR unsigned 64-bit integer, so
        // 2^63 and above reach us as borer OverLongs and have to be widened without a sign.
        // Anything else - a negative integer, or a bignum above the range - is rejected, as
        // the node's `decodeWord64` rejects it.
        private def readConstrIndex(r: Reader): BigInt = {
            r.dataItem() match
                case DI.Int | DI.Long =>
                    val i = r.readLong()
                    if i < 0 then r.unexpectedDataItem(expected = "unsigned Constr index")
                    BigInt(i)
                case DI.OverLong =>
                    if r.overLongNegative then
                        r.unexpectedDataItem(expected = "unsigned Constr index")
                    BigInt(r.readOverLong()) + twoTo64
                case _ => r.unexpectedDataItem(expected = "unsigned Constr index")
        }
    end dataCborDecoder

    extension [A <: Data](a: A)
        /** Encode a [[Data]] value to JSON */
        def toJson: String = writeToString[Data](a)(using dataJsonCodec)

        /** Encode a [[Data]] value to indented JSON */
        def toJsonIndented(indent: Int): String =
            writeToString[Data](a, WriterConfig.withIndentionStep(indent))(using dataJsonCodec)

    extension [A <: Data](a: A)
        /** Encode a [[Data]] value to CBOR. The nesting depth of the value is not limited.
          */
        def toCbor: Array[Byte] = ScalusCbor.encode[Data](a)

        /** Encode a [[Data]] value to CBOR [[ByteString]]
          */
        def toCborByteString: ByteString = ScalusCbor.encodeToByteString[Data](a)

    extension (inline data: Data)
        inline def field[A](inline expr: A => Any): Data = fieldAsData(expr)(data)
        inline def toConstr: BuiltinPair[BigInt, scalus.uplc.builtin.BuiltinList[Data]] =
            Builtins.unConstrData(data)
        inline def toMap: BuiltinList[BuiltinPair[Data, Data]] = Builtins.unMapData(data)
        inline def toList: BuiltinList[Data] = Builtins.unListData(data)
        inline def toI: BigInt = Builtins.unIData(data)
        inline def toBigInt: BigInt = Builtins.unIData(data)
        inline def toB: ByteString = Builtins.unBData(data)
        inline def toByteString: ByteString = Builtins.unBData(data)

    /** Decode a [[Data]] value from JSON */
    def fromJson(json: String): Data = readFromString[Data](json)(using dataJsonCodec)

    /** Encode a [[Data]] value to JSON */
    def toJson(data: Data, indent: Int = -1): String =
        if indent < 0 then writeToString[Data](data)(using dataJsonCodec)
        else writeToString[Data](data, WriterConfig.withIndentionStep(indent))(using dataJsonCodec)

    /** Decode a [[Data]] value from CBOR. The nesting depth of the value is not limited. */
    def fromCbor(bytes: Array[Byte]): Data = ScalusCbor.decode[Data](bytes)

    /** Decode a [[Data]] value from CBOR. The nesting depth of the value is not limited. */
    def fromCbor(is: InputStream): Data =
        Cbor.decode(is).withConfig(ScalusCbor.decodingConfig).to[Data].value

    /** Decode a [[Data]] value from CBOR. The nesting depth of the value is not limited. */
    def fromCbor(bs: ByteString): Data = ScalusCbor.decode[Data](bs.bytes)

    /** Tries to decode a value of type `T` from [[Data]]
      * @return
      *   `Success(value)` if decoding was successful, `Failure(exception)` otherwise
      */
    def tryFromData[T](d: Data)(using fd: FromData[T]): Try[T] = Try(fd(d))

    given Flat[Data] with
        def bitSize(a: Data): Int =
            summon[Flat[Array[Byte]]].bitSize(ScalusCbor.encode(a))

        def encode(a: Data, encode: EncoderState): Unit =
            flat.encode(ScalusCbor.encode(a), encode)

        def decode(decode: DecoderState): Data =
            val bytes = summon[Flat[Array[Byte]]].decode(decode)
            ScalusCbor.decode[Data](bytes)
    end given

    /** Pretty prints Data in debug format with rainbow brackets: `<constr, [args]>`, `{k: v}`,
      * `[v]`, `123`, `"hex"`.
      *
      * A list, map or `Constr` nested more than [[maxDepth]] levels deep is printed as `...`.
      * paiges lays out nested groups with a recursion over the nesting, and in memory that grows
      * faster than the depth: 10000 levels exhaust a 3 GB heap. The document is built without
      * recursion.
      */
    given Pretty[Data] with

        /** The nesting printed in full. */
        private val maxDepth = 100

        /** A list, a map or a `Constr` being printed: the documents of what it holds so far. */
        private final class Open(val data: Data, val depth: Int, val items: Iterator[Data]) {
            val docs: ArrayBuffer[Doc] = ArrayBuffer.empty
        }

        def pretty(a: Data, style: Style): Doc = {
            // the document of a value that holds no other, or what is to print of one that does
            def start(data: Data, depth: Int): Doc | Open = {
                data match
                    case I(value) => lit(str(value), style)
                    case B(value) => lit(char('"') + text(value.toHex) + char('"'), style)
                    case _ if depth >= maxDepth => text("...")
                    case Constr(_, args)        => Open(data, depth, args.toScalaList.iterator)
                    case Map(entries) =>
                        val keysAndValues =
                            entries.toScalaList.iterator.flatMap((k, v) => Seq(k, v))
                        Open(data, depth, keysAndValues)
                    case Data.List(values) => Open(data, depth, values.toScalaList.iterator)
            }

            def close(open: Open): Doc = {
                open.data match
                    case Constr(constr, _) =>
                        val argsDoc = fill(comma + space, open.docs)
                        val inner = lit(str(constr), style) + comma &
                            rainbowBracket(argsDoc, '[', ']', open.depth + 1, style)
                        rainbowBracket(inner, '<', '>', open.depth, style).grouped
                    case Map(_) =>
                        val entries =
                            open.docs.grouped(2).map(kv => kv(0) + char(':') + space + kv(1))
                        rainbowBracket(
                          fill(comma + space, entries.toList),
                          '{',
                          '}',
                          open.depth,
                          style
                        ).grouped
                    case _ =>
                        rainbowBracket(
                          fill(comma + space, open.docs),
                          '[',
                          ']',
                          open.depth,
                          style
                        ).grouped
            }

            start(a, 0) match
                case doc: Doc        => doc
                case outermost: Open =>
                    // what is open, innermost last
                    val open = ArrayBuffer(outermost)
                    var result: Doc = null
                    while result eq null do
                        val innermost = open.last
                        if innermost.items.hasNext then
                            start(innermost.items.next(), innermost.depth + 1) match
                                case doc: Doc     => innermost.docs += doc
                                case nested: Open => open += nested
                        else
                            open.dropRightInPlace(1)
                            val doc = close(innermost)
                            if open.isEmpty then result = doc else open.last.docs += doc
                    result
        }
}

/** The shape of [[Data]]'s standard JSON encoding, as something `JsonCodecMaker` can derive.
  *
  * Exactly one group of fields is set for any given value: `constructor` with `fields`, or one of
  * `map`, `list`, `int`, `bytes`. `transientNone` is on by default, so the unset ones are simply
  * not written, which is what produces the single-key objects the format calls for. Field order
  * here is the order they are written in, so `constructor` must precede `fields`.
  */
private case class DataJson(
    constructor: Option[BigInt] = None,
    fields: Option[List[DataJson]] = None,
    map: Option[List[DataJsonMapEntry]] = None,
    list: Option[List[DataJson]] = None,
    int: Option[BigInt] = None,
    bytes: Option[String] = None
)

private case class DataJsonMapEntry(k: DataJson, v: DataJson)

private object DataJson {

    /** `transientEmpty` is off because an empty `fields`, `list` or `map` must still be written:
      * `{"constructor":1,"fields":[]}`, not `{"constructor":1}`.
      *
      * The digit limit replaces jsoniter's default of 308, which is too small for this domain and
      * would reject values this codec's own writer emits: `expModInteger` exists to work on
      * RSA-sized moduli, and a 2048-bit one already has 617 digits. A bound is kept rather than
      * removed because `fromJson` parses input the caller does not control; 65536 digits is far
      * more than any integer that could reach a script inside a transaction, itself limited to
      * about 16 KB.
      */
    val codec: JsonValueCodec[DataJson] = JsonCodecMaker.make(
      CodecMakerConfig
          .withAllowRecursiveTypes(true)
          .withTransientEmpty(false)
          .withBigIntDigitsLimit(65536)
    )

    /** What is still to write of a value after its start: the end of an object or an array, a key,
      * or a map entry.
      */
    private enum Step {
        case EndObject, EndArray
        case Key(name: String)
        case Entry(key: Data, value: Data)
    }

    /** Writes `data` as [[codec]] writes the [[DataJson]] that mirrors it, call for call, so the
      * output is the same. It does not recurse over the nesting: what is still to write is kept on
      * the heap, the next one last.
      */
    def write(data: Data, out: JsonWriter): Unit = {
        val pending = ArrayBuffer[Data | Step](data)

        // opens the array under the key just written; the items, the array and the object follow
        def writeItems(items: List[Data | Step]): Unit = {
            out.writeArrayStart()
            pending += Step.EndObject
            pending += Step.EndArray
            items.reverseIterator.foreach(pending += _)
        }

        while pending.nonEmpty do
            pending.remove(pending.length - 1) match {
                case Step.EndObject => out.writeObjectEnd()
                case Step.EndArray  => out.writeArrayEnd()
                case Step.Key(name) => out.writeNonEscapedAsciiKey(name)
                case Step.Entry(key, value) =>
                    out.writeObjectStart()
                    out.writeNonEscapedAsciiKey("k")
                    pending += Step.EndObject
                    pending += value
                    pending += Step.Key("v")
                    pending += key
                case Data.Constr(constructor, args) =>
                    out.writeObjectStart()
                    out.writeNonEscapedAsciiKey("constructor")
                    out.writeVal(constructor)
                    out.writeNonEscapedAsciiKey("fields")
                    writeItems(args.toScalaList)
                case Data.Map(entries) =>
                    out.writeObjectStart()
                    out.writeNonEscapedAsciiKey("map")
                    writeItems(entries.toScalaList.map((key, value) => Step.Entry(key, value)))
                case Data.List(values) =>
                    out.writeObjectStart()
                    out.writeNonEscapedAsciiKey("list")
                    writeItems(values.toScalaList)
                case Data.I(value) =>
                    out.writeObjectStart()
                    out.writeNonEscapedAsciiKey("int")
                    out.writeVal(value)
                    out.writeObjectEnd()
                case Data.B(value) =>
                    out.writeObjectStart()
                    out.writeNonEscapedAsciiKey("bytes")
                    out.writeVal(value.toHex)
                    out.writeObjectEnd()
            }
    }

    def toData(json: DataJson, in: JsonReader): Data =
        if json.constructor.isDefined then
            val fields = json.fields.getOrElse(Nil).map(toData(_, in))
            Data.Constr(json.constructor.get, PList.from(fields))
        else if json.map.isDefined then
            val entries = json.map.get.map(entry => toData(entry.k, in) -> toData(entry.v, in))
            Data.Map(PList.from(entries))
        else if json.list.isDefined then Data.List(PList.from(json.list.get.map(toData(_, in))))
        else if json.int.isDefined then Data.I(json.int.get)
        else if json.bytes.isDefined then Data.B(ByteString.fromHex(json.bytes.get))
        else in.decodeError("not a Data value: no constructor, map, list, int or bytes key")
}
