package scalus.builtin

import io.bullet.borer
import io.bullet.borer.Tag.{NegativeBigNum, Other, PositiveBigNum}
import io.bullet.borer.{ByteAccess, Cbor, DataItem as DI, Decoder, Encoder, Reader, Tag}
import scalus.Compiler
import scalus.builtin.Data.{B, Constr, FromData, I, Map}
import upickle.default.*

import java.io.InputStream
import java.math.BigInteger
import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Try

private trait DataApi {
    extension (self: Data)
        inline def dataHash: ByteString =
            Builtins.blake2b_256(Builtins.serialiseData(self))

    /** JSON ReadWriter for the [[Data]] type.
      */
    given DataReadWriter: ReadWriter[Data] = {
        given ReadWriter[Data] = DataReadWriter

        readwriter[ujson.Value].bimap(
          {
              case Data.Constr(constr, args) =>
                  ujson.Obj(
                    "constructor" -> writeJs(constr),
                    "fields" -> ujson.Arr(ArrayBuffer.from(args.map(writeJs)))
                  )
              case Data.Map(values) =>
                  ujson.Obj("map" -> ujson.Arr(ArrayBuffer.from(values.map { case (k, v) =>
                      ujson.Obj("k" -> writeJs(k), "v" -> writeJs(v))
                  })))
              case Data.List(values) =>
                  ujson.Obj("list" -> ujson.Arr(ArrayBuffer.from(values.map(writeJs))))
              case Data.I(value) =>
                  val v = if value.isValidLong then writeJs(value.toLong) else writeJs(value)
                  ujson.Obj("int" -> v)
              case Data.B(value) => ujson.Obj("bytes" -> writeJs(value.toHex))
          },
          json =>
              if json.obj.get("constructor").isDefined then
                  Data.Constr(
                    json.obj("constructor").num.toLong,
                    json.obj("fields").arr.map(f => read[Data](f)).toList
                  )
              else if json.obj.get("map").isDefined then
                  Data.Map(
                    json.obj("map")
                        .arr
                        .map { obj =>
                            val k = read[Data](obj.obj("k"))
                            val v = read[Data](obj.obj("v"))
                            k -> v
                        }
                        .toList
                  )
              else if json.obj.get("list").isDefined then
                  Data.List(json.obj("list").arr.map(e => read[Data](e)).toList)
              else if json.obj.get("int").isDefined then Data.I(json.obj("int").num.toLong)
              else if json.obj.get("bytes").isDefined then
                  Data.B(ByteString.fromHex(json.obj("bytes").str))
              else throw new Exception("Invalid Data")
        )
    }

    /** CBOR encoder for the [[Data]] type. The encoding and decoding logic is based on the
      * [[https://github.com/IntersectMBO/plutus/blob/441b76d9e9745dfedb2afc29920498bdf632f162/plutus-core/plutus-core/src/PlutusCore/Data.hs#L72 Cardano node implementation]].
      */
    given dataCborEncoder: Encoder[Data] with
        override def write(writer: borer.Writer, data: Data): borer.Writer =

            def writeChunkedByteArray(value: Array[Byte]) = {
                if value.length <= 64 then writer.writeBytes(value)
                else writer.writeBytesIterator(value.grouped(64))
            }

            def writeChunkedBigInt(x: BigInt) = {
                // use the default BigInt serialization, it works same as in the Cardano node
                if x.bitLength <= 64 then writer.write(x)
                else
                    // otherwise, chunk the bytes
                    val bytes =
                        val bytes = x.toByteArray
                        if x.signum < 0 then
                            bytes.mapInPlace(b => (~b.toInt).toByte) // basically, -1 - x
                        // in Java first byte CAN be zero (for sign bit), in Haskell it can not
                        if bytes.head == 0 then bytes.tail else bytes
                    if x.signum < 0 then writer.writeTag(Tag.NegativeBigNum)
                    else writer.writeTag(Tag.PositiveBigNum)
                    writeChunkedByteArray(bytes)
            }

            data match
                case Constr(constr, args) if 0 <= constr && constr < 7 =>
                    writer.writeTag(Other(121 + constr))
                    writer.writeLinearSeq(args)
                case Constr(constr, args) if 7 <= constr && constr < 128 =>
                    writer.writeTag(Other(1280 + (constr - 7)))
                    writer.writeLinearSeq(args)
                case Constr(constr, args) =>
                    writer.writeTag(Other(102))
                    writer.writeArrayHeader(2)
                    writer.writeLong(constr)
                    writer.writeLinearSeq(args)
                case Map(values)       => writeMap(writer, values)
                case Data.List(values) => writer.writeLinearSeq(values)
                case I(value)          => writeChunkedBigInt(value)
                case B(value)          => writeChunkedByteArray(value.bytes)
        /*
         * Cardano stores maps as a list of key-value pairs
         * Note, that it allows duplicate keys!
         * This is why we don't use the `writeMap` method from `Writer` here
         */
        private def writeMap[A: Encoder, B: Encoder](
            writer: borer.Writer,
            x: Iterable[(A, B)]
        ): borer.Writer =
            if x.nonEmpty then
                val iterator = x.iterator
                writer.writeMapHeader(x.size)
                while iterator.hasNext do
                    val (k, v) = iterator.next()
                    writer.write(k)
                    writer.write(v)
            else writer.writeEmptyMap()
            writer
    end dataCborEncoder

    /** CBOR decoder for the [[Data]] type. The encoding and decoding logic is based on the
      * [[https://github.com/IntersectMBO/plutus/blob/441b76d9e9745dfedb2afc29920498bdf632f162/plutus-core/plutus-core/src/PlutusCore/Data.hs#L72 Cardano node implementation]].
      */
    given dataCborDecoder: Decoder[Data] with

        override def read(r: Reader): Data =

            val maxCborByteArraySize = 64

            // read sized bytes bounded by 64 bytes
            def readBoundedSizedBytes(): Array[Byte] =
                val bytes = r.readSizedBytes()(using ByteAccess.ForByteArray)
                if bytes.length > maxCborByteArraySize then
                    r.overflow(s"Bytes chunk ${bytes.length} must be <= 64 bytes")
                bytes

            // read chunks of 64 bytes
            def readBoundedBytesIndef(): Array[Byte] =
                val acc = new ArrayBuffer[Byte](64)
                while !r.tryReadBreak() do acc ++= readBoundedSizedBytes()
                acc.toArray

            // read bytes (sized or indefinite) where chunks are bounded by 64 bytes
            def readBoundedBytes(): Array[Byte] =
                r.dataItem() match
                    case DI.Bytes      => readBoundedSizedBytes()
                    case DI.BytesStart =>
                        r.readBytesStart()
                        readBoundedBytesIndef()
                    case _ => r.unexpectedDataItem(expected = "Bytes or BytesStart")

            r.dataItem() match
                case DI.Int | DI.Long | DI.OverLong => I(Decoder.forBigInt.read(r))
                case DI.MapHeader | DI.MapStart     => Map(readMap.read(r))
                case DI.ArrayStart | DI.ArrayHeader =>
                    Data.List(Decoder.forArray[Data].read(r).toList)
                case DI.Bytes | DI.BytesStart => B(ByteString.unsafeFromArray(readBoundedBytes()))
                case DI.Tag                   =>
                    r.readTag() match
                        case Other(102) =>
                            val _ = r.readArrayHeader()
                            val i = r.readLong()
                            val args = Decoder.forArray[Data].read(r)
                            Constr(i, args.toList)
                        case Other(value) if 121 <= value && value < 128 =>
                            Constr(value - 121, Decoder.forArray[Data].read(r).toList)
                        case Other(value) if 1280 <= value && value < 1401 =>
                            Constr(value - 1280 + 7, Decoder.forArray[Data].read(r).toList)
                        case PositiveBigNum => I(BigInteger(1, readBoundedBytes()))
                        case NegativeBigNum => I(BigInteger(1, readBoundedBytes()).not)
                        case tag            =>
                            r.unexpectedDataItem(
                              s"Allowed Data Constr Tag or CBOR BigNum Tag, got $tag"
                            )
                case i => r.unexpectedDataItem(s"Allowed Data Item")

        /*
         * Cardano stores maps as a list of key-value pairs
         * Note, that it allows duplicate keys!
         * This is why we don't use the `readMap` method from `Reader` here
         */
        private def readMap[A: Decoder, B: Decoder]: Decoder[immutable.List[(A, B)]] =
            Decoder { r =>
                if r.hasMapHeader then
                    @tailrec def rec(remaining: Int, map: ListBuffer[(A, B)]): ListBuffer[(A, B)] =
                        if remaining > 0 then rec(remaining - 1, map.append((r[A], r[B])))
                        else map

                    val size = r.readMapHeader()
                    if size <= Int.MaxValue then rec(size.toInt, ListBuffer.empty).toList
                    else r.overflow(s"Cannot deserialize Map with size $size (> Int.MaxValue)")
                else if r.hasMapStart then
                    r.readMapStart()
                    val map = ListBuffer.empty[(A, B)]
                    while !r.tryReadBreak() do map.append((r[A], r[B]))
                    map.toList
                else r.unexpectedDataItem(expected = "Map")
            }
    end dataCborDecoder

    extension [A <: Data: Writer](a: A)
        /** Encode a [[Data]] value to JSON */
        inline def toJson: String = write(a)

        /** Encode a [[Data]] value to indented JSON */
        inline def toJsonIndented(indent: Int): String = write(a, indent)

    extension [A <: Data](a: A)
        /** Encode a [[Data]] value to CBOR.
          */
        def toCbor: Array[Byte] = Cbor.encode(a)(using dataCborEncoder).toByteArray

        /** Encode a [[Data]] value to CBOR [[ByteString]]
          */
        def toCborByteString: ByteString = ByteString.fromArray(toCbor)

    extension (inline data: Data)
        inline def field[A](inline expr: A => Any): Data = Compiler.fieldAsData(expr)(data)
        inline def toConstr: BuiltinPair[BigInt, scalus.builtin.BuiltinList[Data]] =
            Builtins.unConstrData(data)
        inline def toMap: BuiltinList[BuiltinPair[Data, Data]] = Builtins.unMapData(data)
        inline def toList: BuiltinList[Data] = Builtins.unListData(data)
        inline def toI: BigInt = Builtins.unIData(data)
        inline def toBigInt: BigInt = Builtins.unIData(data)
        inline def toB: ByteString = Builtins.unBData(data)
        inline def toByteString: ByteString = Builtins.unBData(data)

    /** Decode a [[Data]] value from JSON */
    def fromJson(json: String): Data = read[Data](json)

    /** Encode a [[Data]] value to JSON */
    def toJson(data: Data, indent: Int = -1): String = write(data, indent)

    /** Decode a [[Data]] value from CBOR */
    def fromCbor(bytes: Array[Byte]): Data = Cbor.decode(bytes).to[Data].value

    /** Decode a [[Data]] value from CBOR */
    def fromCbor(is: InputStream): Data = Cbor.decode(is).to[Data].value

    /** Decode a [[Data]] value from CBOR */
    def fromCbor(bs: ByteString): Data = Cbor.decode(bs.bytes).to[Data].value

    /** Tries to decode a value of type `T` from [[Data]]
      * @return
      *   `Success(value)` if decoding was successful, `Failure(exception)` otherwise
      */
    def tryFromData[T](d: Data)(using fd: FromData[T]): Try[T] = Try(fd(d))
}
