package scalus.serialization.flat

import scala.annotation.static

/** JVM-only, Java-facing facade over the flat codec. Methods are `@static` so Java calls
  * `FlatCodec.encodeLong(v)` rather than `FlatCodec$.MODULE$.encodeLong(v)`. Signatures use only
  * `byte[]`/primitive/`String` types so Java never has to name a Scala collection.
  */
final class FlatCodec private ()

object FlatCodec {
    private def encodeWith[A](fl: Flat[A], value: A): Array[Byte] = {
        val enc = EncoderState(fl.bitSize(value) / 8 + 1)
        fl.encode(value, enc)
        enc.result
    }

    @static def encodeLong(v: Long): Array[Byte] = encodeWith(summon[Flat[Long]], v)
    @static def decodeLong(bytes: Array[Byte]): Long =
        summon[Flat[Long]].decode(DecoderState(bytes))

    @static def encodeBytes(v: Array[Byte]): Array[Byte] = encodeWith(summon[Flat[Array[Byte]]], v)
    @static def decodeBytes(bytes: Array[Byte]): Array[Byte] =
        summon[Flat[Array[Byte]]].decode(DecoderState(bytes))

    @static def encodeString(v: String): Array[Byte] = encodeWith(summon[Flat[String]], v)
    @static def decodeString(bytes: Array[Byte]): String =
        summon[Flat[String]].decode(DecoderState(bytes))

    // Low-level varint/zigzag helpers, Java-typed.
    @static def word7Bytes(v: Long): Array[Byte] = scalus.serialization.flat.word7Bytes(v)
    @static def zigZag(v: Long): Long = scalus.serialization.flat.zigZag(v)
    @static def zagZig(v: Long): Long = scalus.serialization.flat.zagZig(v)
}

/** Named accessors for the primitive `Flat` instances, since Java cannot `summon`. */
final class Flats private ()

object Flats {
    @static def longFlat: Flat[Long] = summon[Flat[Long]]
    @static def byteArrayFlat: Flat[Array[Byte]] = summon[Flat[Array[Byte]]]
    @static def stringFlat: Flat[String] = summon[Flat[String]]
}
