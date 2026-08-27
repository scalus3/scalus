package scalus.serialization.flat

/** Java-facing facade over the flat codec, and the home of the scalar helper implementations — the
  * top-level functions of this package (`zigZag`, `word7Bytes`, ...) delegate here.
  *
  * A plain object without a companion class gets static forwarders in its mirror class, so Java
  * calls `FlatCodec.encodeLong(v)` directly on every platform — no `MODULE$`, no `summon`.
  * Signatures use only `byte[]`/primitive/`String` types plus explicit [[Flat]] instances (see
  * [[Flats]]) so Java never has to name a Scala collection or summon a given.
  */
object FlatCodec {
    private def encodeWith[A](fl: Flat[A], value: A): Array[Byte] = {
        val enc = EncoderState(fl.bitSize(value) / 8 + 1)
        fl.encode(value, enc)
        enc.result
    }

    def encodeLong(v: Long): Array[Byte] = encodeWith(summon[Flat[Long]], v)
    def decodeLong(bytes: Array[Byte]): Long =
        summon[Flat[Long]].decode(DecoderState(bytes))

    def encodeBytes(v: Array[Byte]): Array[Byte] = encodeWith(summon[Flat[Array[Byte]]], v)
    def decodeBytes(bytes: Array[Byte]): Array[Byte] =
        summon[Flat[Array[Byte]]].decode(DecoderState(bytes))

    def encodeString(v: String): Array[Byte] = encodeWith(summon[Flat[String]], v)
    def decodeString(bytes: Array[Byte]): String =
        summon[Flat[String]].decode(DecoderState(bytes))

    /** Encodes a value with an explicit instance — Java cannot `summon`; get instances from
      * [[Flats]].
      */
    def encode[A](value: A, fl: Flat[A]): Array[Byte] = encodeWith(fl, value)

    /** Decodes a value with an explicit instance — Java cannot `summon`; get instances from
      * [[Flats]].
      */
    def decode[A](bytes: Array[Byte], fl: Flat[A]): A = fl.decode(DecoderState(bytes))

    /** Renders a byte as its 8-character binary string, e.g. `0x0b` → `"00001011"`. Debug aid for
      * [[EncoderState.toString]]/[[DecoderState.toString]].
      */
    def byteAsBitString(b: Byte): String =
        String.format("%8s", Integer.toBinaryString(b & 0xff)).replace(' ', '0')

    /** Number of bytes in the variable-length 7-bit encoding of `n`, treating `n` as an unsigned
      * 64-bit value.
      */
    def word7BytesCount(n: Long): Int =
        if n == 0 then 1
        else (63 - java.lang.Long.numberOfLeadingZeros(n)) / 7 + 1

    /** Encodes `n` (treated as an unsigned 64-bit value) as a variable-length byte array: 7 payload
      * bits per byte, least-significant group first, high bit set on every byte except the last.
      * This is the byte layout of flat's `data NonEmptyList = Elem Word7 | Cons Word7
      * NonEmptyList`.
      */
    def word7Bytes(n: Long): Array[Byte] = {
        val size = word7BytesCount(n)
        val result = new Array[Byte](size)
        var v = n
        var i = 0
        while i < size - 1 do
            result(i) = ((v & 0x7f) | 0x80).toByte
            v >>>= 7
            i += 1
        result(size - 1) = (v & 0x7f).toByte
        result
    }

    /** ZigZag encoding: maps signed values to unsigned so small magnitudes of either sign get short
      * varint encodings (0 → 0, -1 → 1, 1 → 2, -2 → 3, 2 → 4, ...). Total over the whole Int/Long
      * range: the doubling overflow is intentional and is inverted by [[zagZig]].
      * https://gist.github.com/mfuerstenau/ba870a29e16536fdbaba
      */
    def zigZag(x: Int): Int = (x << 1) ^ (x >> 31)

    /** Inverse of [[zigZag(x:Int)*]]. */
    def zagZig(u: Int): Int = (u >>> 1) ^ -(u & 1)

    /** See [[zigZag(x:Int)*]]. */
    def zigZag(x: Long): Long = (x << 1) ^ (x >> 63)

    /** Inverse of [[zigZag(x:Long)*]]. */
    def zagZig(u: Long): Long = (u >>> 1) ^ -(u & 1)
}

/** Named accessors for the primitive `Flat` instances, since Java cannot `summon`. */
object Flats {
    def booleanFlat: Flat[Boolean] = summon[Flat[Boolean]]
    def intFlat: Flat[Int] = summon[Flat[Int]]
    def longFlat: Flat[Long] = summon[Flat[Long]]
    def byteArrayFlat: Flat[Array[Byte]] = summon[Flat[Array[Byte]]]
    def stringFlat: Flat[String] = summon[Flat[String]]
}
