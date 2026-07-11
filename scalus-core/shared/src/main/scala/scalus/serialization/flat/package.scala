package scalus.serialization

package object flat:
    case class Natural(n: BigInt)

    def byteAsBitString(b: Byte): String =
        String.format("%8s", Integer.toBinaryString(b & 0xff)).replace(' ', '0')

    def encode[A: Flat](a: A, enc: EncoderState): Unit = summon[Flat[A]].encode(a, enc)
    def decode[A: Flat](dec: DecoderState): A = summon[Flat[A]].decode(dec)

    trait Flat[A]:
        def bitSize(a: A): Int
        def encode(a: A, encode: EncoderState): Unit
        def decode(decode: DecoderState): A

    given Flat[Unit] with
        def bitSize(a: Unit): Int = 0
        def encode(a: Unit, encode: EncoderState): Unit = {}
        def decode(decode: DecoderState): Unit = ()

    given Flat[Boolean] with
        def bitSize(a: Boolean): Int = 1
        def encode(a: Boolean, encode: EncoderState): Unit = encode.bits(1, if a then 1 else 0)
        def decode(decode: DecoderState): Boolean =
            decode.bits8(1) match
                case 0 => false
                case 1 => true

    type Uint8Array = Array[Byte]

    /** Prealigned Arrays of bytes PreAligned a ≡ PreAligned {preFiller :: Filler, preValue :: a}
      *
      * Filler ≡ FillerBit Filler \| FillerEnd
      *
      * Array v = A0 \| A1 v (Array v) \| A2 v v (Array v) ... \| A255 ... (Array v)
      */
    class ArrayByteFlat extends Flat[Array[Byte]]:
        def bitSize(a: Array[Byte]): Int = byteArraySize(a)
        def encode(a: Array[Byte], enc: EncoderState): Unit =
            enc.filler() // pre-align
            var numElems = a.length
            var inx = 0
            var blkLen = Math.min(255, numElems)

            while blkLen > 0 do
                enc.bits(8, blkLen.toByte)
                var i = 0
                while i < blkLen do
                    enc.bits(8, a(inx + i))
                    i += 1

                numElems -= blkLen
                inx += blkLen
                blkLen = Math.min(255, numElems)
            enc.bits(8, 0)

        def decode(decode: DecoderState): Array[Byte] =
            decode.filler()
            val size =
                var numElems = decode.buffer(decode.currPtr) & 0xff
                var decoderOffset = numElems + 1
                var size = numElems
                // calculate size
                while numElems == 255 do
                    numElems = decode.buffer(decode.currPtr + decoderOffset) & 0xff
                    size += numElems
                    decoderOffset += numElems + 1
                size

            val result = new Array[Byte](size)
            var numElems = decode.buffer(decode.currPtr) & 0xff
            decode.currPtr += 1
            var resultOffset = 0
            while numElems > 0 do
                Array.copy(decode.buffer, decode.currPtr, result, resultOffset, numElems)
                decode.currPtr += numElems
                resultOffset += numElems
                numElems = decode.buffer(decode.currPtr) & 0xff
                decode.currPtr += 1
            result
    given Flat[Array[Byte]] = ArrayByteFlat()

    given Flat[Int] with
        def bitSize(a: Int): Int = word7BytesCount(zigZag(a) & 0xffffffffL) * 8

        // Encoded as: data NonEmptyList = Elem Word7 | Cons Word7 NonEmptyList
        def encode(a: Int, encode: EncoderState): Unit =
            val vs = word7Bytes(zigZag(a) & 0xffffffffL)
            var i = 0
            while i < vs.length do
                encode.bits(8, vs(i))
                i += 1

        def decode(decode: DecoderState): Int =
            var w = decode.bits8(8)
            var r = 0
            var shl = 0
            while (w & 0x80) != 0 do
                r = r | ((w & 0x7f) << shl)
                shl += 7
                w = decode.bits8(8)

            r = r | ((w & 0x7f) << shl)
            zagZig(r)

    given Flat[Long] with
        def bitSize(a: Long): Int = word7BytesCount(zigZag(a)) * 8

        // Encoded as: data NonEmptyList = Elem Word7 | Cons Word7 NonEmptyList
        def encode(a: Long, encode: EncoderState): Unit =
            val vs = word7Bytes(zigZag(a))
            var i = 0
            while i < vs.length do
                encode.bits(8, vs(i))
                i += 1
        def decode(decode: DecoderState): Long =
            var w = decode.bits8(8)
            var r = 0L
            var shl = 0
            while (w & 0x80) != 0 do
                r = r | ((w & 0x7f).toLong << shl)
                shl += 7
                w = decode.bits8(8)

            r = r | ((w & 0x7f).toLong << shl)
            zagZig(r)

    given Flat[BigInt] with
        def bitSize(a: BigInt): Int = word7BytesCount(zigZag(a)) * 8

        // Encoded as: data NonEmptyList = Elem Word7 | Cons Word7 NonEmptyList
        def encode(a: BigInt, encode: EncoderState): Unit =
            val vs = word7Bytes(zigZag(a))
            var i = 0
            while i < vs.length do
                encode.bits(8, vs(i))
                i += 1
        def decode(decode: DecoderState): BigInt =
            var w = decode.bits8(8)
            var r = BigInt(0)
            var shl = 0
            while (w & 0x80) != 0 do
                r = r | (BigInt(w & 0x7f) << shl)
                shl += 7
                w = decode.bits8(8)

            r = r | (BigInt(w & 0x7f) << shl)
            zagZig(r)

    given Flat[Natural] with
        def bitSize(a: Natural): Int = word7BytesCount(a.n) * 8

        // Encoded as: data NonEmptyList = Elem Word7 | Cons Word7 NonEmptyList
        def encode(a: Natural, encode: EncoderState): Unit =
            val vs = word7Bytes(a.n)
            var i = 0
            while i < vs.length do
                encode.bits(8, vs(i))
                i += 1

        def decode(decode: DecoderState): Natural =
            var w = decode.bits8(8)
            var r = BigInt(0)
            var shl = 0
            while (w & 0x80) != 0 do
                r = r | (BigInt(w & 0x7f) << shl)
                shl += 7
                w = decode.bits8(8)

            r = r | (BigInt(w & 0x7f) << shl)
            Natural(r)

    given Flat[String] with
        def bitSize(a: String): Int =
            summon[Flat[Array[Byte]]].bitSize(a.getBytes("UTF-8"))

        def encode(a: String, encode: EncoderState): Unit =
            summon[Flat[Array[Byte]]].encode(a.getBytes("UTF-8"), encode)

        def decode(decode: DecoderState): String =
            val baDecoder = summon[Flat[Array[Byte]]]
            val bytes = baDecoder.decode(decode)
            new String(bytes, "UTF-8")

    def iterableFlat[A: Flat, C <: Iterable[A]](factory: scala.collection.Factory[A, C]): Flat[C] =
        new Flat[C]:
            def bitSize(a: C): Int =
                val flat = summon[Flat[A]]
                a.foldLeft(1)((acc, elem) => acc + flat.bitSize(elem) + 1)

            def encode(a: C, encode: EncoderState): Unit =
                val flat = summon[Flat[A]]
                a.foreach { elem =>
                    encode.bits(1, 1)
                    flat.encode(elem, encode)
                }
                encode.bits(1, 0)

            def decode(decode: DecoderState): C =
                val flat = summon[Flat[A]]
                val builder = factory.newBuilder
                while decode.bits8(1) == 1 do {
                    val a = flat.decode(decode)
                    builder.addOne(a)
                }
                builder.result()

    given listFlat[A: Flat]: Flat[List[A]] = iterableFlat[A, List[A]](List)
    // Arrays use the same flat encoding as lists (per CIP-0138)
    given indexedSeqFlat[A: Flat]: Flat[IndexedSeq[A]] = iterableFlat[A, IndexedSeq[A]](IndexedSeq)

    given pairFlat[A: Flat, B: Flat]: Flat[(A, B)] with
        def bitSize(a: (A, B)): Int =
            summon[Flat[A]].bitSize(a._1) + summon[Flat[B]].bitSize(a._2)

        def encode(a: (A, B), encode: EncoderState): Unit =
            summon[Flat[A]].encode(a._1, encode)
            summon[Flat[B]].encode(a._2, encode)

        def decode(decode: DecoderState): (A, B) =
            val a = summon[Flat[A]].decode(decode)
            val b = summon[Flat[B]].decode(decode)
            (a, b)

    /** Number of bytes in the variable-length 7-bit encoding of `n`, treating `n` as an unsigned
      * 64-bit value.
      */
    def word7BytesCount(n: Long): Int =
        if n == 0 then 1
        else (63 - java.lang.Long.numberOfLeadingZeros(n)) / 7 + 1

    /** Encodes `n` (treated as an unsigned 64-bit value) as a variable-length byte array: 7 payload
      * bits per byte, least-significant group first, high bit set on every byte except the last.
      * This is the byte layout of flat's
      * `data NonEmptyList = Elem Word7 | Cons Word7 NonEmptyList`.
      */
    def word7Bytes(n: Long): Array[Byte] =
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

    /** Number of bytes in the variable-length 7-bit encoding of a non-negative `n`. */
    def word7BytesCount(n: BigInt): Int =
        require(n >= 0, s"word7BytesCount: input must be non-negative, got $n")
        if n == 0 then 1 else (n.bitLength - 1) / 7 + 1

    /** Encodes a non-negative `n` as a variable-length byte array, as in [[word7Bytes(n:Long)*]].
      */
    def word7Bytes(n: BigInt): Array[Byte] =
        require(n >= 0, s"word7Bytes: input must be non-negative, got $n")
        if n.isValidLong then word7Bytes(n.toLong)
        else
            val size = word7BytesCount(n)
            val result = new Array[Byte](size)
            var v = n
            var i = 0
            while i < size - 1 do
                result(i) = ((v & 0x7f).toInt | 0x80).toByte
                v = v >> 7
                i += 1
            result(size - 1) = (v & 0x7f).toByte
            result

    @deprecated("Use word7Bytes instead", "0.18.2")
    def w7l(n: Long): List[Byte] = word7Bytes(n).toList

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

    @deprecated("Use word7Bytes instead", "0.18.2")
    def w7l(n: BigInt): List[Byte] = word7Bytes(n).toList

    def zigZag(x: BigInt) = if x >= 0 then x << 1 else -(x << 1) - 1
    def zagZig(u: BigInt) = u >> 1 ^ -(u & 1)

    private def arrayBlocks(len: Int): Int =
        val d = len / 255
        d + (if len % 255 == 0 then 0 else 1)

    private def byteArraySize(arr: Uint8Array): Int =
        val numBytes = arr.length + arrayBlocks(arr.length) + 1 + 1 // +1 for pre-align, +1 for end
        8 * numBytes

    class EncoderState(bufferSize: Int):
        val buffer: Array[Byte] = new Array(bufferSize)
        var nextPtr: Int = 0
        var usedBits: Int = 0
        var currentByte: Int = 0

        /** The bytes encoded so far, including a trailing partially-filled byte if any (unused low
          * bits are zero).
          */
        def result: Array[Byte] =
            val len = if usedBits == 0 then nextPtr else nextPtr + 1
            val result = new Array[Byte](len)
            System.arraycopy(buffer, 0, result, 0, nextPtr)
            if usedBits != 0 then result(nextPtr) = currentByte.toByte
            result

        override def toString: String =
            s"""EncoderState(nextPtr:$nextPtr,usedBits:$usedBits,currentByte:$currentByte,buffer:${buffer
                    .map(byteAsBitString)
                    .mkString(",")})"""

        // add indicated Int of bits (up to ? bits)
        def bits(numBits: Int, value: Byte): Unit =
            this.usedBits += numBits
            val unusedBits = 8 - this.usedBits
            if unusedBits > 0 then this.currentByte |= value << unusedBits
            else if unusedBits == 0 then
                this.currentByte |= value
                this.nextWord()
            else
                val used = (-unusedBits).toByte
                this.currentByte |= (value & 0xff) >>> used
                this.nextWord()
                this.currentByte = value << (8 - used)
                this.usedBits = used

        def nextWord(): Unit =
            this.buffer(this.nextPtr) = this.currentByte.toByte
            this.nextPtr += 1
            this.currentByte = 0
            this.usedBits = 0

        def filler(): Unit =
            this.currentByte |= 1;
            nextWord()

        def bitPosition(): Int =
            this.nextPtr * 8 + this.usedBits

    class DecoderState(
        /** The buffer that contains a sequence of flat-encoded values */
        val buffer: Uint8Array
    ):

        /** Pointer to the current byte being decoded [0..buffer.byteLength) */
        var currPtr: Int = 0

        /** Number of already decoded bits in the current byte [0..7] */
        var usedBits: Int = 0

        val hashConsed: HashConsed.State = HashConsed.State.empty

        override def toString: String =
            s"""DecoderState(currPtr:$currPtr,usedBits:$usedBits,buffer:${buffer
                    .map(byteAsBitString)
                    .mkString(",")})"""

        /** Decode up to 8 bits
          * @param numBits
          *   the Int of bits to decode [0..8]
          */
        def lookupBits8(numBits: Int): Byte =
            if numBits < 0 || numBits > 8 then
                throw new RuntimeException("Decoder.bits8: incorrect value of numBits " + numBits)

            this.ensureBits(numBits)
            // usedBits=1 numBits=8 unusedBits=7 leadingZeros=0 unusedBits+leadingZeros=7
            val unusedBits = 8 - this.usedBits
            val leadingZeros = 8 - numBits
            var r =
                ((this.buffer(this.currPtr) << this.usedBits) & 255) >>> leadingZeros

            if numBits > unusedBits then
                val nextByte: Byte = this.buffer(this.currPtr + 1)
                val lowerBits = (nextByte & 255) >>> (unusedBits + leadingZeros)
                r = r | lowerBits

            (r & 255).toByte

        def bits8(numBits: Int): Byte =
            val r = lookupBits8(numBits)
            this.dropBits(numBits)
            r

        def filler(): Unit =
            while this.bits8(1) == 0 do ()

        def ensureBits(requiredBits: Int): Unit =
            if requiredBits > this.availableBits() then
                throw new RuntimeException(
                  "DecoderState: Not enough data available: " + this.toString
                )

        def bitPosition(): Int = this.currPtr * 8 + this.usedBits

        private def availableBits(): Int = 8 * this.availableBytes() - this.usedBits

        // Available bytes, ignoring used bits
        def availableBytes(): Int = this.buffer.length - this.currPtr

        def remainingBytes(): Array[Byte] = {
            require(this.usedBits == 0, "DecoderState.remainingBytes: usedBits must be 0")

            val remaining = new Array[Byte](this.availableBytes())
            System.arraycopy(this.buffer, this.currPtr, remaining, 0, remaining.length)
            remaining
        }

        private def dropBits(numBits: Int): Unit =
            val totUsed = numBits + this.usedBits
            this.usedBits = totUsed % 8
            this.currPtr += totUsed / 8
