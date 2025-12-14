package scalus.serialization

import scala.collection.mutable.ListBuffer

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
        def bitSize(a: Int): Int =
            val vs = w7l(zigZag(a))
            vs.length * 8

        // Encoded as: data NonEmptyList = Elem Word7 | Cons Word7 NonEmptyList
        def encode(a: Int, encode: EncoderState): Unit =
            val vs = w7l(zigZag(a))
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
        def bitSize(a: Long): Int =
            val vs = w7l(zigZag(a))
            vs.length * 8

        // Encoded as: data NonEmptyList = Elem Word7 | Cons Word7 NonEmptyList
        def encode(a: Long, encode: EncoderState): Unit =
            val vs = w7l(zigZag(a))
            var i = 0
            while i < vs.length do
                encode.bits(8, vs(i))
                i += 1
        def decode(decode: DecoderState): Long =
            var w = decode.bits8(8)
            var r = 0L
            var shl = 0
            while (w & 0x80) != 0 do
                r = r | ((w & 0x7f) << shl)
                shl += 7
                w = decode.bits8(8)

            r = r | ((w & 0x7f) << shl)
            zagZig(r)

    given Flat[BigInt] with
        def bitSize(a: BigInt): Int =
            val vs = w7l(zigZag(a))
            vs.length * 8

        // Encoded as: data NonEmptyList = Elem Word7 | Cons Word7 NonEmptyList
        def encode(a: BigInt, encode: EncoderState): Unit =
            val vs = w7l(zigZag(a))
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
        def bitSize(a: Natural): Int =
            val vs = w7l(a.n)
            vs.length * 8

        // Encoded as: data NonEmptyList = Elem Word7 | Cons Word7 NonEmptyList
        def encode(a: Natural, encode: EncoderState): Unit =
            val vs = w7l(a.n)
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

    given listFlat[A: Flat]: Flat[List[A]] with
        def bitSize(a: List[A]): Int =
            val flat = summon[Flat[A]]
            a.foldLeft(1)((acc, elem) => acc + flat.bitSize(elem) + 1)

        def encode(a: List[A], encode: EncoderState): Unit =
            val flat = summon[Flat[A]]
            a.foreach { elem =>
                encode.bits(1, 1)
                flat.encode(elem, encode)
            }
            encode.bits(1, 0)

        def decode(decode: DecoderState): List[A] =
            val flat = summon[Flat[A]]
            val result = ListBuffer.empty[A]
            while decode.bits8(1) == 1 do {
                val a = flat.decode(decode)
                result.addOne(a)
            }
            result.toList

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

    // Arrays use the same flat encoding as lists (per CIP-0138)
    given vectorFlat[A: Flat]: Flat[Vector[A]] with
        def bitSize(a: Vector[A]): Int =
            val flat = summon[Flat[A]]
            a.foldLeft(1)((acc, elem) => acc + flat.bitSize(elem) + 1)

        def encode(a: Vector[A], encode: EncoderState): Unit =
            val flat = summon[Flat[A]]
            a.foreach { elem =>
                encode.bits(1, 1)
                flat.encode(elem, encode)
            }
            encode.bits(1, 0)

        def decode(decode: DecoderState): Vector[A] =
            val flat = summon[Flat[A]]
            val result = scala.collection.mutable.ArrayBuffer.empty[A]
            while decode.bits8(1) == 1 do {
                val a = flat.decode(decode)
                result.addOne(a)
            }
            result.toVector

    def w7l(n: Long): List[Byte] =
        val low = n & 0x7f
        val t = n >>> 7
        if t == 0 then low.toByte :: Nil else (low | 0x80).toByte :: w7l(t)

    /** ZigZag encoding https://gist.github.com/mfuerstenau/ba870a29e16536fdbaba Maps negative
      * values to positive values while going back and forth (0 = 0, -1 = 1, 1 = 2, -2 = 3, 2 = 4,
      * -3 = 5, 3 = 6 ...)
      */
    def zigZag(x: Int) =
        assert(Math.abs(x) <= (1 << 30), "zigZag: value out or range: -2^31..2^31")
        if x >= 0 then x << 1 else -(x << 1) - 1
    def zigZag(x: Long) =
        assert(Math.abs(x) <= (1L << 62), "zigZag: value out or range: -2^62..2^62")
        if x >= 0 then x << 1 else -(x << 1) - 1
    def zagZig(u: Int) = u >> 1 ^ -(u & 1)
    def zagZig(u: Long) = u >> 1 ^ -(u & 1)

    def w7l(n: BigInt): List[Byte] =
        require(n >= 0, s"w7l: input must be non-negative, got $n")
        val low = n & 0x7f
        val t = n >> 7
        if t == 0 then low.toByte :: Nil else (low | 0x80).toByte :: w7l(t)

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

        def result: Array[Byte] =
            val len = if usedBits == 0 then nextPtr else nextPtr + 1
            val result = new Array[Byte](len)
            System.arraycopy(buffer, 0, result, 0, len)
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
            this.currPtr += Math.floor(totUsed / 8).toInt
