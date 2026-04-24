package scalus.cardano.node.stream.engine.snapshot.ledgerstate

/** Reader for the `mempack` binary format (Haskell library `lehins/mempack`, used by
  * `cardano-ledger` to serialise UTxO-HD ledger-state tables).
  *
  * Ported from `/Users/rssh/packages/lehins/mempack/src/Data/MemPack.hs`. We only implement the
  * unpack (read) side — we never write MemPack.
  *
  * ==Endianness==
  *
  * MemPack writes multi-byte words with GHC's `writeWord8ArrayAsWord64#` family, which emits
  * native-endian bytes. On the platforms that actually produce these snapshots (x86_64 / arm64
  * Linux / macOS) that's **little-endian**. This reader assumes little-endian. A big-endian-host
  * cardano-node would produce non-portable snapshots; we don't support that case.
  *
  * ==VarLen==
  *
  * `VarLen Word` is a big-endian base-128 encoding: the first byte holds the most-significant
  * 7-bit group with the continuation bit (0x80) set; subsequent bytes hold lower groups with the
  * continuation bit until a final byte with the continuation bit clear. `Length` wraps `VarLen
  * Word` but rejects values with the high bit of the underlying `Word` set (would be negative
  * `Int`).
  *
  * ==Usage==
  *
  * A [[MemPack.Reader]] holds a cursor into an `Array[Byte]` and consumes bytes left-to-right.
  * Higher-level decoders (TxIn, CompactAddr, CompactValue, TxOut, …) compose the primitives
  * declared here. Every `read*` method throws [[MemPack.DecodeError]] on end-of-buffer or
  * invariant violation.
  */
object MemPack {

    /** `pos` is the byte offset within the MemPack payload at which decoding failed (0-based,
      * relative to the `Reader`'s start); `-1` when the error isn't tied to a specific cursor
      * position (e.g. raised by [[unknownTag]] from a higher-level reader that doesn't hold a
      * `Reader`). `pos` is also included in `getMessage` so stack-trace lines carry it.
      */
    class DecodeError(msg: String, val pos: Int) extends RuntimeException {
        def this(msg: String) = this(msg, -1)

        override def getMessage: String =
            if pos >= 0 then s"$msg (at MemPack offset $pos)" else msg
    }

    /** Cursor-based reader over a backing `Array[Byte]`. Mutates `pos` in place as bytes are
      * consumed. Construct via [[apply]]; keep each reader instance to a single thread.
      */
    final class Reader private (buf: Array[Byte], start: Int, end: Int) {
        private var pos: Int = start

        def offset: Int = pos - start
        def remaining: Int = end - pos
        def eof: Boolean = pos >= end

        /** Raise a [[DecodeError]] whose `pos` is the current offset — for higher-level readers
          * that detect a semantic violation after already consuming some bytes.
          */
        def fail(msg: String): Nothing = throw new DecodeError(msg, offset)

        /** Consume `n` bytes of forward progress, failing if fewer remain. Returns the offset at
          * which those bytes start in the backing array.
          */
        def advance(n: Int): Int = {
            if n < 0 then fail(s"advance: negative count $n")
            val newPos = pos + n
            if newPos > end then
                fail(s"unexpected end of input, need $n bytes but only ${end - pos} remain")
            val at = pos
            pos = newPos
            at
        }

        /** Read a single byte as an unsigned 0..255 `Int`. */
        def readUnsignedByte(): Int = {
            val at = advance(1)
            buf(at) & 0xff
        }

        def readWord8(): Byte = buf(advance(1))

        /** Little-endian Word16. Returned as `Int` in 0..65535. */
        def readWord16(): Int = {
            val at = advance(2)
            (buf(at) & 0xff) | ((buf(at + 1) & 0xff) << 8)
        }

        /** Little-endian Word32. Returned as `Long` in 0..2^32-1 (so users get the unsigned value
          * without sign-extension surprises). Callers that want a signed `Int` can cast.
          */
        def readWord32(): Long = {
            val at = advance(4)
            (buf(at) & 0xffL) |
                ((buf(at + 1) & 0xffL) << 8) |
                ((buf(at + 2) & 0xffL) << 16) |
                ((buf(at + 3) & 0xffL) << 24)
        }

        /** Little-endian Word64, returned as raw `Long` bits. Callers interpreting this as an
          * unsigned 64-bit number should be aware that values ≥ 2^63 will appear negative.
          * Cardano uses Word64 for lovelace (≤ 45 B ADA = 4.5e16, well below 2^63), so signedness
          * is practically a non-issue.
          */
        def readWord64(): Long = {
            val at = advance(8)
            (buf(at) & 0xffL) |
                ((buf(at + 1) & 0xffL) << 8) |
                ((buf(at + 2) & 0xffL) << 16) |
                ((buf(at + 3) & 0xffL) << 24) |
                ((buf(at + 4) & 0xffL) << 32) |
                ((buf(at + 5) & 0xffL) << 40) |
                ((buf(at + 6) & 0xffL) << 48) |
                ((buf(at + 7) & 0xffL) << 56)
        }

        /** Read `n` raw bytes into a fresh array. */
        def readBytes(n: Int): Array[Byte] = {
            val at = advance(n)
            val out = new Array[Byte](n)
            System.arraycopy(buf, at, out, 0, n)
            out
        }

        /** One-byte discriminator used by `packTagM` / `unpackTagM` for tagged sum types. */
        def readTag(): Int = readUnsignedByte()

        /** Big-endian base-128 `VarLen Word64` — the underlying encoding for `Length` and all
          * size-prefixes in MemPack.
          *
          * On 64-bit platforms `VarLen Word` is `VarLen Word64`; each byte contributes 7 bits,
          * continuation bit is 0x80, most-significant group first. Up to 10 bytes for a full
          * Word64 (10 * 7 = 70 ≥ 64). The top mask check mirrors `unpack7BitVarLenLast`'s
          * `0b_1111_1110` for Word64.
          */
        def readVarLenWord(): Long = {
            var acc: Long = 0L
            var firstByte: Int = 0
            var i: Int = 0
            var done: Boolean = false
            while !done do {
                val b = readUnsignedByte()
                if i == 0 then firstByte = b
                if (b & 0x80) != 0 then {
                    acc = (acc << 7) | (b & 0x7f).toLong
                    i += 1
                    if i >= 10 then fail("VarLen Word64: more than 10 bytes of continuation")
                } else {
                    acc = (acc << 7) | b.toLong
                    done = true
                }
            }
            // Mirror unpack7BitVarLenLast for Word64: if the first byte supplied too many
            // high bits the decode is invalid. See mempack src `0b_1111_1110` mask.
            if i == 9 && (firstByte & 0xfe) != 0x80 then
                fail(f"VarLen Word64: excess bits in leading byte 0x$firstByte%02x")
            acc
        }

        /** `Length` = `VarLen Word` with a rejection for values whose high bit is set (the
          * Haskell side converts Word to Int and errors on negatives).
          */
        def readLength(): Int = {
            val w = readVarLenWord()
            if w < 0 then fail(s"Length: negative value in underlying VarLen Word: $w")
            if w > Int.MaxValue then fail(s"Length: exceeds Int.MaxValue ($w)")
            w.toInt
        }

        /** A `ByteString` / `ShortByteString` / `ByteArray` is written as `Length` followed by
          * that many raw bytes.
          */
        def readLengthPrefixedBytes(): Array[Byte] = {
            val n = readLength()
            readBytes(n)
        }

        /** `Bool`: tag 0 ⇒ False, tag 1 ⇒ True, anything else fails. */
        def readBool(): Boolean = {
            readTag() match {
                case 0 => false
                case 1 => true
                case t => fail(s"Bool: unexpected tag $t")
            }
        }

        /** `Maybe a`: tag 0 ⇒ Nothing, tag 1 ⇒ Just (read `a`). */
        def readMaybe[A](readA: => A): Option[A] = {
            readTag() match {
                case 0 => None
                case 1 => Some(readA)
                case t => fail(s"Maybe: unexpected tag $t")
            }
        }

        /** Produce a sub-reader over `n` bytes of the current buffer, advancing this reader past
          * them. Useful for decoding length-delimited MemPack payloads (e.g. the CBOR-`bytes`
          * values inside a `tables/tvar` map without an intermediate array copy).
          */
        def sliceReader(n: Int): Reader = {
            val at = advance(n)
            new Reader(buf, at, at + n)
        }
    }

    object Reader {
        def apply(bytes: Array[Byte]): Reader =
            new Reader(bytes, 0, bytes.length)

        def apply(bytes: Array[Byte], start: Int, length: Int): Reader = {
            require(start >= 0 && length >= 0 && start + length <= bytes.length)
            new Reader(bytes, start, start + length)
        }
    }

    /** Raise an `unknownTagM`-shaped error — matches the Haskell error message for parity with
      * round-trip fixtures that assert on it. `pos` points at the byte AFTER the tag byte (so
      * the failing tag is at `pos - 1`); passing a [[Reader]] is encouraged over the
      * typeName-only overload so the error carries location info.
      */
    def unknownTag(typeName: String, tag: Int, r: Reader): Nothing =
        throw new DecodeError(s"Unrecognized Tag: $tag while decoding $typeName", r.offset)

    /** Raise an `unknownTagM`-shaped error without a reader context. Use only from call sites
      * that genuinely don't hold a `Reader` (should be rare); prefer the 3-arg overload.
      */
    def unknownTag(typeName: String, tag: Int): Nothing =
        throw new DecodeError(s"Unrecognized Tag: $tag while decoding $typeName")
}
