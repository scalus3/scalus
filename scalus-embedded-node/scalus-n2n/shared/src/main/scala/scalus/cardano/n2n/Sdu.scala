package scalus.cardano.n2n

import scalus.uplc.builtin.ByteString

/** Ouroboros mux SDU wire format — 8-byte header + 0..[[Sdu.MaxPayloadSize]] byte payload.
  * One SDU carries one chunk of one mini-protocol in one direction; multi-byte fields are
  * big-endian, matching ouroboros-network's `network-mux/src/Network/Mux/Types.hs`.
  *
  * Layout:
  * {{{
  *   offset  size  field
  *   0       4     timestamp  (u32, microseconds, low 32 bits of monotonic clock)
  *   4       2     protocol-id + direction-bit
  *                 (high bit = direction: 0 = initiator, 1 = responder;
  *                  low 15 bits = protocol number)
  *   6       2     length     (u16, payload bytes)
  *   8       N     payload    (0..12288 bytes)
  * }}}
  *
  * See `docs/local/claude/indexer/n2n-transport.md` § *Multiplexer* for the full wire contract.
  */
object Sdu {

    /** Maximum payload size per SDU — ouroboros-network's `sduTrailingEdge` default. Messages
      * larger than this MUST be split across multiple SDUs by the mux.
      */
    val MaxPayloadSize: Int = 12288

    /** Fixed 8-byte header size. */
    val HeaderSize: Int = 8

    /** High bit of the 16-bit protocol field carries the direction. */
    private val DirectionBitMask: Int = 0x8000
    private val ProtocolBitsMask: Int = 0x7fff

    /** Decoded SDU header. */
    final case class Header(
        timestamp: Int,
        protocolWire: Int,
        direction: Direction,
        length: Int
    ) {

        /** The [[MiniProtocolId]] if the wire number is one we recognise. `None` means the
          * mux treats this SDU as fatally unexpected.
          */
        def protocol: Option[MiniProtocolId] = MiniProtocolId.fromWire(protocolWire)
    }

    /** Encode a header into a fresh 8-byte array. */
    def encodeHeader(
        timestamp: Int,
        protocolWire: Int,
        direction: Direction,
        length: Int
    ): Array[Byte] = {
        require(
          protocolWire >= 0 && protocolWire <= ProtocolBitsMask,
          s"protocolWire=$protocolWire out of u15 range"
        )
        require(length >= 0 && length <= 0xffff, s"length=$length out of u16 range")
        val out = new Array[Byte](HeaderSize)
        putU32BE(out, 0, timestamp)
        putU16BE(out, 4, (direction.bit << 15) | (protocolWire & ProtocolBitsMask))
        putU16BE(out, 6, length)
        out
    }

    /** Convenience overload taking a [[MiniProtocolId]] directly. */
    def encodeHeader(
        timestamp: Int,
        protocol: MiniProtocolId,
        direction: Direction,
        length: Int
    ): Array[Byte] = encodeHeader(timestamp, protocol.wire, direction, length)

    /** Convenience overload that encodes from an already-built [[Header]]. */
    def encodeHeader(header: Header): Array[Byte] =
        encodeHeader(header.timestamp, header.protocolWire, header.direction, header.length)

    /** Parse an 8-byte header from a [[ByteString]]. The input MUST be exactly 8 bytes — the
      * mux guarantees that by issuing `readExactly(8)` for every header.
      */
    def parseHeader(bytes: ByteString): Header = parseHeader(bytes.bytes)

    /** Array overload — same contract, skips the ByteString wrapping. */
    def parseHeader(arr: Array[Byte]): Header = {
        require(arr.length >= HeaderSize, s"expected at least $HeaderSize bytes, got ${arr.length}")
        val timestamp = getU32BE(arr, 0)
        val protoField = getU16BE(arr, 4)
        val direction =
            if (protoField & DirectionBitMask) != 0 then Direction.Responder else Direction.Initiator
        val protocolWire = protoField & ProtocolBitsMask
        val length = getU16BE(arr, 6)
        Header(timestamp, protocolWire, direction, length)
    }

    // Big-endian byte-packing helpers. `inline` so the call-site body matches what the
    // hand-rolled shifts produced — no runtime overhead.

    private inline def putU32BE(arr: Array[Byte], offset: Int, value: Int): Unit = {
        arr(offset) = ((value >>> 24) & 0xff).toByte
        arr(offset + 1) = ((value >>> 16) & 0xff).toByte
        arr(offset + 2) = ((value >>> 8) & 0xff).toByte
        arr(offset + 3) = (value & 0xff).toByte
    }

    private inline def putU16BE(arr: Array[Byte], offset: Int, value: Int): Unit = {
        arr(offset) = ((value >>> 8) & 0xff).toByte
        arr(offset + 1) = (value & 0xff).toByte
    }

    private inline def getU32BE(arr: Array[Byte], offset: Int): Int =
        ((arr(offset) & 0xff) << 24) |
            ((arr(offset + 1) & 0xff) << 16) |
            ((arr(offset + 2) & 0xff) << 8) |
            (arr(offset + 3) & 0xff)

    private inline def getU16BE(arr: Array[Byte], offset: Int): Int =
        ((arr(offset) & 0xff) << 8) | (arr(offset + 1) & 0xff)
}
