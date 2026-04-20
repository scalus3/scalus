package scalus.cardano.network

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import scalus.uplc.builtin.ByteString

class SduSuite extends AnyFunSuite with ScalaCheckPropertyChecks {

    private val validProtocolWire: Gen[Int] =
        Gen.oneOf(MiniProtocolId.values.toIndexedSeq.map(_.wire))
    private val anyProtocolWire: Gen[Int] = Gen.choose(0, 0x7fff)
    private val anyDirection: Gen[Direction] = Gen.oneOf(Direction.Initiator, Direction.Responder)
    private val anyLength: Gen[Int] = Gen.choose(0, 0xffff)

    test("encode → parse round-trip for any valid (timestamp, protocol, direction, length)") {
        forAll(
          Gen.choose(Int.MinValue, Int.MaxValue),
          anyProtocolWire,
          anyDirection,
          anyLength
        ) { (ts: Int, protoWire: Int, dir: Direction, length: Int) =>
            val bytes = Sdu.encodeHeader(ts, protoWire, dir, length)
            val parsed = Sdu.parseHeader(bytes)
            assert(parsed.timestamp == ts)
            assert(parsed.protocolWire == protoWire)
            assert(parsed.direction == dir)
            assert(parsed.length == length)
        }
    }

    test("boundary lengths — 0, 1, MaxPayloadSize - 1, MaxPayloadSize") {
        for length <- List(0, 1, Sdu.MaxPayloadSize - 1, Sdu.MaxPayloadSize) do {
            val bytes = Sdu.encodeHeader(0, MiniProtocolId.Handshake, Direction.Initiator, length)
            assert(bytes.length == Sdu.HeaderSize)
            assert(Sdu.parseHeader(bytes).length == length)
        }
    }

    test("direction bit placement — Initiator clears high bit, Responder sets it") {
        val init = Sdu.encodeHeader(0, MiniProtocolId.Handshake, Direction.Initiator, 0)
        val resp = Sdu.encodeHeader(0, MiniProtocolId.Handshake, Direction.Responder, 0)
        // Protocol-id = 0, direction bit is high bit of byte offset 4.
        assert(
          (init(4) & 0x80) == 0,
          s"initiator high bit should be clear, got ${init(4).toInt & 0xff}"
        )
        assert(
          (resp(4) & 0x80) != 0,
          s"responder high bit should be set, got ${resp(4).toInt & 0xff}"
        )
    }

    test("golden vector — Handshake (0) / Initiator / length 0, timestamp 0") {
        val expected = Array[Byte](0, 0, 0, 0, 0, 0, 0, 0)
        val bytes = Sdu.encodeHeader(0, MiniProtocolId.Handshake, Direction.Initiator, 0)
        assert(bytes sameElements expected)
    }

    test("golden vector — KeepAlive (8) / Responder / length 3, timestamp 0x01020304") {
        // timestamp 0x01020304 = bytes [01 02 03 04]
        // direction Responder = high bit set: 0x8000
        // protocol 8: low 15 bits = 0x0008
        // combined protocol field: 0x8008 = bytes [80 08]
        // length 3 = bytes [00 03]
        val expected = Array[Byte](0x01, 0x02, 0x03, 0x04, 0x80.toByte, 0x08, 0x00, 0x03)
        val bytes = Sdu.encodeHeader(0x01020304, MiniProtocolId.KeepAlive, Direction.Responder, 3)
        assert(bytes sameElements expected, s"got ${bytes.map("%02x".format(_)).mkString(" ")}")
    }

    test("golden vector — ChainSync (2) / Initiator / length 12288, timestamp 0xFFFFFFFF") {
        // timestamp 0xFFFFFFFF
        // direction Initiator, protocol 2 = 0x0002 → [00 02]
        // length 12288 = 0x3000 → [30 00]
        val expected =
            Array[Byte](0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0x00, 0x02, 0x30, 0x00)
        val bytes =
            Sdu.encodeHeader(0xffffffff, MiniProtocolId.ChainSync, Direction.Initiator, 12288)
        assert(bytes sameElements expected, s"got ${bytes.map("%02x".format(_)).mkString(" ")}")
    }

    test("parse accepts ByteString and Array[Byte] equivalently") {
        val bytes = Sdu.encodeHeader(42, MiniProtocolId.KeepAlive, Direction.Responder, 10)
        val fromArray = Sdu.parseHeader(bytes)
        val fromByteString = Sdu.parseHeader(ByteString.unsafeFromArray(bytes))
        assert(fromArray == fromByteString)
    }

    test("parseHeader preserves the full u32 timestamp including the sign bit") {
        val negTs = 0x80000000 // -2^31 as Int; u32 value 2147483648
        val bytes = Sdu.encodeHeader(negTs, MiniProtocolId.Handshake, Direction.Initiator, 0)
        assert(Sdu.parseHeader(bytes).timestamp == negTs)
    }

    test("Header#protocol resolves known wire numbers and returns None for unknown") {
        val known =
            Sdu.parseHeader(Sdu.encodeHeader(0, MiniProtocolId.BlockFetch, Direction.Initiator, 0))
        assert(known.protocol.contains(MiniProtocolId.BlockFetch))

        // Protocol wire 42 is unassigned.
        val unknown = Sdu.parseHeader(Sdu.encodeHeader(0, 42, Direction.Initiator, 0))
        assert(unknown.protocolWire == 42)
        assert(unknown.protocol.isEmpty)
    }

    test("encodeHeader rejects protocol wire out of u15 range") {
        assertThrows[IllegalArgumentException] {
            Sdu.encodeHeader(0, 0x8000, Direction.Initiator, 0) // would collide with direction bit
        }
        assertThrows[IllegalArgumentException] {
            Sdu.encodeHeader(0, -1, Direction.Initiator, 0)
        }
    }

    test("encodeHeader rejects length out of u16 range") {
        assertThrows[IllegalArgumentException] {
            Sdu.encodeHeader(0, MiniProtocolId.Handshake, Direction.Initiator, 0x10000)
        }
        assertThrows[IllegalArgumentException] {
            Sdu.encodeHeader(0, MiniProtocolId.Handshake, Direction.Initiator, -1)
        }
    }

    test("parseHeader rejects input shorter than HeaderSize") {
        assertThrows[IllegalArgumentException] {
            Sdu.parseHeader(Array[Byte](1, 2, 3, 4, 5, 6, 7))
        }
    }
}
