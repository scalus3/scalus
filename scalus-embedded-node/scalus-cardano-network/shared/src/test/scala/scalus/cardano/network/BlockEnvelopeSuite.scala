package scalus.cardano.network

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

class BlockEnvelopeSuite extends AnyFunSuite {

    test("Era.fromWire maps canonical indices") {
        assert(Era.fromWire(0) == Era.Byron)
        assert(Era.fromWire(1) == Era.Shelley)
        assert(Era.fromWire(2) == Era.Allegra)
        assert(Era.fromWire(3) == Era.Mary)
        assert(Era.fromWire(4) == Era.Alonzo)
        assert(Era.fromWire(5) == Era.Babbage)
        assert(Era.fromWire(6) == Era.Conway)
    }

    test("Era.fromWire maps unknown indices to Unknown") {
        assert(Era.fromWire(7) == Era.Unknown(7))
        assert(Era.fromWire(99) == Era.Unknown(99))
    }

    test("decodeHeader rejects Byron era fast") {
        val bytes = ByteString.fromArray(Array[Byte](1, 2, 3))
        BlockEnvelope.decodeHeader(Era.Byron, bytes) match {
            case Left(_: ChainSyncError.ByronEra) => ()
            case other => fail(s"expected ByronEra error, got $other")
        }
    }

    test("decodeHeader rejects Unknown era with Decode error") {
        val bytes = ByteString.fromArray(Array[Byte](1, 2, 3))
        BlockEnvelope.decodeHeader(Era.Unknown(12), bytes) match {
            case Left(err: ChainSyncError.Decode) =>
                assert(err.getMessage.toLowerCase.contains("unknown era"))
            case other => fail(s"expected Decode error, got $other")
        }
    }

    test("decodeBlock rejects Byron era fast") {
        val bytes = ByteString.fromArray(Array[Byte](1, 2, 3))
        BlockEnvelope.decodeBlock(Era.Byron, bytes) match {
            case Left(_: ChainSyncError.ByronEra) => ()
            case other => fail(s"expected ByronEra error, got $other")
        }
    }

    test("decodeHeader surfaces Decode on malformed Shelley+ bytes") {
        // Clearly not valid BlockHeader CBOR.
        val garbage = ByteString.fromArray(Array[Byte](0xff.toByte, 0xff.toByte))
        BlockEnvelope.decodeHeader(Era.Conway, garbage) match {
            case Left(_: ChainSyncError.Decode) => ()
            case other => fail(s"expected Decode error for garbage, got $other")
        }
    }

    test("decodeBlock surfaces Decode on malformed Shelley+ bytes") {
        val garbage = ByteString.fromArray(Array[Byte](0xff.toByte, 0xff.toByte))
        BlockEnvelope.decodeBlock(Era.Conway, garbage) match {
            case Left(_: ChainSyncError.Decode) => ()
            case other => fail(s"expected Decode error for garbage, got $other")
        }
    }
}
