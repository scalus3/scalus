package scalus.cardano.n2n.keepalive

import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.n2n.keepalive.KeepAliveMessage.*
import scalus.utils.Hex.*

class KeepAliveMessageSuite extends AnyFunSuite {

    private def roundTrip(m: KeepAliveMessage): Unit = {
        val bytes = Cbor.encode(m).toByteArray
        val decoded = Cbor.decode(bytes).to[KeepAliveMessage].value
        assert(decoded == m, s"round-trip mismatch for $m\nbytes=${bytes.toHex}")
    }

    test("round-trip MsgKeepAlive — cookie 0") {
        roundTrip(MsgKeepAlive(0))
    }

    test("round-trip MsgKeepAlive — cookie at u16 max") {
        roundTrip(MsgKeepAlive(0xffff))
    }

    test("round-trip MsgKeepAliveResponse") {
        roundTrip(MsgKeepAliveResponse(42))
    }

    test("round-trip MsgDone") {
        roundTrip(MsgDone)
    }

    test("golden: MsgKeepAlive(cookie=1) — [0, 1]") {
        // 82        array(2)
        //   00      uint 0      ; tag
        //   01      uint 1      ; cookie
        val expected = "82 00 01".filter(_ != ' ')
        val m: KeepAliveMessage = MsgKeepAlive(1)
        assert(Cbor.encode(m).toByteArray.toHex == expected)
    }

    test("golden: MsgKeepAliveResponse(cookie=0x0102) — [1, 258]") {
        // 82            array(2)
        //   01          uint 1                   ; tag
        //   19 01 02    uint 0x0102 (u16 form)
        val expected = "82 01 19 01 02".filter(_ != ' ')
        val m: KeepAliveMessage = MsgKeepAliveResponse(0x0102)
        assert(Cbor.encode(m).toByteArray.toHex == expected)
    }

    test("golden: MsgDone — [2]") {
        // 81 02  —  array(1), uint 2
        val expected = "81 02".filter(_ != ' ')
        val m: KeepAliveMessage = MsgDone
        assert(Cbor.encode(m).toByteArray.toHex == expected)
    }

    test("decode rejects unknown tag") {
        // [7] — tag 7 not a valid keep-alive message
        val ex = intercept[Exception] {
            Cbor.decode(Array[Byte](0x81.toByte, 0x07)).to[KeepAliveMessage].value
        }
        assert(ex.getMessage.toLowerCase.contains("tag"))
    }

    test("decode rejects wrong arity for tag") {
        // [0] — MsgKeepAlive tag without cookie
        val ex = intercept[Exception] {
            Cbor.decode(Array[Byte](0x81.toByte, 0x00)).to[KeepAliveMessage].value
        }
        assert(ex.getMessage.toLowerCase.contains("tag") || ex.getMessage.contains("arrLen"))
    }

    test("cookie out of u16 range is rejected by the constructor") {
        assertThrows[IllegalArgumentException](MsgKeepAlive(-1))
        assertThrows[IllegalArgumentException](MsgKeepAlive(0x10000))
    }
}
