package scalus.cardano.ledger

import io.bullet.borer.{Cbor, Encoder, Writer}
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address
import scalus.uplc.builtin.ByteString

import scala.util.Try

/** A pointer address in a transaction output must decode as the ledger's `fromCborBothAddr`
  * (cardano-ledger-core, `Cardano/Ledger/Address.hs`) decodes it at the transaction's protocol
  * version.
  *
  * Before version 9 a pointer that does not fit the ledger's 32-bit slot and 16-bit indexes reads
  * as `Ptr 0 0 0`; from version 9 it fails. Bytes after the pointer fail from version 7 and are
  * dropped before it.
  */
class PointerAddressDecodingTest extends AnyFunSuite {
    private def hex(bytes: String): Array[Byte] = ByteString.fromHex(bytes).bytes

    /** A mainnet address with a payment key hash and `pointer` as its delegation part. */
    private def pointerAddress(pointer: String): Array[Byte] = hex("41" + "00" * 28 + pointer)

    private val outOfRange = pointerAddress("9080808000" + "0102") // the slot 2^32
    private val leftOver = pointerAddress("050102" + "ff00")

    /** Writes CBOR by hand, so that a test can write an address Scalus cannot build. */
    private final class Raw(val write: Writer => Unit)
    private given Encoder[Raw] = (w, raw) => { raw.write(w); w }
    private def cbor(write: Writer => Unit): Array[Byte] = Cbor.encode(Raw(write)).toByteArray

    private def cborArray(items: (Writer => Unit)*): Writer => Unit = { w =>
        w.writeArrayOpen(items.size)
        items.foreach(_(w))
        w.writeArrayClose()
    }
    private def cborMap(entries: (Writer => Unit, Writer => Unit)*): Writer => Unit = { w =>
        w.writeMapOpen(entries.size)
        entries.foreach((k, v) => { k(w); v(w) })
        w.writeMapClose()
    }
    private def value[A: Encoder](a: A): Writer => Unit = _.write(a)
    private def address(bytes: Array[Byte]): Writer => Unit = _.writeBytes(bytes)

    private def shelleyOutput(bytes: Array[Byte]) = cborArray(address(bytes), value(Coin(1)))
    private def babbageOutput(bytes: Array[Byte]) =
        cborMap(value(0) -> address(bytes), value(1) -> value(Coin(1)))

    /** A transaction body with one input, no outputs and a zero fee, with `fields` added or
      * replacing those, decoded at protocol version `pv`.
      */
    private def body(pv: ProtocolVersion)(fields: (Int, Writer => Unit)*): Try[TransactionBody] = {
        val input = TransactionInput(Hash(ByteString.fromHex("01" * 32)), 0)
        val all = (Map[Int, Writer => Unit](
          0 -> cborArray(value(input)),
          1 -> cborArray(),
          2 -> value(Coin(0))
        ) ++ fields).toSeq.sortBy(_._1)
        val bytes = cbor(cborMap(all.map((k, v) => (value(k), v))*))
        given OriginalCborByteArray = OriginalCborByteArray(bytes)
        given ProtocolVersion = pv
        Cbor.decode(bytes).to[TransactionBody].valueTry
    }

    /** Asserts that decoding failed, and that `reason` is among the messages of the failure. */
    private def assertRejected(result: Try[?], reason: String): Unit = {
        assert(result.isFailure, s"decoded: $result")
        val messages = Iterator
            .iterate(result.failed.get)(_.getCause)
            .takeWhile(_ != null)
            .map(e => String.valueOf(e.getMessage))
            .toList
        assert(messages.exists(_.contains(reason)), messages)
    }

    test("an output whose pointer does not fit is rejected from version 9") {
        for pv <- Seq(ProtocolVersion.conwayPV, ProtocolVersion.plominPV) do
            assertRejected(body(pv)(1 -> cborArray(shelleyOutput(outOfRange))), "32 bits")
            assertRejected(body(pv)(1 -> cborArray(babbageOutput(outOfRange))), "32 bits")
            assertRejected(body(pv)(16 -> babbageOutput(outOfRange)), "32 bits")
    }

    test("before version 9 an output whose pointer does not fit is at the zero pointer") {
        val zero = Address.fromBytes(pointerAddress("000000"))
        val decoded = body(ProtocolVersion.valentinePV)(
          1 -> cborArray(shelleyOutput(outOfRange), babbageOutput(outOfRange)),
          16 -> babbageOutput(outOfRange)
        ).get
        assert(decoded.outputs.map(_.value.address) == IndexedSeq(zero, zero))
        assert(decoded.collateralReturnOutput.map(_.value.address) == Some(zero))
    }

    test(
      "an output with bytes after its pointer is rejected from version 7, and drops them before"
    ) {
        val reason = "2 bytes left after the pointer"
        assertRejected(
          body(ProtocolVersion.vasilPV)(1 -> cborArray(babbageOutput(leftOver))),
          reason
        )
        assertRejected(
          body(ProtocolVersion.conwayPV)(1 -> cborArray(shelleyOutput(leftOver))),
          reason
        )
        val decoded = body(ProtocolVersion.alonzoPV)(1 -> cborArray(shelleyOutput(leftOver))).get
        val dropped = Address.fromBytes(pointerAddress("050102"))
        assert(decoded.outputs.map(_.value.address) == IndexedSeq(dropped))
    }
}
