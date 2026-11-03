package scalus.cardano.ledger
package rules

import io.bullet.borer.{Cbor, Encoder, Tag, Writer}
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.ByronAddress
import scalus.uplc.builtin.ByteString

class OutputBootAddrAttrsSizeValidatorTest extends AnyFunSuite {

    /** A Byron address whose HD payload (attribute 1) is `hdPayloadSize` bytes, encoded as a node
      * encodes it: the attribute value is the payload serialized as a CBOR bytestring
      * (eras/byron/ledger/impl/src/Cardano/Chain/Common/AddrAttributes.hs:109). The ledger's
      * `bootstrapAddressAttrsSize` counts the payload without its CBOR head
      * (libs/cardano-ledger-core/src/Cardano/Ledger/Address.hs:334-340), so the 64/65 boundary here
      * is a node's. The validator reads only the attributes, so the CRC is 0.
      */
    private def byronAddress(hdPayloadSize: Int): ByronAddress = {
        val hdPayload = Cbor.encode(new Array[Byte](hdPayloadSize)).toByteArray
        val payload = Cbor.encode((new Array[Byte](28), Map(1 -> hdPayload), 0)).toByteArray
        ByronAddress(ByteString.fromArray(Cbor.encode(Outer(payload)).toByteArray))
    }

    /** The outer Byron structure: `[tag 24 (bytes payload), crc32]`. */
    private final case class Outer(payload: Array[Byte])

    private given Encoder[Outer] with
        def write(w: Writer, outer: Outer): Writer =
            w.writeArrayOpen(2)
                .writeTag(Tag.EmbeddedCBOR)
                .writeBytes(outer.payload)
                .writeLong(0L)
                .writeArrayClose()

    private val tooBig = byronAddress(65)
    private val fits = byronAddress(64)

    private def tx(
        outputs: IndexedSeq[ByronAddress],
        collateralReturn: Option[ByronAddress]
    ): Transaction =
        Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = outputs.map(a => Sized(TransactionOutput(a, Value.ada(2)))),
            fee = Coin.zero,
            collateralReturnOutput =
                collateralReturn.map(a => Sized(TransactionOutput(a, Value.ada(2))))
          )
        )

    test("the test addresses have the attribute sizes a node computes, spec [TX-4]") {
        assert(tooBig.attributesSize == 65)
        assert(fits.attributesSize == 64)
    }

    test("an output with bootstrap attributes over 64 bytes is rejected") {
        val result = OutputBootAddrAttrsSizeValidator.validate(
          Context(),
          State(),
          tx(IndexedSeq(tooBig), None)
        )
        assert(result.left.map(_.outputsWithOversizedAttrs) == Left(Seq(tooBig)))
    }

    test("a collateral return with bootstrap attributes over 64 bytes is rejected, spec [TX-4]") {
        // B/Rules/Utxo.hs:406-414 checks allSizedOutputsTxBodyF: the outputs and the collateral
        // return
        val result = OutputBootAddrAttrsSizeValidator.validate(
          Context(),
          State(),
          tx(IndexedSeq(fits), Some(tooBig))
        )
        assert(result.left.map(_.outputsWithOversizedAttrs) == Left(Seq(tooBig)))
    }

    test("outputs and a collateral return with 64-byte bootstrap attributes pass") {
        val result =
            OutputBootAddrAttrsSizeValidator.validate(
              Context(),
              State(),
              tx(IndexedSeq(fits), Some(fits))
            )
        assert(result.isRight)
    }
}
