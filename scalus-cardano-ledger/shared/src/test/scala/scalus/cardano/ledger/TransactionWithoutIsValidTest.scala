package scalus.cardano.ledger

import io.bullet.borer.Borer
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.ledger.rules.{Context, State, TransactionSizeValidator, UtxoEnv}
import scalus.cardano.ledger.utils.MinTransactionFee
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.uplc.builtin.ByteString

/** A transaction encoded as `[body, witnesses, auxiliary data]`, without the `isValid` flag.
  *
  * Since cardano-ledger-alonzo 1.16.0.0 (cardano-node 11.1.3), the `AlonzoTx` decoder that Conway
  * uses for submission peeks at the third element: a CBOR bool is `isValid`, anything else is the
  * auxiliary data with `isValid = True`, and the record must then have 3 elements instead of 4. A
  * transaction built for a pre-Alonzo era can so be submitted in a later era.
  */
class TransactionWithoutIsValidTest extends AnyFunSuite {

    given testEnv: CardanoInfo = CardanoInfo.mainnet

    private val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    private val alice = Alice.address(Network.Mainnet)
    private val bob = Bob.address(Network.Mainnet)

    private val utxos: Utxos = Map(Input(genesisHash, 0) -> Output(alice, Value.ada(100)))

    /** A signed payment, as a fresh `TxBuilder` encodes it: 4 elements, `isValid = true` */
    private val built: Transaction =
        TxBuilder(testEnv)
            .payTo(bob, Value.ada(10))
            .complete(utxos, alice)
            .sign(Alice.signer)
            .transaction

    private val fourElements: Array[Byte] = built.toCbor

    private val body: Array[Byte] = built.body.raw
    private val witnesses: Array[Byte] = built.witnessSetRaw.raw
    private val cborTrue: Byte = 0xf5.toByte
    private val cborFalse: Byte = 0xf4.toByte
    private val cborNull: Byte = 0xf6.toByte

    /** The same transaction without the flag: the header says 3, and the `true` is gone */
    private val threeElements: Array[Byte] = {
        val flagAt = 1 + body.length + witnesses.length
        assert(fourElements(0) == 0x84.toByte, "a definite-length array of 4")
        assert(fourElements(flagAt) == cborTrue, "the isValid flag follows the witnesses")
        Array(0x83.toByte) ++ fourElements.slice(1, flagAt) ++ fourElements.drop(flagAt + 1)
    }

    private def hex(bytes: Array[Byte]): String = ByteString.unsafeFromArray(bytes).toHex

    private def items(xs: Array[Byte]*): Array[Byte] = xs.foldLeft(Array.emptyByteArray)(_ ++ _)

    test("a fresh transaction encodes as 4 elements") {
        assert(fourElements(0) == 0x84.toByte)
        assert(Transaction.empty.toCbor(0) == 0x84.toByte)
    }

    test("a 4-element transaction decodes and re-encodes to the same bytes") {
        val tx = Transaction.fromCbor(fourElements)
        assert(tx.isValid)
        assert(hex(tx.toCbor) == hex(fourElements))
        assert(tx == built)
    }

    test("a 3-element transaction decodes with isValid = true") {
        val tx = Transaction.fromCbor(threeElements)
        assert(tx.isValid)
        assert(tx.id == built.id)
        assert(tx.auxiliaryData.isEmpty)
        assert(hex(tx.body.raw) == hex(body))
        assert(hex(tx.witnessSetRaw.raw) == hex(witnesses))
    }

    test("a 3-element transaction re-encodes to its original bytes") {
        val tx = Transaction.fromCbor(threeElements)
        assert(hex(tx.toCbor) == hex(threeElements))
        assert(hex(scalus.serialization.cbor.Cbor.encode(tx)) == hex(threeElements))
    }

    test("size and fee do not depend on the flag, as in the ledger's toCBORForSizeComputation") {
        val tx3 = Transaction.fromCbor(threeElements)
        val tx4 = Transaction.fromCbor(fourElements)
        assert(hex(tx3.toCborForFeeCalculation) == hex(threeElements))
        assert(hex(tx4.toCborForFeeCalculation) == hex(threeElements))
        val params = testEnv.protocolParams
        assert(
          MinTransactionFee.computeMinFee(tx3, utxos, params) ==
              MinTransactionFee.computeMinFee(tx4, utxos, params)
        )
    }

    test("maxTxSize counts the 3-element size, for both encodings") {
        val exact = UtxoEnv.default.copy(
          params = UtxoEnv.default.params.copy(maxTxSize = threeElements.length.toLong)
        )
        val context = Context(env = exact)
        for bytes <- Seq(threeElements, fourElements) do
            val tx = Transaction.fromCbor(bytes)
            assert(TransactionSizeValidator.validate(context, State(), tx).isRight)
        val oneLess = Context(env =
            exact.copy(params = exact.params.copy(maxTxSize = threeElements.length.toLong - 1))
        )
        assert(
          TransactionSizeValidator
              .validate(oneLess, State(), Transaction.fromCbor(fourElements))
              .isLeft
        )
    }

    test("the Emulator accepts a 3-element transaction and keeps its bytes") {
        val emulator = Emulator(initialUtxos = utxos)
        val tx = Transaction.fromCbor(threeElements)
        assert(emulator.submitSync(tx) == Right(built.id))
        val stored = emulator.getTransaction(built.id).getOrElse(fail("the tx was applied"))
        assert(hex(stored.toCbor) == hex(threeElements))
        assert(emulator.utxos.keySet.contains(Input(built.id, 0)))
    }

    test("a changed transaction drops the original bytes and encodes as 4 elements") {
        val tx = Transaction.fromCbor(threeElements)
        val reSigned = tx.withWitness(tx.witnessSet)
        assert(reSigned.toCbor(0) == 0x84.toByte)
        assert(hex(reSigned.toCbor) == hex(fourElements))
        val invalid = tx.copy(isValid = false)
        assert(invalid.toCbor(0) == 0x84.toByte)
        assert(!Transaction.fromCbor(invalid.toCbor).isValid)
    }

    test("an indefinite-length 3-element transaction decodes and keeps its bytes") {
        val bytes = items(Array(0x9f.toByte), body, witnesses, Array(cborNull, 0xff.toByte))
        val tx = Transaction.fromCbor(bytes)
        assert(tx.isValid)
        assert(tx.id == built.id)
        assert(hex(tx.toCbor) == hex(bytes))
    }

    test("an indefinite-length 4-element transaction keeps its flag and its bytes") {
        val bytes =
            items(Array(0x9f.toByte), body, witnesses, Array(cborFalse, cborNull, 0xff.toByte))
        val tx = Transaction.fromCbor(bytes)
        assert(!tx.isValid)
        assert(hex(tx.toCbor) == hex(bytes))
    }

    test("malformed records are rejected as the ledger rejects them") {
        val malformed = Map(
          // a flag, but then no auxiliary data
          "[body, wits, true]" -> items(Array(0x83.toByte), body, witnesses, Array(cborTrue)),
          "[body, wits, false]" -> items(Array(0x83.toByte), body, witnesses, Array(cborFalse)),
          // no flag, so the third element is the auxiliary data and 4 is the wrong length
          "4 x [body, wits, null, null]" ->
              items(Array(0x84.toByte), body, witnesses, Array(cborNull, cborNull)),
          "4 x [body, wits, null, true]" ->
              items(Array(0x84.toByte), body, witnesses, Array(cborNull, cborTrue)),
          "3 x [body, wits, true, null]" ->
              items(Array(0x83.toByte), body, witnesses, Array(cborTrue, cborNull)),
          "5 x [body, wits, true, null, null]" ->
              items(Array(0x85.toByte), body, witnesses, Array(cborTrue, cborNull, cborNull)),
          "[body, wits]" -> items(Array(0x82.toByte), body, witnesses),
          "indefinite [body, wits, null, null]" ->
              items(Array(0x9f.toByte), body, witnesses, Array(cborNull, cborNull, 0xff.toByte)),
          "indefinite [body, wits, true]" ->
              items(Array(0x9f.toByte), body, witnesses, Array(cborTrue, 0xff.toByte))
        )
        for (name, bytes) <- malformed do
            withClue(name) {
                intercept[Borer.Error[?]](Transaction.fromCbor(bytes))
            }
    }

    test("the length is checked against the flag after the elements are read") {
        val noFlagButFour = items(Array(0x84.toByte), body, witnesses, Array(cborNull, cborNull))
        val flagButThree = items(Array(0x83.toByte), body, witnesses, Array(cborTrue, cborNull))
        val excess =
            items(Array(0x9f.toByte), body, witnesses, Array(cborNull, cborNull, 0xff.toByte))
        assert(
          intercept[Borer.Error[?]](Transaction.fromCbor(noFlagButFour)).getMessage
              .contains("without the isValid flag has 3 elements, got 4")
        )
        assert(
          intercept[Borer.Error[?]](Transaction.fromCbor(flagButThree)).getMessage
              .contains("with the isValid flag has 4 elements, got 3")
        )
        assert(
          intercept[Borer.Error[?]](Transaction.fromCbor(excess)).getMessage
              .contains("Excess terms")
        )
    }
}
