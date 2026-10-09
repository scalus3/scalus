package scalus.cardano.ledger.rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.testing.kit.Party.Alice
import scalus.uplc.builtin.ByteString

/** The Conway LEDGER `validateRefScriptSize` check, spec [SC-24]. */
class TxRefScriptsSizeValidatorTest extends AnyFunSuite {

    private val cardanoInfo = CardanoInfo.mainnet
    private val limit = 200 * 1024

    private val context = Context(
      env = UtxoEnv(0, cardanoInfo.protocolParams, CertState.empty, Network.Mainnet, Coin.zero),
      slotConfig = cardanoInfo.slotConfig
    )

    private val txHash = TransactionHash.fromByteString(ByteString.fromHex("1" * 64))
    private def input(index: Int): TransactionInput = TransactionInput(txHash, index)

    /** A Plutus script of exactly `size` bytes. The validator never decodes it. */
    private def plutusScript(size: Int): Script =
        Script.PlutusV3(ByteString.fromArray(Array.fill(size)(0.toByte)))

    private def withScript(script: Script): TransactionOutput =
        TransactionOutput(
          Alice.address(Network.Mainnet),
          Value.ada(10),
          None,
          Some(ScriptRef(script))
        )

    private def tx(
        inputs: Seq[TransactionInput],
        referenceInputs: Seq[TransactionInput]
    ): Transaction =
        Transaction(
          TransactionBody(
            inputs = TaggedSortedSet(inputs*),
            outputs = IndexedSeq.empty[Sized[TransactionOutput]],
            fee = Coin.zero,
            referenceInputs = TaggedSortedSet(referenceInputs*)
          )
        )

    private def validate(utxos: Utxos, transaction: Transaction) =
        TxRefScriptsSizeValidator.validate(context, State(utxos = utxos), transaction)

    private def tooBig(transaction: Transaction, supplied: Int) =
        Left(
          TransactionException.TxRefScriptsSizeTooBigException(
            transaction.id,
            supplied = supplied,
            expected = limit
          )
        )

    test("accepts reference scripts of exactly 200 KiB") {
        val transaction = tx(Seq.empty, Seq(input(0)))
        assert(validate(Map(input(0) -> withScript(plutusScript(limit))), transaction) == Right(()))
    }

    test("rejects reference scripts one byte over 200 KiB") {
        val transaction = tx(Seq.empty, Seq(input(0)))
        val utxos = Map(input(0) -> withScript(plutusScript(limit + 1)))
        assert(validate(utxos, transaction) == tooBig(transaction, limit + 1))
    }

    test("counts the reference script of a spent input") {
        val transaction = tx(Seq(input(0)), Seq.empty)
        val utxos = Map(input(0) -> withScript(plutusScript(limit + 1)))
        assert(validate(utxos, transaction) == tooBig(transaction, limit + 1))
    }

    test("counts the same script once per UTxO that carries it") {
        // txNonDistinctRefScriptsSize: duplicates in different UTxOs count each time
        val script = plutusScript(limit / 2 + 1)
        val transaction = tx(Seq(input(0)), Seq(input(1)))
        val utxos = Map(input(0) -> withScript(script), input(1) -> withScript(script))
        assert(validate(utxos, transaction) == tooBig(transaction, limit + 2))
    }

    test("counts a UTxO that is both spent and referenced once") {
        // txNonDistinctRefScriptsSize takes the union of inputs and reference inputs
        val transaction = tx(Seq(input(0)), Seq(input(0)))
        assert(validate(Map(input(0) -> withScript(plutusScript(limit))), transaction) == Right(()))
    }

    test("measures a native script by the CBOR of its timelock") {
        val timelock = Timelock.Signature(Alice.addrKeyHash)
        val nativeSize = timelock.toCbor.length
        val transaction = tx(Seq.empty, Seq(input(0), input(1)))
        def utxos(plutusSize: Int): Utxos = Map(
          input(0) -> withScript(Script.Native(timelock)),
          input(1) -> withScript(plutusScript(plutusSize))
        )
        assert(validate(utxos(limit - nativeSize), transaction) == Right(()))
        assert(
          validate(utxos(limit - nativeSize + 1), transaction) == tooBig(transaction, limit + 1)
        )
    }

    test("measures a native script by its original bytes") {
        // originalBytesSize: 7 bytes, where the canonical encoding 82040a has 3
        val native = Script.Native.fromCbor(ByteString.fromHex("82041a0000000a").bytes)
        val transaction = tx(Seq.empty, Seq(input(0), input(1)))
        def utxos(plutusSize: Int): Utxos = Map(
          input(0) -> withScript(native),
          input(1) -> withScript(plutusScript(plutusSize))
        )
        assert(validate(utxos(limit - 7), transaction) == Right(()))
        assert(validate(utxos(limit - 6), transaction) == tooBig(transaction, limit + 1))
    }

    test("skips an input that is not in the UTxO") {
        // getReferenceScriptsNonDistinct restricts the UTxO to the inputs it holds
        val transaction = tx(Seq(input(0)), Seq(input(1)))
        assert(validate(Map(input(1) -> withScript(plutusScript(limit))), transaction) == Right(()))
    }

    test("skips the check for a phase-2-invalid tx") {
        // Conway LEDGER runs validateRefScriptSize only when isValid is true
        val transaction = tx(Seq.empty, Seq(input(0))).copy(isValid = false)
        val utxos = Map(input(0) -> withScript(plutusScript(limit + 1)))
        assert(validate(utxos, transaction) == Right(()))
    }

    test("is one of the default validators") {
        assert(DefaultValidators.all.contains(TxRefScriptsSizeValidator))
    }

    test("names its rule TxRefScriptsSizeTooBig") {
        val transaction = tx(Seq.empty, Seq.empty)
        val error = TransactionException.TxRefScriptsSizeTooBigException(
          transaction.id,
          supplied = limit + 1,
          expected = limit
        )
        assert(TransactionException.ruleName(error) == "TxRefScriptsSizeTooBig")
    }
}
