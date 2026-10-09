package scalus.cardano.ledger
package utils

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.ledger.rules.{Context, FeesOkValidator, State, UtxoEnv}
import scalus.testing.kit.Party.Alice
import scalus.uplc.builtin.ByteString

/** The reference-script term of the Conway min fee, `getConwayMinFeeTxUtxo` in cardano-ledger. */
class MinTransactionFeeTest extends AnyFunSuite {

    // From protocol version 11 a tx may spend and reference the same UTxO
    private val params =
        CardanoInfo.mainnet.protocolParams.copy(protocolVersion = ProtocolVersion(11, 0))

    private val input =
        TransactionInput(TransactionHash.fromByteString(ByteString.fromHex("1" * 64)), 0)

    private val scriptSize = 10_000
    private val script = Script.PlutusV3(ByteString.fromArray(Array.fill(scriptSize)(0.toByte)))

    private def output(scriptRef: Option[ScriptRef]): TransactionOutput =
        TransactionOutput(Alice.address(Network.Mainnet), Value.ada(10), None, scriptRef)

    private val withScript: Utxos = Map(input -> output(Some(ScriptRef(script))))
    private val withoutScript: Utxos = Map(input -> output(None))

    /** Spends and references `input`. The fee keeps a 5-byte CBOR width for any value tried. */
    private def tx(fee: Coin): Transaction =
        Transaction(
          TransactionBody(
            inputs = TaggedSortedSet(input),
            outputs = IndexedSeq.empty[Sized[TransactionOutput]],
            fee = fee,
            referenceInputs = TaggedSortedSet(input)
          )
        )

    private val refScriptFee = RefScriptFee.fee(scriptSize, params.minFeeRefScriptCostPerByte)

    private def minFee(utxos: Utxos): Coin =
        MinTransactionFee.computeMinFee(tx(Coin(1_000_000)), utxos, params).toOption.get

    test("a UTxO both spent and referenced adds its script to the min fee once") {
        // txNonDistinctRefScriptsSize takes the union of inputs and reference inputs
        assert(minFee(withScript) == minFee(withoutScript) + refScriptFee)
    }

    test("the fee rule accepts a tx that spends and references a UTxO at its min fee") {
        val transaction = tx(minFee(withoutScript) + refScriptFee)
        val context = Context(
          env = UtxoEnv(0, params, CertState.empty, Network.Mainnet, Coin.zero),
          slotConfig = CardanoInfo.mainnet.slotConfig
        )
        assert(
          FeesOkValidator.validate(context, State(utxos = withScript), transaction) == Right(())
        )
    }
}
