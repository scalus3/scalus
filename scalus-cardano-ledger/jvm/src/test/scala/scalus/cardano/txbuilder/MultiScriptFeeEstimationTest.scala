package scalus.cardano.txbuilder

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.Context
import scalus.cardano.ledger.utils.MinTransactionFee
import scalus.cardano.node.Emulator
import scalus.compiler.compileInline
import scalus.testing.kit.Party.{Alice, Bob, Charles}
import scalus.toUplc
import scalus.uplc.builtin.{ByteString, Data}
import scalus.utils.await

import java.time.Instant
import scala.concurrent.ExecutionContext.Implicits.global

/** Test for TxBuilder.complete fee estimation bug.
  *
  * BUG: TxBuilder.completeLoop adds the sponsor to expectedSigners AFTER balanceContext (line
  * ~1590), but balanceContext uses expectedSigners.size to add dummy signatures for fee
  * calculation.
  *
  * When script inputs have enough ADA for outputs + fees (no additional spending inputs needed from
  * the sponsor), pool.inputs is empty — so no PubKeyWitness spend is created, and expectedSigners
  * remains empty during balanceContext. However, the sponsor MUST still sign the transaction
  * because their collateral UTxO is included. The fee is computed without any VKey witness, but the
  * signed transaction has one.
  *
  * The shortfall is exactly one VKey witness size: ~106 bytes × 44 lovelace/byte ≈ 4664 lovelace.
  *
  * This causes the emulator's FeesOkValidator to reject the transaction.
  *
  * FIX: Add the sponsor to expectedSigners BEFORE calling balanceContext in completeLoop.
  */
class MultiScriptFeeEstimationTest extends AnyFunSuite {

    private given env: CardanoInfo = CardanoInfo.mainnet

    private val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    private val alwaysOkScript: Script.PlutusV3 = {
        val alwaysOk = compileInline((sc: Data) => ())
        Script.PlutusV3(alwaysOk.toUplc().plutusV3.cborByteString)
    }

    private val scriptAddress: ShelleyAddress = ShelleyAddress(
      network = env.network,
      payment = ShelleyPaymentPart.Script(alwaysOkScript.scriptHash),
      delegation = ShelleyDelegationPart.Null
    )

    private val datum42: Data = Data.I(42)

    ignore("complete should produce sufficient fee for multi-script tx submitted to emulator") {
        // Spend 2 script UTxOs with delayed redeemers that compute output indices,
        // mimicking a batch auction end transaction. Submit to emulator to verify
        // the fee is accepted by FeesOkValidator.
        val scriptUtxo1 =
            Utxo(Input(genesisHash, 10), Output(scriptAddress, Value.ada(10), datum42))
        val scriptUtxo2 =
            Utxo(Input(genesisHash, 11), Output(scriptAddress, Value.ada(10), datum42))

        val emulator = Emulator(
          initialUtxos = Map(
            scriptUtxo1.input -> scriptUtxo1.output,
            scriptUtxo2.input -> scriptUtxo2.output,
            Input(genesisHash, 20) ->
                TransactionOutput.Babbage(Alice.address, Value.lovelace(100_000_000L))
          ),
          initialContext = Context.testMainnet()
        )

        // Delayed redeemers compute indices from the final transaction (like auction End action)
        val redeemer1: Transaction => Data = tx => {
            val inputIdx = tx.body.value.inputs.toSeq.indexOf(scriptUtxo1.input)
            val bobIdx = tx.body.value.outputs.indexWhere(_.value.address == Bob.address)
            Data.I(BigInt(inputIdx * 100 + bobIdx))
        }
        val redeemer2: Transaction => Data = tx => {
            val inputIdx = tx.body.value.inputs.toSeq.indexOf(scriptUtxo2.input)
            val charlesIdx = tx.body.value.outputs.indexWhere(_.value.address == Charles.address)
            Data.I(BigInt(inputIdx * 100 + charlesIdx))
        }

        val signedTx = TxBuilder(env)
            .spend(scriptUtxo1, redeemer1, alwaysOkScript, Set.empty)
            .spend(scriptUtxo2, redeemer2, alwaysOkScript, Set.empty)
            .payTo(Bob.address, Value.ada(5))
            .payTo(Charles.address, Value.ada(5))
            .validFrom(Instant.ofEpochSecond(1000))
            .complete(emulator, Alice.address)
            .map(_.sign(Alice.signer).transaction)
            .await()

        val feeInTx = signedTx.body.value.fee
        val minFee = MinTransactionFee
            .computeMinFee(signedTx, emulator.utxos, env.protocolParams)
            .getOrElse(Coin.zero)

        System.err.println(s"Fee in tx: ${feeInTx.value}, min required: ${minFee.value}")
        if feeInTx.value < minFee.value then
            System.err.println(s"Fee shortfall: ${minFee.value - feeInTx.value} lovelace")

        val result = emulator.submit(signedTx).await()
        assert(
          result.isRight,
          s"Multi-script tx fee should be sufficient for emulator: $result " +
              s"(fee=${feeInTx.value}, minFee=${minFee.value})"
        )
    }

    ignore("complete fee should match MinTransactionFee for signed multi-script tx") {
        // Verify that the fee computed by TxBuilder.complete is at least the min fee
        // required for the signed transaction, as computed by MinTransactionFee.
        val scriptUtxo1 =
            Utxo(Input(genesisHash, 10), Output(scriptAddress, Value.ada(10), datum42))
        val scriptUtxo2 =
            Utxo(Input(genesisHash, 11), Output(scriptAddress, Value.ada(10), datum42))

        val emulator = Emulator(
          initialUtxos = Map(
            scriptUtxo1.input -> scriptUtxo1.output,
            scriptUtxo2.input -> scriptUtxo2.output,
            Input(genesisHash, 20) ->
                TransactionOutput.Babbage(Alice.address, Value.lovelace(100_000_000L))
          ),
          initialContext = Context.testMainnet()
        )

        val redeemer1: Transaction => Data = tx => {
            val inputIdx = tx.body.value.inputs.toSeq.indexOf(scriptUtxo1.input)
            val bobIdx = tx.body.value.outputs.indexWhere(_.value.address == Bob.address)
            Data.I(BigInt(inputIdx * 100 + bobIdx))
        }
        val redeemer2: Transaction => Data = tx => {
            val inputIdx = tx.body.value.inputs.toSeq.indexOf(scriptUtxo2.input)
            val charlesIdx = tx.body.value.outputs.indexWhere(_.value.address == Charles.address)
            Data.I(BigInt(inputIdx * 100 + charlesIdx))
        }

        val completed = TxBuilder(env)
            .spend(scriptUtxo1, redeemer1, alwaysOkScript, Set.empty)
            .spend(scriptUtxo2, redeemer2, alwaysOkScript, Set.empty)
            .payTo(Bob.address, Value.ada(5))
            .payTo(Charles.address, Value.ada(5))
            .validFrom(Instant.ofEpochSecond(1000))
            .complete(emulator, Alice.address)
            .await()

        val signedTx = completed.sign(Alice.signer).transaction

        val feeInTx = signedTx.body.value.fee
        val minFee = MinTransactionFee
            .computeMinFee(signedTx, emulator.utxos, env.protocolParams)
            .getOrElse(Coin.zero)

        System.err.println(s"Fee in tx: ${feeInTx.value} lovelace")
        System.err.println(s"Min required: ${minFee.value} lovelace")
        if feeInTx.value < minFee.value then
            System.err.println(
              s"Fee shortfall: ${minFee.value - feeInTx.value} lovelace (~${(minFee.value - feeInTx.value) / 44} bytes)"
            )

        assert(
          feeInTx >= minFee,
          s"Fee in transaction (${feeInTx.value}) should be >= min fee for signed tx (${minFee.value}). " +
              s"Shortfall: ${minFee.value - feeInTx.value} lovelace"
        )
    }
}
