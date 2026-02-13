package scalus.cardano.txbuilder

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{Context, PlutusScriptsTransactionMutator}
import scalus.cardano.ledger.utils.MinTransactionFee
import scalus.cardano.node.Emulator
import scalus.testing.kit.Party.{Alice, Bob, Charles}
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global

/** Tests for fee calculation with signatures.
  *
  * Verifies that the fee accounts for VKey witnesses correctly, both for the sponsor signature and
  * for extra signatures added at signing time.
  */
class SponsorSignatureFeeTest extends AnyFunSuite {
    private given env: CardanoInfo = CardanoInfo.mainnet

    private val genesisHash: TransactionHash =
        TransactionHash.fromByteString(scalus.uplc.builtin.ByteString.fromHex("0" * 64))

    test("fee accounts for sponsor signature") {
        val sender = Alice
        val recipient = Bob

        val provider = Emulator(
          initialUtxos = Map(
            Input(genesisHash, 0) ->
                TransactionOutput.Babbage(sender.address, Value.lovelace(100_000_000L))
          ),
          initialContext = Context.testMainnet(),
          mutators = Set(PlutusScriptsTransactionMutator)
        )

        val txBuilder = TxBuilder(env)
            .payTo(recipient.address, Value.lovelace(10_000_000L))
            .complete(provider, sender.address)
            .map(_.sign(sender.signer))
            .await()

        val signedTx = txBuilder.transaction
        val feeInTx = signedTx.body.value.fee
        val utxos = provider.utxos
        val minFeeForSigned =
            MinTransactionFee.computeMinFee(signedTx, utxos, env.protocolParams).toOption.get

        assert(
          feeInTx.value >= minFeeForSigned.value,
          s"Fee ${feeInTx.value} is less than minimum ${minFeeForSigned.value}"
        )
    }

    test("submission succeeds with correct sponsor fee") {
        val sender = Alice
        val recipient = Bob

        val provider = Emulator(
          initialUtxos = Map(
            Input(genesisHash, 0) ->
                TransactionOutput.Babbage(sender.address, Value.lovelace(100_000_000L))
          ),
          initialContext = Context.testMainnet(),
          mutators = Set(PlutusScriptsTransactionMutator)
        )

        val signedTx = TxBuilder(env)
            .payTo(recipient.address, Value.lovelace(10_000_000L))
            .complete(provider, sender.address)
            .map(_.sign(sender.signer).transaction)
            .await()

        val result = provider.submit(signedTx).await()
        assert(result.isRight, s"Submission failed: ${result.left.getOrElse("")}")
    }

    test("sign with extra keys filters to expected signers only") {
        val sender = Alice
        val recipient = Bob

        val provider = Emulator(
          initialUtxos = Map(
            Input(genesisHash, 0) ->
                TransactionOutput.Babbage(sender.address, Value.lovelace(100_000_000L))
          ),
          initialContext = Context.testMainnet(),
          mutators = Set(PlutusScriptsTransactionMutator)
        )

        // Sign with both Alice and Charles keys, but only Alice is a required signer
        val extraSigner = new TransactionSigner(
          Set(Alice.account.paymentKeyPair, Charles.account.paymentKeyPair)
        )

        val signedTx = TxBuilder(env)
            .payTo(recipient.address, Value.lovelace(10_000_000L))
            .complete(provider, sender.address)
            .map(_.sign(extraSigner).transaction)
            .await()

        // The signed tx should only contain the expected signer (Alice),
        // not the extra key (Charles), to avoid fee underestimation.
        val witnessCount = signedTx.witnessSet.vkeyWitnesses.toSet.size
        assert(
          witnessCount == 1,
          s"Expected 1 VKey witness (Alice only), got $witnessCount"
        )

        // Submission must succeed
        val result = provider.submit(signedTx).await()
        assert(result.isRight, s"Submission failed: ${result.left.getOrElse("")}")
    }
}
