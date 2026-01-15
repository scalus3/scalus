package scalus.cardano.txbuilder

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{Context, PlutusScriptsTransactionMutator}
import scalus.cardano.ledger.utils.MinTransactionFee
import scalus.cardano.node.Emulator
import scalus.serialization.cbor.Cbor
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global

/** Test demonstrating the sponsor signature fee calculation bug.
  *
  * BUG: The TxBuilder adds the sponsor to expectedSigners AFTER finalizeContext (line 1609-1612),
  * but finalizeContext uses expectedSigners.size to add dummy signatures for fee calculation
  * (TransactionBuilder.scala line 514). This means the fee is underestimated by approximately the
  * size of one VKey witness (~100 bytes = ~4400 lovelace).
  *
  * FIX: Move sponsor extraction BEFORE calling finalizeContext in TxBuilder.completeLoop.
  */
class SponsorSignatureFeeTest extends AnyFunSuite {
    private given env: CardanoInfo = CardanoInfo.mainnet

    private val genesisHash: TransactionHash =
        TransactionHash.fromByteString(scalus.builtin.ByteString.fromHex("0" * 64))

    ignore("BUG: Fee calculation does not account for sponsor signature") {
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

        // Build a simple payment transaction
        val txBuilder = TxBuilder(env)
            .payTo(recipient.address, Value.lovelace(10_000_000L))
            .complete(provider, sender.address)
            .map(_.sign(sender.signer))
            .await()

        val unsignedTx = TxBuilder(env)
            .payTo(recipient.address, Value.lovelace(10_000_000L))
            .complete(provider, sender.address)
            .await()
            .transaction

        val signedTx = txBuilder.transaction

        // Get sizes
        val unsignedSize = Cbor.encode(unsignedTx).length
        val signedSize = Cbor.encode(signedTx).length
        val sizeDifference = signedSize - unsignedSize

        // Get the fee that was calculated
        val feeInTx = signedTx.body.value.fee

        // Calculate what the min fee should be for the signed transaction
        val utxos = provider.utxos
        val minFeeForSigned =
            MinTransactionFee.computeMinFee(signedTx, utxos, env.protocolParams).toOption.get

        // The bug: fee was calculated for unsigned tx, but signed tx is larger
        val feeShortfall = minFeeForSigned.value - feeInTx.value

        // Report
        info(s"Unsigned tx size: $unsignedSize bytes")
        info(s"Signed tx size: $signedSize bytes")
        info(s"Size difference (signature): $sizeDifference bytes")
        info(s"Fee in transaction: ${feeInTx.value} lovelace")
        info(s"Min fee for signed tx: ${minFeeForSigned.value} lovelace")
        info(s"Fee shortfall: $feeShortfall lovelace")

        // This should fail, demonstrating the bug
        // The fee in the tx should be >= min fee for the signed tx
        if feeShortfall > 0 then
            fail(
              s"BUG CONFIRMED: Fee is short by $feeShortfall lovelace. " +
                  s"Signature adds $sizeDifference bytes but fee wasn't adjusted."
            )
        else info("Fee calculation is correct (bug may have been fixed)")
    }

    ignore("Sponsor signature fee shortfall causes submission failure") {
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

        // Build and sign
        val signedTx = TxBuilder(env)
            .payTo(recipient.address, Value.lovelace(10_000_000L))
            .complete(provider, sender.address)
            .map(_.sign(sender.signer).transaction)
            .await()

        // Submit - this may fail due to fee shortfall
        val result = provider.submit(signedTx).await()

        result match
            case Right(_) =>
                info("Submission succeeded (fee may have been sufficient)")
            case Left(error) =>
                if error.toString.contains("fee") then
                    fail(s"BUG CONFIRMED: Submission failed due to fee issue: $error")
                else fail(s"Submission failed for other reason: $error")
    }
}
