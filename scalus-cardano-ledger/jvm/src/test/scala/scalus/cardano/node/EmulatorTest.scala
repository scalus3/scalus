package scalus.cardano.node

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.builtin.ByteString
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.Party.{Alice, Bob}
import scalus.cardano.txbuilder.TxBuilder
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global

class EmulatorTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    val testEnv: CardanoInfo = CardanoInfo.mainnet

    test("Emulator.utxos returns all UTXOs") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        val initialUtxos = Map(
          Input(genesisHash, 0) -> TransactionOutput(Alice.address, Value.ada(100)),
          Input(genesisHash, 1) -> TransactionOutput(Bob.address, Value.ada(50))
        )

        val provider = Emulator(
          initialUtxos = initialUtxos,
          validators = Set.empty,
          mutators = Emulator.defaultMutators
        )

        // utxos should return all current UTXOs
        assert(provider.utxos == initialUtxos, "Emulator.utxos should return all UTXOs")

        // Build and submit tx1
        val tx1 = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .await()
            .transaction

        val submitResult = provider.submit(tx1).await()
        assert(submitResult.isRight, s"Transaction should succeed: $submitResult")

        // After tx1, utxos should be updated
        val utxosAfterTx1 = provider.utxos
        assert(utxosAfterTx1.size >= 2, "Should have at least 2 UTXOs after tx1")
        assert(
          utxosAfterTx1.keys.exists(_.transactionId == tx1.id),
          "UTXOs should include outputs from tx1"
        )
    }

    test("Property: submitted valid transaction UTXOs become available") {
        forAll(
          Gen.choose(100L, 1000L),
          Gen.choose(10L, 50L)
        ) { (initialAmount: Long, paymentAmount: Long) =>
            whenever(initialAmount > paymentAmount + 1) {
                val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

                val initialUtxos = Map(
                  Input(genesisHash, 0) -> TransactionOutput(
                    Alice.address,
                    Value.ada(initialAmount)
                  )
                )

                val emulator = Emulator(
                  initialUtxos = initialUtxos,
                  validators = Set.empty,
                  mutators = Emulator.defaultMutators
                )

                val tx = TxBuilder(testEnv)
                    .payTo(Bob.address, Value.ada(paymentAmount))
                    .complete(emulator, Alice.address)
                    .await()
                    .transaction

                val submitResult = emulator.submit(tx).await()

                assert(
                  submitResult.isRight,
                  s"Transaction should be accepted but got: $submitResult"
                )

                // After submission, transaction outputs should be available
                val utxosAfter = emulator.utxos
                tx.body.value.outputs.zipWithIndex.foreach { case (output, idx) =>
                    val txInput = Input(tx.id, idx)
                    assert(
                      utxosAfter.contains(txInput),
                      s"Output $idx from transaction ${tx.id} should be available in UTXOs"
                    )
                    assert(
                      utxosAfter(txInput) == output.value,
                      s"Output $idx value should match"
                    )
                }

                // All inputs should be consumed (removed from UTXO set)
                tx.body.value.inputs.toSeq.foreach { input =>
                    assert(
                      !utxosAfter.contains(input),
                      s"Input $input should be consumed (removed from UTXOs)"
                    )
                }
            }
        }
    }

    test("Property: invalid transaction (double spend) is rejected") {
        forAll(Gen.choose(100L, 1000L)) { initialAmount =>
            val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

            val initialUtxos = Map(
              Input(genesisHash, 0) -> TransactionOutput(
                Alice.address,
                Value.ada(initialAmount)
              )
            )

            val emulator = Emulator(
              initialUtxos = initialUtxos,
              validators = Set.empty,
              mutators = Emulator.defaultMutators
            )

            val tx1 = TxBuilder(testEnv)
                .payTo(Bob.address, Value.ada(10))
                .complete(emulator, Alice.address)
                .await()
                .transaction

            val submit1 = emulator.submit(tx1).await()
            assert(submit1.isRight, "First transaction should succeed")

            val submit2 = emulator.submit(tx1).await()
            assert(submit2.isLeft, "Double spend should be rejected")
        }
    }
}
