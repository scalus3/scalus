package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.TestPeer.{Alice, Bob}
import scalus.cardano.txbuilder.TxBuilder
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global

class EmulatorTest extends AnyFunSuite {

    val testEnv: CardanoInfo = CardanoInfo.mainnet

    test("Emulator.utxos returns all UTXOs") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        val initialUtxos = Map(
          TransactionInput(genesisHash, 0) -> TransactionOutput(Alice.address, Value.ada(100)),
          TransactionInput(genesisHash, 1) -> TransactionOutput(Bob.address, Value.ada(50))
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

        provider.submit(tx1).await()

        // After tx1, utxos should be updated
        val utxosAfterTx1 = provider.utxos
        assert(utxosAfterTx1.size >= 2, "Should have at least 2 UTXOs after tx1")
        assert(
          utxosAfterTx1.keys.exists(_.transactionId == tx1.id),
          "UTXOs should include outputs from tx1"
        )
    }
}
