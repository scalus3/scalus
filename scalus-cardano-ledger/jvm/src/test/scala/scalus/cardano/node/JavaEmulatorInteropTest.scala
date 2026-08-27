package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.utils.await

/** Drives [[JavaEmulatorInterop]] — the compile-time proof that the Emulator is usable from real
  * Java — and checks the results at runtime.
  */
class JavaEmulatorInteropTest extends AnyFunSuite {

    given testEnv: CardanoInfo = CardanoInfo.mainnet
    private val genesisHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    test("Java creates an emulator, submits, and inspects state") {
        val emulator =
            JavaEmulatorInterop.createWithAddresses(java.util.List.of(Alice.address, Bob.address))
        assert(JavaEmulatorInterop.utxoCount(emulator) == 2)
        assert(JavaEmulatorInterop.utxosAt(emulator, Alice.address).size() == 1)

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(emulator, Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

        val result = JavaEmulatorInterop.submit(emulator, tx)
        assert(result.isSuccess, s"expected success: ${result.getErrorMessageOrNull}")
        assert(result.getTxHashOrNull == tx.id)
        assert(result.getErrorOrNull == null)
        assert(JavaEmulatorInterop.lookupOrNull(emulator, tx.id) == tx)
        assert(JavaEmulatorInterop.lookupOrNull(emulator, genesisHash) == null)

        // Resubmitting consumes already-spent inputs — an expected failure, no try/catch needed.
        val rejected = JavaEmulatorInterop.submit(emulator, tx)
        assert(!rejected.isSuccess)
        assert(rejected.getTxHashOrNull == null)
        assert(rejected.getErrorOrNull != null)
        assert(rejected.getErrorMessageOrNull != null)
    }

    test("Java submits via CompletableFuture") {
        val emulator =
            JavaEmulatorInterop.createWithAddresses(java.util.List.of(Alice.address, Bob.address))
        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(emulator, Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction
        val result = JavaEmulatorInterop.submitAsync(emulator, tx)
        assert(result.isSuccess)
    }

    test("Java builds initial state via the builder and controls slots") {
        val input = Input(genesisHash, 0)
        val output = Output(Alice.address, Value.ada(100))
        val emulator = JavaEmulatorInterop.createFromBuilder(input, output)

        assert(JavaEmulatorInterop.utxoCount(emulator) == 1)
        assert(JavaEmulatorInterop.utxosAt(emulator, Alice.address).get(0) == Utxo(input, output))
        assert(JavaEmulatorInterop.advanceTo(emulator, 100L) == 105L)
        assert(JavaEmulatorInterop.snapshotUtxoCount(emulator) == 1)
    }
}
