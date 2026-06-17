package scalus.examples.anonymousdata

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString.utf8
import scalus.uplc.builtin.Data
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.Context
import scalus.cardano.node.Emulator
import scalus.testing.kit.Party.Alice
import scalus.testing.kit.ScalusTest
import scalus.testing.kit.TestUtil.{genesisHash, testEnvironment}
import scalus.utils.await

class AnonymousDataTest extends AnyFunSuite with ScalusTest {

    private given env: CardanoInfo = testEnvironment
    private val txs = AnonymousDataTransactions(env)

    // A deliberately low-entropy secret — the kind that would be brute-forceable without a nonce.
    private val data: Data = Data.B(utf8"vote: yes")
    private val nonce = utf8"f3c1a9e07b2d48569a01ffbe2c7d3a64"

    private def createProvider(): Emulator =
        Emulator(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> TransactionOutput
                .Babbage(Alice.address, Value.ada(5000)),
            TransactionInput(genesisHash, 1) -> TransactionOutput
                .Babbage(Alice.address, Value.ada(5000))
          ),
          initialContext = Context.testMainnet()
        )

    private def storedUtxo(tx: Transaction, hash: DataHash): Utxo =
        tx.utxos
            .collectFirst {
                case entry @ (_, out) if out.datumOption.contains(DatumOption.Hash(hash)) =>
                    Utxo(entry)
            }
            .getOrElse(
              fail("Committed UTxO with the datum hash not found in the store transaction")
            )

    test("store writes only the datum hash — the data itself is not on-chain") {
        val provider = createProvider()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get

        val tx = txs.store(
          utxos,
          data,
          nonce,
          Coin(2_000_000L),
          Alice.address,
          Alice.address,
          Alice.signer
        )
        val result = provider.submit(tx).await()
        assert(result.isRight, s"store should succeed: $result")

        val committed = storedUtxo(tx, txs.commitmentHash(nonce, data))
        // Only the 32-byte hash is present; the preimage (the data) is nowhere on-chain.
        assert(
          committed.output.datumOption.contains(DatumOption.Hash(txs.commitmentHash(nonce, data)))
        )
        assert(committed.output.inlineDatum.isEmpty, "the data must not be stored inline")
    }

    test("retrieve: only the correct (nonce, data) preimage opens the entry") {
        val provider = createProvider()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val tx = txs.store(
          utxos,
          data,
          nonce,
          Coin(2_000_000L),
          Alice.address,
          Alice.address,
          Alice.signer
        )
        provider.submit(tx).await()
        val committed = storedUtxo(tx, txs.commitmentHash(nonce, data))

        assert(
          txs.open(committed, nonce, data).contains(data),
          "correct preimage must open the entry"
        )
        assert(txs.open(committed, utf8"wrong-nonce", data).isEmpty, "wrong nonce must not open it")
        assert(
          txs.open(committed, nonce, Data.B(utf8"vote: no")).isEmpty,
          "wrong data must not open it"
        )
    }

    test("the nonce hides the data: same data under different nonces is unlinkable") {
        val h1 = txs.commitmentHash(utf8"nonce-1", data)
        val h2 = txs.commitmentHash(utf8"nonce-2", data)
        assert(h1 != h2, "the same data under different nonces must produce different hashes")

        // And the commitment is deterministic for a fixed preimage (so retrieval can verify it).
        assert(txs.commitmentHash(nonce, data) == txs.commitmentHash(nonce, data))
    }
}
