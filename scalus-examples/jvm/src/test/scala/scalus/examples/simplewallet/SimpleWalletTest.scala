package scalus.examples.simplewallet

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.Context
import scalus.cardano.node.Emulator
import scalus.testing.kit.Party.{Alice, Bob, Charles, Dave}
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.testing.kit.TestUtil.{genesisHash, testEnvironment}
import scalus.utils.await

class SimpleWalletTest extends AnyFunSuite, ScalusTest {
    private given env: CardanoInfo = testEnvironment
    private val wallet = SimpleWalletTransactions(env)

    private def aliceProvider(): Emulator =
        Emulator(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> TransactionOutput
                .Babbage(Alice.address, Value.ada(10)),
            TransactionInput(genesisHash, 1) -> TransactionOutput
                .Babbage(Alice.address, Value.ada(10))
          ),
          initialContext = Context.testMainnet()
        )

    test("transfer pays the recipient and returns change to the owner") {
        val p = aliceProvider()
        val utxos = p.findUtxos(Alice.address).await().toOption.get

        val tx = wallet.transfer(
          ownerUtxos = utxos,
          recipient = Bob.address,
          amount = Coin(3_000_000L),
          owner = Alice.address,
          signer = Alice.signer
        )
        assert(p.submit(tx).await().isRight, "transfer should submit")
        assert(
          tx.utxos.exists { case (_, o) =>
              o.address == Bob.address && o.value.coin.value == 3_000_000L
          },
          "Bob must receive 3 ADA"
        )
    }

    test("withdrawAll spends every owner UTxO and sends the balance to the recipient") {
        val p = aliceProvider()
        val utxos = p.findUtxos(Alice.address).await().toOption.get

        val tx =
            wallet.withdrawAll(ownerUtxos = utxos, recipient = Bob.address, signer = Alice.signer)
        assert(p.submit(tx).await().isRight, "withdrawAll should submit")

        // Every owner UTxO is consumed and nothing is left at Alice's address.
        assert(utxos.keySet.forall(in => tx.body.value.inputs.toSeq.contains(in)))
        assert(p.findUtxos(Alice.address).await().toOption.get.isEmpty, "owner should be emptied")
    }

    test("2-of-3 multisig: any two owners can spend") {
        val multisig = MultiSigWallet(
          env,
          owners = IndexedSeq(Alice.addrKeyHash, Bob.addrKeyHash, Charles.addrKeyHash),
          required = 2
        )
        val walletInput = TransactionInput(genesisHash, 0)
        val p = Emulator(
          initialUtxos = Map(
            walletInput -> TransactionOutput.Babbage(multisig.address, Value.ada(10))
          ),
          initialContext = Context.testMainnet()
        )
        val walletUtxo = p.findUtxos(multisig.address).await().toOption.get.head

        // Alice + Bob sign (2 of 3) — accepted.
        val ok = multisig.transfer(
          walletUtxo = Utxo(walletUtxo),
          recipient = Dave.address,
          amount = Coin(3_000_000L),
          requiredSigners = Set(Alice.addrKeyHash, Bob.addrKeyHash),
          signers = Seq(Alice.signer, Bob.signer)
        )
        assert(p.submit(ok).await().isRight, "two owners should be able to spend")

        // Only Alice signs (1 of 3) — rejected by the native script.
        val p2 = Emulator(
          initialUtxos = Map(
            walletInput -> TransactionOutput.Babbage(multisig.address, Value.ada(10))
          ),
          initialContext = Context.testMainnet()
        )
        val walletUtxo2 = p2.findUtxos(multisig.address).await().toOption.get.head
        val tooFew = multisig.transfer(
          walletUtxo = Utxo(walletUtxo2),
          recipient = Dave.address,
          amount = Coin(3_000_000L),
          requiredSigners = Set(Alice.addrKeyHash),
          signers = Seq(Alice.signer)
        )
        assert(p2.submit(tooFew).await().isLeft, "one owner must not be enough")
    }
}
