package scalus.examples.atomictransactions

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.Context
import scalus.cardano.node.Emulator
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.testing.kit.ScalusTest
import scalus.testing.kit.TestUtil.{genesisHash, testEnvironment}
import scalus.utils.await

class AtomicTransactionsTest extends AnyFunSuite, ScalusTest {
    private given env: CardanoInfo = testEnvironment
    private val txs = AtomicTransactions(env)

    private def provider(): Emulator =
        Emulator(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> TransactionOutput
                .Babbage(Alice.address, Value.ada(10)),
            TransactionInput(genesisHash, 1) -> TransactionOutput
                .Babbage(Alice.address, Value.ada(10))
          ),
          initialContext = Context.testMainnet()
        )

    test("batchPay spends every sender UTxO in one atomic transaction") {
        val p = provider()
        val aliceUtxos = p.findUtxos(Alice.address).await().toOption.get
        assert(aliceUtxos.size == 2)

        val tx = txs.batchPay(
          senderUtxos = aliceUtxos,
          recipient = Bob.address,
          amount = Coin(5_000_000L),
          changeAddress = Alice.address,
          signer = Alice.signer
        )

        // Both of Alice's UTxOs are inputs — they are consumed all-or-nothing.
        assert(
          aliceUtxos.keySet.forall(in => tx.body.value.inputs.toSeq.contains(in)),
          "every sender UTxO must be an input"
        )

        assert(p.submit(tx).await().isRight, "atomic batch should submit")

        // Bob received exactly the payment.
        val bobPaid = tx.utxos.exists { case (_, out) =>
            out.address == Bob.address && out.value.coin.value == 5_000_000L
        }
        assert(bobPaid, "Bob must receive exactly 5 ADA")
    }
}
