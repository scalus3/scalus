package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.Party.{Alice, Bob}

import java.util.concurrent.{Callable, CountDownLatch, Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicReference

/** The JVM emulator accepts submissions from several threads at once. This suite pins what that has
  * to mean, which a sequential test cannot see: whatever order the threads interleave in, the
  * emulator's four applied-transaction views (`appliedTxLog`, `appliedTxIndex`, `appliedTxs`,
  * `datums`) must all describe the same set of transactions, and the UTxO set must be exactly what
  * replaying that set over the initial UTxOs produces.
  *
  * JVM-only on purpose: JavaScript is single-threaded, so the shared test source set - which has to
  * compile for both - cannot host it.
  */
class EmulatorConcurrencyTest extends AnyFunSuite {

    given testEnv: CardanoInfo = CardanoInfo.mainnet

    private val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    private val alice = Alice.address(Network.Mainnet)
    private val bob = Bob.address(Network.Mainnet)

    /** How many independent transactions run at once. Each spends a UTxO nobody else touches, so
      * all of them must be applied.
      */
    private val independentCount = 6

    /** How many transactions race for one single UTxO. Exactly one may win; the rest must be
      * rejected. This is the group that drives a submitter into the compare-and-set retry, where it
      * has to re-validate against the state that won rather than apply against the state it read.
      */
    private val contenderCount = 4

    /** Repeat count. A threading defect shows up as a flake, not as a deterministic failure, so a
      * single pass proves very little.
      */
    private val rounds = 30

    private val initialUtxos: Utxos =
        (0 until independentCount + 1).map { index =>
            Input(genesisHash, index) -> Output(alice, Value.ada(100))
        }.toMap

    /** The UTxO every contender tries to spend: the last one seeded above. */
    private val contendedInput = Input(genesisHash, independentCount)

    private def payment(available: Utxos, amount: Value, datum: Data): Transaction =
        TxBuilder(testEnv)
            .payTo(bob, amount, (_: Transaction) => datum)
            .complete(available, alice)
            .sign(Alice.signer)
            .transaction

    /** Transactions are deterministic in the UTxO set they were built against, so they are built
      * once and replayed against a fresh emulator each round - transaction building and signing
      * cost far more than submission, and none of it is what is under test.
      */
    private val independentTxs: Seq[Transaction] =
        (0 until independentCount).map { index =>
            val input = Input(genesisHash, index)
            payment(Map(input -> initialUtxos(input)), Value.ada(10 + index), Data.I(index))
        }

    private val contenderTxs: Seq[Transaction] = {
        val available = Map(contendedInput -> initialUtxos(contendedInput))
        (0 until contenderCount).map { i =>
            payment(available, Value.ada(20 + i), Data.I(100 + i))
        }
    }

    /** Replay an applied-tx set over the initial UTxOs: remove what each transaction consumed, add
      * what it produced. The applied transactions never conflict with one another (that is the
      * invariant the emulator is meant to keep), so the result does not depend on their order.
      */
    private def replay(applied: Seq[AppliedTx]): Utxos = {
        val spent = applied.flatMap(_.tx.body.value.inputs.toSeq).toSet
        val produced = applied.flatMap { a =>
            a.tx.body.value.outputs.zipWithIndex.map { case (out, index) =>
                Input(a.tx.id, index) -> out.value
            }
        }.toMap
        (initialUtxos -- spent) ++ produced
    }

    /** The three views of the applied-transaction bookkeeping, checked against each other.
      *
      * The no-duplicates assertion is a fact about *this* scenario rather than about
      * [[EmulatorState]] in general: nothing here puts a spent input back, so the ledger rules
      * reject every re-submission and each transaction is applied at most once. The log is
      * append-per-application, and `EmulatorParityTest` pins what happens when the same transaction
      * really is applied twice.
      */
    private def assertCoherent(emulator: Emulator, round: Int): Unit = {
        val log = emulator.appliedTxLog
        val index = emulator.appliedTxIndex
        val txs = emulator.appliedTxs
        val hint = s"round $round"

        assert(
          log.map(_.txHash).distinct.size == log.size,
          s"$hint: the applied-tx log must not contain the same transaction twice: ${log.map(_.txHash)}"
        )
        assert(
          txs == log.map(_.txHash).toSet,
          s"$hint: every hash in appliedTxs must appear in the log, and vice versa"
        )
        assert(
          index.keySet == txs,
          s"$hint: every hash in appliedTxs must have an index entry"
        )
        for applied <- log do
            assert(
              index.get(applied.txHash).contains(applied),
              s"$hint: the index entry for ${applied.txHash} must be the log's record"
            )

        assert(
          emulator.utxos == replay(log),
          s"$hint: the UTxO set must be exactly the applied set replayed over the initial UTxOs"
        )

        val expectedDatums = log.foldLeft(Map.empty[DataHash, Data]) { (acc, a) =>
            acc ++ EmulatorBase.extractDatums(a.tx)
        }
        assert(
          expectedDatums.forall { case (hash, datum) => emulator.datums.get(hash).contains(datum) },
          s"$hint: every applied transaction's datums must be present"
        )
    }

    test("concurrent submissions keep the applied set, the index, the log and the UTxOs in step") {
        val threads = independentCount + contenderCount
        val pool = Executors.newFixedThreadPool(threads)
        try
            for round <- 1 to rounds do {
                val emulator = Emulator(initialUtxos = initialUtxos)
                val start = new CountDownLatch(1)
                val failure = new AtomicReference[Throwable](null)

                val tasks = (independentTxs ++ contenderTxs).map { tx =>
                    new Callable[Either[SubmitError, TransactionHash]] {
                        def call(): Either[SubmitError, TransactionHash] = {
                            try {
                                start.await()
                                emulator.submitSync(tx)
                            } catch {
                                case t: Throwable =>
                                    failure.compareAndSet(null, t)
                                    throw t
                            }
                        }
                    }
                }

                val futures = tasks.map(task => pool.submit(task))
                start.countDown()
                val results = futures.map(_.get(60, TimeUnit.SECONDS))

                Option(failure.get()).foreach(t => fail(s"round $round: a submitter threw", t))

                val accepted = results.collect { case Right(hash) => hash }
                assert(
                  accepted.size == independentCount + 1,
                  s"round $round: all $independentCount independent transactions and exactly one " +
                      s"of the $contenderCount contenders must be applied, got ${accepted.size}"
                )
                assert(
                  independentTxs.map(_.id).toSet.subsetOf(accepted.toSet),
                  s"round $round: a transaction spending an uncontended UTxO must never be rejected"
                )
                assert(
                  contenderTxs.map(_.id).toSet.intersect(accepted.toSet).size == 1,
                  s"round $round: exactly one contender may spend the contended UTxO"
                )
                assert(
                  emulator.appliedTxs == accepted.toSet,
                  s"round $round: the emulator's applied set must be what the submitters were told"
                )
                assertCoherent(emulator, round)
            }
        finally {
            pool.shutdownNow()
            pool.awaitTermination(30, TimeUnit.SECONDS)
        }
    }

    test("a concurrent setSlot never leaves a transaction stamped with a slot nobody set") {
        val pool = Executors.newFixedThreadPool(independentCount + 1)
        try
            for round <- 1 to rounds do {
                val emulator = Emulator(initialUtxos = initialUtxos)
                val start = new CountDownLatch(1)

                val slotTask = new Callable[Unit] {
                    def call(): Unit = {
                        start.await()
                        for slot <- 1L to 50L do emulator.setSlot(slot)
                    }
                }
                val submitTasks = independentTxs.map { tx =>
                    new Callable[Unit] {
                        def call(): Unit = {
                            start.await()
                            assert(emulator.submitSync(tx).isRight)
                        }
                    }
                }

                val futures = (slotTask +: submitTasks).map(task => pool.submit(task))
                start.countDown()
                futures.foreach(_.get(60, TimeUnit.SECONDS))

                assert(emulator.currentSlotSync == 50L, s"round $round: the last slot set wins")

                // Not "every slot is in [0, 50]": that is the whole reachable set, so nothing can
                // fail it. What the atomic state cell actually buys is this - the slot stamped on
                // a record is the slot of the state the record was written into, and the only
                // writer moves the slot forward, so log order and slot order agree. An emulator
                // that read the slot outside the compare-and-set instead of off the state it was
                // handed could stamp a record with a slot from a state later than its own, and
                // that shows up here as a slot going backwards.
                val slots = emulator.appliedTxLog.map(_.slot)
                assert(
                  slots == slots.sorted,
                  s"round $round: applied slots must not go backwards in log order, got $slots"
                )
                assertCoherent(emulator, round)
            }
        finally {
            pool.shutdownNow()
            pool.awaitTermination(30, TimeUnit.SECONDS)
        }
    }
}
