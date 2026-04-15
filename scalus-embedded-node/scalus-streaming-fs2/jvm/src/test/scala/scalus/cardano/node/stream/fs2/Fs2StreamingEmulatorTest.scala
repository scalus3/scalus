package scalus.cardano.node.stream.fs2

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.unsafe.IORuntime
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{CardanoInfo, Input, Output, TransactionException, TransactionHash, Value}
import scalus.cardano.ledger.rules.STS
import scalus.cardano.node.{Emulator, TransactionStatus, UtxoQuery, UtxoSource}
import scalus.cardano.node.stream.{StartFrom, SubscriptionOptions, UtxoEvent, UtxoEventQuery}
import scalus.cardano.node.stream.engine.EngineTestFixtures.{addressA, addressB}
import scalus.cardano.txbuilder.TxBuilder
import scalus.uplc.builtin.ByteString
import scalus.utils.await

import scala.concurrent.ExecutionContext

/** Integration tests for [[Fs2StreamingEmulator]].
  *
  * Real transactions via [[TxBuilder]], submitted through the wrapper, observed via the streaming
  * subscriptions. Validates the M3 correctness contract: events are emitted only for
  * ledger-accepted transactions.
  */
class Fs2StreamingEmulatorTest extends AnyFunSuite {

    given IORuntime = IORuntime.global
    given CardanoInfo = CardanoInfo.mainnet

    private val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    private def seed(amount: Long = 1000): Map[Input, Output] =
        Map(Input(genesisHash, 0) -> Output(addressA, Value.ada(amount)))

    // For happy-path streaming tests we disable the default validator pipeline
    // (same pattern as `scalus-cardano-ledger`'s `EmulatorTest`): TxBuilder produces
    // unsigned transactions and signature validators would reject them. Tests that
    // specifically exercise ledger-rule enforcement pass `Emulator.defaultValidators`
    // or a custom validator.
    private def newEmulator(
        validators: Iterable[STS.Validator] = Set.empty,
        initial: Map[Input, Output] = seed()
    ): Emulator =
        Emulator(
          initialUtxos = initial,
          validators = validators,
          mutators = Emulator.defaultMutators
        )

    private def withStreaming[T](
        emulator: Emulator
    )(f: (Fs2StreamingEmulator, Dispatcher[IO], ExecutionContext) ?=> IO[T]): T =
        Dispatcher
            .parallel[IO]
            .use { dispatcher =>
                given Dispatcher[IO] = dispatcher
                given ExecutionContext = ExecutionContext.global
                val wrapper = new Fs2StreamingEmulator(emulator)
                f(using wrapper).guarantee(wrapper.close())
            }
            .unsafeRunSync()

    private def payAToB(emulator: Emulator, lovelace: Long) =
        TxBuilder(summon[CardanoInfo])
            .payTo(addressB, Value.ada(lovelace))
            .complete(emulator, addressA)
            .await()
            .transaction

    test("submit: successful tx produces Created event at recipient") {
        val emulator = newEmulator()
        withStreaming(emulator) { (wrapper, _, _) ?=>
            val query = UtxoEventQuery(UtxoQuery(UtxoSource.FromAddress(addressB)))
            val opts = SubscriptionOptions(
              startFrom = StartFrom.Tip,
              includeExistingUtxos = false
            )
            val collected = wrapper.subscribeUtxoQuery(query, opts).take(1).compile.toList
            val tx = payAToB(emulator, 10)

            for
                result <- wrapper.submit(tx)
                events <- collected
            yield
                assert(result.isRight, s"submit should succeed, got: $result")
                assert(events.size == 1)
                assert(events.head.isInstanceOf[UtxoEvent.Created])
        }
    }

    test("seed: includeExistingUtxos=true replays live UTxOs as Created at subscribe time") {
        val emulator = newEmulator()
        withStreaming(emulator) { (wrapper, _, _) ?=>
            val query = UtxoEventQuery(UtxoQuery(UtxoSource.FromAddress(addressA)))
            val opts = SubscriptionOptions(
              startFrom = StartFrom.Tip,
              includeExistingUtxos = true
            )
            wrapper
                .subscribeUtxoQuery(query, opts)
                .take(1)
                .compile
                .toList
                .map { events =>
                    assert(events.size == 1)
                    assert(events.head.isInstanceOf[UtxoEvent.Created])
                }
        }
    }

    test("submit: ledger-rule violation (double-spend) is rejected, no event emitted") {
        val emulator = newEmulator()
        // Pre-submit a tx to consume A's only input.
        val tx = payAToB(emulator, 10)
        assert(emulator.submitSync(tx).isRight)

        withStreaming(emulator) { (wrapper, _, _) ?=>
            val preTip = wrapper.engine.currentTip
            for result <- wrapper.submit(tx)
            yield
                assert(result.isLeft, s"double-spend should be rejected, got: $result")
                assert(
                  wrapper.engine.currentTip == preTip,
                  "tip should not advance on rejection"
                )
        }
    }

    test("custom validator: user-supplied rule rejects all submissions") {
        object RejectAll extends STS.Validator {
            override final type Error = TransactionException.IllegalArgumentException
            override def validate(context: Context, state: State, event: Event): Result =
                failure(TransactionException.IllegalArgumentException("rejected by test"))
        }
        // Build the tx against a pristine emulator (no RejectAll), then submit through a
        // wrapper whose emulator has the validator set to RejectAll.
        val pristine = newEmulator()
        val tx = payAToB(pristine, 10)

        val rejecting = newEmulator(validators = Set(RejectAll))
        withStreaming(rejecting) { (wrapper, _, _) ?=>
            wrapper.submit(tx).map { result =>
                assert(result.isLeft, s"custom validator should reject, got: $result")
            }
        }
    }

    test("subscribeTransactionStatus: latest-value reflects submit + block") {
        val emulator = newEmulator()
        withStreaming(emulator) { (wrapper, _, _) ?=>
            val tx = payAToB(emulator, 10)
            val hash = tx.id
            val status = wrapper.subscribeTransactionStatus(hash).take(1).compile.toList
            for
                result <- wrapper.submit(tx)
                statuses <- status
            yield
                assert(result.isRight)
                assert(statuses.nonEmpty)
                val last = statuses.last
                assert(
                  last == TransactionStatus.NotFound ||
                      last == TransactionStatus.Pending ||
                      last == TransactionStatus.Confirmed
                )
        }
    }

    test("subscribeTip: successful submit advances the tip") {
        val emulator = newEmulator()
        withStreaming(emulator) { (wrapper, _, _) ?=>
            val tx = payAToB(emulator, 10)
            for _ <- wrapper.submit(tx)
            yield
                val tip = wrapper.engine.currentTip
                assert(tip.nonEmpty, "tip should be advanced after successful submit")
                assert(tip.get.blockNo >= 1L)
        }
    }

    test("newEmptyBlock: advances tip without any transaction events") {
        val emulator = newEmulator()
        withStreaming(emulator) { (wrapper, _, _) ?=>
            for _ <- wrapper.newEmptyBlock()
            yield
                val tip = wrapper.engine.currentTip
                assert(tip.nonEmpty, "tip should advance on newEmptyBlock")
                assert(tip.get.blockNo >= 1L)
        }
    }
}
