package scalus.cardano.node.stream.ox

import org.scalatest.funsuite.AnyFunSuite
import ox.*
import scalus.cardano.ledger.{CardanoInfo, Input, Output, TransactionException, TransactionHash, Value}
import scalus.cardano.ledger.rules.STS
import scalus.cardano.node.{Emulator, UtxoQuery, UtxoSource}
import scalus.cardano.node.stream.{StartFrom, SubscriptionOptions, UtxoEvent, UtxoEventQuery}
import scalus.cardano.node.stream.engine.EngineTestFixtures.{addressA, addressB}
import scalus.cardano.txbuilder.TxBuilder
import scalus.uplc.builtin.ByteString
import scalus.utils.await

import scala.concurrent.ExecutionContext

/** ox parity tests for [[OxStreamingEmulator]]. Mirrors [[Fs2StreamingEmulatorTest]]. */
class OxStreamingEmulatorTest extends AnyFunSuite {

    given CardanoInfo = CardanoInfo.mainnet
    private given ExecutionContext = ExecutionContext.global

    private val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    private def seed(amount: Long = 1000): Map[Input, Output] =
        Map(Input(genesisHash, 0) -> Output(addressA, Value.ada(amount)))

    // See comment in `Fs2StreamingEmulatorTest` — validators off for happy-path tests.
    private def newEmulator(
        validators: Iterable[STS.Validator] = Set.empty,
        initial: Map[Input, Output] = seed()
    ): Emulator =
        Emulator(
          initialUtxos = initial,
          validators = validators,
          mutators = Emulator.defaultMutators
        )

    private def payAToB(emulator: Emulator, lovelace: Long) =
        TxBuilder(summon[CardanoInfo])
            .payTo(addressB, Value.ada(lovelace))
            .complete(emulator, addressA)
            .await()
            .transaction

    test("submit: successful tx produces Created event at recipient") {
        supervised {
            val emulator = newEmulator()
            val wrapper = new OxStreamingEmulator(emulator)
            try {
                val query = UtxoEventQuery(UtxoQuery(UtxoSource.FromAddress(addressB)))
                val opts = SubscriptionOptions(
                  startFrom = StartFrom.Tip,
                  includeExistingUtxos = false
                )
                // Subscribe on the main thread before any submit — see the ordering
                // invariant documented on `BaseStreamProvider`.
                val flow = wrapper.subscribeUtxoQuery(query, opts)
                val tx = payAToB(emulator, 10)
                val collected: Fork[Seq[UtxoEvent]] = fork(flow.take(1).runToList())

                val result = wrapper.submit(tx)
                assert(result.isRight, s"submit should succeed, got: $result")

                val events = collected.join()
                assert(events.size == 1)
                assert(events.head.isInstanceOf[UtxoEvent.Created])
            } finally wrapper.close()
        }
    }

    test("seed: includeExistingUtxos=true replays live UTxOs as Created at subscribe time") {
        supervised {
            val emulator = newEmulator()
            val wrapper = new OxStreamingEmulator(emulator)
            try {
                val query = UtxoEventQuery(UtxoQuery(UtxoSource.FromAddress(addressA)))
                val opts = SubscriptionOptions(
                  startFrom = StartFrom.Tip,
                  includeExistingUtxos = true
                )
                val events = wrapper.subscribeUtxoQuery(query, opts).take(1).runToList()
                assert(events.size == 1)
                assert(events.head.isInstanceOf[UtxoEvent.Created])
            } finally wrapper.close()
        }
    }

    test("submit: ledger-rule violation (double-spend) is rejected, no event emitted") {
        supervised {
            val emulator = newEmulator()
            val tx = payAToB(emulator, 10)
            assert(emulator.submitSync(tx).isRight)

            val wrapper = new OxStreamingEmulator(emulator)
            try {
                val preTip = wrapper.engine.currentTip
                val result = wrapper.submit(tx)
                assert(result.isLeft, s"double-spend should be rejected, got: $result")
                assert(
                  wrapper.engine.currentTip == preTip,
                  "tip should not advance on rejection"
                )
            } finally wrapper.close()
        }
    }

    test("custom validator: user-supplied rule rejects all submissions") {
        object RejectAll extends STS.Validator {
            override final type Error = TransactionException.IllegalArgumentException
            override def validate(context: Context, state: State, event: Event): Result =
                failure(TransactionException.IllegalArgumentException("rejected by test"))
        }
        supervised {
            val pristine = newEmulator()
            val tx = payAToB(pristine, 10)

            val rejecting = newEmulator(validators = Set(RejectAll))
            val wrapper = new OxStreamingEmulator(rejecting)
            try {
                val result = wrapper.submit(tx)
                assert(result.isLeft, s"custom validator should reject, got: $result")
            } finally wrapper.close()
        }
    }

    test("subscribeTip: successful submit advances the tip") {
        supervised {
            val emulator = newEmulator()
            val wrapper = new OxStreamingEmulator(emulator)
            try {
                val tx = payAToB(emulator, 10)
                val result = wrapper.submit(tx)
                assert(result.isRight)
                val tip = wrapper.engine.currentTip
                assert(tip.nonEmpty, "tip should be advanced after successful submit")
                assert(tip.get.blockNo >= 1L)
            } finally wrapper.close()
        }
    }

    test("newEmptyBlock: advances tip without any transaction events") {
        supervised {
            val emulator = newEmulator()
            val wrapper = new OxStreamingEmulator(emulator)
            try {
                wrapper.newEmptyBlock()
                val tip = wrapper.engine.currentTip
                assert(tip.nonEmpty, "tip should advance on newEmptyBlock")
                assert(tip.get.blockNo >= 1L)
            } finally wrapper.close()
        }
    }
}
