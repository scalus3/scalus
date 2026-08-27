package scalus.cardano.node.stream

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString
import scalus.cardano.ledger.*
import scalus.cardano.node.{Emulator, UtxoQuery, UtxoSource}
import scalus.testing.kit.Party.Alice

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

/** The delta buffer carries the whole flow-control story, so its two decisions are pinned here.
  *
  * There is no backpressure to be had from a chain — it produces blocks whether or not a subscriber
  * keeps up. That leaves only *how much to hold* and *what to do when that is not enough*, and the
  * answer to the second is never "drop": a missed `Spent` corrupts a subscriber's view of state
  * permanently and invisibly. Overflow behaviour itself is pinned in `MailboxTest`.
  */
class DeltaBufferPolicyTest extends AnyFunSuite {

    given testEnv: CardanoInfo = CardanoInfo.mainnet

    private val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    private def emulatorWith(utxoCount: Int): Emulator = {
        val utxos = (0 until utxoCount).map { i =>
            TransactionInput(genesisHash, i) -> TransactionOutput(Alice.address, Value.ada(10))
        }.toMap
        Emulator(utxos)
    }

    test(
      "the default policy is bounded, so a runaway subscriber fails instead of exhausting heap"
    ) {
        assert(
          DeltaBufferPolicy.default == DeltaBufferPolicy.Bounded(DeltaBufferPolicy.defaultBound),
          "an unbounded default turns a stalled consumer into an OOM that names no subscription"
        )
    }

    test("the snapshot seed does not count against the live bound") {
        // The seed is larger than the bound. It is the subscription's initial state, not evidence
        // of a consumer falling behind, so a wallet with more UTxOs than the bound must still be
        // able to subscribe and receive all of them.
        val seeded = 8
        val provider = new StreamingEmulator(emulatorWith(seeded))
        val opts = SubscriptionOptions(
          includeExistingUtxos = true,
          bufferPolicy = DeltaBufferPolicy.Bounded(2)
        )
        val query = UtxoEventQuery(UtxoQuery(UtxoSource.FromAddress(Alice.address)))

        val src: ScalusAsyncSource[UtxoEvent] = provider.subscribeUtxoQuery(query, opts)

        val delivered = (1 to seeded).map(_ => Await.result(src.pull(), 5.seconds))
        assert(
          delivered.forall(_.isDefined),
          s"all $seeded seeded events should survive a bound of 2, got: $delivered"
        )
        src.cancel()
    }
}
