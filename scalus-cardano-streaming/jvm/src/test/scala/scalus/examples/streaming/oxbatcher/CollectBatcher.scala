package scalus.examples.streaming.oxbatcher

import ox.*
import scalus.cardano.address.Address
import scalus.cardano.ledger.{Transaction, Utxo}
import scalus.cardano.node.stream.{StartFrom, SubscriptionOptions, UtxoEvent, UtxoEventType}
import scalus.cardano.node.stream.UtxoEventQueryMacros.buildEventQuery

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.FiniteDuration

/** Example: an ox-based collect-batcher.
  *
  * Direct-style equivalent of the fs2 batcher in the sibling `fs2batcher` package. Same logical
  * structure, different runtime:
  *
  *   - one `forkUser` runs the Spent subscription, removing entries from the in-memory `pending`
  *     set as our submitted sweeps are observed on chain
  *   - the main path runs the Created subscription, batched via
  *     `Flow.groupedWithin(maxCount, maxAge)`, and submits each batch as a sweep transaction
  *
  * Rollback handling is delegated to the subscription via `SubscriptionOptions.noRollback = true`.
  * Errors propagate through `supervised`.
  *
  * This is a skeleton: `buildSweepTx` is `???` and there is no real provider yet. It exercises the
  * streaming API in the ox runtime to mirror the fs2 example.
  */
class CollectBatcher(
    provider: OxBlockchainStreamProvider,
    config: CollectBatcherConfig
) {

    /** Start the batcher. Blocks the calling thread until either the Created or Spent subscription
      * ends (provider closed, fatal error, etc.).
      */
    def run(startFrom: StartFrom = StartFrom.Tip): Unit = {
        val watch = config.watchAddress
        val opts = SubscriptionOptions(startFrom = startFrom, noRollback = true)

        val createdQuery = buildEventQuery { (u, kind) =>
            u.output.address == watch && kind == UtxoEventType.Created
        }
        val spentQuery = buildEventQuery { (u, kind) =>
            u.output.address == watch && kind == UtxoEventType.Spent
        }

        val pending = new AtomicReference[Set[Utxo]](Set.empty)

        supervised {
            forkUser {
                provider
                    .subscribeUtxoQuery(spentQuery, opts)
                    .collect { case UtxoEvent.Spent(u, _, _) => u }
                    .runForeach(u => { pending.updateAndGet(_ - u); () })
            }

            provider
                .subscribeUtxoQuery(createdQuery, opts)
                .collect { case UtxoEvent.Created(u, _, _) => u }
                .groupedWithin(config.maxUtxoCount, config.maxAge)
                .runForeach(batch => submitSweep(batch.toList, pending))
        }
    }

    private def submitSweep(
        utxos: List[Utxo],
        pending: AtomicReference[Set[Utxo]]
    ): Unit =
        provider.submit(buildSweepTx(utxos)) match {
            case Right(_) =>
                pending.updateAndGet(_ ++ utxos): Unit
            case Left(err) =>
                throw new RuntimeException(s"sweep submission failed: $err")
        }

    /** Build a sweep transaction consuming all pending UTxOs and producing a single output to
      * `config.sweepTo`. Out of scope for the skeleton — wire up via `TxBuilder` when the example
      * moves to `scalus-examples`.
      */
    private def buildSweepTx(utxos: List[Utxo]): Transaction = ???
}

/** Configuration for the ox collect batcher.
  *
  * @param watchAddress
  *   address to monitor for incoming UTxOs — typically a script address whose validator requires
  *   the operator signature
  * @param sweepTo
  *   destination for accumulated funds
  * @param maxUtxoCount
  *   sweep when pending UTxO count reaches this value
  * @param maxAge
  *   sweep when the oldest pending UTxO exceeds this age (implemented via ox `Flow.groupedWithin`'s
  *   time bound)
  */
case class CollectBatcherConfig(
    watchAddress: Address,
    sweepTo: Address,
    maxUtxoCount: Int,
    maxAge: FiniteDuration
)
