package scalus.examples.streaming.fs2batcher

import cats.effect.{IO, Ref}
import scalus.cardano.address.Address
import scalus.cardano.ledger.{Transaction, Utxo}
import scalus.cardano.node.stream.{
    StartFrom,
    SubscriptionOptions,
    UtxoEvent,
    UtxoEventType
}
import scalus.cardano.node.stream.UtxoEventQueryMacros.buildEventQuery

import scala.concurrent.duration.FiniteDuration

/** Example: an fs2-based collect-batcher that monitors incoming UTxOs at
  * a single watched address and sweeps them into one output once a
  * trigger fires (count or age).
  *
  * Written concretely against `fs2.Stream[IO, *]` — the first consumer
  * adapter we support. An ox variant will live in a sibling `oxbatcher`
  * package.
  *
  * Intended setup: the watched address is a script locked with a
  * validator requiring the operator signature (see
  * `scalus.examples.PubKeyValidator.validatorV3` for the on-chain
  * pattern — the batcher is the operator). Clients deposit UTxOs into
  * the script. The batcher subscribes, batches new UTxOs by count or
  * age via fs2's `groupWithin`, and sweeps each batch to `sweepTo`.
  *
  * This is a skeleton: `buildSweepTx` is stubbed and there is no real
  * provider yet. It exists to exercise the streaming API shape and
  * surface gaps as scaladoc comments inline.
  *
  * Rollback handling is delegated to the subscription: `noRollback =
  * true` in `SubscriptionOptions` means the provider emits only stable
  * events, so the batcher never needs to undo state.
  *
  * The batcher tracks an in-memory `pending` set of UTxOs that have
  * been included in a submitted sweep but not yet observed as `Spent`.
  * In production this set must be persistent (a restart would otherwise
  * re-batch the same UTxOs once the subscription replays them); the
  * in-memory version here is the same shape and exists only so the
  * skeleton compiles and is exercisable.
  */
class CollectBatcher(
    provider: Fs2BlockchainStreamProvider,
    config: CollectBatcherConfig
) {

    /** Start the batcher. The returned `IO` completes when either
      * subscription ends (provider closed, fatal error, etc.).
      */
    def run(startFrom: StartFrom = StartFrom.Tip): IO[Unit] = {
        val watch = config.watchAddress
        val opts = SubscriptionOptions(startFrom = startFrom, noRollback = true)

        // Two subscriptions: one for Created (drives the groupWithin
        // trigger), one for Spent (clears entries from `pending` once
        // our submitted sweeps are observed on chain).
        val createdQuery = buildEventQuery { (u, kind) =>
            u.output.address == watch && kind == UtxoEventType.Created
        }
        val spentQuery = buildEventQuery { (u, kind) =>
            u.output.address == watch && kind == UtxoEventType.Spent
        }

        Ref.of[IO, Set[Utxo]](Set.empty).flatMap { pending =>
            val sweeps = provider
                .subscribeUtxoQuery(createdQuery, opts)
                .collect { case UtxoEvent.Created(u, _, _) => u }
                .groupWithin(config.maxUtxoCount, config.maxAge)
                .evalMap(chunk => submitSweep(chunk.toList, pending))

            val confirmations = provider
                .subscribeUtxoQuery(spentQuery, opts)
                .collect { case UtxoEvent.Spent(u, _, _) => u }
                .evalMap(u => pending.update(_ - u))

            sweeps.concurrently(confirmations).compile.drain
        }
    }

    private def submitSweep(utxos: List[Utxo], pending: Ref[IO, Set[Utxo]]): IO[Unit] = {
        val tx = buildSweepTx(utxos)
        provider.submit(tx).flatMap {
            case Right(_) =>
                // Mark these UTxOs as awaiting confirmation. The Spent
                // subscription removes them as the sweep is observed.
                pending.update(_ ++ utxos)
            case Left(err) =>
                IO.raiseError(new RuntimeException(s"sweep submission failed: $err"))
        }
    }

    /** Build a sweep transaction consuming all pending UTxOs and
      * producing a single output to `config.sweepTo`. Requires protocol
      * params, a signing key, and fee coin-selection — out of scope for
      * this skeleton. Wire up via `TxBuilder` when the example moves to
      * `scalus-examples`.
      */
    private def buildSweepTx(utxos: List[Utxo]): Transaction = ???
}

/** Configuration for the collect batcher.
  *
  * @param watchAddress
  *   address to monitor for incoming UTxOs — typically a script address
  *   whose validator requires the operator signature
  * @param sweepTo
  *   destination for accumulated funds
  * @param maxUtxoCount
  *   sweep when pending UTxO count reaches this value
  * @param maxAge
  *   sweep when the oldest pending UTxO exceeds this age (implemented
  *   via fs2 `groupWithin`'s time bound)
  */
case class CollectBatcherConfig(
    watchAddress: Address,
    sweepTo: Address,
    maxUtxoCount: Int,
    maxAge: FiniteDuration
)
