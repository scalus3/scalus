package scalus.examples.platform

import scalus.cardano.ledger.{AssetName, PolicyId, ScriptHash, Transaction, TransactionHash, TransactionInput, TransactionOutput}

import scala.concurrent.duration.*

/** Aspirational **Scalus Application Runtime (ARS)** API, as described in the Scalus 2026 proposal.
  *
  * This is the platform-provided surface that applications build on. It makes two things trivial:
  *
  *   1. Subscribing to a rich on-chain filter in a single fluent expression (see [[UtxoWatch]]).
  *   2. Submitting a reaction transaction under a durable resubmission policy (see [[Runtime.submit]]).
  *
  * The implementations here are stubs (`???`) — the goal is to compile and read as the intended
  * developer experience, not to run. Application code lives separately (see `App.scala`).
  */

/** A native asset: policy id + asset name, e.g. a position-marker NFT. */
final case class Asset(policy: PolicyId, name: AssetName)

/** Backoff schedule for transaction resubmission. */
sealed trait Backoff
object Backoff {

    /** Exponential backoff: wait `start`, then double each attempt, never exceeding `cap`. */
    def exponential(start: Duration, cap: Duration): Backoff = ???

    /** Constant delay between attempts. */
    def fixed(every: Duration): Backoff = ???
}

/** Outcome of submitting a transaction under a resubmission policy. */
enum SubmitOutcome {

    /** Reached the requested number of confirmations. */
    case Confirmed(tx: TransactionHash)

    /** Was included, then dropped by a chain rollback. The handler may rebuild and resubmit. */
    case RolledBack(tx: TransactionHash)

    /** Backoff window elapsed without confirmation. */
    case GaveUp(reason: String)
}

/** A chain event for a UTxO that matches a subscription, carrying its decoded datum `D`. */
enum UtxoEvent[+D] {

    /** A matching UTxO was produced (e.g. a position was opened or updated). */
    case Produced(output: TransactionOutput, datum: D, ref: TransactionInput)

    /** A previously matching UTxO was spent (e.g. a position was closed). */
    case Spent(ref: TransactionInput)
}

/** A live subscription handle. The runtime keeps it alive and persists its cursor, so it resumes
  * from the last processed point across restarts (ARS2/ARS3 durability).
  */
trait Subscription {
    def cancel(): Unit
}

/** Untyped UTxO subscription builder. Each clause AND-composes onto the filter. */
trait UtxoWatch {

    /** Only UTxOs locked at this script hash. */
    def atScript(hash: ScriptHash): UtxoWatch

    /** Only UTxOs whose value contains this asset (e.g. a beacon/position NFT). */
    def containingAsset(asset: Asset): UtxoWatch

    /** Decode the inline datum as `D` and refine the subscription by it. The real runtime requires a
      * `FromData[D]`; omitted here to keep the sketch self-contained.
      */
    def withInlineDatum[D]: TypedUtxoWatch[D]
}

/** UTxO subscription builder refined by a typed inline datum `D`. */
trait TypedUtxoWatch[D] {

    /** Keep only events whose datum satisfies the predicate. */
    def where(predicate: D => Boolean): TypedUtxoWatch[D]

    /** Register the reactive handler and start the subscription. */
    def onEvent(handler: UtxoEvent[D] => Unit): Subscription
}

/** The application runtime: chain subscriptions plus durable, retrying transaction submission. */
trait Runtime {

    /** Entry point for the fluent UTxO subscription builder. */
    def watchUtxos: UtxoWatch

    /** Submit `tx` and follow it to a terminal outcome under the given resubmission policy.
      *
      * @param retry         backoff schedule used while the tx is unconfirmed or evicted
      * @param confirmations blocks required on top before reporting `Confirmed`
      */
    def submit(tx: Transaction, retry: Backoff, confirmations: Int): SubmitOutcome
}

/** Base class for a Scalus Platform application. The runtime host injects [[runtime]] and calls
  * [[run]] once at startup.
  */
abstract class ScalusApp {

    /** Provided by the runtime host. */
    protected def runtime: Runtime = ???

    /** Wire up subscriptions and reactions here. */
    def run(): Unit
}
