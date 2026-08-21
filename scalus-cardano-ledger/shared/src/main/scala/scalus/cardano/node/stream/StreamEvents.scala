package scalus.cardano.node.stream

import scalus.cardano.ledger.{Block, Transaction, TransactionHash, Utxo}

/** UTxO lifecycle event.
  *
  * `RolledBack` is not an error and not an exception: a chain that reorganises is a chain behaving
  * normally, and a subscriber that cannot express "undo everything after this point" has a bug
  * waiting for its first fork. Providers that never signal rollbacks declare
  * `rollbackHorizon = None` so the difference is visible before subscribing rather than after.
  */
enum UtxoEvent {

    /** A UTxO matching the subscription was created by `producedBy`. */
    case Created(utxo: Utxo, producedBy: TransactionHash, at: ChainPoint)

    /** A UTxO matching the subscription was spent by `spentBy`. */
    case Spent(utxo: Utxo, spentBy: TransactionHash, at: ChainPoint)

    /** The chain rolled back to `to`. Discard every previously delivered event that occurred
      * strictly after it, then resume with the events that follow.
      */
    case RolledBack(to: ChainPoint)

    /** Progress signal: the chain advanced to `at` and nothing matched this subscription.
      *
      * Without it, a narrowly-scoped subscription cannot distinguish "quiet chain" from "stalled
      * connection", and has no point to checkpoint. Emitted at provider discretion — see
      * [[SubscriptionOptions.idleSignals]].
      */
    case Idle(at: ChainPoint)
}

/** Transaction stream event. */
enum TransactionEvent {
    case Included(tx: Transaction, at: ChainPoint)
    case RolledBack(to: ChainPoint)
    case Idle(at: ChainPoint)
}

/** Block stream event. */
enum BlockEvent {
    case Applied(block: Block, at: ChainPoint)
    case RolledBack(to: ChainPoint)
}
