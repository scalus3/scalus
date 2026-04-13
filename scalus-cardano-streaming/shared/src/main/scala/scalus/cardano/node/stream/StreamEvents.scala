package scalus.cardano.node.stream

import scalus.cardano.ledger.{Block, Transaction, TransactionHash, TransactionInput, TransactionOutput}

/** UTxO lifecycle event delivered to a subscriber.
  *
  * Every event carries the `ChainPoint` at which it occurred so subscribers
  * can correlate across streams and checkpoint their own progress.
  */
enum UtxoEvent {

    /** A UTxO matching the subscription was created by `producedBy`. */
    case Created(
        input: TransactionInput,
        output: TransactionOutput,
        producedBy: TransactionHash,
        at: ChainPoint
    )

    /** A UTxO matching the subscription was spent by `spentBy`. */
    case Spent(
        input: TransactionInput,
        output: TransactionOutput,
        spentBy: TransactionHash,
        at: ChainPoint
    )

    /** The chain has rolled back to the given point. Subscribers must
      * discard all previously delivered events that occurred strictly
      * after `to` and resume consumption from the subsequent events.
      */
    case RolledBack(to: ChainPoint)
}

/** Transaction stream event. */
enum TransactionEvent {

    /** Transaction was included in a block at the given chain point. */
    case Included(tx: Transaction, at: ChainPoint)

    /** Chain rolled back; discard events past `to`. */
    case RolledBack(to: ChainPoint)
}

/** Block stream event. */
enum BlockEvent {

    /** Block was applied to the chain at the given point. */
    case Applied(block: Block, at: ChainPoint)

    /** Chain rolled back; discard events past `to`. */
    case RolledBack(to: ChainPoint)
}
