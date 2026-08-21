package scalus.cardano.node.stream

import scalus.cardano.node.UtxoQuery

/** Query for a UTxO event subscription.
  *
  * Wraps a [[scalus.cardano.node.UtxoQuery]] — reusing its source/filter algebra and the inline DSL
  * — and adds an event-type set, so a subscriber can ask for only `Created` events without a
  * downstream `collect`.
  *
  * The pagination fields on the inner query (`limit`, `offset`, `minRequiredTotalAmount`) are
  * snapshot concepts and are **ignored** for subscriptions; they stay on `UtxoQuery` because
  * `BlockchainReader.findUtxos` does honour them.
  */
case class UtxoEventQuery(query: UtxoQuery, types: Set[UtxoEventType] = UtxoEventType.all)

/** Which UTxO event kinds a subscription wants.
  *
  * `RolledBack` and `Idle` are stream-wide signals rather than per-UTxO events, and are not
  * selectable here — rollback delivery is governed by `SubscriptionOptions.noRollback`, idle
  * delivery by `SubscriptionOptions.idleSignals`.
  */
enum UtxoEventType {
    case Created
    case Spent
}

object UtxoEventType {
    val all: Set[UtxoEventType] = Set(Created, Spent)
}
