package scalus.cardano.node.stream

import scalus.cardano.node.UtxoQuery

/** Query for a UTxO event subscription.
  *
  * Wraps a `UtxoQuery` (reusing its source/filter algebra and the HOAS
  * DSL via `UtxoQueryMacros.buildQuery`) and adds an event-type set so
  * subscribers can ask for, say, only `Created` events without a
  * downstream `collect` / `filter`.
  *
  * Note: pagination fields on the inner `UtxoQuery` (`limit`, `offset`,
  * `minRequiredTotalAmount`) are snapshot-only and are **ignored** for
  * streaming subscriptions. They remain on `UtxoQuery` because the same
  * query algebra is also used by `BlockchainReader.findUtxos`, which
  * does honour them.
  */
case class UtxoEventQuery(
    query: UtxoQuery,
    types: Set[UtxoEventType] = UtxoEventType.all
)

/** Which UTxO event kinds a subscription wants to observe. `RolledBack`
  * is a stream-wide signal, not a per-UTxO event, and is controlled via
  * `SubscriptionOptions.noRollback` instead.
  */
enum UtxoEventType {
    case Created
    case Spent
}

object UtxoEventType {
    val all: Set[UtxoEventType] = Set(Created, Spent)
}
