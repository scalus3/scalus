package scalus.cardano.node.stream

/** Query describing which transactions a subscriber wants to observe.
  *
  * Placeholder for now — shape to be designed alongside the first
  * subscriber use cases. Expected algebra, mirroring `UtxoQuery`:
  *
  *   - sources: involves address, mints policy, has script hash,
  *     references given input, metadata matches
  *   - composable with `&&`, `||`, `!`
  */
sealed trait TransactionQuery

object TransactionQuery {
    // TODO: sources + filters + combinators.
}

/** Query describing which blocks a subscriber wants to observe.
  *
  * Placeholder. Most apps subscribe to every block in a slot range, so the
  * algebra here is likely simpler than `UtxoQuery` / `TransactionQuery`.
  */
sealed trait BlockQuery

object BlockQuery {
    // TODO: slot range, era filter, contains-tx predicate.
}
