package scalus.cardano.node.stream.engine

import scalus.cardano.node.{UtxoQuery, UtxoSource}

/** Reduce a [[UtxoQuery]] to the set of [[UtxoKey]]s whose [[Bucket]]s must be indexed to cover it.
  *
  * The returned set is a *superset* of "UTxOs matching the query" — the bucket's membership test is
  * coarse (an Addr bucket contains *all* UTxOs at that address, regardless of the query's
  * refinement filters). Fine-grained matching stays on the per-subscription side via
  * [[scalus.cardano.node.stream.engine.QueryDecomposer.matches]], which uses the full [[UtxoQuery]]
  * including filters.
  *
  * `And` sources intersect membership (UTxO must be in *every* side); `Or` sources union it. For
  * index placement we always need every atomic source a UTxO *might* be reached by, so we treat
  * `And` and `Or` alike as a union of atom keys — membership is then enforced by `matches` at
  * fan-out time.
  */
object QueryDecomposer {

    /** Extract every [[UtxoKey]] a query could touch. */
    def keys(query: UtxoQuery): Set[UtxoKey] = query match {
        case UtxoQuery.Simple(source, _, _, _, _) => keysOf(source)
        case UtxoQuery.Or(left, right, _, _, _)   => keys(left) ++ keys(right)
    }

    private def keysOf(source: UtxoSource): Set[UtxoKey] = source match {
        case UtxoSource.FromAddress(a)      => Set(UtxoKey.Addr(a))
        case UtxoSource.FromAsset(p, n)     => Set(UtxoKey.Asset(p, n))
        case UtxoSource.FromInputs(is)      => Set(UtxoKey.Inputs(is))
        case UtxoSource.FromTransaction(tx) => Set(UtxoKey.TxOuts(tx))
        case UtxoSource.Or(l, r)            => keysOf(l) ++ keysOf(r)
        case UtxoSource.And(l, r)           => keysOf(l) ++ keysOf(r)
    }

    /** Evaluate a full [[UtxoQuery]] against a single UTxO.
      *
      * Pagination (`limit`, `offset`, `minRequiredTotalAmount`) is a post-processing concern
      * applied at the `findUtxos` boundary; it does not affect live membership.
      */
    def matches(
        query: UtxoQuery,
        input: scalus.cardano.ledger.TransactionInput,
        output: scalus.cardano.ledger.TransactionOutput
    ): Boolean = query match {
        case UtxoQuery.Simple(source, filter, _, _, _) =>
            matchesSource(source, input, output) &&
            filter.forall(f => UtxoQuery.evalFilter(f, (input, output)))
        case UtxoQuery.Or(left, right, _, _, _) =>
            matches(left, input, output) || matches(right, input, output)
    }

    private def matchesSource(
        source: UtxoSource,
        input: scalus.cardano.ledger.TransactionInput,
        output: scalus.cardano.ledger.TransactionOutput
    ): Boolean = source match {
        case UtxoSource.FromAddress(a) =>
            output.address == a
        case UtxoSource.FromAsset(p, n) =>
            output.value.assets.assets.get(p).exists(_.contains(n))
        case UtxoSource.FromInputs(is) =>
            is.contains(input)
        case UtxoSource.FromTransaction(tx) =>
            input.transactionId == tx
        case UtxoSource.Or(l, r) =>
            matchesSource(l, input, output) || matchesSource(r, input, output)
        case UtxoSource.And(l, r) =>
            matchesSource(l, input, output) && matchesSource(r, input, output)
    }
}
