package scalus.cardano.node.stream.internal

import scalus.cardano.ledger.{MultiAsset, Utxo, Utxos}
import scalus.cardano.node.stream.TransactionQuery
import scalus.cardano.node.{UtxoQuery, UtxoSource}

/** Per-item query evaluation for subscriptions.
  *
  * Distinct from `EmulatorBase.evalQuery`, which evaluates a query over a whole UTxO *set* and
  * honours pagination. A subscription matches one event at a time and pagination is meaningless for
  * it — `limit`/`offset`/`minTotal` describe a snapshot, not a stream — so they are ignored here,
  * as documented on [[scalus.cardano.node.stream.UtxoEventQuery]].
  */
private[stream] object QueryMatching {

    def matchesUtxo(query: UtxoQuery, utxo: Utxo): Boolean = query match
        case q: UtxoQuery.Simple =>
            matchesSource(q.source, utxo) && q.filter.forall(UtxoQuery.evalFilter(_, utxo.toTuple))
        case UtxoQuery.Or(left, right, _, _, _) =>
            matchesUtxo(left, utxo) || matchesUtxo(right, utxo)

    private def matchesSource(source: UtxoSource, utxo: Utxo): Boolean = source match
        case UtxoSource.FromAddress(addr) => utxo.output.address == addr
        case UtxoSource.FromAsset(policyId, assetName) =>
            utxo.output.value.assets.assets.get(policyId).exists(_.contains(assetName))
        case UtxoSource.FromInputs(inputs)    => inputs.contains(utxo.input)
        case UtxoSource.FromTransaction(txId) => utxo.input.transactionId == txId
        case UtxoSource.Or(left, right)  => matchesSource(left, utxo) || matchesSource(right, utxo)
        case UtxoSource.And(left, right) => matchesSource(left, utxo) && matchesSource(right, utxo)

    def matchesTransaction(query: TransactionQuery, applied: AppliedTransaction): Boolean = {
        val tx = applied.tx

        def mint: MultiAsset = tx.body.value.mint.getOrElse(MultiAsset.empty)

        def touchedOutputs: Iterable[scalus.cardano.ledger.TransactionOutput] =
            applied.created.values ++ applied.spent.values

        query match
            case TransactionQuery.All => true
            case TransactionQuery.InvolvesAddress(address) =>
                touchedOutputs.exists(_.address == address)
            case TransactionQuery.MintsPolicy(policy) => mint.assets.contains(policy)
            case TransactionQuery.MintsAsset(policy, name) =>
                mint.assets.get(policy).exists(_.contains(name))
            case TransactionQuery.SpendsInput(input)     => applied.spent.contains(input)
            case TransactionQuery.InvolvesScript(script) =>
                // Approximation on purpose: script *payment credentials* touched by this
                // transaction, plus minting policies. Reference scripts and certificate/withdrawal
                // credentials are not considered — a provider with the full resolved context can
                // do better, and this is only a fallback for providers that cannot push down.
                touchedOutputs.exists(_.address.scriptHashOption.contains(script)) ||
                mint.assets.keySet.exists(_ == script)
            case TransactionQuery.Not(inner)   => !matchesTransaction(inner, applied)
            case TransactionQuery.AllOf(inner) => inner.forall(matchesTransaction(_, applied))
            case TransactionQuery.AnyOf(inner) => inner.exists(matchesTransaction(_, applied))
    }

    /** Every UTxO in `utxos` matching `query`, as a stable sequence. */
    def matching(query: UtxoQuery, utxos: Utxos): Seq[Utxo] =
        utxos.toSeq.map(Utxo.apply).filter(matchesUtxo(query, _))
}
