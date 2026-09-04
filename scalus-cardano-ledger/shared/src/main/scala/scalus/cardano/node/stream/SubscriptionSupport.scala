package scalus.cardano.node.stream

import scalus.cardano.node.{UtxoQuery, UtxoSource}

/** A subscription a caller is considering, or is about to open. */
enum SubscriptionRequest {
    case Utxo(query: UtxoEventQuery, opts: SubscriptionOptions)
    case Transaction(query: TransactionQuery, opts: SubscriptionOptions)
    case Block(query: BlockQuery, opts: SubscriptionOptions)

    /** Following one transaction's status. Carries no options: latest-value streams take none, as
      * [[SubscriptionOptions]] explains.
      */
    case TransactionStatus(txHash: scalus.cardano.ledger.TransactionHash)

    def options: SubscriptionOptions = this match
        case Utxo(_, o)           => o
        case Transaction(_, o)    => o
        case Block(_, o)          => o
        case _: TransactionStatus => SubscriptionOptions()

    def kind: SubscriptionKind = this match
        case _: Utxo              => SubscriptionKind.Utxo
        case _: Transaction       => SubscriptionKind.Transaction
        case _: Block             => SubscriptionKind.Block
        case _: TransactionStatus => SubscriptionKind.TransactionStatus
}

/** The verdict on a subscription request. */
enum SubscriptionSupport {

    /** Served from an index or a server-side filter. */
    case Indexed

    /** Serviceable, but only by scanning every block. Requires
      * `SubscriptionOptions.allowUnindexedScan`, because on a metered backend this is the
      * difference between a handful of requests a day and a blown quota.
      */
    case Unindexed

    /** Cannot be served at all. `subscribe` throws
      * [[scalus.cardano.infra.UnsupportedSubscriptionException]] with this reason.
      */
    case Unsupported(reason: String)
}

object SubscriptionSupport {

    /** Classify a request against a provider's declared capabilities.
      *
      * This is the single implementation used by callers deciding what to ask for, by every
      * provider's own `subscribe`-time validation, and by the conformance suite — so the rule
      * *"`subscribe` throws exactly when this returns `Unsupported`, or returns `Unindexed` while
      * `allowUnindexedScan` is false"* holds by construction rather than by convention.
      *
      * It is also the one place composite queries are reasoned about: a union is indexed only if
      * every arm is, while an intersection needs just one indexed arm because the rest
      * post-filters. That rule replicated across four providers would be four chances to get it
      * subtly wrong.
      */
    def of(request: SubscriptionRequest, caps: StreamCapabilities): SubscriptionSupport = {
        if !caps.kinds.contains(request.kind) then
            Unsupported(s"provider does not serve ${request.kind} subscriptions")
        else
            val opts = request.options
            checkConfirmations(opts, caps)
                .orElse(checkReplay(request, caps))
                .getOrElse(indexing(request, caps))
    }

    /** The depth a subscription actually waits, which is not always what it asked for: `noRollback`
      * means "wait until a reorg cannot reach me", i.e. the provider's declared rollback horizon.
      * The hub computes delivery depth the same way, and both must, or a subscription can be
      * accepted against `maxConfirmations` and then gated far deeper than the provider can manage.
      */
    def effectiveDepth(opts: SubscriptionOptions, caps: StreamCapabilities): Int =
        if opts.noRollback then math.max(opts.confirmations, caps.rollbackHorizon.getOrElse(0))
        else opts.confirmations

    private def checkConfirmations(
        opts: SubscriptionOptions,
        caps: StreamCapabilities
    ): Option[SubscriptionSupport] = {
        val depth = effectiveDepth(opts, caps)
        caps.maxConfirmations.filter(depth > _).map { max =>
            val asked =
                if depth == opts.confirmations then s"confirmations=$depth"
                else s"noRollback implies a depth of $depth"
            Unsupported(s"$asked exceeds the provider's maximum of $max")
        }
    }

    private def checkReplay(
        request: SubscriptionRequest,
        caps: StreamCapabilities
    ): Option[SubscriptionSupport] = request.options.startFrom match
        case StartFrom.Tip => None
        case StartFrom.Origin =>
            caps.replay match
                case ReplaySupport.FullHistory => None
                case other =>
                    Some(Unsupported(s"replay from origin needs FullHistory, have $other"))
        case StartFrom.At(_) =>
            caps.replay match
                case ReplaySupport.NoReplay =>
                    Some(Unsupported("provider is live-only; StartFrom.At is not serviceable"))
                case ReplaySupport.Scoped(kinds) =>
                    // Replay is available only where the query itself pushes down — and a block
                    // subscription has no query to push down, so a provider that replays by
                    // looking up a source cannot serve one at all. `isIndexed` answers `true` for
                    // blocks because there is nothing to *scan*, which is the opposite question.
                    request match
                        case _: SubscriptionRequest.TransactionStatus =>
                            // Nothing to replay: the status of a transaction is a current fact,
                            // not a history the subscriber can resume through.
                            None
                        case _: SubscriptionRequest.Block =>
                            Some(
                              Unsupported(
                                "provider replays only subscriptions it can look up by query " +
                                    s"source ($kinds); a block subscription has none"
                              )
                            )
                        case _ if isIndexed(request, kinds) => None
                        case _ =>
                            Some(
                              Unsupported(
                                s"provider can only replay subscriptions indexed by $kinds"
                              )
                            )
                case ReplaySupport.Buffered(_) | ReplaySupport.FullHistory => None

    private def indexing(
        request: SubscriptionRequest,
        caps: StreamCapabilities
    ): SubscriptionSupport =
        if isIndexed(request, caps.pushdown) then Indexed
        else
            caps.scanning match
                // A provider that already holds every block has nothing to charge for a scan, so
                // there is nothing for the caller to consent to. Making it demand
                // `allowUnindexedScan` would refuse "watch every transaction" on an in-memory
                // ledger, where that is the cheapest thing you can ask for.
                case ScanSupport.Free    => Indexed
                case ScanSupport.Metered => Unindexed
                case ScanSupport.Unsupported =>
                    Unsupported(
                      "this provider serves only subscriptions it can look up by query source " +
                          s"(${caps.pushdown.mkString(", ")}); it has no way to examine a block " +
                          "it was not asked about, so this cannot be served at any price"
                    )

    private def isIndexed(request: SubscriptionRequest, pushdown: Set[PushdownKind]): Boolean =
        request match
            case SubscriptionRequest.Utxo(q, _)        => indexedUtxoQuery(q.query, pushdown)
            case SubscriptionRequest.Transaction(q, _) => indexedTxQuery(q, pushdown)
            // Every block is a match, so there is nothing to look up and nothing to scan for.
            case SubscriptionRequest.Block(_, _) => true
            // A hash is the most direct lookup there is; a provider that serves this kind at all
            // serves it without scanning.
            case _: SubscriptionRequest.TransactionStatus => true

    /** The sources a provider must observe in order to serve this request.
      *
      * Beside [[of]] deliberately, and taking the same `pushdown` set it classifies against. The
      * two walk the same trees under the same rules — a union needs every arm, an intersection is
      * answered from any one — and a provider that classified a query with one and then observed it
      * with the other would accept subscriptions no block it fetches could ever cover: no events,
      * no `Idle`, no error. Deriving both from the same recursion is what stops a new
      * [[scalus.cardano.node.UtxoSource]] or [[TransactionQuery]] case being handled in one and
      * forgotten in the other.
      *
      * `pushdown` is not decoration. An intersection is answered from *either* arm, so which arm to
      * observe depends on which one this provider can look up: `FromAsset && FromAddress` must
      * yield the address for an address-only provider and the asset for an asset-only one, and a
      * version that just picked the left arm would hand back something the caller cannot watch —
      * leaving the subscription accepted, uncovered and silent.
      *
      * Flat leaves, not the composite the caller wrote, because that is the shape both consumers
      * want: a metered provider probes one source endpoint at a time, and
      * [[scalus.cardano.node.stream.internal.BlockCoverage]] tests membership of exactly these.
      *
      * An empty result means "this cannot be served by observing sources" — a block or status
      * subscription, or a query with nothing pushdownable in it. For a provider that serves only
      * what it can look up, that is a request [[of]] will already have refused; getting one here
      * anyway is a disagreement between the two worth raising rather than watching nothing.
      */
    def sourcesFor(request: SubscriptionRequest, caps: StreamCapabilities): Set[UtxoSource] =
        request match
            case SubscriptionRequest.Utxo(q, _) => utxoQuerySources(q.query, caps.pushdown)
            case SubscriptionRequest.Transaction(q, _) =>
                transactionQuerySources(q, caps.pushdown)
            case _: SubscriptionRequest.Block             => Set.empty
            case _: SubscriptionRequest.TransactionStatus => Set.empty

    private def utxoQuerySources(query: UtxoQuery, pushdown: Set[PushdownKind]): Set[UtxoSource] =
        query match
            case q: UtxoQuery.Simple => leafSources(q.source, pushdown)
            case UtxoQuery.Or(l, r, _, _, _) =>
                utxoQuerySources(l, pushdown) ++ utxoQuerySources(r, pushdown)

    private def leafSources(source: UtxoSource, pushdown: Set[PushdownKind]): Set[UtxoSource] =
        source match
            // A union is only as covered as its worst arm, so every arm must be observed.
            case UtxoSource.Or(l, r) => leafSources(l, pushdown) ++ leafSources(r, pushdown)
            // One covered arm answers an intersection and the other post-filters data already in
            // hand, so observing both would double a metered provider's per-block cost for nothing.
            // Mirrors `indexedSource`, which accepts an `And` on either arm for the same reason.
            case UtxoSource.And(l, r) =>
                val left = leafSources(l, pushdown)
                if left.nonEmpty then left else leafSources(r, pushdown)
            case leaf => if indexedSource(leaf, pushdown) then Set(leaf) else Set.empty

    private def transactionQuerySources(
        query: TransactionQuery,
        pushdown: Set[PushdownKind]
    ): Set[UtxoSource] = {
        def pushed(kind: PushdownKind, source: => UtxoSource): Set[UtxoSource] =
            if pushdown.contains(kind) then Set(source) else Set.empty
        query match
            case TransactionQuery.InvolvesAddress(a) =>
                pushed(PushdownKind.Address, UtxoSource.FromAddress(a))
            case TransactionQuery.MintsAsset(p, n) =>
                pushed(PushdownKind.Asset, UtxoSource.FromAsset(p, n))
            case TransactionQuery.SpendsInput(i) =>
                pushed(PushdownKind.Inputs, UtxoSource.FromInputs(Set(i)))
            // An intersection is answered from any one arm; a union needs all of them.
            case TransactionQuery.AllOf(qs) =>
                qs.map(transactionQuerySources(_, pushdown)).find(_.nonEmpty).getOrElse(Set.empty)
            case TransactionQuery.AnyOf(qs) =>
                qs.flatMap(transactionQuerySources(_, pushdown)).toSet
            // No source to observe: `MintsPolicy` has no per-asset endpoint, and `All`,
            // `InvolvesScript` and `Not` have no index at all — matching how `SubscriptionHub`
            // answers coverage for exactly these.
            case TransactionQuery.All | _: TransactionQuery.MintsPolicy |
                _: TransactionQuery.InvolvesScript | _: TransactionQuery.Not =>
                Set.empty
    }

    private def indexedUtxoQuery(query: UtxoQuery, pushdown: Set[PushdownKind]): Boolean =
        query match
            case q: UtxoQuery.Simple => indexedSource(q.source, pushdown)
            case UtxoQuery.Or(l, r, _, _, _) =>
                indexedUtxoQuery(l, pushdown) && indexedUtxoQuery(r, pushdown)

    private def indexedSource(source: UtxoSource, pushdown: Set[PushdownKind]): Boolean =
        source match
            case _: UtxoSource.FromAddress     => pushdown.contains(PushdownKind.Address)
            case _: UtxoSource.FromAsset       => pushdown.contains(PushdownKind.Asset)
            case _: UtxoSource.FromInputs      => pushdown.contains(PushdownKind.Inputs)
            case _: UtxoSource.FromTransaction => pushdown.contains(PushdownKind.Transaction)
            // A union is only as indexed as its worst arm — the events we would miss are the ones
            // the unindexed arm would have found.
            case UtxoSource.Or(l, r) => indexedSource(l, pushdown) && indexedSource(r, pushdown)
            // An intersection can be served from either arm and post-filtered with the other.
            case UtxoSource.And(l, r) => indexedSource(l, pushdown) || indexedSource(r, pushdown)

    private def indexedTxQuery(query: TransactionQuery, pushdown: Set[PushdownKind]): Boolean =
        query match
            case TransactionQuery.All                => false // every block must be examined
            case _: TransactionQuery.InvolvesAddress => pushdown.contains(PushdownKind.Address)
            case _: TransactionQuery.MintsPolicy     => pushdown.contains(PushdownKind.Asset)
            case _: TransactionQuery.MintsAsset      => pushdown.contains(PushdownKind.Asset)
            case _: TransactionQuery.SpendsInput     => pushdown.contains(PushdownKind.Inputs)
            case _: TransactionQuery.InvolvesScript  => false
            // Negation cannot be answered from an index: the matching set is the complement.
            case _: TransactionQuery.Not    => false
            case TransactionQuery.AllOf(qs) => qs.exists(indexedTxQuery(_, pushdown))
            case TransactionQuery.AnyOf(qs) => qs.nonEmpty && qs.forall(indexedTxQuery(_, pushdown))
}
