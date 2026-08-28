package scalus.cardano.node.stream

/** What a provider can index a subscription by.
  *
  * Mirrors the cases of [[scalus.cardano.node.UtxoSource]]: a query whose source reduces to kinds
  * the provider declares here can be served from an index or a server-side filter; anything else
  * needs a scan.
  */
enum PushdownKind {
    case Address
    case Asset
    case Inputs
    case Transaction
}

object PushdownKind {
    val all: Set[PushdownKind] = Set(Address, Asset, Inputs, Transaction)
}

/** Which subscription kinds a provider serves at all. */
enum SubscriptionKind {
    case Utxo
    case Transaction
    case Block
}

object SubscriptionKind {
    val all: Set[SubscriptionKind] = Set(Utxo, Transaction, Block)
}

/** What happens to a subscription the provider cannot serve from an index.
  *
  * The distinction between the first two is not how clever the matching is — it is whether the
  * provider already holds the block's contents. An in-memory ledger, a chain-sync follower and a
  * gRPC stream all see every transaction anyway, so a subscription that matches everything costs
  * them nothing beyond the fan-out they already do. A REST provider whose cheap path is per-address
  * endpoints has to fetch the block and then a UTxO set per transaction in it, which is the
  * difference between a handful of requests a day and a spent quota.
  *
  * The third is a different kind of answer, and it needs to be stated rather than approximated by
  * the second: a provider built entirely out of per-source lookups has no request sequence that
  * would answer "every transaction" at all. Calling that `Metered` would let a caller consent, via
  * `allowUnindexedScan`, to something that cannot happen — and the subscription would then be
  * accepted and deliver nothing, forever, with no error. Consenting to an impossibility is worse
  * than being refused.
  */
enum ScanSupport {

    /** The provider already has every block's contents; scanning adds nothing. */
    case Free

    /** Examining a block the provider would not otherwise fetch costs it real requests, so a caller
      * must opt in with `SubscriptionOptions.allowUnindexedScan`.
      */
    case Metered

    /** The provider cannot examine a block it was not asked about at all, so it serves exactly what
      * [[StreamCapabilities.pushdown]] covers and refuses everything else.
      */
    case Unsupported
}

/** How far back a provider can start a subscription.
  *
  * Replay is not a yes/no property, for the same reason pushdown is not: on a REST backend an
  * address's history is one paginated endpoint away while the chain's history is unreachable.
  */
enum ReplaySupport {

    /** Live only, from the tip. */
    case NoReplay

    /** Replay works for subscriptions whose query pushes down to these kinds, and only those.
      * Blockfrost is `Scoped(Set(Address, Asset))` — which is checkpoint-and-resume for the common
      * case, without a chain store anywhere.
      */
    case Scoped(kinds: Set[PushdownKind])

    /** Replay within a bounded in-memory window of this many blocks. */
    case Buffered(depth: Int)

    /** Arbitrary start point, arbitrary query. */
    case FullHistory
}

/** Everything a provider declares about its streaming behaviour.
  *
  * This is the *only* thing an implementation states. Whether a particular subscription is
  * supported, and whether it is cheap, is derived from this descriptor by
  * [[SubscriptionSupport.of]] — so a provider cannot refuse something it advertised, or accept
  * something it did not, and callers can decide what to ask for before asking.
  *
  * @param kinds
  *   which subscription kinds are served at all
  * @param pushdown
  *   query sources that can be served from an index rather than a scan
  * @param scanning
  *   what a query outside [[pushdown]] costs — `Free` for a provider that already holds every
  *   block, `Metered` for one that would have to go and fetch it
  * @param replay
  *   how far back a subscription can start
  * @param rollbackHorizon
  *   how deep a reorg the provider can report. `None` means it never signals rollbacks — which is
  *   not the same as "no rollbacks happen", and is exactly why it is declared rather than assumed
  * @param maxConfirmations
  *   the largest confirmation depth the provider can gate on, if bounded
  * @param idleSignals
  *   whether the provider can emit progress signals for non-matching blocks
  */
case class StreamCapabilities(
    kinds: Set[SubscriptionKind],
    pushdown: Set[PushdownKind],
    scanning: ScanSupport,
    replay: ReplaySupport,
    rollbackHorizon: Option[Int],
    maxConfirmations: Option[Int],
    idleSignals: Boolean
)

object StreamCapabilities {

    /** An in-process provider holding the whole ledger: everything is indexed, nothing is remote.
      * `rollbackHorizon` is still explicit, because a linear emulator that never forks and one that
      * can be rewound on demand are different things to a subscriber.
      */
    def inProcess(
        kinds: Set[SubscriptionKind],
        replay: ReplaySupport,
        rollbackHorizon: Option[Int]
    ): StreamCapabilities = StreamCapabilities(
      kinds = kinds,
      pushdown = PushdownKind.all,
      scanning = ScanSupport.Free,
      replay = replay,
      rollbackHorizon = rollbackHorizon,
      maxConfirmations = None,
      idleSignals = true
    )
}
