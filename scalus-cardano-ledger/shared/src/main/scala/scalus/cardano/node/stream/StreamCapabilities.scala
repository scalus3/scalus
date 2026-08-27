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

/** What it costs a provider to examine a block it was not asked about.
  *
  * The distinction is not how clever the matching is — it is whether the provider already holds the
  * block's contents. An in-memory ledger, a chain-sync follower and a gRPC stream all see every
  * transaction anyway, so a subscription that matches everything costs them nothing beyond the
  * fan-out they already do. A REST provider whose cheap path is per-address endpoints has to fetch
  * the block and then a UTxO set per transaction in it, which is the difference between a handful
  * of requests a day and a spent quota.
  */
enum ScanCost {

    /** The provider already has every block's contents; scanning adds nothing. */
    case Free

    /** Examining a block the provider would not otherwise fetch costs it real requests. */
    case Metered
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
    scanning: ScanCost,
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
      scanning = ScanCost.Free,
      replay = replay,
      rollbackHorizon = rollbackHorizon,
      maxConfirmations = None,
      idleSignals = true
    )
}
