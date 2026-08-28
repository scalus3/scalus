package scalus.cardano.node.stream

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{CardanoInfo, TransactionHash}
import scalus.uplc.builtin.ByteString
import scalus.cardano.node.{UtxoQuery, UtxoSource}
import scalus.testing.kit.Party

/** The classification rules that decide what every provider accepts, refuses, and charges for. */
class SubscriptionSupportTest extends AnyFunSuite {

    private given CardanoInfo = CardanoInfo.mainnet

    private val alice = Party.Alice.address
    private val bob = Party.Bob.address
    private val someTxHash =
        TransactionHash.fromByteString(ByteString.fromArray(new Array[Byte](32)))

    /** A REST-backed provider: addresses and assets are indexed, nothing else is. */
    private val blockfrostLike = StreamCapabilities(
      kinds = SubscriptionKind.all,
      pushdown = Set(PushdownKind.Address, PushdownKind.Asset),
      scanning = ScanSupport.Metered,
      replay = ReplaySupport.Scoped(Set(PushdownKind.Address, PushdownKind.Asset)),
      rollbackHorizon = Some(50),
      maxConfirmations = Some(100),
      idleSignals = true
    )

    /** An in-process provider: the whole ledger is in memory, so nothing costs extra. */
    private val inMemory = blockfrostLike.copy(
      pushdown = PushdownKind.all,
      scanning = ScanSupport.Free,
      replay = ReplaySupport.NoReplay,
      maxConfirmations = None
    )

    private def utxoRequest(
        source: UtxoSource,
        opts: SubscriptionOptions = SubscriptionOptions()
    ): SubscriptionRequest =
        SubscriptionRequest.Utxo(UtxoEventQuery(UtxoQuery(source)), opts)

    test("a source the provider indexes is served from the index") {
        val verdict =
            SubscriptionSupport.of(utxoRequest(UtxoSource.FromAddress(alice)), blockfrostLike)
        assert(verdict == SubscriptionSupport.Indexed)
    }

    test("a source the provider does not index falls back to a scan") {
        val verdict =
            SubscriptionSupport.of(
              utxoRequest(UtxoSource.FromTransaction(someTxHash)),
              blockfrostLike
            )
        assert(verdict == SubscriptionSupport.Unindexed)
    }

    test("a union is only as indexed as its worst arm") {
        val indexed = UtxoSource.FromAddress(alice)
        val notIndexed = UtxoSource.FromInputs(Set.empty)
        assert(
          SubscriptionSupport.of(utxoRequest(indexed || notIndexed), blockfrostLike) ==
              SubscriptionSupport.Unindexed,
          "the events the unindexed arm would find are exactly the ones a union cannot skip"
        )
        assert(
          SubscriptionSupport.of(
            utxoRequest(indexed || UtxoSource.FromAddress(bob)),
            blockfrostLike
          ) ==
              SubscriptionSupport.Indexed
        )
    }

    test("an intersection needs only one indexed arm, because the rest post-filters") {
        val verdict = SubscriptionSupport.of(
          utxoRequest(UtxoSource.FromAddress(alice) && UtxoSource.FromInputs(Set.empty)),
          blockfrostLike
        )
        assert(verdict == SubscriptionSupport.Indexed)
    }

    test("replay is refused where the query itself is not indexed") {
        val at = SubscriptionOptions(startFrom = StartFrom.At(ChainPoint.origin))
        assert(
          SubscriptionSupport.of(utxoRequest(UtxoSource.FromAddress(alice), at), blockfrostLike) ==
              SubscriptionSupport.Indexed,
          "resuming an address subscription from a checkpoint is the common case and must work"
        )
        assert(
          SubscriptionSupport
              .of(utxoRequest(UtxoSource.FromInputs(Set.empty), at), blockfrostLike)
              .isInstanceOf[SubscriptionSupport.Unsupported],
          "scoped replay cannot serve a query it cannot push down"
        )
    }

    test("replay from origin needs full history") {
        val origin = SubscriptionOptions(startFrom = StartFrom.Origin)
        assert(
          SubscriptionSupport
              .of(utxoRequest(UtxoSource.FromAddress(alice), origin), blockfrostLike)
              .isInstanceOf[SubscriptionSupport.Unsupported]
        )
        val archival = blockfrostLike.copy(replay = ReplaySupport.FullHistory)
        assert(
          SubscriptionSupport.of(utxoRequest(UtxoSource.FromAddress(alice), origin), archival) ==
              SubscriptionSupport.Indexed
        )
    }

    test("a confirmation depth beyond the provider's maximum is refused") {
        val deep = SubscriptionOptions(confirmations = 500)
        assert(
          SubscriptionSupport
              .of(utxoRequest(UtxoSource.FromAddress(alice), deep), blockfrostLike)
              .isInstanceOf[SubscriptionSupport.Unsupported]
        )
    }

    test("an undeclared subscription kind is refused before anything else is considered") {
        val noBlocks = blockfrostLike.copy(kinds = Set(SubscriptionKind.Utxo))
        val verdict =
            SubscriptionSupport.of(
              SubscriptionRequest.Block(BlockQuery.All, SubscriptionOptions()),
              noBlocks
            )
        assert(verdict.isInstanceOf[SubscriptionSupport.Unsupported])
    }

    test("watching every transaction is a scan, watching one address is not") {
        assert(
          SubscriptionSupport.of(
            SubscriptionRequest.Transaction(TransactionQuery.All, SubscriptionOptions()),
            blockfrostLike
          ) == SubscriptionSupport.Unindexed
        )
        assert(
          SubscriptionSupport.of(
            SubscriptionRequest
                .Transaction(TransactionQuery.InvolvesAddress(alice), SubscriptionOptions()),
            blockfrostLike
          ) == SubscriptionSupport.Indexed
        )
    }

    test("a provider that already holds every block is not charged for a scan") {
        val watchEverything =
            SubscriptionRequest.Transaction(TransactionQuery.All, SubscriptionOptions())
        assert(
          SubscriptionSupport.of(watchEverything, blockfrostLike) == SubscriptionSupport.Unindexed,
          "on a metered backend, watching every transaction means fetching every block"
        )
        assert(
          SubscriptionSupport.of(watchEverything, inMemory) == SubscriptionSupport.Indexed,
          "on an in-memory ledger it is the cheapest thing you can ask for, so requiring " +
              "allowUnindexedScan would refuse the obvious test subscription"
        )
    }

    test("negation cannot be answered from an index") {
        val verdict = SubscriptionSupport.of(
          SubscriptionRequest.Transaction(
            TransactionQuery.Not(TransactionQuery.InvolvesAddress(alice)),
            SubscriptionOptions()
          ),
          blockfrostLike
        )
        assert(verdict == SubscriptionSupport.Unindexed)
    }
    test("a provider that cannot scan refuses, rather than letting a caller consent") {
        val lookupOnly = StreamCapabilities(
          kinds = SubscriptionKind.all,
          pushdown = Set(PushdownKind.Address),
          scanning = ScanSupport.Unsupported,
          replay = ReplaySupport.NoReplay,
          rollbackHorizon = None,
          maxConfirmations = None,
          idleSignals = true
        )
        val everyTransaction = SubscriptionRequest.Transaction(
          TransactionQuery.All,
          // Consent given — and it must not help, because the scan is not expensive, it is
          // impossible. Classifying this as Unindexed would accept the subscription and then
          // deliver nothing, forever, with no error.
          SubscriptionOptions(allowUnindexedScan = true)
        )
        assert(
          SubscriptionSupport
              .of(everyTransaction, lookupOnly)
              .isInstanceOf[
                SubscriptionSupport.Unsupported
              ],
          "a lookup-only provider serves exactly what it can push down and refuses the rest"
        )
    }

    test("a metered provider still accepts the same request when the caller consents") {
        val metered = StreamCapabilities(
          kinds = SubscriptionKind.all,
          pushdown = Set(PushdownKind.Address),
          scanning = ScanSupport.Metered,
          replay = ReplaySupport.NoReplay,
          rollbackHorizon = None,
          maxConfirmations = None,
          idleSignals = true
        )
        val everyTransaction = SubscriptionRequest.Transaction(
          TransactionQuery.All,
          SubscriptionOptions(allowUnindexedScan = true)
        )
        assert(
          SubscriptionSupport.of(everyTransaction, metered) == SubscriptionSupport.Unindexed,
          "Unsupported must not have swallowed the case it was carved out of"
        )
    }

    test("a provider that does not declare TransactionStatus refuses to follow a transaction") {
        val noStatus = StreamCapabilities(
          kinds = Set(SubscriptionKind.Utxo, SubscriptionKind.Transaction),
          pushdown = Set(PushdownKind.Address),
          scanning = ScanSupport.Unsupported,
          replay = ReplaySupport.NoReplay,
          rollbackHorizon = None,
          maxConfirmations = None,
          idleSignals = true
        )
        val request = SubscriptionRequest.TransactionStatus(someTxHash)
        assert(
          SubscriptionSupport.of(request, noStatus).isInstanceOf[SubscriptionSupport.Unsupported],
          "a provider that only observes the sources it was asked to watch cannot say what " +
              "happened to an arbitrary hash, and reporting Pending forever would be worse than " +
              "refusing — the subscriber could not tell that apart from a transaction that has " +
              "genuinely not landed"
        )
    }

    test("following a transaction is a lookup, so it is Indexed wherever it is served") {
        val withStatus = StreamCapabilities(
          kinds = SubscriptionKind.all,
          pushdown = Set(PushdownKind.Address),
          // Even where no scan is possible at all: a hash is the most direct lookup there is.
          scanning = ScanSupport.Unsupported,
          replay = ReplaySupport.NoReplay,
          rollbackHorizon = None,
          maxConfirmations = None,
          idleSignals = true
        )
        assert(
          SubscriptionSupport.of(
            SubscriptionRequest.TransactionStatus(someTxHash),
            withStatus
          ) == SubscriptionSupport.Indexed
        )
    }

}
