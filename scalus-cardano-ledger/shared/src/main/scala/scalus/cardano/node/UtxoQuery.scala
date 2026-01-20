package scalus.cardano.node

import scalus.cardano.address.Address
import scalus.cardano.ledger.*

import scala.concurrent.duration.Duration

/** Source specifies WHERE to look for UTxOs. This is required - a query must start from a source.
  */
sealed trait UtxoSource {

    /** Combine two sources with OR - returns UTxOs from either source (union) */
    def ||(other: UtxoSource): UtxoSource = UtxoSource.Or(this, other)

    /** Combine two sources with AND - returns UTxOs that match both sources (intersection) */
    def &&(other: UtxoSource): UtxoSource = UtxoSource.And(this, other)
}

object UtxoSource {

    /** Find UTxOs at a specific address */
    case class FromAddress(address: Address) extends UtxoSource

    /** Find UTxOs containing a specific asset (global search)
      *
      * @param policyId
      *   The minting policy hash
      * @param assetName
      *   The asset name within the policy
      */
    case class FromAsset(policyId: PolicyId, assetName: AssetName) extends UtxoSource

    /** Find UTxOs for specific transaction inputs */
    case class FromInputs(inputs: Set[TransactionInput]) extends UtxoSource

    /** Find UTxOs from outputs of a specific transaction */
    case class FromTransaction(transactionId: TransactionHash) extends UtxoSource

    /** Combine two sources - returns union of UTxOs from both */
    case class Or(left: UtxoSource, right: UtxoSource) extends UtxoSource

    /** Combine two sources - returns intersection of UTxOs from both */
    case class And(left: UtxoSource, right: UtxoSource) extends UtxoSource
}

/** Filter specifies optional refinements to apply after fetching from source.
  */
sealed trait UtxoFilter {

    /** Combine two filters with AND - both conditions must be true */
    def &&(other: UtxoFilter): UtxoFilter = UtxoFilter.And(this, other)

    /** Combine two filters with OR - either condition must be true */
    def ||(other: UtxoFilter): UtxoFilter = UtxoFilter.Or(this, other)

    /** Negate this filter */
    def unary_! : UtxoFilter = UtxoFilter.Not(this)
}

object UtxoFilter {

    /** Filter UTxOs that contain a specific asset
      *
      * @param policyId
      *   The minting policy hash
      * @param assetName
      *   The asset name within the policy
      */
    case class HasAsset(policyId: PolicyId, assetName: AssetName) extends UtxoFilter

    /** Filter UTxOs that have a specific datum */
    case class HasDatum(datum: DatumOption) extends UtxoFilter

    /** Filter UTxOs that have a specific datum hash (more efficient than HasDatum) */
    case class HasDatumHash(hash: DataHash) extends UtxoFilter

    /** Filter UTxOs that have at least the specified lovelace amount */
    case class MinLovelace(amount: Coin) extends UtxoFilter

    /** Filter UTxOs that are in the specified set of inputs. Useful for exclusion with Not. */
    case class AtInputs(inputs: Set[TransactionInput]) extends UtxoFilter

    /** Both filters must match */
    case class And(left: UtxoFilter, right: UtxoFilter) extends UtxoFilter

    /** Either filter must match */
    case class Or(left: UtxoFilter, right: UtxoFilter) extends UtxoFilter

    /** Filter must not match */
    case class Not(filter: UtxoFilter) extends UtxoFilter
}

/** A query for UTxOs. Can be a simple query with source/filter/pagination, or a combination of
  * queries using Or.
  */
sealed trait UtxoQuery {

    /** Combine two queries with OR - returns UTxOs from either query */
    def ||(other: UtxoQuery): UtxoQuery = UtxoQuery.Or(this, other)

    /** Add a filter condition (AND with existing filters) */
    def &&(f: UtxoFilter): UtxoQuery

    /** Limit the number of results */
    def limit(n: Int): UtxoQuery

    /** Skip the first n results */
    def skip(n: Int): UtxoQuery

    /** Set minimum required total lovelace amount (early termination optimization) */
    def minTotal(amount: Coin): UtxoQuery
}

object UtxoQuery {

    /** A simple query with source, optional filter, and pagination.
      *
      * @param source
      *   Where to look for UTxOs (required)
      * @param filter
      *   Optional refinement conditions
      * @param limit
      *   Maximum number of results to return
      * @param offset
      *   Number of results to skip (for pagination)
      * @param minRequiredTotalAmount
      *   Early termination: stop when accumulated lovelace reaches this amount
      */
    case class Simple(
        source: UtxoSource,
        filter: Option[UtxoFilter] = None,
        limit: Option[Int] = None,
        offset: Option[Int] = None,
        minRequiredTotalAmount: Option[Coin] = None
    ) extends UtxoQuery {

        /** Add a filter condition (AND with existing filters) */
        def &&(f: UtxoFilter): UtxoQuery = copy(filter = Some(filter.fold(f)(_ && f)))

        /** Limit the number of results. Takes minimum if limit already set. */
        def limit(n: Int): UtxoQuery =
            copy(limit = Some(limit.fold(n)(existing => math.min(existing, n))))

        /** Skip the first n results */
        def skip(n: Int): UtxoQuery = copy(offset = Some(n))

        /** Set minimum required total lovelace amount (early termination optimization). Takes
          * minimum if already set.
          */
        def minTotal(amount: Coin): UtxoQuery = copy(minRequiredTotalAmount =
            Some(
              minRequiredTotalAmount.fold(amount)(existing =>
                  if existing <= amount then existing else amount
              )
            )
        )
    }

    /** Combine two queries - each is evaluated with its own filters, results are merged.
      *
      * @param left
      *   First query
      * @param right
      *   Second query
      * @param limit
      *   Maximum number of results to return from combined result
      * @param offset
      *   Number of results to skip from combined result
      * @param minRequiredTotalAmount
      *   Early termination: stop when accumulated lovelace reaches this amount
      */
    case class Or(
        left: UtxoQuery,
        right: UtxoQuery,
        limit: Option[Int] = None,
        offset: Option[Int] = None,
        minRequiredTotalAmount: Option[Coin] = None
    ) extends UtxoQuery {

        /** Add a filter using distribution: (A || B) && f becomes (A && f) || (B && f) */
        def &&(f: UtxoFilter): UtxoQuery =
            Or(left && f, right && f, limit, offset, minRequiredTotalAmount)

        /** Limit the number of results. Takes minimum if limit already set. */
        def limit(n: Int): UtxoQuery =
            copy(limit = Some(limit.fold(n)(existing => math.min(existing, n))))

        /** Skip the first n results */
        def skip(n: Int): UtxoQuery = copy(offset = Some(n))

        /** Set minimum required total lovelace amount (early termination optimization). Takes
          * minimum if already set.
          */
        def minTotal(amount: Coin): UtxoQuery = copy(minRequiredTotalAmount =
            Some(
              minRequiredTotalAmount.fold(amount)(existing =>
                  if existing <= amount then existing else amount
              )
            )
        )
    }

    /** Create a simple query from a source */
    def apply(source: UtxoSource): Simple = Simple(source)

    /** Implicit conversion from UtxoSource to UtxoQuery for convenience */
    given Conversion[UtxoSource, UtxoQuery] = Simple(_)

    /** Evaluate a filter against a UTxO
      *
      * @param filter
      *   The filter to evaluate
      * @param utxo
      *   The UTxO (input, output) pair to test
      * @return
      *   true if the UTxO matches the filter
      */
    def evalFilter(filter: UtxoFilter, utxo: (TransactionInput, TransactionOutput)): Boolean = {
        val (_, output) = utxo
        filter match
            case UtxoFilter.HasAsset(policyId, assetName) =>
                output.value.assets.assets.get(policyId).exists(_.contains(assetName))
            case UtxoFilter.HasDatum(datum) =>
                (datum, output.datumOption) match
                    case (d1, Some(d2)) => d1.contentEquals(d2)
                    case _              => false
            case UtxoFilter.HasDatumHash(hash) =>
                output.datumOption.exists(_.dataHash == hash)
            case UtxoFilter.MinLovelace(amount) =>
                output.value.coin >= amount
            case UtxoFilter.AtInputs(inputs) =>
                inputs.contains(utxo._1)
            case UtxoFilter.And(left, right) =>
                evalFilter(left, utxo) && evalFilter(right, utxo)
            case UtxoFilter.Or(left, right) =>
                evalFilter(left, utxo) || evalFilter(right, utxo)
            case UtxoFilter.Not(f) =>
                !evalFilter(f, utxo)
    }

    /** Apply pagination and minTotal early termination to a result set
      *
      * @param candidates
      *   The UTxOs to paginate
      * @param limit
      *   Maximum number of results
      * @param offset
      *   Number of results to skip
      * @param minRequiredTotalAmount
      *   Stop when accumulated lovelace reaches this amount
      * @return
      *   The paginated UTxOs
      */
    def applyPagination(
        candidates: Utxos,
        limit: Option[Int],
        offset: Option[Int],
        minRequiredTotalAmount: Option[Coin]
    ): Utxos = {
        val paginated = candidates.drop(offset.getOrElse(0))
        minRequiredTotalAmount match
            case Some(minTotal) if minTotal.value > 0 =>
                val (collected, _) = paginated.foldLeft(
                  (Map.empty[TransactionInput, TransactionOutput], Coin(0))
                ) { case (acc @ (accUtxos, accAmount), (input, output)) =>
                    if accAmount >= minTotal then acc
                    else
                        val newAmount = Coin(accAmount.value + output.value.coin.value)
                        (accUtxos + (input -> output), newAmount)
                }
                limit.fold(collected)(n => collected.take(n))
            case _ =>
                limit.fold(paginated.toMap)(n => paginated.take(n).toMap)
    }

    /** Propagate limit and minTotal to a subquery for early termination
      *
      * @param q
      *   The query to propagate to
      * @param limit
      *   Optional limit to propagate
      * @param minTotal
      *   Optional minTotal to propagate
      * @return
      *   The query with propagated values
      */
    def propagate(q: UtxoQuery, limit: Option[Int], minTotal: Option[Coin]): UtxoQuery = {
        val withLimit = limit.fold(q)(q.limit)
        minTotal.fold(withLimit)(withLimit.minTotal)
    }
}

/** Errors that can occur when executing a UTxO query */
enum UtxoQueryError {

    /** No UTxOs found matching the query */
    case NotFound(source: UtxoSource)

    /** Query is not supported by this provider */
    case NotSupported(query: UtxoQuery, reason: String)

    /** Network communication error */
    case NetworkError(message: String, cause: Option[Throwable] = None)

    /** Rate limit exceeded */
    case RateLimited(retryAfter: Option[Duration] = None)

    /** Authentication failed */
    case AuthenticationError(message: String)
}
