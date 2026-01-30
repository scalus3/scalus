package scalus.cardano.node

import scalus.cardano.address.Address
import scalus.cardano.ledger.*

import scala.concurrent.Future

@deprecated("Use BlockchainProvider instead", "0.14.2")
type Provider = BlockchainProvider

/** Trait for blockchain providers with generic effect type. (TF is for "tagless final" style, often
  * term used in FP literature).
  */
trait BlockchainProviderTF[F[_]] {

    /** Returns CardanoInfo for this provider.
      */
    def cardanoInfo: CardanoInfo

    /** Fetches the latest protocol parameters from the network.
      */
    def fetchLatestParams: F[ProtocolParams]

    /** Submits a transaction to the network.
      */
    def submit(transaction: Transaction): F[Either[SubmitError, TransactionHash]]

    /** Find UTxOs using a type-safe query.
      */
    def findUtxos(query: UtxoQuery): F[Either[UtxoQueryError, Utxos]]

    /** Returns the current slot number.
      */
    def currentSlot: F[SlotNo]

}

/** Provider for Cardano blockchain operations.
  *
  * Provider is the cross-platform interface for interacting with Cardano nodes. All methods return
  * `Future` values and work on both JVM and JavaScript platforms.
  *
  * Implementations capture their ExecutionContext at construction time, so callers don't need to
  * provide it for each method call.
  *
  * Use `scalus.utils.await` extension for blocking operations on JVM when needed.
  */
trait BlockchainProvider extends BlockchainProviderTF[Future] {

    /** Returns the ExecutionContext captured by this provider.
      *
      * This is used internally by default method implementations. External code can use this when
      * working with Futures returned by provider methods, or provide their own.
      */
    def executionContext: scala.concurrent.ExecutionContext

    /** Returns CardanoInfo for this provider.
      *
      * This is always available synchronously after the provider is constructed. For emulators,
      * this returns the current context. For remote providers like BlockfrostProvider, the
      * CardanoInfo is fetched during async construction.
      */
    def cardanoInfo: CardanoInfo

    def fetchLatestParams: Future[ProtocolParams]

    def submit(transaction: Transaction): Future[Either[SubmitError, TransactionHash]]

    /** Find a single UTxO by its transaction input.
      *
      * @param input
      *   the transaction input to look up
      * @return
      *   Either a UtxoQueryError or the found Utxo
      */
    def findUtxo(input: TransactionInput): Future[Either[UtxoQueryError, Utxo]] = {
        findUtxos(UtxoQuery(UtxoSource.FromInputs(Set(input)))).map { result =>
            result.flatMap { utxos =>
                utxos.headOption match
                    case Some((i, o)) => Right(Utxo(i, o))
                    case None => Left(UtxoQueryError.NotFound(UtxoSource.FromInputs(Set(input))))
            }
        }(executionContext)
    }

    /** Find UTxOs by a set of transaction inputs.
      *
      * @param inputs
      *   the transaction inputs to look up
      * @return
      *   Either a UtxoQueryError or the found UTxOs (fails if not all inputs are found)
      */
    def findUtxos(inputs: Set[TransactionInput]): Future[Either[UtxoQueryError, Utxos]] = {
        findUtxos(UtxoQuery(UtxoSource.FromInputs(inputs))).map { result =>
            result.flatMap { foundUtxos =>
                if foundUtxos.size == inputs.size then Right(foundUtxos)
                else Left(UtxoQueryError.NotFound(UtxoSource.FromInputs(inputs)))
            }
        }(executionContext)
    }

    /** Find all UTxOs at the given address.
      *
      * @param address
      *   the address to query
      * @return
      *   Either a UtxoQueryError or the found UTxOs
      */
    def findUtxos(address: Address): Future[Either[UtxoQueryError, Utxos]] = {
        findUtxos(UtxoQuery(UtxoSource.FromAddress(address)))
    }

    /** Find a single UTxO by address and optional filters.
      *
      * @deprecated
      *   Use findUtxos(UtxoQuery) instead
      */
    @deprecated("Use findUtxos(UtxoQuery) instead", "0.14.2")
    def findUtxo(
        address: Address,
        transactionId: Option[TransactionHash] = None,
        datum: Option[DatumOption] = None,
        minAmount: Option[Coin] = None
    ): Future[Either[UtxoQueryError, Utxo]] = {
        findUtxos(address, transactionId, datum, minAmount, None).map { result =>
            result.flatMap { utxos =>
                utxos.headOption match
                    case Some((i, o)) => Right(Utxo(i, o))
                    case None => Left(UtxoQueryError.NotFound(UtxoSource.FromAddress(address)))
            }
        }(executionContext)
    }

    /** Find UTxOs by address and optional filters.
      *
      * @deprecated
      *   Use findUtxos(UtxoQuery) instead
      */
    @deprecated("Use findUtxos(UtxoQuery) instead", "0.14.2")
    def findUtxos(
        address: Address,
        transactionId: Option[TransactionHash] = None,
        datum: Option[DatumOption] = None,
        minAmount: Option[Coin] = None,
        minRequiredTotalAmount: Option[Coin] = None
    ): Future[Either[UtxoQueryError, Utxos]] = {
        if minRequiredTotalAmount.exists(_ <= Coin(0)) then
            return Future.successful(Right(Map.empty))

        // Build source using And combinator when transactionId is provided
        val source: UtxoSource = transactionId match
            case Some(txId) => UtxoSource.FromAddress(address) && UtxoSource.FromTransaction(txId)
            case None       => UtxoSource.FromAddress(address)

        // Build the query
        var query: UtxoQuery = UtxoQuery(source)

        // Add minRequiredTotalAmount
        query = minRequiredTotalAmount.fold(query)(amt => query.minTotal(amt))

        // Add datum filter
        query = datum.fold(query)(d => query && UtxoFilter.HasDatum(d))

        // Add minAmount filter
        query = minAmount.fold(query)(amt => query && UtxoFilter.MinLovelace(amt))

        findUtxos(query)
    }

    /** Returns the current slot number.
      */
    def currentSlot: Future[SlotNo]

    /** Find UTxOs using a type-safe query.
      *
      * @param query
      *   The query specifying source, filters, and pagination
      * @return
      *   Either a UtxoQueryError or the matching UTxOs
      */
    def findUtxos(query: UtxoQuery): Future[Either[UtxoQueryError, Utxos]]

    /** Query UTxOs using lambda DSL.
      *
      * This method translates a lambda expression to a UtxoQuery at compile time and returns a
      * builder that can be further configured before execution.
      *
      * Example:
      * {{{
      * // Simple query - execute immediately
      * provider.queryUtxos { u =>
      *   u.output.address == myAddress
      * }.execute()
      *
      * // With pagination and minimum total
      * provider.queryUtxos { u =>
      *   u.output.address == myAddress && u.output.value.hasAsset(policyId, assetName)
      * }.minTotal(Coin.ada(100)).limit(10).execute()
      * }}}
      *
      * Supported expressions:
      *   - `u.output.address == addr` - query by address
      *   - `u.input.transactionId == txId` - query by transaction
      *   - `u.output.value.hasAsset(policyId, assetName)` - query/filter by asset
      *   - `u.output.value.coin >= amount` - filter by minimum lovelace
      *   - `u.output.hasDatumHash(hash)` - filter by datum hash
      *   - `&&` - AND combination
      *   - `||` - OR combination
      *
      * @param f
      *   Lambda expression from Utxo to Boolean
      * @return
      *   A UtxoQueryWithProvider builder that can be configured and executed
      */
    inline def queryUtxos(inline f: Utxo => Boolean): UtxoQueryWithProvider =
        UtxoQueryWithProvider(this, UtxoQueryMacros.buildQuery(f))
}

/** A query builder that combines a Provider with a UtxoQuery.
  *
  * Allows chaining configuration methods before executing the query.
  *
  * @param provider
  *   The provider to execute the query against
  * @param query
  *   The query to execute
  */
case class UtxoQueryWithProvider(provider: BlockchainProvider, query: UtxoQuery) {

    /** Limit the number of results */
    def limit(n: Int): UtxoQueryWithProvider = copy(query = query.limit(n))

    /** Skip the first n results */
    def skip(n: Int): UtxoQueryWithProvider = copy(query = query.skip(n))

    /** Set minimum required total lovelace amount (early termination optimization).
      *
      * The query will stop fetching UTxOs once the accumulated lovelace reaches this amount.
      */
    def minTotal(amount: Coin): UtxoQueryWithProvider = copy(query = query.minTotal(amount))

    /** Execute the query and return the results */
    def execute(): Future[Either[UtxoQueryError, Utxos]] =
        provider.findUtxos(query)
}

/** Error returned when submitting a transaction fails.
  *
  * Errors are organized into two categories:
  *   - [[NetworkSubmitError]]: Communication/operational errors (connection, auth, rate limits)
  *   - [[NodeSubmitError]]: Transaction validation errors (invalid inputs, expired, script
  *     failures)
  */
sealed trait SubmitError {
    def message: String
}

/** Network-level errors that occur during communication with the node/provider.
  *
  * These errors are typically transient and may be worth retrying.
  */
sealed trait NetworkSubmitError extends SubmitError

object NetworkSubmitError {

    /** Network-level errors (connection failures, timeouts) */
    case class ConnectionError(message: String, cause: Option[Throwable] = None)
        extends NetworkSubmitError

    /** Authentication/authorization errors (HTTP 403) */
    case class AuthenticationError(message: String) extends NetworkSubmitError

    /** Rate limiting errors (HTTP 402, 429) */
    case class RateLimited(message: String) extends NetworkSubmitError

    /** Auto-banned for flooding (HTTP 418) */
    case class Banned(message: String) extends NetworkSubmitError

    /** Mempool full (HTTP 425) */
    case class MempoolFull(message: String) extends NetworkSubmitError

    /** Internal provider errors (HTTP 500+) */
    case class InternalError(message: String, cause: Option[Throwable] = None)
        extends NetworkSubmitError
}

/** Node validation errors that occur when the transaction is rejected by the ledger.
  *
  * These errors indicate the transaction is invalid and needs to be modified before resubmission.
  */
sealed trait NodeSubmitError extends SubmitError

object NodeSubmitError {

    /** UTXO inputs not available - already spent or never existed.
      *
      * This is the key error for handling race conditions where UTxOs become unavailable between
      * collection and submission. Maps to Cardano ledger `BadInputsUTxO` errors.
      *
      * @param message
      *   the original error message
      * @param unavailableInputs
      *   best-effort set of unavailable inputs (may be empty if parsing failed)
      */
    case class UtxoNotAvailable(
        message: String,
        unavailableInputs: Set[TransactionInput] = Set.empty
    ) extends NodeSubmitError

    /** Transaction expired - validity window passed (maps to OutsideValidityInterval) */
    case class TransactionExpired(message: String) extends NodeSubmitError

    /** Value/balance errors - input/output value mismatch (maps to ValueNotConserved) */
    case class ValueNotConserved(message: String) extends NodeSubmitError

    /** Script execution failures */
    case class ScriptFailure(
        message: String,
        scriptHash: Option[ScriptHash] = None,
        logs: Seq[String] = Seq.empty
    ) extends NodeSubmitError

    /** Other node validation errors (catch-all for unrecognized validation errors) */
    case class ValidationError(message: String, errorCode: Option[String] = None)
        extends NodeSubmitError
}

object SubmitError {
    // Type aliases for backwards compatibility
    type ConnectionError = NetworkSubmitError.ConnectionError
    val ConnectionError = NetworkSubmitError.ConnectionError

    /** @deprecated Use ConnectionError instead */
    @deprecated("Use ConnectionError instead", "0.14.2")
    type NetworkError = NetworkSubmitError.ConnectionError
    @deprecated("Use ConnectionError instead", "0.14.2")
    val NetworkError = NetworkSubmitError.ConnectionError

    type AuthenticationError = NetworkSubmitError.AuthenticationError
    val AuthenticationError = NetworkSubmitError.AuthenticationError

    type RateLimited = NetworkSubmitError.RateLimited
    val RateLimited = NetworkSubmitError.RateLimited

    type Banned = NetworkSubmitError.Banned
    val Banned = NetworkSubmitError.Banned

    type MempoolFull = NetworkSubmitError.MempoolFull
    val MempoolFull = NetworkSubmitError.MempoolFull

    type InternalError = NetworkSubmitError.InternalError
    val InternalError = NetworkSubmitError.InternalError

    type UtxoNotAvailable = NodeSubmitError.UtxoNotAvailable
    val UtxoNotAvailable = NodeSubmitError.UtxoNotAvailable

    type TransactionExpired = NodeSubmitError.TransactionExpired
    val TransactionExpired = NodeSubmitError.TransactionExpired

    type ValueNotConserved = NodeSubmitError.ValueNotConserved
    val ValueNotConserved = NodeSubmitError.ValueNotConserved

    type ScriptFailure = NodeSubmitError.ScriptFailure
    val ScriptFailure = NodeSubmitError.ScriptFailure

    type ValidationError = NodeSubmitError.ValidationError
    val ValidationError = NodeSubmitError.ValidationError

    /** Create a SubmitError from an HTTP status code and message.
      *
      * This is used by HTTP-based providers like Blockfrost to classify errors based on status
      * codes and error message patterns.
      */
    def fromHttpResponse(statusCode: Int, message: String): SubmitError = statusCode match
        case 402 | 429 => RateLimited(message)
        case 403       => AuthenticationError(message)
        case 418       => Banned(message)
        case 425       => MempoolFull(message)
        case c if c >= 500 =>
            InternalError(message)
        case _ =>
            // For 400-level errors, try to parse the message to classify
            parseValidationError(message)

    /** Parse a validation error message to classify it into a specific SubmitError type.
      *
      * This attempts to match known Cardano ledger error patterns.
      */
    def parseValidationError(message: String): SubmitError = {
        val lowerMessage = message.toLowerCase

        if lowerMessage.contains("badinputsutxo") || lowerMessage.contains("bad inputs")
        then UtxoNotAvailable(message, tryParseInputs(message))
        else if lowerMessage.contains("outsidevalidityinterval") || lowerMessage.contains(
              "expired"
            ) || lowerMessage.contains("outside") && lowerMessage.contains("validity")
        then TransactionExpired(message)
        else if lowerMessage.contains("valuenotconserved") || lowerMessage.contains(
              "value not conserved"
            )
        then ValueNotConserved(message)
        else if lowerMessage.contains("script") && (lowerMessage.contains("fail") || lowerMessage
                .contains("error"))
        then ScriptFailure(message)
        else ValidationError(message, extractErrorCode(message))
    }

    /** Try to extract TransactionInput references from an error message.
      *
      * This is a best-effort attempt - returns empty set if parsing fails.
      */
    private def tryParseInputs(message: String): Set[TransactionInput] = {
        // Pattern for tx hash (64 hex chars) followed by # and index
        val pattern = "([0-9a-fA-F]{64})#(\\d+)".r
        pattern
            .findAllMatchIn(message)
            .flatMap { m =>
                try {
                    val txHash = TransactionHash.fromHex(m.group(1))
                    val index = m.group(2).toInt
                    Some(TransactionInput(txHash, index))
                } catch {
                    case _: Exception => None
                }
            }
            .toSet
    }

    /** Try to extract an error code from the message (e.g., "BadInputsUTxO"). */
    private def extractErrorCode(message: String): Option[String] = {
        // Look for common patterns like "BadInputsUTxO" or error codes in parentheses
        val patterns = Seq(
          "(BadInputsUTxO|ValueNotConserved|OutsideValidityInterval|FeeTooSmall|ScriptFailure|ExUnitsTooBig)".r,
          "\\(([A-Z][a-zA-Z]+)\\)".r
        )
        patterns.view
            .flatMap(_.findFirstMatchIn(message).map(_.group(1)))
            .headOption
    }

    /** Create a SubmitError from a TransactionException.
      *
      * This is used by the Emulator to map internal validation exceptions to SubmitError.
      */
    def fromException(ex: TransactionException): SubmitError = ex match
        case e: TransactionException.BadAllInputsUTxOException =>
            val inputs =
                e.missingInputs ++ e.missingCollateralInputs ++ e.missingReferenceInputs
            UtxoNotAvailable(e.explain, inputs)
        case e: TransactionException.BadInputsUTxOException =>
            UtxoNotAvailable(e.explain)
        case e: TransactionException.BadCollateralInputsUTxOException =>
            UtxoNotAvailable(e.explain)
        case e: TransactionException.BadReferenceInputsUTxOException =>
            UtxoNotAvailable(e.explain)
        case e: TransactionException.OutsideValidityIntervalException =>
            TransactionExpired(e.explain)
        case e: TransactionException.ValueNotConservedUTxOException =>
            ValueNotConserved(e.explain)
        case e: TransactionException.NativeScriptsException =>
            ScriptFailure(e.explain)
        case e: TransactionException.PlutusScriptValidationException =>
            ScriptFailure(e.explain, logs = e.logs)
        case e =>
            ValidationError(e.explain)
}
