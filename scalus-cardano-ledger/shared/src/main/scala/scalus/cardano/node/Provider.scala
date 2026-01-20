package scalus.cardano.node

import scalus.cardano.address.Address
import scalus.cardano.ledger.*

import scala.concurrent.{ExecutionContext, Future}

/** Provider for Cardano blockchain operations.
  *
  * Provider is the cross-platform interface for interacting with Cardano nodes. All methods return
  * `Future` values and work on both JVM and JavaScript platforms.
  *
  * Use `scalus.utils.await` extension for blocking operations on JVM when needed.
  */
trait Provider {

    def fetchLatestParams(using ExecutionContext): Future[ProtocolParams]

    def submit(transaction: Transaction)(using
        ExecutionContext
    ): Future[Either[SubmitError, TransactionHash]]

    /** Find a single UTxO by its transaction input.
      *
      * @deprecated
      *   Use findUtxos(UtxoQuery) instead
      */
    @deprecated("Use findUtxos(UtxoQuery) instead", "0.14.2")
    def findUtxo(input: TransactionInput)(using
        ExecutionContext
    ): Future[Either[UtxoQueryError, Utxo]]

    /** Find UTxOs by a set of transaction inputs.
      *
      * @deprecated
      *   Use findUtxos(UtxoQuery) instead
      */
    @deprecated("Use findUtxos(UtxoQuery) instead", "0.14.2")
    def findUtxos(inputs: Set[TransactionInput])(using
        ExecutionContext
    ): Future[Either[UtxoQueryError, Utxos]]

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
    )(using ExecutionContext): Future[Either[UtxoQueryError, Utxo]]

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
    )(using ExecutionContext): Future[Either[UtxoQueryError, Utxos]]

    /** Find UTxOs using a type-safe query.
      *
      * @param query
      *   The query specifying source, filters, and pagination
      * @return
      *   Either a UtxoQueryError or the matching UTxOs
      */
    def findUtxos(query: UtxoQuery)(using ExecutionContext): Future[Either[UtxoQueryError, Utxos]]
}

enum SubmitError:
    /** Network-level errors (connection failures, timeouts) */
    case NetworkError(message: String, exception: Option[Throwable] = None)

    /** Authentication/authorization errors (HTTP 403) */
    case AuthenticationError(message: String)

    /** Rate limiting errors (HTTP 402, 429) */
    case RateLimited(message: String)

    /** Mempool full (HTTP 425) */
    case MempoolFull(message: String)

    /** Auto-banned for flooding (HTTP 418) */
    case Banned(message: String)

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
    case UtxoNotAvailable(message: String, unavailableInputs: Set[TransactionInput] = Set.empty)

    /** Transaction expired - validity window passed (maps to OutsideValidityInterval) */
    case TransactionExpired(message: String)

    /** Value/balance errors - input/output value mismatch (maps to ValueNotConserved) */
    case ValueNotConserved(message: String)

    /** Script execution failures */
    case ScriptFailure(message: String, scriptHash: Option[ScriptHash] = None)

    /** Other node validation errors (catch-all for unrecognized validation errors) */
    case ValidationError(message: String, errorCode: Option[String] = None)

    /** Internal provider errors (HTTP 500+) */
    case InternalError(message: String, exception: Option[Throwable] = None)

    /** @deprecated Use more specific error types. This is kept for backwards compatibility. */
    @deprecated("Use more specific error types like ValidationError", "0.14.2") case NodeError(
        message: String,
        exception: Option[Throwable] = None
    )

object SubmitError {

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
        case e =>
            ValidationError(e.explain)
}
