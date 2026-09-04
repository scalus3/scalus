package scalus.cardano.node

import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.uplc.builtin.Data

import scala.concurrent.Future

/** Read-only blockchain operations with generic effect type. (TF is for "tagless final" style,
  * often term used in FP literature).
  *
  * This trait provides read-only access to blockchain state. Use [[BlockchainProviderTF]] when you
  * also need to submit transactions.
  */
trait BlockchainReaderTF[F[_]] {

    /** Returns CardanoInfo for this provider.
      */
    def cardanoInfo: CardanoInfo

    /** Fetches the latest protocol parameters from the network.
      */
    def fetchLatestParams: F[ProtocolParams]

    /** Find UTxOs using a type-safe query.
      */
    def findUtxos(query: UtxoQuery): F[Either[UtxoQueryError, Utxos]]

    /** Find a single UTxO by its transaction input.
      *
      * @return
      *   Right(utxo) if found, Left(NotFound) otherwise
      */
    def findUtxo(input: TransactionInput): F[Either[UtxoQueryError, Utxo]] =
        mapF(findUtxos(UtxoQuery(UtxoSource.FromInputs(Set(input))))) { result =>
            result.flatMap { utxos =>
                utxos.headOption match
                    case Some((i, o)) => Right(Utxo(i, o))
                    case None => Left(UtxoQueryError.NotFound(UtxoSource.FromInputs(Set(input))))
            }
        }

    /** Find UTxOs by a set of transaction inputs (fails with NotFound if not all are found). */
    def findUtxos(inputs: Set[TransactionInput]): F[Either[UtxoQueryError, Utxos]] =
        mapF(findUtxos(UtxoQuery(UtxoSource.FromInputs(inputs)))) { result =>
            result.flatMap { foundUtxos =>
                if foundUtxos.size == inputs.size then Right(foundUtxos)
                else Left(UtxoQueryError.NotFound(UtxoSource.FromInputs(inputs)))
            }
        }

    /** Find all UTxOs at the given address. */
    def findUtxos(address: Address): F[Either[UtxoQueryError, Utxos]] =
        findUtxos(UtxoQuery(UtxoSource.FromAddress(address)))

    /** Map over this reader's effect `F` — the one primitive needed to give the convenience lookups
      * above ([[findUtxo]] and the `findUtxos` overloads) a single effect-polymorphic default,
      * without imposing an external Functor/Monad constraint on `F`. Future-based readers map via
      * their captured ExecutionContext; monadic effects map via their monad.
      */
    protected def mapF[A, B](fa: F[A])(f: A => B): F[B]

    /** Returns the current slot number.
      */
    def currentSlot: F[SlotNo]

    /** Look up a datum by its hash. Returns `None` if unknown. */
    def getDatum(datumHash: DataHash): F[Option[Data]]

    /** Check the status of a transaction on the blockchain.
      *
      * @param txHash
      *   the transaction hash to check
      * @return
      *   the current status of the transaction
      */
    def checkTransaction(txHash: TransactionHash): F[TransactionStatus]

    /** Query UTxOs using a lambda DSL.
      *
      * Translates the lambda to a [[UtxoQuery]] at compile time and returns a builder that can be
      * further configured before execution. Effect-polymorphic — the resulting `.execute()` returns
      * the reader's own `F[Either[UtxoQueryError, Utxos]]`, so the same call shape works on
      * Future-typed providers (Blockfrost, JS) and direct-style providers (ox `Id`).
      *
      * Example:
      * {{{
      * // Simple query — execute immediately
      * reader.queryUtxos { u =>
      *   u.output.address == myAddress
      * }.execute()
      *
      * // With pagination and minimum total
      * reader.queryUtxos { u =>
      *   u.output.address == myAddress && u.output.value.hasAsset(policyId, assetName)
      * }.minTotal(Coin.ada(100)).limit(10).execute()
      * }}}
      *
      * Supported expressions:
      *   - `u.output.address == addr` — query by address
      *   - `u.input.transactionId == txId` — query by transaction
      *   - `u.output.value.hasAsset(policyId, assetName)` — query/filter by asset
      *   - `u.output.value.coin >= amount` — filter by minimum lovelace
      *   - `u.output.hasDatumHash(hash)` — filter by datum hash
      *   - `&&` — AND combination
      *   - `||` — OR combination
      *
      * @param f
      *   Lambda expression from Utxo to Boolean
      * @return
      *   A [[UtxoQueryWithReaderTF]] builder over this reader's effect type
      */
    inline def queryUtxos(inline f: Utxo => Boolean): UtxoQueryWithReaderTF[F] =
        UtxoQueryWithReaderTF[F](this, UtxoQueryMacros.buildQuery(f))
}

/** A query builder that combines a [[BlockchainReaderTF]] with a [[UtxoQuery]]. Effect-polymorphic
  * — `.execute()` returns the reader's own `F[Either[UtxoQueryError, Utxos]]`.
  *
  * @param reader
  *   The reader to execute the query against
  * @param query
  *   The query to execute
  */
case class UtxoQueryWithReaderTF[F[_]](reader: BlockchainReaderTF[F], query: UtxoQuery) {

    /** Limit the number of results */
    def limit(n: Int): UtxoQueryWithReaderTF[F] = copy(query = query.limit(n))

    /** Skip the first n results */
    def skip(n: Int): UtxoQueryWithReaderTF[F] = copy(query = query.skip(n))

    /** Set minimum required total lovelace amount (early termination optimization).
      *
      * The query will stop fetching UTxOs once the accumulated lovelace reaches this amount.
      */
    def minTotal(amount: Coin): UtxoQueryWithReaderTF[F] = copy(query = query.minTotal(amount))

    /** Execute the query and return the results in the reader's effect type. */
    def execute(): F[Either[UtxoQueryError, Utxos]] = reader.findUtxos(query)
}

/** Trait for blockchain providers with generic effect type. (TF is for "tagless final" style, often
  * term used in FP literature).
  *
  * Extends [[BlockchainReaderTF]] with transaction submission capability.
  */
trait BlockchainProviderTF[F[_]] extends BlockchainReaderTF[F] {

    /** Submits a transaction to the network.
      */
    def submit(transaction: Transaction): F[Either[SubmitError, TransactionHash]]

    /** Poll for transaction confirmation, checking periodically until confirmed or max attempts
      * reached.
      *
      * @param txHash
      *   the transaction hash to poll for
      * @param maxAttempts
      *   maximum number of polling attempts (default 60)
      * @param delayMs
      *   delay between attempts in milliseconds (default 1000)
      * @return
      *   the last observed transaction status
      */
    def pollForConfirmation(
        txHash: TransactionHash,
        maxAttempts: Int = 60,
        delayMs: Long = 1000
    ): F[TransactionStatus]

    /** Submit a transaction and poll until confirmed.
      *
      * Composes [[submit]] and [[pollForConfirmation]]: submits the transaction, then polls until
      * it is confirmed or the maximum number of attempts is reached.
      *
      * @param transaction
      *   the transaction to submit
      * @param maxAttempts
      *   maximum number of polling attempts (default 60)
      * @param delayMs
      *   delay between attempts in milliseconds (default 1000)
      * @return
      *   Right(txHash) if confirmed, Left(error) if submission failed or not confirmed
      */
    def submitAndPoll(
        transaction: Transaction,
        maxAttempts: Int = 60,
        delayMs: Long = 1000
    ): F[Either[SubmitError, TransactionHash]]

}

/** Read-only provider for Cardano blockchain operations.
  *
  * BlockchainReader provides read-only access to blockchain state without the ability to submit
  * transactions. This is useful for:
  *   - Snapshot-based testing where state should not be modified
  *   - Transaction building that only needs to query UTxOs
  *   - APIs that should not have submit capability
  *
  * Use [[BlockchainProvider]] when you also need to submit transactions.
  *
  * Implementations capture their ExecutionContext at construction time, so callers don't need to
  * provide it for each method call.
  */
trait BlockchainReader extends BlockchainReaderTF[Future] {

    /** Returns the ExecutionContext captured by this reader.
      *
      * This is used internally by default method implementations. External code can use this when
      * working with Futures returned by reader methods, or provide their own.
      */
    def executionContext: scala.concurrent.ExecutionContext

    /** Returns CardanoInfo for this reader.
      *
      * This is always available synchronously after the reader is constructed. For emulators, this
      * returns the current context. For remote providers like BlockfrostProvider, the CardanoInfo
      * is fetched during async construction.
      */
    def cardanoInfo: CardanoInfo

    def fetchLatestParams: Future[ProtocolParams]

    /** Map over the `Future` effect using this reader's captured ExecutionContext. */
    override protected def mapF[A, B](fa: Future[A])(f: A => B): Future[B] =
        fa.map(f)(using executionContext)

    /** Returns the current slot number.
      */
    def currentSlot: Future[SlotNo]

    def getDatum(datumHash: DataHash): Future[Option[Data]]

    /** Find UTxOs using a type-safe query.
      *
      * @param query
      *   The query specifying source, filters, and pagination
      * @return
      *   Either a UtxoQueryError or the matching UTxOs
      */
    def findUtxos(query: UtxoQuery): Future[Either[UtxoQueryError, Utxos]]

    /** Check the status of a transaction on the blockchain.
      *
      * Fallback implementation: look for UTxOs produced by the transaction. A provider that can
      * answer authoritatively should override this — `BlockfrostProvider` asks the API, and
      * `EmulatorBase` consults its applied-transaction index — because this inference has a blind
      * spot in each direction. It cannot see a mempool, so a pending transaction reads as
      * `NotFound`; and a transaction all of whose outputs have since been spent also reads as
      * `NotFound`, since it produces no current UTxOs.
      *
      * What it must not do is report a transaction that was never submitted as `Confirmed`. An
      * empty result is therefore not a confirmation — the inherited
      * [[BlockchainProviderTF.submitAndPoll]] builds on this answer, and "confirmed" for a
      * transaction that never reached the chain is the one wrong answer that silently corrupts a
      * caller's state.
      */
    def checkTransaction(txHash: TransactionHash): Future[TransactionStatus] =
        findUtxos(UtxoQuery(UtxoSource.FromTransaction(txHash))).map {
            case Right(utxos) if utxos.nonEmpty => TransactionStatus.Confirmed
            case _                              => TransactionStatus.NotFound
        }(using executionContext)

}

/** Provider for Cardano blockchain operations.
  *
  * Provider is the cross-platform interface for interacting with Cardano nodes. All methods return
  * `Future` values and work on both JVM and JavaScript platforms.
  *
  * Extends [[BlockchainReader]] with transaction submission capability.
  *
  * Implementations capture their ExecutionContext at construction time, so callers don't need to
  * provide it for each method call.
  *
  * Use `scalus.utils.await` extension for blocking operations on JVM when needed.
  */
trait BlockchainProvider extends BlockchainProviderTF[Future] with BlockchainReader {

    // Inherits from BlockchainReader:
    // - executionContext
    // - cardanoInfo
    // - fetchLatestParams
    // - currentSlot
    // - findUtxos(query: UtxoQuery)
    // - findUtxo(input: TransactionInput) (default impl)
    // - findUtxos(inputs: Set[TransactionInput]) (default impl)
    // - findUtxos(address: Address) (default impl)
    // - queryUtxos (returns UtxoQueryWithReaderTF[Future])

    def submit(transaction: Transaction): Future[Either[SubmitError, TransactionHash]]

    /** Poll for transaction confirmation.
      *
      * Default implementation for emulators: a single check is sufficient since confirmation is
      * instant.
      */
    def pollForConfirmation(
        txHash: TransactionHash,
        maxAttempts: Int = 60,
        delayMs: Long = 1000
    ): Future[TransactionStatus] =
        checkTransaction(txHash)

    /** Submit a transaction and poll until confirmed.
      *
      * Default implementation composes [[submit]] and [[pollForConfirmation]].
      */
    def submitAndPoll(
        transaction: Transaction,
        maxAttempts: Int = 60,
        delayMs: Long = 1000
    ): Future[Either[SubmitError, TransactionHash]] =
        submit(transaction).flatMap {
            case Left(err) => Future.successful(Left(err))
            case Right(txHash) =>
                pollForConfirmation(txHash, maxAttempts, delayMs).map {
                    case TransactionStatus.Confirmed => Right(txHash)
                    case status =>
                        Left(
                          NetworkSubmitError.ConnectionError(
                            s"Transaction ${txHash.toHex} not confirmed, last status: $status"
                          )
                        )
                }(using executionContext)
        }(using executionContext)

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

    /** Short, stable name for the condition that produced this error, e.g. `"ValueNotConserved"`.
      * Stable enough to assert on in a test, unlike [[message]].
      *
      * The name is the case's own name for every case but one, whichever provider produced the
      * error, so `err.rule == "UtxoNotAvailable"` means the same thing against the emulator and
      * against a network provider. The exception is [[NodeSubmitError.ValidationError]], the
      * catch-all: it reports its [[NodeSubmitError.ValidationError.errorCode]] when the producer
      * supplied one, which is how the specific condition behind an otherwise unclassified rejection
      * reaches a caller. Those codes are producer-specific — the emulator names the ledger rule
      * that rejected the transaction (`"FeesOk"`, `"MissingKeyHashes"`, … — see
      * [[scalus.cardano.ledger.TransactionException.ruleName]]), while an HTTP provider names
      * whatever its error body mentioned.
      *
      * A ledger rule that maps onto one of the classified cases is therefore *not* visible here.
      * Only rules that really are the same condition are folded together: `BadInputsUTxO` and
      * `BadAllInputsUTxO` both report `"UtxoNotAvailable"`, `NativeScripts` and
      * `PlutusScriptValidation` both report `"ScriptFailure"`, and only [[message]] says which.
      * `BadCollateralInputsUTxO` and `BadReferenceInputsUTxO` are different conditions and reach
      * callers under their own names. This is deliberate: the classified cases are part of the
      * published 1.x API, and refining their rule name per instance would mean a constructor
      * parameter and a binary break for every caller that pattern-matches them.
      */
    def rule: String
}

/** Network-level errors that occur during communication with the node/provider.
  *
  * These errors are typically transient and may be worth retrying.
  */
sealed trait NetworkSubmitError extends SubmitError

object NetworkSubmitError {

    /** Network-level errors (connection failures, timeouts) */
    case class ConnectionError(message: String, cause: Option[Throwable] = None)
        extends NetworkSubmitError {
        def rule: String = productPrefix
    }

    /** Authentication/authorization errors (HTTP 403) */
    case class AuthenticationError(message: String) extends NetworkSubmitError {
        def rule: String = productPrefix
    }

    /** Rate limiting errors (HTTP 402, 429) */
    case class RateLimited(message: String) extends NetworkSubmitError {
        def rule: String = productPrefix
    }

    /** Auto-banned for flooding (HTTP 418) */
    case class Banned(message: String) extends NetworkSubmitError {
        def rule: String = productPrefix
    }

    /** Mempool full (HTTP 425) */
    case class MempoolFull(message: String) extends NetworkSubmitError {
        def rule: String = productPrefix
    }

    /** Internal provider errors (HTTP 500+) */
    case class InternalError(message: String, cause: Option[Throwable] = None)
        extends NetworkSubmitError {
        def rule: String = productPrefix
    }
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
    ) extends NodeSubmitError {
        def rule: String = productPrefix
    }

    /** Transaction expired - validity window passed (maps to OutsideValidityInterval) */
    case class TransactionExpired(message: String) extends NodeSubmitError {
        def rule: String = productPrefix
    }

    /** Value/balance errors - input/output value mismatch (maps to ValueNotConserved) */
    case class ValueNotConserved(message: String) extends NodeSubmitError {
        def rule: String = productPrefix
    }

    /** Script execution failures */
    case class ScriptFailure(
        message: String,
        logs: Seq[String] = Seq.empty,
        scriptHash: Option[ScriptHash] = None,
        spentBudget: ExUnits = ExUnits.zero
    ) extends NodeSubmitError {
        def rule: String = productPrefix
    }

    /** Other node validation errors (catch-all for unrecognized validation errors).
      *
      * @param errorCode
      *   the producer's own name for the condition, when it has one — the emulator puts the ledger
      *   rule that rejected the transaction here (see
      *   [[scalus.cardano.ledger.TransactionException.ruleName]]), an HTTP provider whatever it
      *   could extract from the error body. [[rule]] reports it, so an unclassified rejection is
      *   still assertable by name.
      */
    case class ValidationError(
        message: String,
        errorCode: Option[String] = None
    ) extends NodeSubmitError {
        def rule: String = errorCode.getOrElse(productPrefix)
    }
}

object SubmitError {
    // Type aliases for backwards compatibility
    type ConnectionError = NetworkSubmitError.ConnectionError
    val ConnectionError = NetworkSubmitError.ConnectionError

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
      *
      * The classification it produces is the same vocabulary the emulator's [[fromException]]
      * produces, so [[SubmitError.rule]] means the same thing whichever provider a caller is
      * pointed at. Only the [[NodeSubmitError.ValidationError]] fallback differs: its `errorCode`
      * is whatever [[extractErrorCode]] could find in the provider's own prose, which is not the
      * emulator's ledger-rule vocabulary.
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
                    Some(Input(txHash, index))
                } catch {
                    case _: Exception => None
                }
            }
            .toSet
    }

    /** Try to extract an error code from the message (e.g., "BadCollateralInputsUTxO"). */
    private def extractErrorCode(message: String): Option[String] = {
        // Look for a known rule name, or an error code in parentheses.
        //
        // Only [[parseValidationError]]'s fallback calls this, so the names it classifies before
        // reaching that fallback cannot appear here and are deliberately absent: a message holding
        // `BadInputsUTxO`, `ValueNotConserved`, `OutsideValidityInterval` or `ScriptFailure` is
        // already `UtxoNotAvailable`, `ValueNotConserved`, `TransactionExpired` or `ScriptFailure`
        // by the time this runs - each of those lowercases into a substring that branch tests for.
        // The four below survive that filter: `badcollateralinputsutxo` and
        // `badreferenceinputsutxo` contain neither `badinputsutxo` nor `bad inputs`, and
        // `feetoosmall` and `exunitstoobig` match no branch at all. The first two are here so a
        // provider naming them reports the same [[SubmitError.rule]] the emulator does for the
        // same condition.
        val patterns = Seq(
          "(BadCollateralInputsUTxO|BadReferenceInputsUTxO|FeeTooSmall|ExUnitsTooBig)".r,
          "\\(([A-Z][a-zA-Z]+)\\)".r
        )
        patterns.view
            .flatMap(_.findFirstMatchIn(message).map(_.group(1)))
            .headOption
    }

    /** Create a SubmitError from a TransactionException.
      *
      * This is used by the Emulator to map internal validation exceptions to SubmitError.
      *
      * Two exceptions are folded into [[NodeSubmitError.UtxoNotAvailable]]: `BadInputsUTxO` and the
      * union case `BadAllInputsUTxO`, which is the one that populates `unavailableInputs`. Those
      * two really are one condition – an input this transaction spends is gone, the race a caller
      * retries on. `BadCollateralInputsUTxO` (a wallet problem) and `BadReferenceInputsUTxO` (a
      * script the transaction points at is not there) are not that condition, so they keep their
      * own rule names through [[NodeSubmitError.ValidationError.errorCode]] like every other
      * unclassified rejection.
      *
      * Both script exceptions do stay one case, [[NodeSubmitError.ScriptFailure]]: "a script this
      * transaction carries did not validate" is a single thing to branch on, `logs`, `scriptHash`
      * and `spentBudget` are declared optional precisely because a native script produces none of
      * them, and [[parseValidationError]] cannot tell native from Plutus in a provider's error body
      * either – splitting here would put the two paths back on different vocabularies.
      *
      * Everything else keeps its rule name through [[NodeSubmitError.ValidationError.errorCode]],
      * so an unclassified rejection is `"FeesOk"` or `"MissingKeyHashes"` rather than an anonymous
      * `"ValidationError"`.
      */
    def fromException(ex: TransactionException): SubmitError = ex match
        case e: TransactionException.BadAllInputsUTxOException =>
            val inputs =
                e.missingInputs ++ e.missingCollateralInputs ++ e.missingReferenceInputs
            UtxoNotAvailable(e.explain, inputs)
        case e: TransactionException.BadInputsUTxOException =>
            UtxoNotAvailable(e.explain)
        case e: TransactionException.OutsideValidityIntervalException =>
            TransactionExpired(e.explain)
        case e: TransactionException.ValueNotConservedUTxOException =>
            ValueNotConserved(e.explain)
        case e: TransactionException.NativeScriptsException =>
            ScriptFailure(e.explain)
        case e: TransactionException.PlutusScriptValidationException =>
            ScriptFailure(e.explain, e.logs, e.scriptHash, e.spentBudget)
        case e =>
            ValidationError(e.explain, Some(TransactionException.ruleName(e)))
}
