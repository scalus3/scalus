package scalus.cardano.node

import scalus.cardano.ledger.TransactionHash

/** Outcome of a transaction submission, usable from Java.
  *
  * A null-based view over `Either[SubmitError, TransactionHash]` so non-Scala callers can inspect
  * results without pattern matching — and tests that expect a rejection can assert on
  * [[getErrorOrNull]] without try/catch. Scala code should keep using the `Either`-returning
  * `submit`/`submitSync` (or [[toEither]]).
  */
final class SubmitResult private (private val result: Either[SubmitError, TransactionHash]) {

    def isSuccess: Boolean = result.isRight

    /** The transaction hash on success, `null` on failure. */
    def getTxHashOrNull: TransactionHash = result match
        case Right(hash) => hash
        case Left(_)     => null.asInstanceOf[TransactionHash]

    /** The submission error on failure, `null` on success. */
    def getErrorOrNull: SubmitError = result.left.toOption.orNull

    /** The submission error message on failure, `null` on success. */
    def getErrorMessageOrNull: String = result.left.toOption.map(_.message).orNull

    /** Scala escape hatch. */
    def toEither: Either[SubmitError, TransactionHash] = result

    override def toString: String = result match
        case Right(hash) => s"SubmitResult.Success(${hash.toHex})"
        case Left(error) => s"SubmitResult.Failure(${error.message})"
}

object SubmitResult {
    def fromEither(result: Either[SubmitError, TransactionHash]): SubmitResult =
        new SubmitResult(result)
}
