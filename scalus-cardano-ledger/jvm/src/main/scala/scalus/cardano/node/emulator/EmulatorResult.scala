package scalus.cardano.node.emulator

/** The outcome of an Emulator operation that can fail, usable from Java, spec [SF-5].
  *
  * A null-based view over `Either[E, A]`, as [[scalus.cardano.node.SubmitResult]] is for a
  * submission: Java callers read [[isSuccess]], then [[getValueOrNull]] or [[getErrorOrNull]], with
  * no pattern matching. Scala code should keep using the `Either`-returning members of
  * [[scalus.cardano.node.EmulatorBase]] (or [[toEither]]).
  */
final class EmulatorResult[E, A] private (private val result: Either[E, A]) {

    def isSuccess: Boolean = result.isRight

    /** The value on success, `null` on failure. */
    def getValueOrNull: A = result match
        case Right(value) => value
        case Left(_)      => null.asInstanceOf[A]

    /** The error on failure, `null` on success. */
    def getErrorOrNull: E = result match
        case Left(error) => error
        case Right(_)    => null.asInstanceOf[E]

    /** Scala escape hatch. */
    def toEither: Either[E, A] = result

    override def toString: String = result match
        case Right(value) => s"EmulatorResult.Success($value)"
        case Left(error)  => s"EmulatorResult.Failure($error)"
}

object EmulatorResult {
    def fromEither[E, A](result: Either[E, A]): EmulatorResult[E, A] = new EmulatorResult(result)
}
