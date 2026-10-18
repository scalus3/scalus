package scalus.verify

/** What checking a statement is expected to give (docs/design/verification-overview.md §5.2). */
enum ExpectedOutcome {

    /** The statement holds: the result is a proof. A statement declared with
      * [[Verifier.statement]], or as a contract, a guarantee or an obligation, expects it.
      */
    case Valid

    /** The statement does not hold: the result is a refutation, whose counterexample is replayed in
      * the semantics of the code. A negative control, declared with [[Verifier.refute]], expects
      * it.
      */
    case Falsified
}

/** A statement, the tactic to check it with, and the result the check is expected to give
  * (docs/design/verification-overview.md §4.5). It is data: making it runs nothing. A runner passes
  * its statement and tactic to [[Verifier.verify]], and compares the result with `expected`.
  */
final case class VerificationRequest(
    statement: Statement,
    tactic: Tactic,
    expected: ExpectedOutcome
)

object VerificationRequest {

    /** A request that expects what its statement was declared to be: [[ExpectedOutcome.Valid]] for
      * a statement, [[ExpectedOutcome.Falsified]] for a negative control.
      */
    def apply(statement: Statement, tactic: Tactic): VerificationRequest =
        VerificationRequest(statement, tactic, statement.expected)
}
