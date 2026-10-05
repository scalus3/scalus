package scalus.cardano.onchain

class OnchainError(msg: String) extends RuntimeException(msg) {
    def this() = this("ERROR")
}

class RequirementError(msg: String) extends OnchainError(msg) {
    def this() = this("Requirement error")
}

class ImpossibleLedgerStateError(msg: String) extends OnchainError(msg) {
    def this() = this("impossible ledger state error")
}

/** A specification written with [[scalus.cardano.onchain.plutus.prelude.Spec]] does not hold. It is
  * thrown off-chain only: a compiled script carries no specification.
  *
  * So it is no [[OnchainError]], which stands off-chain for a script that fails. A test that
  * expects the script to reject a transaction does not pass because a specification was violated:
  * the script, without the specification, may accept it. It is an `AssertionError`, as the one
  * `Predef.ensuring` throws: what is wrong is the code or its caller, not the transaction.
  */
class SpecificationError(msg: String) extends AssertionError(msg)
