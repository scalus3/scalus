package scalus.cardano.ledger
package rules

// It's the deposit check of the Conway GOV rule in cardano-ledger
object ProposalDepositValidator extends STS.Validator {
    override final type Error = TransactionException.ProposalDepositIncorrectException

    /** Rejects a tx with a proposal whose deposit differs from `govActionDeposit`. Conway LEDGER
      * runs GOV only when `isValid` is true.
      */
    override def validate(context: Context, state: State, event: Event): Result = {
        val expected = Coin(context.env.params.govActionDeposit)
        val incorrect =
            if event.isValid then
                event.body.value.proposalProcedures.toSeq.map(_.deposit).find(_ != expected)
            else None
        incorrect match
            case Some(supplied) =>
                failure(
                  TransactionException.ProposalDepositIncorrectException(
                    event.id,
                    supplied = supplied,
                    expected = expected
                  )
                )
            case None => success
    }
}
