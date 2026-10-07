package scalus.cardano.ledger
package rules

// It's the Conway DELEG state transition in cardano-ledger
@deprecated(
  "CertsMutator applies every certificate in tx order; do not use both, spec [SC-22]",
  "1.3.0"
)
object StakeCertificatesMutator extends STS.Mutator {
    override final type Error = TransactionException.StakeCertificatesException

    /** Applies the stake certificates only, after the other kinds of the tx. A phase-2-failed tx
      * applies no certificates, spec [SC-3e].
      */
    override def transit(context: Context, state: State, event: Event): Result = {
        val certificates = event.body.value.certificates.toSeq
        if certificates.isEmpty || !event.isValid then success(state)
        else
            StakeCertificatesValidator.validateAfterWithdrawals(context, state, event) match
                case Left(err) => failure(err)
                case Right(_) =>
                    val defaultDeposit = Coin(context.env.params.stakeAddressDeposit)
                    val updatedDState = certificates.foldLeft(state.certState.dstate)(
                      CertsMutator.deleg(defaultDeposit)
                    )
                    val updatedCertState = state.certState.copy(dstate = updatedDState)
                    success(state.copy(certState = updatedCertState))
    }
}
