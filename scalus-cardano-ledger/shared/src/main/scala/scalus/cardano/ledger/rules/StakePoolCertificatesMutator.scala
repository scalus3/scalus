package scalus.cardano.ledger
package rules

// It's the Shelley/Conway POOL state transition in cardano-ledger
@deprecated(
  "CertsMutator applies every certificate in tx order; do not use both, spec [SC-22]",
  "1.3.0"
)
object StakePoolCertificatesMutator extends STS.Mutator {
    override final type Error = TransactionException.StakePoolException

    /** Applies the pool certificates only. A phase-2-failed tx applies no certificates, spec
      * [SC-3e].
      */
    override def transit(context: Context, state: State, event: Event): Result = {
        val certificates = event.body.value.certificates.toSeq
        if certificates.isEmpty || !event.isValid then success(state)
        else
            StakePoolCertificatesValidator.validate(context, state, event) match
                case Left(err) => failure(err)
                case Right(_) =>
                    val poolDeposit = Coin(context.env.params.stakePoolDeposit)
                    val updatedPState =
                        certificates.foldLeft(state.certState.pstate)(
                          CertsMutator.pool(poolDeposit)
                        )
                    val updatedCertState = state.certState.copy(pstate = updatedPState)
                    success(state.copy(certState = updatedCertState))
    }
}
