package scalus.cardano.ledger
package rules

// It's the Conway GOVCERT state transition in cardano-ledger (DRep certificates).
// Committee certificates are accepted but not tracked: the emulator does not
// model committee state yet.
@deprecated(
  "CertsMutator applies every certificate in tx order; do not use both, spec [SC-22]",
  "1.3.0"
)
object VotingCertificatesMutator extends STS.Mutator {
    override final type Error = TransactionException.DRepException

    /** Applies the DRep certificates only, in tx order. A phase-2-failed tx applies no
      * certificates, spec [SC-3e].
      */
    override def transit(context: Context, state: State, event: Event): Result = {
        val certificates = event.body.value.certificates.toSeq
        if certificates.isEmpty || !event.isValid then success(state)
        else
            val initial
                : Either[Error, (Map[Credential, DRepState], Map[Credential, ConwayAccountState])] =
                Right((state.certState.vstate.dreps, state.certState.dstate.accounts))
            certificates
                .foldLeft(initial) { (acc, cert) =>
                    acc.flatMap(CertsMutator.govCert(context, event.id)(_, cert))
                }
                .map { (dreps, accounts) =>
                    val certState = state.certState
                    state.copy(certState =
                        certState.copy(
                          dstate = certState.dstate.copy(accounts = accounts),
                          vstate = certState.vstate.copy(dreps = dreps)
                        )
                    )
                }
    }
}
