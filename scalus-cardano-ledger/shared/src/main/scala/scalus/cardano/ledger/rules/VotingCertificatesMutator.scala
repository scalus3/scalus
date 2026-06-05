package scalus.cardano.ledger
package rules

import scala.annotation.nowarn

// It's the Conway GOVCERT state transition in cardano-ledger (DRep certificates).
// Committee certificates are accepted but not tracked: the emulator does not
// model committee state yet.
object VotingCertificatesMutator extends STS.Mutator {
    override final type Error = TransactionException.DRepException

    // @nowarn: Suppress Long→Double implicit conversion warning. This is intentional for
    // cross-platform compatibility: JS SlotConfig uses Double (JavaScript's number type),
    // while JVM uses Long. The conversion is safe because slot values are well within
    // Double's safe integer range (2^53).
    @nowarn("msg=long2double")
    override def transit(context: Context, state: State, event: Event): Result = {
        val certificates = event.body.value.certificates.toSeq
        if certificates.isEmpty then success(state)
        else {
            val params = context.env.params
            val expectedDeposit = Coin(params.dRepDeposit)
            val currentEpoch = context.slotConfig.epochOf(context.env.slot).toLong
            val expiry = currentEpoch + params.dRepActivity

            def fail(
                alreadyRegistered: Set[Credential] = Set.empty,
                notRegistered: Set[Credential] = Set.empty,
                invalidDeposits: Map[Credential, (Coin, Coin)] = Map.empty,
                invalidRefunds: Map[Credential, (Coin, Coin)] = Map.empty
            ): Left[Error, Nothing] = Left(
              TransactionException.DRepException(
                event.id,
                alreadyRegistered,
                notRegistered,
                invalidDeposits,
                invalidRefunds
              )
            )

            // Certificates are applied sequentially against the evolving DRep state,
            // matching the CERTS rule in cardano-ledger.
            val initial: Either[Error, Map[Credential, DRepState]] =
                Right(state.certState.vstate.dreps)
            val result = certificates.foldLeft(initial) { (acc, cert) =>
                acc.flatMap { dreps =>
                    cert match
                        case Certificate.RegDRepCert(credential, deposit, anchor) =>
                            if dreps.contains(credential) then
                                fail(alreadyRegistered = Set(credential))
                            else if deposit != expectedDeposit then
                                fail(invalidDeposits =
                                    Map(credential -> (expectedDeposit, deposit))
                                )
                            else
                                Right(
                                  dreps.updated(
                                    credential,
                                    DRepState(expiry, anchor, deposit, Set.empty)
                                  )
                                )
                        case Certificate.UnregDRepCert(credential, refund) =>
                            dreps.get(credential) match
                                case None =>
                                    fail(notRegistered = Set(credential))
                                case Some(drepState) if drepState.deposit != refund =>
                                    fail(invalidRefunds =
                                        Map(credential -> (drepState.deposit, refund))
                                    )
                                case Some(_) => Right(dreps - credential)
                        case Certificate.UpdateDRepCert(credential, anchor) =>
                            dreps.get(credential) match
                                case None =>
                                    fail(notRegistered = Set(credential))
                                case Some(drepState) =>
                                    Right(
                                      dreps.updated(
                                        credential,
                                        drepState.copy(expiry = expiry, anchor = anchor)
                                      )
                                    )
                        case _ => Right(dreps)
                }
            }

            result.map { dreps =>
                val updatedCertState =
                    state.certState.copy(vstate = state.certState.vstate.copy(dreps = dreps))
                state.copy(certState = updatedCertState)
            }
        }
    }
}
