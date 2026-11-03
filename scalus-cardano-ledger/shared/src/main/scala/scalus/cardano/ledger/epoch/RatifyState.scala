package scalus.cardano.ledger.epoch

import scalus.cardano.ledger.{Coin, Committee, Constitution, Credential, GovActionId, GovRoots, ProtocolParams}

/** What the enactment of the ratified actions gives, the ledger's `EnactState`
  * (eras/conway/impl/src/Cardano/Ledger/Conway/Governance/Internal.hs:131-141) without the treasury
  * and the previous pparams, ledger-rules spec [DM-12a], D86. The treasury is a variable of one
  * RATIFY run only (RATIFY sets it to 0 when the run ends, conway Rules/Ratify.hs:360), and the
  * ledger never reads `ensPrevPParams`.
  *
  * @param committee
  *   the committee, or `None` after a NoConfidence
  * @param params
  *   the current pparams with the enacted ParameterChange and HardForkInitiation actions applied
  * @param withdrawals
  *   the enacted treasury withdrawals, summed per credential, spec [EN-6]
  * @param roots
  *   the last enacted action of each purpose
  */
case class EnactState(
    committee: Option[Committee],
    constitution: Constitution,
    params: ProtocolParams,
    withdrawals: Map[Credential, Coin],
    roots: GovRoots
)

/** The result of one RATIFY run, the ledger's `RatifyState`
  * (eras/conway/impl/src/Cardano/Ledger/Conway/Governance/Internal.hs:257-275), ledger-rules spec
  * [DM-12a].
  *
  * @param enactState
  *   the enact state after the run
  * @param enacted
  *   the ids of the enacted actions, in the order RATIFY enacted them
  * @param expired
  *   the ids of the actions that expired unenacted, spec [RT-10]
  * @param delayed
  *   whether an enacted delaying action stopped the run from enacting more, spec [RT-5c]
  */
case class RatifyState(
    enactState: EnactState,
    enacted: Vector[GovActionId],
    expired: Set[GovActionId],
    delayed: Boolean
)
