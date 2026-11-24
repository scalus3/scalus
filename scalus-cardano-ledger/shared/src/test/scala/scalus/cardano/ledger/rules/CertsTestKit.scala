package scalus.cardano.ledger.rules

import scalus.cardano.address.Network
import scalus.cardano.ledger.*

import scala.collection.immutable.SortedMap

/** Fixed values for the certificate suites: a context, a transaction with certificates, a pool
  * registration and an account state. No generators: spec `[kit.certs.no-generators]` in
  * `docs/internal/test-refactor/d6-rules-kits.md`.
  */
trait CertsTestKit extends RuleTestValues {

    /** The parameters of every context. A suite may override them, spec
      * `[kit.context.overridable]`.
      */
    def params: ProtocolParams = CardanoInfo.preprod.protocolParams

    /** The slot configuration of every context. A suite may override it. */
    def slotConfig: SlotConfig = CardanoInfo.preprod.slotConfig

    def keyDeposit: Coin = Coin(params.stakeAddressDeposit)

    /** A context on `Network.Testnet`, spec `[kit.context.one-builder]`. With its defaults it is
      * the context that the suites copied before: slot 0, the parameters as they are, no treasury.
      * Another `protocolMajor` sets the version to `(protocolMajor, 0)`; the preprod parameters are
      * version 11.0, so `protocolMajor = 11` keeps them as they are.
      */
    def context(
        certState: CertState = CertState.empty,
        slot: SlotNo = 0,
        protocolMajor: Int = params.protocolVersion.major,
        treasury: Coin = Coin.zero
    ): Context = {
        val ps =
            if protocolMajor == params.protocolVersion.major then params
            else params.copy(protocolVersion = ProtocolVersion(protocolMajor, 0))
        new Context(
          env = UtxoEnv(
            slot = slot,
            params = ps,
            certState = certState,
            network = Network.Testnet,
            treasury = treasury
          ),
          slotConfig = slotConfig
        )
    }

    /** The first slot of `epoch`. On preprod, epoch 0 is not slot 0. */
    def epochStart(epoch: Long): SlotNo = slotConfig.firstSlotOfEpoch(epoch)

    /** A transaction with no inputs, no outputs, no withdrawals and fee 0, spec
      * `[kit.certs.cert-tx]`.
      */
    def certTx(certs: Certificate*): Transaction = txWith(None, certs)

    /** As `certTx(certs*)`, with withdrawals. An empty map still sets `Some(Withdrawals(...))`. */
    def certTx(withdrawals: Map[RewardAccount, Coin], certs: Certificate*): Transaction =
        txWith(Some(Withdrawals(SortedMap.from(withdrawals))), certs)

    /** A transaction with proposals, votes and certificates, as `certTx`. */
    def govTx(
        proposals: Seq[ProposalProcedure] = Nil,
        votes: Option[VotingProcedures] = None,
        certs: Seq[Certificate] = Nil
    ): Transaction =
        txWith(None, certs).withBody(
          _.copy(
            proposalProcedures =
                if proposals.isEmpty then TaggedOrderedSet.empty
                else TaggedOrderedSet.from(proposals),
            votingProcedures = votes
          )
        )

    private def txWith(withdrawals: Option[Withdrawals], certs: Seq[Certificate]): Transaction =
        Transaction(
          emptyBody.copy(
            certificates =
                if certs.isEmpty then TaggedOrderedStrictSet.empty
                else TaggedOrderedStrictSet.from(certs),
            withdrawals = withdrawals
          )
        )

    /** A pool registration with margin 1, no relays and no metadata, spec
      * `[kit.certs.pool-registration]`.
      */
    def poolRegistration(
        operator: AddrKeyHash,
        rewardAccount: RewardAccount,
        cost: Coin = Coin(params.minPoolCost),
        pledge: Coin = Coin.ada(1),
        owners: Set[AddrKeyHash] = Set.empty,
        vrfKeyHash: VrfKeyHash = VrfKeyHash.fromHex("a" * 64)
    ): Certificate.PoolRegistration =
        Certificate.PoolRegistration(
          operator = operator,
          vrfKeyHash = vrfKeyHash,
          pledge = pledge,
          cost = cost,
          margin = UnitInterval.one,
          rewardAccount = rewardAccount,
          poolOwners = owners,
          relays = IndexedSeq.empty,
          poolMetadata = None
        )

    /** A registered account with the key deposit of `params`, spec `[kit.certs.account-state]`. */
    def accountState(
        balance: Coin,
        pool: Option[PoolKeyHash] = None,
        drep: Option[DRep] = None
    ): ConwayAccountState = ConwayAccountState(balance, keyDeposit, pool, drep)

    /** A certificate state from its three maps, spec `[kit.certs.state]`. */
    def certState(
        accounts: Map[Credential, ConwayAccountState] = Map.empty,
        pools: Map[PoolKeyHash, Certificate.PoolRegistration] = Map.empty,
        dreps: Map[Credential, DRepState] = Map.empty
    ): CertState = CertState(
      vstate = VotingState.empty.copy(dreps = dreps),
      pstate = PoolsState.empty.copy(stakePools = pools),
      dstate = DelegationState(accounts)
    )
}
