package scalus.cardano.node

import scalus.cardano.address.{Network, ShelleyAddress, StakeAddress}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.epoch.EnactState
import scalus.cardano.ledger.epoch.GovFixtures.{zeroDReps, zeroPools}
import scalus.cardano.ledger.epoch.internal.Ratify
import scalus.cardano.node.emulator.ChainConfig
import scalus.cardano.txbuilder.TransactionBuilderStep.{IssueCertificate, SubmitProposal, SubmitVotingProcedure}
import scalus.cardano.txbuilder.{PubKeyWitness, TransactionSigner, TxBuilder}
import scalus.testing.kit.Party
import scalus.testing.kit.Party.*
import scalus.uplc.builtin.ByteString

/** An Emulator set up as the Conway Imp tests set up their ledger, for the ports of `RatifySpec`,
  * `EnactSpec` and `EpochSpec` (eras/conway/impl/testlib/Test/Cardano/Ledger/Conway/Imp/) of
  * ledger-rules spec section 16, phase 5.
  *
  * The genesis is that of `initGenesis` (Conway ImpTest.hs:259-306): the voting thresholds,
  * `committeeMinSize` 1, `committeeMaxTermLength` 20, `govActionLifetime` 30, `govActionDeposit`
  * 123, `dRepDeposit` 70 ADA, `dRepActivity` 100, and a committee of two members, threshold 1/1,
  * expiring at `startEpoch + 15`. `adjust` changes the pparams after that, as an Imp test's
  * `modifyPParams` before its first proposal does.
  *
  * The DReps and pools of `setupSingleDRep`, `setupDRepWithoutStake` and `setupPoolWithStake`
  * (ImpTest.hs:480-571) are seeded at genesis, and each DRep delegator delegates its vote by a tx
  * in the first epoch. A stake of 0 seeds no UTxO.
  *
  * Parties: Alice pays every fee from enterprise UTxOs and is the return account of the proposals;
  * Charles and Dave are the cold keys of the genesis committee and Eve and Faith their hot keys;
  * DRep `i` is Bob or Hal, delegated to by Grace or Ivan; pool `i` is run by Judy, Laura or Nick
  * and delegated to by Kevin, Mallory or Oracle; Peggy, Sybil, Trent, Victor and Wendy are
  * [[fresh]] keys.
  *
  * @param hotKeys
  *   whether the genesis committee members have hot keys, as after `registerInitialCommittee`
  * @param accounts
  *   the parties whose stake accounts are registered at genesis, as `registerAccountAddress` does
  */
final class GovImp(
    adjust: ProtocolParams => ProtocolParams = identity,
    dRepStakes: Seq[Long] = Seq.empty,
    poolStakes: Seq[Long] = Seq.empty,
    hotKeys: Boolean = false,
    treasury: Coin = Coin.zero,
    accounts: Seq[Party] = Seq.empty
) {
    import GovImp.*

    val env: CardanoInfo = CardanoInfo.mainnet
    private val slotConfig = env.slotConfig

    val params: ProtocolParams = adjust(impParams(env.protocolParams))

    val e: Emulator = {
        val dRepRegs = dRepStakes.indices.map { i =>
            EmulatorDRepRegistration(dRepCredential(i), Coin(params.dRepDeposit))
        }
        val poolRegs = poolStakes.indices.map(i => EmulatorPoolRegistration(poolParams(i)))
        val stakeRegs =
            EmulatorStakeRegistration(stakeCredential(Alice)) +:
                (dRepStakes.indices
                    .map(i => EmulatorStakeRegistration(stakeCredential(dRepDelegators(i)))) ++
                    poolStakes.indices.flatMap { i =>
                        Seq(
                          EmulatorStakeRegistration(stakeCredential(poolOperators(i))),
                          EmulatorStakeRegistration(
                            stakeCredential(poolDelegators(i)),
                            delegatedTo = Some(poolId(i))
                          )
                        )
                    } ++ accounts.map(p => EmulatorStakeRegistration(stakeCredential(p))))
        val stakeUtxos = (dRepStakes.zip(dRepDelegators) ++ poolStakes.zip(poolDelegators))
            .filter(_._1 > 0)
            .map((stake, p) => Output(baseAddress(p), Value.lovelace(stake)))
        val outputs = Seq.fill(10)(Output(alice, Value.ada(100_000))) ++ stakeUtxos
        val utxos: Utxos = outputs.zipWithIndex.map((out, i) => Input(genesisHash, i) -> out).toMap
        val committee = Committee(
          Map(keyOf(Charles) -> (startEpoch + 15), keyOf(Dave) -> (startEpoch + 15)),
          UnitInterval.one
        )
        val hot =
            if hotKeys then Map(keyOf(Charles) -> keyOf(Eve), keyOf(Dave) -> keyOf(Faith))
            else Map.empty[Credential, Credential]
        val context = rules.Context.testMainnet(slotConfig.firstSlotOfEpoch(startEpoch) + 10)
        val emulator = Emulator.withState(
          EmulatorInitialState.empty
              .withUtxos(utxos)
              .withStakeRegistrations(stakeRegs)
              .withPoolRegistrations(poolRegs)
              .withDRepRegistrations(dRepRegs)
              .withCommittee(committee)
              .withCommitteeHotKeys(hot),
          context.copy(env = context.env.copy(params = params, treasury = treasury)),
          ChainConfig.instant
        )
        emulator
    }

    // the vote delegations of setupSingleDRep and setupDRepWithoutStake
    dRepStakes.indices.foreach { i =>
        val delegator = dRepDelegators(i)
        submit(
          TxBuilder(env).delegateVoteToDRep(stakeAddress(delegator), dRep(i)),
          delegator
        )
    }

    /** `passEpoch`: moves to the first slot of the next epoch, which runs its boundary. */
    def passEpoch(): Unit = e.setSlot(slotConfig.firstSlotOfEpoch(e.currentEpoch + 1))

    def passNEpochs(n: Int): Unit = (1 to n).foreach(_ => passEpoch())

    /** The epoch `startEpoch + n`, the Imp epoch `n` of a test. */
    def epoch(n: Long): EpochNo = startEpoch + n

    /** Submits the tx of `builder`, paid by Alice and signed by the payment and stake keys of Alice
      * and `signers`.
      */
    def submit(builder: TxBuilder, signers: Party*): Transaction = {
        val keys =
            (Alice +: signers).flatMap(p => Seq(p.account.paymentKeyPair, p.account.stakeKeyPair))
        val aliceUtxos = e.utxos.filter((_, out) => out.address == alice)
        val tx =
            builder.complete(aliceUtxos, alice).sign(new TransactionSigner(keys.toSet)).transaction
        val result = e.submitSync(tx)
        assert(result == Right(tx.id), s"submit failed: $result")
        tx
    }

    /** Submits `action` with the deposit of the pparams and the return account of `returnTo`;
      * `mkProposal` and `submitProposal` (ImpTest.hs:680-760).
      */
    def propose(action: GovAction, returnTo: Party = Alice): GovActionId = {
        val proposal = ProposalProcedure(
          Coin(e.currentParams.govActionDeposit),
          RewardAccount(stakeAddress(returnTo)),
          action,
          anchor
        )
        GovActionId(submit(TxBuilder(env).addSteps(SubmitProposal(proposal))).id, 0)
    }

    /** `mkUpdateCommitteeProposal` (ImpTest.hs:1718-1736): each term is the current epoch plus the
      * interval; the parent is the committee root when `parent` is `None`.
      */
    def updateCommittee(
        parent: Option[Option[GovActionId]],
        remove: Set[Party],
        add: Seq[(Party, Long)],
        threshold: UnitInterval
    ): GovActionId =
        propose(
          GovAction.UpdateCommittee(
            parent.getOrElse(e.govState.roots.committee),
            remove.map(keyOf),
            add.map((p, interval) => keyOf(p) -> (e.currentEpoch + interval)).toMap,
            threshold
          )
        )

    /** `submitParameterChange` (ImpTest.hs:870-880). */
    def parameterChange(parent: Option[GovActionId], update: ProtocolParamUpdate): GovActionId =
        propose(GovAction.ParameterChange(parent, update, None))

    /** `submitConstitution` (ImpTest.hs:1587-1600), with an anchor that names its parent. */
    def constitution(parent: Option[GovActionId]): (GovActionId, Constitution) = {
        val tag = parent.fold("root")(_.transactionId.toHex.take(8))
        val constitution = Constitution(Anchor(s"https://example.com/$tag", anchor.dataHash), None)
        (propose(GovAction.NewConstitution(parent, constitution)), constitution)
    }

    def vote(voter: Voter, id: GovActionId, signer: Party, vote: Vote): Unit =
        submit(
          TxBuilder(env).addSteps(
            SubmitVotingProcedure(voter, Map(id -> VotingProcedure(vote, None)))
          ),
          signer
        )

    def yesDRep(i: Int, id: GovActionId): Unit =
        vote(Voter.DRepKey(dRepParties(i).addrKeyHash), id, dRepParties(i), Vote.Yes)

    def yesPool(i: Int, id: GovActionId): Unit =
        vote(Voter.StakingPoolKey(poolOperators(i).addrKeyHash), id, poolOperators(i), Vote.Yes)

    /** A Yes of a committee member by its hot key `hot`. */
    def yesCC(hot: Party, id: GovActionId): Unit =
        vote(Voter.ConstitutionalCommitteeHotKey(hot.addrKeyHash), id, hot, Vote.Yes)

    /** `submitYesVoteCCs_` with the hot keys of the genesis committee. */
    def yesCCs(id: GovActionId): Unit = Seq(Eve, Faith).foreach(yesCC(_, id))

    /** `registerCommitteeHotKey`: `cold` authorizes `hot`. */
    def authorizeHotKey(cold: Party, hot: Party): Unit =
        submit(
          TxBuilder(env)
              .addSteps(
                IssueCertificate(
                  Certificate.AuthCommitteeHotCert(keyOf(cold), keyOf(hot)),
                  PubKeyWitness
                )
              )
              // the builder collects no cold-key witness of a committee cert
              .requireSignature(cold.addrKeyHash),
          cold
        )

    /** `resignCommitteeColdKey`. */
    def resign(cold: Party): Unit =
        submit(
          TxBuilder(env)
              .addSteps(
                IssueCertificate(
                  Certificate.ResignCommitteeColdCert(keyOf(cold), None),
                  PubKeyWitness
                )
              )
              .requireSignature(cold.addrKeyHash),
          cold
        )

    def committeeMembers: Set[Credential] =
        e.govState.committee.fold(Set.empty[Credential])(_.members.keySet)

    def authorization(cold: Party): Option[CommitteeAuthorization] =
        e.certState.vstate.committeeState.get(keyOf(cold))

    def balance(p: Party): Coin = e.certState.dstate.accounts(stakeCredential(p)).balance

    /** `isCommitteeAccepted` (ImpTest.hs:1211-1218): the committee check of RATIFY on the state
      * now, with Imp's `reCurrentEpoch = current epoch − 1` (`getRatifyEnv`, :1029-1049). The SPO
      * and DRep thresholds are set to 0, so only the committee decides.
      */
    def isCommitteeAccepted(id: GovActionId): Boolean =
        accepted(id) { es =>
            es.copy(params =
                es.params.copy(
                  poolVotingThresholds = zeroPools,
                  dRepVotingThresholds = zeroDReps
                )
            )
        }

    /** `isDRepAccepted` (ImpTest.hs:1193-1200), as [[isCommitteeAccepted]] with the committee and
      * SPO thresholds set to 0 and `committeeMinSize` 0.
      */
    def isDRepAccepted(id: GovActionId): Boolean =
        accepted(id) { es =>
            es.copy(
              committee = es.committee.map(_.copy(threshold = UnitInterval.zero)),
              params = es.params.copy(poolVotingThresholds = zeroPools, committeeMinSize = 0)
            )
        }

    private def accepted(id: GovActionId)(only: EnactState => EnactState): Boolean = {
        val state = EmulatorBase.stateOf(e)
        val ledger = state.ledger
        val ratifyEnv = Ratify.envOf(ledger, state.snapshots.mark, e.currentEpoch - 1)
        val enactState = only(Ratify.enactStateOf(ledger.govState, e.currentParams))
        val proposal = e.govState.proposal(id).getOrElse(sys.error(s"no proposal $id"))
        Ratify.ratify(ratifyEnv, enactState, e.treasury, Vector(proposal)).enacted == Vector(id)
    }

    def poolId(i: Int): PoolKeyHash = PoolKeyHash.fromByteString(poolOperators(i).addrKeyHash)

    def dRep(i: Int): DRep = DRep.KeyHash(dRepParties(i).addrKeyHash)

    /** The parameters of pool `i`: pledge 0, so its members need not delegate to it. */
    private def poolParams(i: Int): Certificate.PoolRegistration = {
        val operator = poolOperators(i)
        Certificate.PoolRegistration(
          operator = operator.addrKeyHash,
          vrfKeyHash = VrfKeyHash.fromHex(f"${i + 1}%02x" * 32),
          pledge = Coin.zero,
          cost = Coin(params.minPoolCost),
          margin = UnitInterval.zero,
          rewardAccount = RewardAccount(stakeAddress(operator)),
          poolOwners = Set(operator.addrKeyHash),
          relays = IndexedSeq.empty,
          poolMetadata = None
        )
    }

    private def dRepCredential(i: Int): Credential = Credential.KeyHash(dRepParties(i).addrKeyHash)
}

object GovImp {
    val startEpoch: EpochNo = 500L

    val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    val alice: ShelleyAddress = Alice.address(Network.Mainnet)

    val anchor: Anchor = Anchor("https://example.com", DataHash.fromHex("0" * 64))

    val dRepParties: Seq[Party] = Seq(Bob, Hal)
    val dRepDelegators: Seq[Party] = Seq(Grace, Ivan)
    val poolOperators: Seq[Party] = Seq(Judy, Laura, Nick)
    val poolDelegators: Seq[Party] = Seq(Kevin, Mallory, Oracle)

    /** Keys no fixture role uses, for `freshKeyHash`. */
    val fresh: Seq[Party] = Seq(Peggy, Sybil, Trent, Victor, Wendy)

    def stakeAddress(p: Party): StakeAddress = p.account.stakeAddress(Network.Mainnet)

    def stakeCredential(p: Party): Credential = stakeAddress(p).credential

    /** The key-hash credential of the payment key of `p`, for a committee cold or hot key. */
    def keyOf(p: Party): Credential = Credential.KeyHash(p.addrKeyHash)

    /** The address with the payment key and the stake key of `p`, so its coin counts as `p`'s
      * stake.
      */
    def baseAddress(p: Party): ShelleyAddress = p.account.baseAddress(Network.Mainnet)

    private def ratio(n: Long, d: Long): UnitInterval = UnitInterval(n, d)

    /** The pparams of `initGenesis` (Conway ImpTest.hs:270-302) on `base`. */
    def impParams(base: ProtocolParams): ProtocolParams = base.copy(
      poolVotingThresholds = PoolVotingThresholds(
        motionNoConfidence = ratio(51, 100),
        committeeNormal = ratio(65, 100),
        committeeNoConfidence = ratio(65, 100),
        hardForkInitiation = ratio(51, 100),
        ppSecurityGroup = ratio(51, 100)
      ),
      dRepVotingThresholds = DRepVotingThresholds(
        motionNoConfidence = ratio(51, 100),
        committeeNormal = ratio(65, 100),
        committeeNoConfidence = ratio(65, 100),
        updateToConstitution = ratio(65, 100),
        hardForkInitiation = ratio(51, 100),
        ppNetworkGroup = ratio(51, 100),
        ppEconomicGroup = ratio(51, 100),
        ppTechnicalGroup = ratio(51, 100),
        ppGovGroup = ratio(75, 100),
        treasuryWithdrawal = ratio(51, 100)
      ),
      committeeMinSize = 1,
      committeeMaxTermLength = 20,
      govActionLifetime = 30,
      govActionDeposit = 123,
      dRepDeposit = 70_000_000,
      dRepActivity = 100
    )

    /** `disableTreasuryExpansion` (Shelley ImpTest.hs:2090-2091): `tau = 0`. */
    def noTreasuryExpansion(p: ProtocolParams): ProtocolParams =
        p.copy(treasuryCut = UnitInterval.zero)

    extension (e: Emulator) {
        def currentParams: ProtocolParams = EmulatorBase.stateOf(e).context.env.params
    }
}
