package scalus.cardano.ledger.rules

import org.scalatest.{EitherValues, OptionValues}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionException.GovException

import scala.collection.immutable.SortedMap

/** The Conway GOV rule, spec [GV-1] to [GV-20], [GV-2a], [GV-3a], [GV-3b], [GV-7a], [GV-10a],
  * [GV-10b], [GV-11a], [GV-17a] (eras/conway/impl/src/Cardano/Ledger/Conway/Rules/Gov.hs:456-643).
  *
  * The tx runs at epoch 10 with `govActionLifetime = 6`, as in the spec section 15 lifecycle
  * example. The pre-tx state has a registered return account, DRep A and a script DRep, one pool, a
  * committee of two members, one with a hot key and one with a hot script, and one InfoAction that
  * expires after epoch 14.
  */
class GovMutatorTest extends AnyFunSuite with Matchers with EitherValues with OptionValues {

    private val slotConfig =
        SlotConfig(zeroTime = 0, zeroSlot = 0, slotLength = 1000, epochLength = 100, zeroEpoch = 0)
    private val baseParams = CardanoInfo.preprod.protocolParams.copy(govActionLifetime = 6)
    private val deposit = Coin(baseParams.govActionDeposit)

    private def rewardAccount(hex: String, network: Network = Network.Testnet): RewardAccount =
        RewardAccount(StakeAddress(network, StakePayload.Stake(StakeKeyHash.fromHex(hex * 56))))

    private val returnAccount = rewardAccount("e")
    private val unregisteredAccount = rewardAccount("7")
    private val mainnetReturnAccount = rewardAccount("e", Network.Mainnet)

    private val drepA = Credential.KeyHash(AddrKeyHash.fromHex("a" * 56))
    private val drepB = Credential.KeyHash(AddrKeyHash.fromHex("b" * 56))
    private val drepVoterA = Voter.DRepKey(AddrKeyHash.fromHex("a" * 56))
    private val drepVoterB = Voter.DRepKey(AddrKeyHash.fromHex("b" * 56))
    private val unknownDRepVoter = Voter.DRepKey(AddrKeyHash.fromHex("3" * 56))
    private val scriptDRepHash = ScriptHash.fromHex("d" * 56)
    private val scriptDRep = Credential.ScriptHash(scriptDRepHash)
    private val scriptDRepVoter = Voter.DRepScript(scriptDRepHash)
    private val drepState = DRepState(100, None, Coin(baseParams.dRepDeposit), Set.empty)

    private val operator = AddrKeyHash.fromHex("1" * 56)
    private val poolId = PoolKeyHash.fromByteString(operator)
    private val poolVoter = Voter.StakingPoolKey(operator)

    private val cold = Credential.KeyHash(AddrKeyHash.fromHex("c0" * 28))
    private val hotHash = AddrKeyHash.fromHex("c1" * 28)
    private val hot = Credential.KeyHash(hotHash)
    private val ccVoter = Voter.ConstitutionalCommitteeHotKey(hotHash)
    // the second member has script credentials, cold and hot
    private val coldScript = Credential.ScriptHash(ScriptHash.fromHex("c5" * 28))
    private val hotScriptHash = ScriptHash.fromHex("c6" * 28)
    private val hotScript = Credential.ScriptHash(hotScriptHash)
    private val ccScriptVoter = Voter.ConstitutionalCommitteeHotScript(hotScriptHash)
    private val committee = Committee(Map(cold -> 100L, coldScript -> 100L), UnitInterval(1, 2))
    private val newMember = Credential.KeyHash(AddrKeyHash.fromHex("c2" * 28))

    private val guardrails = ScriptHash.fromHex("9" * 56)
    private val otherScript = ScriptHash.fromHex("8" * 56)
    private val anchor = Anchor("https://example.com", DataHash.fromHex("0" * 64))
    private val constitution = Constitution(anchor, Some(guardrails))

    private def actionId(n: Int): GovActionId =
        GovActionId(TransactionHash.fromHex(f"$n%064x"), 0)

    private def pending(
        id: GovActionId,
        action: GovAction,
        expiresAfter: Long = 14
    ): GovActionState =
        GovActionState(
          id,
          Map.empty,
          Map.empty,
          Map.empty,
          ProposalProcedure(deposit, returnAccount, action, anchor),
          expiresAfter - 6,
          expiresAfter
        )

    private val infoId = actionId(1)

    private val poolRegistration: Certificate.PoolRegistration = Certificate.PoolRegistration(
      operator = operator,
      vrfKeyHash = VrfKeyHash.fromHex("a1" * 32),
      pledge = Coin.ada(1),
      cost = Coin(baseParams.minPoolCost),
      margin = UnitInterval.one,
      rewardAccount = returnAccount,
      poolOwners = Set.empty,
      relays = IndexedSeq.empty,
      poolMetadata = None
    )

    private val baseState: State = State(
      certState = CertState(
        vstate = VotingState(
          Map(drepA -> drepState, scriptDRep -> drepState),
          0L,
          Map(
            cold -> CommitteeAuthorization.HotCredential(hot),
            coldScript -> CommitteeAuthorization.HotCredential(hotScript)
          )
        ),
        pstate = PoolsState(
          Map[PoolKeyHash, Certificate.PoolRegistration](poolId -> poolRegistration),
          Map.empty,
          Map.empty,
          Map[PoolKeyHash, Coin](poolId -> Coin(baseParams.stakePoolDeposit)),
          Map.empty
        ),
        dstate = DelegationState(
          Map(
            returnAccount.address.credential -> ConwayAccountState(
              Coin.zero,
              Coin(baseParams.stakeAddressDeposit),
              None,
              None
            )
          )
        )
      ),
      govState = GovState(
        Vector(pending(infoId, GovAction.InfoAction)),
        GovRoots.empty,
        Some(committee),
        constitution
      )
    )

    private def withGov(f: GovState => GovState): State =
        baseState.copy(govState = f(baseState.govState))

    private def proposal(
        action: GovAction,
        account: RewardAccount = returnAccount
    ): ProposalProcedure = ProposalProcedure(deposit, account, action, anchor)

    private def tx(
        proposals: Seq[ProposalProcedure] = Seq.empty,
        votes: Seq[(Voter, GovActionId, Vote)] = Seq.empty,
        certs: Seq[Certificate] = Seq.empty
    ): Transaction = {
        val votingProcedures =
            if votes.isEmpty then None
            else
                Some(
                  VotingProcedures(
                    SortedMap.from(votes.groupBy(_._1).map { (voter, vs) =>
                        voter -> SortedMap.from(vs.map(v => v._2 -> VotingProcedure(v._3, None)))
                    })
                  )
                )
        Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            certificates =
                if certs.isEmpty then TaggedOrderedStrictSet.empty
                else TaggedOrderedStrictSet.from(certs),
            votingProcedures = votingProcedures,
            proposalProcedures =
                if proposals.isEmpty then TaggedOrderedSet.empty
                else TaggedOrderedSet.from(proposals)
          )
        )
    }

    private def context(state: State, major: Int, epoch: Long): Context = new Context(
      env = UtxoEnv(
        slot = slotConfig.firstSlotOfEpoch(epoch),
        params = baseParams.copy(protocolVersion = ProtocolVersion(major, 0)),
        certState = state.certState,
        network = Network.Testnet,
        treasury = Coin.zero
      ),
      slotConfig = slotConfig
    )

    private def run(
        transaction: Transaction,
        state: State = baseState,
        major: Int = 11,
        epoch: Long = 10
    ): Either[TransactionException, State] =
        GovMutator.transit(context(state, major, epoch), state, transaction)

    /** A tx id for the tests of a vote or a parent that names a proposal of the same tx. A real tx
      * cannot name its own id, which is the hash of its body, so these tests give GOV the id.
      */
    private val fakeTxId = TransactionHash.fromHex("ab" * 32)

    private def runAs(
        transaction: Transaction,
        state: State = baseState
    ): Either[TransactionException, State] =
        GovMutator.transit(context(state, 11, 10), state, transaction, fakeTxId)

    /** CERTS, then GOV, as in the LEDGER rule, spec [GV-2a]. */
    private def runWithCerts(
        transaction: Transaction,
        state: State = baseState
    ): Either[TransactionException, State] =
        STS.Mutator.transit[TransactionException](
          Seq(CertsMutator, GovMutator),
          context(state, 11, 10),
          state,
          transaction
        )

    private def failureOf(result: Either[TransactionException, State]): GovFailure =
        result.left.value match
            case e: GovException => e.failure
            case other           => fail(s"not a GovException: $other")

    private def ppChange(
        update: ProtocolParamUpdate,
        prev: Option[GovActionId] = None,
        policy: Option[ScriptHash] = Some(guardrails)
    ): GovAction = GovAction.ParameterChange(prev, update, policy)

    private val validUpdate = ProtocolParamUpdate(maxTxSize = Some(16384))

    // ---------------------------------------------------------------- proposals

    test("spec [GV-4], [GV-5]: proposals get the ids (txId, index) and the epochs of the tx") {
        val transaction =
            tx(proposals = Seq(proposal(GovAction.InfoAction), proposal(ppChange(validUpdate))))
        val stored = run(transaction).value.govState.proposals
        stored.map(_.id) shouldBe Vector(
          infoId,
          GovActionId(transaction.id, 0),
          GovActionId(transaction.id, 1)
        )
        stored(1) shouldBe GovActionState(
          GovActionId(transaction.id, 0),
          Map.empty,
          Map.empty,
          Map.empty,
          proposal(GovAction.InfoAction),
          10,
          16
        )
        stored(2).action shouldBe ppChange(validUpdate)
    }

    test("spec [GV-6]: a parent is the purpose root or a pending proposal of the same purpose") {
        val rootId = actionId(2)
        val pendingPp = actionId(3)
        val state = withGov(g =>
            g.copy(
              proposals = g.proposals :+ pending(pendingPp, ppChange(validUpdate)),
              roots = GovRoots(Some(rootId), None, None, None)
            )
        )
        run(
          tx(proposals = Seq(proposal(ppChange(validUpdate, Some(rootId))))),
          state
        ).isRight shouldBe true
        run(
          tx(proposals = Seq(proposal(ppChange(validUpdate, Some(pendingPp))))),
          state
        ).isRight shouldBe true
        // a proposal of the same tx can be a parent
        val chained = tx(proposals =
            Seq(
              proposal(ppChange(validUpdate, Some(rootId))),
              proposal(ppChange(validUpdate, Some(GovActionId(fakeTxId, 0))))
            )
        )
        runAs(chained, state).isRight shouldBe true
        // no parent while the root is set
        val orphan = proposal(ppChange(validUpdate, None))
        failureOf(run(tx(proposals = Seq(orphan)), state)) shouldBe
            GovFailure.InvalidPrevGovActionId(orphan)
        // a parent of another purpose
        val wrongPurpose = proposal(GovAction.NoConfidence(Some(pendingPp)))
        failureOf(run(tx(proposals = Seq(wrongPurpose)), state)) shouldBe
            GovFailure.InvalidPrevGovActionId(wrongPurpose)
        // an unknown parent
        val unknown = proposal(ppChange(validUpdate, Some(actionId(99))))
        failureOf(run(tx(proposals = Seq(unknown)), state)) shouldBe
            GovFailure.InvalidPrevGovActionId(unknown)
    }

    test("spec [GV-7], [GV-7a]: a zero field or an empty update is a MalformedProposal") {
        val zeros = Seq(
          ProtocolParamUpdate(maxBlockBodySize = Some(0)),
          ProtocolParamUpdate(maxTxSize = Some(0)),
          ProtocolParamUpdate(maxBlockHeaderSize = Some(0)),
          ProtocolParamUpdate(maxValueSize = Some(0)),
          ProtocolParamUpdate(collateralPercentage = Some(0)),
          ProtocolParamUpdate(committeeTermLimit = Some(0)),
          ProtocolParamUpdate(governanceActionValidityPeriod = Some(0)),
          ProtocolParamUpdate(poolDeposit = Some(Coin.zero)),
          ProtocolParamUpdate(governanceActionDeposit = Some(Coin.zero)),
          ProtocolParamUpdate(drepDeposit = Some(Coin.zero)),
          ProtocolParamUpdate(adaPerUtxoByte = Some(Coin.zero)),
          ProtocolParamUpdate(nOpt = Some(0)),
          ProtocolParamUpdate()
        )
        for update <- zeros do
            withClue(update) {
                failureOf(run(tx(proposals = Seq(proposal(ppChange(update)))))) shouldBe
                    GovFailure.MalformedProposal(ppChange(update))
            }
        // a zero in a field the check does not cover is well formed
        run(
          tx(proposals = Seq(proposal(ppChange(ProtocolParamUpdate(keyDeposit = Some(Coin.zero))))))
        ).isRight shouldBe true
    }

    test("spec [GV-7a]: nOpt = 0 is malformed from PV 11, adaPerUtxoByte = 0 from PV 10") {
        val nOpt = proposal(ppChange(ProtocolParamUpdate(nOpt = Some(0))))
        failureOf(run(tx(proposals = Seq(nOpt)), major = 11)) shouldBe
            GovFailure.MalformedProposal(nOpt.govAction)
        run(tx(proposals = Seq(nOpt)), major = 10).isRight shouldBe true
        val perByte = proposal(ppChange(ProtocolParamUpdate(adaPerUtxoByte = Some(Coin.zero))))
        failureOf(run(tx(proposals = Seq(perByte)), major = 10)) shouldBe
            GovFailure.MalformedProposal(perByte.govAction)
        run(tx(proposals = Seq(perByte)), major = 9).isRight shouldBe true
    }

    test("spec [GV-8]: the policy hash must equal the constitution's guardrails hash") {
        val noPolicy = ppChange(validUpdate, policy = None)
        failureOf(run(tx(proposals = Seq(proposal(noPolicy))))) shouldBe
            GovFailure.InvalidGuardrailsScriptHash(None, Some(guardrails))
        val withdrawal =
            GovAction.TreasuryWithdrawals(Map(returnAccount -> Coin(5)), Some(otherScript))
        failureOf(run(tx(proposals = Seq(proposal(withdrawal))))) shouldBe
            GovFailure.InvalidGuardrailsScriptHash(Some(otherScript), Some(guardrails))
        val noGuardrails = withGov(_.copy(constitution = Constitution(anchor, None)))
        run(tx(proposals = Seq(proposal(noPolicy))), noGuardrails).isRight shouldBe true
        failureOf(run(tx(proposals = Seq(proposal(ppChange(validUpdate)))), noGuardrails)) shouldBe
            GovFailure.InvalidGuardrailsScriptHash(Some(guardrails), None)
    }

    test("spec [GV-3b]: the return account and the withdrawal accounts must be registered") {
        val p = proposal(GovAction.InfoAction, unregisteredAccount)
        failureOf(run(tx(proposals = Seq(p)))) shouldBe
            GovFailure.ProposalReturnAccountDoesNotExist(unregisteredAccount)
        val withdrawal = GovAction.TreasuryWithdrawals(
          Map(returnAccount -> Coin(5), unregisteredAccount -> Coin(5)),
          Some(guardrails)
        )
        failureOf(run(tx(proposals = Seq(proposal(withdrawal))))) shouldBe
            GovFailure.TreasuryWithdrawalReturnAccountsDoNotExist(List(unregisteredAccount))
    }

    test("spec [GV-2], [GV-3b]: an account registered by the same tx is a valid return account") {
        val credential = unregisteredAccount.address.credential
        val registration =
            Certificate.RegCert(credential, Some(Coin(baseParams.stakeAddressDeposit)))
        val p = proposal(GovAction.InfoAction, unregisteredAccount)
        runWithCerts(tx(proposals = Seq(p), certs = Seq(registration))).isRight shouldBe true
    }

    test(
      "spec [GV-3b], D40: at PV 9 the return-account checks and ZeroTreasuryWithdrawals are skipped"
    ) {
        // `unless (hardforkConwayBootstrapPhase ...)`, Gov.hs:514-530 and :559-562, D40. The
        // ledger's own test for the first case is GovSpec.hs:90-97 (`FailPostBootstrap`).
        val info = proposal(GovAction.InfoAction, unregisteredAccount)
        run(tx(proposals = Seq(info)), major = 9).isRight shouldBe true
        failureOf(run(tx(proposals = Seq(info)), major = 10)) shouldBe
            GovFailure.ProposalReturnAccountDoesNotExist(unregisteredAccount)
        // The two withdrawal cases pin only that the skipped checks do not fire: at PV 9 the
        // ledger rejects every TreasuryWithdrawals with DisallowedProposalDuringBootstrap
        // (`isBootstrapAction`, Gov.hs:645-650), which spec section 5.8 leaves N/A.
        val unregistered =
            GovAction.TreasuryWithdrawals(Map(unregisteredAccount -> Coin.zero), Some(guardrails))
        run(tx(proposals = Seq(proposal(unregistered))), major = 9).isRight shouldBe true
        failureOf(run(tx(proposals = Seq(proposal(unregistered))), major = 10)) shouldBe
            GovFailure.TreasuryWithdrawalReturnAccountsDoNotExist(List(unregisteredAccount))
        val zero = GovAction.TreasuryWithdrawals(Map(returnAccount -> Coin.zero), Some(guardrails))
        run(tx(proposals = Seq(proposal(zero))), major = 9).isRight shouldBe true
        failureOf(run(tx(proposals = Seq(proposal(zero))), major = 10)) shouldBe
            GovFailure.ZeroTreasuryWithdrawals(zero)
    }

    test(
      "spec [GV-3], [GV-3a]: ProposalProcedureNetworkIdMismatch and TreasuryWithdrawalsNetworkIdMismatch"
    ) {
        val p = proposal(GovAction.InfoAction, mainnetReturnAccount)
        failureOf(run(tx(proposals = Seq(p)))) shouldBe
            GovFailure.ProposalProcedureNetworkIdMismatch(mainnetReturnAccount, Network.Testnet)
        // only the mismatched accounts are reported (Gov.hs:552)
        val withdrawal = GovAction.TreasuryWithdrawals(
          Map(mainnetReturnAccount -> Coin(5), returnAccount -> Coin(5)),
          Some(guardrails)
        )
        failureOf(run(tx(proposals = Seq(proposal(withdrawal))))) shouldBe
            GovFailure.TreasuryWithdrawalsNetworkIdMismatch(
              Set(mainnetReturnAccount),
              Network.Testnet
            )
    }

    test(
      "spec [GV-3], [GV-3a]: ZeroTreasuryWithdrawals, the withdrawals must sum to more than zero"
    ) {
        val withdrawal =
            GovAction.TreasuryWithdrawals(Map(returnAccount -> Coin.zero), Some(guardrails))
        failureOf(run(tx(proposals = Seq(proposal(withdrawal))))) shouldBe
            GovFailure.ZeroTreasuryWithdrawals(withdrawal)
        val empty = GovAction.TreasuryWithdrawals(Map.empty, Some(guardrails))
        failureOf(run(tx(proposals = Seq(proposal(empty))))) shouldBe
            GovFailure.ZeroTreasuryWithdrawals(empty)
    }

    test("spec [GV-18]: ConflictingCommitteeUpdate and ExpirationEpochTooSmall") {
        val conflicting =
            GovAction.UpdateCommittee(
              None,
              Set(newMember),
              Map(newMember -> 20L),
              UnitInterval(1, 2)
            )
        failureOf(run(tx(proposals = Seq(proposal(conflicting))))) shouldBe
            GovFailure.ConflictingCommitteeUpdate(Set(newMember))
        // only the members whose term ends by the current epoch are reported (Gov.hs:567)
        val ending = GovAction.UpdateCommittee(
          None,
          Set.empty,
          Map(newMember -> 10L, cold -> 11L),
          UnitInterval(1, 2)
        )
        failureOf(run(tx(proposals = Seq(proposal(ending))))) shouldBe
            GovFailure.ExpirationEpochTooSmall(Map(newMember -> 10L))
        val next =
            GovAction.UpdateCommittee(None, Set.empty, Map(newMember -> 11L), UnitInterval(1, 2))
        run(tx(proposals = Seq(proposal(next)))).isRight shouldBe true
    }

    test("spec [GV-17]: a hard fork version must follow its predecessor") {
        def hf(major: Int, minor: Int, prev: Option[GovActionId] = None) =
            proposal(GovAction.HardForkInitiation(prev, ProtocolVersion(major, minor)))
        run(tx(proposals = Seq(hf(12, 0)))).isRight shouldBe true
        run(tx(proposals = Seq(hf(11, 1)))).isRight shouldBe true
        for (major, minor) <- Seq((11, 0), (11, 2), (12, 1), (13, 0)) do
            withClue(s"$major.$minor") {
                failureOf(run(tx(proposals = Seq(hf(major, minor))))) shouldBe
                    GovFailure.ProposalCantFollow(
                      None,
                      ProtocolVersion(major, minor),
                      ProtocolVersion(11, 0)
                    )
            }
        // the predecessor is the pending hard fork the proposal names
        val pendingHf = actionId(4)
        val state = withGov(g =>
            g.copy(proposals =
                g.proposals :+ pending(
                  pendingHf,
                  GovAction.HardForkInitiation(None, ProtocolVersion(12, 0))
                )
            )
        )
        run(tx(proposals = Seq(hf(12, 1, Some(pendingHf)))), state).isRight shouldBe true
        failureOf(run(tx(proposals = Seq(hf(12, 5, Some(pendingHf)))), state)) shouldBe
            GovFailure.ProposalCantFollow(
              Some(pendingHf),
              ProtocolVersion(12, 5),
              ProtocolVersion(12, 0)
            )
        // a major above current + 1 is compared with the current version
        failureOf(run(tx(proposals = Seq(hf(13, 0, Some(pendingHf)))), state)) shouldBe
            GovFailure.ProposalCantFollow(
              Some(pendingHf),
              ProtocolVersion(13, 0),
              ProtocolVersion(11, 0)
            )
    }

    test("spec [GV-17a]: a hard fork of the same tx is not a predecessor for ProposalCantFollow") {
        val first = proposal(GovAction.HardForkInitiation(None, ProtocolVersion(12, 0)))
        // 12.5 cannot follow 12.0, but the check only looks at the proposals before the tx
        val second = proposal(
          GovAction.HardForkInitiation(Some(GovActionId(fakeTxId, 0)), ProtocolVersion(12, 5))
        )
        runAs(tx(proposals = Seq(first, second))).isRight shouldBe true
    }

    test("spec [GV-17]: the hard-fork root as parent makes the current version the predecessor") {
        // `mPrev == pgaids ^. grHardForkL`, Gov.hs:692-707
        val root = actionId(5)
        val state = withGov(_.copy(roots = GovRoots(None, Some(root), None, None)))
        val next = proposal(GovAction.HardForkInitiation(Some(root), ProtocolVersion(12, 0)))
        run(tx(proposals = Seq(next)), state).isRight shouldBe true
        val skipped = proposal(GovAction.HardForkInitiation(Some(root), ProtocolVersion(12, 1)))
        failureOf(run(tx(proposals = Seq(skipped)), state)) shouldBe
            GovFailure.ProposalCantFollow(
              Some(root),
              ProtocolVersion(12, 1),
              ProtocolVersion(11, 0)
            )
    }

    test("spec [GV-17]: a parent that is not a pending hard fork falls to InvalidPrevGovActionId") {
        val state = withGov(_.copy(roots = GovRoots(None, Some(actionId(5)), None, None)))
        val noParent = proposal(GovAction.HardForkInitiation(None, ProtocolVersion(12, 0)))
        failureOf(run(tx(proposals = Seq(noParent)), state)) shouldBe
            GovFailure.InvalidPrevGovActionId(noParent)
        val infoParent =
            proposal(GovAction.HardForkInitiation(Some(infoId), ProtocolVersion(12, 0)))
        failureOf(run(tx(proposals = Seq(infoParent)), state)) shouldBe
            GovFailure.InvalidPrevGovActionId(infoParent)
    }

    test("spec [GV-3a]: the checks of one proposal run in the ledger's order") {
        val state = withGov(_.copy(roots = GovRoots(Some(actionId(2)), None, None, None)))
        // MalformedProposal before the return account, the guardrails and the parent
        val malformed =
            proposal(ppChange(ProtocolParamUpdate(), None, None), unregisteredAccount)
        failureOf(run(tx(proposals = Seq(malformed)), state)) shouldBe
            GovFailure.MalformedProposal(malformed.govAction)
        // the return account before the network id
        val wrongNetwork = rewardAccount("7", Network.Mainnet)
        failureOf(run(tx(proposals = Seq(proposal(GovAction.InfoAction, wrongNetwork))))) shouldBe
            GovFailure.ProposalReturnAccountDoesNotExist(wrongNetwork)
        // the return account before the withdrawal accounts (Gov.hs:518-530)
        val unregisteredWithdrawal =
            GovAction.TreasuryWithdrawals(Map(unregisteredAccount -> Coin(5)), Some(guardrails))
        failureOf(
          run(tx(proposals = Seq(proposal(unregisteredWithdrawal, unregisteredAccount))))
        ) shouldBe GovFailure.ProposalReturnAccountDoesNotExist(unregisteredAccount)
        // the withdrawal accounts before the return account's network id (Gov.hs:520-530, 543-546);
        // the mainnet return account is registered, as the accounts are keyed by credential
        failureOf(
          run(tx(proposals = Seq(proposal(unregisteredWithdrawal, mainnetReturnAccount))))
        ) shouldBe GovFailure.TreasuryWithdrawalReturnAccountsDoNotExist(List(unregisteredAccount))
        // the return account's network id before the withdrawals' (Gov.hs:543-555)
        val mainnetWithdrawal =
            GovAction.TreasuryWithdrawals(Map(mainnetReturnAccount -> Coin(5)), Some(guardrails))
        failureOf(
          run(tx(proposals = Seq(proposal(mainnetWithdrawal, mainnetReturnAccount))))
        ) shouldBe GovFailure.ProposalProcedureNetworkIdMismatch(
          mainnetReturnAccount,
          Network.Testnet
        )
        // the withdrawal network before the guardrails and the zero sum; only the mismatched
        // account is reported
        val withdrawal = GovAction.TreasuryWithdrawals(
          Map(mainnetReturnAccount -> Coin.zero, returnAccount -> Coin.zero),
          None
        )
        failureOf(run(tx(proposals = Seq(proposal(withdrawal))))) shouldBe
            GovFailure.TreasuryWithdrawalsNetworkIdMismatch(
              Set(mainnetReturnAccount),
              Network.Testnet
            )
        // the guardrails before the zero sum (Gov.hs:555-562)
        val zeroWithOtherScript =
            GovAction.TreasuryWithdrawals(Map(returnAccount -> Coin.zero), Some(otherScript))
        failureOf(run(tx(proposals = Seq(proposal(zeroWithOtherScript))))) shouldBe
            GovFailure.InvalidGuardrailsScriptHash(Some(otherScript), Some(guardrails))
        // ConflictingCommitteeUpdate before ExpirationEpochTooSmall (Gov.hs:563-568)
        val conflictingAndEnding =
            GovAction.UpdateCommittee(
              None,
              Set(newMember),
              Map(newMember -> 10L),
              UnitInterval(1, 2)
            )
        failureOf(run(tx(proposals = Seq(proposal(conflictingAndEnding))))) shouldBe
            GovFailure.ConflictingCommitteeUpdate(Set(newMember))
        // ExpirationEpochTooSmall before the parent (Gov.hs:567-578); only the ending member is
        // reported
        val committeeRoot = withGov(_.copy(roots = GovRoots(None, None, Some(actionId(2)), None)))
        val ending = GovAction.UpdateCommittee(
          None,
          Set.empty,
          Map(newMember -> 10L, cold -> 11L),
          UnitInterval(1, 2)
        )
        failureOf(run(tx(proposals = Seq(proposal(ending))), committeeRoot)) shouldBe
            GovFailure.ExpirationEpochTooSmall(Map(newMember -> 10L))
        // the guardrails before the parent
        val badBoth = proposal(ppChange(validUpdate, None, None))
        failureOf(run(tx(proposals = Seq(badBoth)), state)) shouldBe
            GovFailure.InvalidGuardrailsScriptHash(None, Some(guardrails))
        // ProposalCantFollow first
        val hf = proposal(
          GovAction.HardForkInitiation(None, ProtocolVersion(13, 0)),
          unregisteredAccount
        )
        failureOf(run(tx(proposals = Seq(hf)))) shouldBe
            GovFailure.ProposalCantFollow(None, ProtocolVersion(13, 0), ProtocolVersion(11, 0))
        // the proposals are checked in tx order
        val first = proposal(GovAction.InfoAction, unregisteredAccount)
        val second = proposal(ppChange(ProtocolParamUpdate()))
        failureOf(run(tx(proposals = Seq(first, second)))) shouldBe
            GovFailure.ProposalReturnAccountDoesNotExist(unregisteredAccount)
    }

    // ---------------------------------------------------------------- votes

    test("spec [GV-9]: a vote on a proposal of the same tx is stored") {
        val newId = GovActionId(fakeTxId, 0)
        val transaction =
            tx(
              proposals = Seq(proposal(GovAction.InfoAction)),
              votes = Seq((drepVoterA, newId, Vote.Yes))
            )
        runAs(transaction).value.govState.proposal(newId).value.drepVotes shouldBe
            Map(drepA -> Vote.Yes)
    }

    test(
      "spec [GV-10], [GV-10b]: an unknown DRep on an unknown action gets only VotersDoNotExist"
    ) {
        // the example of spec section 11.1
        failureOf(run(tx(votes = Seq((unknownDRepVoter, actionId(99), Vote.Yes))))) shouldBe
            GovFailure.VotersDoNotExist(List(unknownDRepVoter))
        failureOf(run(tx(votes = Seq((drepVoterA, actionId(99), Vote.Yes))))) shouldBe
            GovFailure.GovActionsDoNotExist(List(actionId(99)))
        // an unknown voter and a known voter on an unknown action: VotersDoNotExist first
        failureOf(
          run(
            tx(votes =
                Seq((unknownDRepVoter, infoId, Vote.Yes), (drepVoterA, actionId(99), Vote.Yes))
            )
          )
        ) shouldBe GovFailure.VotersDoNotExist(List(unknownDRepVoter))
    }

    test("spec [GV-10]: an unknown voter with no votes still gets VotersDoNotExist") {
        // Gov.hs:609-613 reads the voters from the outer map, so a voter whose inner map is
        // empty is checked too
        val empty = tx().withBody(
          _.copy(votingProcedures =
              Some(VotingProcedures(SortedMap(unknownDRepVoter -> SortedMap.empty)))
          )
        )
        failureOf(run(empty)) shouldBe GovFailure.VotersDoNotExist(List(unknownDRepVoter))
    }

    test("spec [GV-10a]: a pool and a script DRep must be registered to vote") {
        val unknownPool = Voter.StakingPoolKey(AddrKeyHash.fromHex("2" * 56))
        failureOf(run(tx(votes = Seq((unknownPool, infoId, Vote.Yes))))) shouldBe
            GovFailure.VotersDoNotExist(List(unknownPool))
        run(tx(votes = Seq((poolVoter, infoId, Vote.Yes)))).isRight shouldBe true
        val unknownScriptDRep = Voter.DRepScript(otherScript)
        failureOf(run(tx(votes = Seq((unknownScriptDRep, infoId, Vote.Yes))))) shouldBe
            GovFailure.VotersDoNotExist(List(unknownScriptDRep))
        run(tx(votes = Seq((scriptDRepVoter, infoId, Vote.Yes)))).isRight shouldBe true
    }

    test("spec [GV-10b]: VotingOnExpiredGovAction, a vote after the action's last epoch") {
        run(tx(votes = Seq((drepVoterA, infoId, Vote.Yes))), epoch = 14).isRight shouldBe true
        failureOf(run(tx(votes = Seq((drepVoterA, infoId, Vote.Yes))), epoch = 15)) shouldBe
            GovFailure.VotingOnExpiredGovAction(List(drepVoterA -> infoId))
    }

    test("spec [GV-11], [GV-11a]: the voter permission table of spec section 15") {
        val actions: Seq[(String, GovAction)] = Seq(
          "NoConfidence" -> GovAction.NoConfidence(None),
          "UpdateCommittee" -> GovAction
              .UpdateCommittee(None, Set.empty, Map.empty, UnitInterval(1, 2)),
          "NewConstitution" -> GovAction.NewConstitution(None, constitution),
          "HardForkInitiation" -> GovAction.HardForkInitiation(None, ProtocolVersion(12, 0)),
          "ParameterChange security" -> ppChange(validUpdate),
          "ParameterChange other" -> ppChange(ProtocolParamUpdate(keyDeposit = Some(Coin(1)))),
          "TreasuryWithdrawals" -> GovAction
              .TreasuryWithdrawals(Map(returnAccount -> Coin(1)), Some(guardrails)),
          "InfoAction" -> GovAction.InfoAction
        )
        // (CC, DRep, SPO)
        val allowed = Map(
          "NoConfidence" -> (false, true, true),
          "UpdateCommittee" -> (false, true, true),
          "NewConstitution" -> (true, true, false),
          "HardForkInitiation" -> (true, true, true),
          "ParameterChange security" -> (true, true, true),
          "ParameterChange other" -> (true, true, false),
          "TreasuryWithdrawals" -> (true, true, false),
          "InfoAction" -> (true, true, true)
        )
        val id = actionId(6)
        for (name, action) <- actions do
            val state = withGov(g => g.copy(proposals = g.proposals :+ pending(id, action)))
            val (cc, drep, spo) = allowed(name)
            // the permission is keyed on the role, not on the credential kind
            // (Procedures.hs:331-335, Internal.hs:358-540)
            val voters = Seq(
              ccVoter -> cc,
              ccScriptVoter -> cc,
              drepVoterA -> drep,
              scriptDRepVoter -> drep,
              poolVoter -> spo
            )
            for (voter, ok) <- voters do
                withClue(s"$name, $voter") {
                    val result = run(tx(votes = Seq((voter, id, Vote.Yes))), state)
                    if ok then result.isRight shouldBe true
                    else failureOf(result) shouldBe GovFailure.DisallowedVoters(List(voter -> id))
                }
    }

    test("spec [GV-11a]: each security-group field lets pools vote on a ParameterChange") {
        val security = Seq(
          ProtocolParamUpdate(minFeeA = Some(Coin(1))),
          ProtocolParamUpdate(minFeeB = Some(Coin(1))),
          ProtocolParamUpdate(maxBlockBodySize = Some(1)),
          ProtocolParamUpdate(maxTxSize = Some(1)),
          ProtocolParamUpdate(maxBlockHeaderSize = Some(1)),
          ProtocolParamUpdate(adaPerUtxoByte = Some(Coin(1))),
          ProtocolParamUpdate(maxBlockExUnits = Some(ExUnits(1, 1))),
          ProtocolParamUpdate(maxValueSize = Some(1)),
          ProtocolParamUpdate(governanceActionDeposit = Some(Coin(1))),
          ProtocolParamUpdate(minFeeRefScriptCoinsPerByte = Some(NonNegativeInterval(1)))
        )
        val id = actionId(7)
        for update <- security do
            val state =
                withGov(g => g.copy(proposals = g.proposals :+ pending(id, ppChange(update))))
            withClue(update) {
                run(tx(votes = Seq((poolVoter, id, Vote.Yes))), state).isRight shouldBe true
            }
    }

    test("spec [GV-12]: at PV 11 a hot key that no elected member authorized cannot vote") {
        val strangerHash = AddrKeyHash.fromHex("c3" * 28)
        val stranger = Voter.ConstitutionalCommitteeHotKey(strangerHash)
        failureOf(run(tx(votes = Seq((stranger, infoId, Vote.Yes))))) shouldBe
            GovFailure.UnelectedCommitteeVoters(Set(Credential.KeyHash(strangerHash)))
        // at PV 10 the same voter is unknown
        failureOf(run(tx(votes = Seq((stranger, infoId, Vote.Yes))), major = 10)) shouldBe
            GovFailure.VotersDoNotExist(List(stranger))
        // a hot script that no member authorized is reported under its script hash
        val strangerScriptHash = ScriptHash.fromHex("c7" * 28)
        val strangerScript = Voter.ConstitutionalCommitteeHotScript(strangerScriptHash)
        failureOf(run(tx(votes = Seq((strangerScript, infoId, Vote.Yes))))) shouldBe
            GovFailure.UnelectedCommitteeVoters(Set(Credential.ScriptHash(strangerScriptHash)))
        run(tx(votes = Seq((ccScriptVoter, infoId, Vote.Yes)))).isRight shouldBe true
        // a hot key authorized by a cold key that is not a member
        val nonMember = Credential.KeyHash(AddrKeyHash.fromHex("c4" * 28))
        val authorized = baseState.copy(certState =
            baseState.certState.copy(vstate =
                baseState.certState.vstate.copy(committeeState =
                    Map(nonMember -> CommitteeAuthorization.HotCredential(hot))
                )
            )
        )
        failureOf(run(tx(votes = Seq((ccVoter, infoId, Vote.Yes))), authorized)) shouldBe
            GovFailure.UnelectedCommitteeVoters(Set(hot))
        run(
          tx(votes = Seq((ccVoter, infoId, Vote.Yes))),
          authorized,
          major = 10
        ).isRight shouldBe true
        // no committee: every committee voter is unelected
        val noCommittee = withGov(_.copy(committee = None))
        failureOf(run(tx(votes = Seq((ccVoter, infoId, Vote.Yes))), noCommittee)) shouldBe
            GovFailure.UnelectedCommitteeVoters(Set(hot))
        run(
          tx(votes = Seq((ccVoter, infoId, Vote.Yes))),
          noCommittee,
          major = 10
        ).isRight shouldBe true
    }

    test("spec [GV-10a]: a resigned member's hot key is unknown") {
        val resigned = baseState.copy(certState =
            baseState.certState.copy(vstate =
                baseState.certState.vstate
                    .copy(committeeState = Map(cold -> CommitteeAuthorization.Resigned(None)))
            )
        )
        failureOf(run(tx(votes = Seq((ccVoter, infoId, Vote.Yes))), resigned, major = 10)) shouldBe
            GovFailure.VotersDoNotExist(List(ccVoter))
    }

    test("spec [GV-10b]: UnelectedCommitteeVoters is checked before the proposals") {
        val stranger = Voter.ConstitutionalCommitteeHotKey(AddrKeyHash.fromHex("c3" * 28))
        val malformed = proposal(ppChange(ProtocolParamUpdate()))
        failureOf(
          run(tx(proposals = Seq(malformed), votes = Seq((stranger, infoId, Vote.Yes))))
        ) shouldBe
            GovFailure.UnelectedCommitteeVoters(
              Set(Credential.KeyHash(AddrKeyHash.fromHex("c3" * 28)))
            )
        // the other vote checks come after the proposals
        failureOf(
          run(tx(proposals = Seq(malformed), votes = Seq((unknownDRepVoter, infoId, Vote.Yes))))
        ) shouldBe
            GovFailure.MalformedProposal(malformed.govAction)
    }

    test("spec [FL-1], [FL-7]: the collecting entry reports every GOV failure in source order") {
        // Gov.hs:489-620 checks every predicate and adds a failing proposal all the same
        val stranger = Voter.ConstitutionalCommitteeHotKey(AddrKeyHash.fromHex("c3" * 28))
        val malformed = proposal(ppChange(ProtocolParamUpdate(), None, None), unregisteredAccount)
        val transaction =
            tx(proposals = Seq(malformed), votes = Seq((stranger, infoId, Vote.Yes)))
        val failures = GovMutator
            .transitAll(context(baseState, 11, 10), baseState, transaction, transaction.id)
            .left
            .value
            .map(e => failureOf(Left(e)))
        failures shouldBe Seq(
          GovFailure.UnelectedCommitteeVoters(
            Set(Credential.KeyHash(AddrKeyHash.fromHex("c3" * 28)))
          ),
          GovFailure.MalformedProposal(malformed.govAction),
          GovFailure.ProposalReturnAccountDoesNotExist(unregisteredAccount),
          GovFailure.InvalidGuardrailsScriptHash(None, Some(guardrails)),
          GovFailure.VotersDoNotExist(List(stranger))
        )
        // transit keeps the first one
        failureOf(run(transaction)) shouldBe failures.head
    }

    test("spec [GV-3a]: transit reports a bad deposit first, transitAll in the ledger order") {
        // ProposalDepositValidator is a validator, and transit runs every validator before any
        // mutator (D30); transitAll sorts by the ledger order, where MalformedProposal comes
        // before ProposalDepositIncorrect
        val malformed = proposal(ppChange(ProtocolParamUpdate(), None, None))
            .copy(deposit = deposit + Coin(1))
        val transaction = tx(proposals = Seq(malformed))
        val ctx = context(baseState, 11, 10)
        val validators = Seq(ProposalDepositValidator)
        val mutators = Seq(GovMutator)
        val first = STS.Mutator.transit(validators, mutators, ctx, baseState, transaction)
        first.left.value shouldBe a[TransactionException.ProposalDepositIncorrectException]
        val all = STS.Mutator.transitAll(validators, mutators, ctx, baseState, transaction)
        all.left.value.flatMap(TransactionException.ledgerNames) shouldBe Seq(
          "MalformedProposal",
          "ProposalDepositIncorrect",
          "InvalidGuardrailsScriptHash"
        )
    }

    test("spec [FL-1]: a proposal that fails a check is still added for the votes and parents") {
        // Gov.hs:574-578 adds a failing proposal; curGovActionIds comes from the proposals after
        // the fold (Gov.hs:582-604), so a vote on it and a child of it name a known action
        val malformed = proposal(ppChange(ProtocolParamUpdate(), None, None), unregisteredAccount)
        val child = proposal(ppChange(validUpdate, Some(GovActionId(fakeTxId, 0))))
        val transaction = tx(
          proposals = Seq(malformed, child),
          votes = Seq((drepVoterA, GovActionId(fakeTxId, 0), Vote.Yes))
        )
        val failures = GovMutator
            .transitAll(context(baseState, 11, 10), baseState, transaction, fakeTxId)
            .left
            .value
            .map(e => failureOf(Left(e)))
        failures shouldBe Seq(
          GovFailure.MalformedProposal(malformed.govAction),
          GovFailure.ProposalReturnAccountDoesNotExist(unregisteredAccount),
          GovFailure.InvalidGuardrailsScriptHash(None, Some(guardrails))
        )
    }

    test("spec [GV-10b]: VotersDoNotExist, GovActionsDoNotExist, expiry, then permission") {
        val noConfidence = actionId(8)
        val state = withGov(g =>
            g.copy(proposals =
                g.proposals :+ pending(noConfidence, GovAction.NoConfidence(None), 9)
            )
        )
        // an unknown action and an expired one: GovActionsDoNotExist first
        failureOf(
          run(
            tx(votes =
                Seq((drepVoterA, actionId(99), Vote.Yes), (drepVoterA, noConfidence, Vote.No))
            ),
            state
          )
        ) shouldBe GovFailure.GovActionsDoNotExist(List(actionId(99)))
        // expired and disallowed: the committee vote on an expired NoConfidence is expired first
        failureOf(run(tx(votes = Seq((ccVoter, noConfidence, Vote.Yes))), state)) shouldBe
            GovFailure.VotingOnExpiredGovAction(List(ccVoter -> noConfidence))
    }

    test("spec [GV-13]: a member whose term ended still votes without a GOV failure") {
        // the example of spec section 11.1: term ends in epoch 50, the vote is in epoch 60
        val ended = withGov(g =>
            g.copy(
              proposals = Vector(pending(infoId, GovAction.InfoAction, 70)),
              committee = Some(Committee(Map(cold -> 50L), UnitInterval(1, 2)))
            )
        )
        val result = run(tx(votes = Seq((ccVoter, infoId, Vote.Yes))), ended, epoch = 60)
        result.value.govState.proposal(infoId).value.committeeVotes shouldBe Map(hot -> Vote.Yes)
    }

    test("spec [GV-14]: a new vote replaces the voter's earlier vote, keyed by its credential") {
        val voted = withGov(g =>
            g.copy(proposals =
                Vector(
                  pending(infoId, GovAction.InfoAction).copy(
                    committeeVotes = Map(hotScript -> Vote.No),
                    drepVotes = Map(drepA -> Vote.No, scriptDRep -> Vote.No),
                    spoVotes = Map(poolId -> Vote.No)
                  )
                )
            )
        )
        val stored = run(
          tx(votes =
              Seq(
                (drepVoterA, infoId, Vote.Yes),
                (scriptDRepVoter, infoId, Vote.Yes),
                (poolVoter, infoId, Vote.Abstain),
                (ccVoter, infoId, Vote.Yes),
                (ccScriptVoter, infoId, Vote.Abstain)
              )
          ),
          voted
        ).value.govState.proposal(infoId).value
        stored.drepVotes shouldBe Map(drepA -> Vote.Yes, scriptDRep -> Vote.Yes)
        stored.spoVotes shouldBe Map(poolId -> Vote.Abstain)
        stored.committeeVotes shouldBe Map(hot -> Vote.Yes, hotScript -> Vote.Abstain)
    }

    test("spec [GV-15]: unregistering a DRep removes its votes from every proposal") {
        val drepDeposit = Coin(baseParams.dRepDeposit)
        val secondId = actionId(9)
        val withB = baseState.copy(
          certState = baseState.certState.copy(vstate =
              baseState.certState.vstate.copy(dreps =
                  baseState.certState.vstate.dreps +
                      (drepB -> DRepState(100, None, drepDeposit, Set.empty))
              )
          ),
          govState = baseState.govState.copy(proposals =
              Vector(
                pending(infoId, GovAction.InfoAction)
                    .copy(drepVotes = Map(drepA -> Vote.Yes, drepB -> Vote.No)),
                pending(secondId, GovAction.InfoAction).copy(drepVotes = Map(drepB -> Vote.Yes))
              )
          )
        )
        val result = runWithCerts(
          tx(
            certs = Seq(Certificate.UnregDRepCert(drepB, drepDeposit)),
            votes = Seq((drepVoterA, secondId, Vote.No))
          ),
          withB
        ).value.govState
        result.proposal(infoId).value.drepVotes shouldBe Map(drepA -> Vote.Yes)
        result.proposal(secondId).value.drepVotes shouldBe Map(drepA -> Vote.No)
    }

    test("spec [GV-2a]: a DRep registered by the same tx can vote") {
        val registration =
            Certificate.RegDRepCert(drepB, Coin(baseParams.dRepDeposit), None)
        val transaction = tx(certs = Seq(registration), votes = Seq((drepVoterB, infoId, Vote.Yes)))
        runWithCerts(transaction).value.govState.proposal(infoId).value.drepVotes shouldBe
            Map(drepB -> Vote.Yes)
        failureOf(run(transaction)) shouldBe GovFailure.VotersDoNotExist(List(drepVoterB))
    }

    test("spec [GV-16]: a phase-2-failed tx changes no gov state") {
        val bad = tx(proposals = Seq(proposal(ppChange(ProtocolParamUpdate()))))
        run(bad.copy(isValid = false)).value shouldBe baseState
    }

    test("spec [GV-19]: ruleName is the ledger constructor name") {
        val id = actionId(1)
        val failures: Seq[(GovFailure, String)] = Seq(
          GovFailure.GovActionsDoNotExist(List(id)) -> "GovActionsDoNotExist",
          GovFailure.MalformedProposal(GovAction.InfoAction) -> "MalformedProposal",
          GovFailure.ProposalProcedureNetworkIdMismatch(returnAccount, Network.Mainnet) ->
              "ProposalProcedureNetworkIdMismatch",
          GovFailure.TreasuryWithdrawalsNetworkIdMismatch(Set(returnAccount), Network.Mainnet) ->
              "TreasuryWithdrawalsNetworkIdMismatch",
          GovFailure.DisallowedVoters(List(ccVoter -> id)) -> "DisallowedVoters",
          GovFailure.ConflictingCommitteeUpdate(Set(cold)) -> "ConflictingCommitteeUpdate",
          GovFailure.ExpirationEpochTooSmall(Map(cold -> 1L)) -> "ExpirationEpochTooSmall",
          GovFailure.InvalidPrevGovActionId(proposal(GovAction.InfoAction)) ->
              "InvalidPrevGovActionId",
          GovFailure.VotingOnExpiredGovAction(List(ccVoter -> id)) -> "VotingOnExpiredGovAction",
          GovFailure.ProposalCantFollow(None, ProtocolVersion(13, 0), ProtocolVersion(11, 0)) ->
              "ProposalCantFollow",
          GovFailure.InvalidGuardrailsScriptHash(None, None) -> "InvalidGuardrailsScriptHash",
          GovFailure.VotersDoNotExist(List(ccVoter)) -> "VotersDoNotExist",
          GovFailure.ZeroTreasuryWithdrawals(GovAction.InfoAction) -> "ZeroTreasuryWithdrawals",
          GovFailure.ProposalReturnAccountDoesNotExist(returnAccount) ->
              "ProposalReturnAccountDoesNotExist",
          GovFailure.TreasuryWithdrawalReturnAccountsDoNotExist(List(returnAccount)) ->
              "TreasuryWithdrawalReturnAccountsDoNotExist",
          GovFailure.UnelectedCommitteeVoters(Set(hot)) -> "UnelectedCommitteeVoters"
        )
        failures.map(_._2).distinct.size shouldBe 16
        for (failure, name) <- failures do
            TransactionException.ruleName(
              GovException(TransactionHash.fromHex("0" * 64), failure)
            ) shouldBe name
    }
}
