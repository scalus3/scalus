package scalus.cardano.ledger.rules

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*

/** `State.deposited` and `DRepState.delegates` of the CERTS step, spec [BK-1] to [BK-3]. */
class CertsMutatorBookkeepingTest extends AnyFunSuite with Matchers with EitherValues {

    private val cardanoInfo = CardanoInfo.preprod
    private val params = cardanoInfo.protocolParams
    private val keyDeposit = Coin(params.stakeAddressDeposit)
    private val drepDeposit = Coin(params.dRepDeposit)

    private val account = Credential.KeyHash(AddrKeyHash.fromHex("1" * 56))
    private val drepA = Credential.KeyHash(AddrKeyHash.fromHex("a" * 56))
    private val drepB = Credential.KeyHash(AddrKeyHash.fromHex("b" * 56))
    private val scriptDRep = Credential.ScriptHash(ScriptHash.fromHex("c" * 56))
    private def asDRep(credential: Credential): DRep = credential match
        case Credential.KeyHash(hash)    => DRep.KeyHash(hash)
        case Credential.ScriptHash(hash) => DRep.ScriptHash(hash)

    private def run(
        state: State,
        certs: Seq[Certificate],
        proposals: Seq[ProposalProcedure] = Seq.empty
    ): State = {
        val context = new Context(
          env = UtxoEnv(
            slot = 0,
            params = params,
            certState = state.certState,
            network = Network.Testnet,
            treasury = Coin.zero
          ),
          slotConfig = cardanoInfo.slotConfig
        )
        val tx = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            certificates =
                if certs.isEmpty then TaggedOrderedStrictSet.empty
                else TaggedOrderedStrictSet.from(certs),
            proposalProcedures =
                if proposals.isEmpty then TaggedOrderedSet.empty
                else TaggedOrderedSet.from(proposals)
          )
        )
        CertsMutator.transit(context, state, tx).value
    }

    private def dreps(state: State): Map[Credential, Set[Credential]] =
        state.certState.vstate.dreps.view.mapValues(_.delegates).toMap

    /** Two registered DReps and a registered account with no vote delegation. */
    private val base: State = run(
      State(),
      Seq(
        Certificate.RegDRepCert(drepA, drepDeposit, None),
        Certificate.RegDRepCert(drepB, drepDeposit, None),
        Certificate.RegCert(account, Some(keyDeposit))
      )
    )

    test("spec [BK-1]: registrations add their deposits") {
        base.deposited shouldBe drepDeposit + drepDeposit + keyDeposit
    }

    test("spec [BK-1]: an unregistration subtracts the deposit of the state before CERTS") {
        val after = run(base, Seq(Certificate.UnregCert(account, Some(keyDeposit))))
        after.deposited shouldBe drepDeposit + drepDeposit
    }

    test("spec [BK-1]: a DRep unregistration subtracts the DRep deposit") {
        val after = run(base, Seq(Certificate.UnregDRepCert(drepA, drepDeposit)))
        after.deposited shouldBe drepDeposit + keyDeposit
    }

    test("spec [BK-1]: register and unregister in one tx leave deposited as it was") {
        val other = Credential.KeyHash(AddrKeyHash.fromHex("2" * 56))
        val after = run(
          base,
          Seq(
            Certificate.RegCert(other, Some(keyDeposit)),
            Certificate.UnregCert(other, Some(keyDeposit))
          )
        )
        after.deposited shouldBe base.deposited
    }

    test("spec [BK-2]: a tx with a proposal and no certificate adds the proposal deposit") {
        val proposal = ProposalProcedure(
          deposit = Coin(params.govActionDeposit),
          rewardAccount = RewardAccount(
            StakeAddress(Network.Testnet, StakePayload.Stake(StakeKeyHash.fromHex("1" * 56)))
          ),
          govAction = GovAction.InfoAction,
          anchor = Anchor("https://example.com", DataHash.fromHex("0" * 64))
        )
        val after = run(base, Seq.empty, Seq(proposal))
        after.deposited shouldBe base.deposited + Coin(params.govActionDeposit)
    }

    test("spec [BK-3]: a vote delegation adds the account to the DRep") {
        val after = run(base, Seq(Certificate.VoteDelegCert(account, asDRep(drepA))))
        dreps(after) shouldBe Map(drepA -> Set(account), drepB -> Set.empty)
    }

    test("spec [BK-3]: a vote delegation to a script DRep adds the account") {
        val registered = run(base, Seq(Certificate.RegDRepCert(scriptDRep, drepDeposit, None)))
        val after = run(registered, Seq(Certificate.VoteDelegCert(account, asDRep(scriptDRep))))
        dreps(after)(scriptDRep) shouldBe Set(account)
    }

    test("spec [BK-3]: a re-delegation moves the account to the new DRep") {
        val delegated = run(base, Seq(Certificate.VoteDelegCert(account, asDRep(drepA))))
        val after = run(delegated, Seq(Certificate.VoteDelegCert(account, asDRep(drepB))))
        dreps(after) shouldBe Map(drepA -> Set.empty, drepB -> Set(account))
    }

    test("spec [BK-3]: a delegation to the same DRep changes nothing") {
        val delegated = run(base, Seq(Certificate.VoteDelegCert(account, asDRep(drepA))))
        val after = run(delegated, Seq(Certificate.VoteDelegCert(account, asDRep(drepA))))
        dreps(after) shouldBe dreps(delegated)
    }

    test("spec [BK-3]: a delegation to AlwaysAbstain removes the account from the old DRep") {
        val delegated = run(base, Seq(Certificate.VoteDelegCert(account, asDRep(drepA))))
        val after = run(delegated, Seq(Certificate.VoteDelegCert(account, DRep.AlwaysAbstain)))
        dreps(after) shouldBe Map(drepA -> Set.empty, drepB -> Set.empty)
    }

    test("spec [BK-3]: an unregistration removes the account from its DRep") {
        val delegated = run(base, Seq(Certificate.VoteDelegCert(account, asDRep(drepA))))
        val after = run(delegated, Seq(Certificate.UnregCert(account, Some(keyDeposit))))
        dreps(after) shouldBe Map(drepA -> Set.empty, drepB -> Set.empty)
    }

    test("spec [BK-3]: a registration with vote delegation adds the account") {
        val other = Credential.KeyHash(AddrKeyHash.fromHex("2" * 56))
        val after =
            run(base, Seq(Certificate.VoteRegDelegCert(other, asDRep(drepB), keyDeposit)))
        dreps(after) shouldBe Map(drepA -> Set.empty, drepB -> Set(other))
    }

    test("spec [BK-3]: a delegation then the DRep's unregistration in one tx clear both") {
        val after = run(
          base,
          Seq(
            Certificate.VoteDelegCert(account, asDRep(drepA)),
            Certificate.UnregDRepCert(drepA, drepDeposit)
          )
        )
        dreps(after) shouldBe Map(drepB -> Set.empty)
        after.certState.dstate.accounts(account).dRepDelegation shouldBe None
    }
}
