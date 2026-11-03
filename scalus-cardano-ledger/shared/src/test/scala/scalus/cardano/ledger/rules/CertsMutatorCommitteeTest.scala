package scalus.cardano.ledger.rules

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.CommitteeAuthorization.{HotCredential, Resigned}
import scalus.cardano.ledger.TransactionException.{CommitteeHasPreviouslyResignedException, CommitteeIsUnknownException}

/** The committee certificates of the Conway GOVCERT rule, spec [GC-1] to [GC-4], [GC-2a]
  * (eras/conway/impl/src/Cardano/Ledger/Conway/Rules/GovCert.hs:193-211, 276-279).
  *
  * The committee has one member, `member`. `future` is added by a pending `UpdateCommittee`
  * proposal, and `stranger` is neither.
  */
class CertsMutatorCommitteeTest extends AnyFunSuite with Matchers with EitherValues {

    private val cardanoInfo = CardanoInfo.preprod
    private val params = cardanoInfo.protocolParams

    private def keyCredential(hex: String) = Credential.KeyHash(AddrKeyHash.fromHex(hex * 28))

    private val member = keyCredential("c0")
    private val future = keyCredential("c2")
    private val stranger = keyCredential("c4")
    private val hot = keyCredential("d0")
    private val otherHot = keyCredential("d1")
    private val anchor = Anchor("https://example.com", DataHash.fromHex("0" * 64))

    private val returnAccount = RewardAccount(
      StakeAddress(Network.Testnet, StakePayload.Stake(StakeKeyHash.fromHex("e" * 56)))
    )

    private def addFuture(prev: Option[GovActionId]) =
        GovAction.UpdateCommittee(prev, Set.empty, Map(future -> 200L), UnitInterval(1, 2))

    private val committee = Committee(Map(member -> 100L), UnitInterval(1, 2))

    private val pendingAddition = GovActionState(
      GovActionId(TransactionHash.fromHex("1" * 64), 0),
      Map.empty,
      Map.empty,
      Map.empty,
      ProposalProcedure(Coin(params.govActionDeposit), returnAccount, addFuture(None), anchor),
      0L,
      6L
    )

    private val govState =
        GovState(
          Vector(pendingAddition),
          GovRoots.empty,
          Some(committee),
          GovState.empty.constitution
        )

    private def run(
        certs: Seq[Certificate],
        committeeState: Map[Credential, CommitteeAuthorization] = Map.empty,
        gov: GovState = govState,
        proposals: Seq[ProposalProcedure] = Seq.empty
    ) = {
        val certState = CertState.empty.copy(vstate = VotingState(Map.empty, 0L, committeeState))
        val body = TransactionBody(
          inputs = TaggedSortedSet.empty,
          outputs = IndexedSeq.empty,
          fee = Coin.zero,
          certificates = TaggedOrderedStrictSet.from(certs),
          proposalProcedures =
              if proposals.isEmpty then TaggedOrderedSet.empty
              else TaggedOrderedSet.from(proposals)
        )
        val context = new Context(
          env = UtxoEnv(
            slot = 0,
            params = params,
            certState = certState,
            network = Network.Testnet,
            treasury = Coin.zero
          ),
          slotConfig = cardanoInfo.slotConfig
        )
        val tx = Transaction(body)
        CertsMutator
            .transit(context, State(certState = certState, govState = gov), tx)
            .map(_.certState.vstate.committeeState)
    }

    private def auth(cold: Credential, hotKey: Credential = hot) =
        Certificate.AuthCommitteeHotCert(cold, hotKey)

    private def resign(cold: Credential) = Certificate.ResignCommitteeColdCert(cold, Some(anchor))

    test("a member authorizes a hot key, spec [GC-3]") {
        run(Seq(auth(member))).value shouldBe Map(member -> HotCredential(hot))
    }

    test("a new authorization overwrites the old hot key, spec [GC-3]") {
        run(Seq(auth(member, otherHot)), Map(member -> HotCredential(hot))).value shouldBe
            Map(member -> HotCredential(otherHot))
    }

    test("a member resigns, spec [GC-3]") {
        run(Seq(resign(member)), Map(member -> HotCredential(hot))).value shouldBe
            Map(member -> Resigned(Some(anchor)))
    }

    test("a resigned member can not authorize a hot key or resign again, spec [GC-1]") {
        val resigned = Map[Credential, CommitteeAuthorization](member -> Resigned(None))
        for cert <- Seq(auth(member), resign(member)) do
            run(Seq(cert), resigned).left.value match
                case e: CommitteeHasPreviouslyResignedException => e.coldCredential shouldBe member
                case other                                      => fail(s"unexpected $other")
    }

    test("the certificates of one tx apply in order, spec [GC-1], [GC-3]") {
        run(Seq(auth(member), resign(member))).value shouldBe Map(member -> Resigned(Some(anchor)))
        run(Seq(resign(member), auth(member))).left.value shouldBe a[
          CommitteeHasPreviouslyResignedException
        ]
    }

    test("an unknown cold credential is rejected, spec [GC-2]") {
        for cert <- Seq(auth(stranger), resign(stranger)) do
            run(Seq(cert)).left.value match
                case e: CommitteeIsUnknownException => e.coldCredential shouldBe stranger
                case other                          => fail(s"unexpected $other")
    }

    test("with no committee only a pending UpdateCommittee makes a member known, spec [GC-2]") {
        val noCommittee = govState.copy(committee = None)
        run(Seq(auth(member)), gov = noCommittee).left.value shouldBe a[CommitteeIsUnknownException]
        run(Seq(auth(future)), gov = noCommittee).value shouldBe Map(future -> HotCredential(hot))
    }

    test("a member added by a pending UpdateCommittee can authorize a hot key, spec [GC-2]") {
        run(Seq(auth(future))).value shouldBe Map(future -> HotCredential(hot))
        run(Seq(auth(future)), gov = govState.copy(proposals = Vector.empty)).left.value shouldBe
            a[CommitteeIsUnknownException]
    }

    test("an UpdateCommittee proposed in the same tx does not make a member known, spec [GC-2a]") {
        val sameTx = ProposalProcedure(
          Coin(params.govActionDeposit),
          returnAccount,
          addFuture(None),
          anchor
        )
        run(
          Seq(auth(future)),
          gov = govState.copy(proposals = Vector.empty),
          proposals = Seq(sameTx)
        ).left.value shouldBe a[CommitteeIsUnknownException]
    }

    test("a resigned cold credential that is unknown fails as resigned first, spec [GC-4]") {
        run(Seq(auth(stranger)), Map(stranger -> Resigned(None))).left.value match
            case e: CommitteeHasPreviouslyResignedException => e.coldCredential shouldBe stranger
            case other                                      => fail(s"unexpected $other")
    }

    test("ruleName returns the ledger names, spec [GC-4]") {
        val txId = TransactionHash.fromHex("2" * 64)
        TransactionException.ruleName(
          CommitteeHasPreviouslyResignedException(txId, member)
        ) shouldBe "ConwayCommitteeHasPreviouslyResigned"
        TransactionException.ruleName(CommitteeIsUnknownException(txId, member)) shouldBe
            "ConwayCommitteeIsUnknown"
    }
}
