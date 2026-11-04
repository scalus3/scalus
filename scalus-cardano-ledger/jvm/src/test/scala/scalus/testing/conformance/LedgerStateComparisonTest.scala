package scalus.testing.conformance

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*
import scalus.testing.conformance.VectorLedgerState.*

class LedgerStateComparisonTest extends AnyFunSuite {

    private val credential = Credential.KeyHash(AddrKeyHash.fromHex("a" * 56))
    private val pool = PoolKeyHash.fromHex("b" * 56)

    private val account = ConwayAccountState(
      balance = Coin(7),
      deposit = Coin(2),
      stakePoolDelegation = Some(pool),
      dRepDelegation = Some(DRep.AlwaysAbstain)
    )

    private val expected = VectorLedgerState(
      certs = VectorLedgerState.CertState.empty.copy(
        dstate = VectorLedgerState.DState.empty.copy(accounts = Map(credential -> account))
      ),
      utxos = UTxOState(
        utxo = Map.empty,
        deposited = Coin(2),
        fees = Coin(10),
        govState = GovState.empty,
        stakeDistribution = Map(credential -> Coin(100)),
        donation = Coin(3)
      )
    )

    private def fields(actual: rules.State): List[String] =
        LedgerStateComparison.compare(expected, actual).map(_.field)

    // spec [SC-14]
    test("a state equal to newLedgerState has no mismatch") {
        assert(fields(expected.ruleState) == Nil)
    }

    // spec [SC-14a]
    test("each mismatching top-level field is reported by its name") {
        val actual = expected.ruleState.copy(
          fees = Coin(11),
          deposited = Coin(0),
          donation = Coin(0),
          stakeDistribution = Map.empty
        )
        assert(fields(actual).toSet == Set("fees", "deposited", "donation", "instantStake"))
    }

    // spec [SC-14a]
    test("an account balance mismatch names the account field") {
        val state = expected.ruleState
        val dstate = state.certState.dstate
        val actual = state.copy(certState =
            state.certState.copy(dstate =
                DelegationState(
                  dstate.accounts.updated(credential, account.copy(balance = Coin(0)))
                )
            )
        )
        assert(fields(actual) == List("accounts.balance"))
    }

    // spec [SC-14a]
    test("a deregistered account is reported as an accounts mismatch") {
        val state = expected.ruleState
        val actual = state.copy(certState = state.certState.copy(dstate = DelegationState()))
        assert(fields(actual) == List("accounts"))
    }

    private val anchor = Anchor("https://example.com", DataHash.fromHex("0" * 64))

    private def proposal(n: Int): GovActionState = GovActionState(
      GovActionId(TransactionHash.fromHex(f"$n%064x"), 0),
      committeeVotes = Map.empty,
      drepVotes = Map(credential -> Vote.Yes),
      spoVotes = Map.empty,
      procedure = ProposalProcedure(
        Coin(5),
        RewardAccount(
          StakeAddress(Network.Testnet, StakePayload.Stake(StakeKeyHash.fromHex("e" * 56)))
        ),
        GovAction.InfoAction,
        anchor
      ),
      proposedIn = 900,
      expiresAfter = 906
    )

    private val govState = GovState(
      Vector(proposal(1), proposal(2)),
      GovRoots.empty,
      Some(Committee(Map(credential -> 950L), UnitInterval(1, 2))),
      Constitution(anchor, None)
    )

    private def govFields(actual: GovState): List[String] = {
        val withGov = expected.copy(utxos = expected.utxos.copy(govState = govState))
        LedgerStateComparison
            .compare(withGov, withGov.ruleState.copy(govState = actual))
            .map(_.field)
    }

    // spec [CV-7]
    test("each mismatching part of the gov state is reported by its name") {
        assert(govFields(govState) == Nil)
        val vote =
            govState.withVote(Voter.DRepKey(AddrKeyHash.fromHex("a" * 56)), proposal(1).id, Vote.No)
        assert(govFields(vote) == List("govState.proposals"))
        val root = govState.copy(roots = GovRoots.empty.copy(committee = Some(proposal(1).id)))
        assert(govFields(root) == List("govState.roots"))
        assert(govFields(govState.copy(committee = None)) == List("govState.committee"))
        val constitution = govState.copy(constitution = Constitution(anchor.copy(url = "x"), None))
        assert(govFields(constitution) == List("govState.constitution"))
    }

    // spec [CV-7]: the ledger stores the proposals in the order they were made
    test("the proposals are compared in order") {
        assert(
          govFields(govState.copy(proposals = govState.proposals.reverse)) == List(
            "govState.proposals"
          )
        )
    }

    // spec [CV-7], [GC-3]: the GOVCERT vectors check the authorizations through this comparison
    test("a committee authorization mismatch names committeeState") {
        val hot = Credential.KeyHash(AddrKeyHash.fromHex("c" * 56))
        val authorized = expected.copy(certs =
            expected.certs.copy(vstate =
                VState.empty.copy(committeeState =
                    Map(credential -> CommitteeAuthorization.HotCredential(hot))
                )
            )
        )
        val state = authorized.ruleState
        def mismatches(committeeState: Map[Credential, CommitteeAuthorization]) =
            val actual = state.copy(certState =
                state.certState.copy(vstate =
                    state.certState.vstate.copy(committeeState = committeeState)
                )
            )
            LedgerStateComparison.compare(authorized, actual).map(_.field)
        assert(mismatches(authorized.certs.vstate.committeeState) == Nil)
        assert(
          mismatches(Map(credential -> CommitteeAuthorization.Resigned(None))) == List(
            "committeeState"
          )
        )
        assert(mismatches(Map.empty) == List("committeeState"))
    }

    // spec [CV-3]
    test("the gov state has no exclusion") {
        assert(!LedgerStateComparison.exclusions.exists(_.field.startsWith("govState")))
    }

    // spec [SC-14b]
    test("an exclusion removes only its field, and only in matching vectors") {
        import LedgerStateComparison.Exclusion
        val actual = expected.ruleState.copy(fees = Coin(11), donation = Coin(0))
        val mismatches = LedgerStateComparison.compare(expected, actual)
        val exclusions = List(Exclusion("fees", "13.9", "Withdraw"))
        def kept(vector: String) =
            LedgerStateComparison.withoutExcluded(vector, mismatches, exclusions).map(_.field)
        assert(kept("UTXO.Withdraw twice") == List("donation"))
        assert(kept("UTXO.Mint a Token") == List("fees", "donation"))
    }

    // spec [SC-14c]
    test("each exclusion names the step that removes it") {
        for e <- LedgerStateComparison.exclusions do
            assert(
              e.removedBy.matches("""13\.\d+[a-z]? \[SC-[0-9a-z]+\]""") ||
                  e.removedBy.matches("""never, D\d+( and D\d+)*"""),
              e.toString
            )
    }
}
