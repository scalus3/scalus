package scalus.testing.conformance

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.testing.conformance.LedgerState.*

class LedgerStateComparisonTest extends AnyFunSuite {

    private val credential = Credential.KeyHash(AddrKeyHash.fromHex("a" * 56))
    private val pool = PoolKeyHash.fromHex("b" * 56)

    private val account = ConwayAccountState(
      balance = Coin(7),
      deposit = Coin(2),
      stakePoolDelegation = Some(pool),
      dRepDelegation = Some(DRep.AlwaysAbstain)
    )

    private val expected = LedgerState(
      certs = LedgerState.CertState.empty.copy(
        dstate = LedgerState.DState.empty.copy(accounts = Map(credential -> account))
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
                dstate.copy(rewards = dstate.rewards.updated(credential, Coin(0)))
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
              e.removedBy == LedgerStateComparison.NoStep ||
                  e.removedBy.matches("""13\.\d+[a-z]? \[SC-[0-9a-z]+\]"""),
              e.toString
            )
    }
}
