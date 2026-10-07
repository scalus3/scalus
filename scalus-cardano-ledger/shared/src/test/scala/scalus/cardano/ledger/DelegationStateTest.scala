package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.nowarn

class DelegationStateTest extends AnyFunSuite {

    private val alice = Credential.KeyHash(AddrKeyHash.fromHex("a" * 56))
    private val bob = Credential.KeyHash(AddrKeyHash.fromHex("b" * 56))
    private val pool = PoolKeyHash.fromHex("2" * 56)
    private val drep = DRep.KeyHash(AddrKeyHash.fromHex("3" * 56))

    private val aliceAccount = ConwayAccountState(Coin(7), Coin(2), Some(pool), Some(drep))
    private val bobAccount = ConwayAccountState(Coin(0), Coin(2), None, None)
    private val state = DelegationState(Map(alice -> aliceAccount, bob -> bobAccount))

    // spec [SC-15]
    test("the accounts map holds one record per account") {
        assert(state.accounts == Map(alice -> aliceAccount, bob -> bobAccount))
        assert(DelegationState.empty.accounts.isEmpty)
    }

    @nowarn("cat=deprecation")
    private def oldViews(s: DelegationState) = (s.rewards, s.deposits, s.stakePools, s.dreps)

    @nowarn("cat=deprecation")
    private def fromOldMaps(
        rewards: Map[Credential, Coin],
        deposits: Map[Credential, Coin],
        stakePools: Map[Credential, PoolKeyHash],
        dreps: Map[Credential, DRep]
    ) = DelegationState(rewards, deposits, stakePools, dreps)

    // spec [SC-17]
    test("the deprecated maps are views of the accounts") {
        val (rewards, deposits, stakePools, dreps) = oldViews(state)
        assert(rewards == Map(alice -> Coin(7), bob -> Coin(0)))
        assert(deposits == Map(alice -> Coin(2), bob -> Coin(2)))
        assert(stakePools == Map(alice -> pool))
        assert(dreps == Map(alice -> drep))
    }

    // spec [SC-17a]
    test("the deprecated apply builds one account per registered credential") {
        val built = fromOldMaps(
          Map(alice -> Coin(7), bob -> Coin(0)),
          Map(alice -> Coin(2), bob -> Coin(2)),
          Map(alice -> pool),
          Map(alice -> drep)
        )
        assert(built == state)
    }

    // spec [SC-17a]: with the old maps, `rewards` and `deposits` both marked an account as
    // registered. A credential in either one becomes an account.
    test("the deprecated apply registers a credential found in rewards or deposits only") {
        val built = fromOldMaps(
          Map(alice -> Coin(7)),
          Map(bob -> Coin(2)),
          Map.empty,
          Map.empty
        )
        assert(
          built.accounts == Map(
            alice -> ConwayAccountState(Coin(7), Coin.zero, None, None),
            bob -> ConwayAccountState(Coin.zero, Coin(2), None, None)
          )
        )
    }

    // 1.3 call sites: binary `new DelegationState(4 maps)`, source named arguments with defaults
    @nowarn("cat=deprecation")
    private def viaOldConstructor(
        rewards: Map[Credential, Coin],
        deposits: Map[Credential, Coin],
        stakePools: Map[Credential, PoolKeyHash],
        dreps: Map[Credential, DRep]
    ) = new DelegationState(rewards, deposits, stakePools, dreps)

    test("the deprecated constructor builds the same accounts as the 4-map apply") {
        val built = viaOldConstructor(
          Map(alice -> Coin(7), bob -> Coin(0)),
          Map(alice -> Coin(2), bob -> Coin(2)),
          Map(alice -> pool),
          Map(alice -> drep)
        )
        assert(built == state)
    }

    @nowarn("cat=deprecation")
    private def rewardsOnly = DelegationState(rewards = Map(alice -> Coin(7)))

    @nowarn("cat=deprecation")
    private def depositsThenRewards =
        DelegationState(deposits = Map(bob -> Coin(2)), rewards = Map(alice -> Coin(7)))

    test("the deprecated apply keeps the 1.3 defaults for named arguments") {
        assert(
          rewardsOnly == DelegationState(
            Map(alice -> ConwayAccountState(Coin(7), Coin.zero, None, None))
          )
        )
        assert(
          depositsThenRewards == DelegationState(
            Map(
              alice -> ConwayAccountState(Coin(7), Coin.zero, None, None),
              bob -> ConwayAccountState(Coin.zero, Coin(2), None, None)
            )
          )
        )
    }

    test("an empty argument list and a single map still pick the accounts forms") {
        assert(DelegationState() == DelegationState.empty)
        val accounts: DelegationState = DelegationState(Map.empty)
        assert(accounts == DelegationState.empty)
    }
}
