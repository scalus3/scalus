package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite

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

    test("an empty argument list and a single map still pick the accounts forms") {
        assert(DelegationState() == DelegationState.empty)
        val accounts: DelegationState = DelegationState(Map.empty)
        assert(accounts == DelegationState.empty)
    }
}
