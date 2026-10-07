package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.utils.Hex

class ConwayAccountStateTest extends AnyFunSuite {

    private val pool = PoolKeyHash.fromHex("2" * 56)
    private val drep = DRep.KeyHash(AddrKeyHash.fromHex("3" * 56))

    // spec [SC-18]: the Haskell encoding is [balance, deposit, null | pool, null | drep]
    // (Cardano.Ledger.Conway.State.Account, EncCBOR ConwayAccountState).
    test("an account with no delegation encodes both delegations as null") {
        val account = ConwayAccountState(Coin(7), Coin(2), None, None)
        assert(Hex.bytesToHex(account.toCbor) == "840702f6f6")
        assert(ConwayAccountState.fromCbor(Hex.hexToBytes("840702f6f6")) == account)
    }

    // spec [SC-18]
    test("an account delegated to a pool and a DRep encodes both delegations") {
        val account = ConwayAccountState(Coin(7), Coin(2), Some(pool), Some(drep))
        val hex = "840702581c" + "2" * 56 + "8200581c" + "3" * 56
        assert(Hex.bytesToHex(account.toCbor) == hex)
        assert(ConwayAccountState.fromCbor(Hex.hexToBytes(hex)) == account)
    }
}
