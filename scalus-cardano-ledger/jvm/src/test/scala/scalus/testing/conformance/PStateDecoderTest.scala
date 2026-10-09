package scalus.testing.conformance

import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.testing.conformance.LedgerState.PState
import scalus.utils.Hex

class PStateDecoderTest extends AnyFunSuite {

    private def decode(hex: String): PState = Cbor.decode(Hex.hexToBytes(hex)).to[PState].value

    // PState of `UTXOS.Conway features fail in Plutusdescribe v1 and v2.Certificates.Translated
    // .UnRegDepositTxCert.V1/4.json` (IT vectors): the current cardano-ledger layout
    // [psVRFKeyHashes, psStakePools, psFutureStakePoolParams, psRetiring],
    // with one registered pool whose StakePoolState carries a 500000000 deposit.
    private val currentLayout =
        "84a0a1581c05274248430b63b14904ada7edf63a8291a165f01204dbabced36e418a5820420d803e20b939" +
            "9245706e3c572370fa051107c215ef1cbff2fa045c2c134ebf1a0271e8f21a1653ce04d81e8200018200" +
            "581c9e255991108cd1ff30add9f4c0779e7a8df41e1409bb674f12fb3bf8d901028080801a1dcd6500d9" +
            "0102818200581cf7fb447d7320c8dc5efab99419b81fa74ecfb1629801c8d62a1958dda0a0"

    // PState of `Conway.Imp.AlonzoImpSpec.UTXOW.Invalid transactions.PlutusV1.Extra Redeemer
    // .Multiple equal plutus-locked certs/3` (JVM vectors): the older layout
    // [psStakePoolParams, psFutureStakePoolParams, psRetiring, psDeposits].
    private val olderLayout =
        "84a1581c9b062c24a47d0cbacb34d18668c1b68d35385575da55c6e86a7b22f689581c9b062c24a47d0cba" +
            "cb34d18668c1b68d35385575da55c6e86a7b22f65820551ca97976e146c87cd36e8c7e6a5d3b809f0b16" +
            "db4589b04edc58a507dfe0351a00784bd21a17255414d81e820001581de07de35556f76e5595c01b0eb8" +
            "83ad7e68e04c1d6b9b88916036f445ced901028080f6a0a0a1581c9b062c24a47d0cbacb34d18668c1b6" +
            "8d35385575da55c6e86a7b22f61a1dcd6500"

    test("a pool in psStakePools of the current layout is a registered pool") {
        val pool = PoolKeyHash.fromHex("05274248430b63b14904ada7edf63a8291a165f01204dbabced36e41")
        val pstate = decode(currentLayout)
        assert(pstate.stakePools.keySet == Set(pool))
        assert(
          pstate.stakePools(pool).vrfKeyHash == VrfKeyHash.fromHex(
            "420d803e20b9399245706e3c572370fa051107c215ef1cbff2fa045c2c134ebf"
          )
        )
        assert(pstate.deposits == Map(pool -> Coin(500000000)))
        assert(pstate.futureStakePoolParams.isEmpty)
        assert(pstate.retiring.isEmpty)
    }

    test("a pool in psStakePoolParams of the older layout is a registered pool") {
        val pool = PoolKeyHash.fromHex("9b062c24a47d0cbacb34d18668c1b68d35385575da55c6e86a7b22f6")
        val pstate = decode(olderLayout)
        assert(pstate.stakePools.keySet == Set(pool))
        assert(pstate.deposits == Map(pool -> Coin(500000000)))
        assert(pstate.futureStakePoolParams.isEmpty)
        assert(pstate.retiring.isEmpty)
    }
}
