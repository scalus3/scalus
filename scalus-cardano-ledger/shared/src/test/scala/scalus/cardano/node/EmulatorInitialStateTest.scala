package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.ledger.epoch.AccountState
import scalus.testing.kit.Party.{Alice, Bob, Charles}
import scalus.uplc.builtin.Data

import scala.annotation.nowarn

/** `EmulatorInitialState` is built from `empty` and one `with` method per field, D121. */
class EmulatorInitialStateTest extends AnyFunSuite {

    private val input = TransactionInput(TransactionHash.fromHex("0" * 64), 0)
    private val output = TransactionOutput(Alice.address(Network.Mainnet), Value.ada(10))
    private val aliceKey = Credential.KeyHash(Alice.addrKeyHash)
    private val coldKey = Credential.KeyHash(Charles.addrKeyHash)
    private val hotKey = Credential.KeyHash(Bob.addrKeyHash)
    private val committee = Committee(Map(coldKey -> 10L), UnitInterval(1, 1))
    private val constitution =
        Constitution(Anchor("https://example.com", DataHash.fromHex("2" * 64)), None)
    private val datum = Data.I(7)
    private val stake = EmulatorStakeRegistration.of(aliceKey)
    private val drep = EmulatorDRepRegistration.of(aliceKey, Coin(500))

    test("empty has no UTxOs, no registrations, no gov state and the mainnet reserves") {
        val e = EmulatorInitialState.empty
        assert(e.utxos.isEmpty)
        assert(e.stakeRegistrations.isEmpty && e.poolRegistrations.isEmpty)
        assert(e.drepRegistrations.isEmpty && e.datums.isEmpty)
        assert(e.committee.isEmpty && e.committeeHotKeys.isEmpty && e.constitution.isEmpty)
        assert(e.reserves == AccountState.mainnet.reserves)
    }

    test("each with method sets its field and keeps the others") {
        val e = EmulatorInitialState.empty
            .withUtxos(Map(input -> output))
            .withStakeRegistrations(Seq(stake))
            .withPoolRegistrations(Seq.empty)
            .withDRepRegistrations(Seq(drep))
            .withDatums(Map(DataHash.fromHex("1" * 64) -> datum))
            .withCommittee(committee)
            .withCommitteeHotKeys(Map(coldKey -> hotKey))
            .withConstitution(constitution)
            .withReserves(Coin(77))
        assert(e.utxos == Map(input -> output))
        assert(e.stakeRegistrations == Seq(stake))
        assert(e.drepRegistrations == Seq(drep))
        assert(e.datums == Map(DataHash.fromHex("1" * 64) -> datum))
        assert(e.committee.contains(committee))
        assert(e.committeeHotKeys == Map(coldKey -> hotKey))
        assert(e.constitution.contains(constitution))
        assert(e.reserves == Coin(77))
        assert(e.withReserves(Coin(1)).utxos == Map(input -> output))
    }

    test("the builder gives the same value as the with methods") {
        val built = EmulatorInitialState
            .builder()
            .putUtxo(input, output)
            .addStakeRegistration(stake)
            .addDRepRegistration(drep)
            .committee(committee)
            .addCommitteeHotKey(coldKey, hotKey)
            .constitution(constitution)
            .reserves(Coin(77))
            .build()
        val withs = EmulatorInitialState.empty
            .withUtxos(Map(input -> output))
            .withStakeRegistrations(Vector(stake))
            .withDRepRegistrations(Vector(drep))
            .withCommittee(committee)
            .withCommitteeHotKeys(Map(coldKey -> hotKey))
            .withConstitution(constitution)
            .withReserves(Coin(77))
        assert(built == withs)
    }

    // The 1.4 apply is deprecated: this test checks it still builds the 1.4 value.
    @nowarn("msg=deprecated")
    private def viaDeprecatedApply: EmulatorInitialState =
        EmulatorInitialState(utxos = Map(input -> output))

    test("the deprecated 1.4 apply keeps its defaults") {
        assert(viaDeprecatedApply == EmulatorInitialState.empty.withUtxos(Map(input -> output)))
    }
}
