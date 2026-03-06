package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.{ScriptSource, TwoArgumentPlutusScriptWitness, TxBuilder}
import scalus.testing.kit.Party.Alice
import scalus.uplc.PlutusV3

class EmulatorJsTest extends AnyFunSuite {

    given testEnv: CardanoInfo = CardanoInfo.mainnet
    val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    test("Emulator.withRegisteredStakeCredentials allows zero-withdrawal without registration tx") {
        val alwaysOkScript = PlutusV3.alwaysOk
        val scriptHash = alwaysOkScript.script.scriptHash
        val stakeCred = Credential.ScriptHash(scriptHash)
        val stakeAddress =
            StakeAddress(Network.Mainnet, StakePayload.Script(scriptHash))
        val witness = TwoArgumentPlutusScriptWitness(
          ScriptSource.PlutusScriptValue(alwaysOkScript.script),
          Data.unit
        )
        val alice = Alice.address(Network.Mainnet)
        val initialUtxos = Map(
          Input(genesisHash, 0) -> Output(alice, Value.ada(5000))
        )
        val emulator = Emulator.withRegisteredStakeCredentials(
          initialUtxos = initialUtxos,
          initialStakeRewards = Map(stakeCred -> Coin.zero)
        )
        val tx = TxBuilder(testEnv)
            .withdrawRewards(stakeAddress, Coin.zero, witness)
            .complete(initialUtxos, alice)
            .sign(Alice.signer)
            .transaction
        val result = emulator.submitSync(tx)
        assert(
          result.isRight,
          s"Zero-withdrawal should succeed with pre-registered credential: $result"
        )
    }

    test("Emulator.withRegisteredStakeCredentials pre-populates certState correctly") {
        val alwaysOkScript = PlutusV3.alwaysOk
        val scriptHash = alwaysOkScript.script.scriptHash
        val stakeCred = Credential.ScriptHash(scriptHash)
        val alice = Alice.address(Network.Mainnet)
        val initialUtxos = Map(
          Input(genesisHash, 0) -> Output(alice, Value.ada(5000))
        )
        val initialReward = Coin.ada(42L)
        val emulator = Emulator.withRegisteredStakeCredentials(
          initialUtxos = initialUtxos,
          initialStakeRewards = Map(stakeCred -> initialReward)
        )
        val cs = emulator.certState
        assert(
          cs.dstate.rewards.get(stakeCred).contains(initialReward),
          s"rewards should contain stake credential with expected amount: ${cs.dstate.rewards}"
        )
        val expectedDeposit = Coin(testEnv.protocolParams.stakeAddressDeposit)
        assert(
          cs.dstate.deposits.get(stakeCred).contains(expectedDeposit),
          s"deposits should contain stake credential with protocol deposit: ${cs.dstate.deposits}"
        )
    }
}
