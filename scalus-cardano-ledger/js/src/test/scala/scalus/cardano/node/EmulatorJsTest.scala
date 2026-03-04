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
          stakeCredentials = Seq(stakeCred)
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
}
