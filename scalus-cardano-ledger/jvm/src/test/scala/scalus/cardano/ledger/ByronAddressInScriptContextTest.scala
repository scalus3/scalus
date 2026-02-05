package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.compile
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.address.*
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.TxBuilder
import scalus.cardano.txbuilder.TxBuilderException.BuildStepException
import scalus.testing.kit.Party.Alice
import scalus.testing.kit.TestUtil

class ByronAddressInScriptContextTest extends AnyFunSuite {

    given testEnv: CardanoInfo = TestUtil.testEnvironment
    val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    private val alwaysSucceedsScript = {
        import scalus.*
        val program = compile((data: Data) => ()).toUplc(true).plutusV3
        Script.PlutusV3(program.cborByteString)
    }
    private val scriptAddress =
        Address(testEnv.network, Credential.ScriptHash(alwaysSucceedsScript.scriptHash))

    // Known Byron b58 address
    private val byronAddress = ByronAddress
        .fromBase58(
          "Ae2tdPwUPEZDoUnyXuAgqzhkjNXNJeiZ5nqwprg9sArZmRNjySfJ5uz4FjB"
        )
        .get

    test("TxBuilder rejects does not allow to assemble transactions with Byron addresses") {
        val aliceInput = Input(genesisHash, 0)
        val byronInput = Input(genesisHash, 1)
        val scriptInput = Input(genesisHash, 2)
        val datum = Data.unit

        val aliceAddress = Alice.address(Network.Mainnet)
        val initialUtxos = Map(
          aliceInput -> Output(aliceAddress, Value.ada(100)),
          byronInput -> Output(byronAddress, Value.ada(50)),
          scriptInput -> Output(scriptAddress, Value.ada(25), inlineDatum = datum)
        )

        assertThrows[BuildStepException] {
            TxBuilder(testEnv)
                .spend(Utxo(byronInput, Output(byronAddress, Value.ada(50))))
                .spend(
                  Utxo(scriptInput, Output(scriptAddress, Value.ada(25), inlineDatum = datum)),
                  Data.unit,
                  alwaysSucceedsScript
                )
                .payTo(aliceAddress, Value.ada(70))
                .complete(initialUtxos, aliceAddress)
                .transaction
        }
    }

    test("Emulator rejects transaction with Byron output alongside script input") {
        // When a transaction spends from a script and outputs to a Byron address,
        // it should fail because the output cannot be translated to Plutus Address format.

        val scriptInput = Input(genesisHash, 0)
        val collateralInput = Input(genesisHash, 1)
        val datum = Data.unit

        val initialUtxos = Map(
          scriptInput -> Output(scriptAddress, Value.ada(250), inlineDatum = datum),
          collateralInput -> Output(Alice.address(Network.Mainnet), Value.ada(250))
        )

        val emulator = Emulator(initialUtxos = initialUtxos)

        val redeemers = Redeemers(
          Redeemer(
            tag = RedeemerTag.Spend,
            index = 0,
            data = Data.unit,
            exUnits = ExUnits(14_000_000, 10_000_000_000L)
          )
        )

        val witnessSet = TransactionWitnessSet(
          scripts = Seq(alwaysSucceedsScript),
          redeemers = Some(redeemers),
          vkeyWitnesses = Set.empty,
          plutusData = Seq.empty
        )

        val scriptDataHash = ScriptDataHashGenerator.computeScriptDataHash(
          witnessSet,
          testEnv.protocolParams,
          scala.collection.immutable.TreeSet(Language.PlutusV3),
          witnessSet.redeemers,
          witnessSet.plutusData
        )

        val unsignedTx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet(scriptInput),
            collateralInputs = TaggedSortedSet(collateralInput),
            outputs = IndexedSeq(
              Sized(Output(byronAddress, Value.ada(245)))
            ),
            fee = Coin.ada(5),
            donation = Some(Coin.zero),
            currentTreasuryValue = Some(Coin.zero),
            scriptDataHash = scriptDataHash
          ),
          witnessSet = witnessSet
        )

        val tx = Alice.signer.sign(unsignedTx)

        val result = emulator.submitSync(tx)

        assert(result.isLeft, s"Transaction should fail with Byron output: $result")
        val message = result.left.get.message
        assert(
          message == "Error during Plutus script evaluation: Byron addresses not supported in script contexts"
        )
    }
}
