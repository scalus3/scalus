package scalus.cardano.txbuilder

import org.scalatest.funsuite.AnyFunSuite
import scalus.Compiler
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.Address
import scalus.cardano.address.Address.{addr, stake}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.TestPeer.{Alice, Bob}
import scalus.uplc.PlutusV3
import scalus.uplc.eval.PlutusVM
import scalus.uplc.transform.V3Optimizer
import scalus.utils.await

import java.time.{Duration, Instant}
import scala.concurrent.ExecutionContext.Implicits.global
given Compiler.Options = Compiler.Options.default

class TxBuilderDemo extends AnyFunSuite {

    val env: CardanoInfo = CardanoInfo.mainnet
    // Common test values
    val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    // Helper methods for creating UTXOs
    def input(index: Int): TransactionInput = TransactionInput(genesisHash, index)
    def adaOutput(address: Address, ada: Int): TransactionOutput =
        TransactionOutput(address, Value.ada(ada))

    val alwaysOkScript: Script.PlutusV3 = {
        val alwaysOk = PlutusV3.compile((sc: Data) => ())
        alwaysOk.script
    }

    val emulator = Emulator(
      Map(
        input(0) -> adaOutput(Alice.address, 100),
        input(1) -> adaOutput(Alice.address, 50),
        input(2) -> adaOutput(Alice.address, 50)
      )
    )

    test("Building a simple transaction with `complete()`") {
        // Simple tx, using magic `complete()`
        val tx = TxBuilder(env)
            .payTo(Bob.address, Value.ada(10))
            // Magic: sets inputs, collateral input/output, execution budgets,
            // fee, handle change, etc.
            .complete(provider = emulator, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

//        pprintln(tx.body.value)
//        pprintln(tx.witnessSet)
    }

    test("Building a simple transaction with full control") {
        val utxo = Utxo(emulator.utxos.head)
        val now = Instant.now()

        val token = Value.asset(
          alwaysOkScript.scriptHash,
          AssetName.fromString("Beacon Token"),
          1
        )
        // Value and Coin arithmetic
        val amount = Value.ada(10) + token - Value.ada(10) - token
        val coin = Coin.ada(2) + Coin(123456) - Coin(1)

//        println(s"token   = $token")
//        println(s"amount  = $amount")
//        println(s"coin    = $coin")

        // Address bech32 literals
        val Bob = addr"addr1v9qy3lufef8c3en9nrnzp2svwy5vy9zangvp46dy4qw23ccw05pjg"
        val stakeAddress = stake"stake1uyehkck0lajq8gr28t9uxnuvgcqrc6070x3k9r8048z8y5gh6ffgw"

        // Complex Franken address
        val address = Address(
          network = env.network,
          payment = Credential.ScriptHash(alwaysOkScript.scriptHash),
          delegation = Credential.ScriptHash(alwaysOkScript.scriptHash)
        )

//        val collateral = emulator.findUtxo(...)
//        val referenceUtxo = emulator.findUtxo(...)

        val tx = TxBuilder(env)
            .spend(utxo)
//            .collaterals(collateral)
//            .references(referenceUtxo)
            .output(
              TransactionOutput(
                address = Bob,
                value = amount,
                datumOption = Some(DatumOption.Inline(0.toData)),
                scriptRef = Some(ScriptRef(alwaysOkScript))
              )
            )
            .metadata(
              Metadata(
                Map(
                  Word64(674) -> Metadatum.Text("Test metadata"),
                  Word64(1234) -> Metadatum.Int(42)
                )
              )
            )
            // validity interval
            .validFrom(now)
            .validTo(now.plus(Duration.ofHours(2)))
            // use low level intention-based API
            .addSteps(TransactionBuilderStep.Fee(Coin.ada(1)))
            // immutable, pure (effectless) operations
            .build(changeTo = Alice.address)
//            .sign(Alice.signer)
            .transaction

//        pprintln(tx.body.value)
//        pprintln(tx.witnessSet)
//        pprintln(tx.auxiliaryData.get.value)
    }

    test("Building a smart contract transaction") {
        // Simple tx, using magic `complete()`
        val utxo = Utxo(emulator.utxos.head)
        val now = Instant.now()

        // Deploy a smart contract
        val tx = TxBuilder(env)
            .spend(utxo)
            .output(
              TransactionOutput(
                address = Alice.address,
                value = Value.ada(10),
                datumOption = Some(DatumOption.Inline(0.toData)),
                scriptRef = Some(ScriptRef(alwaysOkScript))
              )
            )
            // produces only 1 output, puts the change to it
            .build(changeTo = Alice.address)
            .sign(Alice.signer)
            .transaction

        emulator.submit(tx).await()

//        pprintln(tx.body.value)
//        pprintln(tx.witnessSet)

        val scriptUtxo = Utxo(tx.utxos.head)

        val assets = Map(AssetName.fromString("Beacon Token") -> 1L)

        val mintTx = TxBuilder(env)
            .references(scriptUtxo)
            .mint(
              policyId = alwaysOkScript.scriptHash,
              assets = assets,
              redeemer = ()
            )
            .payTo(
              Bob.address,
              Value.fromPolicy(alwaysOkScript.scriptHash, assets) + Value.ada(2)
            )
            .complete(provider = emulator, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

//        println(s"\nMinting TX")
//        pprintln(mintTx.body.value)
//        pprintln(mintTx.witnessSet)
    }

    test("Applying parameters to scripts") {
        given PlutusVM = PlutusVM.makePlutusV3VM()

        // parameterized validator
        val parameterized = PlutusV3.compile { (config: Data) => (sc: Data) =>
            val a = BigInt(2)
            ()
        }

        // running validator code on JVM (for debugging)
        val param = ("Config", 123).toData
//        println(parameterized.code(param)(().toData))

        // apply a `param` to UPLC program
        val applied = parameterized.program $ param
        val opt = V3Optimizer()
//        println(opt(applied.term).showHighlighted)
        // apply a `scriptContext` argument to UPLC program
        val fully = applied $ ().toData
//        println(fully.evaluateDebug)
    }
}
