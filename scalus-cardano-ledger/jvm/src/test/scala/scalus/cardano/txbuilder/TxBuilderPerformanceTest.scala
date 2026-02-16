package scalus.cardano.txbuilder

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.Value.ada
import scalus.cardano.ledger.rules.ValidatorRulesTestKit
import scalus.cardano.ledger.*
import scalus.cardano.node.Emulator
import scalus.compiler.compileInline
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.toUplc
import scalus.utils.await

/** Performance measurement tests for TxBuilder transaction balancing.
  *
  * These tests measure:
  *   - Transaction build time
  *   - Number of balancing iterations
  *   - Script evaluation overhead
  */
class TxBuilderPerformanceTest extends AnyFunSuite, ValidatorRulesTestKit {

    given testEnv: CardanoInfo = CardanoInfo.mainnet

    val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    val alwaysOkScript: Script.PlutusV3 = {
        val alwaysOk = compileInline((sc: Data) => ())
        val alwaysOkCborBytes = alwaysOk.toUplc().plutusV3.cborByteString
        Script.PlutusV3(alwaysOkCborBytes)
    }

    val policyId: ScriptHash = alwaysOkScript.scriptHash

    val emptyRedeemer: Data = Data.List(PList.Nil)
    val inlineDatum42: DatumOption = Inline(Data.I(42))

    val scriptAddress: ShelleyAddress = ShelleyAddress(
      network = testEnv.network,
      payment = ShelleyPaymentPart.Script(alwaysOkScript.scriptHash),
      delegation = ShelleyDelegationPart.Null
    )

    def input(index: Int): TransactionInput = Input(genesisHash, index)

    def scriptOutput(
        address: ShelleyAddress = scriptAddress,
        ada: Int = 20,
        datum: DatumOption = inlineDatum42
    ): TransactionOutput =
        Output(address, Value.ada(ada), datum)

    def scriptUtxo(
        index: Int,
        address: ShelleyAddress = scriptAddress,
        ada: Int = 20,
        datum: DatumOption = inlineDatum42
    ): Utxo =
        Utxo(input(index), scriptOutput(address, ada, datum))

    /** Measure performance of building a transaction with single script spend */
    test("Performance: single script spend transaction") {
        val sUtxo = scriptUtxo(2)

        val provider = Emulator(
          Map(
            input(0) -> Output(Alice.address, ada(100)),
            input(1) -> Output(Alice.address, ada(50)),
            sUtxo.input -> sUtxo.output
          )
        )

        // Warmup
        for _ <- 1 to 3 do {
            TxBuilder(testEnv)
                .spend(sUtxo, emptyRedeemer, alwaysOkScript)
                .payTo(Bob.address, ada(5))
                .complete(provider, Alice.address)
                .await()
        }

        // Measure
        val iterations = 10
        val times = (1 to iterations).map { _ =>
            val start = System.nanoTime()
            val tx = TxBuilder(testEnv)
                .spend(sUtxo, emptyRedeemer, alwaysOkScript)
                .payTo(Bob.address, ada(5))
                .complete(provider, Alice.address)
                .await()
                .transaction
            val elapsed = (System.nanoTime() - start) / 1_000_000
            (elapsed, tx)
        }

        val avgTime = times.map(_._1).sum / iterations
        val minTime = times.map(_._1).min
        val maxTime = times.map(_._1).max

        println(s"\n=== Single Script Spend Performance ===")
        println(s"Iterations: $iterations")
        println(s"Avg time: ${avgTime}ms")
        println(s"Min time: ${minTime}ms")
        println(s"Max time: ${maxTime}ms")

        assert(avgTime < 5000, s"Average time should be under 5 seconds, was ${avgTime}ms")
    }

    /** Measure performance of building a transaction with minting */
    test("Performance: minting transaction") {
        val assets = Map(AssetName.fromString("token") -> 100L)

        val provider = Emulator(
          Map(
            input(0) -> Output(Alice.address, ada(100)),
            input(1) -> Output(Alice.address, ada(50))
          )
        )

        // Warmup
        for _ <- 1 to 3 do {
            TxBuilder(testEnv)
                .mint(alwaysOkScript, assets, emptyRedeemer)
                .payTo(Bob.address, Value.fromPolicy(policyId, assets, Coin.ada(5)))
                .complete(provider, Alice.address)
                .await()
        }

        // Measure
        val iterations = 10
        val times = (1 to iterations).map { _ =>
            val start = System.nanoTime()
            val tx = TxBuilder(testEnv)
                .mint(alwaysOkScript, assets, emptyRedeemer)
                .payTo(Bob.address, Value.fromPolicy(policyId, assets, Coin.ada(5)))
                .complete(provider, Alice.address)
                .await()
                .transaction
            val elapsed = (System.nanoTime() - start) / 1_000_000
            (elapsed, tx)
        }

        val avgTime = times.map(_._1).sum / iterations
        val minTime = times.map(_._1).min
        val maxTime = times.map(_._1).max

        println(s"\n=== Minting Transaction Performance ===")
        println(s"Iterations: $iterations")
        println(s"Avg time: ${avgTime}ms")
        println(s"Min time: ${minTime}ms")
        println(s"Max time: ${maxTime}ms")

        assert(avgTime < 5000, s"Average time should be under 5 seconds, was ${avgTime}ms")
    }

    /** Measure performance of transaction with both spending and minting scripts */
    test("Performance: spend and mint transaction (2 scripts)") {
        val sUtxo = scriptUtxo(2)
        val assets = Map(AssetName.fromString("token") -> 100L)

        val provider = Emulator(
          Map(
            input(0) -> Output(Alice.address, ada(100)),
            input(1) -> Output(Alice.address, ada(50)),
            sUtxo.input -> sUtxo.output
          )
        )

        // Warmup
        for _ <- 1 to 3 do {
            TxBuilder(testEnv)
                .spend(sUtxo, emptyRedeemer, alwaysOkScript)
                .mint(alwaysOkScript, assets, emptyRedeemer)
                .payTo(Bob.address, Value.fromPolicy(policyId, assets, Coin.ada(5)))
                .complete(provider, Alice.address)
                .await()
        }

        // Measure
        val iterations = 10
        val times = (1 to iterations).map { _ =>
            val start = System.nanoTime()
            val tx = TxBuilder(testEnv)
                .spend(sUtxo, emptyRedeemer, alwaysOkScript)
                .mint(alwaysOkScript, assets, emptyRedeemer)
                .payTo(Bob.address, Value.fromPolicy(policyId, assets, Coin.ada(5)))
                .complete(provider, Alice.address)
                .await()
                .transaction
            val elapsed = (System.nanoTime() - start) / 1_000_000
            (elapsed, tx)
        }

        val avgTime = times.map(_._1).sum / iterations
        val minTime = times.map(_._1).min
        val maxTime = times.map(_._1).max

        println(s"\n=== Spend + Mint Transaction Performance (2 scripts) ===")
        println(s"Iterations: $iterations")
        println(s"Avg time: ${avgTime}ms")
        println(s"Min time: ${minTime}ms")
        println(s"Max time: ${maxTime}ms")

        val finalTx = times.last._2
        println(s"Final fee: ${finalTx.body.value.fee}")
        println(s"Number of inputs: ${finalTx.body.value.inputs.toSeq.size}")
        println(s"Number of outputs: ${finalTx.body.value.outputs.size}")
        println(
          s"Number of redeemers: ${finalTx.witnessSet.redeemers.map(_.value.toSeq.size).getOrElse(0)}"
        )

        assert(avgTime < 5000, s"Average time should be under 5 seconds, was ${avgTime}ms")
    }

    /** Measure performance with multiple UTXOs requiring coin selection */
    test("Performance: coin selection with many UTXOs") {
        // Create many small UTXOs to stress coin selection
        val manyUtxos = (0 until 50).map { i =>
            input(i) -> Output(Alice.address, ada(10 + i))
        }.toMap

        val provider = Emulator(manyUtxos)

        // Warmup
        for _ <- 1 to 3 do {
            TxBuilder(testEnv)
                .payTo(Bob.address, ada(200))
                .complete(provider, Alice.address)
                .await()
        }

        // Measure
        val iterations = 10
        val times = (1 to iterations).map { _ =>
            val start = System.nanoTime()
            val tx = TxBuilder(testEnv)
                .payTo(Bob.address, ada(200))
                .complete(provider, Alice.address)
                .await()
                .transaction
            val elapsed = (System.nanoTime() - start) / 1_000_000
            (elapsed, tx)
        }

        val avgTime = times.map(_._1).sum / iterations
        val minTime = times.map(_._1).min
        val maxTime = times.map(_._1).max

        println(s"\n=== Coin Selection Performance (50 UTXOs) ===")
        println(s"Iterations: $iterations")
        println(s"Avg time: ${avgTime}ms")
        println(s"Min time: ${minTime}ms")
        println(s"Max time: ${maxTime}ms")

        val finalTx = times.last._2
        println(s"Number of inputs selected: ${finalTx.body.value.inputs.toSeq.size}")

        assert(avgTime < 5000, s"Average time should be under 5 seconds, was ${avgTime}ms")
    }

    /** Count the number of balancing iterations by tracking evaluator calls */
    test("Measure balancing iterations for script transaction") {
        val sUtxo = scriptUtxo(2)

        val availableUtxos: Utxos = Map(
          input(0) -> Output(Alice.address, ada(100)),
          input(1) -> Output(Alice.address, ada(50)),
          sUtxo.input -> sUtxo.output
        )

        // Create a custom evaluator that counts calls
        var evaluationCount = 0
        val countingEvaluator = new PlutusScriptEvaluator {
            private val delegate =
                PlutusScriptEvaluator(testEnv, EvaluatorMode.EvaluateAndComputeCost)
            override def evalPlutusScriptsWithContexts(
                tx: Transaction,
                utxos: Utxos,
                debugScripts: Map[ScriptHash, scalus.uplc.CompiledPlutus[?]]
            ): Seq[(Redeemer, scalus.cardano.onchain.plutus.ScriptContext, ScriptHash)] = {
                evaluationCount += 1
                delegate.evalPlutusScriptsWithContexts(tx, utxos, debugScripts)
            }
        }

        val builder = TxBuilder(testEnv, countingEvaluator)
            .spend(sUtxo, emptyRedeemer, alwaysOkScript)
            .payTo(Bob.address, ada(5))
            .complete(availableUtxos, Alice.address)

        val tx = builder.transaction

        println(s"\n=== Balancing Iterations Measurement ===")
        println(s"Script evaluation count (balancing iterations): $evaluationCount")
        println(s"Final fee: ${tx.body.value.fee}")
        println(s"Number of inputs: ${tx.body.value.inputs.toSeq.size}")
        println(s"Number of outputs: ${tx.body.value.outputs.size}")

        // Current behavior: 2 balancing iterations for script transactions
        // (Optimized from 3 by skipping collateral return for small collateral ≤5 ADA)
        assert(
          evaluationCount == 2,
          s"Expected 2 balancing iterations (current baseline), had $evaluationCount"
        )
    }

    /** Baseline comparison: simple ADA transfer without scripts */
    test("Baseline: simple ADA transfer performance") {
        val provider = Emulator(
          Map(
            input(0) -> Output(Alice.address, ada(100)),
            input(1) -> Output(Alice.address, ada(50))
          )
        )

        // Warmup
        for _ <- 1 to 3 do {
            TxBuilder(testEnv)
                .payTo(Bob.address, ada(10))
                .complete(provider, Alice.address)
                .await()
        }

        // Measure
        val iterations = 10
        val times = (1 to iterations).map { _ =>
            val start = System.nanoTime()
            val tx = TxBuilder(testEnv)
                .payTo(Bob.address, ada(10))
                .complete(provider, Alice.address)
                .await()
                .transaction
            val elapsed = (System.nanoTime() - start) / 1_000_000
            (elapsed, tx)
        }

        val avgTime = times.map(_._1).sum / iterations
        val minTime = times.map(_._1).min
        val maxTime = times.map(_._1).max

        println(s"\n=== Baseline: Simple ADA Transfer Performance ===")
        println(s"Iterations: $iterations")
        println(s"Avg time: ${avgTime}ms")
        println(s"Min time: ${minTime}ms")
        println(s"Max time: ${maxTime}ms")

        assert(avgTime < 1000, s"Simple transfer should be under 1 second, was ${avgTime}ms")
    }
}
