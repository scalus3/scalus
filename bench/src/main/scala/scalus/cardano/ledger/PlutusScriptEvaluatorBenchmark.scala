package scalus.cardano.ledger

import org.openjdk.jmh.annotations.*
import scalus.bloxbean.ResourcesUtxoResolver
import scalus.builtin.Data
import scalus.cardano.ledger.*
import scalus.uplc.eval.ExBudget

import java.util.concurrent.TimeUnit
import scala.annotation.nowarn
import scala.util.Try

/** Benchmark for PlutusScriptEvaluator performance evaluation.
  *
  * This benchmark measures the performance of evaluating Plutus scripts in real Cardano
  * transactions from mainnet blocks. It uses block data from test resources to ensure realistic
  * benchmarking scenarios.
  *
  * The benchmark focuses on measuring TxInfo construction and script evaluation overhead,
  * specifically testing the optimization where TxInfo is computed once per transaction/Plutus
  * version instead of once per redeemer.
  *
  * **Implementation details:**
  *   - The PlutusScriptEvaluator is initialized once with mainnet parameters and reused across all
  *     benchmark iterations for better performance and more accurate measurements
  *   - Block data is loaded from test resources using the classpath
  *   - Transactions are pre-filtered to include only valid transactions with redeemers
  *   - UTxOs are resolved using ResourcesUtxoResolver from test resources
  *
  * **Prerequisites:**
  *   - Block CBOR files must exist in test resources at `/blocks/block-{blockNumber}.cbor`
  *   - UTxO CBOR files must exist in test resources at `/utxos/{txId}-{index}` for all transaction
  *     inputs and reference inputs used in the block transactions
  *   - Run the PlutusScriptEvaluatorTest tests first to populate UTxO resources if needed
  *
  * **Usage:**
  *   - Run all benchmarks: `sbtn "bench/Jmh/run -i 3 -wi 2 -f 1 -t 1
  *     .*PlutusScriptEvaluatorBenchmark.*"`
  *   - Run single transaction benchmark: `sbtn "bench/Jmh/run -i 3 -wi 2 -f 1 -t 1
  *     .*evaluateSingleTransaction.*"`
  *   - Run full block benchmark: `sbtn "bench/Jmh/run -i 3 -wi 2 -f 1 -t 1 .*evaluateBlock.*"`
  *
  * **Benchmark methods:**
  *   - `evaluateBlock`: Evaluates all transactions with redeemers in the selected block
  *   - `evaluateSingleTransaction`: Evaluates only the first transaction for detailed per-tx
  *     metrics
  *
  * @param blockNumber
  *   The Cardano mainnet block number to benchmark (11553070, 11544748, or 11544518)
  */
@State(Scope.Benchmark)
class PlutusScriptEvaluatorBenchmark {

    @Param(Array("11553070", "11544748", "11544518"))
    @nowarn("msg=unset private variable")
    private var blockNumber: String = ""

    private var transactions: Seq[(Transaction, Map[TransactionInput, TransactionOutput])] =
        Seq.empty

    // Create evaluator once with mainnet parameters since it doesn't change
    private val evaluator = PlutusScriptEvaluator(CardanoInfo.mainnet, EvaluatorMode.Validate)

    private val utxoResolver = new ResourcesUtxoResolver()

    @Setup
    def setup(): Unit = {
        // Load block from resources
        val bytes = getClass.getResourceAsStream(s"/blocks/block-$blockNumber.cbor").readAllBytes()
        given OriginalCborByteArray = OriginalCborByteArray(bytes)
        val block = BlockFile.fromCborArray(bytes).block

        // Pre-load transactions with redeemers and resolve their UTxOs
        // Only include transactions where we can successfully resolve all UTxOs
        transactions = block.transactions
            .filter(tx => tx.witnessSet.redeemers.nonEmpty && tx.isValid)
            .flatMap { tx => Try((tx, utxoResolver.resolveUtxos(tx))).toOption }

        if transactions.isEmpty then
            throw new IllegalStateException(
              s"No transactions with fully resolvable UTxOs found in block $blockNumber"
            )
    }

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MILLISECONDS)
    def evaluateBlock(): Int = {
        var totalRedeemers = 0
        for (tx, utxos) <- transactions do
            val redeemers = evaluator.evalPlutusScripts(tx, utxos)
            totalRedeemers += redeemers.size
        totalRedeemers
    }

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MILLISECONDS)
    def evaluateSingleTransaction(): Int = {
        // Benchmark just the first transaction for detailed per-transaction metrics
        val (tx, utxos) = transactions.head
        val redeemers = evaluator.evalPlutusScripts(tx, utxos)
        redeemers.size
    }
}
