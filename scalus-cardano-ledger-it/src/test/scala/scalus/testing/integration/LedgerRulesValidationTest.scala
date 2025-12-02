package scalus.testing.integration

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.testing.integration.BlocksTestUtils.*

import java.nio.file.Path

/** Base class for ledger rules validation tests
  *
  * Uses BlocksTestUtils for common infrastructure like loading blocks from SCALUS_IT_DATA_PATH and
  * creating UTxO resolver.
  */
class LedgerRulesValidationTest extends AnyFunSuite {

    // Default context for validation
    protected def defaultContext: Context = Context.testMainnet()

    // Default initial state
    protected def defaultState: State = State()

    // Data class for validation failure
    case class ValidationFailure(
        blockPath: Path,
        txIndex: Int,
        txId: TransactionHash,
        validatorName: String,
        error: TransactionException
    )

    // Data class for validation summary
    case class ValidationSummary(
        totalBlocks: Int,
        totalTransactions: Int,
        totalValidations: Int,
        failures: List[ValidationFailure]
    ) {
        def failureCount: Int = failures.size
        def successCount: Int = totalValidations - failureCount
        def successRate: Double =
            if totalValidations > 0 then successCount.toDouble / totalValidations else 0.0

        def report: String = {
            val sb = new StringBuilder
            sb.append(s"\n=== Validation Summary ===\n")
            sb.append(s"Blocks processed: $totalBlocks\n")
            sb.append(s"Transactions validated: $totalTransactions\n")
            sb.append(s"Total validations: $totalValidations\n")
            sb.append(s"Successes: $successCount\n")
            sb.append(s"Failures: $failureCount\n")
            sb.append(f"Success rate: ${successRate * 100}%.2f%%\n")

            if failures.nonEmpty then
                sb.append(s"\n=== Failures by Validator ===\n")
                val byValidator = failures.groupBy(_.validatorName)
                for (validator, fails) <- byValidator.toSeq.sortBy(-_._2.size) do
                    sb.append(s"  $validator: ${fails.size} failures\n")

                sb.append(s"\n=== First 10 Detailed Failures ===\n")
                for failure <- failures.take(10) do
                    sb.append(s"  Block: ${failure.blockPath.getFileName}\n")
                    sb.append(s"  TX Index: ${failure.txIndex}\n")
                    sb.append(s"  TX ID: ${failure.txId}\n")
                    sb.append(s"  Validator: ${failure.validatorName}\n")
                    sb.append(s"  Error: ${failure.error.getMessage}\n")
                    sb.append(s"  ---\n")

            sb.toString
        }
    }

    protected def validateMutatorsWithSinglePass(
        mutators: List[STS.Mutator],
        context: Context = defaultContext,
        initialState: State = defaultState
    ): ValidationSummary = {
        var failures = List.empty[ValidationFailure]
        var totalTxs = 0
        var totalValidations = 0
        var consecutiveMissingUtxos = 0
        val maxConsecutiveMissingUtxos = 10

        val blockPaths = getAllBlocksPaths()
        var processedBlocks = 0

        for
            (blockPath, blockIdx) <- blockPaths.zipWithIndex
            if consecutiveMissingUtxos < maxConsecutiveMissingUtxos
        do
            val (blockFile, blockBytes) = loadBlock(blockPath)
            given OriginalCborByteArray = OriginalCborByteArray(blockBytes)
            val block = blockFile.block
            var state = initialState
            processedBlocks += 1

            for (tx, txIdx) <- block.transactions.zipWithIndex do
                totalTxs += 1
                try
                    val utxos = scalusUtxoResolver.resolveUtxos(tx)
                    val txState = state.copy(utxos = state.utxos ++ utxos)
                    consecutiveMissingUtxos = 0 // Reset on success

                    // Run all mutators sequentially
                    var currentState = txState
                    for mutator <- mutators do
                        totalValidations += 1
                        mutator(context, currentState, tx) match {
                            case Left(error) =>
                                failures = ValidationFailure(
                                  blockPath,
                                  txIdx,
                                  tx.id,
                                  mutator.name,
                                  error
                                ) :: failures
                            case Right(newState) =>
                                currentState = newState
                        }
                    state = currentState
                catch
                    case e: IllegalStateException if e.getMessage.contains("UTXO not found") =>
                        // Skip transactions with missing UTxOs
                        consecutiveMissingUtxos += 1
                        if consecutiveMissingUtxos == maxConsecutiveMissingUtxos then
                            println(
                              s"⚠ Stopping validation (mutators single pass): ${consecutiveMissingUtxos} consecutive missing UTxO errors"
                            )

        ValidationSummary(
          processedBlocks,
          totalTxs,
          totalValidations,
          failures.reverse
        )
    }

    test("validate transactions") {
        val summary = validateMutatorsWithSinglePass(List(CardanoMutator))
        println(summary.report)
        // assert(summary.failureCount == 0, s"Found ${summary.failureCount} failures")
    }
}
