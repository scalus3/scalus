package scalus.testing.kit

import org.scalatest.Assertions
import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.MinTransactionFee
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.TxBuilderException
import scalus.utils.await

import scala.concurrent.ExecutionContext

/** Transaction testing utilities (JVM-only).
  *
  * Provides assertion methods for testing transaction building and submission against an emulator.
  */
trait TxTestKit extends Assertions {

    /** Verifies successful transaction with real script evaluation.
      *
      * Checks:
      *   - Execution units are within expected bounds
      *   - Execution fee is positive
      *   - Transaction fee covers minimum required fee
      *   - Emulator submission succeeds
      *
      * @param provider
      *   The emulator to submit to
      * @param tx
      *   The transaction to verify
      * @param env
      *   The Cardano environment info
      * @param ec
      *   ExecutionContext for async operations
      */
    protected def assertTxSuccess(provider: Emulator, tx: Transaction)(using
        env: CardanoInfo,
        ec: ExecutionContext
    ): Unit = {
        // Verify execution units are reasonable (not max budget)
        val totalExUnits = tx.witnessSet.redeemers.map(_.value.totalExUnits).getOrElse(ExUnits.zero)
        assert(totalExUnits.memory > 0, "Execution units memory should be positive")
        assert(totalExUnits.steps > 0, "Execution units steps should be positive")
        assert(
          totalExUnits.memory < env.protocolParams.maxTxExecutionUnits.memory,
          s"ExUnits memory ${totalExUnits.memory} should be less than max ${env.protocolParams.maxTxExecutionUnits.memory}"
        )
        assert(
          totalExUnits.steps < env.protocolParams.maxTxExecutionUnits.steps,
          s"ExUnits steps ${totalExUnits.steps} should be less than max ${env.protocolParams.maxTxExecutionUnits.steps}"
        )

        // Verify execution fee
        val executionFee = totalExUnits.fee(env.protocolParams.executionUnitPrices)
        assert(executionFee.value > 0, "Execution fee should be positive")

        // Verify transaction fee covers minimum
        val txFee = tx.body.value.fee
        val allInputs = tx.body.value.inputs.toSet ++ tx.body.value.referenceInputs.toSet
        val utxos = provider.findUtxos(allInputs).await().toOption.get
        val minFee = MinTransactionFee.computeMinFee(tx, utxos, env.protocolParams).toOption.get
        assert(
          txFee >= minFee,
          s"Transaction fee $txFee should be >= minimum fee $minFee"
        )

        // Verify emulator submission succeeds
        val submissionResult = provider.submit(tx).await()
        assert(submissionResult.isRight, s"Emulator submission failed: $submissionResult")
    }

    /** Verifies that transaction building fails with the expected error.
      *
      * @param expectedError
      *   Substring expected in error message or script logs
      * @param buildTx
      *   The transaction building code that should fail
      */
    protected def assertTxFail(expectedError: String)(buildTx: => Transaction): Unit = {
        try
            val tx = buildTx
            fail(s"Transaction building should have failed but succeeded: $tx")
        catch
            case e: TxBuilderException.BalancingException =>
                val logs = e.scriptLogs.getOrElse(Seq.empty)
                assert(
                  logs.exists(_.contains(expectedError)),
                  s"Expected error containing '$expectedError' but got logs: ${logs.mkString("\n")}"
                )
            case e: Throwable =>
                assert(
                  e.getMessage.contains(expectedError),
                  s"Expected error containing '$expectedError' but got: ${e.getMessage}"
                )
    }

    /** Verifies that transaction budget is within specified limits.
      *
      * @param tx
      *   The transaction to check
      * @param maxMemory
      *   Maximum allowed memory units
      * @param maxSteps
      *   Maximum allowed CPU steps
      */
    protected def assertTxBudgetWithin(tx: Transaction, maxMemory: Long, maxSteps: Long): Unit = {
        val totalExUnits = tx.witnessSet.redeemers.map(_.value.totalExUnits).getOrElse(ExUnits.zero)
        assert(
          totalExUnits.memory <= maxMemory,
          s"Memory ${totalExUnits.memory} exceeds max $maxMemory"
        )
        assert(
          totalExUnits.steps <= maxSteps,
          s"Steps ${totalExUnits.steps} exceeds max $maxSteps"
        )
    }

    /** Assert that transaction submission to emulator succeeds.
      *
      * @param provider
      *   The emulator
      * @param tx
      *   The transaction
      * @param ec
      *   ExecutionContext for async operations
      */
    protected def assertTxSubmits(provider: Emulator, tx: Transaction)(using
        ec: ExecutionContext
    ): Unit = {
        val result = provider.submit(tx).await()
        assert(result.isRight, s"Transaction submission failed: $result")
    }

    /** Assert that transaction submission to emulator fails.
      *
      * @param provider
      *   The emulator
      * @param tx
      *   The transaction
      * @param ec
      *   ExecutionContext for async operations
      */
    protected def assertTxSubmitFails(provider: Emulator, tx: Transaction)(using
        ec: ExecutionContext
    ): Unit = {
        val result = provider.submit(tx).await()
        assert(result.isLeft, s"Transaction submission should have failed but succeeded")
    }
}
