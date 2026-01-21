package scalus.testing.kit

import org.scalatest.Assertions
import scalus.cardano.ledger.*
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.TxBuilderException
import scalus.utils.await

import scala.concurrent.ExecutionContext

/** Transaction testing utilities (JVM-only).
  *
  * Provides assertion methods for testing transaction building and submission against an emulator.
  */
trait TxTestKit extends Assertions {

    /** Verifies that transaction building fails with the expected error.
      *
      * @param expectedError
      *   Substring expected in error message or script logs
      * @param buildTx
      *   The transaction building code that should fail
      */
    protected def assertScriptFail(expectedError: String)(buildTx: => Transaction): Unit = {
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
            case e: Exception =>
                val message = Option(e.getMessage).getOrElse(e.getClass.getSimpleName)
                assert(
                  message.contains(expectedError),
                  s"Expected error containing '$expectedError' but got: $message"
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
