package scalus.testing

import cps.*
import scalus.cardano.ledger.Transaction
import scalus.cardano.node.BlockchainReader

import scala.concurrent.ExecutionContext

/** A violation found during scenario exploration. */
case class Violation(
    /** The predicate expression that failed. */
    predicate: String,
    /** Optional message provided to the check. */
    message: String,
    /** Source location of the failed check. */
    location: SourceLocation,
    /** Transaction path that led to this violation. */
    path: Seq[Transaction]
)

/** Explores contract state space by applying transaction variations at each step.
  *
  * ScenarioExplorer performs bounded exploration of contract interactions. At each depth it:
  *   1. Gets a snapshot reader
  *   2. Calls the transaction generator to get transactions to try
  *   3. Branches over all transactions
  *   4. Submits each transaction
  *   5. On success, recurses to next depth
  *   6. On transaction rejection, prunes branch
  *   7. On check failure, captures violation with path
  *
  * Use `Scenario.check` within the transaction generator to verify invariants.
  */
object ScenarioExplorer {

    /** Explore contract interactions up to a maximum depth.
      *
      * @param maxDepth
      *   maximum number of sequential transactions to explore
      * @param genTransactions
      *   function that takes a reader and returns transactions to try; use `Scenario.check` inside
      *   to verify invariants
      * @return
      *   Scenario returning None if path succeeded, Some(Violation) if a check failed
      */
    def explore(maxDepth: Int)(
        genTransactions: BlockchainReader => Scenario[Seq[Transaction]]
    ): Scenario[Option[Violation]] = {
        given ExecutionContext = ExecutionContext.parasitic
        exploreRec(maxDepth, genTransactions, path = Seq.empty, depth = 1)
    }

    private def exploreRec(
        maxDepth: Int,
        genTransactions: BlockchainReader => Scenario[Seq[Transaction]],
        path: Seq[Transaction],
        depth: Int
    )(using ExecutionContext): Scenario[Option[Violation]] = {
        if depth > maxDepth then
            // Reached max depth successfully
            Scenario.scenarioLogicMonad.pure(None)
        else
            Scenario.scenarioLogicMonad.flatMapTry(exploreStep(genTransactions, path)) {
                case scala.util.Success(newPath) =>
                    // Transaction succeeded, recurse
                    exploreRec(maxDepth, genTransactions, newPath, depth + 1)
                case scala.util.Failure(e: CheckFailure) =>
                    // Check failed - return violation with path
                    Scenario.scenarioLogicMonad.pure(
                      Some(Violation(e.predicate, e.message, e.location, path))
                    )
                case scala.util.Failure(e) =>
                    // Other error - propagate
                    Scenario.error(e)
            }
    }

    private def exploreStep(
        genTransactions: BlockchainReader => Scenario[Seq[Transaction]],
        path: Seq[Transaction]
    )(using ExecutionContext): Scenario[Seq[Transaction]] = {
        async[Scenario] {
            val reader = Scenario.snapshotReader.await
            val txs = genTransactions(reader).await
            val tx = Scenario.fromCollection(txs).await
            val result = Scenario.submit(tx).await
            result match
                case Right(_) =>
                    path :+ tx
                case Left(_) =>
                    // Transaction rejected - prune this branch
                    Scenario.fail[Seq[Transaction]].await
        }
    }
}
