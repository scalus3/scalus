package scalus.testing

import cps.*
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
    /** Action path that led to this violation. */
    path: Seq[StepAction]
)

/** Explores contract state space by applying actions at each step.
  *
  * ScenarioExplorer performs bounded exploration of contract interactions. At each depth it:
  *   1. Gets a snapshot reader
  *   2. Calls the step function which performs one interaction using normal Scenario operations
  *   3. Actions ([[Scenario.submit]], [[Scenario.sleep]]) are automatically logged
  *   4. On successful step, recurses to next depth
  *   5. On check failure, captures violation with the full action path
  *   6. On other errors, propagates
  *
  * The step function can use [[Scenario.choices]]/[[Scenario.fromCollection]] for branching,
  * [[Scenario.submit]] for transactions, [[Scenario.sleep]] for time advancement, and
  * [[Scenario.check]] for invariant verification.
  */
object ScenarioExplorer {

    /** Explore contract interactions up to a maximum depth.
      *
      * @param maxDepth
      *   maximum number of steps to explore
      * @param step
      *   function that performs one interaction step using normal Scenario operations
      * @return
      *   Scenario returning None if all paths succeeded, Some(Violation) if a check failed
      */
    def explore(maxDepth: Int)(
        step: BlockchainReader => Scenario[Unit]
    ): Scenario[Option[Violation]] = {
        given ExecutionContext = ExecutionContext.parasitic
        exploreRec(maxDepth, step, depth = 1)
    }

    private def exploreRec(
        maxDepth: Int,
        step: BlockchainReader => Scenario[Unit],
        depth: Int
    )(using ExecutionContext): Scenario[Option[Violation]] = {
        if depth > maxDepth then Scenario.scenarioLogicMonad.pure(None)
        else
            Scenario.scenarioLogicMonad.flatMapTry(exploreStep(step)) {
                case scala.util.Success(_) =>
                    exploreRec(maxDepth, step, depth + 1)
                case scala.util.Failure(e: CheckFailure) =>
                    // Check failed - capture violation with the full action path
                    async[Scenario] {
                        val path = Scenario.actionLog.await
                        Some(Violation(e.predicate, e.message, e.location, path))
                    }
                case scala.util.Failure(e) =>
                    Scenario.error(e)
            }
    }

    private def exploreStep(
        step: BlockchainReader => Scenario[Unit]
    )(using ExecutionContext): Scenario[Unit] = {
        async[Scenario] {
            val reader = Scenario.snapshotReader.await
            step(reader).await
        }
    }
}
