package scalus.testing

import scalus.cardano.ledger.Transaction
import scalus.cardano.node.BlockchainReader
import scalus.cardano.txbuilder.TxBuilderException

import scala.concurrent.{ExecutionContext, Future}

/** An agent that interacts with a contract under test.
  *
  * Each agent represents a participant (or role) in the contract interaction. Given the current
  * blockchain state, an agent generates the actions it can take — transactions to submit and/or
  * slot delays to wait.
  *
  * Use agents with [[ContractStepVariations.fromAgents]] to compose multi-agent test scenarios.
  *
  * @tparam S
  *   the contract state type
  */
trait ContractTestAgent[S] {

    /** Human-readable name for diagnostics. */
    def name: String

    /** Generate actions this agent can take given current state.
      *
      * Returns empty if the agent cannot act in the current state.
      */
    def actions(reader: BlockchainReader, state: S)(using ExecutionContext): Future[Seq[StepAction]]
}

object ContractTestAgent {

    /** Create an agent that builds a base transaction and applies variations (attack patterns).
      *
      * When `baseTx` returns `None`, the agent produces no actions (it cannot act in the current
      * state). When it returns `Some(template)`, the agent produces the base transaction plus all
      * variation transactions.
      *
      * @param agentName
      *   human-readable name
      * @param baseTx
      *   builds the base transaction template, or None if the agent cannot act
      * @param txVariations
      *   variations to apply to the base template (default: empty)
      */
    def withVariations[S](
        agentName: String,
        baseTx: (BlockchainReader, S) => Future[Option[TxTemplate]],
        txVariations: TxVariations[S] = TxVariations.empty[S]
    ): ContractTestAgent[S] = new ContractTestAgent[S] {
        override def name: String = agentName

        override def actions(reader: BlockchainReader, state: S)(using
            ExecutionContext
        ): Future[Seq[StepAction]] =
            baseTx(reader, state).flatMap {
                case None => Future.successful(Seq.empty)
                case Some(template) =>
                    val baseTxFuture = template
                        .complete(reader)
                        .map { tx =>
                            Seq(StepAction.Submit(tx))
                        }
                        .recover { case _: TxBuilderException => Seq.empty }

                    val variationsFuture = txVariations
                        .enumerate(reader, state, template)
                        .map(_.map(StepAction.Submit(_)))
                        .recover { case _: TxBuilderException => Seq.empty }

                    for
                        base <- baseTxFuture
                        vars <- variationsFuture
                    yield base ++ vars
            }
    }

    /** Create an agent from a simple transaction builder. No variations.
      *
      * When `buildTx` returns `None`, the agent produces no actions. When it returns
      * `Some(transaction)`, the agent produces a single `Submit` action.
      *
      * @param agentName
      *   human-readable name
      * @param buildTx
      *   builds a completed, signed transaction, or None if the agent cannot act
      */
    def simple[S](
        agentName: String,
        buildTx: (BlockchainReader, S) => Future[Option[Transaction]]
    ): ContractTestAgent[S] = new ContractTestAgent[S] {
        override def name: String = agentName

        override def actions(reader: BlockchainReader, state: S)(using
            ExecutionContext
        ): Future[Seq[StepAction]] =
            buildTx(reader, state)
                .map {
                    case None     => Seq.empty
                    case Some(tx) => Seq(StepAction.Submit(tx))
                }
                .recover { case _: TxBuilderException => Seq.empty }
    }

    /** Create an agent that produces multiple transactions.
      *
      * When `buildTxs` returns empty, the agent produces no actions.
      *
      * @param agentName
      *   human-readable name
      * @param buildTxs
      *   builds completed, signed transactions
      */
    def multi[S](
        agentName: String,
        buildTxs: (BlockchainReader, S) => Future[Seq[Transaction]]
    ): ContractTestAgent[S] = new ContractTestAgent[S] {
        override def name: String = agentName

        override def actions(reader: BlockchainReader, state: S)(using
            ExecutionContext
        ): Future[Seq[StepAction]] =
            buildTxs(reader, state)
                .map(_.map(StepAction.Submit(_)))
                .recover { case _: TxBuilderException => Seq.empty }
    }
}
