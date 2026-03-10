package scalus.testing

import scalus.cardano.ledger.Transaction
import scalus.cardano.node.BlockchainReader
import scalus.cardano.txbuilder.TxBuilderException

import scala.concurrent.{ExecutionContext, Future}

/** An actor (participant) that interacts with a contract under test.
  *
  * Each actor represents a role in the contract interaction — a bidder, a donor, an attacker, etc.
  * Given the current blockchain state, an actor generates the actions it can take — transactions to
  * submit and/or slot delays to wait.
  *
  * Use actors with [[ContractStepVariations.fromActors]] to compose multi-actor test scenarios.
  *
  * @tparam S
  *   the contract state type
  */
trait ContractTestActor[S] {

    /** Human-readable name for diagnostics. */
    def name: String

    /** Generate actions this actor can take given current state.
      *
      * Returns empty if the actor cannot act in the current state.
      */
    def actions(reader: BlockchainReader, state: S)(using ExecutionContext): Future[Seq[StepAction]]
}

object ContractTestActor {

    /** Create an actor that builds a base transaction and applies variations (attack patterns).
      *
      * When `baseTx` returns `None`, the actor produces no actions (it cannot act in the current
      * state). When it returns `Some(template)`, the actor produces the base transaction plus all
      * variation transactions.
      *
      * @param actorName
      *   human-readable name
      * @param baseTx
      *   builds the base transaction template, or None if the actor cannot act
      * @param txVariations
      *   variations to apply to the base template (default: empty)
      */
    def withVariations[S](
        actorName: String,
        baseTx: (BlockchainReader, S) => Future[Option[TxTemplate]],
        txVariations: TxVariations[S] = TxVariations.empty[S]
    ): ContractTestActor[S] = new ContractTestActor[S] {
        override def name: String = actorName

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

    /** Create an actor from a simple transaction builder. No variations.
      *
      * When `buildTx` returns `None`, the actor produces no actions. When it returns
      * `Some(transaction)`, the actor produces a single `Submit` action.
      *
      * @param actorName
      *   human-readable name
      * @param buildTx
      *   builds a completed, signed transaction, or None if the actor cannot act
      */
    def simple[S](
        actorName: String,
        buildTx: (BlockchainReader, S) => Future[Option[Transaction]]
    ): ContractTestActor[S] = new ContractTestActor[S] {
        override def name: String = actorName

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

    /** Create an actor that produces multiple transactions.
      *
      * When `buildTxs` returns empty, the actor produces no actions.
      *
      * @param actorName
      *   human-readable name
      * @param buildTxs
      *   builds completed, signed transactions
      */
    def multi[S](
        actorName: String,
        buildTxs: (BlockchainReader, S) => Future[Seq[Transaction]]
    ): ContractTestActor[S] = new ContractTestActor[S] {
        override def name: String = actorName

        override def actions(reader: BlockchainReader, state: S)(using
            ExecutionContext
        ): Future[Seq[StepAction]] =
            buildTxs(reader, state)
                .map(_.map(StepAction.Submit(_)))
                .recover { case _: TxBuilderException => Seq.empty }
    }
}
