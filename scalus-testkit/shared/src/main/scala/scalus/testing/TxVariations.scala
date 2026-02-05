package scalus.testing

import org.scalacheck.Gen
import scalus.cardano.ledger.Transaction
import scalus.cardano.node.BlockchainReader

import scala.concurrent.{ExecutionContext, Future}

/** Transaction variations generator for property-based and exhaustive testing.
  *
  * `TxVariations` generates multiple transaction variations from a given contract state and base
  * template. Each variation represents a different way to construct a transaction (e.g., different
  * input selection, datum values, redeemer arguments).
  *
  * The `TxTemplate` provides the sponsor (who pays fees) and signer, while variations build the
  * transaction body.
  *
  * @tparam S
  *   the contract state type, typically extracted from blockchain UTxOs
  */
trait TxVariations[S] {

    /** Generate all transaction variations for the given state.
      *
      * Returns completed, signed transactions ready to submit.
      *
      * @param reader
      *   blockchain reader for UTxO queries and tx completion
      * @param state
      *   contract state extracted from blockchain
      * @param template
      *   base template providing sponsor and signer
      * @return
      *   future containing all transaction variations
      */
    def enumerate(
        reader: BlockchainReader,
        state: S,
        txTemplate: TxTemplate
    )(using ExecutionContext): Future[Seq[Transaction]]

    /** Combine two variation generators.
      *
      * The resulting generator produces variations from both sources.
      */
    def ++(other: TxVariations[S]): TxVariations[S] = TxVariations.Combined(this, other)
}

object TxVariations {

    /** Access standard transaction variation patterns.
      *
      * Returns `StandardTxVariations` object which provides factory methods for common attack
      * patterns like stealing funds, corrupting datums, wrong addresses, etc.
      *
      * @see
      *   [[StandardTxVariations]] for available variation patterns
      */
    def standard: StandardTxVariations.type = StandardTxVariations

    /** Combined variations from two sources. */
    private[testing] case class Combined[S](left: TxVariations[S], right: TxVariations[S])
        extends TxVariations[S] {
        override def enumerate(
            reader: BlockchainReader,
            state: S,
            txTemplate: TxTemplate
        )(using ExecutionContext): Future[Seq[Transaction]] =
            for
                leftTxs <- left.enumerate(reader, state, txTemplate)
                rightTxs <- right.enumerate(reader, state, txTemplate)
            yield leftTxs ++ rightTxs
    }

    /** Create variations from a sequence of transactions (already completed). */
    def fromTransactions[S](txs: Seq[Transaction]): TxVariations[S] =
        new TxVariations[S] {
            override def enumerate(
                reader: BlockchainReader,
                state: S,
                txTemplate: TxTemplate
            )(using ExecutionContext): Future[Seq[Transaction]] =
                Future.successful(txs)
        }

    /** Create empty variations (no transactions). */
    def empty[S]: TxVariations[S] = fromTransactions(Seq.empty)
}

/** Sampling-based variations for fuzz testing.
  *
  * Unlike `TxVariations` which enumerates all variations exhaustively, `TxSamplingVariations` uses
  * a generator to sample variations. This is useful for large or continuous variation spaces.
  *
  * The primary method is `gen`; `enumerate` samples from the generator.
  *
  * @tparam S
  *   the contract state type
  */
trait TxSamplingVariations[S] extends TxVariations[S] {

    /** Generate a single transaction variation.
      *
      * Returns a Gen that produces Future[Transaction] — the transaction is completed
      * asynchronously.
      */
    def gen(
        reader: BlockchainReader,
        state: S,
        txTemplate: TxTemplate
    ): Gen[Future[Transaction]]

    /** Number of samples to take when enumerating. */
    def sampleSize: Int = 20

    override def enumerate(
        reader: BlockchainReader,
        state: S,
        txTemplate: TxTemplate
    )(using ExecutionContext): Future[Seq[Transaction]] = {
        val futures = List.fill(sampleSize)(gen(reader, state, txTemplate).sample).flatten
        Future.sequence(futures)
    }
}

/** Bundles contract step state extraction, base template building, and variations.
  *
  * `ContractStepVariations` represents a single step in a contract interaction (e.g., "bid on
  * auction", "claim HTLC"). It combines:
  *   - State extraction from blockchain UTxOs
  *   - Base template construction (builder + sponsor + signer)
  *   - Transaction variations for testing
  *
  * Use with ScenarioExplorer for exhaustive testing.
  *
  * @tparam S
  *   the contract state type
  */
trait ContractStepVariations[S] {

    /** Extract contract state from the blockchain.
      *
      * Typically queries UTxOs at the contract address and parses datums.
      */
    def extractState(reader: BlockchainReader)(using ExecutionContext): Future[S]

    /** Build the base transaction template for this step.
      *
      * The template includes the base TxBuilder (the "correct" transaction), sponsor (who pays
      * fees), and signer. Variations will modify or replace the builder while using the same
      * sponsor/signer.
      */
    def makeBaseTx(reader: BlockchainReader, state: S)(using
        ExecutionContext
    ): Future[TxTemplate]

    /** Transaction variations to test.
      *
      * Variations receive the base template from `makeBaseTx` and produce transaction variations.
      */
    def variations: TxVariations[S]

    /** Generate all transaction variations for the given state.
      *
      * Convenience method that builds the template and delegates to `variations.enumerate`.
      */
    def allVariations(
        reader: BlockchainReader,
        state: S
    )(using ExecutionContext): Future[Seq[Transaction]] =
        makeBaseTx(reader, state).flatMap { txTemplate =>
            variations.enumerate(reader, state, txTemplate)
        }

    /** Slot delays to explore at this step. Default: none.
      *
      * Override to include [[StepAction.Wait]] actions in [[allActions]]. Each delay creates a
      * separate branch that advances the slot before the next step.
      */
    def slotDelays(state: S): Seq[Long] = Seq.empty

    /** All actions (submits + waits) for this step.
      *
      * Combines transaction variations (as [[StepAction.Submit]]) with slot delays (as
      * [[StepAction.Wait]]).
      */
    def allActions(
        reader: BlockchainReader,
        state: S
    )(using ExecutionContext): Future[Seq[StepAction]] =
        allVariations(reader, state).map { txs =>
            txs.map(StepAction.Submit(_)) ++ slotDelays(state).map(StepAction.Wait(_))
        }
}
