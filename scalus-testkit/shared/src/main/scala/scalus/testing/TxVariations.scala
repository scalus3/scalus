package scalus.testing

import org.scalacheck.Gen
import scalus.cardano.address.Address
import scalus.cardano.ledger.Transaction
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}

import scala.concurrent.{ExecutionContext, Future}

/** Transaction variations generator for property-based and exhaustive testing.
  *
  * `TxVariations` generates multiple transaction variations from a given contract state. Each
  * variation represents a different way to construct a transaction (e.g., different input
  * selection, datum values, redeemer arguments).
  *
  * Sponsor and signer are passed as parameters because they are orthogonal to contract state — the
  * same contract state can be interacted with by different parties (Alice, Bob, an attacker).
  *
  * @tparam S
  *   the contract state type, typically extracted from blockchain UTxOs
  */
trait TxVariations[S] {

    /** Generate all transaction variations for the given state.
      *
      * Returns completed, signed transactions ready to submit.
      *
      * @param provider
      *   blockchain provider for UTxO queries and tx completion
      * @param state
      *   contract state extracted from blockchain
      * @param sponsor
      *   address that pays for the transaction (provides UTxOs for fees/collateral)
      * @param signer
      *   signs the completed transaction
      * @return
      *   future containing all transaction variations
      */
    def enumerate(
        provider: BlockchainProvider,
        state: S,
        sponsor: Address,
        signer: TransactionSigner
    )(using ExecutionContext): Future[Seq[Transaction]]

    /** Combine two variation generators.
      *
      * The resulting generator produces variations from both sources.
      */
    def ++(other: TxVariations[S]): TxVariations[S] = TxVariations.Combined(this, other)
}

object TxVariations {

    /** Combined variations from two sources. */
    private case class Combined[S](left: TxVariations[S], right: TxVariations[S])
        extends TxVariations[S] {
        override def enumerate(
            provider: BlockchainProvider,
            state: S,
            sponsor: Address,
            signer: TransactionSigner
        )(using ExecutionContext): Future[Seq[Transaction]] =
            for
                leftTxs <- left.enumerate(provider, state, sponsor, signer)
                rightTxs <- right.enumerate(provider, state, sponsor, signer)
            yield leftTxs ++ rightTxs
    }

    /** Create variations from a sequence of transactions (already completed). */
    def fromTransactions[S](txs: Seq[Transaction]): TxVariations[S] =
        new TxVariations[S] {
            override def enumerate(
                provider: BlockchainProvider,
                state: S,
                sponsor: Address,
                signer: TransactionSigner
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
        provider: BlockchainProvider,
        state: S,
        sponsor: Address,
        signer: TransactionSigner
    ): Gen[Future[Transaction]]

    /** Number of samples to take when enumerating. */
    def sampleSize: Int = 20

    override def enumerate(
        provider: BlockchainProvider,
        state: S,
        sponsor: Address,
        signer: TransactionSigner
    )(using ExecutionContext): Future[Seq[Transaction]] = {
        val futures = List.fill(sampleSize)(gen(provider, state, sponsor, signer).sample).flatten
        Future.sequence(futures)
    }
}

/** Bundles contract step state extraction, base transaction building, and variations.
  *
  * `ContractStepVariations` represents a single step in a contract interaction (e.g., "bid on
  * auction", "claim HTLC"). It combines:
  *   - State extraction from blockchain UTxOs
  *   - Base transaction construction
  *   - Transaction variations for testing
  *
  * Use with Scenario for exhaustive testing or with ContractScalaCheckCommands for property-based
  * testing.
  *
  * @tparam S
  *   the contract state type
  */
trait ContractStepVariations[S] {

    /** Extract contract state from the blockchain.
      *
      * Typically queries UTxOs at the contract address and parses datums.
      */
    def extractState(provider: BlockchainProvider)(using ExecutionContext): Future[S]

    /** Build the base transaction for this step.
      *
      * The base transaction is the "correct" transaction that variations modify. Provider gives
      * access to `cardanoInfo` for protocol params, network, etc.
      */
    def baseTx(provider: BlockchainProvider, state: S)(using ExecutionContext): Future[TxBuilder]

    /** Transaction variations to test.
      *
      * Variations modify the base transaction in different ways (boundary values, invalid inputs,
      * etc.) to test contract behavior.
      */
    def variations: TxVariations[S]

    /** Generate all transaction variations for the given state.
      *
      * Convenience method that delegates to `variations.enumerate`.
      */
    def allVariations(
        provider: BlockchainProvider,
        state: S,
        sponsor: Address,
        signer: TransactionSigner
    )(using ExecutionContext): Future[Seq[Transaction]] =
        variations.enumerate(provider, state, sponsor, signer)
}
