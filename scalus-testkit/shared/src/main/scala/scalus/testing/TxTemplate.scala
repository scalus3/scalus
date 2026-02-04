package scalus.testing

import scalus.cardano.address.Address
import scalus.cardano.ledger.Transaction
import scalus.cardano.node.BlockchainReader
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}

import scala.concurrent.{ExecutionContext, Future}

/** A transaction template bundling a builder with its sponsor and signer.
  *
  * TxTemplate represents a transaction that is ready to be completed and signed. It bundles:
  *   - `builder`: the TxBuilder with inputs, outputs, etc.
  *   - `sponsor`: the address that pays fees (provides UTxOs for fees/collateral)
  *   - `signer`: signs the completed transaction
  *
  * This allows variations to be self-contained - each variation knows who will sign it.
  */
case class TxTemplate(
    builder: TxBuilder,
    sponsor: Address,
    signer: TransactionSigner
) {

    /** Complete and sign the transaction.
      *
      * Uses the sponsor's UTxOs for fees and collateral, then signs with the signer.
      */
    def complete(reader: BlockchainReader)(using ExecutionContext): Future[Transaction] =
        builder.complete(reader, sponsor).map(_.sign(signer).transaction)

    /** Transform the builder while keeping the same sponsor and signer. */
    def mapBuilder(f: TxBuilder => TxBuilder): TxTemplate =
        copy(builder = f(builder))

    /** Change the sponsor (who pays fees). */
    def withSponsor(newSponsor: Address): TxTemplate =
        copy(sponsor = newSponsor)

    /** Change the signer. */
    def withSigner(newSigner: TransactionSigner): TxTemplate =
        copy(signer = newSigner)
}
