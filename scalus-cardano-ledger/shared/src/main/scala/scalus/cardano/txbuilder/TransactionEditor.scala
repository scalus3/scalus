package scalus.cardano.txbuilder

/** Transaction editing utilities with automatic redeemer re-indexing.
  *
  * According to the ledger spec, redeemers, which are stored in a `TransactionWitnessSet`, contain
  * pointers to various transaction parts. The pointers are just numbers corresponding to indices in
  * arrays.
  *
  * For example, a redeemer for spending a UTxO locked at a script address contains an index of the
  * corresponding input. It's not very convenient because of the need to keep the indices correct
  * while modifying the transaction.
  *
  * For example, if a new mint is added, all mint redeemer indices may have to be updated. This
  * module automates these updates by providing a better API for modifying transactions that lets
  * the developer abstract away from the indices.
  *
  * The main functions are `editTransaction` and `editTransactionSafe`.
  */

import cats.implicits.*
import monocle.Lens
import monocle.Monocle.focus
import scalus.builtin.Data
import scalus.cardano.ledger.*

// ============================================================================
// DetachedRedeemer
// ============================================================================

/** Redeemer that was detached from a transaction. Contains just enough info for it to be
  * re-attached again, if a transaction needs a redeemer for some action.
  */
case class DetachedRedeemer(
    datum: Data,
    purpose: RedeemerPurpose
)

// ============================================================================
// RedeemerPurpose
// ============================================================================

/** Contains a value that a redeemer corresponds to. Allows finding a redeemer index, given a
  * transaction contains the value.
  */
sealed trait RedeemerPurpose {
    def redeemerTag: RedeemerTag
}

object RedeemerPurpose {
    case class ForSpend(input: TransactionInput) extends RedeemerPurpose {
        def redeemerTag: RedeemerTag = RedeemerTag.Spend
    }
    case class ForMint(scriptHash: ScriptHash) extends RedeemerPurpose {
        def redeemerTag: RedeemerTag = RedeemerTag.Mint
    }
    case class ForReward(rewardAddress: RewardAccount) extends RedeemerPurpose {
        def redeemerTag: RedeemerTag = RedeemerTag.Reward
    }
    case class ForCert(certificate: Certificate) extends RedeemerPurpose {
        def redeemerTag: RedeemerTag = RedeemerTag.Cert
    }
    case class ForVote(voter: Voter) extends RedeemerPurpose {
        def redeemerTag: RedeemerTag = RedeemerTag.Voting
    }
    case class ForPropose(proposal: ProposalProcedure) extends RedeemerPurpose {
        def redeemerTag: RedeemerTag = RedeemerTag.Proposing
    }
}

@deprecated("use RedeemerPurpose instead", "0.13.0")
object RedeemerPurposeUtils {
    @deprecated("use purpose.redeemerTag instead", "0.13.0")
    def redeemerPurposeToRedeemerTag(purpose: RedeemerPurpose): RedeemerTag =
        purpose.redeemerTag
}

// ============================================================================
// RedeemersContext
// ============================================================================

/** Contains parts of a transaction that are needed for redeemer processing.
  */
@deprecated("Use Transaction-based methods directly", "0.13.0")
case class RedeemersContext(
    inputs: Vector[TransactionInput] = Vector.empty,
    mintingPolicyHashes: Vector[ScriptHash] = Vector.empty,
    rewardAddresses: Vector[RewardAccount] = Vector.empty,
    certs: Vector[Certificate] = Vector.empty,
    proposals: Vector[ProposalProcedure] = Vector.empty,
    voters: Vector[Voter] = Vector.empty
)

// ============================================================================
// RedeemersContext utilities
// ============================================================================

object RedeemersContext {
    @deprecated("Use Transaction-based methods directly", "0.13.0")
    def fromTransaction(tx: Transaction): RedeemersContext = {
        val body = tx.body.value
        RedeemersContext(
          inputs = body.inputs.toSeq.toVector,
          mintingPolicyHashes = body.mint.map(_.assets.keys.toVector).getOrElse(Vector.empty),
          rewardAddresses = body.withdrawals.getOrElse(Withdrawals.empty).withdrawals.keys.toVector,
          /*
            TODO: shouldn't this be TaggedOrderedSet?
            /** Certificates for delegation, stake operations, etc. */
            certificates: TaggedSet[Certificate] = TaggedSet.empty,
           */
          certs = body.certificates.toSeq.toVector,
          proposals = body.proposalProcedures.toSeq.toVector,
          voters = body.votingProcedures match {
              case Some(voters) =>
                  voters.procedures.keys.toVector
              case None => Vector.empty
          }
        )
    }
}

// ============================================================================
// Redeemer attachment/detachment
// ============================================================================

object RedeemerManagement {
    @deprecated("Use detachRedeemer(Transaction, Redeemer) instead", "0.13.0")
    def detachRedeemer(ctx: RedeemersContext, redeemer: Redeemer): Option[DetachedRedeemer] = {
        val index = redeemer.index
        val purposeOpt = redeemer.tag match
            case RedeemerTag.Spend =>
                ctx.inputs.lift(index).map(RedeemerPurpose.ForSpend.apply)
            case RedeemerTag.Mint =>
                ctx.mintingPolicyHashes.lift(index).map(RedeemerPurpose.ForMint.apply)
            case RedeemerTag.Reward =>
                ctx.rewardAddresses.lift(index).map(RedeemerPurpose.ForReward.apply)
            case RedeemerTag.Cert =>
                ctx.certs.lift(index).map(RedeemerPurpose.ForCert.apply)
            case RedeemerTag.Proposing =>
                ctx.proposals.lift(index).map(RedeemerPurpose.ForPropose.apply)
            case RedeemerTag.Voting =>
                ctx.voters.lift(index).map(RedeemerPurpose.ForVote.apply)

        purposeOpt.map(purpose => DetachedRedeemer(redeemer.data, purpose))
    }

    @deprecated("Use attachRedeemer(Transaction, DetachedRedeemer) instead", "0.13.0")
    def attachRedeemer(ctx: RedeemersContext, detached: DetachedRedeemer): Option[Redeemer] = {
        val tag = detached.purpose.redeemerTag
        val indexOpt = detached.purpose match {
            case RedeemerPurpose.ForSpend(input)     => ctx.inputs.indexOf(input)
            case RedeemerPurpose.ForMint(scriptHash) => ctx.mintingPolicyHashes.indexOf(scriptHash)
            case RedeemerPurpose.ForReward(rewardAddress) =>
                ctx.rewardAddresses.indexOf(rewardAddress)
            case RedeemerPurpose.ForCert(certificate) => ctx.certs.indexOf(certificate)
            case RedeemerPurpose.ForPropose(proposal) => ctx.proposals.indexOf(proposal)
            case RedeemerPurpose.ForVote(voter)       => ctx.voters.indexOf(voter)
        }

        if indexOpt >= 0 then {
            Some(
              Redeemer(
                tag = tag,
                index = indexOpt,
                data = detached.datum,
                exUnits = ExUnits.zero
              )
            )
        } else {
            None
        }
    }

    @deprecated("Use attachRedeemers(Transaction, Vector[DetachedRedeemer]) instead", "0.13.0")
    def attachRedeemers(
        ctx: RedeemersContext,
        detached: Vector[DetachedRedeemer]
    ): Either[DetachedRedeemer, Vector[Redeemer]] = {
        detached.traverse(redeemer => attachRedeemer(ctx, redeemer).toRight(redeemer))
    }

    /** Detach a redeemer from a transaction, converting index-based reference to value-based.
      */
    def detachRedeemer(tx: Transaction, redeemer: Redeemer): Option[DetachedRedeemer] = {
        val body = tx.body.value
        val index = redeemer.index
        val purposeOpt = redeemer.tag match
            case RedeemerTag.Spend =>
                body.inputs.toSeq.lift(index).map(RedeemerPurpose.ForSpend.apply)
            case RedeemerTag.Mint =>
                body.mint
                    .map(_.assets.keys.toVector)
                    .getOrElse(Vector.empty)
                    .lift(index)
                    .map(RedeemerPurpose.ForMint.apply)
            case RedeemerTag.Reward =>
                body.withdrawals
                    .getOrElse(Withdrawals.empty)
                    .withdrawals
                    .keys
                    .toVector
                    .lift(index)
                    .map(RedeemerPurpose.ForReward.apply)
            case RedeemerTag.Cert =>
                body.certificates.toSeq.lift(index).map(RedeemerPurpose.ForCert.apply)
            case RedeemerTag.Proposing =>
                body.proposalProcedures.toSeq.lift(index).map(RedeemerPurpose.ForPropose.apply)
            case RedeemerTag.Voting =>
                body.votingProcedures
                    .map(_.procedures.keys.toVector)
                    .getOrElse(Vector.empty)
                    .lift(index)
                    .map(RedeemerPurpose.ForVote.apply)

        purposeOpt.map(purpose => DetachedRedeemer(redeemer.data, purpose))
    }

    /** Attach a detached redeemer to a transaction, converting value-based reference to
      * index-based.
      */
    def attachRedeemer(tx: Transaction, detached: DetachedRedeemer): Option[Redeemer] = {
        val body = tx.body.value
        val tag = detached.purpose.redeemerTag
        val indexOpt = detached.purpose match {
            case RedeemerPurpose.ForSpend(input) => body.inputs.toSeq.indexOf(input)
            case RedeemerPurpose.ForMint(scriptHash) =>
                body.mint.map(_.assets.keys.toVector).getOrElse(Vector.empty).indexOf(scriptHash)
            case RedeemerPurpose.ForReward(rewardAddress) =>
                body.withdrawals
                    .getOrElse(Withdrawals.empty)
                    .withdrawals
                    .keys
                    .toVector
                    .indexOf(rewardAddress)
            case RedeemerPurpose.ForCert(certificate) =>
                body.certificates.toSeq.indexOf(certificate)
            case RedeemerPurpose.ForPropose(proposal) =>
                body.proposalProcedures.toSeq.indexOf(proposal)
            case RedeemerPurpose.ForVote(voter) =>
                body.votingProcedures
                    .map(_.procedures.keys.toVector)
                    .getOrElse(Vector.empty)
                    .indexOf(voter)
        }

        if indexOpt >= 0 then
            Some(
              Redeemer(tag = tag, index = indexOpt, data = detached.datum, exUnits = ExUnits.zero)
            )
        else None
    }

    /** Attach multiple detached redeemers to a transaction.
      */
    def attachRedeemers(
        tx: Transaction,
        detached: Vector[DetachedRedeemer]
    ): Either[DetachedRedeemer, Vector[Redeemer]] = {
        detached.traverse(redeemer => attachRedeemer(tx, redeemer).toRight(redeemer))
    }
}

// ============================================================================
// EditableTransaction
// ============================================================================

/** A transaction with redeemers detached.
  */
case class EditableTransaction(
    transaction: Transaction,
    redeemers: Vector[DetachedRedeemer]
)

// ============================================================================
// Transaction conversion functions
// ============================================================================

object TransactionConversion {

    /** Detach transaction redeemers. Leaves invalid redeemers in the transaction's witness set, and
      * places the valid ones alongside the transaction.
      */
    def toEditableTransaction(tx: Transaction): EditableTransaction = {
        val witnessSet = tx.witnessSet

        val (validRedeemers, invalidRedeemers) = {
            witnessSet.redeemers.map(_.value) match {
                case None => (Seq.empty, Seq.empty)
                case Some(rs) =>
                    rs.toSeq.partition(redeemer =>
                        RedeemerManagement.detachRedeemer(tx, redeemer).isDefined
                    )
            }
        }

        val redeemers = validRedeemers.flatMap(RedeemerManagement.detachRedeemer(tx, _)).toVector
        val updatedWitnessSet = witnessSet.copy(redeemers =
            if invalidRedeemers.isEmpty then None
            else Some(KeepRaw(Redeemers(invalidRedeemers*)))
        )

        val updatedTx =
            tx.focus(_.witnessSet).replace(updatedWitnessSet)

        EditableTransaction(updatedTx, redeemers)
    }

    /** Detach transaction redeemers. Removes redeemers from the witness set and places them
      * alongside the transaction. Fails if there are redeemers that do not point to anything.
      */
    def toEditableTransactionSafe(
        tx: Transaction
    ): Either[Redeemer, EditableTransaction] = {
        for {
            redeemers <- tx.witnessSet.redeemers match {
                case None => Right(Vector.empty)
                case Some(rs) =>
                    rs.value.toSeq
                        .traverse { redeemer =>
                            RedeemerManagement.detachRedeemer(tx, redeemer).toRight(redeemer)
                        }
            }

            updatedTx = tx
                .focus(_.witnessSet)
                .replace(TransactionWitnessSet.empty)
        } yield EditableTransaction(updatedTx, redeemers.toVector)
    }

    /** Re-attach transaction redeemers. Returns Left with the problematic redeemer if a detached
      * redeemer does not point to anything in the transaction (e.g., a redeemer for spending an
      * input that doesn't exist, or minting with a policy that isn't in the mint field).
      */
    def fromEditableTransactionSafe(
        editable: EditableTransaction
    ): Either[DetachedRedeemer, Transaction] = {
        RedeemerManagement.attachRedeemers(editable.transaction, editable.redeemers) match {
            case Left(problematicRedeemer) => Left(problematicRedeemer)
            case Right(attachedRedeemers) =>
                val currentWitnessSet = editable.transaction.witnessSet
                val invalidRedeemers =
                    currentWitnessSet.redeemers.map(_.value.toSeq.toVector).getOrElse(Vector.empty)
                val allRedeemers = (invalidRedeemers ++ attachedRedeemers).distinct
                val updatedWitnessSet =
                    currentWitnessSet.copy(redeemers =
                        if allRedeemers.isEmpty then None
                        else Some(KeepRaw(Redeemers(allRedeemers*)))
                    )
                val updatedTx = editable.transaction
                    .focus(_.witnessSet)
                    .replace(updatedWitnessSet)
                Right(updatedTx)
        }
    }

    /** Re-attach transaction redeemers. Silently drops detached redeemers that are not valid.
      */
    def fromEditableTransaction(editable: EditableTransaction): Transaction = {
        val attachedRedeemers =
            editable.redeemers.flatMap(RedeemerManagement.attachRedeemer(editable.transaction, _))

        val currentWitnessSet = editable.transaction.witnessSet
        val invalidRedeemers =
            currentWitnessSet.redeemers.map(_.value.toSeq).getOrElse(Seq.empty)

        val allRedeemers = (invalidRedeemers ++ attachedRedeemers).distinct
        val updatedWitnessSet =
            currentWitnessSet.copy(redeemers =
                if allRedeemers.isEmpty then None
                else Some(KeepRaw(Redeemers(allRedeemers*)))
            )
        val updatedTx = editable.transaction
            .focus(_.witnessSet)
            .replace(updatedWitnessSet)
        updatedTx
    }
}

// ============================================================================
// Main transaction editing functions
// ============================================================================

object TransactionEditor {

    /** Edit a transaction, ensuring proper handling of redeemers.
      *
      * You can insert or delete inputs, certificates, mints or reward withdrawals: regardless of
      * the changes, the redeemers will be re-indexed to point to the correct transaction
      * components.
      *
      *   - If you add any new redeemers, and they point to the transaction components correctly,
      *     they are guaranteed to have correct indices in the output tx.
      *   - If some component that has a redeemer pointing to it is removed, the corresponding
      *     redeemer will be removed as well from the resulting transaction.
      *
      * @param f
      *   endomorphism
      * @param tx
      *   source transaction
      * @return
      *   target transaction
      */
    def editTransaction(
        f: Transaction => Transaction
    )(tx: Transaction): Transaction = {
        val editableTx = TransactionConversion.toEditableTransaction(tx)
        val processedTransaction = f(editableTx.transaction)
        val newEditableTx = TransactionConversion.toEditableTransaction(processedTransaction)
        val editedTx = editableTx.copy(
          transaction = processedTransaction,
          redeemers = (editableTx.redeemers ++ newEditableTx.redeemers).distinct
        )
        TransactionConversion.fromEditableTransaction(editedTx)
    }

    /** Like `editTransaction`, but fails if:
      *   - the input transaction's redeemers have invalid `index` pointers
      *   - the resulting transaction's redeemers have invalid `index` pointers
      *
      * The first problematic redeemer will be returned as an error value.
      *
      * @param f
      *   endomorphism
      * @param tx
      *   source transaction
      * @return
      *   target transaction
      */
    def editTransactionSafe(
        f: Transaction => Transaction
    )(
        tx: Transaction
    ): Either[Redeemer, Transaction] = {
        for {
            editableTx <- TransactionConversion.toEditableTransactionSafe(tx)
            processedTx = f(editableTx.transaction)
            newEditableTx <- TransactionConversion.toEditableTransactionSafe(processedTx)
            editedTx = editableTx.copy(
              transaction = processedTx,
              redeemers = (editableTx.redeemers ++ newEditableTx.redeemers).distinct
            )
            // Not using the safe variant: we want to drop stale redeemers
            result = TransactionConversion.fromEditableTransaction(editedTx)
        } yield result
    }
}
