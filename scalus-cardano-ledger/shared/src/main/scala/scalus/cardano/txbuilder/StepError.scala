package scalus.cardano.txbuilder

import io.bullet.borer.Cbor
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.TransactionBuilder.Operation

/** Internal error indicating a redeemer could not be attached to any transaction component.
  *
  * This error occurs when a detached redeemer's purpose does not match any input, mint, withdrawal,
  * certificate, vote, or proposal in the transaction. This typically indicates an internal
  * inconsistency in the transaction builder - a redeemer was tracked but its corresponding
  * transaction component was removed or never added.
  *
  * @param detachedRedeemer
  *   the redeemer that could not be attached, containing its data and purpose
  * @param steps
  *   the sequence of transaction builder steps that were processed
  */
case class RedeemerIndexingInternalError(
    detachedRedeemer: DetachedRedeemer,
    steps: Seq[TransactionBuilderStep]
)

/** Base trait for errors that occur during transaction building step processing.
  *
  * Each error captures the specific [[TransactionBuilderStep]] that caused the failure, allowing
  * users to identify which operation in their transaction building sequence failed. The `explain`
  * method provides a human-readable description of the error.
  *
  * @see
  *   [[TransactionBuilder.build]] for the main entry point that may return these errors
  * @see
  *   [[SomeBuildError.SomeStepError]] for how step errors are wrapped in the build result
  */
sealed trait StepError {

    /** The transaction builder step that caused this error */
    def step: TransactionBuilderStep

    /** Human-readable explanation of the error */
    def explain: String
}

object StepError {
    case class Unimplemented(description: String, step: TransactionBuilderStep) extends StepError {
        override def explain: String = s"$description is not yet implemented. If you need it, " +
            s"submit a request at $bugTrackerUrl."
    }

    // TODO: Remove this error and just use a nonzero type
    case class CannotMintZero(
        scriptHash: ScriptHash,
        assetName: AssetName,
        step: TransactionBuilderStep
    ) extends StepError {
        override def explain: String =
            "You cannot pass a \"amount = zero\" to a mint step, but we recieved it for" +
                s"(policyId, assetName) == ($scriptHash, $assetName)." +
                "\n You should not use the Mint step to calculate your mint amounts."
    }

    // TODO: more verbose error -- we'll need to pass more information from the assertion/step.
    case class InputAlreadyExists(input: TransactionInput, step: TransactionBuilderStep)
        extends StepError {
        override def explain: String =
            s"The transaction input $input already exists in the transaction as " +
                "either an input or reference input."
    }

    case class ResolvedUtxosIncoherence(
        input: TransactionInput,
        existingOutput: TransactionOutput,
        incoherentOutput: TransactionOutput,
        step: TransactionBuilderStep
    ) extends StepError {
        override def explain: String =
            "The context's resolvedUtxos already contain an input associated with a different output." +
                s"\nInput: $input" +
                s"\nExisting Output: $existingOutput" +
                s"\nIncoherent Output: $incoherentOutput"
    }

    case class CollateralNotPubKey(utxo: Utxo, step: TransactionBuilderStep) extends StepError {
        override def explain: String =
            s"The UTxO passed as a collateral input is not a PubKey UTxO. UTxO: $utxo"
    }

    // TODO: This error could probably be improved.
    case class CannotExtractSignatures(step: TransactionBuilderStep) extends StepError {
        override def explain: String =
            s"Could not extract signatures via _.additionalSigners from $step"
    }

    case class DatumIsMissing(utxo: Utxo, step: TransactionBuilderStep) extends StepError {
        override def explain: String =
            s"Given witness to spend an output requires a datum that is missing: $utxo"
    }

    case class IncorrectDatumHash(
        utxo: Utxo,
        datum: Data,
        datumHash: DataHash,
        step: TransactionBuilderStep
    ) extends StepError {
        override def explain
            : String = "You provided a `DatumWitness` with a datum that does not match the datum hash present in a transaction output.\n " +
            s" Datum: $datum (CBOR: ${ByteString.fromArray(Cbor.encode(datum).toByteArray).toHex})\n  " +
            s" Datum hash: ${datumHash.toHex}\n  " +
            s"UTxO: $utxo"
    }

    case class IncorrectScriptHash(
        script: Script,
        hash: ScriptHash,
        step: TransactionBuilderStep
    ) extends StepError {
        override def explain: String = script match {
            case nativeScript: Script.Native =>
                s"Provided script hash ($hash) does not match the provided native script ($nativeScript)"
            case plutusScript: PlutusScript =>
                s"Provided script hash ($hash) does not match the provided Plutus script ($plutusScript)"
        }
    }

    case class RedeemerComputationFailed(message: String, step: TransactionBuilderStep)
        extends StepError {
        override def explain: String =
            s"Failed to compute delayed redeemer: $message"
    }

    case class WrongOutputType(
        expectedType: WitnessKind,
        utxo: Utxo,
        step: TransactionBuilderStep
    ) extends StepError {
        override def explain: String =
            s"The UTxO you provided has the wrong type. We require a `$expectedType`. " +
                s"UTxO: $utxo"
    }

    case class WrongCredentialType(
        action: Operation,
        expectedType: WitnessKind,
        cred: Credential,
        step: TransactionBuilderStep
    ) extends StepError {
        override def explain: String =
            s"${action.explain} ($action) requires a $expectedType witness: $cred"
    }

    case class DatumWitnessNotProvided(utxo: Utxo, step: TransactionBuilderStep) extends StepError {
        override def explain: String =
            "The output you are trying to spend contains a datum hash, you need to provide " +
                s"a `DatumValue`, output: $utxo"
    }

    case class DatumValueForUtxoWithInlineDatum(
        utxo: Utxo,
        datum: Datum,
        step: TransactionBuilderStep
    ) extends StepError {
        override def explain: String =
            "You can't provide datum value for a utxo with inlined datum: " +
                s"You tried to provide: $datum for the UTxO: $utxo"
    }

    case class UnneededDeregisterWitness(
        stakeCredential: Credential,
        witness: PubKeyWitness.type | TwoArgumentPlutusScriptWitness | NativeScriptWitness,
        step: TransactionBuilderStep
    ) extends StepError {
        override def explain: String =
            "You've provided an optional `CredentialWitness`, " +
                "but the stake credential you are trying to issue a deregistering certificate for " +
                "is a PubKeyHash credential. You should omit the provided credential witness for this " +
                s"credential: $stakeCredential. Provided witness: $witness"
    }

    case class UnneededSpoVoteWitness(
        cred: Credential,
        witness: TwoArgumentPlutusScriptWitness | NativeScriptWitness,
        step: TransactionBuilderStep
    ) extends StepError {
        override def explain: String =
            "You've provided an optional `CredentialWitness`, but the corresponding Voter is " +
                "SPO (Stake Pool Operator). You should omit the provided credential witness " +
                s"for this credential: $cred. Provided witness: $witness"
    }

    case class UnneededProposalPolicyWitness(
        proposal: ProposalProcedure,
        witness: PubKeyWitness.type | TwoArgumentPlutusScriptWitness | NativeScriptWitness,
        step: TransactionBuilderStep
    ) extends StepError {
        override def explain: String =
            "You've provided an optional `CredentialWitness`, but the corresponding proposal" +
                " does not need to validate against the proposal policy. You should omit the " +
                s"provided credential witness for this proposal: $proposal. Provided witness: $witness"
    }

    case class RedeemerIndexingError(redeemer: Redeemer, step: TransactionBuilderStep)
        extends StepError {
        override def explain: String =
            s"Redeemer indexing error. Problematic redeemer that does not have a valid index: $redeemer"
    }

    private val bugTrackerUrl: String = "https://github.com/cardano-hydrozoa/hydrozoa/issues"

    case class WrongNetworkId(address: Address, step: TransactionBuilderStep) extends StepError {
        override def explain: String =
            "The following `Address` that was specified in one of the UTxOs has a `NetworkId`" +
                s" different from the one `TransactionBody` has: $address"
    }

    case class AttachedScriptNotFound(scriptHash: ScriptHash, step: TransactionBuilderStep)
        extends StepError {
        override def explain: String =
            s"No witness or ref/spent output is found for script matching $scriptHash. " +
                "Note that the builder steps are not commutative: you must attach the script " +
                "before using an AttachedScript ScriptWitness."
    }

    case class ByronAddressesNotSupported(address: Address, step: TransactionBuilderStep)
        extends StepError {
        override def explain: String =
            s"Byron addresses are not supported: $address."
    }

    // We can't really return meaningful information, because scalus doesn't
    // provide it. See [[assertAttachedScriptExists]]
    case class ScriptResolutionError(step: TransactionBuilderStep) extends StepError {
        override def explain: String =
            "An error was returned when trying to resolve scripts for the transaction."
    }

    case class FeeAlreadySet(currentFee: Long, step: TransactionBuilderStep) extends StepError {
        override def explain: String =
            s"The fee ($currentFee) is already set. You cannot set the fee more than once."
    }

    case class ValidityStartSlotAlreadySet(slot: Long, step: TransactionBuilderStep)
        extends StepError {
        override def explain: String =
            s"The validity start slot ($slot) is already set. You cannot set the validity start slot more than once."
    }

    case class ValidityEndSlotAlreadySet(slot: Long, step: TransactionBuilderStep)
        extends StepError {
        override def explain: String =
            s"The validity end slot ($slot) is already set. You cannot set the validity end slot more than once."
    }
}
