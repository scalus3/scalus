package scalus.cardano.txbuilder

import scalus.uplc.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.*

import scala.annotation.nowarn

sealed trait TransactionBuilderStep

/** Steps to build the transaction:
  *   - generally non-commutative, so the order matters
  *   - some are additive (e.g. [[Mint]]), some are not, e.g., ([[Spend]])
  */
@nowarn("msg=SpendWithDelayedRedeemer .* is deprecated")
object TransactionBuilderStep {

    /** Spend any utxo. An attempt to consume (reference or spend) the same utxo twice will error.
      * If a reference script is used, the containing utxo should be added beforehand with
      * [[ReferenceOutput]] or [[Spend]] steps.
      */
    case class Spend(
        utxo: Utxo,
        witness: SpendWitness = PubKeyWitness
    ) extends TransactionBuilderStep

    /** Spend a utxo guarded by plutus script.
      *
      * The [[redeemerBuilder]] is invoked after [[TransactionBuilder.build()]], but before it's
      * balanced by the low lever builder. As a result, the number and order of inputs, outputs,
      * certificates etc. is predetermined.
      *
      * Use this instead of [[Spend]] when assembling the redeemer requires the knowledge of the
      * transaction contents, e.g. to include the indices of inputs or outputs.
      */
    @deprecated("Use Spend directly", "0.13.0")
    case class SpendWithDelayedRedeemer(
        utxo: Utxo,
        redeemerBuilder: Transaction => Data,
        validator: PlutusScript,
        datum: Option[Data] = None
    ) extends TransactionBuilderStep

    /** Send some funds/data to an address. Multiple identical steps are acceptable. */
    case class Send(output: TransactionOutput) extends TransactionBuilderStep

    /** Mint/burn tokens using a native/plutus script. Additive - sum monoid over amount. You should
      * determine your aggregate mint amount _outside_ of the builder. Chaining steps together to
      * calculate the mint amount.
      *
      * @throws IllegalArgumentException
      *   if amount is zero
      *
      * WARNING: If you do a reciprocal pair of mint/burn of the same amount (i.e., Mint 4, Mint
      * -4), you will nullify the mint amount, but the additionalSigners/requiredSigners/witnesses
      * will not be removed.
      */
    case class Mint(
        scriptHash: ScriptHash,
        assetName: AssetName,
        amount: Long,
        witness: NativeScriptWitness | TwoArgumentPlutusScriptWitness
    ) extends TransactionBuilderStep {
        require(amount != 0, "Mint amount cannot be zero")
    }

    /** Add a [[scalus.cardano.ledger.Utxo]] as a CIP-31 reference input. Consuming the same UTxO
      * twice (reference or spend) is an error
      *
      * The reason that action is represented as a step is that reference utxos should be added to
      * the context and also may be required to create a [[scalus.cardano.txbuilder.SpendWitness]].
      *
      * @param utxo
      *   any utxo
      */
    case class ReferenceOutput(utxo: Utxo) extends TransactionBuilderStep

    /** Set the minimal fee. */
    case class Fee(fee: Coin) extends TransactionBuilderStep

    /** Set transactions validity start slot, can be used once.
      */
    case class ValidityStartSlot(slot: Long) extends TransactionBuilderStep

    /** Set transaction validity end slot (aka TTL), can be used once. */
    case class ValidityEndSlot(slot: Long) extends TransactionBuilderStep

    /** Add a utxo as a collateral input. The utxo must be controlled by a key (not a script).
      *
      * Per Babbage era specification (CIP-40), native tokens in collateral inputs are allowed. All
      * tokens will be returned via the collateral return output. Use [[SetCollateralReturn]] to
      * specify the return address, or it will default to the address of the first collateral input.
      */
    case class AddCollateral(
        utxo: Utxo
    ) extends TransactionBuilderStep

    /** Set the address where collateral return output should be sent.
      *
      * This is optional - if not set, the first collateral input's address will be used. Use this
      * when you want the collateral return to go to a different address than where the collateral
      * came from.
      *
      * The collateral return output is automatically created during transaction finalization when
      * collateral contains native tokens (required by protocol) or when excess ADA above the
      * required collateral can cover the min ADA for a return output.
      */
    case class SetCollateralReturn(returnAddress: Address) extends TransactionBuilderStep

    case class ModifyAuxiliaryData(f: Option[AuxiliaryData] => Option[AuxiliaryData])
        extends TransactionBuilderStep

    case class IssueCertificate(
        cert: Certificate,
        witness: PubKeyWitness.type | NativeScriptWitness | TwoArgumentPlutusScriptWitness =
            PubKeyWitness
    ) extends TransactionBuilderStep

    case class WithdrawRewards(
        stakeCredential: Credential,
        amount: Coin,
        witness: PubKeyWitness.type | NativeScriptWitness | TwoArgumentPlutusScriptWitness =
            PubKeyWitness
    ) extends TransactionBuilderStep

    case class SubmitProposal(
        proposal: ProposalProcedure,
        witness: PubKeyWitness.type | NativeScriptWitness | TwoArgumentPlutusScriptWitness =
            PubKeyWitness
    ) extends TransactionBuilderStep

    case class SubmitVotingProcedure(
        voter: Voter,
        votes: Map[GovActionId, VotingProcedure],
        witness: PubKeyWitness.type | NativeScriptWitness | TwoArgumentPlutusScriptWitness =
            PubKeyWitness
    ) extends TransactionBuilderStep
}
