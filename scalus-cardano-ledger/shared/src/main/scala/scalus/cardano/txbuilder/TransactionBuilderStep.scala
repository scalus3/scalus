package scalus.cardano.txbuilder

import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.uplc.builtin.Data

sealed trait TransactionBuilderStep

/** Steps to build the transaction:
  *   - generally non-commutative, so the order matters
  *   - some are additive (e.g. [[Mint]]), some are not, e.g., ([[Spend]])
  */
object TransactionBuilderStep {

    /** Spend any utxo. An attempt to consume (reference or spend) the same utxo twice will error.
      * If a reference script is used, the containing utxo should be added beforehand with
      * [[ReferenceOutput]] or [[Spend]] steps.
      */
    case class Spend(
        utxo: Utxo,
        witness: SpendWitness = PubKeyWitness
    ) extends TransactionBuilderStep

    /** Send some funds/data to an address. Multiple identical steps are acceptable. */
    case class Send(output: TransactionOutput) extends TransactionBuilderStep

    /** Like [[Send]], but the inline datum is computed from the fully-assembled transaction.
      *
      * The [[datumBuilder]] receives the sorted, finalised [[Transaction]] (same object passed to
      * delayed-redeemer builders) and must return the [[Data]] to embed as an inline datum. The
      * output is first added with a placeholder datum; the placeholder is replaced just before the
      * transaction is finalised, after all delayed redeemers have also been resolved.
      */
    case class SendWithDatumBuilder(
        output: TransactionOutput,
        datumBuilder: Transaction => Data
    ) extends TransactionBuilderStep

    /** Mint/burn tokens using a native/plutus script. Additive - sum monoid over amount. You should
      * determine your aggregate mint amount _outside_ of the builder. Chaining steps together to
      * calculate the mint amount.
      *
      * @throws IllegalArgumentException
      *   if amount is zero
      *
      * WARNING: If you do a reciprocal pair of mint/burn of the same amount (i.e., Mint 4, Mint
      * -4), you will nullify the mint amount, but the requiredSigners/witnesses will not be
      * removed.
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

    /** Requires a public key hash to sign the transaction. */
    case class RequireSignature(signer: AddrKeyHash) extends TransactionBuilderStep
}
