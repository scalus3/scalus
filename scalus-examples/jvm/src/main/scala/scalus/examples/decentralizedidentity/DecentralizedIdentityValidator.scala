package scalus.examples.decentralizedidentity

import scalus.compiler.Compile
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Builtins.blake2b_224
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.cardano.onchain.plutus.v1.{PolicyId, PosixTime, PubKeyHash}
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v3.DataParameterizedValidator

// ===== Data types =====

/** Datum for the identity NFT UTXO */
case class IdentityDatum(ownerPkh: PubKeyHash) derives FromData, ToData

/** Datum for a delegation token UTXO */
case class DelegationDatum(
    identityTokenName: ByteString,
    delegatePkh: PubKeyHash,
    validFrom: PosixTime,
    validUntil: PosixTime,
    delegateType: ByteString
) derives FromData,
      ToData

/** Datum for an attribute token UTXO */
case class AttributeDatum(
    identityTokenName: ByteString,
    key: ByteString,
    value: ByteString
) derives FromData,
      ToData

// ===== Redeemers =====

enum MintAction derives FromData, ToData {
    case CreateIdentity(seedIndex: BigInt, identityOutIndex: BigInt)
    case AddDelegate(identityRefInputIndex: BigInt, delegationOutIndex: BigInt)
    case PublishAttribute(delegationRefInputIndex: BigInt, attributeOutIndex: BigInt)
    case Burn
}

enum SpendAction derives FromData, ToData {
    case TransferOwnership(newOwnerPkh: PubKeyHash, identityOutIndex: BigInt)
    case RevokeDelegate
    case RevokeAttribute
}

/** Decentralized Identity (SSI) validator.
  *
  * A single minting policy + spending validator for Self-Sovereign Identity on Cardano. Token name
  * prefixes distinguish identity, delegation, and attribute tokens:
  *   - Identity: "i" ++ uniqueId (computed off-chain, 29 bytes)
  *   - Delegation: "d" ++ blake2b_224(identityTn ++ delegatePkh) (29 bytes)
  *   - Attribute: "a" ++ blake2b_224(identityTn ++ key) (29 bytes)
  *
  * Identity tokens hold an IdentityDatum with the current owner PubKeyHash. Delegation and
  * attribute tokens are non-transferable (must remain at script address or be burned).
  */
@Compile
object DecentralizedIdentityValidator extends DataParameterizedValidator {

    // ===== Token naming helpers =====

    /** Identity token: "i" ++ uniqueId = 29 bytes */
    inline def identityTokenName(uniqueId: ByteString): ByteString =
        ByteString.fromString("i") ++ uniqueId

    /** Delegation token: "d" ++ blake2b_224(identityTn ++ delegatePkh) = 29 bytes */
    inline def delegationTokenName(
        identityTn: ByteString,
        delegatePkh: PubKeyHash
    ): ByteString =
        ByteString.fromString("d") ++ blake2b_224(identityTn ++ delegatePkh.hash)

    /** Attribute token: "a" ++ blake2b_224(identityTn ++ key) = 29 bytes */
    inline def attributeTokenName(identityTn: ByteString, key: ByteString): ByteString =
        ByteString.fromString("a") ++ blake2b_224(identityTn ++ key)

    // ===== Minting policy =====

    inline def mint(param: Data, redeemer: Data, policyId: PolicyId, tx: TxInfo): Unit = {
        val r = redeemer.to[MintAction]
        r match {
            case MintAction.CreateIdentity(seedIndex, identityOutIndex) =>
                // One-shot: must spend the exact parameterized seed UTXO
                val seedRef = param.to[TxOutRef]
                val spentInput = tx.inputs.at(seedIndex)
                require(spentInput.outRef === seedRef, "Must spend the parameterized seed UTxO")

                // Identity output must be at script address with inline datum
                val identityOutput = tx.outputs.at(identityOutIndex)
                identityOutput.datum.inlineOrFail[IdentityDatum]("Identity must have inline datum")

                // Output must be at own script address
                require(
                  identityOutput.address.credential
                      .scriptHashOrFail("Identity must go to script address") === policyId,
                  "Identity must go to script address"
                )

                // Find the identity token name from the output (starts with "i")
                val idTn = findTokenWithPrefix(identityOutput.value, policyId, "i")

                // Exactly one identity token minted, nothing else under this policy
                require(
                  tx.mint.hasOnly(policyId, idTn, 1),
                  "Must mint exactly 1 identity token and nothing else"
                )

            case MintAction.AddDelegate(identityRefInputIndex, delegationOutIndex) =>
                // Identity must be present as reference input
                val identityRefInput = tx.referenceInputs.at(identityRefInputIndex)
                val identityDatum = identityRefInput.resolved.datum
                    .inlineOrFail[IdentityDatum]("Identity ref input must have inline datum")

                // Must be signed by identity owner
                require(
                  tx.isSignedBy(identityDatum.ownerPkh),
                  "Must be signed by identity owner"
                )

                // Find the identity token name from the reference input
                val identityTn =
                    findTokenWithPrefix(identityRefInput.resolved.value, policyId, "i")

                // Delegation output must be at script address
                val delegationOutput = tx.outputs.at(delegationOutIndex)
                require(
                  delegationOutput.address.credential
                      .scriptHashOrFail("Delegation must go to script address") === policyId,
                  "Delegation must go to script address"
                )

                // Check delegation datum
                val delegDatum = delegationOutput.datum.inlineOrFail[DelegationDatum](
                  "Delegation must have inline datum"
                )

                require(
                  delegDatum.identityTokenName === identityTn,
                  "Delegation must reference correct identity"
                )

                // Note: self-delegation (owner == delegate) is intentionally allowed.
                // The owner cannot publish attributes directly — only delegates can.
                // Without self-delegation, the owner would have no way to publish attributes
                // about their own identity.

                // Build expected token name and check minting
                val delegTn = delegationTokenName(identityTn, delegDatum.delegatePkh)
                require(
                  tx.mint.hasOnly(policyId, delegTn, 1),
                  "Must mint exactly 1 delegation token and nothing else"
                )

                require(
                  delegationOutput.value.hasNft(policyId, delegTn),
                  "Delegation output must hold the delegation token"
                )

            case MintAction.PublishAttribute(delegationRefInputIndex, attributeOutIndex) =>
                // Delegation must be present as reference input
                val delegationRefInput = tx.referenceInputs.at(delegationRefInputIndex)
                val delegDatum = delegationRefInput.resolved.datum
                    .inlineOrFail[DelegationDatum]("Delegation ref input must have inline datum")

                // Delegation must be at script address (proving it's valid/non-forged)
                require(
                  delegationRefInput.resolved.address.credential
                      .scriptHashOrFail("Delegation must be at script address") === policyId,
                  "Delegation must be at script address"
                )

                // The delegation must actually hold its delegation token. Being at the script
                // address with a datum-shaped value is not enough — anyone can pay a forged
                // DelegationDatum there. The token is minted only by AddDelegate (which requires the
                // identity owner's signature) and burned by RevokeDelegate, so requiring it both
                // proves the delegation is genuine and makes revocation effective.
                val delegTn =
                    delegationTokenName(delegDatum.identityTokenName, delegDatum.delegatePkh)
                require(
                  delegationRefInput.resolved.value.hasNft(policyId, delegTn),
                  "Delegation reference input must hold the delegation token"
                )

                // Must be signed by delegate
                require(
                  tx.isSignedBy(delegDatum.delegatePkh),
                  "Must be signed by delegate"
                )

                // Check delegation validity: entire tx validity range must fall within delegation period
                val txStartTime =
                    tx.validFromOrFail("Transaction must have a finite lower validity bound")
                require(txStartTime >= delegDatum.validFrom, "Delegation not yet valid")

                val txEndTime =
                    tx.validToOrFail("Transaction must have a finite upper validity bound")
                require(txEndTime <= delegDatum.validUntil, "Delegation expired")

                // Attribute output must be at script address
                val attributeOutput = tx.outputs.at(attributeOutIndex)
                require(
                  attributeOutput.address.credential
                      .scriptHashOrFail("Attribute must go to script address") === policyId,
                  "Attribute must go to script address"
                )

                // Check attribute datum
                val attrDatum = attributeOutput.datum.inlineOrFail[AttributeDatum](
                  "Attribute must have inline datum"
                )

                require(
                  attrDatum.identityTokenName === delegDatum.identityTokenName,
                  "Attribute must reference correct identity"
                )

                // Build expected token name and check minting
                val attrTn = attributeTokenName(delegDatum.identityTokenName, attrDatum.key)
                require(
                  tx.mint.hasOnly(policyId, attrTn, 1),
                  "Must mint exactly 1 attribute token and nothing else"
                )

                require(
                  attributeOutput.value.hasNft(policyId, attrTn),
                  "Attribute output must hold the attribute token"
                )

            case MintAction.Burn =>
                // Every quantity under this policy must be negative; onlyBurnsUnder also
                // rejects an empty sub-map, where a bare forall would be vacuously true.
                require(tx.onlyBurnsUnder(policyId), "Burn action must only burn tokens")
        }
    }

    // ===== Spending validator =====

    inline def spend(
        param: Data,
        d: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val ownInput = tx.findInputOrFail(ownRef)
        val scriptAddress = ownInput.resolved.address
        val policyId = scriptAddress.credential.scriptHashOrFail("Expected script credential")

        redeemer.to[SpendAction] match {
            case SpendAction.TransferOwnership(newOwnerPkh, identityOutIndex) =>
                // Only identity tokens can be transferred
                val datum = d.getOrFail("Datum required").to[IdentityDatum]

                // Find identity token name on the input
                val identityTn =
                    findTokenWithPrefix(ownInput.resolved.value, policyId, "i")

                // Must be signed by current owner
                require(
                  tx.isSignedBy(datum.ownerPkh),
                  "Must be signed by current owner"
                )

                // Must be signed by new owner (accept transfer)
                require(
                  tx.isSignedBy(newOwnerPkh),
                  "Must be signed by new owner"
                )

                // Identity token must be returned to script address with new datum
                val newOutput = tx.outputs.at(identityOutIndex)
                require(
                  newOutput.address.credential
                      .scriptHashOrFail("Identity must return to script address") === policyId,
                  "Identity must return to script address"
                )

                require(
                  newOutput.value.hasNft(policyId, identityTn),
                  "Must return identity token"
                )

                val newDatum = newOutput.datum.inlineOrFail[IdentityDatum]("Must have inline datum")

                require(newDatum.ownerPkh === newOwnerPkh, "Datum must reflect new owner")

            case SpendAction.RevokeDelegate =>
                // Find the delegation token on the input being spent
                val delegTn = findTokenWithPrefix(ownInput.resolved.value, policyId, "d")
                val datum = d.getOrFail("Datum required").to[DelegationDatum]

                // Must be signed by identity owner: find identity via reference inputs
                val ownerPkh = findIdentityOwner(tx, policyId, datum.identityTokenName)

                require(
                  tx.isSignedBy(ownerPkh),
                  "Must be signed by identity owner to revoke"
                )

                // Delegation token must be burned
                require(
                  tx.mint.quantityOf(policyId, delegTn) === BigInt(-1),
                  "Must burn delegation token"
                )

            case SpendAction.RevokeAttribute =>
                val attrDatum = d.getOrFail("Datum required").to[AttributeDatum]
                val attrTn = findTokenWithPrefix(ownInput.resolved.value, policyId, "a")

                // Must be signed by identity owner: find identity via reference inputs
                val ownerPkh = findIdentityOwner(tx, policyId, attrDatum.identityTokenName)

                require(
                  tx.isSignedBy(ownerPkh),
                  "Must be signed by identity owner to revoke"
                )

                // Attribute token must be burned
                require(
                  tx.mint.quantityOf(policyId, attrTn) === BigInt(-1),
                  "Must burn attribute token"
                )
        }
    }

    // ===== Helpers =====

    /** Find a token at the given UTXO value that starts with a specific prefix. Returns its full
      * token name.
      */
    private inline def findTokenWithPrefix(
        value: Value,
        policyId: PolicyId,
        prefix: String
    ): ByteString = {
        val prefixBs = ByteString.fromString(prefix)
        val tokenMap = value.tokens(policyId)
        // Find first token whose name starts with the prefix
        tokenMap
            .find { case (tn, qty) =>
                qty > 0 && tn.take(prefixBs.length) === prefixBs
            }
            .map(_._1)
            .getOrFail("Token with prefix not found")
    }

    /** Find the identity owner by looking up the identity token in reference inputs. */
    private inline def findIdentityOwner(
        tx: TxInfo,
        policyId: PolicyId,
        identityTokenName: ByteString
    ): PubKeyHash = {
        val identityRefInput = tx.referenceInputs
            .find { txInInfo =>
                txInInfo.resolved.value.hasNft(policyId, identityTokenName)
            }
            .getOrFail("Identity reference input not found")

        identityRefInput.resolved.datum
            .inlineOrFail[IdentityDatum]("Identity must have inline datum")
            .ownerPkh
    }
}
