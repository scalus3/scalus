package scalus.examples.editablenft

import scalus.Compile
import scalus.builtin.{ByteString, Data}
import scalus.builtin.Data.{FromData, ToData}
import scalus.ledger.api.v1.{Credential, PolicyId}
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*

case class ReferenceNftDatum(
    tokenId: ByteString,
    data: ByteString,
    isSealed: Boolean
) derives FromData,
      ToData

case class MintRedeemer(tokenId: ByteString) derives FromData, ToData

/** CIP-68 style editable NFT validator.
  *
  * Allows editing the data until the NFT is sealed (via [[ReferenceNftDatum.isSealed]]). After
  * sealing, the data is no longer editable. NFT cannot be unsealed
  *
  * Makes sure that 2 assets exists -- one reference asset (ref NFT) holding the data, and the other
  * asset (user NFT) proving ownership. The editing and sealing can only be done by the owner, and
  * is ensured by requiring a user NFT.
  */
@Compile
object EditableNftValidator extends Validator {

    /** Minting policy: creates paired reference and user NFTs.
      *
      * Redeemer contains the base token name (tokenId) without label prefix. This enforces that
      * both tokens are minted as a matching pair:
      *   - Reference NFT: "100" ++ tokenId
      *   - User NFT: "222" ++ tokenId
      */
    inline def mint(redeemer: Data, policyId: PolicyId, tx: TxInfo): Unit = {
        val tokenId = redeemer.to[MintRedeemer].tokenId

        val refTokenName = REFERENCE_NFT_LABEL ++ tokenId
        val userTokenName = USER_NFT_LABEL ++ tokenId

        // Verify exactly one reference NFT is minted with correct name
        require(
          tx.mint.quantityOf(policyId, refTokenName) === BigInt(1),
          "Must mint exactly 1 reference NFT"
        )

        // Verify exactly one user NFT is minted with correct name
        require(
          tx.mint.quantityOf(policyId, userTokenName) === BigInt(1),
          "Must mint exactly 1 user NFT"
        )

        // Find the reference NFT output - must be at script address with inline datum
        val refOutput = tx.outputs
            .find { out =>
                out.address.credential match
                    case Credential.ScriptCredential(hash) =>
                        hash === policyId &&
                        out.value.quantityOf(policyId, refTokenName) === BigInt(1)
                    case _ => false
            }
            .getOrFail("Reference NFT must be sent to script address")

        // Validate datum structure and content
        val datum = refOutput.datum match
            case OutputDatum.OutputDatum(d) => d.to[ReferenceNftDatum]
            case _                          => fail("Reference NFT must have inline datum")

        // Datum tokenId must match - creates the link that spend validator uses
        require(datum.tokenId === tokenId, "Datum tokenId must match minted token")
        require(!datum.isSealed, "New NFT must not be sealed")
    }

    /** Spending validator: enforces edit-until-sealed policy.
      *
      * To spend the reference NFT, the user token must be in transaction inputs.
      */
    inline def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val oldDatum = datum.getOrFail("Datum required").to[ReferenceNftDatum]

        // Get script address and policy ID from own input
        val ownInput = tx.findOwnInputOrFail(ownRef)
        val scriptAddress = ownInput.resolved.address
        val policyId = scriptAddress.credential match
            case Credential.ScriptCredential(hash) => hash
            case _                                 => fail("Expected script credential")

        // Ownership check: user token must be in transaction inputs
        val userTokenName = USER_NFT_LABEL ++ oldDatum.tokenId
        val hasUserToken = tx.inputs.exists { input =>
            input.resolved.value.quantityOf(policyId, userTokenName) === BigInt(1)
        }
        require(hasUserToken, "User token must be in inputs")

        // Find continuation output (reference NFT going back to script)
        val refTokenName = REFERENCE_NFT_LABEL ++ oldDatum.tokenId
        val newOutput = tx.outputs
            .find { out =>
                out.address === scriptAddress &&
                out.value.quantityOf(policyId, refTokenName) === BigInt(1)
            }
            .getOrFail("Reference NFT must return to script")

        val newDatum = newOutput.datum match
            case OutputDatum.OutputDatum(d) => d.to[ReferenceNftDatum]
            case _                          => fail("Continuation must have inline datum")

        // Invariant: tokenId can never change
        require(newDatum.tokenId === oldDatum.tokenId, "Token ID immutable")

        // Sealed policy enforcement
        if oldDatum.isSealed then {
            // Already sealed: nothing can change
            require(newDatum.data === oldDatum.data, "Sealed: data immutable")
            require(newDatum.isSealed, "Sealed: cannot unseal")
        }
    }

    inline def REFERENCE_NFT_LABEL: ByteString = ByteString.fromString("100")
    inline def USER_NFT_LABEL: ByteString = ByteString.fromString("222")
}
