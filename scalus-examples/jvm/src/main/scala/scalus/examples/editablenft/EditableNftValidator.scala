package scalus.examples.editablenft

import scalus.Compile
import scalus.builtin.{ByteString, Data}
import scalus.builtin.Data.{FromData, ToData}
import scalus.ledger.api.v1.{Credential, PolicyId}
import scalus.ledger.api.v2.OutputDatum
import scalus.builtin.Data.toData
import scalus.ledger.api.v3.*
import scalus.prelude.*

case class ReferenceNftParam(seed: TxOutRef) derives FromData, ToData

case class ReferenceNftDatum(
    tokenId: ByteString,
    data: ByteString,
    isSealed: Boolean
) derives FromData,
      ToData

@Compile
object ReferenceNftDatum {

    extension (self: ReferenceNftDatum) {
        inline def refNftName: ByteString = EditableNftValidator.refNftName(self.tokenId)
        inline def userNftName: ByteString = EditableNftValidator.userNftName(self.tokenId)
    }
}

enum MintRedeemer derives FromData, ToData {
    case Mint(seedIndex: BigInt, refNftOutIndex: BigInt)
    case Burn
}

enum SpendRedeemer derives FromData, ToData {
    case Spend(userNftInputIndex: BigInt, refNftOutputIndex: BigInt)
    case Burn(userNftInputIndex: BigInt)
}

/** CIP-68 style editable NFT validator.
  *
  * Allows editing the data until the NFT is sealed (via [[ReferenceNftDatum.isSealed]]). After
  * sealing, the data is no longer editable. NFT cannot be unsealed
  *
  * Makes sure that 2 assets exists -- one reference asset (ref NFT) holding the data, and the other
  * asset (user NFT) proving ownership. The editing and sealing can only be done by the owner, and
  * is ensured by requiring a user NFT
  */
@Compile
object EditableNftValidator extends DataParameterizedValidator {

    /** Minting policy: creates paired reference and user NFTs.
      *
      * Redeemer contains the base token name (tokenId) without label prefix. This enforces that
      * both tokens are minted as a matching pair:
      *   - Reference NFT: "100" ++ tokenId
      *   - User NFT: "222" ++ tokenId
      */
    inline def mint(param: Data, redeemer: Data, policyId: PolicyId, tx: TxInfo): Unit = {
        val seed = param.to[ReferenceNftParam].seed
        val r = redeemer.to[MintRedeemer]
        r match {
            case MintRedeemer.Mint(seedIndex, refNftOutIndex) =>
                val isSpent = tx.inputs.get(seedIndex).isDefined
                require(isSpent, "Must spend seed UTxO")

                // Find the reference NFT output - must be at script address with inline datum
                val refNftOutput = tx.outputs.at(refNftOutIndex)

                // Validate datum structure and content
                val datum = refNftOutput.datum match
                    case OutputDatum.OutputDatum(d) => d.to[ReferenceNftDatum]
                    case _                          => fail("Reference NFT must have inline datum")

                val refTokenName = EditableNftValidator.refNftName(datum.tokenId)
                val userTokenName = EditableNftValidator.userNftName(datum.tokenId)

                refNftOutput.address.credential match
                    case Credential.ScriptCredential(hash) =>
                        val policyIdMatches = hash === policyId
                        val exactlyOneRefNft =
                            refNftOutput.value.quantityOf(policyId, refTokenName) === BigInt(1)
                        val isPreserved = policyIdMatches && exactlyOneRefNft

                        require(isPreserved, ReferenceNftMustBePreserved)
                    case _ => fail(ReferenceNftMustBePreserved)

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
            case MintRedeemer.Burn =>
            // handled by in the `spend`
        }
    }

    /** Spending validator: enforces edit-until-sealed policy.
      *
      * To spend the reference NFT, the user token must be in transaction inputs.
      */
    inline def spend(
        param: Data,
        d: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val datum = d.getOrFail("Datum required").to[ReferenceNftDatum]
        val ownInput = tx.findOwnInputOrFail(ownRef)
        val scriptAddress = ownInput.resolved.address
        val policyId = scriptAddress.credential match
            case Credential.ScriptCredential(hash) => hash
            case _                                 => fail("Expected script credential")

        val userTokenName = EditableNftValidator.userNftName(datum.tokenId)
        val refTokenName = EditableNftValidator.refNftName(datum.tokenId)

        redeemer.to[SpendRedeemer] match {
            case SpendRedeemer.Spend(userNftInputIndex, refNftOutputIndex) => {
                val userTokenInput = tx.inputs.at(userNftInputIndex)
                val hasUserToken =
                    userTokenInput.resolved.value.quantityOf(policyId, userTokenName) === BigInt(1)

                require(hasUserToken, "Must present user token to edit the reference NFT")

                val newOutput = tx.outputs.at(refNftOutputIndex)
                val correctAddress = newOutput.address === scriptAddress
                val correctQuantity =
                    newOutput.value.quantityOf(policyId, refTokenName) === BigInt(1)
                val validContinuation = correctAddress && correctQuantity
                require(validContinuation, "Must return reference NFT to the script address")

                val newDatum = newOutput.datum match
                    case OutputDatum.OutputDatum(d) => d.to[ReferenceNftDatum]
                    case _                          => fail("Continuation must have inline datum")

                // Sealed policy enforcement
                if datum.isSealed then
                    // check the entire datum
                    require(newDatum.toData === d.get, "Sealed NFTs are immutable")
                else
                    // just check the token id, rest is ok to change
                    require(newDatum.tokenId === datum.tokenId, "Token ID immutable")
            }
            case SpendRedeemer.Burn(userNftInputIndex) => {
                val refNftName = datum.refNftName
                val userNftName = datum.userNftName

                val isRefNftBurned = tx.mint.quantityOf(policyId, refNftName) === BigInt(-1)
                require(isRefNftBurned, "Must burn ref NFT")
                val isUserNftBurned = tx.mint.quantityOf(policyId, userNftName) === BigInt(-1)
                require(isUserNftBurned, "Must burn ref NFT")
            }
        }

    }

    inline def userNftName(tokenId: ByteString): ByteString =
        ByteString.fromString("100") ++ tokenId
    inline def refNftName(tokenId: ByteString): ByteString = ByteString.fromString("222") ++ tokenId

    private inline val ReferenceNftMustBePreserved = "Reference NFT must go to this script address"
}
