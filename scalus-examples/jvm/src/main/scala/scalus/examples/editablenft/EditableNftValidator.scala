package scalus.examples.editablenft

import scalus.compiler.Compile
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.cardano.onchain.plutus.v1.{Credential, PolicyId}
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*

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
        val seed = param.to[TxOutRef]
        val r = redeemer.to[MintRedeemer]
        r match {
            case MintRedeemer.Mint(seedIndex, refNftOutIndex) =>
                // Bind the seed: the input at seedIndex must be the exact parameterized seed UTxO,
                // not merely some input that exists. Otherwise the one-shot guarantee is defeated
                // and the same policy can mint unlimited NFTs (uniqueness broken). A wrong index
                // simply fails the check (fails closed), so it cannot be bypassed.
                require(tx.inputs.at(seedIndex).outRef === seed, MustSpendSeed)

                // Find the reference NFT output - must be at script address with inline datum
                val refNftOutput = tx.outputs.at(refNftOutIndex)

                // Validate datum structure and content
                val datum = refNftOutput.datum match
                    case OutputDatum.OutputDatum(d) => d.to[ReferenceNftDatum]
                    case _                          => fail(ReferenceNftMustHaveInlineDatum)

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
                  MustMintOneRefNft
                )

                // Verify exactly one user NFT is minted with correct name
                require(
                  tx.mint.quantityOf(policyId, userTokenName) === BigInt(1),
                  MustMintOneUserNft
                )
            case MintRedeemer.Burn =>
                // The Burn redeemer may only burn. Reject any positive mint under this policy:
                // otherwise it is a side door around the one-shot seed check in the Mint branch
                // (an attacker could mint fresh ref/user pairs with this redeemer, never spending
                // the seed or any script UTxO). The actual "both tokens burned" check lives in the
                // spend validator, which runs because the reference NFT is spent from the script.
                val noPositiveMint = tx.mint.toSortedMap.get(policyId) match
                    case Option.Some(tokens) => tokens.values.forall(_ <= BigInt(0))
                    case Option.None         => true
                require(noPositiveMint, BurnMustNotMint)
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
        val datum = d.getOrFail(DatumRequired).to[ReferenceNftDatum]
        val ownInput = tx.findOwnInputOrFail(ownRef)
        val scriptAddress = ownInput.resolved.address
        val policyId = scriptAddress.credential match
            case Credential.ScriptCredential(hash) => hash
            case _                                 => fail(ExpectedScriptCredential)

        val userTokenName = EditableNftValidator.userNftName(datum.tokenId)
        val refTokenName = EditableNftValidator.refNftName(datum.tokenId)

        redeemer.to[SpendRedeemer] match {
            case SpendRedeemer.Spend(userNftInputIndex, refNftOutputIndex) => {
                val userTokenInput = tx.inputs.at(userNftInputIndex)
                val hasUserToken =
                    userTokenInput.resolved.value.quantityOf(policyId, userTokenName) === BigInt(1)

                require(hasUserToken, MustPresentUserToken)

                val newOutput = tx.outputs.at(refNftOutputIndex)
                val correctAddress = newOutput.address === scriptAddress
                val correctQuantity =
                    newOutput.value.quantityOf(policyId, refTokenName) === BigInt(1)
                val validContinuation = correctAddress && correctQuantity
                require(validContinuation, MustReturnRefNft)

                val newDatum = newOutput.datum match
                    case OutputDatum.OutputDatum(d) => d.to[ReferenceNftDatum]
                    case _                          => fail(ContinuationMustHaveInlineDatum)

                // Sealed policy enforcement
                if datum.isSealed then
                    // check the entire datum
                    require(newDatum.toData === d.get, SealedNftImmutable)
                else
                    // just check the token id, rest is ok to change
                    require(newDatum.tokenId === datum.tokenId, TokenIdImmutable)
            }
            case SpendRedeemer.Burn(userNftInputIndex) => {
                val refNftName = datum.refNftName
                val userNftName = datum.userNftName

                val isRefNftBurned = tx.mint.quantityOf(policyId, refNftName) === BigInt(-1)
                require(isRefNftBurned, MustBurnRefNft)
                val isUserNftBurned = tx.mint.quantityOf(policyId, userNftName) === BigInt(-1)
                require(isUserNftBurned, MustBurnUserNft)
            }
        }

    }

    // CIP-67/68 asset name labels: 100 (0x000643b0) = reference token, 222 (0x000de140) = user token.
    inline def refNftName(tokenId: ByteString): ByteString = Cip68ReferenceLabel ++ tokenId
    inline def userNftName(tokenId: ByteString): ByteString = Cip68UserLabel ++ tokenId

    private inline def Cip68ReferenceLabel: ByteString = hex"000643b0"
    private inline def Cip68UserLabel: ByteString = hex"000de140"

    // Error messages
    private inline val MustSpendSeed = "Must spend the seed UTxO"
    private inline val ReferenceNftMustHaveInlineDatum = "Reference NFT must have an inline datum"
    private inline val ReferenceNftMustBePreserved = "Reference NFT must go to this script address"
    private inline val MustMintOneRefNft = "Must mint exactly 1 reference NFT"
    private inline val MustMintOneUserNft = "Must mint exactly 1 user NFT"
    private inline val DatumRequired = "Datum required"
    private inline val ExpectedScriptCredential = "Expected script credential"
    private inline val MustPresentUserToken = "Must present user token to edit the reference NFT"
    private inline val MustReturnRefNft = "Must return reference NFT to the script address"
    private inline val ContinuationMustHaveInlineDatum = "Continuation must have an inline datum"
    private inline val SealedNftImmutable = "Sealed NFTs are immutable"
    private inline val TokenIdImmutable = "Token ID is immutable"
    private inline val MustBurnRefNft = "Must burn the reference NFT"
    private inline val MustBurnUserNft = "Must burn the user NFT"
    private inline val BurnMustNotMint = "Burn redeemer must not mint tokens"
}
