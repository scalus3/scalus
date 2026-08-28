package scalus.examples.editablenft

import scalus.compiler.Compile
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.cardano.onchain.plutus.v1.{Credential, PolicyId}
import scalus.cardano.onchain.plutus.v2.OutputDatum
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
    case Burn(tokenId: ByteString)
}

enum SpendRedeemer derives FromData, ToData {
    case Spend(userNftInputIndex: BigInt, refNftOutputIndex: BigInt)
    case Burn
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
      * The tokenId comes from the reference NFT datum (Mint) or the redeemer (Burn). Both branches
      * pin the mint under this policy to exactly the matching pair:
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
                val datum =
                    refNftOutput.datum.inlineOrFail[ReferenceNftDatum](
                      ReferenceNftMustHaveInlineDatum
                    )

                val refTokenName = refNftName(datum.tokenId)
                val userTokenName = userNftName(datum.tokenId)

                require(
                  refNftOutput.address.credential === Credential.ScriptCredential(policyId),
                  ReferenceNftMustBePreserved
                )

                // The reference NFT output must hold exactly the reference NFT under this
                // policy. In particular the user NFT must NOT ride along: the spend validator
                // accepts any input holding the user NFT as the ownership proof, so a reference
                // UTxO that also held the user NFT would be spendable by anyone.
                require(
                  refNftOutput.value.hasOnly(policyId, refTokenName, 1),
                  RefOutputMustHoldOnlyRefNft
                )

                // Pin the whole mint under this policy to exactly the ref/user pair. Checking
                // only the two expected names would let the minter forge arbitrary extra tokens
                // (other names, any quantity) under the same policy in this transaction.
                val expectedMint =
                    Value(policyId, refTokenName, 1) + Value(policyId, userTokenName, 1)
                require(
                  tx.mint.tokens(policyId) === expectedMint.tokens(policyId),
                  MustMintExactlyNftPair
                )
            case MintRedeemer.Burn(tokenId) =>
                // Both tokens must be burned together and nothing else may be minted or burned
                // under this policy. This closes two side doors: minting fresh tokens via the
                // Burn redeemer (bypassing the one-shot seed check), and burning the user NFT
                // alone, which would orphan the reference NFT at the script forever (editing
                // and burning both require the user NFT).
                val expectedBurn =
                    Value(policyId, refNftName(tokenId), -1) + Value(
                      policyId,
                      userNftName(tokenId),
                      -1
                    )
                require(
                  tx.mint.tokens(policyId) === expectedBurn.tokens(policyId),
                  MustBurnExactlyNftPair
                )
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
        val ownInput = tx.findInputOrFail(ownRef)
        val scriptAddress = ownInput.resolved.address
        val policyId = scriptAddress.credential.scriptHashOrFail(ExpectedScriptCredential)

        val userTokenName = userNftName(datum.tokenId)
        val refTokenName = refNftName(datum.tokenId)

        redeemer.to[SpendRedeemer] match {
            case SpendRedeemer.Spend(userNftInputIndex, refNftOutputIndex) => {
                val userTokenInput = tx.inputs.at(userNftInputIndex)
                val hasUserToken =
                    userTokenInput.resolved.value.hasNft(policyId, userTokenName)

                require(hasUserToken, MustPresentUserToken)

                val newOutput = tx.outputs.at(refNftOutputIndex)
                val correctAddress = newOutput.address === scriptAddress
                val correctQuantity =
                    newOutput.value.hasNft(policyId, refTokenName)
                val validContinuation = correctAddress && correctQuantity
                require(validContinuation, MustReturnRefNft)

                // Sealed policy enforcement
                if datum.isSealed then
                    // check the entire datum: the continuation must carry it unchanged
                    require(newOutput.hasInlineDatum(datum), SealedNftImmutable)
                else
                    // just check the token id, rest is ok to change
                    val newDatum = newOutput.datum
                        .inlineOrFail[ReferenceNftDatum](ContinuationMustHaveInlineDatum)
                    require(newDatum.tokenId === datum.tokenId, TokenIdImmutable)
            }
            case SpendRedeemer.Burn => {
                val isRefNftBurned = tx.mint.quantityOf(policyId, refTokenName) === BigInt(-1)
                require(isRefNftBurned, MustBurnRefNft)
                val isUserNftBurned = tx.mint.quantityOf(policyId, userTokenName) === BigInt(-1)
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
    private inline val RefOutputMustHoldOnlyRefNft =
        "Reference NFT output must hold exactly the reference NFT"
    private inline val MustMintExactlyNftPair = "Must mint exactly the reference and user NFT pair"
    private inline val MustBurnExactlyNftPair = "Must burn exactly the reference and user NFT pair"
    private inline val DatumRequired = "Datum required"
    private inline val ExpectedScriptCredential = "Expected script credential"
    private inline val MustPresentUserToken = "Must present user token to edit the reference NFT"
    private inline val MustReturnRefNft = "Must return reference NFT to the script address"
    private inline val ContinuationMustHaveInlineDatum = "Continuation must have an inline datum"
    private inline val SealedNftImmutable = "Sealed NFTs are immutable"
    private inline val TokenIdImmutable = "Token ID is immutable"
    private inline val MustBurnRefNft = "Must burn the reference NFT"
    private inline val MustBurnUserNft = "Must burn the user NFT"
}
