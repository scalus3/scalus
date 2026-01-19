package scalus.examples

import scalus.*
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.builtin.Data.toData
import scalus.compiler.Options
import scalus.ledger.api.v1.Value.*
import scalus.ledger.api.v3.*
import scalus.patterns.{ParameterValidation, ParameterValidationOnChain}
import scalus.prelude.*
import scalus.uplc.PlutusV3

/** Parameter Validation Example - NFT with Verified Marketplace
  *
  * Demonstrates the Parameter Validation pattern where a minting policy ensures NFTs can only be
  * sent to a marketplace validator parameterized with the correct creator royalty address.
  *
  * '''Use Case:''' An NFT creator wants to ensure their NFTs are always minted directly to a
  * marketplace that will enforce their royalty settings. The minting policy verifies that:
  *   1. The output address is a script address (not a pub key)
  *   2. The script hash matches the marketplace parameterized with their pub key hash
  *
  * '''How it works:'''
  *   1. Off-chain: Compile marketplace with creator's PubKeyHash, compute expected hash
  *   2. Off-chain: Pass expected hash as parameter to minting policy
  *   3. On-chain: Minting policy verifies NFTs go to the expected marketplace script
  *
  * @see
  *   [[https://github.com/Anastasia-Labs/aiken-design-patterns/blob/develop/lib/aiken-design-patterns/parameter-validation.ak]]
  */

/** Listing datum for the marketplace */
case class Listing(
    seller: PubKeyHash,
    price: BigInt
) derives ToData,
      FromData

/** Redeemer for marketplace actions */
enum MarketplaceRedeemer derives ToData, FromData:
    case Buy
    case Cancel

/** Marketplace Validator - parameterized by creator's royalty address
  *
  * A simple marketplace that ensures the creator (royalty recipient) receives a portion of sales.
  * The creator PubKeyHash is baked into the script at deployment time.
  *
  * @param creatorPkh
  *   The creator who receives royalties (script parameter)
  */
@Compile
object MarketplaceValidator {

    inline def validate(creatorPkh: PubKeyHash)(scData: Data): Unit = {
        val sc = scData.to[ScriptContext]
        sc.scriptInfo match
            case ScriptInfo.SpendingScript(_, datum) =>
                val listing = datum.getOrFail("Datum required").to[Listing]
                val action = sc.redeemer.to[MarketplaceRedeemer]

                action match
                    case MarketplaceRedeemer.Buy =>
                        // Verify royalty payment to creator (10% royalty)
                        val royaltyAmount = listing.price * BigInt(10) / BigInt(100)
                        val creatorCred = Credential.PubKeyCredential(creatorPkh)
                        val sellerCred = Credential.PubKeyCredential(listing.seller)

                        // Find payments to creator and seller
                        val (creatorPaid, sellerPaid) =
                            sc.txInfo.outputs.foldLeft((BigInt(0), BigInt(0))) {
                                case ((creatorSum, sellerSum), output) =>
                                    val cred = output.address.credential
                                    val value = output.value.getLovelace
                                    if cred === creatorCred then (creatorSum + value, sellerSum)
                                    else if cred === sellerCred then (creatorSum, sellerSum + value)
                                    else (creatorSum, sellerSum)
                            }

                        require(creatorPaid >= royaltyAmount, "Creator royalty not paid")
                        require(
                          sellerPaid >= listing.price - royaltyAmount,
                          "Seller payment insufficient"
                        )

                    case MarketplaceRedeemer.Cancel =>
                        // Only seller can cancel
                        require(
                          sc.txInfo.signatories.contains(listing.seller),
                          "Only seller can cancel"
                        )
            case _ => fail("Unsupported script purpose")
    }
}

/** NFT Minting Policy parameters */
case class NFTMintParams(
    expectedMarketplaceHash: ValidatorHash,
    tokenName: TokenName
) derives ToData,
      FromData

/** NFT Minting Policy - ensures NFTs go to verified marketplace
  *
  * This policy verifies that minted NFTs are sent to a marketplace script with the expected hash.
  * The expected hash is computed off-chain from the base marketplace script + creator parameter.
  *
  * @param params
  *   Contains the expected marketplace hash and token name
  */
@Compile
object NFTMintingPolicy {

    inline def validate(params: NFTMintParams)(scData: Data): Unit = {
        val sc = scData.to[ScriptContext]
        sc.scriptInfo match
            case ScriptInfo.MintingScript(policyId) =>
                // Get minted amount for our token
                val mintedAmount = sc.txInfo.mint.quantityOf(policyId, params.tokenName)

                require(mintedAmount === BigInt(1), "Must mint exactly 1 NFT")

                // Find outputs containing our NFT
                val nftOutputs = sc.txInfo.outputs.filter { output =>
                    output.value.quantityOf(policyId, params.tokenName) > 0
                }

                require(nftOutputs.length === BigInt(1), "NFT must go to exactly one output")

                val nftOutput = nftOutputs.head

                // Verify the output goes to the expected marketplace script
                ParameterValidationOnChain.verifyAddressScript(
                  nftOutput.address,
                  params.expectedMarketplaceHash
                )
            case _ => fail("Unsupported script purpose")
    }
}

// --- Compilation and Off-chain Usage ---

private object ParameterValidationCompilation {
    private given compilerOptions: Options = Options.release

    /** Base marketplace validator program (not yet parameterized) */
    lazy val marketplaceBase = PlutusV3.compile(MarketplaceValidator.validate)

    /** Base NFT minting policy program (not yet parameterized) */
    lazy val nftMintingBase = PlutusV3.compile(NFTMintingPolicy.validate)
}

/** Compiled marketplace base program for parameter application */
lazy val MarketplaceBaseProgram = ParameterValidationCompilation.marketplaceBase

/** Compiled NFT minting base program for parameter application */
lazy val NFTMintingBaseProgram = ParameterValidationCompilation.nftMintingBase

/** Example showing off-chain usage of the Parameter Validation pattern.
  *
  * This demonstrates how to:
  *   1. Compute the expected marketplace hash for a given creator
  *   2. Create minting policy parameters with that hash
  *   3. Apply parameters to create the final scripts
  */
object ParameterValidationUsage {

    /** Compute the marketplace script hash for a given creator.
      *
      * @param creatorPkh
      *   The creator's public key hash
      * @return
      *   The script hash of the marketplace parameterized with this creator
      */
    def computeMarketplaceHash(creatorPkh: PubKeyHash): ValidatorHash = {
        ParameterValidation.computeScriptHashV3(
          MarketplaceBaseProgram.program.deBruijnedProgram,
          creatorPkh.toData
        )
    }

    /** Create the parameterized marketplace script for a creator.
      *
      * @param creatorPkh
      *   The creator's public key hash
      * @return
      *   The parameterized marketplace program
      */
    def createMarketplaceScript(creatorPkh: PubKeyHash) = {
        MarketplaceBaseProgram.program.deBruijnedProgram $ creatorPkh.toData
    }

    /** Create the parameterized NFT minting policy.
      *
      * @param expectedMarketplaceHash
      *   The expected marketplace script hash
      * @param tokenName
      *   The NFT token name
      * @return
      *   The parameterized minting policy program
      */
    def createNFTMintingPolicy(expectedMarketplaceHash: ValidatorHash, tokenName: TokenName) = {
        val params = NFTMintParams(expectedMarketplaceHash, tokenName)
        NFTMintingBaseProgram.program.deBruijnedProgram $ params.toData
    }

    /** Complete workflow for setting up NFT minting with verified marketplace.
      *
      * @param creatorPkh
      *   The NFT creator's public key hash
      * @param tokenName
      *   The NFT token name
      * @return
      *   Tuple of (marketplace program, minting policy program, marketplace hash)
      */
    def setupNFTWithMarketplace(creatorPkh: PubKeyHash, tokenName: TokenName) = {
        val marketplaceHash = computeMarketplaceHash(creatorPkh)
        val marketplace = createMarketplaceScript(creatorPkh)
        val mintingPolicy = createNFTMintingPolicy(marketplaceHash, tokenName)
        (marketplace, mintingPolicy, marketplaceHash)
    }
}
