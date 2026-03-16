package scalus.patterns

import scalus.*
import scalus.uplc.builtin.Builtins
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.ByteString.given
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*

/** Product datum stored in each product UTxO.
  *
  * @param tag
  *   Application-specific tag for this product
  * @param creator
  *   The public key hash of the creator who owns this product
  */
case class ProductDatum(
    tag: ByteString,
    creator: PubKeyHash
) derives FromData,
      ToData

/** Redeemer for the factory minting policy.
  *
  * @note
  *   `Create` includes a `seedUtxo` for one-shot minting: the seed must be consumed in the
  *   transaction, guaranteeing that the derived token name is globally unique.
  */
enum FactoryAction derives FromData, ToData:
    case Create(tag: ByteString, seedUtxo: TxOutRef)
    case Destroy

/** Factory Pattern for Cardano UTxO Model
  *
  * A minting policy acts as a factory: "creating a product" means minting a unique NFT and
  * producing a UTxO with a [[ProductDatum]] at a spending validator address. "Destroying a product"
  * means burning the NFT and consuming the UTxO.
  *
  * The token name is `blake2b_256(serialiseData(seedUtxo))`, where `seedUtxo` is a one-shot UTxO
  * reference consumed during creation. Since each UTxO can only be spent once, this guarantees
  * globally unique token names and prevents duplicate products.
  *
  * @see
  *   [[https://github.com/blockchain-unica/rosetta-smart-contracts/tree/main/contracts/factory]]
  */
@Compile
object Factory {

    /** Compute the expected token name for a product from its seed UTxO.
      *
      * The seed UTxO must be consumed in the minting transaction, ensuring one-shot uniqueness.
      *
      * @param seedUtxo
      *   The UTxO reference consumed to seed this product's unique identity
      * @return
      *   blake2b_256(serialiseData(seedUtxo))
      */
    def computeTokenName(seedUtxo: TxOutRef): TokenName =
        Builtins.blake2b_256(Builtins.serialiseData(seedUtxo.toData))

    /** Validate product creation (minting policy logic for `Create`).
      *
      * Checks:
      *   - Tx is signed by the creator
      *   - The seed UTxO is consumed as an input (one-shot guarantee)
      *   - Exactly 1 token minted under this policy
      *   - Token name matches `blake2b_256(serialiseData(seedUtxo))`
      *   - A product output exists at `spendingScriptHash` with the minted NFT and correct inline
      *     datum
      *
      * @param creator
      *   The creator's public key hash (first signatory)
      * @param tag
      *   The product tag from the redeemer
      * @param seedUtxo
      *   The seed UTxO reference for one-shot minting
      * @param policyId
      *   This minting policy's hash
      * @param spendingScriptHash
      *   The spending validator address where products must be locked
      * @param tx
      *   The transaction info
      */
    def validateCreate(
        creator: PubKeyHash,
        tag: ByteString,
        seedUtxo: TxOutRef,
        policyId: PolicyId,
        spendingScriptHash: ValidatorHash,
        tx: TxInfo
    ): Unit = {
        // Creator must sign the transaction
        require(tx.isSignedBy(creator), CreatorMustSign)

        // Seed UTxO must be consumed (one-shot guarantee)
        require(tx.inputs.exists(_.outRef === seedUtxo), SeedUtxoMustBeConsumed)

        // Compute expected token name from seed UTxO
        val expectedTokenName = computeTokenName(seedUtxo)

        // Check exactly 1 token minted under this policy with the correct name
        val mintedTokens = tx.mint.toSortedMap.get(policyId).getOrFail(NoTokensMinted)
        val (tokenName, quantity) = mintedTokens.toList match
            case List.Cons(pair, List.Nil) => pair
            case _                         => fail(MustMintExactlyOneToken)
        require(tokenName === expectedTokenName, WrongTokenName)
        require(quantity === BigInt(1), MustMintExactlyOneToken)

        // Find a product output at the spending script address with the NFT and correct datum
        val scriptCred = Credential.ScriptCredential(spendingScriptHash)
        val productOutput = tx.outputs
            .find { output =>
                output.address.credential === scriptCred
                && output.value.quantityOf(policyId, expectedTokenName) === BigInt(1)
            }
            .getOrFail(MissingProductOutput)

        // Verify inline datum matches expected ProductDatum
        productOutput.datum match
            case OutputDatum.OutputDatum(datum) =>
                val productDatum = datum.to[ProductDatum]
                require(productDatum.tag === tag, DatumTagMismatch)
                require(productDatum.creator === creator, DatumCreatorMismatch)
            case _ => fail(MissingInlineDatum)
    }

    /** Validate product destruction (minting policy logic for `Destroy`).
      *
      * Checks:
      *   - Exactly 1 token burned (qty = -1) under this policy
      *   - Tx is signed by the product's creator
      *
      * @note
      *   Token name validation is handled by the spending validator (`validateSpend`), which
      *   ensures the burned token matches the NFT held in the product UTxO.
      *
      * @param creator
      *   The creator's public key hash (first signatory)
      * @param policyId
      *   This minting policy's hash
      * @param tx
      *   The transaction info
      */
    def validateDestroy(
        creator: PubKeyHash,
        policyId: PolicyId,
        tx: TxInfo
    ): Unit = {
        // Creator must sign the transaction
        require(tx.isSignedBy(creator), CreatorMustSign)

        // Check exactly 1 token burned under this policy
        val mintedTokens = tx.mint.toSortedMap.get(policyId).getOrFail(NoTokensMinted)
        val (_, quantity) = mintedTokens.toList match
            case List.Cons(pair, List.Nil) => pair
            case _                         => fail(MustBurnExactlyOneToken)
        require(quantity === BigInt(-1), MustBurnExactlyOneToken)
    }

    /** Validate spending a product UTxO.
      *
      * Checks:
      *   - Tx is signed by the creator from the datum
      *   - The product's NFT (extracted from own input) is burned in this tx
      *
      * @param datum
      *   The product datum
      * @param factoryPolicyId
      *   The factory minting policy hash
      * @param ownInputValue
      *   The value of the input being spent (used to find the factory NFT)
      * @param tx
      *   The transaction info
      */
    def validateSpend(
        datum: ProductDatum,
        factoryPolicyId: PolicyId,
        ownInputValue: Value,
        tx: TxInfo
    ): Unit = {
        // Creator must sign
        require(tx.isSignedBy(datum.creator), CreatorMustSign)

        // Extract the factory NFT token name from our own input
        val ownFactoryTokens =
            ownInputValue.toSortedMap.get(factoryPolicyId).getOrFail(NoFactoryToken)
        val (tokenName, _) = ownFactoryTokens.toList match
            case List.Cons(pair, List.Nil) => pair
            case _                         => fail(MustHaveExactlyOneFactoryToken)

        // The NFT must be burned (qty = -1)
        val burnQty = tx.mint.quantityOf(factoryPolicyId, tokenName)
        require(burnQty === BigInt(-1), ProductNFTMustBeBurned)
    }

    inline val CreatorMustSign = "Creator must sign the transaction"
    inline val SeedUtxoMustBeConsumed = "Seed UTxO must be consumed"
    inline val NoTokensMinted = "No tokens minted under this policy"
    inline val MustMintExactlyOneToken = "Must mint exactly one token"
    inline val WrongTokenName = "Token name does not match expected hash"
    inline val MissingProductOutput = "No product output found at spending script"
    inline val MissingInlineDatum = "Product output must have an inline datum"
    inline val DatumTagMismatch = "Product datum tag does not match"
    inline val DatumCreatorMismatch = "Product datum creator does not match"
    inline val MustBurnExactlyOneToken = "Must burn exactly one token"
    inline val NoFactoryToken = "No factory token found in input"
    inline val MustHaveExactlyOneFactoryToken = "Input must have exactly one factory token"
    inline val ProductNFTMustBeBurned = "Product NFT must be burned to spend"
}
