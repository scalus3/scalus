package scalus.examples.pricebet

import scalus.Compile
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.examples.pricebet.MintOracleRedeemer.{Burn, Mint}
import scalus.ledger.api.v1.{PosixTime, PubKeyHash}
import scalus.ledger.api.v2
import scalus.ledger.api.v3.{TxInfo, TxOutRef}
import scalus.prelude.*

// Parameter
case class OracleConfig(
    seedUtxo: TxOutRef,
    beaconPolicyId: ByteString,
    beaconName: ByteString,
    authorizedSigner: PubKeyHash
) derives FromData,
      ToData

// Datum
case class OracleState(
    timestamp: PosixTime,
    exchangeRate: Rational
) derives FromData,
      ToData

enum MintOracleRedeemer derives FromData, ToData:
    case Mint
    case Burn

enum SpendOracleRedeemer derives FromData, ToData:
    case Update(oracleUtxoIndex: BigInt)
    case Burn

@Compile
object OracleValidator extends DataParameterizedValidator {

    /** Minting policy for the oracle beacon token. Ensures exactly one beacon token is minted by
      * spending a specific seed UTXO.
      */
    inline def mint(
        param: Data,
        redeemer: Data,
        policyId: scalus.ledger.api.v3.PolicyId,
        tx: TxInfo
    ): Unit = {
        val mintRedeemer = redeemer.to[MintOracleRedeemer]
        val config = param.to[OracleConfig]

        require(tx.isSignedBy(config.authorizedSigner), MustBeSigned)
        mintRedeemer match {
            case Mint =>
                // Verify the seed UTXO is being spent
                val seedUtxoIsSpent = tx.inputs.exists(_.outRef === config.seedUtxo)
                require(seedUtxoIsSpent, "Must spend seed utxo to mint the beacon")

                // Get the minted value and sum all quantities
                // We expect exactly 1 token to be minted (the beacon NFT)
                val mintedValue = tx.mint
                val allMintedTokens = mintedValue.toSortedMap.toList.flatMap {
                    case (policyId, tokens) =>
                        tokens.toList
                }

                // Verify exactly one token is minted with quantity 1
                require(allMintedTokens.length === BigInt(1), "Must mint exactly one token")
                val (tokenName, quantity) = allMintedTokens.head
                require(quantity === BigInt(1), "Must mint exactly 1 beacon token")
            case Burn =>
                // Verify exactly one beacon token is burned (quantity = -1)
                val mintedValue = tx.mint
                val burnedTokens = mintedValue.toSortedMap.toList.flatMap {
                    case (policyId, tokens) =>
                        tokens.toList
                }

                // Verify exactly one token entry with quantity -1
                require(burnedTokens.length === BigInt(1), "Must burn exactly one token type")
                val (tokenName, quantity) = burnedTokens.head
                require(quantity === BigInt(-1), "Must burn exactly 1 beacon token")
        }
    }

    /** Spending validator for oracle UTXOs. Validates oracle updates and ensures beacon token
      * preservation. Also allows closing the oracle when burning the beacon.
      */
    inline def spend(
        param: Data,
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val config = param.to[OracleConfig]
        val r = redeemer.to[SpendOracleRedeemer]
        val state = datum.getOrFail("Must have inline datum").to[OracleState]
        val ownInput = tx.findOwnInputOrFail(ownRef)

        // Verify exchange rate is non-zero
        state.exchangeRate.checkDenominator()
        require(!state.exchangeRate.isZero, "Zero rate is not allowed")

        // Verify authorized signer
        require(
          tx.isSignedBy(config.authorizedSigner),
          "Must be signed by authorized signer"
        )

        r match {
            case SpendOracleRedeemer.Update(oracleUtxoIndex) =>
                // Verify continuation output goes to same address (preserves the oracle)
                val continuationOutput = tx.outputs.at(oracleUtxoIndex)
                require(
                  continuationOutput.address === ownInput.resolved.address,
                  "Continuation output must be at the same script address"
                )

                // Extract new state and verify timestamp is within validity window
                val newState = continuationOutput.datum match {
                    case v2.OutputDatum.OutputDatum(d) => d.to[OracleState]
                    case _ => fail("Continuation must have inline datum")
                }

                // Verify timestamp is within tx validity window
                val validRange = tx.validRange
                //                   validity range
                //  -------------+--------------------+-----------
                //  timestamp ^
                require(
                  validRange.isEntirelyAfter(newState.timestamp),
                  "Oracle timestamp must be in the past relative to the tx validity interval"
                )

            case SpendOracleRedeemer.Burn =>
                // Verify the beacon token is being burned in this transaction
                val ownScriptHash = ownInput.resolved.address.credential match {
                    case scalus.ledger.api.v1.Credential.ScriptCredential(hash) => hash
                    case _ => fail("Own input must be a script")
                }
                val burnedAmount = tx.mint.quantityOf(ownScriptHash, config.beaconName)
                require(burnedAmount === BigInt(-1), "Must burn the beacon token")
        }
    }

    private inline val ZeroExchangeRateError = "Nominator and denominator must be non-zero"
    private inline val MustBeSigned = "Must be signed by the authorized signer"
}
