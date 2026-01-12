package scalus.examples.pricebet

import scalus.Compile
import scalus.builtin.{FromData, ToData}
import scalus.builtin.ByteString
import scalus.ledger.api.v1.PubKeyHash
import scalus.ledger.api.v3.{TxInfo, TxOutRef}
import scalus.ledger.api.v2
import scalus.prelude.*
import scalus.builtin.Data
import scalus.ledger.api.v1.PosixTime

// Datum
case class OracleState(
    timestamp: PosixTime,
    exchangeRateNominator: BigInt,
    exchangeRateDenominator: BigInt,
    beaconPolicyId: ByteString,
    beaconTokenName: ByteString,
    authorizedSigner: PubKeyHash
) derives FromData,
      ToData

case class MintRedeemer(
    seedUtxo: TxOutRef
) derives FromData,
      ToData

@Compile
object OracleValidator extends Validator {

    /** Minting policy for the oracle beacon token. Ensures exactly one beacon token is minted by
      * spending a specific seed UTXO.
      */
    inline def mint(
        redeemer: Data,
        policyId: scalus.ledger.api.v3.PolicyId,
        tx: TxInfo
    ): Unit = {
        val mintRedeemer = redeemer.to[MintRedeemer]

        // Verify the seed UTXO is being spent
        val seedUtxoIsSpent = tx.inputs.exists(_.outRef === mintRedeemer.seedUtxo)
        require(seedUtxoIsSpent, "Seed UTXO must be spent to mint beacon")

        // Get the minted value and sum all quantities
        // We expect exactly 1 token to be minted (the beacon NFT)
        val mintedValue = tx.mint
        val allMintedTokens = mintedValue.toSortedMap.toList.flatMap { case (policyId, tokens) =>
            tokens.toList
        }

        // Verify exactly one token is minted with quantity 1
        require(allMintedTokens.length === BigInt(1), "Must mint exactly one token")
        val (tokenName, quantity) = allMintedTokens.head
        require(quantity === BigInt(1), "Must mint exactly 1 beacon token")
    }

    /** Spending validator for oracle UTXOs. Validates oracle updates and ensures beacon token
      * preservation.
      */
    inline def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val state = datum.getOrFail("Must have inline datum").to[OracleState]
        val ownInput = tx.findOwnInputOrFail(ownRef)

        // Verify exchange rate is non-zero
        require(state.exchangeRateNominator !== BigInt(0), ZeroExchangeRateError)
        require(state.exchangeRateDenominator != BigInt(0), ZeroExchangeRateError)

        // Verify authorized signer
        require(
          tx.signatories.exists(_ === state.authorizedSigner),
          "Must be signed by authorized signer"
        )

        // Verify beacon token in input
        val inputBeaconQty =
            ownInput.resolved.value.quantityOf(state.beaconPolicyId, state.beaconTokenName)
        require(inputBeaconQty === BigInt(1), "Input must have beacon token")

        // Find continuation output
        val continuationOutputs =
            tx.outputs.filter(out => out.address === ownInput.resolved.address)
        require(
          continuationOutputs.length === BigInt(1),
          "Must have exactly one continuation output"
        )

        val continuationOutput = continuationOutputs.head

        // Verify beacon token in output
        val outputBeaconQty =
            continuationOutput.value.quantityOf(state.beaconPolicyId, state.beaconTokenName)
        require(outputBeaconQty === BigInt(1), "Output must have beacon token")

        // Verify ADA preservation
        val inputAda = ownInput.resolved.value.getLovelace
        val outputAda = continuationOutput.value.getLovelace
        require(inputAda === outputAda, "ADA amount must not change")

        // Extract new state and verify timestamp is within validity window
        val newState = continuationOutput.datum match {
            case v2.OutputDatum.OutputDatum(d) => d.to[OracleState]
            case _                             => fail("Continuation must have inline datum")
        }

        // Verify timestamp is within tx validity window
        // TODO: Implement proper interval bound checking
        val validRange = tx.validRange
        // For now, just verify non-zero timestamp
        require(newState.timestamp > 0, "Timestamp must be positive")

        // Verify beacon info unchanged
        require(
          newState.beaconPolicyId === state.beaconPolicyId,
          "Beacon policy must not change"
        )
        require(
          newState.beaconTokenName === state.beaconTokenName,
          "Beacon token name must not change"
        )
        require(
          newState.authorizedSigner === state.authorizedSigner,
          "Authorized signer must not change"
        )
    }

    private inline val ZeroExchangeRateError = "Nominator and denominator must be non-zero"
}
