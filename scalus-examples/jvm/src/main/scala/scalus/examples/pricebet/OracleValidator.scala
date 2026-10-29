package scalus.examples.pricebet

import scalus.compiler.Compile
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.examples.pricebet.MintOracleRedeemer.{Burn, Mint}
import scalus.cardano.onchain.plutus.v3.{Credential, PosixTime, PubKeyHash}
import scalus.cardano.onchain.plutus.v3.{DataParameterizedValidator, PolicyId, TxInfo, TxOutRef}
import scalus.cardano.onchain.plutus.prelude.*

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
    case Mint(oracleOutputIndex: BigInt)
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
        policyId: PolicyId,
        tx: TxInfo
    ): Unit = {
        val config = param.to[OracleConfig]
        redeemer.to[MintOracleRedeemer] match {
            case Mint(oracleOutputIndex) =>
                require(tx.isSignedBy(config.authorizedSigner), MustBeSigned)
                // Verify the seed UTXO is being spent
                tx.findInputOrFail(config.seedUtxo, "Must spend seed utxo to mint the beacon")

                // Verify exactly one beacon token is minted with quantity 1 and nothing else
                // under this policy
                require(
                  tx.mint.hasOnly(policyId, config.beaconName, 1),
                  "Must mint exactly 1 beacon token and nothing else"
                )

                // Base case of the beacon invariant: the beacon starts in a valid oracle UTxO, at
                // this script, with an inline OracleState that an Update would accept. Pricebet
                // trusts any UTxO at the oracle script that holds the beacon.
                val oracleOutput = tx.outputs.at(oracleOutputIndex)
                require(oracleOutput.value.hasNft(policyId, config.beaconName), BeaconToOracle)
                require(
                  oracleOutput.address.credential === Credential.ScriptCredential(policyId),
                  BeaconToOracle
                )
                requireValidRate(
                  oracleOutput.datum.inlineOrFail[OracleState](OracleOutputDatum).exchangeRate
                )
            case Burn =>
                // Only burning. The beacon is only ever in the oracle UTxO (the mint puts it there
                // and Update keeps it), so burning it spends that UTxO with the Burn redeemer, and
                // the spend branch checks the signature and the exact quantity.
                require(tx.onlyBurnsUnder(policyId), "Burn redeemer must only burn")
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
        val ownInput = tx.findInputOrFail(ownRef)
        val ownScriptHash =
            ownInput.resolved.address.credential.scriptHashOrFail("Own input must be a script")

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

                // Step of the beacon invariant: the continuation keeps the beacon
                require(
                  continuationOutput.value.hasNft(ownScriptHash, config.beaconName),
                  BeaconMustStay
                )

                // Extract new state and verify timestamp is within validity window
                val newState = continuationOutput.datum.inlineOrFail[OracleState](
                  "Continuation must have inline datum"
                )

                // Checking the new state, not the input, keeps a bad rate from locking the oracle
                // UTxO: Burn has no rate check.
                requireValidRate(newState.exchangeRate)

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
                require(
                  tx.mint.hasOnly(ownScriptHash, config.beaconName, -1),
                  "Must burn the beacon token"
                )
        }
    }

    /** A rate an oracle may publish: a non-zero denominator and a non-zero numerator. Public: the
      * plugin cannot link a private def that the inline handlers call.
      */
    def requireValidRate(rate: Rational): Unit = {
        rate.checkDenominator()
        require(!rate.isZero, ZeroRate)
    }

    private inline val MustBeSigned = "Must be signed by the authorized signer"
    inline val BeaconToOracle = "Beacon must go to the oracle script"
    inline val OracleOutputDatum = "Oracle output must have an inline datum"
    inline val ZeroRate = "Zero rate is not allowed"
    inline val BeaconMustStay = "Beacon must stay in the oracle"
}
