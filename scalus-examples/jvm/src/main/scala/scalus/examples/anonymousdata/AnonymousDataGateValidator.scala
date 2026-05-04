package scalus.examples.anonymousdata

import scalus.compiler.Compile

import scalus.*
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v1.Credential
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.v3.ParameterizedValidator

// ============================================================================
// DATA MODELS
// ============================================================================

/** Datum for the gate contract.
  *
  * @param expectedDataHash
  *   blake2b_256 hash of the expected decrypted data
  * @param creatorPkh
  *   pubkeyhash of the gate creator (for refund)
  */
case class GateDatum(expectedDataHash: ByteString, creatorPkh: ByteString) derives FromData, ToData

@Compile
object GateDatum {
    given Eq[GateDatum] = Eq.derived
}

/** Redeemer for the gate contract. */
enum GateRedeemer derives FromData, ToData:
    /** Unlock by proving anonymous data knowledge. */
    case Unlock(dataKey: ByteString, decKey: ByteString)

    /** Refund to creator (if data entry was deleted or no longer available). */
    case Refund

@Compile
object GateRedeemer

// ============================================================================
// VALIDATOR
// ============================================================================

/** Gate spending validator parameterized by anonymous data policyId.
  *
  * Demonstrates on-chain reading of anonymous data via reference inputs. Funds locked at this
  * script can be released only by proving that specific data exists in the anonymous data store and
  * that its hash matches the expected value.
  *
  * If the underlying data entry is deleted, the gate creator can reclaim funds via `Refund`.
  *
  * '''Usage:'''
  *   - Lock funds with `GateDatum(blake2b_256(expectedData), creatorPkh)`
  *   - Unlock by providing `GateRedeemer.Unlock(dataKey, decKey)` and the anonymous data UTXO as a
  *     reference input
  *   - Refund by providing `GateRedeemer.Refund` signed by the creator
  */
@Compile
object AnonymousDataGateValidator extends ParameterizedValidator[ByteString] {

    inline override def spend(
        anonDataPolicyId: ByteString,
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit = {
        val gateDatum = datum.getOrFail("Expected datum").to[GateDatum]
        val gateRedeemer = redeemer.to[GateRedeemer]

        gateRedeemer match
            case GateRedeemer.Unlock(dataKey, decKey) =>
                // Find the anonymous data UTXO in reference inputs by script address + beacon token
                val anonDataRef = txInfo.referenceInputs
                    .find { txInInfo =>
                        val out = txInInfo.resolved
                        out.address.credential === Credential.ScriptCredential(
                          anonDataPolicyId
                        ) &&
                        out.value.quantityOf(
                          anonDataPolicyId,
                          AnonymousDataValidator.beaconTokenName
                        ) === BigInt(1)
                    }
                    .getOrFail("Anonymous data reference input not found")

                // Read and decrypt the entry
                val decryptedData = AnonymousDataReader.readAndDecrypt(
                  anonDataRef.resolved,
                  anonDataPolicyId,
                  dataKey,
                  decKey
                )

                // Verify hash matches expected
                require(
                  blake2b_256(decryptedData) === gateDatum.expectedDataHash,
                  "Decrypted data hash does not match expected"
                )

            case GateRedeemer.Refund =>
                // Only the gate creator can reclaim
                require(
                  txInfo.signatories.exists(s => s.hash === gateDatum.creatorPkh),
                  "Only gate creator can refund"
                )
    }

}

// ============================================================================
// CONTRACT COMPILATION
// ============================================================================

private given gateOptions: Options = Options.release

lazy val AnonymousDataGateContract = PlutusV3.compile(AnonymousDataGateValidator.validate)
