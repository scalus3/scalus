package scalus.examples.anonymousdata

import scalus.compiler.Compile

import scalus.*
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v1.{Credential, PolicyId}
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.v3.ParameterizedValidator

// ============================================================================
// DATA MODELS
// ============================================================================

/** Datum for the shared anonymous data UTXO.
  *
  * @param participantsRoot
  *   MerkleTree root of authorized participant pubkeyhashes
  * @param dataMap
  *   map from hash(pkh, nonce) to encrypted data
  */
case class AnonymousDataDatum(
    participantsRoot: ByteString,
    dataMap: AssocMap[ByteString, ByteString]
) derives FromData,
      ToData

@Compile
object AnonymousDataDatum {
    given Eq[AnonymousDataDatum] = Eq.derived
}

/** Redeemer for the minting policy. */
enum AnonymousDataMintRedeemer derives FromData, ToData:
    case MintBeacon
    case BurnBeacon

@Compile
object AnonymousDataMintRedeemer

/** Redeemer for the spending validator. */
enum AnonymousDataSpendRedeemer derives FromData, ToData:
    case StoreData(
        membershipProof: ByteString,
        dataKey: ByteString,
        encryptedData: ByteString
    )
    case UpdateData(
        membershipProof: ByteString,
        dataKey: ByteString,
        newEncryptedData: ByteString
    )
    case DeleteData(
        membershipProof: ByteString,
        dataKey: ByteString
    )
    case UpdateParticipants(
        newParticipantsRoot: ByteString
    )

@Compile
object AnonymousDataSpendRedeemer

// ============================================================================
// VALIDATOR
// ============================================================================

/** Anonymous data validator parameterized by admin pubkeyhash.
  *
  * A single minting policy + spending validator for anonymous data storage on Cardano. A shared
  * UTXO (identified by a beacon token) holds a map of encrypted data entries keyed by
  * hash(pubkeyhash, nonce). Anonymity comes from hash-based keys and encrypted values.
  *
  * '''Minting:'''
  *   - MintBeacon: admin creates the shared UTXO with beacon token, empty dataMap
  *   - BurnBeacon: admin destroys the shared UTXO
  *
  * '''Spending:'''
  *   - StoreData: participant adds an entry (verified via MerkleTree proof)
  *   - UpdateData: participant updates their entry
  *   - DeleteData: participant removes their entry
  *   - UpdateParticipants: admin updates the MerkleTree root
  */
@Compile
object AnonymousDataValidator extends ParameterizedValidator[ByteString] {
    import scalus.cardano.onchain.plutus.crypto.tree.MerkleTree

    /** Beacon token name */
    inline def beaconTokenName: ByteString = ByteString.fromString("anon_data")

    inline override def mint(
        adminPkh: ByteString,
        redeemer: Data,
        policyId: PolicyId,
        txInfo: TxInfo
    ): Unit = {
        val action = redeemer.to[AnonymousDataMintRedeemer]
        action match
            case AnonymousDataMintRedeemer.MintBeacon =>
                // Admin must sign
                require(
                  txInfo.signatories.exists(s => s.hash === adminPkh),
                  "Admin must sign MintBeacon"
                )

                // Exactly 1 beacon token minted
                require(
                  txInfo.mint.quantityOf(policyId, beaconTokenName) === BigInt(1),
                  "Must mint exactly 1 beacon token"
                )

                val allMinted = txInfo.mint.flatten.filter { case (pid, _, _) =>
                    pid === policyId
                }
                require(allMinted.length === BigInt(1), "Only one token allowed per mint")

                // Output at script address with valid datum and beacon token
                val scriptCred = Credential.ScriptCredential(policyId)
                val scriptOutputs = txInfo.findOwnOutputsByCredential(scriptCred)
                require(scriptOutputs.length === BigInt(1), "Expected one script output")
                val scriptOut = scriptOutputs.head

                require(
                  scriptOut.value.quantityOf(policyId, beaconTokenName) === BigInt(1),
                  "Script output must carry the beacon token"
                )

                val datum = scriptOut.datum match
                    case OutputDatum.OutputDatum(d) => d.to[AnonymousDataDatum]
                    case _                          => fail("Expected inline datum")

                require(
                  lengthOfByteString(datum.participantsRoot) > BigInt(0),
                  "participantsRoot must be non-empty"
                )
                require(datum.dataMap.isEmpty, "dataMap must be empty at initialization")

            case AnonymousDataMintRedeemer.BurnBeacon =>
                // Admin must sign
                require(
                  txInfo.signatories.exists(s => s.hash === adminPkh),
                  "Admin must sign BurnBeacon"
                )

                // Exactly 1 beacon token burned
                require(
                  txInfo.mint.quantityOf(policyId, beaconTokenName) === BigInt(-1),
                  "Must burn exactly 1 beacon token"
                )
    }

    inline override def spend(
        adminPkh: ByteString,
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit = {
        val currentDatum = datum.getOrFail("Expected datum").to[AnonymousDataDatum]
        val ownInput = txInfo.findOwnInputOrFail(txOutRef)
        val scriptAddress = ownInput.resolved.address
        val policyId = scriptAddress.credential match
            case Credential.ScriptCredential(hash) => hash
            case _                                 => fail("Expected script credential")

        val action = redeemer.to[AnonymousDataSpendRedeemer]
        action match
            case AnonymousDataSpendRedeemer.StoreData(membershipProof, dataKey, encryptedData) =>
                // Verify signer is a participant
                require(txInfo.signatories.length > BigInt(0), "No signatories")
                val signer = txInfo.signatories.head
                MerkleTree.verifyMembership(
                  currentDatum.participantsRoot,
                  signer.hash,
                  membershipProof
                )

                // dataKey must not already exist; limit map size to prevent datum bloat
                require(currentDatum.dataMap.size < BigInt(100), "dataMap is full")
                require(!currentDatum.dataMap.contains(dataKey), "dataKey already exists")

                // Verify output: beacon token present, updated map, participantsRoot unchanged
                val newDatum = findContinuingOutput(txInfo, scriptAddress, policyId)
                require(
                  newDatum.participantsRoot === currentDatum.participantsRoot,
                  "participantsRoot must not change"
                )
                val expectedMap = currentDatum.dataMap.insert(dataKey, encryptedData)
                require(newDatum.dataMap === expectedMap, "dataMap must have new entry")

            case AnonymousDataSpendRedeemer.UpdateData(
                  membershipProof,
                  dataKey,
                  newEncryptedData
                ) =>
                // Verify signer is a participant
                require(txInfo.signatories.length > BigInt(0), "No signatories")
                val signer = txInfo.signatories.head
                MerkleTree.verifyMembership(
                  currentDatum.participantsRoot,
                  signer.hash,
                  membershipProof
                )

                // dataKey must exist
                require(currentDatum.dataMap.contains(dataKey), "dataKey does not exist")

                // Verify output
                val newDatum = findContinuingOutput(txInfo, scriptAddress, policyId)
                require(
                  newDatum.participantsRoot === currentDatum.participantsRoot,
                  "participantsRoot must not change"
                )
                val expectedMap = currentDatum.dataMap.insert(dataKey, newEncryptedData)
                require(newDatum.dataMap === expectedMap, "dataMap must have updated entry")

            case AnonymousDataSpendRedeemer.DeleteData(membershipProof, dataKey) =>
                // Verify signer is a participant
                require(txInfo.signatories.length > BigInt(0), "No signatories")
                val signer = txInfo.signatories.head
                MerkleTree.verifyMembership(
                  currentDatum.participantsRoot,
                  signer.hash,
                  membershipProof
                )

                // dataKey must exist
                require(currentDatum.dataMap.contains(dataKey), "dataKey does not exist")

                // Verify output
                val newDatum = findContinuingOutput(txInfo, scriptAddress, policyId)
                require(
                  newDatum.participantsRoot === currentDatum.participantsRoot,
                  "participantsRoot must not change"
                )
                val expectedMap = currentDatum.dataMap.delete(dataKey)
                require(newDatum.dataMap === expectedMap, "dataMap must have entry removed")

            case AnonymousDataSpendRedeemer.UpdateParticipants(newParticipantsRoot) =>
                // Admin must sign
                require(
                  txInfo.signatories.exists(s => s.hash === adminPkh),
                  "Admin must sign UpdateParticipants"
                )

                require(
                  lengthOfByteString(newParticipantsRoot) > BigInt(0),
                  "New participantsRoot must be non-empty"
                )

                // Verify output
                val newDatum = findContinuingOutput(txInfo, scriptAddress, policyId)
                require(
                  newDatum.participantsRoot === newParticipantsRoot,
                  "participantsRoot must be updated"
                )
                require(
                  newDatum.dataMap === currentDatum.dataMap,
                  "dataMap must not change"
                )
    }

    /** Find the continuing output at the script address with beacon token, return its datum. */
    private inline def findContinuingOutput(
        txInfo: TxInfo,
        scriptAddress: Address,
        policyId: PolicyId
    ): AnonymousDataDatum = {
        val continuingOutputs = txInfo.outputs.filter { out =>
            out.address === scriptAddress &&
            out.value.quantityOf(policyId, beaconTokenName) === BigInt(1)
        }
        require(continuingOutputs.length === BigInt(1), "Expected exactly one continuing output")
        continuingOutputs.head.datum match
            case OutputDatum.OutputDatum(d) => d.to[AnonymousDataDatum]
            case _                          => fail("Expected inline datum on continuing output")
    }
}

// ============================================================================
// ON-CHAIN READER
// ============================================================================

/** On-chain helper for reading and decrypting anonymous data from a reference input.
  *
  * Other contracts call `readAndDecrypt` to verify anonymous data without consuming the shared
  * UTXO. The caller provides `decKey = blake2b_256(nonce || "enc")` which decrypts the data but
  * cannot be reversed to find the nonce or link to the entry owner.
  */
@Compile
object AnonymousDataReader {

    /** Read and decrypt an anonymous data entry from a reference input.
      *
      * @param refOutput
      *   the resolved TxOut of the reference input carrying the beacon token
      * @param anonDataPolicyId
      *   policy ID of the anonymous data contract
      * @param dataKey
      *   the map key (hash of pubkeyhash and nonce)
      * @param decKey
      *   the decryption key (blake2b_256(nonce || "enc"))
      * @return
      *   the decrypted data
      */
    def readAndDecrypt(
        refOutput: TxOut,
        anonDataPolicyId: PolicyId,
        dataKey: ByteString,
        decKey: ByteString
    ): ByteString = {
        // 1. Verify beacon token
        require(
          refOutput.value
              .quantityOf(anonDataPolicyId, AnonymousDataValidator.beaconTokenName) === BigInt(1),
          "Reference input must carry anonymous data beacon token"
        )
        // 2. Extract datum
        val datum = refOutput.datum match
            case OutputDatum.OutputDatum(d) => d.to[AnonymousDataDatum]
            case _                          => fail("Expected inline datum on anonymous data UTXO")
        // 3. Lookup entry
        val encryptedData =
            datum.dataMap.get(dataKey).getOrFail("dataKey not found in anonymous data")
        // 4. XOR decrypt with key expansion
        xorDecrypt(encryptedData, decKey)
    }

    /** On-chain XOR decrypt with blake2b_256 key expansion.
      *
      * Uses `xorByteString(false, ...)` which truncates to the shorter input length, so no explicit
      * slicing is needed since `keyStream.length >= data.length`.
      */
    def xorDecrypt(data: ByteString, key: ByteString): ByteString = {
        val dataLen = lengthOfByteString(data)
        val keyStream = expandKeyOnChain(key, dataLen, key, BigInt(1))
        xorByteString(false, data, keyStream)
    }

    /** Expand key to >= targetLen bytes via blake2b_256 chaining. */
    private def expandKeyOnChain(
        baseKey: ByteString,
        targetLen: BigInt,
        accum: ByteString,
        counter: BigInt
    ): ByteString =
        if lengthOfByteString(accum) >= targetLen then accum
        else
            expandKeyOnChain(
              baseKey,
              targetLen,
              appendByteString(
                accum,
                blake2b_256(appendByteString(baseKey, consByteString(counter, ByteString.empty)))
              ),
              counter + 1
            )
}

// ============================================================================
// CONTRACT COMPILATION
// ============================================================================

private given Options = Options.release

lazy val AnonymousDataContract = PlutusV3.compile(AnonymousDataValidator.validate)
