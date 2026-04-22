# Anonymous Data Contract

On-chain anonymous storage in Scalus.

## Problem

Store data on-chain so that only the owner can link it to their identity. Others can see the data exists but can't determine who owns which entry.

## Cardano Adaptation

Unlike account-based blockchains where each user has a visible account, Cardano uses UTXOs. Our design uses:

1. **Shared UTXO** — All participants reference a single UTXO (identified by a beacon token), so individual read/write patterns don't leak identity.
2. **Hash-based keys** — `blake2b_256(pubkeyhash || nonce)` maps are unlinkable without knowing the secret nonce.
3. **Encrypted values** — Data is XOR-encrypted with a key derived from `blake2b_256(nonce || "enc")`; only the nonce holder can decrypt.
4. **Separate decrypt key** — For on-chain usage by other contracts, a user provides `decKey = blake2b_256(nonce || "enc")` which can't be reversed to find the nonce or link to the entry owner.

## Design

### Datum

```scala
case class AnonymousDataDatum(
    participantsRoot: ByteString,                // MerkleTree root of authorized pubkeyhashes
    dataMap: AssocMap[ByteString, ByteString]     // hash(pkh, nonce) -> encrypt(data, deriveEncKey(nonce))
)
```

### Operations

| Operation | Who | What happens |
|-----------|-----|-------------|
| MintBeacon | Admin | Creates shared UTXO with beacon token, empty data map |
| BurnBeacon | Admin | Destroys shared UTXO |
| StoreData | Participant (MerkleTree proof) | Adds new entry to data map |
| UpdateData | Participant (MerkleTree proof) | Updates existing entry |
| DeleteData | Participant (MerkleTree proof) | Removes entry from data map |
| UpdateParticipants | Admin | Changes the MerkleTree root |

### Anonymity Model

| Scenario | What user provides | What's revealed |
|----------|-------------------|-----------------|
| Off-chain read | nothing | nothing (reads chain locally, decrypts with nonce) |
| On-chain proof | decKey + point to entry | decrypted data, NOT identity |
| Store/Update/Delete | membership proof + signature | signer visible at write-time only |

## On-Chain Reading via Reference Inputs

The anonymous data store can be read on-chain by other contracts using CIP-31 reference inputs. The key insight: providing `decKey = blake2b_256(nonce || "enc")` decrypts data without revealing the nonce or the owner's identity.

### AnonymousDataReader

`AnonymousDataReader.readAndDecrypt(refOutput, policyId, dataKey, decKey)` is an on-chain helper that:
1. Verifies the reference input carries the beacon token
2. Extracts the datum and looks up the entry by `dataKey`
3. XOR-decrypts with `decKey` using blake2b_256 key expansion
4. Returns the decrypted `ByteString`

### Example: Gate Contract

`AnonymousDataGateValidator` demonstrates this pattern — a spending contract that releases locked funds only when someone proves specific data exists in the anonymous data store:

1. **Lock:** `createGate(expectedDataHash, lockedValue)` — lock funds with `GateDatum(blake2b_256(expectedData))`
2. **Unlock:** `unlockGate(gateUtxo, sharedUtxo, dataKey, decKey)` — provide the anonymous data UTXO as a reference input, the gate reads and decrypts the entry, and verifies `blake2b_256(decryptedData) == expectedDataHash`

Anyone with the `dataKey` and `decKey` can unlock the gate — the anonymous data owner's identity is never revealed on-chain.

## Files

- `AnonymousDataValidator.scala` — On-chain validator (ParameterizedValidator[ByteString])
- `AnonymousDataReader` — On-chain helper for reading/decrypting anonymous data from reference inputs (in AnonymousDataValidator.scala)
- `AnonymousDataGateValidator.scala` — Example gate contract using on-chain anonymous data reading
- `AnonymousDataCrypto.scala` — Off-chain key derivation + XOR encryption
- `AnonymousDataTransactions.scala` — Off-chain transaction builder (includes gate operations)
- `AnonymousDataTest.scala` — Test suite

## Running

```bash
sbtn "scalusExamplesJVM/testOnly scalus.examples.anonymousdata.AnonymousDataTest"
```
