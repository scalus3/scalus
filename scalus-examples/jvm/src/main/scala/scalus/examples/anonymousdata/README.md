# Anonymous Data Contract

On-chain anonymous storage. Users can store, update, and delete data entries that are visible to everyone but
unlinkable to their identity.

## How it works

All participants share a single UTxO (identified by a beacon token) so that individual read/write patterns don't leak
identity. The datum contains a Merkle tree root of authorized participants and a map of encrypted entries.

Each entry is keyed by `blake2b_256(pubkeyhash || nonce)` — unlinkable without the secret nonce. Values are
XOR-encrypted with a key derived from `blake2b_256(nonce || "enc")`.

- **MintBeacon / BurnBeacon** — the admin creates or destroys the shared UTxO.
- **StoreData / UpdateData / DeleteData** — a participant proves membership via a Merkle tree proof, then adds, updates,
  or removes their entry.
- **UpdateParticipants** — the admin changes the Merkle tree root.

### On-chain reading

Other contracts can read anonymous data via CIP-31 reference inputs. The `AnonymousDataReader` helper (in
`AnonymousDataValidator.scala`) verifies the beacon token, looks up an entry, and XOR-decrypts it. The caller provides
`decKey = blake2b_256(nonce || "enc")`, which decrypts the data without revealing the nonce or the owner's identity.

`AnonymousDataGateValidator.scala` demonstrates this pattern: a spending contract that releases locked funds only when
someone proves that specific data exists in the anonymous data store.

`AnonymousDataValidator.scala` is the on-chain validator. `AnonymousDataCrypto.scala` handles off-chain key derivation
and encryption. `AnonymousDataTransactions.scala` builds all transactions including gate operations.
