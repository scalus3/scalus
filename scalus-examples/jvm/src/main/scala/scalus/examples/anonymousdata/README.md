# Anonymous Data Contract

On-chain anonymous storage. Users store data entries that are visible to everyone but unlinkable to their identity.

## How it works

All participants share a single UTxO (identified by a beacon token) so that individual write patterns don't leak
identity. The datum contains a Merkle tree root of authorized participants and a map of encrypted entries.

Each entry is keyed by `blake2b_256(pubkeyhash || nonce)` — unlinkable without the secret nonce. Values are
XOR-encrypted with a key derived from `blake2b_256(nonce || "enc")`.

- **MintBeacon / BurnBeacon** — the admin creates or destroys the shared UTxO.
- **StoreData** — a participant proves membership via a Merkle tree proof and adds an entry under their key.
- **UpdateParticipants** — the admin changes the Merkle tree root.

### Append-only by design (this is what keeps it anonymous)

There is intentionally **no Update and no Delete**, following the rosetta `anonymous_data` reference (store-once,
off-chain reads). Entries are immutable: a second `StoreData` at an occupied key is rejected, so neither the owner nor
anyone else can change or remove a stored value.

This is a deliberate privacy trade-off, not a missing feature. An entry's key is `blake2b_256(pubkeyhash || nonce)`,
so the only way to authorize a mutation on-chain would be to reveal the `nonce` — which would publish the
`signer ↔ entry` link and destroy the anonymity the contract exists to provide. Mutable anonymous storage needs a
nullifier or zero-knowledge scheme; this example keeps the secret off-chain instead. Immutability also removes the
entire class of "any participant overwrites/deletes another's entry" attacks for free.

To "change" data, store a new entry under a fresh nonce. Reading never reveals the nonce (see below), so anonymity
holds for the lifetime of the data.

### On-chain reading

Other contracts can read anonymous data via CIP-31 reference inputs. The `AnonymousDataReader` helper (in
`AnonymousDataValidator.scala`) verifies the beacon token, looks up an entry, and XOR-decrypts it. The caller provides
`decKey = blake2b_256(nonce || "enc")`, which decrypts the data without revealing the nonce or the owner's identity.

`AnonymousDataGateValidator.scala` demonstrates this pattern: a spending contract that releases locked funds only when
someone proves that specific data exists in the anonymous data store.

`AnonymousDataValidator.scala` is the on-chain validator. `AnonymousDataCrypto.scala` handles off-chain key derivation
and encryption. `AnonymousDataTransactions.scala` builds all transactions including gate operations.
