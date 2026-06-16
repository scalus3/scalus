# Anonymous Data

Store data on-chain, associated with a cryptographic hash, so that only someone who can reproduce that hash can
retrieve it.

The interesting part: on Cardano this needs **no smart contract**. It is built entirely from a native ledger
feature — the **datum hash** — and all logic is off-chain. The example exists to show that this requirement does not
call for on-chain execution at all.

## How it works

Every Cardano transaction output may carry a datum as either the value itself (an *inline datum*) or just its
**hash** (`DatumOption.Hash`). A datum hash is a 32-byte commitment: the preimage is never written to the chain.

We commit to the preimage `Data.List([B(nonce), data])`:

- **Store** — create a UTxO carrying only `blake2b_256(serialise([nonce, data]))`. The chain shows a hash; the data
  stays private. The UTxO sits at the owner's address, so only the owner's key can spend it.
- **Retrieve** — reveal `(nonce, data)`; anyone recomputes the hash and checks it against the on-chain UTxO. Only
  someone who already knows the preimage can produce a match. This is pure off-chain verification — no transaction,
  no script. (The ledger will not even let you attach a datum when spending an ordinary key-locked UTxO, so
  disclosure is inherently an off-chain act: the commitment is already on-chain, and revealing the preimage to a
  verifier is all that "retrieve" requires.)

### Why the nonce

Without it, low-entropy `data` — a vote, a yes/no flag, a small number — could be brute-forced by hashing every
candidate and comparing. The random `nonce` makes the preimage high-entropy, so the commitment is genuinely hiding.
A fresh nonce per entry also makes the same `data` stored twice produce two **unlinkable** hashes.

### What this does and does not hide

A datum hash hides the **contents** of an entry until its owner chooses to reveal them — confidentiality with
selective disclosure. It does **not** hide *who stored it*: the storing transaction is signed by some wallet, so on a
public chain an observer can link that wallet to the UTxO it created. Breaking that link is a separate, much harder
problem that needs a relayer plus a zero-knowledge membership proof (e.g. a bilinear accumulator or a zk-SNARK), not
a hash. This example deliberately stays within what the native primitive provides.

## Files

`AnonymousDataTransactions.scala` builds the store/retrieve/reveal transactions and the off-chain verification. There
is no validator — that is the point.
