Source: https://scalus.org/docs/advanced-data-structures/merkle-tree

# Merkle Tree

A binary tree where each leaf is a hash of data and each internal node is a hash of its children. Proofs are log₂(N) hashes.

**Best for:** Static sets known at deploy time — airdrop allowlists, governance voter registries, configuration snapshots.

## How It Works

```
                       ┌─────────┐
                       │  root   │
                       │ H(A|B)  │
                       └────┬────┘
               ┌────────────┴──────────┐
          ┌────┴────┐             ┌────┴────┐
          │    A    │             │    B    │
          │ H(C|D)  │            │ H(E|F)  │
          └────┬────┘             └────┬────┘
        ┌──────┴──────┐          ┌─────┴──────┐
   ┌────┴────┐   ┌────┴────┐┌────┴────┐  ┌────┴────┐
   │    C    │   │    D    ││    E    │  │    F    │
   │ H(d₁|d₂)│  │ H(d₃|d₄)││ H(d₅|d₆)│  │ H(d₇|d₈)│
   └────┬────┘   └────┬────┘└────┬────┘  └────┬────┘
    ┌───┴───┐     ┌───┴───┐  ┌───┴───┐    ┌───┴───┐
   d₁      d₂     d₃      d₄ d₅      d₆   d₇      d₈

Proof that d₃ is in the tree:  [d₄, C, B]
Verify:  D = H(d₃|d₄),  A = H(C|D),  root = H(A|B)  ✓
```

To verify membership, the on-chain validator receives the element and its proof (the sibling hashes along the path to the root). It recomputes the hashes from leaf to root and checks that the result matches the known root hash.

## Cost

- **Verification:** log₂(N) + 1 blake2b calls
- **Proof size:** 33 × ceil(log₂(N)) bytes

## Limitations

- **No mutations** — the tree is built once and cannot be modified on-chain
- **Membership only** — cannot prove that an element is *not* in the set

## Background

Ralph Merkle described the idea in 1979 in "A Certified Digital Signature". The motivation was signing many messages at once: instead of signing each message individually, hash them into a root and sign only the root.

## Example: Membership Token Validator

The [MembershipTokenValidator](https://github.com/scalus3/scalus/tree/master/scalus-examples/jvm/src/main/scala/scalus/examples/MembershipToken.scala) uses a Merkle Tree to gate token minting — only members in a pre-built allowlist can mint. The Merkle root is baked into the script at deployment time via `ParameterizedValidator[ByteString]`.

**On-chain** — verify the signer is in the allowlist:

```scala
import scalus.cardano.onchain.plutus.crypto.tree.MerkleTree

// merkleRoot is the script parameter (baked in at deploy time)
// signer.hash is the pubkeyhash of the transaction signer
// proof is the sibling hashes along the path to the root
MerkleTree.verifyMembership(merkleRoot, signer.hash, proof)
```

**Off-chain** — build the tree and generate a proof:

```scala
import scalus.crypto.tree.MerkleTree

// Build the tree from a list of member pubkeyhashes
val members: Seq[ByteString] = Seq(alice.hash, bob.hash, charlie.hash)
val tree = MerkleTree.fromHashes(members)
val root = tree.rootHash  // bake this into the script parameter

// Generate a membership proof for alice
val proof = tree.proveMembership(alice.hash)
```

The [AnonymousDataValidator](https://github.com/scalus3/scalus/tree/master/scalus-examples/jvm/src/main/scala/scalus/examples/anonymousdata/AnonymousDataValidator.scala) demonstrates another pattern: Merkle Tree for participant authorization combined with `AssocMap` for encrypted key-value storage.

## Related

- [Incremental Merkle Tree](/docs/advanced-data-structures/incremental-merkle-tree) — append-only variant for dynamic sets
- [Merkle Patricia Forestry](/docs/advanced-data-structures/merkle-patricia-forestry) — general key-value with insert/delete support
