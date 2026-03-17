# Authenticated Collections in Scalus

Starting from 0.16.0, Scalus includes a library of authenticated collections: Merkle Trees, Merkle Patricia Tries, Bilinear Accumulators, and more.

Why do we have more than one collection type? They differ in cost, proof size, mutability, and what they can prove.

## When to Use Each Collection

| Collection | Onchain Verification Cost | Proof Size | Grows with N? | Use When |
|---|---|---|---|---|
| **MerkleTree** | log2(N)+1 blake2b calls | 33 x ceil(log2(N)) bytes | Yes (logarithmic) | Static set known at deploy time. Membership-only, no mutations. |
| **IncrementalMerkleTree / FrontierMerkleTree** | Verify: log2(N)+1 blake2b. Append: 2*log2(N)+2 blake2b | 32-33 x ceil(log2(N)) bytes | Yes (logarithmic) | Append-only set, oracle-managed. Same onchain verifier; FrontierMerkleTree is an offchain optimization needing O(depth) memory instead of O(N). |
| **MerklePatriciaForestry** | ~5-15 blake2b calls per proof step | ~200-800 bytes (Data-encoded) | **No** (constant, max 2^256 entries) | General key-value with insert/delete/update + membership and non-membership proofs. Aiken-compatible. |
| **FusedMerklePatriciaForestry** | ~8-12% less CPU than MPF | ~100-500 bytes (flat ByteString) | **No** (constant, max 2^256 entries) | Same as MPF but cheaper onchain (fused proof encoding). Not Aiken-compatible. |
| **BilinearAccumulator (G2)** | 2 pairings + G1 MSM (**~35x more CPU** than tries) | ~48-96 bytes | **No** (constant, 1 group element) | Large sets where constant proof size matters. Requires trusted setup (CRS). Cheaper variant (G1 MSM). |
| **BilinearAccumulator (G1)** | 2 pairings + G2 MSM (**~35x more CPU** than tries) | ~48-96 bytes | **No** (constant, 1 group element) | Same as G2 but compatible with Ethereum KZG ceremony. Slightly more expensive (G2 MSM). |


### Decision Flowchart

1. **Static set?** -> **MerkleTree** (cheapest possible)
2. **Append-only?** -> **IncrementalMerkleTree** / **FrontierMerkleTree**
3. **Need insert + delete?** -> **FusedMerklePatriciaForestry** (cheapest) or **MerklePatriciaForestry** (Aiken-compatible)
4. **Proof size is critical** (e.g., many proofs per tx, tight tx size limits)? -> **BilinearAccumulator** (smallest constant proof size, but ~35x more CPU)
5. **Need non-membership proofs?** -> MPF, FusedMPF, or BilinearAccumulator (all support it)

### Supported Onchain Operations

| Operation | MerkleTree | IMT | MPF | FusedMPF | Accumulator |
|---|---|---|---|---|---|
| Verify membership | yes | yes | yes | yes | yes |
| Verify non-membership | — | — | yes | yes | yes |
| Insert (with proof) | — | append only | yes | yes | — |
| Delete (with proof) | — | — | yes | yes | — |
| Update (with proof) | — | — | yes | yes | — |

IMT = Incremental Merkle Tree. FrontierMerkleTree uses the same onchain verifier as IMT (it is an offchain memory optimization).

# Brief Algorithmic Descriptions

## Trees

### Merkle Tree
- Binary tree where each leaf is a hash of data and each internal node is a hash of its children. Proofs are log2(N) hashes.

```
                        ┌─────────┐
                        │  root   │
                        │ H(A|B)  │
                        └────┬────┘
                   ┌─────────┴─────────┐
              ┌────┴────┐         ┌────┴────┐
              │    A    │         │    B    │
              │H(C|D)   │         │H(E|F)   │
              └────┬────┘         └────┬────┘
            ┌──────┴──────┐     ┌──────┴──────┐
       ┌────┴────┐  ┌────┴────┐┌────┴────┐  ┌────┴────┐
       │    C    │  │    D    ││    E    │  │    F    │
       │H(d₁|d₂)│  │H(d₃|d₄)││H(d₅|d₆)│  │H(d₇|d₈)│
       └────┬────┘  └────┬────┘└────┬────┘  └────┬────┘
        ┌───┴───┐    ┌───┴───┐  ┌───┴───┐    ┌───┴───┐
       d₁      d₂  d₃      d₄ d₅      d₆  d₇      d₈

  Proof that d₃ is in the tree:  [d₄, C, B]
  Verify:  D = H(d₃|d₄),  A = H(C|D),  root = H(A|B)  ✓
```

Author — Ralph Merkle, described the idea in 1979 in "A Certified Digital Signature". The motivation examples in the article are about signing many messages at once:
instead of signing each message individually, hash them into a root and sign only root.

### Incremental Merkle Tree

The idea of append-only hash tree commitments was explored by Peter Todd in his [merkle-mountain-range](https://github.com/opentimestamps/opentimestamps-server/blob/master/doc/merkle-mountain-range.md) work for OpenTimestamps (2012–2016) — a variable-depth structure where each append grows the "mountain range". A different, fixed-depth variant became widely known through **Zcash** (2016, [Zerocash paper by Ben-Sasson et al., 2014](https://eprint.iacr.org/2014/349)): the Sapling and Orchard shielded pools use incremental Merkle trees of depth 32 to commit to note hashes — each new transaction appends a note commitment, and spending requires a membership proof against the tree root. Our implementation follows the Zcash approach: a fixed-depth binary tree filled left-to-right, with empty slots initialized to a null hash.

- Imagine already drawn MerkleTree of N = 2^D leaves which is already fully filled. On the first line, each cell contains either hash(0) or hash(data).
When you append a new leaf, you replace the leftmost hash(0) with hash(data). Then you recompute all the hashes on the path to the root.
The proof for the new leaf is the sibling hashes along that path.

```
  Before append (3 elements, depth=3):

              root
            ┌──┴──┐
           A      B=H(E|0₂)
         ┌─┴─┐    ┌─┴──┐
         C   D    E    0₂
        ┌┴┐ ┌┴┐  ┌┴┐  ┌┴┐
        d₁ d₂ d₃ 0  0  0  0  0

  Append d₄ → replace leftmost 0:

              root'
            ┌──┴──┐
           A      B'=H(E'|0₂)
         ┌─┴─┐    ┌──┴──┐
         C   D    E'    0₂
        ┌┴┐ ┌┴┐  ┌─┴─┐  ┌┴┐
        d₁ d₂ d₃ 0  d₄  0  0  0

  Recompute:  E'=H(d₄|0), B'=H(E'|0₂), root'=H(A|B')
  Proof for d₄:  [0, 0₂, A]
```

### Frontier Merkle Tree

- It's just the name of the other offchain Facade for the Incremental Merkle Tree, which holds not all N leaves, but only the "frontier" of the tree (the rightmost path of hashes).
This allows it to support up to 2^256 leaves without needing to store all the empty hashes.

```
  Full IMT stores all nodes (O(N) memory):

              root
            ┌──┴──┐
           A      B
         ┌─┴─┐    ┌─┴──┐
         C   D    E    0₂
        ┌┴┐ ┌┴┐  ┌┴┐  ┌┴┐
        d₁ d₂ d₃ d₄  0  0  0  0

  Frontier stores only the rightmost hashes (O(depth) memory):

              root
            ┌──┴──┐
           A      ●  ← frontier[2] = B
                  ┌─┴──┐
                  ●    0₂  ← frontier[1] = E
                 ┌┴┐
                 ●  0  ← frontier[0] = d₄
                  ↑
              next append goes here

  Stored:  frontier = [d₄, E, B]  (3 hashes, not 8 leaves)
           left_siblings = [A]     (frozen left subtree roots)

  To append d₅:
    1. E' = H(d₄ | d₅)        ← combine frontier[0] with new leaf
    2. B' = H(E' | 0₂)        ← combine up using pre-computed null hashes
    3. root' = H(A | B')       ← A was stored as frozen left sibling
    4. Update frontier: [d₅, E', B']
```

## Merkle Patricia Forestry

Patricia here is not a name, but an acronym for "Practical Algorithm To Retrieve Information Coded In Alphanumeric", created by Donald R. Morrison in 1968.
He invented prefix tries with path compression.

#### Prefix Trie — what it is

Every key is a 256-bit number, which we represent as a row of 64 hex nibbles (each nibble is 0–F).

```
key_A: 3  A  7  0  F  2  ...  (64 nibbles = 256 bits)
```

This is the "address" of the value in a conceptual 16^64 = 2^256 address space.

Now stack two keys and read columns left to right:

```
key_A: 3  A  7  0  F  2  ...
key_B: 3  A  7  0  F  8  ...
```

The first 5 nibbles (`3 A 7 0 F`) are identical — that's the **common prefix**. At column 5 they diverge: A goes to slot 2, B goes to slot 8.

Instead of storing all 2^256 slots, you store only the divergence points:

```
root
└─ skip "3A70F" (common prefix)
   ├─ [2] → leaf(key_A, value_A)
   └─ [8] → leaf(key_B, value_B)
```

Add a third key that diverges earlier:

```
key_A: 3  A  7  0  F  2  ...
key_B: 3  A  7  0  F  8  ...
key_C: 3  A  1  ...
```

Now the common prefix is only `3 A`. At column 2, C diverges (nibble 1) from A and B (nibble 7):

```
root
└─ skip "3A"                          ← skip node (encodes 2 nibbles)
   ├─ [1] → leaf(key_C, value_C)     ← branch: nibble 1 selects child
   └─ [7] → skip "0F"                ← branch: nibble 7 selects child, then skip node
              ├─ [2] → leaf(key_A)   ← branch: nibble 2 selects child
              └─ [8] → leaf(key_B)   ← branch: nibble 8 selects child
```

A **skip node** encodes a shared prefix — many nibbles at once (0, 1, 10, 30+, any length). This is the Patricia compression: you don't need 5 separate nodes for `3, A, 7, 0, F` — one skip node labeled `"3A70F"` does the job.

A **branch node** has up to 16 children (one per nibble 0–F). It encodes exactly 4 bits of the key — the nibble that selects which child to follow.

Not all 256 bits of the key go through branch nodes. With, say, 3 branch nodes on a path, only 12 bits are branched; the remaining 244 bits are encoded in skip prefixes (flat byte comparisons). Every path from root to leaf consumes all 256 bits.

Each node in the trie stores a hash:

- **Leaf hash** = `blake2b(suffix_nibbles ++ blake2b(value))`
- **Branch hash** = `blake2b(skip_nibbles ++ merkle_root_of_16_children)`

The root hash is a single 32-byte commitment to the entire 2^256 address space. Most of the space is empty (hash = 0x00...00), and the sparse merkle tree of 16 children efficiently hashes away all the empty slots.

#### Radix — the Forestry optimization

Now, what does a branch node actually look like inside?

In Ethereum's original MPT (radix-16, formalized in the Yellow Paper by Gavin Wood, 2014), branch nodes store 16 child hashes as a flat list. To prove membership, you provide all 15 sibling hashes at each branch step — that's 15 × 32 = 480 bytes per step.

The [Merkle Patricia Forestry](https://github.com/aiken-lang/merkle-patricia-forestry) (Aiken team) applies a well-known sparse Merkle tree technique: arrange the 16 children as a **binary tree of depth 4** (since 2^4 = 16). Now a proof needs only **log₂(16) = 4 sibling hashes** per step (one per level of the binary tree) — that's 4 × 32 = 128 bytes instead of 480. A ~73% reduction in proof size, at the cost of ~30% more CPU for verification.

#### Proofs

To prove key_A is in the trie, you walk root → leaf and at each branch collect the sibling hashes needed to reconstruct the root. The verifier replays the hash computation and checks it matches the known root.

- **Membership proof:** provide the path from root to the existing leaf. The verifier recomputes hashes and confirms they match the root.
- **Non-membership proof:** provide the path to the point where the key *would* be, showing that the slot is empty or the skip prefix doesn't match. This is why MPT supports non-membership proofs while plain Merkle trees do not.

## Fused Merkle Patricia Forestry

The Fused Merkle Patricia Forestry (`FusedMerklePatriciaForestry`) is a Scalus-specific optimization of the Merkle Patricia Forestry described above. The trie structure is the same — radix-16 with skip compression and the Forestry binary tree of depth 4 at each branch. The difference is in **how proofs and hashes are encoded**.

#### Proof encoding: flat ByteString instead of Plutus Data

In the nibble (Aiken-compatible) variant, proofs are `List[ProofStep]` encoded as Plutus Data with CBOR serialization overhead per step.

In the fused variant, proofs are packed into a single flat `ByteString` with fixed-size fields:

- **Branch step**: 130 bytes — `0x00 | skip[1] | neighbors[128]`
- **Fork step**: 68 bytes — `0x01 | skip[1] | nibble[1] | prefixLen[1] | halfLeft[32] | halfRight[32]`
- **Leaf step**: 66 bytes — `0x02 | skip[1] | key[32] | value[32]`

No CBOR, no Data deserialization — just flat byte parsing on-chain.

#### Hashing: combine3

The nibble variant hashes a branch as `blake2b(nibbles_as_bytes ++ merkle_root_of_16_children)` where `merkle_root_of_16_children` is `blake2b(halfLeft ++ halfRight)` — two nested blake2b calls.
(the 16 children are first reduced to two halves (`halfLeft`, `halfRight`) via a binary merkle tree of depth 3).

The fused variant then hashes a branch as `blake2b(skip_count ++ halfLeft ++ halfRight)` — a single blake2b call instead of two, 
saving one hash per branch step.

#### Trade-offs

- **~8–12% less CPU** than the nibble variant (fewer blake2b calls via combine3)
- **~100–500 bytes** proof size (vs ~200–800 for nibble variant) — the proofs encode the same (or more) information, but the flat fused encoding is more compact than Plutus Data/CBOR serialization
- **Not Aiken-compatible** — different hashing scheme, so cannot share roots with Aiken contracts

## Bilinear Accumulators

Bilinear accumulators represent a set as a single elliptic curve point. 
Unlike Merkle trees and tries (which use hash chains), 
accumulators use **polynomial commitments** and **pairings** on the BLS12-381 curve.

The high-level intuition is:

 - A set {a₁, a₂, …, aₙ} is encoded as a polynomial P(x) = (x + a₁)(x + a₂)…(x + aₙ).
 - This polynomial is then **committed** to a single elliptic curve point using a **polynomial commitment scheme** (specifically, [KZG commitments](https://dankradfeist.de/ethereum/2020/06/16/kate-polynomial-commitments.html)).
 - A commitment is computed from the polynomial coefficients and a trusted setup: given P(x) = c₀ + c₁x + … + cₙxⁿ and CRS points [g, τ·g, τ²·g, …], the commitment is `c₀·g + c₁·(τ·g) + … + cₙ·(τⁿ·g) = P(τ)·g` — a single group element (48 or 96 bytes) that uniquely represents the polynomial (and thus the set).
 - Properties of the polynomial (like whether it has certain roots, i.e., whether elements are in the set) can then be proven with small proofs verified using **pairings**.

The accumulator value is a single group element — constant size regardless of set size.

#### Trusted setup (CRS)

The accumulator requires a **Common Reference String** (CRS): a list of curve points [g, τ·g, τ²·g, …, τᵈ·g] where τ is a secret that must be destroyed after setup. Anyone who knows τ can forge proofs, so the setup must be performed by a trusted party or via a multi-party ceremony where at least one participant deletes their share. The maximum set size is bounded by the degree d.

#### Proofs

- **Membership:** to prove S ⊆ U, compute quotient Q = P<sub>U</sub> / P<sub>S</sub> and commit it. The verifier checks a pairing equation confirming that P<sub>S</sub> divides P<sub>U</sub>.
- **Non-membership:** to prove D ∩ U = ∅, use extended GCD to find S, T such that S·P<sub>U</sub> + T·P<sub>D</sub> = 1 (possible only when the polynomials share no roots). The proof is (S, T) committed to curve points.

#### When to use

Accumulators shine when proof size is critical — each proof is ~48-96 bytes (1-2 compressed curve points), regardless of set size. The trade-off is ~35x more CPU for on-chain verification compared to tries. Use them when you need many proofs per transaction or are hitting transaction size limits.

#### Background reading

The math behind accumulators builds on several concepts. Here are recommended tutorials for each:

- **Polynomial arithmetic and interpolation** — For concrete small examples of modular polynomial math, see the Lagrange interpolation section in [FROST: Schnorr Threshold Signatures](https://lantr.io/blog/frost-schnorr-threshold-signatures-bitcoin/).
- **KZG polynomial commitments** — The commitment scheme used here. Dankrad Feist's [KZG Polynomial Commitments](https://dankradfeist.de/ethereum/2020/06/16/kate-polynomial-commitments.html) explains the construction from scratch, including how the trusted setup works and why pairings enable verification.
- **Elliptic curve pairings** — Vitalik Buterin's [Exploring Elliptic Curve Pairings](https://medium.com/@VitalikButerin/exploring-elliptic-curve-pairings-c73c1f6b6b0) gives an accessible introduction to pairings and the bilinear map property e(a·G, b·H) = e(G, H)^(ab) that makes this all work.
- **Bilinear accumulators specifically** — The original construction is from [Nguyen (2005)](https://link.springer.com/chapter/10.1007/978-3-540-30580-4_19). For a more accessible treatment, see the accumulator section in [Boneh, Bünz, Fisch: Batching Techniques for Accumulators](https://eprint.iacr.org/2018/1188) — particularly Section 3 on bilinear accumulators with membership and non-membership proofs.

#### Scalus API

Any trusted setup ceremony can produce a CRS. Scalus provides:
- `BilinearAccumulatorProver.trustedSetup(tau, maxDegree)` to run your own ceremony — generate τ, create the CRS, then delete τ
- `Setup.fromPoints(g1Powers, g2Powers)` to load CRS points from an external ceremony
- The `scalus-ethereum-kzg-ceremony` module bundles the [Ethereum KZG ceremony](https://ceremony.ethereum.org/) as a ready-made production CRS via `EthereumKzgCeremony.loadSetup()`

Scalus provides two variants of the accumulator — **G1** and **G2** — depending on which curve group holds the accumulator point. BLS12-381 has two groups: G1 (48-byte points, cheaper arithmetic) and G2 (96-byte points). The accumulator polynomial can be committed on either group; the CRS and proofs use the corresponding groups.

| | **G2 Accumulator** | **G1 Accumulator** |
|---|---|---|
| Accumulator on | G2 | G1 |
| Off-chain prover needs | G2 powers | G1 powers |
| On-chain verifier needs | G1 powers | G2 powers |
| Proof on | G2 | G1 |
| On-chain cost | Cheaper (G1 MSM) | Slightly more expensive (G2 MSM) |

Both use 2 pairings for verification. The G1 variant costs **~35x more CPU** than tries (benchmarked at 32K elements). The G2 variant would be cheaper on-chain (G1 MSM instead of G2 MSM) but the off-chain prover requires many G2 powers to build the accumulator — the Ethereum KZG ceremony has 32768 G1 powers but only 65 G2 powers, so only the G1 variant is practical with it.

## Optimizations Journey

 It can be tricky.  If we go through algorithms, each local optimum is an equilibrium between different aspects, where improving one aspect may worsen another.
 Therefore, instead of a 'simple elegant idea', we can end up with a complex unwieldy beast, which is hard to explain.

For example, looking at Merkle Patricia Tree: the first impulse is to check a very elegant mathematical structure, which we can name Merkle Patricia Tree (i.e. radix-2).
Each prefix is 2 bits, branch nodes are minimal (2 children), proofs are minimal (1 sibling hash per branch step).
One problem - 256 steps per path, 512 blake2b calls per proof. That's a lot of CPU.

For a general radix-2ᵏ MPF (k bits per branch step), we can derive the formulas. A 256-bit key is consumed k bits at a time, giving **256/k branch steps** along a path (worst case, no skip compression). At each step, the Forestry binary tree of depth k requires **k blake2b calls** to reconstruct the merkle root from siblings, plus **1 blake2b call** to combine the prefix with the merkle root. The proof carries **k sibling hashes** (one per level of the inner binary tree), each 32 bytes. So:

> **Blake2b calls per proof ≈ 256/k × (k + 1) = 256 + 256/k**
>
> **Branch proof size per step = k × 32 bytes**

| Radix | k | Steps (worst case) | Blake2b calls | Branch proof bytes/step |
|-------|---|---------------------|---------------|-------------------------|
| 2     | 1 | 256                 | 512           | 32                      |
| 16    | 4 | 64                  | 320           | 128                     |
| 64    | 6 | ~43                 | ~299          | 192                     |
| 256   | 8 | 32                  | 288           | 256                     |

The blake2b count decreases with larger radix (converging to 256), so **larger radix is always cheaper in CPU**.

What about proof size? The number of branch steps on a path depends on how many keys share prefixes — not on the radix. With uniform keys (blake2b-hashed), the expected number of branch nodes on a path is **B ≈ log₂(N) / k**. Since each branch costs k × 32 bytes of siblings:

> **Actual proof size ≈ B × k × 32 = (log₂(N) / k) × k × 32 = log₂(N) × 32**

The k cancels out. With uniform keys, the branch sibling bytes are approximately the same regardless of radix. The remaining proof overhead (step headers, skip prefixes, serialization) is small and roughly constant per step.

#### Benchmark (unfused)

We compiled MPF-2, MPF-16 and MPF-64 on-chain verifiers (all unfused, Data-encoded proofs) and measured the cost of a standalone `has()` call (no transaction overhead) across collection sizes:

| N | Variant | CPU | Memory | Fee | Proof (B) |
|---|---------|-----|--------|-----|-----------|
| 30 | MPF-2 | 39,899,850 | 133,522 | 10,581 | 238 |
| 30 | MPF-16 | 24,549,465 | 87,845 | 6,839 | 252 |
| 30 | MPF-64 | 27,582,061 | 100,332 | 7,778 | 236 |
| 100 | MPF-2 | 53,547,765 | 178,036 | 14,134 | 330 |
| 100 | MPF-16 | 35,254,107 | 122,665 | 9,620 | 323 |
| 100 | MPF-64 | 34,212,855 | 123,597 | 9,599 | 372 |
| 1,000 | MPF-2 | 64,170,230 | 211,647 | 16,839 | 398 |
| 1,000 | MPF-16 | 37,066,889 | 129,836 | 10,165 | 413 |
| 1,000 | MPF-64 | 33,241,765 | 121,068 | 9,383 | 419 |
| 32,000 | MPF-2 | 100,029,655 | 328,930 | 26,192 | 645 |
| 32,000 | MPF-16 | 55,386,806 | 191,515 | 15,044 | 609 |
| 32,000 | MPF-64 | 46,582,977 | 167,625 | 13,031 | 624 |

At 1K+ elements, MPF-64 uses **10–16% less CPU** than MPF-16. MPF-2 is ~80% more expensive. The proof sizes are nearly identical across radixes, confirming the log₂(N) × 32 prediction.

The `insert()` results (using a combined single-pass optimization to compute both excluding and including roots simultaneously) are more mixed:

| N | Variant | CPU | Memory | Fee |
|---|---------|-----|--------|-----|
| 30 | MPF-2 | 48,495,553 | 164,030 | 12,962 |
| 30 | MPF-16 | 38,380,526 | 136,916 | 10,668 |
| 30 | MPF-64 | 34,192,374 | 127,785 | 9,839 |
| 100 | MPF-2 | 61,801,320 | 207,787 | 16,446 |
| 100 | MPF-16 | 41,467,658 | 150,413 | 11,669 |
| 100 | MPF-64 | 42,747,508 | 158,537 | 12,230 |
| 1,000 | MPF-2 | 86,230,755 | 288,755 | 22,879 |
| 1,000 | MPF-16 | 54,647,249 | 196,775 | 15,294 |
| 1,000 | MPF-64 | 52,396,164 | 193,902 | 14,966 |
| 32,000 | MPF-2 | 120,584,161 | 401,727 | 31,874 |
| 32,000 | MPF-16 | 69,631,026 | 250,450 | 19,472 |
| 32,000 | MPF-64 | 72,937,542 | 267,852 | 20,714 |

Insert is mixed — MPF-64 wins at N=30 and 1K, but loses at 100 and 32K. The combined single-pass traversal has different cost characteristics than a simple `has()`.

But everything changes when we compare full transaction costs with reference scripts. We wrote a simple validator, which maintains a single MPF collection in its datum and supports `withdraw` and `deposit` operations with proofs. 
Then we benchmarked the full transaction costs (fees, CPU, memory):


| Variant | op | fee | CPU | mem | txSize | proof (B) |
|---------|-----|---------|-------------|---------|--------|-----------|
| MPF-2 | withdraw | 287,983 | 173,039,003 | 574,868 | 1,162 | 622 |
| MPF-16 | withdraw | 287,027 | 119,282,279 | 415,719 | 1,132 | 593 |
| MPF-64 | withdraw | 298,528 | 120,209,248 | 424,809 | 1,177 | 637 |
| MPF-2 | deposit | 285,122 | 167,672,401 | 558,604 | 1,127 | 626 |
| MPF-16 | deposit | 283,052 | 114,165,751 | 397,909 | 1,074 | 572 |
| MPF-64 | deposit | 295,307 | 116,659,208 | 412,552 | 1,125 | 624 |

Despite MPF-64's 12–17% CPU advantage in raw UPLC `has()`, it **loses** in full transactions. Even MPF-2, with ~45% more CPU, has nearly the same fee as MPF-16 (287,983 vs 287,027) thanks to its tiny validator.

The Cardano fee has three components: `fee = txSizeFee + exUnitsFee + refScriptFee`. Here's the breakdown for MPF-16 (withdraw, fee = 287,027):

| Category | lovelace | % of total |
|----------|---------|-----------|
| Base fee (fixed) | 155,381 | 54.1% |
| Tx size (1,132 bytes × 44) | 49,808 | 17.4% |
| Reference script | ~49,251 | 17.2% |
| Memory (415,719 × 0.0577) | 23,987 | 8.4% |
| CPU (119.3M × 0.0000721) | 8,600 | 3.0% |

The base fee dominates (54%). Reference script and tx size contribute equally (~17% each). Execution units (CPU + memory) are only 11.4% of the total fee.
This explains why MPF-2 — despite 45% more CPU — has nearly the same fee: its smaller validator saves on reference script cost, offsetting the execution cost increase.

The fee difference breakdown (MPF-64 vs MPF-16, withdraw, Δ = +11,501):

| Category | Δ lovelace | % of diff |
|----------|-----------|-----------|
| Reference script (larger validator) | +8,930 | 78% |
| Tx size (+45 bytes × 44) | +1,980 | 17% |
| Memory (+9,090 units) | +524 | 5% |
| CPU (+0.9M steps) | +67 | 1% |

The dominant factor is **reference script cost** (78%). Even though the validator code doesn't travel in the transaction body (we use reference scripts),
Cardano charges a tiered fee based on reference script size (15 lovelace/byte, ×1.2 multiplier per 25,600-byte tier). MPF-64's larger validator (with `merkle64`, `sparseMerkle64` functions) costs more.


### Fusing optimization

We can fuse both the hash computations and the proof encoding.

The idea of fusing is
  - combine multiple calls of hash computations into a single call, reducing the total number of calls and thus saving CPU.
  - combine proof encoding into a single flat binary format, eliminating serialization overhead and reducing proof size. 
  - pass parts of the proof directly into the hash computation as already concatenated bytes, avoiding intermediate data structures and redundant hashing.

Start with combining computations: at each branch step, the unfused variant computes `blake2b(prefix ++ blake2b(halfLeft ++ halfRight))` — two blake2b calls. 
By fusing these into a single `blake2b(skipLen ++ halfLeft ++ halfRight)` (called `combine3`), we save one blake2b call per branch step.
The blake2b calls per proof with fusing become:
> **Blake2b calls (fused) ≈ 256/k × k = 256** (constant, independent of radix!)

Then, if we represent proof as a byte string, we can run hash computation over the proof bytes directly, without deserialization overhead. For some types of nodes,
we can just hash the proof bytes as they are. 

#### Benchmark: MPF-16 unfused vs FusedMPF-16

`has()` comparison:

| N | Variant | CPU | Memory | Fee | Proof (B) |
|---|---------|-----|--------|-----|-----------|
| 30 | MPF-16 | 24,549,465 | 87,845 | 6,839 | 252 |
| 30 | FusedMPF-16 | 22,631,145 | 83,475 | 6,449 | 244 |
| 100 | MPF-16 | 35,254,107 | 122,665 | 9,620 | 323 |
| 100 | FusedMPF-16 | 32,051,872 | 115,511 | 8,976 | 311 |
| 1,000 | MPF-16 | 37,066,889 | 129,836 | 10,165 | 413 |
| 1,000 | FusedMPF-16 | 34,182,522 | 124,071 | 9,624 | 398 |
| 32,000 | MPF-16 | 55,386,806 | 191,515 | 15,044 | 609 |
| 32,000 | FusedMPF-16 | 50,652,213 | 182,352 | 14,174 | 586 |

FusedMPF-16 is consistently **~8% cheaper in CPU** and **~5% cheaper in memory** for `has()` across all collection sizes. The proof sizes are also ~4% smaller thanks to the flat binary encoding. Combined fee savings: ~6%.

`insert()` comparison:

| N | Variant | CPU | Memory | Fee |
|---|---------|-----|--------|-----|
| 30 | MPF-16 | 38,380,526 | 136,916 | 10,668 |
| 30 | FusedMPF-16 | 33,373,523 | 121,214 | 9,401 |
| 100 | MPF-16 | 41,467,658 | 150,413 | 11,669 |
| 100 | FusedMPF-16 | 36,520,296 | 134,103 | 10,371 |
| 1,000 | MPF-16 | 54,647,249 | 196,775 | 15,294 |
| 1,000 | FusedMPF-16 | 48,116,882 | 175,439 | 13,593 |
| 32,000 | MPF-16 | 69,631,026 | 250,450 | 19,472 |
| 32,000 | FusedMPF-16 | 61,165,915 | 222,972 | 17,276 |

FusedMPF-16 saves **~12% CPU** and **~11% memory** on `insert()` — the combined single-pass traversal benefits more from fusing because it computes two roots (excluding and including) simultaneously, doubling the per-step blake2b savings. Combined fee savings: ~11%.

Compiled program sizes: MPF-16 (unfused) = 1,467 bytes, FusedMPF-16 = 1,460 bytes — virtually identical. 

In full transactions (emulator), fusing saves 0.75–1% in total fee:

| Variant | op | N | fee | CPU | mem | txSize | proof (B) |
|---------|-----|---|---------|-------------|---------|--------|-----------|
| MPF-16 | withdraw | 32K | 287,027 | 119,282,279 | 415,719 | 1,132 | 593 |
| FusedMPF-16 | withdraw | 32K | 284,865 | 111,441,826 | 390,672 | 1,112 | 572 |
| MPF-16 | deposit | 32K | 283,052 | 114,165,751 | 397,909 | 1,074 | 572 |
| FusedMPF-16 | deposit | 32K | 280,927 | 106,332,852 | 373,105 | 1,054 | 552 |
| MPF-16 | withdraw | 1M | 298,663 | 137,858,158 | 479,472 | 1,283 | 740 |
| FusedMPF-16 | withdraw | 1M | 295,607 | 127,402,136 | 446,777 | 1,256 | 713 |
| MPF-16 | deposit | 1M | 297,231 | 135,013,911 | 470,793 | 1,266 | 760 |
| FusedMPF-16 | deposit | 1M | 294,138 | 124,550,336 | 437,855 | 1,239 | 733 |

Despite ~7% savings in CPU and ~6% in memory, the total fee savings are only ~0.75% (N=32K) to ~1% (N=1M). This is because the Cardano fee is dominated by the base fee (54%) — execution units contribute only ~11% of the total.

Fee difference breakdown (MPF-16 → FusedMPF-16, withdraw N=32K, Δfee = −2,162):

| Category | Δ lovelace | % of saving |
|----------|-----------|------------|
| Memory (−25,047 units) | −1,445 | 66.8% |
| Tx size (proof −21B) | −880 | 40.7% |
| CPU (−7.8M steps) | −565 | 26.1% |
| RefScript (fused validator is larger) | +728 | −33.7% |

Memory reduction is the largest factor (~67%): fusing eliminates intermediate ByteString allocations from separate prefix hashing. Proof shrinking (~41%) comes from flat binary encoding saving ~20B per proof. CPU savings (~26%) come from fewer blake2b calls. The fused validator is slightly larger (3,323B vs 3,275B), partially offsetting the gains via higher reference script fees.

For light validators (proof-only, no output/lovelace checks), the savings are more noticeable (~3.8%) because execution is a larger share of the fee. However, fusing is still worthwhile when you hit the per-transaction CPU/memory limits (14B steps, 10M mem units) or transaction size limits — the ~7–12% execution budget savings and ~4% smaller proofs provide headroom that can make the difference between a transaction that fits and one that doesn't.

#### Fused radix comparison: does radix still matter?

Quick idea — maybe the fused MPF-2 has a chance because of tiny validator, which saves on reference script fees, and the CPU explosion is not so bad because of fusing?
Let's see:

 | Category | Δ lovelace | % of saving |
 |----------|-----------|-----------|
 | Memory (+283,184 units × 0.0577) | +16,341 | — |
 | CPU (+83.3M steps × 0.0000721) | +5,998 | — |
 | Reference script (smaller validator!) | −10,695 | — |
 | Tx size (+3 bytes × 44) | +132 | — |

Nope.  FusedMPF-2's tiny validator saves on reference script fees, but the 2× CPU and memory explosion (more branch steps = more per-step UPLC overhead: pattern matching, cursor arithmetic, sliceByteString) overwhelms the savings. Radix-16 remains the sweet spot across all encodings.

### Benchmarking Incremental Merkle Tree

 In a typical oracle use case, we have a stream of data points (e.g. price updates) that we want to commit to on-chain. And clients should have a proof that some data is based on the latest N data points, but we don't care about old data points.
So, we need proof of membership, but we don't need non-membership proofs.

 We can use an incremental Merkle tree to maintain a running commitment to the latest N data points, allowing us to prove membership of any recent data point with a compact proof.

So, let's write a simple contract, which performs only append operation and benchmark, how we can save in comparison to fused MPF-16.

#### IMT vs FusedMPF-16: emulator benchmark

IMT validator script size: 1,329 bytes (vs 3,323 bytes for FusedMPF-16). The IMT validator is 60% smaller because the on-chain logic is much simpler: a binary tree with `combine(left, right) = blake2b_256(left ++ right)` and linear walk from leaf to root.

Membership verification (`withdraw`):

| Variant | N | fee | CPU | mem | txSize | proof (B) |
|---------|---|---------|-------------|---------|--------|-----------|
| IMT | 32K | 253,350 | 87,911,926 | 308,948 | 1,045 | 513 |
| FusedMPF-16 | 32K | 284,865 | 111,441,826 | 390,672 | 1,112 | 572 |
| IMT | 100K | 257,587 | 91,870,073 | 324,204 | 1,115 | 581 |
| FusedMPF-16 | 100K | 285,647 | 113,122,334 | 396,098 | 1,120 | 579 |
| IMT | 1M | 263,898 | 97,812,763 | 347,088 | 1,218 | 683 |
| FusedMPF-16 | 1M | 295,607 | 127,402,136 | 446,777 | 1,256 | 713 |

IMT is **11–12% cheaper** in total fee for membership verification. At N=32K, it saves 31,515 lovelace per transaction.

Fee difference breakdown (IMT vs FusedMPF-16, withdraw N=32K, Δfee = −31,515):

| Category | Δ lovelace | % of saving |
|----------|-----------|------------|
| RefScript (60% smaller validator) | −22,157 | 70.3% |
| Memory (−81,724 units) | −4,715 | 15.0% |
| Tx size (−67 bytes) | −2,948 | 9.4% |
| CPU (−23.5M steps) | −1,695 | 5.4% |

The dominant saving is reference script cost (70%) — the IMT validator is much smaller. But IMT also wins on execution: 21% less CPU and 21% less memory, because the binary Merkle proof walk is simpler (just `blake2b_256(left ++ right)` per level, no radix-16 merkle/sparseMerkle functions).

Append (`add` for IMT, `deposit` for FusedMPF-16):

| Variant | N | fee | CPU | mem | txSize | proof (B) |
|---------|---|---------|-------------|---------|--------|-----------|
| IMT add | 32K | 252,266 | 94,441,577 | 322,949 | 991 | 498 |
| FusedMPF-16 deposit | 32K | 280,927 | 106,332,852 | 373,105 | 1,054 | 552 |
| IMT add | 1M | 263,372 | 107,885,827 | 370,129 | 1,160 | 662 |
| FusedMPF-16 deposit | 1M | 294,138 | 124,550,336 | 437,855 | 1,239 | 733 |

IMT append is **10–11% cheaper** than FusedMPF-16 insert. Note that IMT `add` is a single-pass operation that verifies the empty slot AND computes the new root with ~2D blake2b calls, while FusedMPF-16 `insert` needs a combined traversal with ~512 blake2b calls.

IMT proof sizes scale as D×33 bytes (membership) or D×32 bytes (append), where D = ceil(log₂(N)). At N=1M, D=20, so proofs are ~660–683 bytes — comparable to FusedMPF-16's ~713–733 bytes.

The tradeoff: IMT only supports **append** (sequential insertion) and **membership verification**. It cannot delete elements or prove non-membership. For oracle use cases where you only add data points and verify membership, IMT is the clear winner.

#### IMT scaling with depth

The on-chain cost scales linearly with tree depth D:

| N | D | withdraw fee | withdraw CPU | add fee | add CPU | proof (B) |
|---|---|-------------|-------------|---------|---------|-----------|
| 10 | 4 | 230,210 | 66,136,510 | 230,118 | 67,565,376 | 139 |
| 100 | 7 | 236,508 | 72,074,704 | — | — | 241 |
| 10K | 14 | 251,227 | 85,930,784 | — | — | 479 |
| 32K | 15 | 253,350 | 87,911,926 | 252,266 | 94,441,577 | 513 |
| 100K | 17 | 257,587 | 91,870,073 | 256,814 | 99,822,428 | 581 |
| 1M | 20 | 263,898 | 97,812,763 | 263,372 | 107,885,827 | 683 |

Each extra depth level adds ~2M CPU steps (~144 lovelace) for verification and ~3.4M steps for append. The fee grows gently from 230K (D=4) to 264K (D=20).

An important off-chain advantage: it is possible to generate append proofs without storing the full history of data points. The `FrontierMerkleTree` stores only D sibling hashes (the "frontier" — the rightmost path of the tree), requiring O(D) memory instead of O(N). For D=20 that's just 20 × 32 = 640 bytes regardless of how many millions of elements have been appended. The on-chain costs are identical — the validator doesn't care how the proof was generated. The off-chain build time is higher (12s vs 2s for 1M elements) because the frontier tree recomputes intermediate hashes rather than looking them up in a stored structure, but for an oracle that appends one data point per block this is negligible.

For extreme depths (D=64, D=256), the costs grow proportionally:
- D=64 (N≤2⁶⁴): fee=359,342, CPU=226M, proof=2,114B
- D=256 (N≤2²⁵⁶): fee=779,925, CPU=742M, proof=8,450B

Even D=256 stays well within Cardano's per-transaction budget (14B CPU steps, 10M memory units).

### Conclusion

For **append-only** use cases (oracles, event logs, audit trails), IMT is 11–12% cheaper than FusedMPF-16 thanks to its smaller validator and simpler on-chain logic.

For **general set membership** (with insert + delete + non-membership proofs), MPF-16 and FusedMPF-16 remain the best choice. Radix-16 hits the sweet spot: small enough validator for low reference script fees, large enough branching factor to minimize per-step UPLC overhead.

For **tight transaction size constraints**, BilinearAccumulator is the option — each proof is 48 bytes regardless of set size. The ~35× CPU overhead sounds dramatic, but in real transactions the base fee and reference script fee dominate, so the actual fee increase is more moderate. Accumulators are most valuable when packing many proofs into one transaction or approaching the 16KB tx size limit.

**A note on methodology:** Looking only at script execution costs (CPU and memory) can be misleading. A 7–8% reduction in execution budget may sound significant, but execution costs constitute only ~11% of the total transaction fee on Cardano — the base fee alone accounts for over 50%. When we measure real transactions through the emulator, that 7–8% execution saving translates to less than 1% in actual fee. Similarly, a 2× difference in standalone UPLC evaluation cost between two variants may shrink to a modest 10–15% difference in total fee once reference script size, transaction size, and the base fee are factored in. Always benchmark with full transactions to get the true picture.