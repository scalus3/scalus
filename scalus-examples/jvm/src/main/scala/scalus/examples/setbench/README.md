# Set Membership Benchmark

Benchmark of authenticated set data structures for Cardano smart contracts.
Measures per-operation transaction cost (fee, CPU, memory, proof size) at N=32K
elements, using the emulator with reference scripts.

## Results (N=32K)

### Verify membership

| Variant  | Fee     | CPU         | Memory  | Tx Size | Proof |
|----------|---------|-------------|---------|---------|-------|
| IMT-D15  | 259,196 |  86,763,084 | 305,006 | 1,045 B | 513 B |

MPF also supports `has()` but it is not benchmarked separately here —
insert/delete costs below include membership verification.

### Insert element

| Variant  | Fee     | CPU         | Memory  | Tx Size | Proof |
|----------|---------|-------------|---------|---------|-------|
| MPF-16b  | 287,639 | 107,974,988 | 377,682 | 1,054 B | 552 B |
| MPF-16   | 291,503 | 130,955,229 | 449,560 | 1,074 B | 572 B |
| MPF-16o  | 291,478 | 135,702,654 | 469,192 | 1,074 B | 572 B |

### Delete element

| Variant  | Fee     | CPU         | Memory  | Tx Size | Proof |
|----------|---------|-------------|---------|---------|-------|
| MPF-16b  | 291,390 | 112,341,601 | 392,919 | 1,112 B | 572 B |
| MPF-16   | 295,488 | 136,075,224 | 467,533 | 1,132 B | 593 B |
| MPF-16o  | 295,461 | 140,780,888 | 487,180 | 1,132 B | 593 B |

### Append element (oracle-managed)

| Variant  | Fee     | CPU         | Memory  | Tx Size | Proof |
|----------|---------|-------------|---------|---------|-------|
| IMT-D15  | 258,308 |  94,035,095 | 321,337 |   991 B | 498 B |

## Architecture

### IMT (Incremental Merkle Tree)

Append-only fixed-depth D binary Merkle tree. Leaves are appended sequentially
(tree is 1-indexed: `tree(2^D + i) = leaf i`). State is `(root, size)`.

Operations:
- **verifyMembership**: user proves membership via D sibling hashes (D+1 blake2b)
- **append**: oracle adds a new key at position `size` (2D+2 blake2b, single pass)

Tradeoff: cheapest per-operation cost, but append-only — elements cannot be
deleted. Suitable for oracle-managed sets where the oracle controls membership.
D=15 supports up to 32K elements; D=17 supports up to 131K.

### MPF (Merkle Patricia Forestry)

Adaptive radix-16 trie with path compression. Proof steps follow the trie
structure: Branch (dense, 16 children), Fork (sparse, 2 children), or Leaf.
Proof size scales with trie depth, O(log₁₆ N) with path compression.

Operations:
- **has**: verify membership
- **insert/delete**: modify the set, returning new root

Tradeoff: higher per-operation cost than IMT, but supports true insert/delete
without a trusted oracle. Any user can provide a proof to mutate the set.

Variants:
- **MPF-16b**: Binary (flat `ByteString`) proof encoding — fastest
- **MPF-16**: `List[ProofStep]` encoding
- **MPF-16o**: Original Aiken-compatible encoding

## Running

```bash
# MPF benchmarks
sbtn 'scalusExamplesJVM/testOnly *SetBenchEmulatorTest -- -n scalus.testing.Benchmark'
# IMT benchmarks
sbtn 'scalusExamplesJVM/testOnly *ImtBenchEmulatorTest -- -n scalus.testing.Benchmark'
```
