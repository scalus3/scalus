# Set Membership Benchmark

Benchmark of authenticated set data structures for Cardano smart contracts.
Measures per-operation transaction cost (fee, CPU, memory, proof size) for
withdraw (delete) and deposit (insert) operations.

## Winner: MPF-16b

**Merkle Patricia Forestry, radix-16, binary proof encoding.**

| Op       | Fee     | CPU         | Memory  | Tx Size | Proof |
|----------|---------|-------------|---------|---------|-------|
| Withdraw | 298,408 | 138,685,033 | 500,607 | 1,112 B | 572 B |
| Deposit  | 294,319 | 133,210,115 | 480,891 | 1,054 B | 552 B |

N=32K elements. Measured via emulator with reference scripts.

## All Variants (N=32K)

### Withdraw

| Variant | Fee     | CPU         | Memory  | Tx Size | Proof | Build  |
|---------|---------|-------------|---------|---------|-------|--------|
| MPF-16b | 298,408 | 138,685,033 | 500,607 | 1,112 B | 572 B | 1.6 s  |
| MPF-16  | 299,371 | 139,988,080 | 489,897 | 1,132 B | 593 B | 1.9 s  |
| MPF-16o | 299,725 | 145,367,344 | 513,754 | 1,132 B | 593 B | 2.1 s  |
| MPF-64b | 309,229 | 140,495,512 | 508,371 | 1,162 B | 623 B | 4.6 s  |
| MPF-64  | 310,040 | 141,325,773 | 500,202 | 1,177 B | 637 B | 5.1 s  |
| IMT-D15 | 321,609 | 206,565,785 | 726,368 | 1,597 B | 1065 B | 2.4 s |

### Deposit

| Variant | Fee     | CPU         | Memory  | Tx Size | Proof | Build  |
|---------|---------|-------------|---------|---------|-------|--------|
| MPF-16b | 294,319 | 133,210,115 | 480,891 | 1,054 B | 552 B | 1.6 s  |
| MPF-16  | 295,274 | 134,525,277 | 470,412 | 1,074 B | 572 B | 1.6 s  |
| MPF-16o | 295,627 | 139,938,302 | 494,204 | 1,074 B | 572 B | 1.8 s  |
| MPF-64b | 305,955 | 136,830,623 | 495,333 | 1,111 B | 609 B | 5.2 s  |
| MPF-64  | 306,771 | 137,708,421 | 487,181 | 1,125 B | 624 B | 4.5 s  |
| IMT-D15 | 318,600 | 201,541,379 | 712,144 | 1,555 B | 1062 B | 2.2 s |

## Why IMT Loses

The Indexed Merkle Tree (fixed-depth binary Merkle tree with sorted linked-list
leaves) trades a much smaller script (~150 UPLC nodes vs ~800 for MPF-16b) for
significantly more blake2b calls per operation.

- **CPU: +49%.** IMT needs 4×D + 4 = 64 blake2b calls (D=15) vs ~15 for MPF-16b.
  The radix-invariance theorem means MPF's total hashing cost stays roughly
  constant regardless of radix — IMT's binary radix gets no such benefit because
  it has fixed depth rather than adaptive path compression.
- **Proof size: +86%.** IMT proofs are 67 + 64×D = 1027 bytes (insert) vs ~550
  for MPF-16b. Each operation requires two full root-to-leaf paths (predecessor
  update + target update), each carrying D=15 sibling hashes.
- **Fee: +8%.** Driven by both higher CPU cost and larger proof/tx size.
- **Script size advantage doesn't help.** Reference scripts amortize script size
  across transactions; the per-tx savings from a smaller script are negligible
  compared to the per-tx cost of more hashing and larger proofs.

## Architecture

### MPF (Merkle Patricia Forestry)

Adaptive radix trie with path compression. Proof steps follow the trie structure:
each step is Branch (dense, 16 or 64 children), Fork (sparse, 2 children), or
Leaf. Proof size scales with trie depth, which is O(log_r N) with path compression
making it even shorter in practice.

Variants:
- **MPF-16**: Radix-16, `List[ProofStep]` encoding
- **MPF-16o**: Radix-16, original Aiken-compatible encoding
- **MPF-64**: Radix-64, `List[ProofStep]` encoding
- **MPF-16b**: Radix-16, binary (flat `ByteString`) proof encoding
- **MPF-64b**: Radix-64, binary proof encoding

### IMT (Indexed Merkle Tree)

Fixed-depth D binary Merkle tree. Leaves are `(key, next_key)` pairs forming a
sorted linked list. Insert/delete require two Merkle root updates (predecessor
leaf + target leaf). Proof is two sets of D sibling hashes.

## Running

```bash
sbtn 'scalusExamplesJVM/testOnly *SetBenchEmulatorTest -- -n scalus.testing.Benchmark'
```
