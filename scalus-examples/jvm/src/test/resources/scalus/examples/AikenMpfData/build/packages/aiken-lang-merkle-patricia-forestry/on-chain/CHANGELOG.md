# Changelog

## v2.1.0 - 2025-07-12

### Added

- New function `miss` to verify non-membership for keys using proofs of exclusions.

### Changed

- Fixed proof verification for terminal forks with non-empty prefixes.

### Removed

N/A

## v2.0.1 - 2025-06-25

### Added

N/A

### Changed

- Fixed proof verification on-chain for leaf fork with non-zero common prefix.

### Removed

N/A

## v2.0.0 - 2024-09-06

### Added

N/A

### Changed

- Requires `aiken >= 1.1.0`.

- Requires `aiken-lang/stdlib >= 2.0.0` && `aiken-lang/stdlib < 3.0.0`.

- Creating an empty trie is no longer a function `empty()` but a constant `empty`.

### Removed

N/A

## v1.1.0 - 2024-07-23

### Added

- Two new functions to the on-chain API:
  - [`root(MerklePatriciaForestry) -> ByteArray`](https://aiken-lang.github.io/merkle-patricia-forestry/aiken/merkle_patricia_forestry.html#root)
  - [`update(self, key, proof, old_value, new_value) -> MerklePatriciaForestry`](https://aiken-lang.github.io/merkle-patricia-forestry/aiken/merkle_patricia_forestry.html#update)

### Changed

N/A

### Removed

N/A

## v1.0.1 - 2024-06-02

### Added

- Initial implementation of _Merkle-Patricia Forestry_, with an on-chain (Aiken) and off-chain (Node.js) backend:
  - **Constructing**
    - [`empty() -> MerklePatriciaForestry`](https://aiken-lang.github.io/merkle-patricia-forestry/aiken/merkle_patricia_forestry.html#empty)
    - [`from_root(root) -> MerklePatriciaForestry`](https://aiken-lang.github.io/merkle-patricia-forestry/aiken/merkle_patricia_forestry.html#from_root)
  - **Modifying**
    - [`insert(self, key, value, proof) -> MerklePatriciaForestry`](https://aiken-lang.github.io/merkle-patricia-forestry/aiken/merkle_patricia_forestry.html#insert)
    - [`delete(self, key, value, proof) -> MerklePatriciaForestry`](https://aiken-lang.github.io/merkle-patricia-forestry/aiken/merkle_patricia_forestry.html#delete)
  - **Inspecting**
    - [`has(self, key, value, proof) -> Bool`](https://aiken-lang.github.io/merkle-patricia-forestry/aiken/merkle_patricia_forestry.html#has)

### Changed

N/A

### Removed

N/A
