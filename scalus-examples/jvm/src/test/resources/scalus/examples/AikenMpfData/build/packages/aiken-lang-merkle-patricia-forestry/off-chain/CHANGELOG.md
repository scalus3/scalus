# Changelog

## v1.3.1 - 2025-07-12

### Added

N/A

### Changed

- Fixed deployment of npm package.

### Removed

N/A

## v1.3.0 - 2025-07-12

### Added

- New utility functions for proofs:
  - [`Proof.fromJSON(key, value, steps): Proof`](https://github.com/aiken-lang/merkle-patricia-forestry/tree/main/off-chain#prooffromjson-key-value-steps-proof) to recover a proof from an already serialized JSON value.
  - [`Proof.setValue(value)`](https://github.com/aiken-lang/merkle-patricia-forestry/tree/main/off-chain#proofsetvaluevalue) to quickly change the value associated with a proof (without having to re-calculate a whole proof).
  - [`Proof.toUPLC()`](https://github.com/aiken-lang/merkle-patricia-forestry/tree/main/off-chain#prooftouplc) provides a proof in a textual UPLC format; handy to use with `aiken uplc eval`.

### Changed

- Allow [`childAt`](https://github.com/aiken-lang/merkle-patricia-forestry/tree/main/off-chain#triechildatpath-string-promisetrieundefined) to return with intermediate sub-tree when passing an incomplete path.

- Adjust `trie.prove` to now accept an optional (default=`false`) flag: `allowMissing`. When set, it becomes possible to build partial proofs for elements that
are not in the trie. The proof can in particular be verified in exclusion to prove that the given element isn't in the tree (i.e. non-membership).

- Fixed proof verification for terminal forks with non-empty prefixes.

### Removed

N/A

## v1.2.0 - 2024-10-17

### Added

- [`get(key) -> Promise<Buffer|undefined>`](https://github.com/aiken-lang/merkle-patricia-forestry/tree/main/off-chain#triegetkey-stringbuffer-promisebufferundefined)

### Changed

- The `.hash` property of empty tries now returns `null` in the off-chain library, instead of a bytes buffer filled with zeroes. This is to avoid a confusion where one would be encouraged to construct an empty trie from such a root.

### Removed

- N/A

## v1.0.1 - 2024-06-02

### Added

- Initial implementation of _Merkle-Patricia Forestry_, with an on-chain (Aiken) and off-chain (Node.js) backend on top of [level.js](https://leveljs.org/) with an option to run in-memory:
  - **Constructing**
    - [`new Trie(store?: Store): Trie`](https://github.com/aiken-lang/merkle-patricia-forestry/tree/main/off-chain#new-triestore-store-trie)
    - [`Trie.fromList(items: Array<Item>, store?: Store): Promise<Trie>`](https://github.com/aiken-lang/merkle-patricia-forestry/tree/main/off-chain#triefromlistitems-arrayitem-store-store-promisetrie)
    - [`Trie.load(store: Store): Promise<Trie>`](https://github.com/aiken-lang/merkle-patricia-forestry/tree/main/off-chain#trieloadstore-store-promisetrie)
    - [`trie.save(): Promise<(Trie)>`](https://github.com/aiken-lang/merkle-patricia-forestry/tree/main/off-chain#triesave-promisetrie)
  - **Modifying**
    - [`trie.insert(key: string|Buffer, value: string|Buffer) -> Promise<Trie>`](https://github.com/aiken-lang/merkle-patricia-forestry/tree/main/off-chain#trieinsertkey-stringbuffer-value-stringbuffer---promisetrie)
    - [`trie.delete(key: string|Buffer) -> Promise<Trie>`](https://github.com/aiken-lang/merkle-patricia-forestry/tree/main/off-chain#triedeletekey-stringbuffer---promisetrie)
  - **Inspecting**
    - [`trie[inspect.custom]`](https://github.com/aiken-lang/merkle-patricia-forestry/tree/main/off-chain#inspecting)
    - [`trie.fetchChildren(depth?: number = 0): Promise<()>`](https://github.com/aiken-lang/merkle-patricia-forestry/tree/main/off-chain#triefetchchildrendepth-number--0-promise)
    - [`trie.childAt(path: string): Promise<Trie|undefined>`](https://github.com/aiken-lang/merkle-patricia-forestry/tree/main/off-chain#triechildatpath-string-promisetrieundefined)
  - **Proving**
    - [`trie.prove(key: string|Buffer): Promise<Proof>`](https://github.com/aiken-lang/merkle-patricia-forestry/tree/main/off-chain#trieprovekey-stringbuffer-promiseproof)
    - [`proof.verify(includingItem?: bool = true): Buffer`](https://github.com/aiken-lang/merkle-patricia-forestry/tree/main/off-chain#proofverifyincludingitem-bool--true-buffer)
    - [`proof.toJSON(): object`](https://github.com/aiken-lang/merkle-patricia-forestry/tree/main/off-chain#prooftojson-object)
    - [`proof.toCBOR(): Buffer`](https://github.com/aiken-lang/merkle-patricia-forestry/tree/main/off-chain#prooftocbor-buffer)
    - [`proof.toAiken(): string`](https://github.com/aiken-lang/merkle-patricia-forestry/tree/main/off-chain#prooftoaiken-string)

### Changed

N/A

### Removed

N/A
