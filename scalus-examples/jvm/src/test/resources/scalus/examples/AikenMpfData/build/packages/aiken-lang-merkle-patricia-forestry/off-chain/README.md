# Merkle Patricia Forestry

This package provides a Node.js backend for working with an authenticated key/value store, a.k.a [Merkle Patricia Forestry](../README.md). It comes with an out-of-the-box on-disk storage solution based on [level.js](https://leveljs.org/), but it also has an in-memory store available for debugging.

The library provides ways of constructing the trie incrementally (by inserting items into it) while keeping a low memory footprint. By default, only the topmost node is kept in memory, and children are only references to nodes stored on disk.

This package also allows producing succinct proofs for items, thus allowing the proof of membership, insertion, and deletion of items in the trie only from root hashes (and proofs!).

## Installation

```
yarn add @aiken-lang/merkle-patricia-forestry
```

## Overview

### Constructing

#### `new Trie(store?: Store): Trie`

You can construct a new trie using `new Trie`, providing an (optional) store as a parameter. Using an on-disk store for non-trivial tries is strongly recommended, so only omit this argument for dummy scenarios.

```js
import { Store, Trie } from '@aiken-lang/merkle-patricia-forestry';

// Construct a new trie with on-disk storage under the file path 'db'.
const trie = new Trie(new Store('db'));
```

#### `Trie.fromList(items: Array<Item>, store?: Store): Promise<Trie>`

> With `Item = { key: string|Buffer, value: string|Buffer }`

Using `Trie. fromList`, you can rapidly bootstrap a new trie from many items. This method is convenient for small tries and rapid prototyping. The store is optional and defaults to in-memory when omitted.

```js
const trie = await Trie.fromList([
  { key: 'apple', value: '🍎'},
  { key: 'blueberry', value: '🫐'},
  { key: 'cherries', value: '🍒'},
  { key: 'grapes', value: '🍇'},
  { key: 'tangerine', value: '🍊'},
  { key: 'tomato', value: '🍅'},
]);
```

#### `trie.insert(key: string|Buffer, value: string|Buffer) -> Promise<Trie>`

While `Trie.fromList` provides a convenient way to create small tries, mainly for testing purposes, the primary way of constructing tries is by repeatedly calling `.insert` for each item to insert. It returns the same reference to the trie as a Promise for convenience.

Note that you can only insert one item at a time, so make sure to `await` promises before attempting a new insert. Not doing so will trigger an exception. An exception is also raised when trying to insert an item at an already-known key. Delete first!

```js
// Insert items one-by-one
await trie.insert('apple', '🍎');
await trie.insert('blueberry', '🫐');
await trie.insert('cherries', '🍒');
await trie.insert('grapes', '🍇');
await trie.insert('tangerine', '🍊');
await trie.insert('tomato', '🍅');

// Insert many items
const items = [
  { key: 'apple', value: '🍎'},
  { key: 'blueberry', value: '🫐'},
  { key: 'cherries', value: '🍒'},
  { key: 'grapes', value: '🍇'},
  { key: 'tangerine', value: '🍊'},
  { key: 'tomato', value: '🍅'},
];

await items.reduce(async (trie, { key, value }) => {
  return (await trie).insert(key, value);
}, trie);
```

> [!TIP]
>
> Both the _key_ and _value_ must be either `string`s or byte `Buffer`. When a `string` is provided, it is treated as a UTF-8 byte `Buffer`.

#### `trie.delete(key: string|Buffer) -> Promise<Trie>`

Similarly, the reverse operation `delete` is available to remove elements from the trie. It fails with an exception if the given key is not in the trie. For convenience, it returns the same reference to the trie as a Promise.

```js
// Remove 'apple'
await trie.delete('apple');

// Throws an exception, apple is no longer in the trie.
await trie.delete('apple');
```

> [!TIP]
>
> The _key_ must be either `string`s or byte `Buffer`. When a `string` is provided, it is treated as a UTF-8 byte `Buffer`.

#### `Trie.load(store: Store): Promise<Trie>`

Using `Trie. load`, you can load any previously constructed trie back from disk. In this case, the `store` argument must be provided.

```js
import { Store, Trie } from '@aiken-lang/merkle-patricia-forestry';

// Construct a new trie with on-disk storage under the file path 'db'.
const trie = await Trie.load(new Store('db'));
```

### Inspecting

You can inspect any trie using the [`util.inspect`](https://nodejs.org/api/util.html#utilinspectobject-options) from Node.js or simply by calling `console.log`:

```js
console.log(trie);
// ╔═══════════════════════════════════════════════════════════════════╗
// ║ #ee54d685370064b61cd8921f8476e54819990a67f6ebca402d1280ba1b03c75f ║
// ╚═══════════════════════════════════════════════════════════════════╝
//  ┌─ 0 #33af5a3bbf8f
//  ├─ 1 #a38f7e65ebf6
//  ├─ 3 #ac9d183ca637
//  └─ 9 #75eba4e4dae1
```

The output is a pretty-printed visualisation of the tree, with the entire root hash first in a box, followed by the rest of the trie. For each sub-trie, a summary of their hashes is shown using a pound sign (e.g. `#eceea58af726`) as well as the branch path and prefix, if any, that leads to that point.

For leaves, the remaining path (a.k.a suffix) is shown but truncated for brevity. The output also includes the key/value pertaining to that leaf.

Finally, the function is synchronous and thus doesn't have access to children beyond
their hashes. You can, however, fetch more children using:

#### `trie.fetchChildren(depth?: number = 0): Promise<()>`

The `depth` parameter is optional and must be a positive integer when provided. It corresponds to the number of sub-levels to fetch.


```js
await trie.fetchChildren(2);

console.log(trie);
// ╔═══════════════════════════════════════════════════════════════════╗
// ║ #ee54d685370064b61cd8921f8476e54819990a67f6ebca402d1280ba1b03c75f ║
// ╚═══════════════════════════════════════════════════════════════════╝
//  ┌─ 09ad7..[55 digits]..19d9 #33af5a3bbf8f { apple → 🍎 }
//  ├─ 1 #a38f7e65ebf6
//  │  ├─ b021f..[54 digits]..2290 #e5f9beffc856 { tomato → 🍅 }
//  │  └─ e7b4b..[54 digits]..0675 #b5e92076b81f { cherries → 🍒 }
//  ├─ 39cd4..[55 digits]..9e65 #ac9d183ca637 { blueberry → 🫐 }
//  └─ 9 #75eba4e4dae1
//     ├─ 702e3..[54 digits]..3a28 #c8b244fad188 { grapes → 🍇 }
//     └─ b19ae..[54 digits]..962c #830b96edc35b { tangerine → 🍊 }
```

> [!TIP]
>
> To retrieve the entire trie, use `Number.MAX_SAFE_INTEGER`. But be careful, for large tries may not fit in memory!

#### `trie.save(): Promise<(Trie)>`

To replace children with references again, use `trie.save()`.

```js
await trie.save();

console.log(trie);
// ╔═══════════════════════════════════════════════════════════════════╗
// ║ #ee54d685370064b61cd8921f8476e54819990a67f6ebca402d1280ba1b03c75f ║
// ╚═══════════════════════════════════════════════════════════════════╝
//  ┌─ 0 #33af5a3bbf8f
//  ├─ 1 #a38f7e65ebf6
//  ├─ 3 #ac9d183ca637
//  └─ 9 #75eba4e4dae1
```

> [!NOTE]
>
> There's, in principle, no need to _manually save_ the trie otherwise. Operations on the trie such as _insert_ or _remove_ automatically modify the store.

### Accessing

#### `trie.get(key: string|Buffer): Promise<Buffer|undefined>`

You can efficiently fetch any value from the trie from its key using the `trie.get` method.

```js
await trie.get('cherries');
// 🍒

await trie.get('cherrie');
// undefined
```

#### `trie.childAt(path: string): Promise<Trie|undefined>`

You can retrieve any child from the trie at a given _path_. A _path_ is a sequence of hexadecimal digits (a.k.a nibbles) given by the hash digest (blake2b-256) of the key, encoded in base16.

```js
import blake2b from 'blake2b';

// Manually constructed, from a plain string prefix.
const cherries = await trie.childAt('1e');
// 7b4b..[54 digits]..0675 #b5e92076b81f { cherries → 🍒 }

const none = await trie.childAt('ffff');
// undefined

// Using the hash digest
const apple = await trie.childAt(
  blake2b(32).update(Buffer.from('apple')).digest('hex')
);
// 9ad7..[55 digits]..19d9 #33af5a3bbf8f { apple → 🍎 }
```

### Proving

#### `trie.prove(key: string|Buffer, allowMissing?: bool): Promise<Proof>`

Let's get to the exciting part! The whole point of building a Merkle Patricia Forestry is to provide succinct proofs for items. A proof is portable and bound to both:

1. A specific item
2. A specific trie

Proofs are only valid for a precise trie root hash and state. So inserting (resp. removing) any item into (resp. from) the trie will invalidate any previously generated proof.

```js
const proofTangerine = await trie.prove('tangerine');
// Proof {}
```

Note that it is possible to create partial proofs (or imply, proof of exclusion) for elements that are not in the trie. This is useful to prove non-membership: verifying the proof in exclusion would yield the current trie root.

```js
// 'melon' isn't in the Trie, but allowMissing is set `true`
const proofMelon = await trie.prove('melon', true);
// Proof {}
```

#### `proof.verify(includingItem?: bool = true): Buffer`

A proof can be _verified_ by calling `.verify` on it. The verification is fully synchronous and yields a hash as a byte `Buffer`. If that hash matches the trie root hash, the proof is valid.

```js
proofTangerine.verify().equals(trie.hash);
// true
```

Proofs can be computed in two manners. By default, they _include_ the item in the proof, thus checking for _inclusion_ in the trie. However, by setting `includingItem` to `false`, the proof will check for _exclusion_ and yield the root hash of the trie _without the item_.

```js
const previousHash = trie.hash;

await trie.insert('banana', '🍌');

const proofBanana = await trie.prove('banana');

proofBanana.verify(true).equals(trie.hash);
// true

proofBanana.verify(false).equals(previousHash);
// true
```

> [!TIP]
>
> Hence, you insert an element in the trie from just a proof and a current root hash (without the item), yielding a new root hash that includes just the item.
>
> ```js
> function insert(root, proof) {
>   assert(proof.verify(false).equals(root));
>   return proof.verify(true);
> }
> ```

#### `proof.setValue(value)`

Once a proof has been generated for a given key/value element; it is possible to quickly change the value associated with that proof using `.setValue` so as the key -- and the rest of the trie; stay the same.

#### `proof.toJSON(): object`

Proofs are opaque but can be serialised into various formats, such as JSON. The proof format is explained in greater detail in [the wiki](https://github.com/aiken-lang/merkle-patricia-forestry/wiki/Proof-format).

```js
proofTangerine.toJSON();
// [
//   {
//     type: 'branch',
//     skip: 0,
//     neighbors: '17a27bc4ce61078d26372800d331d6b8c4b00255080be66977c78b1554aabf8985c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f17
// 22acc3b10eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000
// '
//   },
//   {
//     type: 'leaf',
//     skip: 0,
//     neighbor: {
//       key: '9702e39845bfd6e0d0a5b6cb4a3a1c25262528c11bcff857867a50a0670e3a28',
//       value: 'b5898c51c32083e91b8c18c735d0ba74e08f964a20b1639c189d1e8704b78a09'
//     }
//   }
// ]
```

#### `proof.fromJSON(key, value, steps): Proof`

Re-construct a JavaScript Proof object from a serialized JSON value. This requires providing the original key/value pair.

```js
const proofTangerine = Proof.fronJSON(
  'tangerine',
  '🍊',
  [
    {
      type: 'branch',
      skip: 0,
      neighbors: '17a27bc4ce61078d26372800d331d6b8c4b00255080be66977c78b1554aabf8985c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f17
  22acc3b10eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000
  '
    },
    {
      type: 'leaf',
      skip: 0,
      neighbor: {
        key: '9702e39845bfd6e0d0a5b6cb4a3a1c25262528c11bcff857867a50a0670e3a28',
        value: 'b5898c51c32083e91b8c18c735d0ba74e08f964a20b1639c189d1e8704b78a09'
      }
    }
  ]
);
```


#### `proof.toCBOR(): Buffer`

JSON is cool, but proofs are ultimately meant to be passed on-chain as redeemer or datum. Thus, we provide a method `.toCBOR` to serialise a proof into a format compatible with the on-chain expectations.

```js
proofTangerine.toCBOR().toString('hex');
// 9fd8799f005f58404be28f4839135e1f8f5372a90b54bb7bfaf997a5d13711bb4d7d93f9d4e04fbefa63eb4576001d8658219f928172eccb5448b4d7d62cd6d95228e13ebcbd53505840c1e96bcc431893eef34e03989814375d439faa592edf75c9e5dc10b3c30766700000000000000000000000000000000000000000000000000000000000000000ffffff
```

#### `proof.toAiken(): string`

For convenience, you can also generate Aiken code that works with the on-chain part of the library using `.toAiken`.

```js
proofTangerine.toAiken();
```

```aiken
[
  Branch {
    skip: 0,
    neighbors: #"17a27bc4ce61078d26372800d331d6b8c4b00255080be66977c78b1554aabf8985c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b10eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000",
  },
  Leaf {
    skip: 0,
    key: #"9702e39845bfd6e0d0a5b6cb4a3a1c25262528c11bcff857867a50a0670e3a28",
    value: #"b5898c51c32083e91b8c18c735d0ba74e08f964a20b1639c189d1e8704b78a09",
  },
]
```

#### `proof.toUPLC(): String`

Finally, one can also convert proofs in a textual UPLC format which as currently expected by some tools such as `aiken uplc eval`.

```js
proofTangerine.toAiken();
```

```uplc
(con data
  (List
    [ Constr 0 [I 0, B #"17a27bc4ce61078d26372800d331d6b8c4b00255080be66977c78b1554aabf8985c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b10eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000"]
    , Constr 2 [I 0, B #"9702e39845bfd6e0d0a5b6cb4a3a1c25262528c11bcff857867a50a0670e3a28", B #"b5898c51c32083e91b8c18c735d0ba74e08f964a20b1639c189d1e8704b78a09"]
    ]
  )
)
```
