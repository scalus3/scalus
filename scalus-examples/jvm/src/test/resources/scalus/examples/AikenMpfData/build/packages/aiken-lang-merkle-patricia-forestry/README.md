<div align="center">

  <h1 align="center">Merkle Patricia Forestry</h1>
  <img alt="Merkle Patricia Forestry" src=".github/logo.png" height="250">
  <p align="center" style="border-bottom: none">A set of (on-chain & off-chain) libraries for working with <strong>Merkle Patricia Tries</strong> on Cardano.</p>

  <hr/>

[![Licence](https://img.shields.io/github/license/aiken-lang/merkle-patricia-forestry?style=for-the-badge)](https://github.com/aiken-lang/merkle-patricia-forestry/blob/main/LICENSE)
[![Continuous Integration](https://img.shields.io/github/actions/workflow/status/aiken-lang/merkle-patricia-forestry/continuous-integration.yml?style=for-the-badge&label=continuous%20integration)](https://github.com/aiken-lang/merkle-patricia-forestry/actions/workflows/continuous-integration.yml)
[![NPM](https://img.shields.io/npm/v/%40aiken-lang%2Fmerkle-patricia-forestry?style=for-the-badge)](https://www.npmjs.com/package/@aiken-lang/merkle-patricia-forestry)

  <hr/>
</div>

## Overview

A Merkle Patricia Trie is a persistent & authenticated data structure to map between arbitrary keys and values. It's like a hashmap on steroids, which isn't tamperable. The items are represented in a space-optimized trie (a.k.a prefix tree) of radix 16. The hash digest of their keys gives the path to values in the trie. For more details, read [the wiki](https://github.com/aiken-lang/merkle-patricia-forestry/wiki/Technical-analysis).

The use cases are numerous, such as maintaining large on-chain registries (e.g. domains) or providing unreasonably large oracled datasets of intrinsic data (e.g. a map of delegators/delegatees) or extrinsic data (e.g. GitHub data pertaining to an ecosystem of projects). It's also perfectly suited for long-running datasets that grow at a _slow_ rate (e.g. a PoW blockchain).

### Features

Using only a root hash digest (32 bytes) and a succinct proof (<1KB), Merkle Patricia Tries provides rapid:

- [x] membership (i.e. inclusion of key/value pairs)
- [x] non-membership (i.e. exclusion of keys)
- [x] insertion
- [x] deletion
- [x] update (i.e. deletation followed by insertion)

...of any key/value item in a large (billions) store.

## Getting Started

### Off-chain (JavaScript / Node.js)

```bash
yarn add @aiken-lang/merkle-patricia-forestry
```

See [off-chain](./off-chain#readme) for usage.

### On-chain (Aiken)

```bash
aiken add aiken-lang/merkle-patricia-forestry --version 2.1.0
```

See [on-chain](./on-chain#readme) for usage.

## Performances

This library implements a few optimizations. We borrow ideas from the [Ethereum's Modified Merkle Patricia Trie (MPT)](https://ethereum.org/en/developers/docs/data-structures-and-encoding/patricia-merkle-trie/) and also introduce a novel approach for organizing nodes as tiny [Sparse Merkle Trees](https://eprint.iacr.org/2016/683.pdf) that result in much smaller proof sizes, and gives the name to the structure: Merkle Patricia Forestry. This optimization and overall approach are covered in more detail [in the wiki](https://github.com/aiken-lang/merkle-patricia-forestry/wiki/Technical-analysis#forestry).

While this optimization sacrifices some memory and CPU execution units for smaller proof sizes, the library ultimately achieves a good trade-off. The table below summarizes the proof size, memory units, and CPU units for various sizes of tries and operations. On top of that, including the MPF library as a dependency adds ~2.5KB of generated UPLC.

size | proof size (bytes) | insert or delete | update | membership | non-membership |
---:      | -------------:         | ------------:        | ------------: | ------------:   | ------------: |
10² | ~200 | <sub>mem: 188.5K</sub><br/><sup>cpu: 53.9M</sup> | <sub>mem: 222.5K</sub><br/><sup>cpu: 64.5M</sup> | <sub>mem: 111.3K</sub><br/><sup>cpu: 32.3M</sup> | <sub>mem: 77.3K</sub><br/><sup>cpu: 21.6M</sup> |
10³ | ~340 | <sub>mem: 252.5K</sub><br/><sup>cpu: 72M</sup> | <sub>mem: 284.5K</sub><br/><sup>cpu: 82M</sup> | <sub>mem: 142.3K</sub><br/><sup>cpu: 41M</sup> | <sub>mem: 110.3K</sub><br/><sup>cpu: 31M</sup> |
10⁴ | ~480 | <sub>mem: 316.5K</sub><br/><sup>cpu: 90.1M</sup> | <sub>mem: 346.5K</sub><br/><sup>cpu: 99.5M</sup> | <sub>mem: 173.3K</sub><br/><sup>cpu: 49.8M</sup> | <sub>mem: 143.3K</sub><br/><sup>cpu: 40.3M</sup> |
10⁵ | ~620 | <sub>mem: 380.5K</sub><br/><sup>cpu: 108.2M</sup> | <sub>mem: 408.5K</sub><br/><sup>cpu: 117M</sup> | <sub>mem: 204.3K</sub><br/><sup>cpu: 58.5M</sup> | <sub>mem: 176.3K</sub><br/><sup>cpu: 49.7M</sup> |
10⁶ | ~760 | <sub>mem: 444.5K</sub><br/><sup>cpu: 126.3M</sup> | <sub>mem: 470.5K</sub><br/><sup>cpu: 134.5M</sup> | <sub>mem: 235.3K</sub><br/><sup>cpu: 67.3M</sup> | <sub>mem: 209.3K</sub><br/><sup>cpu: 59M</sup> |
10⁷ | ~900 | <sub>mem: 508.5K</sub><br/><sup>cpu: 144.4M</sup> | <sub>mem: 532.5K</sub><br/><sup>cpu: 152M</sup> | <sub>mem: 266.3K</sub><br/><sup>cpu: 76M</sup> | <sub>mem: 242.3K</sub><br/><sup>cpu: 68.4M</sup> |
10⁸ | ~1040 | <sub>mem: 572.5K</sub><br/><sup>cpu: 162.5M</sup> | <sub>mem: 594.5K</sub><br/><sup>cpu: 169.5M</sup> | <sub>mem: 297.3K</sub><br/><sup>cpu: 84.8M</sup> | <sub>mem: 275.3K</sub><br/><sup>cpu: 77.7M</sup> |
10⁹ | ~1180 | <sub>mem: 636.5K</sub><br/><sup>cpu: 180.6M</sup> | <sub>mem: 656.5K</sub><br/><sup>cpu: 187M</sup> | <sub>mem: 328.3K</sub><br/><sup>cpu: 93.5M</sup> | <sub>mem: 308.3K</sub><br/><sup>cpu: 87.1M</sup> |

 > [!NOTE]
 >
 > On current mainnet, 140K mem units and 100M cpu units corresponds respectively to 1% of the maximum transaction mem and cpu budgets.
