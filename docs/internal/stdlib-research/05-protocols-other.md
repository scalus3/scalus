# What real (non-DEX) Cardano protocols re-implement by hand

Research date: 2026-08-26. Worktree: `.claude/worktrees/stdlib-api-research`, branch `master`.

Purpose: find the helper functions and validation idioms that production, open-source, non-DEX
Cardano protocols keep writing themselves, so the new Scalus "smart contract standard library" can
absorb them. Every claim carries `repo` + absolute `file:line` and quoted source.

Companion documents in this directory:

| File | Content |
| --- | --- |
| `02-scalus-existing-api.md` | What Scalus already ships on-chain (the baseline for gap analysis) |
| `03-aiken-stdlib.md` | Library inventory: `aiken-lang/stdlib`, `aiken-design-patterns`, `vodka` |
| `07-efficiency-constraints.md` | What compiles to cheap UPLC in Scalus today |
| `raw-l2-rollup.md` | Raw extraction: Midgard, Hydrozoa, AdaStream, Binocular |
| `raw-lending-cdp.md` | Raw extraction: Lenfi, Indigo, Butane, Liqwid Plutarch extra, Fortuna |
| `raw-nft-marketplace.md` | Raw extraction: JPG Store v3, CIP-113, Bodega, Cardano-Swaps |
| `05-protocols-other.md` | **This file** — synthesis, cross-repo matrix, ranked API proposals |

---

## 0. Corpus

### 0.1 Analyzed (source read)

| # | Protocol / library | Domain | Lang | Location |
| --- | --- | --- | --- | --- |
| 1 | **Anastasia Labs `aiken-design-patterns`** v1.8.0 | pattern library | Aiken | `scratchpad/repos/aiken-design-patterns` |
| 2 | **SundaeSwap / Intersect `treasury-contracts`** | treasury / governance | Aiken | `/Users/nau/projects/lantr/treasury-contracts` |
| 3 | **PRAGMA `amaru-treasury`** | treasury / governance | Aiken | `/Users/nau/projects/lantr/amaru-treasury` |
| 4 | **SundaeSwap `aicone`** (`sundae/multisig`) | authorization library | Aiken | `scratchpad/repos/aicone` |
| 5 | **Hydra** (`hydra-plutus`) | L2 head | PlutusTx | `scratchpad/repos/hydra` |
| 6 | **Charli3** `oracle-integration-aiken` | oracle | Aiken | `scratchpad/repos/charli3-aiken` |
| 7 | **sidan-lab `vodka`** (cocktail/cip) | utility library (MeshJS) | Aiken | `scratchpad/repos/vodka` |
| 8 | **aiken-lang `merkle-patricia-forestry`** | MPT proofs | Aiken | `scratchpad/repos/mpf` |
| 9 | **Anastasia Labs `aiken-linked-list`** | on-chain linked list | Aiken | `scratchpad/repos/aiken-linked-list` |
| 10 | **Anastasia Labs Midgard** | optimistic rollup | Aiken | `/Users/nau/projects/lantr/midgard` |
| 11 | **Hydrozoa** | L2 head | Scala/Scalus | `/Users/nau/projects/lantr/hydrozoa` |
| 12 | **AdaStream** | data streaming | Scalus | `/Users/nau/projects/lantr/adastream` |
| 13 | **Binocular** | Bitcoin oracle | Scalus | `/Users/nau/projects/lantr/binocular` |
| 14 | **Lenfi (Aada)** | lending | Aiken | `scratchpad/repos/lenfi` |
| 15 | **Indigo** | CDP / synthetics | PlutusTx | `scratchpad/repos/indigo` |
| 16 | **Butane** | stablecoin / CDP | Aiken | `scratchpad/repos/butane` |
| 17 | **Liqwid `liqwid-plutarch-extra`** | utility library | Plutarch | `scratchpad/repos/liqwid-plutarch-extra` |
| 18 | **Fortuna (TUNA)** | proof-of-work | Aiken | `scratchpad/repos/fortuna` |
| 19 | **JPG Store v3** | NFT marketplace | Aiken | `scratchpad/repos/jpgstore-v3` |
| 20 | **CIP-113 programmable tokens** (Cardano Foundation) | token standard | Aiken | `/Users/nau/projects/lantr/cip113-programmable-tokens` |
| 21 | **Bodega** | prediction market | Aiken | `/Users/nau/projects/lantr/bodega-market-smart-contracts` |
| 22 | **Cardano-Swaps** | p2p order book | Aiken/PlutusTx | `/Users/nau/projects/lantr/cardano-swaps` |

### 0.2 Not analyzed, and why

Recorded explicitly so the coverage claim is honest.

| Target | Status |
| --- | --- |
| **Liqwid** (core protocol) | On-chain protocol code is not public. Only `liqwid-plutarch-extra` (their utility library) is open, and it *is* analyzed. |
| **Djed (COTI)** | COTI open-sourced the order API, chain indexer and frontend only. The Plutus on-chain contracts are not in a public repo (`artifi-labs/open-djed` is a web app). |
| **USDM (Moneta)** | No public on-chain repo found. |
| **jamonbread** | No public contract repo found; the marketplace contract is described in blog posts only. |
| **NMKR** | Minting service; no public validator source. |
| **Summon / Clarity (DAO)** | No public on-chain repo found. |
| **Orcfax** | Only a `vesting-oracle-smart-contract-example` tutorial repo (OpShin) is public; the oracle validators are not. Charli3 covers the oracle-consumer idiom instead. |
| **Levvy, Danogo, Fluid Tokens** | Aiken-based per ecosystem listings, but no public contract repositories found. |
| **Gummiworm (SundaeSwap L2)** | `SundaeSwap-finance/gummiworm` returns 404 (private/removed). |
| **Plutarch merkle-patricia-forestry** | Anastasia's Plutarch port not found; the canonical `aiken-lang/merkle-patricia-forestry` is analyzed instead. |

---

> **Overlap note.** `03-aiken-stdlib.md` catalogues `aiken-lang/stdlib`, `aiken-design-patterns`
> and `vodka` as *libraries*. Sections 1 and 5 below re-examine the same two libraries from the
> **demand** side: they are evidence of what protocols kept re-writing until somebody packaged it.
> Sections 2, 3, 4, 6 and the raw files are net-new protocol evidence.

## 1. Pattern libraries: the idioms the ecosystem already agreed on

The single most informative artefact is `aiken-design-patterns`, because Anastasia Labs distilled it
from real client work. Its module list *is* a specification for a stdlib.

| Module | Idiom |
| --- | --- |
| `stake-validator.ak` | withdraw-0 trick / delegate spend logic to a staking script |
| `singular-utxo-indexer.ak` | one input ↔ one/many outputs, addressed by redeemer index |
| `multi-utxo-indexer.ak` | many inputs ↔ many outputs, ascending-index proof |
| `tx-level-minter.ak` | spend-forwards-to-mint |
| `merkelized-validator.ak` | off-load computation to a withdrawal script, verify via redeemer |
| `validity-range-normalization.ak` | normalize `ValidityRange` into a total, canonical datatype |
| `parameter-validation.ak` (+ `advanced`) | prove an address is an instance of a parameterized script |
| `linked-list.ak` (+ `advanced`, `nested`) | authenticated on-chain linked list |
| `utils.ak` | authentic-input helpers, single-asset extraction, input resolution |

### 1.1 Withdraw-0 forwarding (`stake_validator`)

```rust
// repos/aiken-design-patterns/lib/aiken-design-patterns/stake-validator.ak:27
pub fn validate_withdraw(
  withdraw_script_hash: ScriptHash,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  withdraw_redeemer_index: Int,
  withdraw_redeemer_validator: fn(Redeemer) -> Bool,
) -> Bool {
  get_withdraw_scripts_redeemer_at(
    withdraw_script_hash,
    redeemers,
    withdraw_redeemer_index,
  )
    |> withdraw_redeemer_validator
}

// :69 — the cheapest variant: only prove the staking script ran
pub fn validate_withdraw_minimal(
  withdraw_script_hash: ScriptHash,
  withdrawals: Pairs<Credential, Lovelace>,
  withdrawal_index: Int,
) -> Bool {
  expect Some(withdrawal_pair) = withdrawals |> list.at(withdrawal_index)
  withdrawal_pair.1st == Script(withdraw_script_hash)
}
```

The redeemer lookup underneath is itself a reusable helper:

```rust
// repos/aiken-design-patterns/lib/aiken-design-patterns/utils.ak:18
pub fn get_withdraw_scripts_redeemer_at(
  withdraw_script_hash: ScriptHash,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  redeemer_index: Int,
) -> Data {
  expect Some(rdmr_pair) = redeemers |> list.at(redeemer_index)
  expect Withdraw(Script(withdraw_script_hash)) == rdmr_pair.1st
  rdmr_pair.2nd
}
```

Rationale, from the module header: *"With a minimal spending logic (which is executed for each
UTxO), and an arbitrary withdrawal logic (which is executed only once), a much more optimized script
can be implemented."* (`stake-validator.ak:1-10`)

`vodka` ships the degenerate version of the same check:

```rust
// repos/vodka/lib/cocktail/vodka_withdrawals.ak:13
pub fn withdrawal_script_validated(
  withdrawals: Pairs<Credential, Lovelace>,
  withdrawal_script_hash: ScriptHash,
) -> Bool {
  withdrawals |> has_key(Script(withdrawal_script_hash))
}
```

and Sundae's `MultisigScript` uses it as an authorization primitive (`Script { script_hash }` leaf,
below).

### 1.2 UTxO indexer (input-index / output-index redeemer hints)

Instead of `list.find`-ing your own input, the redeemer carries the positional index and the
validator only verifies it. This is *the* standard on-chain cost optimization.

```rust
// repos/aiken-design-patterns/lib/aiken-design-patterns/singular-utxo-indexer.ak:20
pub fn one_to_one(
  input_index: Int,
  output_index: Int,
  own_ref: OutputReference,
  inputs: List<Input>,
  outputs: List<Output>,
  double_satisfaction_prevented: Bool,
  validation_logic: fn(Input, Output) -> Bool,
) -> Bool {
  expect double_satisfaction_prevented
  expect Some(Input { output_reference: in_ref, .. } as in_input) = inputs
    |> list.at(input_index)
  expect Some(out_utxo) = outputs |> list.at(output_index)
  // Indicated input must match the spending one.
  expect own_ref == in_ref
  validation_logic(in_input, out_utxo)
}
```

Note the `double_satisfaction_prevented: Bool` parameter: a *deliberate* type-level nag. The README
says:

> Neither of the singular UTxO indexer patterns provides protection against the double satisfaction
> vulnerability, as this can be done in multiple ways depending on the contract. However, they
> require a dedicated argument as a reminder for the potential requirement of implementing a
> protection against this vulnerability.
> — `repos/aiken-design-patterns/README.md:118-122`

The multi variant enforces **strictly ascending indices** so an index cannot be reused (this *is* a
double-spend/double-count guard for the index list itself):

```rust
// repos/aiken-design-patterns/lib/aiken-design-patterns/multi-utxo-indexer.ak:32-51
fn(input, remaining_indices, in0, out0, i, return) {
  let next_i = i + 1
  if input.output.address.payment_credential == script_spending_credential {
    when remaining_indices is {
      [] -> fail @"More UTxOs of the script are spent than specified"
      [Pair(in1, out1), ..rest_of_indices] ->
        if i == in1 && in1 > in0 && out1 > out0 {
          expect Some(out_utxo) = outputs |> list.at(out1)
          ...
        } else {
          fail @"Input and output indices must be in ascending orders"
        }
    }
  } else { return(remaining_indices, in0, out0, next_i) }
}
```

and `one_to_many` (`singular-utxo-indexer.ak:54`) does the same with `511` as the initial sentinel
index for a `foldr2` descending scan.

### 1.3 Spend-forwards-to-mint (`tx_level_minter`)

```rust
// repos/aiken-design-patterns/lib/aiken-design-patterns/tx-level-minter.ak:24
pub fn validate_mint(
  mint_script_hash: PolicyId,
  mint: Value,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  mint_redeemer_index: Int,
  mint_validator: fn(Redeemer, Dict<AssetName, Int>) -> Bool,
) -> Bool {
  let mint_purpose = Mint(mint_script_hash)
  expect Some(rdmr_pair) = redeemers |> list.at(mint_redeemer_index)
  expect rdmr_pair.1st == mint_purpose
  let tokens = mint |> assets.tokens(mint_script_hash)
  mint_validator(rdmr_pair.2nd, tokens)
}

// :45
pub fn validate_mint_minimal(mint_script_hash: PolicyId, mint: Value) -> Bool {
  !(mint |> assets.tokens(mint_script_hash) |> dict.is_empty)
}
```

### 1.4 Authentic input reproduced unchanged (state-token continuity)

This is the "state machine step" primitive in its strictest form — the NFT-carrying UTxO must come
back byte-for-byte identical:

```rust
// repos/aiken-design-patterns/lib/aiken-design-patterns/utils.ak:214
pub fn authentic_input_is_reproduced_unchanged(
  auth_symbol: PolicyId,
  optional_auth_name: Option<AssetName>,
  in_utxo: Output,
  out_utxo: Output,
) -> Bool {
  ...
  let in_sym, in_name, in_quantity <- get_single_asset_from_value_apart_from_ada(in_val)
  let name_is_authentic = when optional_auth_name is {
    Some(auth_name) -> in_name == auth_name
    None -> True
  }
  and {
    in_val == out_val,
    in_dat == out_dat,
    in_addr == out_addr,
    in_ref_script == out_ref_script,
    in_sym == auth_symbol,
    name_is_authentic,
    in_quantity == 1,
  }
}
```

with two supporting extractors that show up again and again across protocols:

```rust
// repos/aiken-design-patterns/lib/aiken-design-patterns/utils.ak:55
pub fn get_single_asset_from_value_apart_from_ada(
  v: Value,
  return: Scott3<PolicyId, AssetName, Int, result>,
) -> result {
  expect [Pair(sym, tn_qty_pairs)] = v
    |> assets.to_dict
    |> dict.delete(assets.ada_policy_id)
    |> dict.to_pairs
  expect [Pair(tn, qty)] = tn_qty_pairs |> dict.to_pairs
  return(sym, tn, qty)
}

// :93
pub fn get_lovelace_and_single_nft_name(
  value: Value,
  nft_policy_id: PolicyId,
  return: fn(Lovelace, AssetName) -> result,
) -> result {
  let (ada_tn_qty_dict, rest_of_policies) = value
    |> assets.to_dict
    |> dict.expect_pop(ada_policy_id)
  ...
  expect nft_policy == nft_policy_id
  expect nft_qty == 1
  return(lovelace_name_qty_pair.2nd, nft_name)
}
```

Note the efficiency shape: it never calls `flatten`. It pops the ADA entry off the `Dict` and
pattern-matches the remainder to exactly one policy / one name. `assets.to_dict` is already sorted,
so this is a constant number of head/tail steps.

`amaru-treasury` writes the same helper independently:

```rust
// /Users/nau/projects/lantr/amaru-treasury/lib/outputs.ak:54
pub fn expect_nft(
  value: Value,
  expected_policy: ScriptHash,
  expected_asset_name: AssetName,
) -> Bool {
  let pairs = value |> assets.to_dict |> dict.to_pairs
  expect [_ada, Pair(policy, assets)] = pairs
  expect [Pair(asset_name, quantity)] = assets |> dict.to_pairs
  and {
    (policy == expected_policy)?,
    (asset_name == expected_asset_name)?,
    (quantity == 1)?,
  }
}
```

and Hydra writes it as `hasST`:

```haskell
-- repos/hydra/hydra-plutus/src/Hydra/Contract/Util.hs:43
hasST :: CurrencySymbol -> Value -> Bool
hasST headPolicyId v =
  fromMaybe False $ do
    tokenMap <- AssocMap.lookup headPolicyId $ getValue v
    quantity <- AssocMap.lookup (TokenName hydraHeadV2) tokenMap
    pure $ quantity == 1
```

### 1.5 Validity-range normalization

```rust
// repos/aiken-design-patterns/lib/aiken-design-patterns/validity-range-normalization.ak:16
pub type NormalizedTimeRange {
  ClosedRange { lower: Int, upper: Int }
  FromNegInf { upper: Int }
  ToPosInf { lower: Int }
  Always
  InvalidRange
}

// :25
pub fn normalize_time_range(validity_range: ValidityRange) -> NormalizedTimeRange {
  ...
  (interval.Finite(init_lower_int), interval.Finite(init_upper_int)) -> {
    let lower = resolve_lower(init_lower_int, lower_is_inclusive)
    let upper = resolve_upper(init_upper_int, upper_is_inclusive)
    // This check is performed by Cardano's first phase of validation.
    // However, we are re-performing it here in order to keep this helper
    // generic for `ValidityRange` instances from any source.
    if lower >= upper { InvalidRange } else { ClosedRange { lower, upper } }
  }
  ...
}
// :62
fn resolve_lower(init_lower: Int, is_inclusive: Bool) -> Int {
  if is_inclusive { init_lower } else { init_lower + 1 }
}
```

The motivation (module header, `:1-9`): *"The datatype that models validity range in Cardano
currently allows for values that are either meaningless, or can have more than one representation."*
This is exactly the kind of paper-cut a stdlib should erase.

`treasury-contracts` writes a related guard because an attacker can widen the lower bound:

```rust
// /Users/nau/projects/lantr/treasury-contracts/lib/utilities.ak:167
// Check that an interval is *at most* a specific length, in milliseconds
// In particular, can be used to make sure the transaction isn't playing games with the
// lower bound of the transaction
pub fn interval_length_at_most(interval: Interval<Int>, length: Int) -> Bool {
  when (interval.upper_bound.bound_type, interval.lower_bound.bound_type) is {
    (Finite(b), Finite(a)) -> b - a <= length
    _ -> False
  }
}
pub const thirty_six_hours: Int = 36 * 60 * 60 * 1000   // :162
```

### 1.6 Merkelized validator (delegated computation)

```rust
// repos/aiken-design-patterns/lib/aiken-design-patterns/merkelized-validator.ak:37
pub type ComputationRedeemer<a, b> { input_arg: a, result: b }

// :53
pub fn delegated_compute(
  function_input: a,
  staking_validator: ScriptHash,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  redeemer_index: Int,
  input_data_coercer: fn(Data) -> a,
  output_data_coercer: fn(Data) -> b,
) -> b {
  expect ComputationRedeemer { input_arg, result }: ComputationRedeemer<Data, Data> =
    get_withdraw_scripts_redeemer_at(staking_validator, redeemers, redeemer_index)
  let coerced_input = input_data_coercer(input_arg)
  // Given input argument must be identical to the one provided to the withdrawal validator.
  expect coerced_input == function_input
  output_data_coercer(result)
}

// :104
pub fn computation_withdrawal_wrapper(
  redeemer: ComputationRedeemer<a, b>,
  function: fn(a) -> b,
) -> Bool {
  let ComputationRedeemer { input_arg, result } = redeemer
  let computed_result = function(input_arg)
  result == computed_result
}
```

### 1.7 On-chain linked list

Two independent implementations exist and they agree on the mechanism: **membership is authenticated
by a per-node NFT whose asset name is `prefix ++ key`, and insertion proves `prev < key < next`
lexicographically.**

```rust
// repos/aiken-design-patterns/lib/aiken-design-patterns/linked-list/internal.ak:96
pub fn key_fits_between(
  required_ordering: Ordering,
  previous_key: Option<ByteArray>,
  new_key: ByteArray,
  next_key: Option<ByteArray>,
) -> Bool {
  expect when previous_key is {
      None -> True
      Some(key) -> bytearray.compare(key, new_key) == required_ordering
    }
  expect when next_key is {
      None -> True
      Some(key) -> bytearray.compare(new_key, key) == required_ordering
    }
  True
}

// :66 — "exactly one asset changes under this policy, with this quantity"
pub fn is_only_mint_under_policy(
  tx_mint: Value, policy_id: PolicyId, asset_name: AssetName, expected_quantity: Int,
) -> Bool {
  expect Some(actual_minted_assets) = assets.to_dict(tx_mint) |> dict.get(policy_id)
  expect [Pair(actual_asset_name, actual_quantity)] = dict.to_pairs(actual_minted_assets)
  expect actual_asset_name == asset_name
  expect actual_quantity == expected_quantity
  True
}
```

```rust
// repos/aiken-linked-list/lib/linkedlist/utils.ak:26
pub fn cover_key(node: SetNode, insert_key: PubKeyHash) -> Bool {
  let less_than_key = when node.key is {
      Empty -> True
      Key(key) -> bytearray.compare(key, insert_key) == Less
    }
  let more_than_key = when node.next is {
      Empty -> True
      Key(key) -> bytearray.compare(key, insert_key) == Greater
    }
  less_than_key? && more_than_key?
}

// :40
pub fn as_predecessor_of(node: SetNode, next_key: PubKeyHash) -> SetNode {
  SetNode { key: node.key, next: Key(next_key) }
}
pub fn as_successor_of(prev_key: PubKeyHash, node: SetNode) -> SetNode {
  SetNode { key: Key(prev_key), next: node.next }
}

// lib/linkedlist/linked_list.ak:37
pub fn insert(common: Common, insert_key: PubKeyHash, node: SetNode) -> Bool {
  let must_cover_inserting_key = utils.cover_key(node, insert_key)
  expect [covering_node] = common.node_inputs
  let prev_node_datum = utils.as_predecessor_of(node, insert_key)
  let node_datum = utils.as_successor_of(insert_key, node)
  ...
  let must_mint_correct =
    utils.validate_mint(
      common.mint, common.own_cs,
      bytearray.concat(constants.origin_node_token_name, insert_key), 1)
  must_cover_inserting_key? && must_has_datum_in_output? && must_correct_node_output? && must_mint_correct?
}
```

The Anastasia design-patterns README states the invariant a stdlib must encode
(`README.md:280-292`): *"List membership is authenticated by an asset under the list NFT policy,
never by the payment credential alone… An ADA-only UTxO, or a UTxO carrying only foreign-policy
assets, is therefore not a list element even when it sits at that credential."*

### 1.7b Parameterized-script instance proof

A minting policy that must only pay to *instances of* a parameterized spending script cannot compare
script hashes directly (each parameter yields a different hash). The pattern reconstructs the applied
script's hash on-chain from a precomputed Flat/CBOR prefix:

```rust
// repos/aiken-design-patterns/lib/aiken-design-patterns/parameter-validation.ak:38
const param_header_28_bytes = #"004c011e581c"
const postfix: ByteArray = #"0001"

// :42
fn append_first_param_to_prefix(version: Int, prefix: ByteArray, param: ByteArray) -> ByteArray {
  builtin.integer_to_bytearray(True, 1, version)
    |> bytearray.concat(prefix)
    |> bytearray.concat(blake2b_224(param))
}

// :73
pub fn apply_param(version: Int, prefix: ByteArray, param: ByteArray) -> ScriptHash {
  append_first_param_to_prefix(version, prefix, param)
    |> bytearray.concat(postfix)
    |> blake2b_224
}
```

Module header states the restrictions this imposes (`parameter-validation.ak:15-23`):

> 1. Parameters of the script must have constant lengths, which can be achieved by having them
>    hashed
> 2. Consequently, for each transaction, the resolved value of those parameters must be provided
>    through the redeemer
> 3. The dependent script must be provided with CBOR bytes of instances before and after the
>    parameter(s)
> 4. Wrapping instances' logic in an outer function so that there'll be single occurrences of each
>    parameter

Variants: `apply_prehashed_param` (89), `apply_param_2` (100), `apply_prehashed_param_2` (119),
`apply_param_3` (133), `apply_prehashed_param_3` (154), plus `wrapper` / `wrapper_2` / `wrapper_3`
(179/196/219) and `wrapper_no_redeemer*` (244/260/282) for the parameterized side.

### 1.8 Merkle Patricia Forestry (MPF)

The de-facto standard for "big state, small UTxO" on Cardano.

Public surface (`repos/mpf/on-chain/lib/aiken/merkle-patricia-forestry.ak`):

| Function | Line | Purpose |
| --- | --- | --- |
| `from_root(root: ByteArray) -> MerklePatriciaForestry` | 58 | construct trie handle from a root hash |
| `is_empty(self) -> Bool` | 80 | |
| `has(self, key, value, proof) -> Bool` | 95 | membership proof |
| `miss(self, key, proof) -> Bool` | 114 | **non**-membership proof |
| `insert(self, key, value, proof) -> MerklePatriciaForestry` | 131 | returns new root |
| `delete(self, key, value, proof) -> MerklePatriciaForestry` | 152 | |
| `update(self, key, proof, old_value, new_value) -> …` | 176 | |
| `root(self) -> ByteArray` | 194 | |

Measured cost from `repos/mpf/README.md:58-67` — worth quoting in the Scalus design because it sets
the bar:

| trie size | proof bytes | insert/delete | membership | non-membership |
| --- | --- | --- | --- | --- |
| 10² | ~200 | 53.9M cpu | 32.3M cpu | 21.6M cpu |
| 10⁶ | ~760 | 126.3M cpu | 67.3M cpu | 59.0M cpu |
| 10⁹ | ~1180 | 180.6M cpu | 93.5M cpu | 87.1M cpu |

> "On current mainnet, 140K mem units and 100M cpu units corresponds respectively to 1% of the
> maximum transaction mem and cpu budgets." — `repos/mpf/README.md:71`

---

## 2. Treasury / governance: SundaeSwap + Intersect + PRAGMA

### 2.1 `MultisigScript` — a reusable authorization DSL (the strongest single find)

Shared by `treasury-contracts` (Intersect Cardano Treasury) and `amaru-treasury` (PRAGMA) through
the `SundaeSwap-finance/aicone` package. It is the closest thing Cardano has to a standard
"who may do this" type.

```rust
// repos/aicone/lib/sundae/multisig.ak:11
pub type MultisigScript {
  Signature { key_hash: ByteArray }
  AllOf { scripts: List<MultisigScript> }
  AnyOf { scripts: List<MultisigScript> }
  AtLeast { required: Int, scripts: List<MultisigScript> }
  Before { time: Int }
  After { time: Int }
  Script { script_hash: ByteArray }
}

// :21
pub fn satisfied(
  script: MultisigScript,
  signatories: List<ByteArray>,
  valid_range: ValidityRange,
  withdrawals: Pairs<Credential, Lovelace>,
) -> Bool {
  when script is {
    Signature { key_hash } -> list.has(signatories, key_hash)
    AllOf { scripts } -> list.all(scripts, fn(s) { satisfied(s, signatories, valid_range, withdrawals) })
    AnyOf { scripts } -> list.any(scripts, fn(s) { satisfied(s, signatories, valid_range, withdrawals) })
    AtLeast { required, scripts } ->
      required <= list.count(scripts, fn(s) { satisfied(s, signatories, valid_range, withdrawals) })
    Before { time } -> when valid_range.upper_bound is {
        IntervalBound { bound_type: Finite(hi), is_inclusive: True } -> hi <= time
        IntervalBound { bound_type: Finite(hi), is_inclusive: False } -> hi < time
        _ -> False
      }
    After { time } -> when valid_range.lower_bound is {
        IntervalBound { bound_type: Finite(lo), is_inclusive: True } -> time <= lo
        IntervalBound { bound_type: Finite(lo), is_inclusive: False } -> time < lo
        _ -> False
      }
    Script { script_hash } -> pairs.has_key(withdrawals, address.Script(script_hash))
  }
}
```

Two things to steal:

1. The `Script { script_hash }` leaf **is** the withdraw-0 pattern used as delegation: "this
   authorization is satisfied if that other script also ran in this transaction". PRAGMA's comment
   is explicit: *"The 'withdraw' purpose is used by sundae/multisig to simulate a remote-call from
   another script. So this is effectively the script interface to others."*
   (`/Users/nau/projects/lantr/amaru-treasury/validators/permissions.ak:33-35`)
2. `satisfied_payload` (`repos/aicone/lib/sundae/multisig.ak:65`) is the same DSL evaluated against
   **detached ed25519 signatures** rather than `extra_signatories`, with a documented subtlety:

```rust
// repos/aicone/lib/sundae/multisig.ak:73-82
    // A member without a matching signature is unsatisfied, not an abort:
    // AtLeast counts with list.count (no short-circuit), so a trapping leaf
    // would fail a met threshold on the first non-signing member.
    Signature { key_hash } ->
      when list.find(signatures, fn((k, _)) { blake2b_224(k) == key_hash }) is {
        Some((public_key, signature)) -> verify_ed25519_signature(public_key, payload, signature)
        None -> False
      }
```

PRAGMA then *composes* the DSL algebraically to express "the scope owner and someone else":

```rust
// /Users/nau/projects/lantr/amaru-treasury/lib/scope.ak:128
pub fn approved_by_owner_and_someone_else(self, scope, scopes) -> Bool {
  multisig.satisfied(
    when scope is {
      CoreDevelopment ->
        multisig.AllOf([
          scopes.core_development,
          multisig.AnyOf([scopes.ops_and_use_cases, scopes.network_compliance, scopes.middleware]),
        ])
      ...
    },
    self.extra_signatories, self.validity_range, self.withdrawals)
}
```

### 2.2 Value conservation with min-ADA slack — `equal_plus_min_ada`

Appears in almost every treasury operation. Encodes "the tokens must be identical, but ADA may rise
to satisfy minUTxO":

```rust
// /Users/nau/projects/lantr/treasury-contracts/lib/utilities.ak:69
// Check that the assets are identical, but the lovelace of `actual` is *at least* as much as `expected`
// For example, if comparing inputs to outputs, this allows the ADA to increase if it needs to to cover minUTxO
// but doesn't allow it to decrease, and doesn't allow arbitrary other tokens to be added
pub fn equal_plus_min_ada(expected: Value, actual: Value) -> Bool {
  and {
    without_lovelace(expected) == without_lovelace(actual),
    lovelace_of(expected) <= lovelace_of(actual),
  }
}

// :80
pub fn greater_than_or_equal_to(left: Value, right: Value) -> Bool {
  right |> assets.flatten
    |> list.all(fn((policy, name, qty)) { assets.quantity_of(left, policy, name) >= qty })
}
```

Typical use (`disburse`): `input_sum - amount ≤ output_sum` modulo min-ADA.

```rust
// /Users/nau/projects/lantr/treasury-contracts/lib/logic/treasury/disburse.ak:28
let input_sum = value_sum(outputs_of(inputs), account, True)
let output_sum = value_sum(outputs, account, False)
expect and {
    equal_plus_min_ada(merge(input_sum, negate(amount)), output_sum)?,
    ...
  }
```

### 2.3 Sum value at a credential (with an explicit stake-credential policy)

```rust
// /Users/nau/projects/lantr/treasury-contracts/lib/utilities.ak:94
pub fn value_sum(
  outputs: List<Output>,
  account: Credential,
  allow_different_stake: Bool,
) -> Value {
  outputs
    |> list.filter(fn(output) { output.address.payment_credential == account })
    |> list.map(fn(output) {
          expect or {
              allow_different_stake,
              output.address.stake_credential == Some(Inline(account)),
            }
          output.value
        })
    |> list.reduce(assets.zero, assets.merge)
}

// :65 — the ubiquitous adapter
pub fn outputs_of(inputs: List<Input>) -> List<Output> {
  list.map(inputs, fn(input) { input.output })
}
```

The `allow_different_stake` flag encodes a real attack: outputs sent to the *same payment script*
but a *different stake credential* would otherwise leak control of staking rewards. `amaru-treasury`
codifies the safe address construction:

```rust
// /Users/nau/projects/lantr/amaru-treasury/lib/address.ak:22
/// Create an `Address` from a `ScriptHash`, using the script for both payment
/// credential and stake credential.
pub fn from_script(script: ScriptHash) -> Address {
  Address {
    payment_credential: Credential.Script(script),
    stake_credential: Some(Referenced.Inline(Credential.Script(script))),
  }
}
```

### 2.4 Payout schedule validation with hard size bounds

The bounds are a *safety* mechanism (exceeding ExUnits would lock funds forever), and the comments
show they were derived by benchmarking:

```rust
// /Users/nau/projects/lantr/treasury-contracts/lib/utilities.ak:119
pub fn payout_sum(
  outputs: List<Output>, vendor: Credential, filter: fn(VendorDatum, Payout) -> Bool,
) -> Value {
  outputs
    |> list.filter(fn(output) { output.address.payment_credential == vendor })
    |> list.foldl(assets.zero, fn(output, total) {
          expect InlineDatum(datum) = output.datum
          expect datum: VendorDatum = datum
          let (this_payout_sum, payout_count) = datum.payouts
            |> list.foldl((assets.zero, 0), fn(payout, acc) {
                  // Note that from_asset_list enforces invariants about the value stored in the InlineDatum
                  // such as no duplicates etc.
                  let value = assets.from_asset_list(payout.value)
                  // Benchmarking shows that if we allow more than 4 tokens in the value, we can exceed execution units
                  // on the sweep / withdraw operations quickly, leaving funds locked forever
                  expect 4 >= ( value |> assets.flatten |> list.length )
                  ...
                })
          expect payout_count <= 24
          // Ensure that the output has enough funds to cover the payout
          expect equal_plus_min_ada(this_payout_sum, output.value)
          assets.merge(total, this_payout_sum)
        })
}
```

Two reusable ideas: **datum-carried `Value` must be re-validated** (`from_asset_list` enforces
sorted/no-duplicate/no-zero), and **bounded traversal** to keep a validator inside the budget.

### 2.5 Reference-input script registry (breaking circular script dependencies)

Treasury needs the vendor script hash, vendor needs the treasury script hash. Solution: a one-shot
NFT locked at an always-fail address whose inline datum names both, read as a reference input.

```rust
// /Users/nau/projects/lantr/treasury-contracts/lib/utilities.ak:13
pub fn find_script_hash_registry(
  reference_inputs: List<Input>, registry_token: PolicyId,
) -> ScriptHashRegistry {
  expect Some(input) = reference_inputs
      |> list.find(fn(input) {
            assets.quantity_of(input.output.value, registry_token, registry_token_name()) > 0
          })
  expect InlineDatum(datum) = input.output.datum
  expect registry: ScriptHashRegistry = datum
  registry
}
```

`amaru-treasury` has the exact same shape for its dynamic permission state:

```rust
// /Users/nau/projects/lantr/amaru-treasury/lib/scope.ak:76
pub fn expect_scopes(inputs: List<Input>, policy_id: PolicyId) -> Scopes {
  when inputs is {
    [] -> { trace @"no scopes found in reference inputs for policy id": policy_id
            fail }
    [head, ..tail] ->
      if assets.quantity_of(head.output.value, policy_id, config.scopes_token_name) == 1 {
        with_inline_scopes(head.output.datum, identity)
      } else {
        expect_scopes(tail, policy_id)
      }
  }
}
```

Both also share the "assert-inline-datum-of-shape" continuation helper:

```rust
// /Users/nau/projects/lantr/amaru-treasury/lib/registry.ak:59
pub fn with_inline_registry(datum: Datum, return: fn(ScriptHashRegistry) -> result) -> result {
  expect Datum.InlineDatum(data) = datum
  expect registry: ScriptHashRegistry = data
  return(registry)
}
```

### 2.6 One-shot mint tied to a consumed `OutputReference`

```rust
// /Users/nau/projects/lantr/treasury-contracts/validators/oneshot.ak:9
// A one-shot NFT for the script hash registry
// Allows minting a single NFT with a unique policy ID
// Also disallows spending, so we can use this script address to hold the registry
validator oneshot(utxo_ref: OutputReference) {
  spend(_d: Option<ScriptHashRegistry>, _r: Data, _o: OutputReference, _s: Transaction) {
    False
  }
  mint(_r: Void, _policy_id: ByteArray, self: Transaction) {
    let Transaction { inputs, .. } = self
    expect Some(_) = list.find(inputs, fn(input) { input.output_reference == utxo_ref })
    expect [(_, _, qty)] = self.mint |> assets.flatten
    expect qty == 1
    True
  }
  else(_) { fail }
}
```

Hydra writes the identical idiom, plus the "the seed must also be recorded in the produced datum"
refinement:

```haskell
-- repos/hydra/hydra-plutus/src/Hydra/Contract/HeadTokens.hs:85
  seedInputIsConsumed =
    traceIfFalse $(errorCode SeedNotSpent) $
      seedInput `L.elem` (txInInfoOutRef <$> txInfoInputs txInfo)

-- :112
  checkDatum =
    traceIfFalse $(errorCode WrongDatum) $
      headId == currency && seed == seedInput
```

### 2.7 Anti-fee-griefing: forbid reference scripts on outputs

A subtle, easily-missed protection that a stdlib should name:

```rust
// /Users/nau/projects/lantr/treasury-contracts/lib/utilities.ak:57
// Because reference scripts have an exponential (if small) effect on the fee,
// we disallow reference scripts on the outputs, to prevent someone from being annoying
pub fn ensure_no_ref_scripts(outputs: List<Output>) {
  expect outputs |> list.all(fn(output) { option.is_none(output.reference_script) })
}
```

### 2.8 Governance-specific: certificate and vote endpoints

Rarely-seen but standard-shaped constitutional constraints, worth a stdlib helper:

```rust
// /Users/nau/projects/lantr/treasury-contracts/validators/treasury.ak:83
  publish(_redeemer: Data, certificate: Certificate, self: Transaction) {
    // Article IV - Section 5 of the constitution requires that funds:
    //   - SHALL NOT be delegated to an SPO
    //   - MUST be delegated to an auto-abstain DRep
    when certificate is {
      UnregisterCredential { .. } -> is_entirely_after(self.validity_range, config.expiration)
      RegisterCredential { .. } -> True
      DelegateCredential { delegate, .. } | RegisterAndDelegateCredential { delegate, .. } ->
        delegate == DelegateVote(AlwaysAbstain)
      _ -> False
    }
  }
  vote(_r, _v, _t) { False }
```

---

## 3. Anti-double-satisfaction: distinct mechanisms

Four are quoted here from the repos I read first-hand; the full set of six, with per-protocol
attribution, is consolidated in §7.1.

Double satisfaction is the single most-discussed vulnerability, and protocols solve it in
**structurally different** ways. A stdlib needs one API per mechanism, not one generic helper.

### DS-1. Input whitelist ("no foreign script inputs")

Used by Intersect treasury. Cheapest to reason about, but only works for a closed protocol.

```rust
// /Users/nau/projects/lantr/treasury-contracts/lib/utilities.ak:33
// To prevent double satisfaction, we disallow any inputs from *other* scripts
// ONLY the treasury and vendor scripts are allowed on the inputs
pub fn ensure_compliant_scripts(inputs: List<Input>, registry: ScriptHashRegistry) {
  expect inputs |> list.all(fn(input) {
        when input.output.address.payment_credential is {
          // However, someone needs to pay fees, so verification key credentials are allowed
          VerificationKey(_) -> True
          Script(_) -> or {
              input.output.address.payment_credential == registry.treasury,
              input.output.address.payment_credential == registry.vendor,
            }
        }
      })
}
```

Wired into every spend:

```rust
// /Users/nau/projects/lantr/treasury-contracts/validators/treasury.ak:48
    // We should *only* have inputs from the vendor and treasury, to prevent double satisfaction issues
    ensure_compliant_scripts(self.inputs, registry)
```

### DS-2. "Exactly one script input, and all its redeemers agree"

Used by PRAGMA's `amaru-treasury`. Generalizes DS-1: any number of UTxOs may be spent, but they must
all belong to **one** script hash and carry **identical** redeemers.

```rust
// /Users/nau/projects/lantr/amaru-treasury/lib/inputs.ak:46
/// Traverse all inputs searching for script-locked UTxOs. Return the associated redeemer.
/// Fails if:
/// - there's no script-locked UTxO whatsoever;
/// - multiple UTxOs are locked by *different* script hash;
/// - not all redeemers for these scripts are equal to one another.
pub fn expect_single_script(
  inputs: List<Input>, redeemers: Pairs<ScriptPurpose, Data>,
) -> Data {
  expect [Pair(_, redeemer)] = collect_script_inputs(inputs, redeemers) |> dict.to_pairs
  redeemer
}

// :60 — de-duplication via Dict, with an equality merge that FAILS on disagreement
fn collect_script_inputs(
  inputs: List<Input>, redeemers: Pairs<ScriptPurpose, Data>,
) -> Dict<ScriptHash, Data> {
  let next, scripts <- list.foldl(inputs, dict.empty)
  when next.output.address.payment_credential is {
    Credential.Script(script_hash) -> {
      expect Some(redeemer) = pairs.get_first(redeemers, ScriptPurpose.Spend(next.output_reference))
      scripts |> dict.insert_with(script_hash, redeemer,
          fn(_, left, right) { expect left == right
                               Some(left) })
    }
    Credential.VerificationKey(..) -> scripts
  }
}
```

The `dict.insert_with` merge function doing `expect left == right` is a neat trick: one fold does
both de-duplication and agreement checking.

### DS-3. Single-input assertion at the operation level

The simplest form — used per-endpoint rather than per-transaction:

```rust
// /Users/nau/projects/lantr/treasury-contracts/lib/logic/vendor/withdraw.ak:27
  expect [vendor_input] =
    outputs_of(inputs)
      |> list.filter(fn(input) { input.address.payment_credential == account })
```

and its output twin (`:62`):

```rust
    expect [vendor_output] =
      outputs |> list.filter(fn(output) { output.address.payment_credential == account })
    expect vendor_output.address.stake_credential == Some(Inline(account))
```

### DS-0 (non-mechanism). Explicit "you must handle this yourself" marker

The design-patterns library refuses to pretend it solved the problem, and makes the caller pass a
proof-obligation boolean (`singular-utxo-indexer.ak:26`, `:60`; `multi-utxo-indexer.ak` relies on
ascending indices instead). Quoted in §1.2.

*(DS-4, output-tagged-with-spent-`OutputReference`, is the JPG Store / Cardano-Swaps / Lenfi
mechanism; DS-5 is index-hints-plus-count; DS-6 is Butane/Indigo aggregate accounting. All three are
quoted in `raw-nft-marketplace.md` and `raw-lending-cdp.md`, and consolidated in §7.1.)*

---

## 4. Efficiency tricks observed

| Trick | Where | Quote / note |
| --- | --- | --- |
| **Index hints in the redeemer** instead of `find` | design-patterns `singular-utxo-indexer.ak:20`, `multi-utxo-indexer.ak:18`, `stake-validator.ak:44` | `inputs \|> list.at(input_index)` then `expect own_ref == in_ref` |
| **Ascending-index proof** to avoid a set/dedup structure | `multi-utxo-indexer.ak:38` | `if i == in1 && in1 > in0 && out1 > out0` |
| **Direct `builtin.tail_list` recursion** rather than `list.find` | design-patterns `utils.ak:42` (copied from Fortuna) | see below |
| **Never `flatten` a `Value`** — pop the ADA entry and match the rest | design-patterns `utils.ak:55`/`:93`, amaru `outputs.ak:54` | `assets.to_dict \|> dict.delete(ada_policy_id) \|> dict.to_pairs` then `expect [Pair(sym, …)]` |
| **`serialiseData` for hashing aggregates** | Hydra `Util.hs:104` | `sha2_256 . F.foldMap (Builtins.serialiseData . toBuiltinData)` |
| **Whole-`Output` structural equality** instead of field-by-field | design-patterns `utils.ak:245-253`; Hydra `Util.hs:72` | `in_val == out_val, in_dat == out_dat, in_addr == out_addr, …` |
| **Single fold doing dedup + agreement** | amaru `inputs.ak:71` | `dict.insert_with(…, fn(_, left, right) { expect left == right; Some(left) })` |
| **Early exit on unique tokens** | amaru `outputs.ak:40-43` | *"We need not to check for the tail here because the token is guaranteed to be unique."* |
| **Bounded traversal to stay inside ExUnits** | treasury `utilities.ak:143`, `:154` | `expect 4 >= (value \|> flatten \|> length)`, `expect payout_count <= 24` |
| **Scott-encoded continuation returns** (avoid tuple allocation) | design-patterns `utils.ak:55` | `return: Scott3<PolicyId, AssetName, Int, result>` |
| **Delegated computation via withdraw-0** (trade tx size for CPU) | `merkelized-validator.ak:53` | |
| **Withdraw-0 to run per-tx logic once** instead of per-input | `stake-validator.ak:1-10` | |
| **Lazy field decoding of the whole `ScriptContext`** | Indigo `src/Indigo/Utils/Spooky.hs:823` | every field wrapped in a `Spooky` (`BuiltinData` newtype) so untouched fields are never decoded — the strongest possible statement of the "Data-level representation" argument in `07-efficiency-constraints.md` |
| **Aggregate accounting instead of per-action checks** | Butane `cdp_script.ak:845` | accumulate a `StateDelta{mint, btn_delta, fee, lock_mints}` across every action, then assert **one** equality against `tx.mint`; strictly stronger anti-double-satisfaction than tagging, but needs the withdraw-0 architecture |
| **Counter token as datum-free state** | Fortuna `tunav2.ak` | the block number is encoded in a token name that is burned and re-minted each block, so a *minting policy* can read protocol state with no datum at all |
| **Salt-grinding to control list ordering** | Butane `upgradeable.ak:24` | a `_salt: Int` script parameter is ground off-chain so the script's own credential lands at a predictable index in the `withdrawals` list |
| **Datum / redeemer size caps** | Fortuna `tunav1.ak:307` | griefing protection: an oversized datum otherwise inflates every future spend |
| **Non-reducing rational** (`gcd` is expensive on-chain) | liqwid `Rational.hs:65` + `mulTruncate`/`divTruncate`/`mulDivTruncate` `:153-186` | the "apply a rate to an integer" primitive every lending protocol hand-rolls |

```rust
// repos/aiken-design-patterns/lib/aiken-design-patterns/utils.ak:41
/// Copied from [Fortuna](https://github.com/cardano-miners/fortuna/blob/…/lib/fortuna/utils.ak#L6-L17).
pub fn resolve_output_reference(
  inputs: List<Input>, output_ref: OutputReference,
) -> Output {
  expect [input, ..] = inputs
  if input.output_reference == output_ref {
    input.output
  } else {
    resolve_output_reference(builtin.tail_list(inputs), output_ref)
  }
}
```

The most aggressive example in the whole corpus: `validate_singular_authentic_input` scans **all**
transaction inputs in one `foldr`, using raw builtins to peel the ADA entry off the `Value` map
without ever building an `assets` dict, and simultaneously proves *exactly one* input carries the
list policy:

```rust
// repos/aiken-design-patterns/lib/aiken-design-patterns/linked-list.ak:1552
pub fn validate_singular_authentic_input(
  inputs: List<Input>,
  nft_policy_id: PolicyId,
  return: fn(Input, Lovelace, AssetName, GenericElementData, Link) -> Bool,
) -> Bool {
  let nft_policy_id_data: Data = builtin.b_data(nft_policy_id)
  let current_input, input_found <- list.foldr(inputs, False)
  let current_output = current_input.output
  let current_value = current_output.value

  if input_found {
    let value_unmapped: List<Pair<Data, Data>> = builtin.un_map_data(current_value)
    let non_ada_asset_pairs = value_unmapped |> builtin.tail_list
    when non_ada_asset_pairs is {
      [policy_name_pair] -> {
        // Once the singular element has been found, another singleton
        // list-shaped input must not use the same list policy.
        expect policy_name_pair.1st != nft_policy_id_data
        input_found
      }
      _ -> input_found
    }
  } else {
    let current_value_pairs: List<Pair<Data, Data>> = builtin.un_map_data(current_value)
    let current_non_ada_asset_pairs = current_value_pairs |> builtin.tail_list
    when current_non_ada_asset_pairs is {
      [policy_asset_names_pair] ->
        if policy_asset_names_pair.1st == nft_policy_id_data { … } else { input_found }
      _ -> input_found
    }
  }
}
```

Three techniques in one function: **`Data`-level comparison** (`builtin.b_data(policy) ==
pair.1st`, no ByteString decoding), **`builtin.tail_list` to skip the always-first ADA entry**, and
**one pass that both finds and proves uniqueness**.

PRAGMA writes `resolve_output_reference` without the builtin, showing it is a universal need:

```rust
// /Users/nau/projects/lantr/amaru-treasury/lib/inputs.ak:27
pub fn resolve(inputs: List<Input>, utxo_ref: OutputReference) -> Output {
  when inputs is {
    [] -> fail
    [head, ..tail] ->
      if head.output_reference == utxo_ref { head.output } else { resolve(tail, utxo_ref) }
  }
}
```

---

## 4b. Lending / CDP / PoW — first-hand corroboration

Read directly (independently of `raw-lending-cdp.md`), so the `LEN` / `BUT` / `FOR` marks in §7 stand
on their own.

### 4b.1 Lenfi (Aada) — `lib/aada/`

| Helper | Line | Purpose |
| --- | --- | --- |
| `id_from_utxo(consumed_utxo: OutputReference) -> ByteArray` | `nft.ak:38` | one-shot token name = `blake2b_256(serialise_data(outRef))` |
| `check_uniqueness(nft_action, inputs, mint, own_policy) -> Bool` | `nft.ak:43` | the full one-shot mint proof |
| `validate_mint_nft(mints, policy, name, amt) -> Bool` | `utils.ak:52` | exactly this asset under this policy |
| `validate_mint_nft_few_per_policy(...)` | `utils.ak:65` | this asset is among them |
| `get_ref_token_datum(ref_inputs, ref_token) -> Option<ReferenceTokenDatum>` | `utils.ak:32` | config-NFT indirection |
| `get_input_by_nft(inputs, policy, name) -> Input` | `utils.ak:105` | auth-NFT lookup (`quantity_of(...) == 1`) |
| `get_outputs_by_nft(outputs, policy, name) -> List<Output>` | `utils.ak:93` | |
| `output_has_quantity_with_address(output, address, policy, name, amount)` | `utils.ak:79` | payout check |
| `authorized_by_credential(extra_signatories, withdrawals, payment_key) -> Bool` | `utils.ak:123` | stake-credential-approves |
| `retrieve_oracle_data(inputs, oracle_asset, redeemers, valid_to)` | `utils.ak:144` | oracle read via the oracle's **withdrawal redeemer** |
| `calculate_health_factor`, `check_is_overcollaterized`, `check_is_undercollaterized`, `get_interest_rates`, `calculate_interest_amount` | `finance.ak:71/83/104/122/6` | ratio math |
| `safe_div(left: Rational, right: Rational) -> Option<Rational>` | `utils.ak:137` | with a comment: *"Used to circumvent a strange codegen bug that doesn't work with the standard rational div"* |

```rust
// repos/lenfi/lib/aada/nft.ak:38
pub fn id_from_utxo(consumed_utxo: OutputReference) -> ByteArray {
  hash.blake2b_256(builtin.serialise_data(consumed_utxo))
}

// :43
pub fn check_uniqueness(
  nft_action: NFTAction, inputs: List<Input>, mint: MintedValue, own_policy: ByteArray,
) -> Bool {
  when nft_action is {
    MintNFT(consumed_utxo, _) -> {
      let nft_name = id_from_utxo(consumed_utxo)
      utils.validate_mint_nft(mint, own_policy, nft_name, 1) && list.any(
        inputs, fn(input) { input.output_reference == consumed_utxo })
    }
    BurnNFT(nft_name) -> utils.validate_mint_nft(mint, own_policy, nft_name, -1)
  }
}
```

```rust
// repos/lenfi/lib/aada/utils.ak:118 (doc comment) / :123
/// Transaction authorized by credential.
/// If verification key, check it is contained within extra_signatories
/// If script, check it is in stake withdrawals (programmable by stake scripts)
pub fn authorized_by_credential(
  extra_signatories: List<Hash<Blake2b_224, VerificationKey>>,
  withdrawals: dict.Dict<StakeCredential, Int>,
  payment_key: Credential,
) -> Bool {
  when payment_key is {
    VerificationKeyCredential(payment_key_hash) -> list.has(extra_signatories, payment_key_hash)
    script_credential -> dict.has_key(withdrawals, Inline(script_credential))
  }
}
```

That last one is the `stake-credential-approves` idiom in its clearest form: **a `Credential` is a
first-class authorizer, and a script credential is satisfied by a withdraw-0.** Cardano-Swaps and
CIP-113 write the same function (`raw-nft-marketplace.md` §2.5, §4.5).

### 4b.2 Butane — `lib/butane/utils.ak`

| Helper | Line | Purpose |
| --- | --- | --- |
| `contains_interval(self, interval) -> Bool` | 23 | interval containment via `intersection == interval` |
| `finite_interval_range(interval) -> Int` | 28 | width of a finite interval |
| `check_price_feed_validity(pf_validity, tx_validity)` | 36 | **oracle freshness** |
| `until_input_from(cred, inputs)` | 46 | `builtin.tail_list` scan to the first input at a credential |
| `mints_nothing_here(v, p)` | 61 | |
| `only_mints_this(v, p, n, a)` | 66 | |
| `withdraws_zero(withdrawals, cred)` | 400 | withdraw-0, checked as *exactly zero* |
| `not_withdrawing_from(withdrawals, cred)` | 393 | the negative form |
| `params_from_refs(reference_inputs, own_hash)` | 417 | config-NFT indirection, keyed by an **asset-name prefix** |
| `authorization_check(...)` | 326 | |
| `calculate_fee_percent` / `get_treasury_share` | 151 / 286 | protocol-fee cut |
| `sorted_list_is_unique(l)` / `list_insert_at(l, idx, el)` | 126 / 88 | sorted-list guards |
| `compare_asset_classes(a, b)` | 78 | canonical pair ordering |
| `list_at(list, i)` / `unsome(x)` | `unsafe.ak:3` / `:11` | index hints, unchecked |

```rust
// repos/butane/lib/butane/utils.ak:36
pub fn check_price_feed_validity(
  pf_validity: Interval<PosixTime>, tx_validity: Interval<PosixTime>,
) {
  and {
    (finite_interval_range(pf_validity) < types.milliseconds_in_day)?,
    contains_interval(pf_validity, tx_validity)?,
  }
}

// :61 / :66
pub fn mints_nothing_here(v: Value, p: PolicyId) {
  builtin.null_list(value.tokens(v, p) |> dict.to_pairs)
}
pub fn only_mints_this(v: Value, p: PolicyId, n: AssetName, a: Int) {
  ( value.tokens(v, p) |> dict.to_pairs ) == [Pair(n, a)]
}

// :400
pub fn withdraws_zero(withdrawals: Pairs<StakeCredential, Int>, cred: StakeCredential) -> Bool {
  pairs.get_first(withdrawals, cred) == Some(0)
}
```

`check_price_feed_validity` is the sharpest oracle-freshness formulation in the corpus: the price
feed carries its **own validity interval**, the transaction's interval must be *inside* it, and that
interval must be shorter than a day. That is strictly stronger than Charli3's `current_time <=
expiry` (§6) and is what a stdlib helper should offer.

`params_from_refs` is config-NFT indirection with a documented invariant:

```rust
// repos/butane/lib/butane/utils.ak:417
pub fn params_from_refs(reference_inputs: List<Input>, own_hash: ByteArray) {
  let ret_list, ref_input <- list.reduce(reference_inputs, [])
  ...
  // We use invariant that any params tokens minted are the only Butane tokens in that utxo
  when value.tokens(ref_input_value, own_hash) |> dict.to_pairs is {
    [Pair(k, _)] ->
      if bytearray.take(k, types.params_prefix_length) == types.params_prefix {
        let params_synthetic_name = bytearray.drop(k, types.params_prefix_length)
        expect InlineDatum(params_data) = ref_input_data
        expect types.ParamsWrapper(params_datum) = to_monodatum(params_data)
        [ types.ParamsData { params: params_datum, synthetic: params_synthetic_name }, ..ret_list ]
      } else { ret_list }
    _ -> ret_list
  }
}
```

### 4b.3 Fortuna — `lib/fortuna/utils.ak`

The whole file is 154 lines and is *entirely* stdlib material. It is also the origin of
`resolve_output_reference`, which `aiken-design-patterns` copies with attribution (§4).

```rust
// repos/fortuna/lib/fortuna/utils.ak:6
pub fn resolve_output_reference(inputs: List<Input>, output_ref: OutputReference) -> Output {
  expect [input, ..] = inputs
  if input.output_reference == output_ref { input.output }
  else { resolve_output_reference(builtin.tail_list(inputs), output_ref) }
}

// :19 (doc) / :21
/// Disallows negative values by spiraling off into infinity (or the end of the list)
/// which will cause the transaction to fail.
pub fn list_at(l: List<a>, index: Int) -> a {
  if index == 0 { builtin.head_list(l) } else { list_at(builtin.tail_list(l), index - 1) }
}

// :60 — value shape assertion that exploits on-chain map ordering
pub fn value_has_nft_and_lovelace(val: Value, policy: PolicyId, asset_name: AssetName) -> Bool {
  expect [Pair(policy1, amount1), Pair(policy2, amount2)] =
    val |> value.to_dict() |> dict.to_pairs()
  if policy1 == value.ada_policy_id {
    // Should always reach here since maps come in ordered on chain
    expect [Pair(token_name, quantity)] = amount2 |> dict.to_pairs()
    and { policy2 == policy, quantity == 1, token_name == asset_name }
  } else {
    expect [Pair(token_name, quantity)] = amount1 |> dict.to_pairs()
    and { policy1 == policy, quantity == 1, token_name == asset_name }
  }
}

// :124
pub fn get_inline_datum(datum: Datum) -> Data {
  when datum is { InlineDatum(d) -> d
                  _ -> fail @"Expected inline datum" }
}

// :131 (doc) / :133 — big-endian int→bytes before `integerToByteString` existed
pub fn integer_to_bytes(i: Int, bytes: ByteArray) -> ByteArray {
  if i == 0 { bytes }
  else { integer_to_bytes(builtin.quotient_integer(i, 256), builtin.cons_bytearray(i, bytes)) }
}
```

Five of these six functions map 1:1 onto proposed stdlib rows (§8 rows 1, 5, 6, 14, 17).

---

## 5. `vodka` — the de-facto community stdlib surface

`sidan-lab/vodka` (shipped with MeshJS) is the closest thing to "what people expect a Cardano
on-chain stdlib to contain". Full public surface, grouped:

### 5.1 Value (`lib/cocktail/vodka_value.ak`)

| Function | Line |
| --- | --- |
| `value_length(value) -> Int` | 14 |
| `get_all_value_to(outputs, address) -> Value` | 22 |
| `get_all_value_from(inputs, address) -> Value` | 40 |
| `get_all_value_to_script(outputs, script_hash) -> Value` | 58 |
| `get_all_value_from_script(inputs, script_hash) -> Value` | 83 |
| `get_all_value_to_cred(outputs, cred) -> Value` | 108 |
| `get_all_value_from_cred(inputs, cred) -> Value` | 126 |
| `value_geq(greater, smaller) -> Bool` | 144 |
| `value_policy_info(...)` | 155 |
| `value_tokens(value) -> List<(PolicyId, AssetName, Int)>` | 166 |
| `inputs_value(inputs) -> Value` / `outputs_value(outputs) -> Value` | 174 / 182 |

### 5.2 Inputs / outputs (`vodka_inputs.ak`, `vodka_outputs.ak`)

Symmetric families, each with a curried predicate twin (`i_at`, `o_at`, …):

`input_inline_datum` (11) · `only_input_datum_with` (20) · `inputs_at` (33) · `inputs_with` (50) ·
`inputs_with_policy` (71) · `inputs_at_with` (90) · `inputs_at_with_policy` (118) ·
`inputs_token_quantity` (139) · `group_inputs` (154) · `group_inputs_2` (175)

`output_inline_datum` (10) · `outputs_at` (19) · `outputs_with` (36) · `outputs_with_policy` (57) ·
`outputs_at_with` (79) · `outputs_at_with_policy` (107) · `group_outputs` (144) ·
`group_outputs_2` (165)

`group_outputs_2` is a **single-pass 3-way partition** — the kind of primitive a budget-conscious
stdlib wants:

```rust
// repos/vodka/lib/cocktail/vodka_outputs.ak:165
pub fn group_outputs_2(
  outputs: List<Output>, group1: fn(Output) -> Bool, group2: fn(Output) -> Bool,
) -> (List<Output>, List<Output>, List<Output>) {
  outputs |> list.foldr(([], [], []), fn(output, acc) {
      if group1(output) { (acc.1st |> list.push(output), acc.2nd, acc.3rd) }
      else if group2(output) { (acc.1st, acc.2nd |> list.push(output), acc.3rd) }
      else { (acc.1st, acc.2nd, acc.3rd |> list.push(output)) }
    })
}
```

### 5.3 Mint (`vodka_mints.ak`) — four distinct strictness levels

```rust
// repos/vodka/lib/cocktail/vodka_mints.ak:8
pub fn check_policy_only_burn(mint: Value, policy: PolicyId) -> Bool {
  let mint_value = flatten(mint)
  list.all(mint_value, fn(x) { if x.1st == policy { x.3rd < 0 } else { True } })
}

// :26 — this policy minted exactly this one asset (other policies allowed)
pub fn policy_only_minted_token(mint, policy, name, quantity) {
  when flatten(mint) |> list.filter(fn(asset) { asset.1st == policy }) is {
    [(_, minted_asset_name, minted_quantity)] ->
      minted_asset_name == name && minted_quantity == quantity
    _ -> False
  }
}

// :43 — the whole tx minted exactly this one asset
pub fn only_minted_token(mint, policy, name, quantity) {
  when flatten(mint) is {
    [(minted_policy, minted_asset_name, minted_quantity)] ->
      minted_policy == policy && minted_asset_name == name && minted_quantity == quantity
    _ -> False
  }
}

// :60 — this asset is among the minted ones
pub fn token_minted(...)
```

### 5.4 CIP-68 (`lib/cip.ak`)

```rust
// repos/vodka/lib/cip.ak:17
pub const cip68_100_prefix: ByteArray = #"000643b0"   // reference token
pub const cip68_222_prefix: ByteArray = #"000de140"   // NFT
pub const cip68_333_prefix: ByteArray = #"0014df10"   // FT
pub const cip68_444_prefix: ByteArray = #"001bc280"   // rich-FT

pub fn drop_cip68_prefix(cip_68_asset_name: AssetName) -> AssetName {  // :28
  cip_68_asset_name |> bytearray.drop(4)
}
pub fn cip68_100(asset_name) { concat(cip68_100_prefix, asset_name) }  // :41
pub fn cip68_222(asset_name) { concat(cip68_222_prefix, asset_name) }  // :49
pub fn cip68_333(asset_name) { concat(cip68_333_prefix, asset_name) }  // :57
pub fn cip68_444(asset_name) { concat(cip68_444_prefix, asset_name) }  // :65
pub type CIP68Metadata { ... }                                         // :70
```

### 5.5 Other

| Module | Functions |
| --- | --- |
| `vodka_validity_range.ak` | `valid_after(range, time)` (8), `valid_before(range, time)` (24) |
| `vodka_extra_signatories.ak` | `key_signed` (13), `one_of_keys_signed` (27), `all_key_signed` (44) |
| `vodka_address.ak` | `compare_script_address` (6), `compare_address` (13), `address_payment_key` (29), `address_pub_key` (40), `address_script_hash` (51) |
| `vodka_redeemers.ak` | `redeemer_from(redeemers, inputs, out_ref, addr)` (12), `withdrawal_redeemer(redeemers, hash)` (32), `compare_output_reference` (41) |
| `vodka_withdrawals.ak` | `withdrawal_script_validated` (13) |
| `vodka_converter.ak` | `convert_int_to_bytes` (8), `get_number_digit` (24) |

---

## 6. Oracle consumption (Charli3)

The oracle-consumer side is small and highly standardized: a datum shape plus a freshness check.

```rust
// repos/charli3-aiken/lib/oracle_datum.ak:7
pub type PriceMap = Pairs<Int, Int>
pub type PriceData {
  SharedData
  ExtendedData
  GenericData { price_map: PriceMap }
}
pub type OracleDatum { price_data: PriceData }

// :33 / :48 / :63 — well-known integer keys 0/1/2
pub fn get_price(price_data)     { … pairs.get_first(price_map, 0) … }
pub fn get_timestamp(price_data) { … pairs.get_first(price_map, 1) … }
pub fn get_expiry(price_data)    { … pairs.get_first(price_map, 2) … }

// :79
pub fn is_oracle_valid(oracle_datum: OracleDatum, current_time: Int) -> Bool {
  current_time <= get_expiry(oracle_datum.price_data)
}
```

The consumer idiom is therefore: *find the reference input carrying the oracle NFT → decode inline
datum → assert `is_oracle_valid(datum, tx.validityRange.lowerBound)` → read price*. That is exactly
`find_script_hash_registry` (§2.5) + a freshness predicate, i.e. **one** stdlib helper covers both.

---

## 7. Cross-repo idiom matrix

Protocol abbreviations (only **protocols** count — libraries are tracked separately, so that
"N protocols re-implement this" is not inflated by the libraries that later packaged it):

`TRE` Intersect/Sundae treasury · `AMA` amaru-treasury · `HYD` Hydra · `MID` Midgard ·
`HZA` Hydrozoa · `ADS` AdaStream · `BIN` Binocular · `JPG` JPG Store v3 ·
`113` CIP-113 · `BOD` Bodega · `SWP` Cardano-Swaps · `LEN` Lenfi · `IND` Indigo ·
`BUT` Butane · `FOR` Fortuna

Library abbreviations: `ADP` aiken-design-patterns · `VOD` vodka · `AIC` aicone/`sundae/multisig` ·
`ALL` aiken-linked-list · `MPF` merkle-patricia-forestry · `C3` Charli3 oracle-datum ·
`LPE` liqwid-plutarch-extra

All `LEN` / `IND` / `BUT` / `FOR` marks are cross-checked against the per-repo table in
`raw-lending-cdp.md:1551-1590`; `LEN`, `BUT`, `FOR` are additionally corroborated first-hand in §4b.

| Idiom (canonical name) | Protocols that hand-roll it | N | Already packaged by |
| --- | --- | --- | --- |
| `auth-nft-check` (find/assert UTxO by beacon/thread token) | TRE AMA HYD MID HZA BIN 113 BOD SWP LEN IND BUT FOR | **13** | ADP, VOD, LPE |
| `data-level-compare` (`equalsData` / `serialiseData` instead of field-by-field) | HYD MID HZA ADS BIN JPG 113 BOD SWP LEN IND BUT FOR | **13** | ADP |
| `output-to-self-with-datum` (state-machine step) | TRE HYD MID HZA BIN 113 BOD SWP LEN IND BUT FOR | **12** | ADP |
| `validity-range-check` / `deadline-check` | TRE AMA MID HZA ADS BIN BOD SWP LEN IND BUT FOR | **12** | ADP, VOD, AIC, C3, LPE |
| `value-shape-assert` (single asset apart from ADA, `flatten.length == n`, dust guard) | AMA HYD MID HZA BIN 113 BOD SWP LEN IND BUT FOR | **12** | ADP, VOD, LPE |
| `no-double-satisfaction` (6 mechanisms, §3 + §7.1) | TRE AMA MID HZA BIN JPG 113 BOD SWP LEN IND BUT | **12** | ADP (marker only) |
| `single-script-input` / `exactly-one` | TRE AMA MID HZA BIN 113 SWP LEN IND BUT FOR | **11** | LPE (`phasOnlyOneTokenOfCurrencySymbol`) |
| `input-index-hint` / `output-index-hint` | MID HZA BIN JPG 113 BOD SWP LEN BUT FOR | **10** | ADP |
| `min-ada` handling (ADA may rise, tokens fixed) | TRE MID HZA BIN JPG BOD LEN IND BUT FOR | **10** | — |
| `datum-rebuild-and-compare` / `datum-field-preservation` | MID HZA BIN 113 BOD SWP LEN IND BUT | **9** | — |
| `exact-mint-burn-assert` (only/exactly this asset minted or burnt) | TRE HYD MID 113 BOD SWP LEN BUT FOR | **9** | VOD (4 levels), ADP, ALL |
| `payout-to-address-with-datum` (exact address + datum + value) | TRE MID HZA JPG 113 BOD LEN IND BUT | **9** | — |
| `value-conservation` / `equal-plus-min-ada` / "no extra tokens on the output" | TRE MID HZA 113 BOD SWP LEN IND BUT | **9** | — |
| `config-nft-indirection` / script registry via reference input | TRE AMA MID HZA BIN 113 LEN BUT | **8** | — |
| `one-shot-mint` tied to a consumed `TxOutRef` | TRE HYD MID BIN 113 LEN BUT FOR | **8** | LPE (`withStateThread`) |
| `asset-name-codec` (prefix build/strip, name-as-key, name-as-int) | MID HZA BIN 113 BOD SWP BUT FOR | **8** | VOD (CIP-68), ALL |
| `withdraw-zero-forward` | TRE AMA MID 113 SWP LEN BUT | **7** | ADP, VOD, AIC |
| `merkle-proof` / MPF membership **and** non-membership | MID HZA ADS BIN 113 FOR | **6** | MPF, ADP |
| `spend-forwards-to-mint` (or to-withdraw) | MID 113 SWP LEN BUT FOR | **6** | ADP |
| `protocol-fee-cut` / royalty split | MID JPG BOD LEN IND BUT | **6** | — |
| `oracle-freshness` | BIN BOD LEN IND BUT | **5** | C3 |
| `ratio-math` (collateral ratio, interest, division-free compare) | SWP LEN IND BUT | **4** | LPE (`PRationalNoReduce`) |
| `stake-credential-approves` (pub-key signed **or** stake script ran) | AMA 113 SWP LEN | **4** | — |
| `upgradeable-pointer-script` (indirection so logic can be replaced) | LEN IND BUT FOR | **4** | — |
| `batch-cursor-fold` (monotonic cursor over inputs + terminal "nothing unaccounted") | 113 BOD LEN BUT | **4** | ADP (multi-utxo-indexer) |
| `multisig-script` / `n-of-m-signature-threshold` | TRE AMA HZA LEN | **4** | AIC, VOD (flat form), LPE |
| `sorted-merge-join` on `Value` (containment without `flatten`) | 113 BUT FOR | **3** | LPE (`pelimValue`/`precValue`) |
| `linked-list-node` (insert/remove with `prev < key < next`) | MID HZA 113 | **3** | ADP, ALL |
| `no-reference-script-guard` (fee/DoS grief) | TRE MID HZA | **3** | — |
| `script-hash-recompute` (parameterized-instance proof) | MID 113 | **2** | ADP |
| `monotonic-ratchet` (version/nonce replay guard) | MID HZA | **2** | — |
| `merkelized-validator` (delegated computation) | MID | **1** | ADP |
| `bounded-traversal` (hard size caps so ExUnits cannot brick funds) | TRE FOR | **2** | — |
| `delegated-signature-permit` (meta-transaction: signature over serialised payload) | BUT HZA | **2** | AIC (`satisfied_payload`) |
| `cip68-ref-pair` / label prefixes | *none of the 15* | **0** | VOD only |

Two observations that matter more than any single row:

1. **`cip68-ref-pair` is a library-only idiom.** None of the 15 analyzed protocols implements CIP-68
   reference-token pairing on-chain (`raw-nft-marketplace.md:1435`). It is real, but its demand is
   in minting tooling, not in validators. Rank it low.
2. **Two of the analyzed codebases are already the library we are designing.** Treat their export
   lists as a checklist: `indigo/src/Indigo/Utils/Helpers.hs` (fills 8 of the 24 `find*` grid cells,
   plus `checkOutput`) and all 48 modules of `liqwid-plutarch-extra` (`Value.hs`, `Rational.hs`,
   `StateThread.hs`, `ScriptContext.hs`, `Ord`, `Time`, `Map`, …). Details in
   `raw-lending-cdp.md` §4.
3. **Midgard is the proof that the library approach works.** Midgard does not hand-roll linked
   lists, UTxO indexers or merkelized validators — it *imports* `aiken-design-patterns v1.2.0` and
   `merkle-patricia-forestry 2.0.0` (`raw-l2-rollup.md:26-45`). Hydrozoa, on Scalus, hand-rolls its
   linked list because there is nothing to import. That is the gap in one sentence.

### 7.1 The six anti-double-satisfaction mechanisms, consolidated

| # | Mechanism | Protocols | Cost | Works when |
| --- | --- | --- | --- | --- |
| DS-1 | **Input whitelist**: no script input outside the protocol's own hashes | TRE | one pass over inputs | closed protocol |
| DS-2 | **Exactly one script hash among inputs + all redeemers equal** | AMA | one fold with a `Dict` merge | multi-UTxO, single script |
| DS-3 | **`expect [x] = filter(...)`**: exactly one own input / own output | TRE HZA BIN 113 MID | one filter | per-endpoint |
| DS-4 | **Tag the output with the spent `OutputReference`** — either hashed into a datum (JPG: `InlineDatum(blake2b_256(serialise_data(out_ref)))`) or embedded in the continuing datum (SWP: `prev_input = Some(input_ref)`) | JPG SWP MID BIN | 1 serialise + 1 hash, or 1 datum equality | open protocol, many concurrent spends |
| DS-5 | **Index hints + exact own-input count** (only sound with *both* the ascending-index guard and the count guard) | 113 BOD LEN BUT | O(idx) | batching |
| DS-6 | **Global aggregate accounting**: accumulate an expected mint/fee delta across every action in the transaction, then assert **one** final equality against `tx.mint` — Butane `cdp_script.ak:845`, Indigo `CDP/OnChain.hs:576` (leader/follower: every other CDP input must use redeemer `MergeAuxiliary ownRef`) | BUT IND | one fold, one equality | requires "all logic in one withdraw-0 script" |

DS-4/DS-5 detail and the soundness guards are in `raw-nft-marketplace.md:1486-1526`; DS-1/DS-2/DS-3
are quoted in §3 above. `aiken-design-patterns` deliberately provides **none** of these and instead
forces the caller to pass `double_satisfaction_prevented: Bool` as a proof obligation
(`singular-utxo-indexer.ak:26`).

---

## 8. Ranked API proposals

One line per idiom. `N` is the protocol count from §7. "Status" is measured against
`02-scalus-existing-api.md`: **NEW** = nothing comparable exists; **UPGRADE** = a query-shaped
version exists and needs an assertion-shaped sibling; **EXISTS** = already covered.

| # | N | Idiom | Proposed one-line Scalus API | Status |
| --- | --- | --- | --- | --- |
| 1 | 13 | `auth-nft-check` | `def TxInfo.inputWithToken(policy: PolicyId, name: TokenName): TxInInfo` (+ `.referenceInputWithToken`, `.inputWithPolicy`, `.inputWithTokenPrefix`) — fails unless **exactly one** | **NEW** |
| 2 | 13 | `data-level-compare` | `def Data.sameAs(other: Data): Boolean` documented as one `equalsData`, plus `def Data.hash: ByteString` (`blake2b_256 ∘ serialiseData`) | **UPGRADE** (`.toData ==` works today, undocumented) |
| 3 | 12 | `output-to-self-with-datum` | `def TxInfo.continuingOutput(ownRef: TxOutRef): TxOut` + `def TxOut.mustPreserve(from: TxOut, allowExtraLovelace: Boolean): Unit` | **NEW** |
| 4 | 12 | `validity-range-check` | `enum NormalizedTimeRange { ClosedRange, FromNegInf, ToPosInf, Always, InvalidRange }` + `Interval.normalize`, `Interval.widthAtMost(ms)`, `Interval.contains(other: Interval)` | **UPGRADE** (`isEntirelyAfter/Before` exist) |
| 5 | 12 | `value-shape-assert` | `def Value.singleAssetApartFromAda: (PolicyId, TokenName, BigInt)`, `def Value.lovelaceAndSingleNftOf(policy): (Lovelace, TokenName)`, `def Value.hasNoAssetsBesides(...)` | **NEW** |
| 6 | 12 | `no-double-satisfaction` | five separate members, not one: `TxInfo.mustHaveNoForeignScriptInputs(allowed)`, `TxInfo.expectSingleScriptWithSameRedeemer[R]`, `TxInfo.mustSpendExactlyOneFrom(cred)`, `TxOutRef.asOutputTag: OutputDatum` + `TxInfo.mustHaveTaggedOutput(tag)`, `IndexedInputs` (row 8). DS-6 (aggregate accounting) is an architecture, not a member — it is served by row 24 | **NEW** |
| 7 | 11 | `single-script-input` / `exactly-one` | `def List[A].theOnly(inline msg: String): A` and `def TxInfo.mustSpendExactlyOneFrom(cred: Credential): TxInInfo` | **NEW** |
| 8 | 10 | `input-index-hint` | `def TxInfo.inputAt(i: BigInt, expecting: TxOutRef): TxInInfo`; and `IndexedInputs(indices).forEachPair{…}` which bakes in *ascending-unique* **and** *own-input-count-exhaustive* | **NEW** |
| 9 | 10 | `min-ada` | `def Value.equalPlusMinAda(expected: Value, actual: Value, maxSurplus: Lovelace): Boolean` — three formulations found; adopt Indigo's `checkOutput` (`indigo/.../Utils/Helpers.hs:337`: subtract expected, flatten the residual, allow **only** an ADA surplus in `[0, minAdaTxOut]`) with Butane's cheap variant (`treasury.ak:520`) as the fast path | **NEW** |
| 10 | 9 | `datum-rebuild-and-compare` | `def TxOut.mustHaveInlineDatum[T: ToData](expected: T): Unit` (compiles to one `equalsData`) | **NEW** |
| 11 | 9 | `exact-mint-burn-assert` | `TxInfo.mustMintExactly(policy, name, qty)`, `.mustMintOnly(policy, name, qty)`, `.mustOnlyBurnUnder(policy)`, `.mustNotMintUnder(policy)`, `.mustNotMintOrBurn` — the strictness levels vodka/Butane/Lenfi each found necessary | **NEW** |
| 12 | 9 | `payout-to-address-with-datum` | `def TxInfo.mustPayTo(addr: Address, value: Value, datum: OutputDatum): Unit` + `def TxInfo.valuePaidTo(addr): Value` | **UPGRADE** (`findOwnOutputsByCredential` exists) |
| 13 | 9 | `value-conservation` | `def Value.mustBePreserved(inputs, outputs, minus: Value)` built on row 9, plus `def Value.containsNoAssetsBeyond(other)` | **NEW** |
| 14 | 8 | `config-nft-indirection` | `def TxInfo.referenceDatumWithToken[T: FromData](policy, name): T` — one call replaces the find + inline-datum + decode triple | **NEW** |
| 15 | 8 | `one-shot-mint` | `def TxInfo.mustConsume(outRef: TxOutRef): Unit`, `def TxOutRef.toAssetName: TokenName` (`blake2b_256 ∘ serialiseData`), and a `OneShotPolicy(seed)` validator template | **NEW** |
| 16 | 8 | `asset-name-codec` | `object AssetName { def withPrefix(p, n); def stripPrefix(p, n); def startsWith(p, n); def fromInt(i); def toInt(n) }` | **NEW** |
| 17 | 7 | `withdraw-zero-forward` | `def TxInfo.withdrawalScriptRan(hash: ScriptHash): Boolean`, `.withdrawsExactlyZero(hash)`, `def TxInfo.withdrawRedeemerOf[R: FromData](hash, index): R` | **NEW** |
| 18 | 6 | `merkle-proof` / MPF | keep and document `onchain/plutus/crypto/*`; make sure **non-membership** (`miss`) exists alongside `has`, and publish the MPF cost table next to it | **EXISTS** (verify parity) |
| 19 | 6 | `spend-forwards-to-mint` | `def TxInfo.mintsAnythingUnder(policy): Boolean` + `def TxInfo.mintRedeemerOf[R: FromData](policy, index): R` | **NEW** |
| 20 | 6 | `protocol-fee-cut` | `def TxInfo.mustPayCut(to: Address, of: Value, bps: BigInt, floor: Lovelace)` | **NEW** |
| 21 | 5 | `oracle-freshness` | `def Interval.contains(other: Interval)` + `def Interval.widthAtMost(ms)` (the Butane form, strictly stronger than an expiry compare) and a `PriceFeed` datum shape with Charli3 keys 0/1/2 | **NEW** |
| 22 | 4 | `ratio-math` | `def Rational.compare(a, b)` by cross-multiplication (no division, no `gcd`), `divCeil`, `divFloor`, `percentOf(bps)` | **UPGRADE** (`prelude/Math` exists) |
| 23 | 4 | `stake-credential-approves` | `def Credential.approves(tx: TxInfo): Boolean` (pub-key in `signatories` **or** script hash present in `withdrawals`) | **NEW** |
| 24 | 4 | `batch-cursor-fold` | `def List[TxInInfo].cursorFold[A](start, at: Credential)(step)(finish)` — monotonic cursor plus terminal "nothing left unaccounted" | **NEW** |
| 25 | 4 | `multisig-script` | `enum MultisigScript { Signature, AllOf, AnyOf, AtLeast, Before, After, Script }` + `satisfied(tx)` + `satisfiedPayload(payload, sigs, tx)` — port `sundae/multisig` verbatim; it is already a de-facto standard | **NEW** |
| 26 | 4 | `upgradeable-pointer-script` | documentation pattern + `TxInfo.referenceDatumWithToken` (row 14); no new primitive needed | **DOC** |
| 27 | 3 | `sorted-merge-join` on `Value` | `def Value.containsAtLeast` is already O(n·m); add an O(n+m) sorted-merge variant and document the sortedness precondition | **UPGRADE** |
| 28 | 3 | `linked-list-node` | a `LinkedList` module: `init`/`deinit`/`insertAscending`/`remove`/`proveIsMember`/`proveIsNotMember`, node NFT name = `prefix ++ key`, ordering by `ByteString.compare` | **NEW** |
| 29 | 3 | `no-reference-script-guard` | `def TxInfo.mustHaveNoReferenceScriptsOnOutputs: Unit` | **NEW** |
| 30 | 2 | `script-hash-recompute` | `def ScriptHash.applyParam(version, prefix, param): ScriptHash` (+ 2- and 3-param variants) | **NEW** |
| 31 | 2 | `monotonic-ratchet` | `def mustAdvance(old: BigInt, next: BigInt): Unit` — trivial, but naming it prevents the omission | **NEW** |
| 32 | 2 | `bounded-traversal` | `def List[A].takeAtMostOrFail(n, msg)` — the treasury pattern that stops an attacker bricking funds via ExUnits | **NEW** |
| 33 | 2 | `delegated-signature-permit` | `MultisigScript.satisfiedPayload` (row 25) covers it | **NEW** (folded into 25) |
| 34 | 1 | `merkelized-validator` | `def delegatedCompute[A, B](input: A, staking: ScriptHash, redeemers, i): B` + `def computationWithdrawalWrapper` | **NEW** |
| 35 | 0 | `cip68-ref-pair` | `object Cip68 { val label100/222/333/444; def apply(label, name); def stripLabel(name) }` — cheap and standard, but zero protocol demand in this corpus | **NEW** (low priority) |

### 8.1 The one structural recommendation

`raw-lending-cdp.md:1554` names the real shape of the problem better than any single row:

> **`find*` variants multiply by three axes**: (inputs | reference inputs | outputs) ×
> (by oref | by token | by address | by index) × (`Option` | fail-fast | "exactly one").
> Indigo has 8 of the 24 cells; Lenfi has 5; Butane 4. A stdlib that generates the grid once would
> delete hundreds of lines per protocol.

Scalus currently implements roughly 6 of those 24 cells, all in the `Option`/`List` column
(`02-scalus-existing-api.md` §A.0). **Rows 1, 7, 8, 12 and 14 above are all cells of that same
grid.** Generating the grid systematically — with "exactly one" as a first-class result kind, not an
afterthought — is worth more than any individual helper, and it is what every repo in the corpus
re-derives by hand.

The safety argument for making "exactly one" first-class is stated most clearly by two independent
sources:

- Liqwid distinguishes `phasOnlyOneTokenOfCurrencySymbol` from `phasOneTokenOfCurrencySymbol`
  (`liqwid-plutarch-extra .../Value.hs:575` / `:613`).
- Binocular writes the rule out: *"`filter` then match, NOT `find`: `find` stops at the first and
  would silently accept a second"* (`raw-l2-rollup.md`, Binocular section).

---

## 9. Notes on fit with the existing Scalus surface

Cross-referenced against `02-scalus-existing-api.md`.

**Already exists — do not re-propose:**
`TxInfo.findOwnInput` / `findOwnInputOrFail` / `findOwnScriptOutputs` /
`findOwnInputsByCredential` / `findOwnOutputsByCredential` / `isSignedBy` /
`getValidityStartTime`; `Utils.findInput` / `findScriptOutputs` / `getAdaFromOutputs`;
`OutputDatum.inlineOrFail`; `Interval.contains` / `isEntirelyAfter` / `isEntirelyBefore`;
`Value.quantityOf` / `containsAtLeast` / `hasOnly` / `withoutLovelace` / `tokens` / `flatten` /
`insertCoin` / `policyIds`.

**The structural gap** (also observed in `02-scalus-existing-api.md`): Scalus's high-level layer is
*query*-shaped (`find…: Option[…]`), while every protocol above is *assertion*-shaped
(`expect …`, `must…`, `ensure…`). Almost all the value in this report is in adding an
assertion-shaped layer whose failure messages are useful. Concretely, protocols need
`mustSpendExactlyOne`, `mustPreserve`, `mustPayTo`, `mustMintExactly` — none of which exist today.
