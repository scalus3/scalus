# Raw research: marketplace / order-book / programmable-token validators

Source material for the Scalus "smart contract standard library" design. Everything below is
quoted from real production (or Cardano Foundation reference) on-chain code. Goal: identify what
protocols keep re-implementing by hand.

Repos analysed (all read-only):

| # | Repo | Path | Lang | Plutus |
|---|------|------|------|--------|
| 1 | JPG Store v3 (NFT marketplace) | `/private/tmp/claude-501/-Users-nau-projects-lantr-scalus/bbb8c499-c57d-432b-96c1-767073900d25/scratchpad/repos/jpgstore-v3` | Aiken (stdlib 1.x, `ScriptContext`) | V2 |
| 2 | CIP-113 programmable tokens | `/Users/nau/projects/lantr/cip113-programmable-tokens/src/programmable-tokens-onchain-aiken` + `/Users/nau/projects/lantr/cip113-programmable-tokens/src/substandards` | Aiken (stdlib 2.x) | V3 |
| 3 | Bodega prediction market | `/Users/nau/projects/lantr/bodega-market-smart-contracts` | Aiken (stdlib 2.1) | V3 |
| 4 | Cardano-Swaps (p2p order book) | `/Users/nau/projects/lantr/cardano-swaps` | Aiken (stdlib 1.7) + Haskell off-chain | V2 |

---

## 1. JPG Store v3 (`jpgstore-v3`)

Tiny surface: one spending validator (`ask.ak`, 96 lines) plus a 697-line util module that is
~80 % tests. Everything interesting is payout enforcement + anti-double-satisfaction.

### 1.1 Helpers the protocol defines itself

| Name | Signature | Purpose | file:line |
|---|---|---|---|
| `find_payout_outputs` | `fn(outputs: List<a>, payout_outputs_offset: Int) -> List<a>` | Skip to the first payout output using a redeemer-supplied offset; jumps 10 at a time | `/private/tmp/.../jpgstore-v3/lib/jpg/utils.ak:23` |
| `do_find_payout_outputs` | `fn(outputs: List<a>, current_index: Int) -> List<a>` | Inner 1-at-a-time `tail_list` loop | `.../lib/jpg/utils.ak:36` |
| `skip_10_items` | `fn(some_list: List<a>) -> List<a>` | Ten unrolled `tail_list` calls | `.../lib/jpg/utils.ak:56` |
| `check_marketplace_payout` | `fn(output: Output, marketplace_fee: Int, datum_tag: Datum) -> Bool` | Marketplace-cut output must go to the hardcoded marketplace address, be ADA-only, `>= fee`, and carry the anti-DS tag | `.../lib/jpg/utils.ak:81` |
| `check_payouts` | `fn(outputs: List<Output>, payouts: List<Payout>, datum_tag: Datum) -> Int` | Walks payouts and outputs in lockstep; first output must carry the tag; returns sum of payout amounts | `.../lib/jpg/utils.ak:198` |
| `check_payouts_aux` | `fn(outputs: List<Output>, payouts: List<Payout>) -> Int` | Same as above but requires `NoDatum` (all non-first payout outputs) | `.../lib/jpg/utils.ak:242` |
| `marketplace_addr` | `fn() -> Address` | Rebuilds the hardcoded marketplace address from two hardcoded script hashes (payment + inline stake) | `.../lib/jpg/constants.ak:10` |
| `authorizers` | `fn() -> List<VerificationKeyHash>` | Hardcoded list of "jpg signed this tx" keys (fee-discount escape hatch) | `.../lib/jpg/constants.ak:17` |

Types: `Payout { address: Address, amount_lovelace: Int }` (`lib/jpg/types.ak:10`),
`marketplace_pct = 20`, `min_payout = 1000000` (`lib/jpg/constants.ak:24-26`).

### 1.2 THE anti-double-satisfaction technique: datum tagging with the spent `OutputReference`

This is the canonical case study. The validator hashes the `OutputReference` it is currently
spending and requires the *first* payout output to carry that hash as its inline datum:

```aiken
// validators/ask.ak:42-51
Buy { payout_outputs_offset } -> {
  expect Spend(out_ref) = purpose

  // for double satisfaction
  let datum_tag =
    out_ref
      |> serialise_data
      |> blake2b_256
      |> InlineDatum
```

Enforcement in the payout walker – the *first* output must match the tag, every subsequent one
must have `NoDatum`:

```aiken
// lib/jpg/utils.ak:203-208  (check_payouts)
expect [first_output, ..rest_outputs] = outputs
let Output { address: output_address, value, datum, .. } = first_output
expect datum == datum_tag
expect [payout, ..rest_payouts] = payouts
```

```aiken
// lib/jpg/utils.ak:243-247  (check_payouts_aux)
expect [first_output, ..rest_outputs] = outputs
let Output { address: output_address, value, datum, .. } = first_output
expect datum == NoDatum
```

**Mechanism.** Every spent listing UTxO produces a *distinct* `datum_tag` (its own `OutputReference`
is unique by construction). A payout output can satisfy at most one script execution, because a
given output has exactly one datum, and the untagged (`NoDatum`) rest-outputs are *positionally*
bound to the tagged head via `find_payout_outputs(offset)`. So two listings cannot both point at
the same seller-payout output. This is the "tag an output with the spent OutputReference" family.

Off-chain, the tag is reconstructed byte-identically:

```typescript
// e2e/main.ts:93-95
const datumTag = Data.to(toHex(C.hash_blake2b256(fromHex(Data.to(
  new Constr(0, [new Constr(0, [tx.toHash()]), BigInt(0)]),
)))));
```

Note this is a *serialised-Data* level trick: `serialise_data` + `blake2b_256` are direct builtins,
giving a 32-byte tag independent of the datum schema.

### 1.3 Payout / royalty / marketplace-fee enforcement

Datum-driven payout list, output order must match payout order, `>=` not `==` (over-payment ok):

```aiken
// lib/jpg/utils.ak:210-227
let Payout { address: payout_address, amount_lovelace } = payout
// The `Output` address must match the address specified in the corresponding payout
expect payout_address == output_address
expect [(policy, tokens)] =
  value
    |> to_dict
    |> dict.to_list
expect [(_, quantity)] = dict.to_list(tokens)
expect policy == ada_policy_id
expect quantity >= amount_lovelace && amount_lovelace > 0
```

The `expect [(policy, tokens)] = ...` + `expect [(_, quantity)] = ...` idiom is a *cheap ADA-only
check*: instead of `value.lovelace_of` plus a separate "no other tokens" scan, they pattern-match
the value's dict to exactly one policy with exactly one asset name. This enforces "ADA-only output"
and reads the quantity in one shot.

Marketplace cut is *derived*, not stored:

```aiken
// validators/ask.ak:70-84
expect [marketplace_output, ..rest_outputs] = payout_outputs
let payouts_sum = check_payouts(rest_outputs, payouts, NoDatum)
// This approximates the marketplace fee given only the payouts to a very high degree.
// For a payouts in excess of 100k ada the error is less than 40000 lovelace.
let marketplace_fee = payouts_sum * 50 / 49 / 50
check_marketplace_payout(marketplace_output, marketplace_fee, datum_tag)
```

Fee-discount escape hatch (protocol-signed transactions skip the fee derivation entirely):

```aiken
// validators/ask.ak:59-66
let can_have_discount =
  constants.authorizers()
    |> list.any(fn(authorizer) { list.has(extra_signatories, authorizer) })
if can_have_discount {
  check_payouts(payout_outputs, payouts, datum_tag) > 0
} else { ... }
```

Note the tag placement swaps: with a discount the tag sits on the *first payout*, without it the
tag sits on the *marketplace output* (`check_payouts(rest_outputs, payouts, NoDatum)`).

Hardcoded destination address rebuilt on-chain from two script hashes:

```aiken
// lib/jpg/constants.ak:10-15
pub fn marketplace_addr() -> Address {
  Address {
    payment_credential: ScriptCredential(marketplace_sh),
    stake_credential: Some(Inline(ScriptCredential(marketplace_stake_sh))),
  }
}
```

### 1.4 Efficiency tricks

- **Redeemer index hint with a coarse "skip 10" fast path.** The doc comment is the design rationale:

```aiken
// lib/jpg/utils.ak:12-33
/// We use a heuristic function combined with the given
/// redeemer offset to jump 10 at a time towards first output to validate.
/// This saves a massive amount in budget allowing us to support 40+ inputs.
/// If the wrong offset is given then the script will fail or cost more.
/// ... This is pretty much `list.drop` but optimized for our situation.
pub fn find_payout_outputs(outputs: List<a>, payout_outputs_offset: Int) -> List<a> {
  if payout_outputs_offset >= 10 {
    outputs |> skip_10_items |> find_payout_outputs(payout_outputs_offset - 10)
  } else {
    do_find_payout_outputs(outputs, payout_outputs_offset)
  }
}
```

  `skip_10_items` is a hand-unrolled chain of 10 `builtin.tail_list` calls (`utils.ak:56-68`) –
  it avoids the per-iteration integer compare + branch of a naive `drop`.
- **Direct builtins**: `builtin.tail_list`, `builtin.serialise_data`, `builtin.blake2b_256`.
- **Single-pass lockstep walk**: `check_payouts`/`check_payouts_aux` consume the output list and the
  payout list simultaneously, returning the accumulated `Int` sum – no second traversal to total the
  payouts.
- **Positional coupling instead of search**: no `list.find` over outputs anywhere. Everything is
  offset + head/tail. The README documents the resulting constraint: outputs must not alternate
  (`[payout, random, payout]` is rejected; `[random, payout, payout, random]` is fine).
- **Budget claim**: "Ask bulk-purchase currently maxes out at **54** assets in a single transaction"
  (`README.md:21`).

### 1.5 Idioms present

`tag-with-txoutref`, `no-double-satisfaction`, `payout-to-address-with-datum`,
`protocol-fee-cut`, `royalty-split`, `min-ada` (`min_payout` constant),
`output-index-hint`, `data-level-compare` (serialise+hash), `owner-signature-cancel`,
`hardcoded-address-build`, `ada-only-value-check`.

Notably **absent**: no minting policy, no beacon/thread token, no CIP-68, no validity range.

---

## 2. CIP-113 programmable tokens (Cardano Foundation)

Two sub-repos: the core (`programmable-tokens-onchain-aiken`, Plutus V3, migrated from Plutarch)
and `substandards/freeze-and-seize` (blacklist / seizure substandard). This is by far the most
idiom-dense repo of the four.

### 2.1 Core helper library (`lib/utils.ak`)

| Name | Signature | Purpose | file:line |
|---|---|---|---|
| `RawValue` | `type = Dict<PolicyId, Dict<AssetName, Int>>` | Un-wrapped Value used for cheap arithmetic | `lib/utils.ak:13` |
| `expect_inline_datum` | `fn(output: Output) -> Data` | Unwrap `InlineDatum` or fail | `lib/utils.ak:17` |
| `find_input` / `must_find_input` | `fn(List<Input>, fn(Input)->Bool) -> Option<Input>` / `-> Input` | Predicate input lookup | `lib/utils.ak:23`, `:31` |
| `has_currency_symbol` | `fn(haystack: Value, needle: PolicyId) -> Bool` | Policy presence, **skipping the always-first ADA pair** | `lib/utils.ak:42` |
| `count_unique_tokens` | `fn(value: Value) -> Int` | Distinct (policy, name) count via `assets.reduce` (no `flatten` allocation) | `lib/utils.ak:60` |
| `has_signatory` | `fn(List<VerificationKeyHash>, VerificationKeyHash) -> Bool` | Signature check via raw `head_list`/`tail_list`, fails loudly on empty | `lib/utils.ak:65` |
| `elem_at` | `fn(lst: List<a>, idx: Int) -> a` | Index into a list, **two `tail_list` per iteration** | `lib/utils.ak:73` |
| `bytearray_lt` | `fn(a, b: ByteArray) -> Bool` | `builtin.less_than_bytearray` | `lib/utils.ak:84` |
| `apply_hashed_parameter` | `fn(prefix, postfix, hashed_param: ByteArray) -> ByteArray` | **Recompute a parameterised script's policy id on-chain** | `lib/utils.ak:91` |
| `is_programmable_token_registration` | `fn(cs, prefix, postfix, hashed_param, mint_value) -> Bool` | Registration proof: policy is minting AND computed cs matches | `lib/utils.ak:117` |
| `has_key` | `fn(List<Pair<Credential, a>>, Credential) -> Bool` | Withdrawal-map membership (withdraw-zero check) | `lib/utils.ak:142` |
| `sum_output_values` | `fn(List<i>, strategy: fn(i, select, discard) -> RawValue) -> RawValue` | CPS single-pass "filter and sum values" | `lib/utils.ak:168` |
| `merge_raw_values` | `fn(RawValue, RawValue) -> RawValue` | Nested `dict.union_with` sum | `lib/utils.ak:203` |
| `match_assets` | `fn(RawValue, RawValue) -> Bool` | Structural equality ignoring lovelace | `lib/utils.ak:214` |
| `value_contains` | `fn(superset, subset: Pairs<PolicyId, Dict<AssetName,Int>>) -> Bool` | Sorted-merge containment, O(n+m) | `lib/utils.ak:241` |
| `token_contains` | `fn(Pairs<AssetName,Int>, Pairs<AssetName,Int>) -> Bool` | Inner sorted-merge | `lib/utils.ak:260` |
| `drop_until` | `fn(Pairs<ByteArray,v>, until, return) -> result` | CPS "advance sorted list to key" | `lib/utils.ak:276` |

Linked-list library (`lib/linked_list.ak`):

| Name | Signature | Purpose | file:line |
|---|---|---|---|
| `validate_directory_node_output` | `fn(output: Output, node_cs: PolicyId) -> RegistryNode` | Node output must hold exactly ADA + 1 node NFT whose name == node key, ordered `key < next` | `lib/linked_list.ak:17` |
| `collect_node_ios` | `fn(tx: Transaction, node_cs) -> (List<Input>, List<Output>)` | Gather node inputs/outputs and require all node outputs share the first node input's address | `lib/linked_list.ak:47` |
| `validate_directory_init` | `fn(node_inputs, node_outputs, mint, node_cs) -> Bool` | Origin node: `key == ""`, `next == 0xff…ff`, mint exactly the origin NFT | `lib/linked_list.ak:81` |
| `is_inserted_directory_node` | `fn(node, insert_key, next_key) -> Bool` | New node shape | `lib/linked_list.ak:106` |
| `is_updated_directory_node` | `fn(node, original, insert_key) -> Bool` | Covering node re-issue, **all other fields must be preserved** | `lib/linked_list.ak:144` |

`substandards/freeze-and-seize/lib/utils.ak` mostly duplicates the above but adds:
`value_contains` / `value_contains_v2` / `expect_value_contains_v3` (three tries at the same check,
`:12`, `:23`, `:34`), `outputs_at_credential` (`:85`), `inputs_from_credential` (`:93`),
`count_inputs_from_credential` (`:104`), `is_signed_by` (`:112`), `is_script_invoked` (`:117`).
The fact that this file exists twice, near-identically, is itself a stdlib signal.

### 2.2 withdraw-zero forwarding (the dominant architecture here)

Spending validator that does nothing but check a stake script ran:

```aiken
// validators/programmable_logic_base.ak:10-23
validator programmable_logic_base(stake_cred: Credential) {
  spend(_datum: Option<Data>, _redeemer: Data, _own_ref: Data, self: Transaction) {
    // The programmable logic base validator simply checks that the global
    // programmable logic stake script is invoked in the transaction via the
    // withdraw-zero pattern Check that the stake credential is invoked
    self.withdrawals |> has_key(stake_cred)
  }
  else(_) { fail }
}
```

The check itself, hand-written with raw builtins (fails loudly on empty list rather than returning
`False`):

```aiken
// lib/utils.ak:142-144
pub fn has_key(haystack: List<Pair<Credential, a>>, needle: Credential) -> Bool {
  head_list(haystack).1st == needle || has_key(tail_list(haystack), needle)
}
```

Three more instances:
- per-token transfer logic must be invoked: `has_key(self.withdrawals, transfer_logic_script)`
  (`validators/programmable_logic_global.ak:456`)
- third-party (seizure) logic: `has_key(tx.withdrawals, registry_node.third_party_transfer_logic_script)`
  (`validators/programmable_logic_global.ak:215`)
- minting policy forwards to a withdraw script: `list.has(invoked_scripts, minting_logic_cred)`
  (`validators/issuance_mint.ak:71`)

And the reverse direction – **spend forwards to mint**:

```aiken
// substandards/freeze-and-seize/validators/blacklist_spend.ak:14-26
validator blacklist_spend(blacklist_cs: PolicyId) {
  spend(_datum: Option<Data>, _redeemer: Data, _own_ref: Data, self: Transaction) {
    // Check that the blacklist currency symbol appears in the mint field
    // This ensures the minting policy is being executed, which contains
    // the actual validation logic for blacklist operations
    to_dict(self.mint) |> has_key(blacklist_cs)
  }
  else(_) { fail }
}
```

```aiken
// validators/registry_spend.ak:41-51
let minting_registry_nodes =
  list.any(
    flatten(self.mint),
    fn(asset) {
      let (policy, _tn, amt) = asset
      policy == registry_node_cs && amt != 0
    },
  )
minting_registry_nodes?
```

A withdraw script also self-checks that it really is a rewarding execution:

```aiken
// substandards/freeze-and-seize/validators/example_transfer_logic.ak:115-129
/// Check if this is a rewarding script (withdraw-zero trick)
fn is_rewarding_script(redeemers, account: Credential) -> Bool {
  list.any(redeemers, fn(pair) {
    let Pair(purpose, _redeemer) = pair
    when purpose is {
      Withdraw(cred) -> cred == account
      _ -> False
    }
  })
}
```

CIP-113's global validator is withdraw-only (`validators/programmable_logic_global.ak:21-26`), so
the "one script body, mint *or* withdraw purpose" variant does not appear here. Cardano-Swaps has
it – see §4.4.

### 2.3 One-shot mint tied to consuming a specific `TxOutRef`

Three separate copies of the same eight lines:

```aiken
// validators/protocol_params_mint.ak:22-24
// Check 1: This is a one-shot minting policy - must spend the specified UTXO
let consumed =
  list.any(self.inputs, fn(input) { input.output_reference == utxo_ref })
```

```aiken
// validators/issuance_cbor_hex_mint.ak:18-20
// Must consume the one-shot UTxO
let consumed =
  list.any(self.inputs, fn(input) { input.output_reference == utxo_ref })
```

```aiken
// validators/registry_mint.ak:33-38
RegistryInit -> {
  // Ensure this is a one-shot minting policy by checking that utxo_ref is spent
  let is_utxo_consumed =
    list.any(self.inputs, fn(input) { input.output_reference == utxo_ref })
```

```aiken
// substandards/freeze-and-seize/validators/blacklist_mint.ak:27-32
BlacklistInit -> {
  expect
    list.any(self.inputs, fn(input) { input.output_reference == utxo_ref })
```

Each is paired with an "exactly one token, exactly this name, exactly qty 1, locked at the
always-fail address with a well-typed inline datum" block:

```aiken
// validators/protocol_params_mint.ak:26-62
let minted_tokens = flatten(self.mint)
let own_minted = list.filter(minted_tokens, fn(token) { let (cs,_,_) = token; cs == own_policy })
expect [(_, tn, qty)] = own_minted
let expected_address = address.from_script(always_fail_hash)
expect Some(nft_output) =
  list.find(self.outputs, fn(output) {
    assets.has_nft_strict(output.value, own_policy, protocol_params_token)
  })
expect InlineDatum(datum) = nft_output.datum
expect _params: ProgrammableLogicGlobalParams = datum
and {
  consumed?,
  (tn == protocol_params_token)?,
  (qty == 1)?,
  (nft_output.address == expected_address)?,
}
```

### 2.4 On-chain sorted linked list (registry / blacklist) with membership + non-membership proofs

**Non-membership proof** – redeemer names a reference input holding the *covering* node, and the
script checks `key < needle < next`:

```aiken
// validators/programmable_logic_global.ak:459-465
TokenDoesNotExist { .. } -> {
  // Validate the node covers the currency symbol (nodeKey < cs < nodeNext)
  expect bytearray_lt(key, cs)
  expect bytearray_lt(cs, next)
  // This is NOT a programmable token (proof is valid)
  False
}
```

**Membership proof** – node key equals the needle, and the node's transfer-logic script must run:

```aiken
// validators/programmable_logic_global.ak:451-457
TokenExists { .. } -> {
  // Validate the node's key matches the currency symbol
  expect key == cs
  // Validate the transfer logic script is invoked
  has_key(self.withdrawals, transfer_logic_script)
}
```

**Node authenticity** is by NFT presence, and the node NFT's *asset name is the node key*:

```aiken
// lib/linked_list.ak:24-41
// Must have exactly 2 unique tokens (Ada + node token)
expect count_unique_tokens(value) == 2
let node_tokens = tokens(value, node_cs)
expect dict.size(node_tokens) == 1
expect node: RegistryNode = datum
// Node must be ordered: key < next
expect bytearray_lt(node.key, node.next)
let token_pairs = dict.to_pairs(node_tokens)
expect [Pair(tn, qty)] = token_pairs
expect qty == 1
expect tn == node.key || tn == origin_node_tn
```

Sentinel origin node:

```aiken
// lib/linked_list.ak:94-101
(node.key == #"")?,
(node.next == #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")?,
...
assets.has_nft(mint, node_cs, origin_node_tn)?,
```

**Insert** = 1 node input, 2 node outputs, 1 NFT minted, covering node's other fields preserved:

```aiken
// validators/registry_mint.ak:112-124
and {
  is_token_registered?,
  (bytearray.length(key) == 28)?,
  bytearray_lt(covering_node.key, key)?,
  bytearray_lt(key, covering_node.next)?,
  just_single_mint?,
  (list.length(output_nodes) == 2)?,
  registry_node_updated?,
  registry_node_inserted?,
}
```

```aiken
// lib/linked_list.ak:138-155
/// Fix for NEW-1 — matches Plutarch's pisInsertedOnNode strict equality.
pub fn is_updated_directory_node(node, original, insert_key) -> Bool {
  and {
    (node.key == original.key)?,
    (node.next == insert_key)?,
    (node.transfer_logic_script == original.transfer_logic_script)?,
    (node.third_party_transfer_logic_script == original.third_party_transfer_logic_script)?,
    (node.global_state_cs == original.global_state_cs)?,
  }
}
```

**Remove** = 2 node inputs, 1 node output, 1 NFT burned, merge the links:

```aiken
// substandards/freeze-and-seize/validators/blacklist_mint.ak:116-130
let just_single_burn = assets.quantity_of(self.mint, policy_id, key) == -1
let checks =
  if node_a.key == key {
    output_node.key == node_b.key && output_node.next == node_a.next && node_b.next == key
  } else if node_b.key == key {
    output_node.key == node_a.key && output_node.next == node_b.next && node_a.next == key
  } else {
    fail @"Neither input matches the key to remove"
  }
```

The blacklist substandard uses the same structure to prove a *sender* is not sanctioned:

```aiken
// substandards/freeze-and-seize/validators/example_transfer_logic.ak:98-111
let datum_data = utils.expect_inline_datum(node_output)
expect node: types.BlacklistNode = datum_data
// Verify non-membership: nodeKey < witnessKey < nodeNext
// This proves the witness is NOT in the blacklist
builtin.less_than_bytearray(node.key, witness) && builtin.less_than_bytearray(witness, node.next)
...
// Zip witnesses with proofs and validate each pair
list.map2(witnesses, proofs, validate_pair) |> list.all(fn(x) { x })
```

Witnesses are derived from the *stake* part of every programmable-token input:

```aiken
// substandards/freeze-and-seize/validators/example_transfer_logic.ak:43-65
fn extract_required_witnesses(inputs: List<Input>, prog_base_cred: Credential) -> List<ByteArray> {
  list.foldr(inputs, [], fn(input, acc) {
    if input.output.address.payment_credential == prog_base_cred {
      when input.output.address.stake_credential is {
        Some(Inline(cred)) ->
          when cred is {
            VerificationKey(pkh) -> [pkh, ..acc]
            Script(script_hash) -> [script_hash, ..acc]
          }
        _ -> acc
      }
    } else { acc }
  })
}
```

### 2.5 Ownership via the stake credential (signature OR withdraw script)

```aiken
// validators/programmable_logic_global.ak:246-270
fn get_signed_prog_value(tx: Transaction, prog_logic_cred: Credential) -> RawValue {
  sum_output_values(tx.inputs, fn(input, select, discard) {
    let output = input.output
    if output.address.payment_credential == prog_logic_cred {
      expect Some(Inline(stake_cred)) = output.address.stake_credential
      when stake_cred is {
        VerificationKey(pkh) -> { expect has_signatory(tx.extra_signatories, pkh) }
        Script(_hash)        -> { expect has_key(tx.withdrawals, stake_cred) }
      }
      select(output)
    } else { discard() }
  })
}
```

This is exactly Cardano-Swaps' `staking_credential_approves` (§4.1) with a different failure mode:
CIP-113 *fails*, Cardano-Swaps *returns True* when no stake credential is present.

### 2.6 Index-hint redeemers made sound

`ThirdPartyAct` passes `input_idxs: List<Int>`, `outputs_start_idx: Int` and `length_input_idxs: Int`
(`lib/types.ak:61-70`). Two checks make this safe, and they are the spec for any stdlib
"indexed inputs" helper:

**(a) strictly decreasing indices** – prevents an attacker listing the same input twice to
double-count it:

```aiken
// validators/programmable_logic_global.ak:168-178
list.indexed_foldr(input_idxs, (-1, 0, dict.empty),
  fn(idx, input_idx, (prev_idx, acc, input_tokens_acc)) {
    // Ensures inputs are sorted with no duplicates
    expect prev_idx > input_idx || prev_idx == -1

    let input = elem_at(tx.inputs, input_idx)
    let output_idx = outputs_start_idx + idx
    let output = elem_at(tx.outputs, output_idx)
```

**(b) exact count of own-script inputs** – prevents an attacker sneaking in an *unlisted* script
input that the loop never inspects:

```aiken
// validators/programmable_logic_global.ak:208-223
// Verify length parameter matches actual list length
expect input_size == length_input_idxs
...
// Verify that ONLY the specified inputs from prog_logic_cred are consumed
let prog_input_count =
  list.count(tx.inputs, fn(input) {
    input.output.address.payment_credential == prog_logic_cred
  })
expect prog_input_count == length_input_idxs
```

Plus per-pair continuity (address + datum preserved, non-target policies untouched, and a
"something must actually change" DoS guard):

```aiken
// validators/programmable_logic_global.ak:184-199
expect input.output.address.payment_credential == prog_logic_cred
// Output must preserve address and datum
expect output.address == input.output.address
expect output.datum == input.output.datum

// Non-policy assets must be exactly equal (only the seized policy can change)
expect dict.delete(input_dict, policy_id) == dict.delete(output_dict, policy_id)

// Policy tokens must actually change (prevent DDOS with no-op seizures)
let input_policy_tokens = get_policy_tokens(input_dict, policy_id)
let output_policy_tokens = get_policy_tokens(output_dict, policy_id)
expect input_policy_tokens != output_policy_tokens
```

### 2.7 Efficiency tricks (richest set of the four repos)

**a) CPS filter-and-sum over inputs or outputs in one pass**, with a caller-supplied select/discard
strategy so the same loop serves inputs and outputs:

```aiken
// lib/utils.ak:146-199
/// A faster version of assets.merge that is specialized to positive values
/// (typically found in outputs). This allows to bypass the null check on value
/// since quantities can only ever increase.
pub fn sum_output_values(
  elems: List<i>,
  strategy: fn(i, fn(Output) -> RawValue, fn() -> RawValue) -> RawValue,
) -> RawValue {
  do_sum_output_values(elems, strategy, dict.empty)
}
```

**b) Sorted-merge value containment, O(n+m), no `flatten`**, with the pre-condition written down:

```aiken
// lib/utils.ak:230-258
/// A fast check for inclusion of a value inside another. ...
/// /!\ PRE-CONDITION /!\
/// Given pairs are assumed to be well-formed and semantically 'correct'. That
/// is, keys contain no duplicates and are in ascending order.
pub fn value_contains(superset, subset) -> Bool {
  when subset is {
    [] -> True
    [head_subset, ..tail_subset] -> {
      let head_superset, tail_superset <- drop_until(superset, head_subset.1st)
      and {
        token_contains(dict.to_pairs(head_superset), dict.to_pairs(head_subset.2nd)),
        value_contains(tail_subset, tail_superset),
      }
    }
  }
}
```

Call site documents why the pre-condition holds:

```aiken
// validators/programmable_logic_global.ak:128-132
// The output must contain all the expected programmable tokens (inputs + mints)
// Pre-condition of 'value_contains' is guaranteed since both pairs
// come from 'Dict', thus guaranteeing key uniqueness and ordering.
dict.to_pairs(output_prog_value) |> utils.value_contains(dict.to_pairs(expected_value))
```

**c) Skip the always-first ADA entry** rather than filtering:

```aiken
// lib/utils.ak:42-46
pub fn has_currency_symbol(haystack: Value, needle: PolicyId) -> Bool {
  // Fast skip first pair which is always ada
  let haystack = haystack |> assets.to_dict |> dict.to_pairs |> tail_list
  do_has_currency_symbol(haystack, needle)
}
```

```aiken
// validators/programmable_logic_global.ak:283
let assets = tail_list(dict.to_pairs(value))
```

```aiken
// lib/utils.ak:214-222  (match_assets)
let without_lovelace = fn(pairs) {
  if head_list(pairs).1st == ada_policy_id { tail_list(pairs) } else { pairs }
}
```

**d) Partial, schema-free datum decoding with `unconstr_fields`** – decode only the prefix of fields
you need, and say why:

```aiken
// validators/programmable_logic_global.ak:469-488
/// Extract useful information from an inlined 'RegistryNode' datum. Note that
/// we need not to 'validate' the full shape of the datum here, because we do not
/// produce it. ... This even makes the contract more 'interoperable', as the linked-list datum
/// can change in addititive manner without invalidating this contract.
fn expect_registry_node(datum: Datum, return: fn(ByteArray, ByteArray, Credential) -> result) -> result {
  expect InlineDatum(inline_datum) = datum
  let fields = unconstr_fields(inline_datum)
  expect key: ByteArray = head_list(fields)
  let fields = tail_list(fields)
  expect next: ByteArray = head_list(fields)
  let fields = tail_list(fields)
  expect transfer_logic_script: Credential = head_list(fields)
  return(key, next, transfer_logic_script)
}
```

Same trick to pull one `Int` out of a redeemer without decoding the constructor:

```aiken
// validators/programmable_logic_global.ak:438
expect node_idx: Int = head_list(unconstr_fields(proof))
```

**e) `elem_at` unrolled two steps at a time** (`lib/utils.ak:73-81`), same motivation as
JPG's `skip_10_items`.

**f) Recompute a parameterised script's hash on-chain** so a registry entry proves its own policy id:

```aiken
// lib/utils.ak:91-111
pub fn apply_hashed_parameter(prefix, postfix, hashed_param: ByteArray) -> ByteArray {
  // Plutus V3 version header: most significant first encoding of version 3
  let version_header = #"03"
  let script_bytes =
    builtin.append_bytearray(version_header,
      builtin.append_bytearray(prefix, builtin.append_bytearray(hashed_param, postfix)))
  builtin.blake2b_224(script_bytes)
}
```

**g) Redeemer-map introspection to forbid concurrent duplicate mints**:

```aiken
// validators/issuance_mint.ak:94-119
fn single_mint_with_credential(redeemers, target_redeemer) -> Bool {
  let target_redeemer_data: Data = target_redeemer
  let matching_count =
    list.foldl(redeemers, 0, fn(pair, count) {
      let Pair(purpose, rdmr) = pair
      when purpose is {
        Mint(_) -> if rdmr == target_redeemer_data { count + 1 } else { count }
        _ -> count
      }
    })
  matching_count == 1
}
```

Note the comparison `rdmr == target_redeemer_data` is a **raw `Data` equality**, avoiding a decode.

**h) `count_unique_tokens` via `assets.reduce` instead of `flatten |> length`** – the discarded
version is left in a comment in the substandard copy (`substandards/.../lib/utils.ak:78-82`).

### 2.8 Idioms present

`withdraw-zero-forward`, `spend-forwards-to-mint`, `mint-forwards-to-withdraw`,
`auth-nft-check` (registry/params/blacklist node NFTs), `one-shot-mint` (x4),
`onchain-linked-list` (insert/remove/init), `non-membership-proof`, `membership-proof`,
`input-index-hint` + `output-index-hint`, `sorted-index-list-guard`, `own-input-count-exact`,
`output-to-self-with-datum` (address + datum preserved), `stake-credential-approves`,
`script-hash-recompute`, `data-level-compare` (`unconstr_fields`, raw `Data` ==),
`value-contains-sorted`, `single-pass-value-sum`, `nft-name-is-key`,
`always-fail-address-lock`, `no-extraneous-assets`, `balance-invariant`.

**Not present:** CIP-68 label prefixes, validity-interval/deadline logic, explicit min-ADA handling,
protocol fee cut, `tag-with-txoutref`.

---

## 3. Bodega prediction market

Three validators. The distinguishing feature is that **every** redeemer is a bag of list indices,
and cross-validator authentication is entirely "is the market NFT in input #n".

### 3.1 Helpers

| Name | Signature | Purpose | file:line |
|---|---|---|---|
| `find_posix_time_range` | `fn(time_rage: ValidityRange) -> (Int, Int)` | Both bounds must be `Finite`; returns `(lo, hi)` | `lib/bodega/utils.ak:15` |
| `find_license_deadline` | `fn(license_symbol: PolicyId, val: Value) -> Int` | Batcher license NFT: exactly one token of that policy, qty 1, **asset name is an ASCII deadline** | `lib/bodega/utils.ak:21` |
| `bytearray_to_int` | `fn(byte_arr: ByteArray) -> Int` | ASCII decimal (with `-`) to Int | `lib/bodega/utils.ak:30` |
| `do_bytearray_to_int` | `fn(ByteArray, idx, acc, byte_len) -> Int` | Tail-recursive accumulator over `builtin.index_bytearray` | `lib/bodega/utils.ak:35` |
| `correct_own_output_datum` | `fn(in_datum, out_datum: PredictionDatum) -> Bool` | Whitelist of immutable datum fields that must survive a step | `validators/predictions.ak:405` |

Redeemer index hints (`lib/bodega/types.ak:22-24`, `:57-76`):

```aiken
pub type PositionRedeemer { pred_in_idx: Int }

pub type PredictionRedeemer {
  PredApply { own_input_idx: Int, own_output_idx: Int, license_idx: Int, pos_indices: List<(Int, Int)> }
  PredReward { own_input_idx: Int, own_output_idx: Int, license_idx: Int, pos_indices: List<(Int, Int)> }
  PredWithdrawAdminFee { own_input_idx: Int, own_output_idx: Int, license_idx: Int, treasury_out_idx: Int }
}
```

### 3.2 Auth-NFT delegation (a whole validator that is one NFT check)

```aiken
// validators/positions.ak:18-32
validator positions(prediction_nft: Asset) {
  spend(_datum, redeemer: PositionRedeemer, _own_ref, self: Transaction) {
    let Transaction { inputs, .. } = self
    expect Some(pred_input) = list.at(inputs, redeemer.pred_in_idx)
    assets.quantity_of(
      pred_input.output.value,
      prediction_nft.policy_id,
      prediction_nft.asset_name,
    ) == 1
  }
  else(_) { fail }
}
```

Same pattern in the minting policy's burn path:

```aiken
// validators/mint_shares.ak:100-107
Reward { pred_in_idx } -> {
  expect Some(pred_input) = list.at(inputs, pred_in_idx)
  assets.quantity_of(pred_input.output.value,
    params.prediction_nft.policy_id, params.prediction_nft.asset_name) == 1
}
```

And on the *reference* side for the buy path:

```aiken
// validators/mint_shares.ak:42-51
Buy { pred_ref_idx } -> {
  expect Some(pred_ref_input) = list.at(reference_inputs, pred_ref_idx)
  expect
    assets.quantity_of(pred_ref_input.output.value,
      params.prediction_nft.policy_id, params.prediction_nft.asset_name) == 1
  expect InlineDatum(pred_output_datum) = pred_ref_input.output.datum
  expect pred_datum: PredictionDatum = pred_output_datum
```

The oracle is a *reference input located by NFT search* rather than by index:

```aiken
// validators/predictions.ak:217-229
expect Some(oracle_ref_input) =
  reference_inputs
    |> list.find(fn(input) {
        assets.quantity_of(input.output.value,
          params.oracle_nft.policy_id, params.oracle_nft.asset_name) == 1
      })
expect InlineDatum(oracle_inline_datum) = oracle_ref_input.output.datum
expect oracle_datum: OracleDatum = oracle_inline_datum
```

### 3.3 State-machine step: own input and own output identified by NFT + index

```aiken
// validators/predictions.ak:46-61
expect Some(own_input) = list.at(inputs, own_input_idx)
expect Some(own_output) = list.at(outputs, own_output_idx)
expect InlineDatum(own_output_inline_datum) = own_output.datum
expect own_output_datum: PredictionDatum = own_output_inline_datum
expect
  assets.quantity_of(own_input.output.value,
    params.prediction_nft.policy_id, params.prediction_nft.asset_name) == 1
expect
  assets.quantity_of(own_output.value,
    params.prediction_nft.policy_id, params.prediction_nft.asset_name) == 1
```

The address-continuity check and the immutable-fields check close the step:

```aiken
// validators/predictions.ak:180
correct_own_output_datum(own_input_datum, own_output_datum)
  && own_input.output.address == own_output.address
  && are_correct_positions
```

```aiken
// validators/predictions.ak:405-410
fn correct_own_output_datum(in_datum, out_datum: PredictionDatum) -> Bool {
  in_datum.true_position_name == out_datum.true_position_name
  && in_datum.false_position_name == out_datum.false_position_name
  && in_datum.dead_line == out_datum.dead_line
  && in_datum.position_script_hash == out_datum.position_script_hash
  && in_datum.admin_fee == out_datum.admin_fee
  && in_datum.envelope_amount == out_datum.envelope_amount
}
```

Note: `_own_ref` is **ignored** (`predictions.ak:28`). Authentication rests entirely on the market
NFT being in `inputs[own_input_idx]`, which is sound only because that NFT is unique.

### 3.4 Batch processing: `List<(Int, Int)>` input/output index pairs

```aiken
// validators/predictions.ak:62-72
let (true_shares, false_shares, batch_size, are_correct_positions) =
  list.foldl(pos_indices, (0, 0, 0, True),
    fn(curr, acc) {
      let (in_idx, out_idx) = curr
      let (num_true, num_false, bs, are_correct) = acc
      expect Some(in) = list.at(inputs, in_idx)
      expect Some(out) = list.at(outputs, out_idx)
      expect InlineDatum(pos_inline_datum) = in.output.datum
      expect pos_datum: PositionDatum = pos_inline_datum
```

Unlike CIP-113, **Bodega does NOT check that the index pairs are sorted or that the batch covers
every script input**. The single-pass fold accumulates `true_shares`, `false_shares`, `batch_size`
and a running `Bool` in one tuple – a nice pattern, but the missing sorted/exhaustive guards from
§2.6 are exactly what a stdlib helper should supply.

### 3.5 "Must send value to address with datum", rebuilding the address from the datum

```aiken
// validators/predictions.ak:73-80
let user_address =
  when pos_datum.pos_user_stake_key is {
    Some(key) ->
      address.from_verification_key(pos_datum.pos_user_pkh)
        |> address.with_delegation_key(key)
    None -> address.from_verification_key(pos_datum.pos_user_pkh)
  }
expect user_address == out.address
```

Exact output-value assertion via `flatten` list equality (both value *and* absence of extra tokens
in one comparison):

```aiken
// validators/predictions.ak:93-121
if num_true_shares > 0 {
  expect
    [
      (assets.ada_policy_id, assets.ada_asset_name, own_output_datum.envelope_amount),
      (params.share_policy_id, own_input_datum.true_position_name, num_true_shares),
    ] == assets.flatten(out.value)
} else {
  expect
    [
      (assets.ada_policy_id, assets.ada_asset_name, own_output_datum.envelope_amount),
      (params.share_policy_id, own_input_datum.false_position_name, num_false_shares),
    ] == assets.flatten(out.value)
}
```

Plus a redundant belt-and-braces token count:

```aiken
// validators/predictions.ak:135-141
let num_out_assets = list.length(assets.flatten(out.value))
(num_true + num_true_shares, num_false + num_false_shares, bs + 1,
 num_out_assets <= 2 && are_correct)
```

Script-hash destination check for the fee sweep:

```aiken
// validators/predictions.ak:362-367
expect Some(Output { address: treasury_address, value: treasury_value, .. }) =
  list.at(outputs, treasury_out_idx)
expect from_script(params.treasury_script_hash) == treasury_address
```

And for the buy path, "exactly one output at the position script with enough ADA":

```aiken
// validators/mint_shares.ak:55-70
let pos_script_cred = Script(pred_datum.position_script_hash)
let pos_outputs =
  list.filter(outputs, fn(output) {
      let Output { address: Address { payment_credential, .. }, .. } = output
      payment_credential == pos_script_cred && assets.lovelace_of(output.value)
        >= pred_datum.admin_fee + pred_datum.envelope_amount
    })
expect [pos_output] = pos_outputs
expect InlineDatum(pos_output_datum) = pos_output.datum
expect pos_datum: PositionDatum = pos_output_datum
expect pos_datum.pos_batcher_fee > 0 && pos_datum.pos_amount > 0
```

### 3.6 Min-ADA ("envelope") threading and fee accounting

`envelope_amount` is the min-ADA that rides with each user output, carried in the market datum and
asserted exactly (`predictions.ak:99`, `:113`, `:282`). The required lovelace on a new position UTxO
is the explicit sum of three components:

```aiken
// validators/mint_shares.ak:71-83
let out_pos_lovelace = assets.lovelace_of(pos_output.value)
let required_lovelace =
  pred_datum.admin_fee + pred_datum.envelope_amount + pos_datum.pos_batcher_fee
let amount =
  when params.payment_asset.policy_id == assets.ada_policy_id is {
    True -> ( out_pos_lovelace - required_lovelace ) / decimals
    False ->
      assets.quantity_of(pos_output.value,
        params.payment_asset.policy_id, params.payment_asset.asset_name) / decimals
  }
expect amount > 0
```

Fee accrual into the datum (`admin_fee` per batch item), and a monotonic-value assertion:

```aiken
// validators/predictions.ak:148-174
when params.payment_asset.policy_id == assets.ada_policy_id is {
  True -> {
    expect list.length(flatten_own_output_value) == 2
    expect input_lovelace + total_shares * decimals
      + batch_size * own_input_datum.admin_fee <= output_lovelace
  }
  False -> { ... expect list.length(flatten_own_output_value) <= 3
             expect input_payment + total_shares * decimals <= output_payment
             expect input_lovelace + batch_size * own_input_datum.admin_fee <= output_lovelace }
}
expect own_output_datum.cur_total_fee
  == own_input_datum.cur_total_fee + batch_size * own_input_datum.admin_fee
```

The "ADA is the payment asset OR it isn't" branch appears **five times** across the two validators.
That polymorphism (lovelace vs native asset as the unit of account) is a real stdlib candidate.

### 3.7 Batcher licence NFT = deadline encoded in the asset name

```aiken
// lib/bodega/utils.ak:21-28
pub fn find_license_deadline(license_symbol: PolicyId, val: Value) -> Int {
  expect [Pair(tn, am)] = val |> assets.tokens(license_symbol) |> dict.to_pairs()
  expect am == 1
  bytearray_to_int(tn)
}
```

```aiken
// validators/predictions.ak:36-45
expect Some(Input { output: Output { value: batcher_value, .. }, .. }) =
  list.at(inputs, license_idx)
let license_deadline = find_license_deadline(params.license_symbol, batcher_value)
let (start_valid_time_range, end_valid_time_range) = find_posix_time_range(validity_range)
expect and {
    license_deadline >= end_valid_time_range,
    license_deadline <= start_valid_time_range + params.maximum_deadline_range,
  }
```

The repo's own `ANALYSIS.md:321` describes it as: "The batcher must include a 'license' NFT whose
asset name encodes a deadline timestamp. This prevents replay attacks and ensures the batcher is
authorized." This is an **asset-name-as-data** idiom, adjacent to CIP-68 label prefixes.

Deadline check on the buy side:

```aiken
// validators/mint_shares.ak:52-54
let (_start_valid_time_range, end_valid_time_range) = find_posix_time_range(validity_range)
expect end_valid_time_range < pred_datum.dead_line
```

### 3.8 Exact mint/burn assertions

```aiken
// validators/mint_shares.ak:85-97
when pos_datum.pos_side is {
  SideTrue ->
    assets.quantity_of(pos_output.value, policy_id, pred_datum.true_position_name) == amount
      && [(policy_id, pred_datum.true_position_name, amount)] == flatten_mint
  SideFalse -> ...
}
```

```aiken
// validators/predictions.ak:323-326
expect
  [(params.share_policy_id, oracle_datum.position_name, -num_shares)] == flatten(mint)
```

Pro-rata payout arithmetic (integer, denominators from datum accumulators):

```aiken
// validators/predictions.ak:268-269
let reward_amount =
  curr_shares * ( total_winning + total_losing ) * decimals / total_winning
```

### 3.9 Idioms present

`auth-nft-check` (market NFT, oracle NFT, licence NFT), `input-index-hint`, `output-index-hint`,
`batched-index-pairs`, `output-to-self-with-datum`, `datum-field-preservation`,
`payout-to-address-with-datum` (address rebuilt from PKH + optional stake key),
`exact-value-equality-via-flatten`, `exact-mint-equality`, `validity-range-check`,
`deadline-check`, `asset-name-as-data` (licence deadline), `min-ada` (`envelope_amount`),
`protocol-fee-cut` (`admin_fee` accrual + treasury sweep), `oracle-reference-input`,
`ada-or-native-payment-asset` branch, `single-pass-fold-accumulator`.

**Missing (and notable):** no sorted/exhaustive guard on `pos_indices`, no `tag-with-txoutref`,
no `own_ref` verification, no CIP-68.

---

## 4. Cardano-Swaps

Two validator pairs (one-way / two-way), each a *spending* validator plus a *dual-purpose
mint+stake* beacon script. Also the second canonical anti-double-satisfaction case study.

### 4.1 Common helpers

| Name | Signature | Purpose | file:line |
|---|---|---|---|
| `trace_if_false` | `fn(msg: String, predicate: Bool) -> Bool` | Trace on failure, keep composing with `&&` | `aiken/lib/cardano_swaps/common/utils.ak:17` |
| `error_if_false` | `fn(msg: String, predicate: Bool) -> Bool` | Hard `error` with a message | `.../common/utils.ak:26` |
| `staking_credential_approves` | `fn(swap_addr, withdrawals, extra_signatories) -> Bool` | Owner auth: pubkey signed OR stake script executed; `None` stake ⇒ `True` | `.../common/utils.ak:35` |
| `compare_asset_config` | `fn(AssetConfig, AssetConfig) -> Ordering` | Lexicographic (policy, name) ordering for canonical pair sorting | `.../common/utils.ak:59` |
| `has_beacon_script_minting_execution` | `fn(beacon_id: PolicyId, mint: MintedValue) -> Bool` | Beacon policy appears in `mint` | `.../common/utils.ak:69` |
| `has_beacon_script_staking_execution` | `fn(beacon_id: PolicyId, withdrawals) -> Bool` | `Inline(ScriptCredential(beacon_id))` in withdrawals | `.../common/utils.ak:76` |
| `parse_datum` | `fn(raw_datum: Datum) -> SwapDatum` | Inline-only datum decode, hard error otherwise | `.../one_way_swap/utils.ak:28` |
| `get_upper_bound` | `fn(val_range: ValidityRange) -> Option<PosixTime>` | Extract `invalid-hereafter` | `.../one_way_swap/utils.ak:43` |
| `generate_pair_beacon` | `fn(asset1: AssetConfig, asset2: AssetConfig) -> AssetName` | `sha2_256(a1id ++ a1name ++ a2id ++ a2name)`, ADA policy substituted by `#"00"` | `.../one_way_swap/utils.ak:54` |
| `generate_offer_beacon` | `fn(policy_id, asset_name) -> AssetName` | `sha2_256(#"01" ++ pid ++ name)` | `.../one_way_swap/utils.ak:71` |
| `generate_ask_beacon` | `fn(policy_id, asset_name) -> AssetName` | `sha2_256(#"02" ++ pid ++ name)` | `.../one_way_swap/utils.ak:83` |
| `extract_ask_and_offer_quantity` | `fn(val, beacon_id, pair_beacon, offer_id, offer_name, offer_beacon, ask_id, ask_name, ask_beacon) -> (Int,Int,Int)` | ONE fold over the value that validates beacons, rejects extraneous assets, and returns `(offer, ask, ada)` | `.../one_way_swap/utils.ak:95` |
| `valid_swap_output` | `fn(beacon_id, output_value, output_datum, invalid_hereafter) -> Bool` | Full datum + value well-formedness for a produced swap UTxO | `.../one_way_swap/utils.ak:232` |
| `beacon_destination_check` | `fn(dapp_hash, beacon_id, invalid_hereafter, outputs) -> Bool` | Every output holding beacons must be at the DApp script address with a stake credential and be well-formed | `.../one_way_swap/utils.ak:324` |
| `swap_output_value` | `fn(swap_addr, …, req_datum: Datum, outputs) -> (Int,Int,Int)` | Find THE corresponding output by exact datum match; error if absent | `.../one_way_swap/utils.ak:393` |
| `valid_swap` | `fn(input_ref, input_val, invalid_hereafter, swap_addr, …, outputs) -> Bool` | The whole public swap rule | `.../one_way_swap/utils.ak:472` |

Two-way variants: `generate_asset_beacon` (`two_way_swap/utils.ak:71`), `valid_swap` with a
`taking_asset2: Bool` direction flag (`two_way_swap/utils.ak:479`).

### 4.2 Anti-double-satisfaction: `prev_input = Some(input_ref)` in the continuing datum

Datum field (`one_way_swap/types.ak:17`, `two_way_swap/types.ak:18`):

```aiken
    swap_price: Rational, // The swap price as a fraction: Ask/Offer.
    prev_input: Option<OutputReference>,
    expiration: Option<PosixTime>
```

The spending validator constructs the *exact expected output datum* including its own
`OutputReference`, then searches outputs for a byte-identical inline datum:

```aiken
// aiken/lib/cardano_swaps/one_way_swap/utils.ak:489-502
let req_datum =
  SwapDatum(
    beacon_id, pair_beacon,
    offer_id, offer_name, offer_beacon,
    ask_id, ask_name, ask_beacon,
    swap_price,
    Some(input_ref),
    expiration,
  ) |> InlineDatum
```

```aiken
// aiken/lib/cardano_swaps/one_way_swap/utils.ak:390-416
// This is a recursive function that will terminate when the corresponding output is found or when
// no corresponding output is found. ... It looks for the required datum first to minimize the
// number of checks needed.
fn swap_output_value(…, req_datum: Datum, outputs: List<Output>) -> (Int,Int,Int) {
  when outputs is {
    [] -> error @"Corresponding swap output not found"
    [Output(addr,oval,d,_), ..rest] -> {
      // Requirements for a valid corresponding output:
      // 1) The output is to the same swap address where the input originates.
      // 2) The output has exactly one of each of the proper beacons.
      // 3) The output's datum is exactly the same accept the `prev_input` == `Some(input_ref)`.

      // Check if the datum is correct. This is done first to rule out all but the target UTxO.
      if req_datum == d {
        if swap_addr == addr { extract_ask_and_offer_quantity(oval, …) }
```

**Mechanism.** Each spent swap UTxO forces a *distinct* continuing output (distinct because the
datum embeds that input's unique `OutputReference`). Two swap inputs can therefore never be paid by
the same output. Unlike JPG's hashed tag, the whole datum is the tag, and the datum-equality check
doubles as "all other datum fields unchanged" – one comparison enforces both continuity and
uniqueness. The README states it plainly:

> `datum must match the input's, except prevInput is updated to the TxOutRef of the input.`
> (`README.md:342`)

### 4.3 Beacon tokens as authenticity + off-chain index, and beacon-scoped executions

The design note explaining *why* per-UTxO script execution makes this cheap:

```
// README.md:326-334
> Since the spending script first checks for the trading pair beacon, each execution is dedicated to
> a specific trading pair. Any other outputs are ignored in this specific execution. This logic
> works because a script is executed once for every UTxO spent from the address. If input 1 is for
> beacon XYZ and input 2 is for beacon ABC, the first execution can be dedicated to beacon XYZ and
> the second execution can be dedicated to ABC. The net transaction will only succeed if all
> executions succeed. This behavior allows cheaply composing swaps of different trading pairs that
> are located at the same address.
```

Deterministic beacon names derived by hashing, with a **1-byte domain-separation prefix** – the
closest thing in these repos to CIP-68 label prefixes:

```aiken
// aiken/lib/cardano_swaps/one_way_swap/utils.ak:50-90
// When ada is part of the pair, the empty bytestring is replaced so that the beacon is distinct
// for each direction when ada is part of the pair.
pub fn generate_pair_beacon(asset1: AssetConfig, asset2: AssetConfig) -> AssetName {
  let asset1_id = if asset1_id_ == #"" { #"00" } else { asset1_id_ }
  let asset2_id = if asset2_id_ == #"" { #"00" } else { asset2_id_ }
  hash.sha2_256(
    bytearray.concat(asset1_id,asset1_name)
    |> bytearray.concat(_,asset2_id)
    |> bytearray.concat(_,asset2_name))
}

// The pre-hash is prefixed with "01" to keep it distinct from the other beacons.
pub fn generate_offer_beacon(policy_id: PolicyId, asset_name: AssetName) -> AssetName {
  hash.sha2_256(bytearray.concat(#"01", bytearray.concat(policy_id, asset_name)))
}
// The pre-hash is prefixed with "02" ...
pub fn generate_ask_beacon(policy_id: PolicyId, asset_name: AssetName) -> AssetName {
  hash.sha2_256(bytearray.concat(#"02", bytearray.concat(policy_id, asset_name)))
}
```

The mint-side destination check ties beacons to the DApp address AND requires a stake credential
(so funds cannot be permanently locked and the owner always has an auth path):

```aiken
// aiken/lib/cardano_swaps/one_way_swap/utils.ak:353-383
let foo = fn (out: Output, acc: Bool) {
  let Output(Address(pay,stake),val,d,_) = out
  let beacon_list = value.tokens(val,beacon_id) |> dict.to_list()
  when beacon_list is {
    // If beacons are not present, this output can be ignored.
    [] -> acc
    // The UTxO has three types of beacon.
    [(_,_),(_,_),(_,_)] -> {
      acc &&
      trace_if_false(
        concat(@"Beacon must go to a ", app_name) |> concat(_,@" DApp address with staking"),
        pay == ScriptCredential(dapp_hash) && is_some(stake)) &&
      valid_swap_output(beacon_id,val,d,invalid_hereafter)
    }
    _ -> error @"One-way swaps must have exactly three kinds of beacons"
  }
}
list.foldl( outputs, True, foo )
```

### 4.4 Spend forwards to a dual-purpose mint-or-stake script

The spending validator delegates the owner path entirely:

```aiken
// aiken/validators/one_way_swap.ak:31-53
SpendWithMint -> {
  // 1) The address' staking credential must signal approval.
  // 2) The beacon script must be executed as a minting policy.
  …
  trace_if_false(@"Staking credential did not approve",
    common_utils.staking_credential_approves( addr, wtdr, sigs)) &&
  trace_if_false(@"Beacon script not executed as minting policy",
    common_utils.has_beacon_script_minting_execution(beacon_id,mint))
}
```

```aiken
// aiken/lib/cardano_swaps/common/utils.ak:69-84
pub fn has_beacon_script_minting_execution(beacon_id: PolicyId, mint: MintedValue) -> Bool {
  !(value.from_minted_value(mint) |> value.tokens(_,beacon_id) |> dict.is_empty(_))
}
pub fn has_beacon_script_staking_execution(beacon_id: PolicyId, withdrawals) -> Bool {
  dict.has_key(withdrawals, Inline(ScriptCredential(beacon_id)))
}
```

One script body serving two purposes, with the policy id recovered from whichever purpose fired:

```aiken
// aiken/validators/one_way_swap.ak:194-200
let ScriptContext(Transaction{outputs,validity_range,..}, purpose) = ctx
let policy_id = when purpose is {
  tx.Mint(policy_id) -> policy_id
  tx.WithdrawFrom(Inline(ScriptCredential(policy_id))) -> policy_id
  _ -> error @"Redeemer not used with minting execution"
}
```

Certificate-purpose gating (register only, never delegate/deregister):

```aiken
// aiken/validators/one_way_swap.ak:155-163
RegisterBeaconScript -> {
  // Only allow registering the script. It cannot be delegated or deregistered.
  when ctx is {
    ScriptContext(_, tx.Publish(CredentialRegistration(_))) -> True
    _ -> error "This redeemer can only be used to register the beacon script"
  }
}
```

### 4.5 Owner auth via the address' own stake credential

```aiken
// aiken/lib/cardano_swaps/common/utils.ak:34-57
pub fn staking_credential_approves(swap_addr, withdrawals, extra_signatories) -> Bool {
  let Address(_,staking_cred) = swap_addr
  when staking_cred is {
    // This is to prevent permanent locking of funds. Beacons can never be minted to an address
    // without a valid staking credential.
    None -> True
    // If the address uses a staking pubkey, it must have signed the tx.
    Some(Inline(VerificationKeyCredential(skey))) -> list.has(extra_signatories,skey)
    // If the address uses a staking script, it must have been executed in the tx. In order for
    // the staking credential to show up in this dictionary, it must be registered.
    Some(svh) -> dict.has_key(withdrawals,svh)
  }
}
```

### 4.6 Validity-interval / expiration handling

Uses `invalid-hereafter` as a *proof that a deadline has not passed yet*, plus a 1-minute
quantisation so front-ends can index the order book:

```aiken
// aiken/lib/cardano_swaps/one_way_swap/utils.ak:38-48
// Get the time from the tx's invalid-hereafter setting. The invalid-hereafter setting is used for
// expiration times since it guarantees that the specified time has NOT passed yet; the transaction
// would fail otherwise. By having users set the invalid-hereafter to the closest expiration time,
// users can assert to the plutus script that the expiration indeed has not passed.
pub fn get_upper_bound(val_range: ValidityRange) -> Option<PosixTime> {
  when val_range is {
    Interval(_,IntervalBound(Finite(x),_)) -> Some(x)
    _ -> None
  }
}
```

```aiken
// aiken/lib/cardano_swaps/one_way_swap/utils.ak:269-279
expect True = when (expiration_,invalid_hereafter) is {
  (None,_) -> True
  (Some(expir),Some(hereafter)) ->
    error_if_false(@"Expiration must be >= invalid-hereafter", hereafter <= expir) &&
    error_if_false(@"Must use 1-min expiration intervals", expir % 60000 == 0)
  (Some(_),None) -> error "invalid-hereafter required but not set"
}
```

### 4.7 Price / value-delta arithmetic

Delta-based accounting (input vs output quantities), rational price compared without division:

```aiken
// aiken/lib/cardano_swaps/one_way_swap/utils.ak:544-573
// Calculate the quantity of the ask asset in flux. Output - Input.
let ask_given = ask_o - ask_i
// Calculate the quantity of the offer asset in flux. Input - Output.
let offer_taken = offer_i - offer_o

// Ada can always be deposited if not part of the pair.
expect True =
  if offer_id == #"" || ask_id == #"" { True }
  else { error_if_false(@"Ada can only be deposited", ada_o - ada_i >= 0) }

// The swap_ratio must be correct. To avoid rounding issues, the ask_given is multiplied
// by the price_den. This is mathematically equivalent to:
// offer_taken * price_num / price_den <= ask_given
trace_if_false(@"Fail: offer_taken * price <= ask_given",
  offer_taken * price_num <= ask_given * price_den) &&

// The ask asset cannot be taken. This would allow the swap to go in reverse.
trace_if_false(@"The ask asset cannot be taken from the swap", ask_given >= 0)
```

Canonical pair sorting for two-way swaps, so `(A,B)` and `(B,A)` collapse to one beacon:

```aiken
// aiken/lib/cardano_swaps/two_way_swap/utils.ak:271-275
// The trading pair must be sorted: asset1 < asset2. They cannot be equal.
trace_if_false(@"Asset1 must be less than asset2",
  compare_asset_config(asset1,asset2) == Less) &&
```

Direction selection collapses two symmetric rules into one:

```aiken
// aiken/lib/cardano_swaps/two_way_swap/utils.ak:524-530
// Which asset is ask/offer and what price should be used.
let (offer_id,offer_name,offer_beacon,ask_id,ask_name,ask_beacon,Rational(price_num,price_den)) =
  if taking_asset2 {
    (asset2_id,asset2_name,asset2_beacon,asset1_id,asset1_name,asset1_beacon,asset2_price)
  } else {
    (asset1_id,asset1_name,asset1_beacon,asset2_id,asset2_name,asset2_beacon,asset1_price)
  }
```

### 4.8 Efficiency tricks

- **One fold does everything.** `extract_ask_and_offer_quantity` (`one_way_swap/utils.ak:95-229`)
  walks `value.to_dict(val) |> dict.to_list(_)` exactly once and simultaneously: (i) verifies there
  is exactly one of each of the three beacons, (ii) rejects any extraneous asset with a hard
  `error`, (iii) returns `(offer, ask, ada)` quantities. The accumulator is a 4-tuple
  `(Int, Int, Int, Bool)` and the beacon sub-check uses an inner 3-tuple `(Bool, Bool, Bool)` fold:

```aiken
// aiken/lib/cardano_swaps/one_way_swap/utils.ak:114-131
let beacon_check = fn(y: (AssetName,Int), acc: (Bool,Bool,Bool)) {
  let (tn,n) = y
  let (pair_check,offer_check,ask_check) = acc
  if tn == pair_beacon { (n == 1, offer_check, ask_check) }
  else if tn == offer_beacon { (pair_check, n == 1, ask_check) }
  else if tn == ask_beacon { (pair_check, offer_check, n == 1) }
  else { error @"UTxO has wrong beacons" }
}
when list.foldl(tns_list,(False,False,False),beacon_check) is {
  (True,True,True) -> (offer,ask,ada,True)
  _ -> acc
}
```

- **Datum-first output matching.** `swap_output_value` compares the whole inline datum *before*
  touching the address or value, "to rule out all but the target UTxO" (`utils.ak:414-416`). One
  structural equality replaces N field comparisons.
- **Forced evaluation of a checking function** via a trivial use of its result:
  `o >= 0` at `one_way_swap/utils.ak:317-318` ("This is just to force the
  extract_ask_and_offer_quantity function to be executed") – a laziness workaround worth noting.
- **Division-free rational comparison** (`offer_taken * price_num <= ask_given * price_den`).
- **Beacon-scoped executions** turn N swaps into N independent cheap validations instead of one
  expensive cross-product check (README design note quoted in §4.3).
- **`error_if_false` vs `trace_if_false`**: hard-fail for invariants, soft trace for composable
  predicates (`common/utils.ak:17-32`). A tiny but repeated ergonomics helper.

### 4.9 Idioms present

`prev-input-tag`, `no-double-satisfaction`, `output-to-self-with-datum` (whole-datum equality),
`auth-nft-check` / `beacon-token`, `beacon-name-from-hash` with 1-byte domain prefixes,
`spend-forwards-to-mint`, `spend-forwards-to-withdraw`, `withdraw-zero-forward`,
`dual-purpose-mint-or-stake-script`, `certificate-purpose-gate`, `stake-credential-approves`,
`validity-range-check`, `deadline-check` (invalid-hereafter as proof), `time-quantisation`,
`canonical-pair-sort`, `no-extraneous-assets`, `partial-fill-delta-accounting`,
`rational-price-no-division`, `single-pass-value-fold`, `data-level-compare`.

**Missing:** one-shot mint tied to a `TxOutRef` (beacons are re-mintable by design), index hints,
CIP-68, protocol fee cut, min-ADA logic (ADA deposits are simply always allowed).

---

## 5. CIP-68 reference-token handling: NOT PRESENT in any of the four repos

`grep -rn '000643b0|cip68|CIP68|CIP-68|cip_68'` over all four repos returns **zero hits**. Neither
the `100`/`222`/`333`/`444` label prefixes nor the `(100)`/`(222)` reference/user token pairing
appear in any on-chain or off-chain source.

Closest analogues actually found:

1. **Cardano-Swaps' hashed beacon names with 1-byte domain prefixes** (§4.3) – same *idea* as a
   label prefix (namespace an asset name so distinct roles never collide), different encoding:
   `sha2_256(#"01" ++ policy ++ name)` vs CIP-68's `#"000de140" ++ name`.
2. **CIP-113's node NFT whose asset name IS the key** (`lib/linked_list.ak:41`:
   `expect tn == node.key || tn == origin_node_tn`) – asset-name-as-identity.
3. **Bodega's licence NFT whose asset name is an ASCII deadline** (`lib/bodega/utils.ak:21-33`) –
   asset-name-as-data, decoded on-chain with a hand-written `bytearray_to_int`.

For reference, a real CIP-68 helper module exists in the `vodka` library that was cloned alongside
these repos (outside the four assigned repos, included here only because the prefixes were
requested), at
`/private/tmp/claude-501/-Users-nau-projects-lantr-scalus/bbb8c499-c57d-432b-96c1-767073900d25/scratchpad/repos/vodka/lib/cip.ak:17-73`:

```aiken
/// The byte prefix for CIP-68 asset - Reference Token
pub const cip68_100_prefix: ByteArray = #"000643b0"
/// The byte prefix for CIP-68 asset - Non-Fungible Token
pub const cip68_222_prefix: ByteArray = #"000de140"
/// The byte prefix for CIP-68 asset - Fungible Token
pub const cip68_333_prefix: ByteArray = #"0014df10"
/// The byte prefix for CIP-68 asset - Rich-Fungible Token
pub const cip68_444_prefix: ByteArray = #"001bc280"

pub fn drop_cip68_prefix(cip_68_asset_name: AssetName) -> AssetName {
  cip_68_asset_name |> bytearray.drop(4)
}
pub fn cip68_100(asset_name: AssetName) -> AssetName { concat(cip68_100_prefix, asset_name) }
pub fn cip68_222(asset_name: AssetName) -> AssetName { concat(cip68_222_prefix, asset_name) }
pub fn cip68_333(asset_name: AssetName) -> AssetName { concat(cip68_333_prefix, asset_name) }
pub fn cip68_444(asset_name: AssetName) -> AssetName { concat(cip68_444_prefix, asset_name) }

/// The metadata attached with CIP-68 reference token (100)
pub type CIP68Metadata { metadata: Pairs<Data, Data>, version: Int }
```

Takeaway for the stdlib: CIP-68 helpers should be *offered*, but the more universal need is a
generic **asset-name codec** layer (prefix/suffix construct + strip, name-as-key, name-as-integer),
of which CIP-68 labels are one instance.

---

## 6. Cross-repo synthesis

### 6.1 Three distinct anti-double-satisfaction families

| Family | Repo | Mechanism | Cost profile | Stdlib primitive it implies |
|---|---|---|---|---|
| **(a) Hash-tag an output datum with the spent `OutputReference`** | JPG Store v3 (`validators/ask.ak:44-51`) | `InlineDatum(blake2b_256(serialise_data(out_ref)))` on the head payout output; all following payout outputs must be `NoDatum` | 1 serialise + 1 hash + 1 datum compare | `tagOf(ownRef): Datum` + `expectTaggedOutput(outputs, tag)` |
| **(b) Embed the spent `OutputReference` in the continuing datum** | Cardano-Swaps (`one_way_swap/utils.ak:489-502`, `:406-416`) | Build the full expected `SwapDatum` with `prev_input = Some(input_ref)`, then find the output whose inline datum equals it exactly | 1 datum construction + N datum compares | `expectContinuingOutput(ownRef, datumUpdate)` – one equality gives both continuity and uniqueness |
| **(c) Index hints + exact own-input count** | CIP-113 (`programmable_logic_global.ak:168-223`), Bodega (`predictions.ak:34-72`) | Redeemer names input/output indices; script checks indices are strictly ordered AND that the number of own-script inputs equals the number listed | O(idx) list walks, no hashing | `IndexedInputs` helper enforcing sorted-unique + exhaustive |

Families (a) and (b) work per-execution and need no global count. Family (c) needs the two guards
from §6.2 or it is unsound.

Two more notes:

- A *fourth*, weaker family is used implicitly by Cardano-Swaps: **beacon-scoped executions**
  (`README.md:326-334`). Each execution binds itself to one trading-pair beacon, so executions
  never overlap on the same output.
- JPG's positional coupling (`find_payout_outputs` offset + `NoDatum` tail) is a hybrid of (a) and
  (c): one tagged output anchors a *contiguous run* of untagged ones.

### 6.2 The two checks that make an index-hint redeemer sound

Both from `/Users/nau/projects/lantr/cip113-programmable-tokens/src/programmable-tokens-onchain-aiken/validators/programmable_logic_global.ak`:

```aiken
// :174  -- no duplicate / out-of-order indices (prevents double-counting one input)
expect prev_idx > input_idx || prev_idx == -1
```

```aiken
// :218-223 -- no unlisted script input can hide in the transaction
let prog_input_count =
  list.count(tx.inputs, fn(input) {
    input.output.address.payment_credential == prog_logic_cred
  })
expect prog_input_count == length_input_idxs
```

Bodega implements the index-hint pattern **without either guard** (`predictions.ak:62-72`), which is
the difference between a safe and an unsafe instance of the same idiom. Any Scalus stdlib API for
"process these inputs by index" must bake both in.

### 6.3 Ranked stdlib candidates (by how many of the four repos hand-roll them)

| Candidate | Repos | Evidence |
|---|---|---|
| Find/assert **the** continuing output of this script (address + datum policy) | 4/4 | jpg `check_payouts`, swaps `swap_output_value`, CIP-113 `validate_third_party`, bodega `own_output_idx` |
| Anti-double-satisfaction tag/uniqueness helper | 3/4 | jpg, swaps, CIP-113 |
| Auth by NFT presence in an input / reference input | 3/4 | bodega (x4), CIP-113 (nodes, params), swaps (beacons) |
| withdraw-zero forwarding + "is this script invoked" | 2/4 (many sites) | CIP-113 x6, swaps x2 |
| One-shot mint tied to a `TxOutRef` | 1/4 but x4 copies | CIP-113 `protocol_params_mint:22`, `issuance_cbor_hex_mint:18`, `registry_mint:33`, `blacklist_mint:27` |
| "Exact value" assertion (no extraneous assets) | 3/4 | bodega `flatten` equality, swaps `error @"No extraneous assets allowed"`, CIP-113 `count_unique_tokens == 2` |
| Value containment / merge without `flatten` | 2/4 | CIP-113 `value_contains`, freeze-and-seize `value_contains` x3 |
| Index-hint list access (`elem_at`, `skip_n`) with guards | 3/4 | jpg `skip_10_items`, CIP-113 `elem_at`, bodega `list.at` |
| Datum field-preservation ("all fields but X unchanged") | 3/4 | bodega `correct_own_output_datum`, CIP-113 `is_updated_directory_node`, swaps whole-datum equality |
| Validity range → `(lo, hi)` / upper bound, deadline compare | 2/4 | bodega `find_posix_time_range`, swaps `get_upper_bound` |
| Payout list enforcement (address + min amount, ordered) | 1/4 but is the whole product | jpg `check_payouts` |
| Sorted on-chain linked list (insert/remove, membership + non-membership) | 1/4 but x2 copies | CIP-113 core + freeze-and-seize |
| Asset-name codec (prefix construct/strip, name-as-int, name-as-key) | 3/4 in disguise | swaps beacon prefixes, CIP-113 node key, bodega ASCII deadline |
| Stake-credential-approves (pubkey sig OR stake script ran) | 2/4 | swaps `staking_credential_approves`, CIP-113 `get_signed_prog_value` |
| Rational price compare without division | 1/4 | swaps |
| ADA-vs-native "unit of account" polymorphism | 1/4 but x5 sites | bodega |

### 6.4 Efficiency techniques worth exposing in Scalus

1. **Single-pass fold with a tuple accumulator that both validates and totals**
   (swaps `extract_ask_and_offer_quantity`, jpg `check_payouts` returning `Int`,
   bodega `list.foldl(pos_indices, (0,0,0,True), …)`).
2. **CPS select/discard traversal** so one loop serves inputs and outputs
   (CIP-113 `sum_output_values`).
3. **Sorted-merge containment on `Dict`-derived pairs**, O(n+m), with the sortedness pre-condition
   documented at the call site (CIP-113 `value_contains` + `drop_until`).
4. **Compare serialised/structural `Data` instead of field-by-field**
   (swaps `req_datum == d`; CIP-113 `rdmr == target_redeemer_data`; jpg `serialise_data |> blake2b_256`;
   bodega `flatten(value) == [ … ]`).
5. **Partial datum decoding with `unconstr_fields` + `head_list`/`tail_list`** to read the first k
   fields without validating the whole schema, and to stay forward-compatible with additive datum
   changes (CIP-113 `expect_registry_node`, `expect_programmable_logic_params`).
6. **Skip the always-first ADA entry** of a `Value`'s policy dict rather than filtering
   (CIP-113 `has_currency_symbol`, `match_assets`, `check_transfer_and_compute_prog_value`).
7. **Unrolled list skipping** driven by a redeemer index hint (jpg `skip_10_items` at 10x,
   CIP-113 `elem_at` at 2x).
8. **Direct builtin calls** in hot paths: `tail_list`, `head_list`, `index_bytearray`,
   `less_than_bytearray`, `serialise_data`, `blake2b_256`, `blake2b_224`, `append_bytearray`,
   `unconstr_fields`.
9. **Order checks cheapest-first**: swaps checks the datum before the address before the value
   ("done first to rule out all but the target UTxO").
10. **Division-free rational comparison** (cross-multiply).

---

## 7. Per-repo idiom lists (canonical short names)

### jpgstore-v3
`tag-with-txoutref`, `no-double-satisfaction`, `payout-to-address-with-datum`,
`ordered-payout-list`, `protocol-fee-cut`, `royalty-split`, `min-ada`, `output-index-hint`,
`unrolled-list-skip`, `data-level-compare`, `ada-only-value-check`, `hardcoded-address-build`,
`owner-signature-cancel`, `authorizer-discount-escape-hatch`, `single-pass-accumulator`.

### cip113-programmable-tokens (core + substandards)
`withdraw-zero-forward`, `spend-forwards-to-mint`, `mint-forwards-to-withdraw`,
`one-shot-mint`, `auth-nft-check`, `nft-name-is-key`, `onchain-linked-list`,
`membership-proof`, `non-membership-proof`, `input-index-hint`, `output-index-hint`,
`sorted-index-list-guard`, `own-input-count-exact`, `output-to-self-with-datum`,
`datum-field-preservation`, `stake-credential-approves`, `script-hash-recompute`,
`data-level-compare`, `partial-datum-decode`, `value-contains-sorted`,
`single-pass-value-sum`, `no-extraneous-assets`, `balance-invariant`,
`always-fail-address-lock`, `single-mint-redeemer-count`, `no-op-change-guard`.

### bodega-market-smart-contracts
`auth-nft-check`, `oracle-reference-input`, `input-index-hint`, `output-index-hint`,
`batched-index-pairs`, `output-to-self-with-datum`, `datum-field-preservation`,
`payout-to-address-with-datum`, `address-rebuild-from-datum`,
`exact-value-equality-via-flatten`, `exact-mint-equality`, `exact-burn-equality`,
`validity-range-check`, `deadline-check`, `asset-name-as-data`, `min-ada`,
`protocol-fee-cut`, `treasury-sweep`, `ada-or-native-payment-asset`,
`single-pass-fold-accumulator`, `licence-nft-with-deadline`.
*(Missing and worth flagging: `sorted-index-list-guard`, `own-input-count-exact`, `own-ref-check`.)*

### cardano-swaps
`prev-input-tag`, `no-double-satisfaction`, `output-to-self-with-datum`,
`whole-datum-equality`, `beacon-token`, `auth-nft-check`, `beacon-name-from-hash`,
`domain-prefix-asset-name`, `spend-forwards-to-mint`, `spend-forwards-to-withdraw`,
`withdraw-zero-forward`, `dual-purpose-mint-or-stake-script`, `certificate-purpose-gate`,
`stake-credential-approves`, `validity-range-check`, `deadline-check`, `time-quantisation`,
`canonical-pair-sort`, `no-extraneous-assets`, `partial-fill-delta-accounting`,
`rational-price-no-division`, `single-pass-value-fold`, `data-level-compare`,
`beacon-scoped-execution`.

### CIP-68 (`cip68-ref-pair`, `cip68-label-prefix`)
**Absent from all four repos.** Reference implementation quoted from `vodka/lib/cip.ak` in §5.
