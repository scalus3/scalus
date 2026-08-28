# Raw research: L2 / rollup / bridge on-chain code

Source-code survey of four real Cardano protocols, done to find what production
validators keep re-implementing by hand. Everything below is quoted from local
checkouts. Absolute paths, per-file line numbers.

| # | Repo | Path | Language | On-chain LOC |
|---|------|------|----------|--------------|
| 1 | Midgard (Anastasia Labs optimistic rollup) | `/Users/nau/projects/lantr/midgard/onchain/aiken` | Aiken (Plutus V3) | ~6 100 (56 `.ak`) |
| 2 | Hydrozoa (L2 head, rule-based regime) | `/Users/nau/projects/lantr/hydrozoa/cardano-onchain` | Scala 3 / Scalus | ~1 733 |
| 3 | AdaStream (file-sale bond + HTLC) | `/Users/nau/projects/lantr/adastream/src` | Scala 3 / Scalus | ~246 |
| 4 | Binocular (Bitcoin oracle + Bifrost bridge) | `/Users/nau/projects/lantr/binocular/src/main` | Scala 3 / Scalus | ~3 000 across 7 `@Compile` files |

Method: `grep -rn` for `pub fn` / `def ` / `expect` / `own_` / `must_` / `single` /
`exactly` / `index` / `quantity_of` / `flatten` / `serialise` / `withdraw` / `double`,
then full reads of the ~18 most idiom-dense files.

---

# 1. Midgard — `/Users/nau/projects/lantr/midgard/onchain/aiken`

Optimistic rollup. Twelve interlocking scripts (hub oracle, state queue, settlement
queue, three operator sets, scheduler, three user-event families, computation
threads, fraud proofs). This is by far the densest idiom source of the four.

## 1.0 The single biggest finding: half the primitives are already a *library*

Midgard's core structural checks are **not** written in Midgard. They come from
`anastasia-labs/aiken-design-patterns v1.2.0`, declared in
`/Users/nau/projects/lantr/midgard/onchain/aiken/aiken.toml`:

```toml
[[dependencies]]
name = "anastasia-labs/aiken-design-patterns"
version = "v1.2.0"
source = "github"

[[dependencies]]
name = "aiken-lang/merkle-patricia-forestry"
version = "2.0.0"
source = "github"
```

The package is **not vendored in this checkout** (`build/packages` is absent), so the
API below is *inferred from call sites only* — treat semantics as unverified.

| Imported symbol | Where used (examples) | Inferred contract |
|---|---|---|
| `linked_list/unordered.{init, deinit}` | `validators/state-queue.ak:59,63` | mint/burn the whole list's head node, gated by a companion NFT mint |
| `unordered.append_unsafe(key, out_ix, prev_out_ix, node_ins, node_outs, node_mint)` | `validators/state-queue.ak:100`, `validators/settlement-queue.ak:362` | append node at output index, prev node re-emitted at another index; `_unsafe` = key ordering not checked (unordered list) |
| `unordered.prepend_unsafe` | `validators/registered-operators.ak:82` | prepend variant |
| `unordered.remove(key, in_ix, prev_in_ix, …)` | `validators/state-queue.ak:221`, `settlement-queue.ak:405` | remove node, splice predecessor's link |
| `unordered.prove_is_root_node(policy, output)` | `validators/state-queue.ak:233,238` | this UTxO is the list head |
| `unordered.prove_is_last_node(policy, output)` | `validators/state-queue.ak:396` | this UTxO is the tail |
| `unordered.prove_is_member(policy, key, output)` | `validators/registered-operators.ak:237` | UTxO carries the node NFT for `key` |
| `unordered.get_raw_key_and_link` / `get_key` | `validators/state-queue.ak:308-310` | read key/link straight out of the datum without full typed decode |
| `unordered.list_state_transition(tokens)` | `validators/state-queue.ak:35`, `registered-operators.ak:25` | **spend-forwards-to-mint**: the spending validator does nothing but assert the mint map for the node policy is non-trivial, delegating all logic to the minting policy |
| `ordered.{init,deinit,append,insert,remove,prove_is_member,prove_is_not_member,prove_is_root_node,prove_is_last_node}` | `validators/active-operators.ak`, `registered-operators.ak`, `catalogue.ak`, `scheduler.ak` | same, plus **sorted-key** variants: `prove_is_not_member` is a *non-membership* proof by covering key range |
| `singular_utxo_indexer.spend(cb, purpose, in_ix, out_ix, own_ref, tx)` | `validators/user-events/deposit.ak:76`, `withdrawal.ak:84`, `tx-order.ak:69` | "exactly one script input paired with exactly one output, both by redeemer index" |
| `merkelized_validator.delegated_validation(function_input, staking_validator, input_data_coercer, redeemers)` | `lib/midgard/fraud-proof/common/utils.ak:86,110,134` | **withdraw-0 trick**: heavy computation moved into a staking script; the caller asserts a withdrawal redeemer equals the expected input |
| `merkelized_validator.generic_delegated_validation(staking_validator, withdraw_redeemer_validator, redeemers)` | `lib/midgard/common/utils.ak:349,381` | same, with a caller-supplied predicate over the withdrawal redeemer |

**Stdlib implication:** three of these four protocols independently need
linked-list-in-UTxO, one-input-one-output pairing, and off-loaded computation, and in
Aiken-land they are already a shared library. Scalus has no equivalent.

## 1.1 Helpers Midgard defines itself

All from `grep -rn "^pub fn \|^fn "` (per-file line numbers, authoritative).

### `lib/midgard/common/utils.ak`

| Name | Signature | Purpose | Line |
|---|---|---|---|
| `is_hereafter` | `(Interval<Int>, Int) -> Bool` | interval starts on/after point | `:20` |
| `is_herebefore` | `(Interval<Int>, Int) -> Bool` | interval ends on/before point | `:29` |
| `validate_mint` | `(Value, ByteArray, ByteArray, Int) -> Bool` | mint map for a policy is *exactly* `[(name, amt)]` | `:37` |
| `safe_list_last` / `safe_list_head` / `safe_list_init` | `List<a> -> a` / `List<a>` | `expect Some(..)` wrappers | `:47,:52,:57` |
| `is_posix_time_none` | `PosixTime -> Bool` | sentinel `-1` check | `:62` |
| `quantity_of_policy_id` | `(Value, PolicyId) -> Int` | sum of all token quantities under one policy | `:67` |
| `get_own_hash` | `(OutputReference, List<Input>) -> ByteArray` | own script hash from the spent input | `:71` |
| `get_own_address` | `(OutputReference, List<Input>) -> Address` | own address from the spent input | `:79` |
| `get_own_input_at` | `(List<Input>, OutputReference, Int) -> Output` | **input-index hint**: `list.at` + assert the outref matches `own_out_ref` | `:87` |
| `is_output_to_sc` | `(Output, ByteArray) -> Bool` | output pays a given script hash | `:98` |
| `has_signed` | `(VerificationKeyHash, List<VerificationKeyHash>) -> Bool` | readable `extra_signatories` membership | `:107` |
| `get_single_asset_from_value` | `Value -> (PolicyId, AssetName, Int)` | value has exactly one asset class | `:116` |
| `get_single_asset_from_value_apart_from_ada` | `Value -> (PolicyId, AssetName, Int)` | value = ADA + exactly one token | `:127` |
| `get_authentic_input_with_policy_at` | `(List<Input>, PolicyId, Int) -> Input` | **auth-NFT + index hint**: input at index holds exactly 1 token of that policy | `:141` |
| `get_authentic_input_with_nft_at` | `(List<Input>, PolicyId, AssetName, Int) -> Input` | same, pinned to an asset name | `:159` |
| `get_authentic_input_of` | `(List<Input>, ScriptHash, ByteArray, Int) -> Input` | same, *and* address payment credential == policy id (self-minted beacon) | `:179` |
| `get_asset_name_from_value_with_policy` | `(Value, PolicyId) -> AssetName` | the single asset name under a policy | `:207` |
| `get_singleton_asset_with_policy` | `(Value, PolicyId) -> Pair<AssetName, Int>` | the single `(name, qty)` under a policy | `:220` |
| `verify_hash_32` / `verify_hash_28` | `(Data, H32/H28) -> Unit` | `blake2b(cbor.serialise(data)) == h` | `:233,:238` |
| `authentic_value_has_tokens` | `(Value, PolicyId, AssetName, ValuePairs) -> Bool` | value == datum-declared `ValuePairs` + NFT + ADA | `:248` |
| `zip_foldr` / `zip_foldl` | `(List<a>, List<b>, c, fn(a,b,c)->c) -> c` | parallel two-list fold, no zip allocation | `:264,:284` |
| `compare_authentic_value_with_target` | `(Value, PolicyId, AssetName, ValuePairs, fn(Int,Int)->Bool) -> Bool` | compare a `Value` against a datum `ValuePairs` under a quantity predicate | `:309` |
| `get_redeemer_at` | `(Pairs<ScriptPurpose,Redeemer>, ScriptPurpose, Int) -> Redeemer` | **cross-script coordination**: redeemer at index, purpose asserted | `:338` |
| `plutarch_phas` | `(ByteArray, Data, Data, mpf.Proof, Redeemers) -> Bool` | MPF **membership** via withdraw-0 delegation | `:349` |
| `plutarch_pexcludes` | `(ByteArray, Data, mpf.Proof, Redeemers) -> Bool` | MPF **non-membership** via withdraw-0 delegation | `:381` |
| `get_verification_key_hash` | `VerificationKey -> VerificationKeyHash` | `blake2b_224` | `:409` |

### Other Midgard-defined helpers

| Name | File:line | Purpose |
|---|---|---|
| `hub_oracle.get_datum` | `lib/midgard/hub-oracle.ak:40` | read the protocol-wide config datum from an NFT-authenticated reference input |
| `settlement_queue.get_datum` | `lib/midgard/settlement-queue.ak:75` | ditto for a settlement node |
| `ledger_state.unsafe_event_to_id_data` | `lib/midgard/ledger-state.ak:86` | `builtin.unconstr_fields` → first field, no typed decode |
| `ledger_state.unsafe_event_to_key_value_pair` | `lib/midgard/ledger-state.ak:91` | ditto → `(id, info)` pair |
| `user_events.out_ref_to_nonce` | `lib/midgard/user-events.ak:54` | `blake2b_256(cbor.serialise(out_ref))` — per-event unique token name |
| `user_events.validate_refund` | `lib/midgard/user-events.ak:60` | refund path: pay-to-address-with-datum + MPF non-membership + time-window |
| `user_events.validate_mint` | `lib/midgard/user-events.ak:136` | shared mint/burn skeleton for deposit/withdrawal/tx-order |
| `user_events.authenticate_event` | `lib/midgard/user-events.ak:194` | nonce → witness script hash → registration cert → NFT mint → output shape |
| `user_events.validate_event_nft_burn` | `lib/midgard/user-events.ak:280` | burn NFT + deregister witness |
| `parameter_validation.apply_prehashed_param` | `lib/midgard/common/parameter-validation.ak:11` | **derive a parameterised script hash on-chain** |
| `parameter_validation.prehashed_param_no_datum_wrapper` | `lib/midgard/common/parameter-validation.ak:23` | thin wrapper making the param explicit |
| `witness.validate_witness_redeemer` | `lib/midgard/user-events/witness.ak:20` | assert a `Publish` (de)registration cert for the expected script |
| `fraud_proof.get_tx_root_validate_block_hash` | `lib/midgard/fraud-proof/common/utils.ak:33` | read tx root from a state-queue ref input, bind to the thread token name |
| `fraud_proof.get_prev_utxo_root` | `…/utils.ak:57` | ditto for `prev_utxos_root` |
| `fraud_proof.validate_membership_proof` / `validate_exclusion_proof` / `validate_exclusion_utxo_proof` | `…/utils.ak:80,104,128` | Merkle proofs delegated to a staking validator |
| `fraud_proof.validate_step_output` | `…/utils.ak:153` | one computation-thread step → next step's script, token forwarded |
| `fraud_proof.validate_final_step_output` | `…/utils.ak:193` | last step mints the fraud token, burns the thread token |
| `fraud_proof.handle_cancel` | `…/utils.ak:233` | burn thread token + return ADA to the prover |
| `fraud_proof.validate_ct_token_burn` | `…/utils.ak:252` | exactly-one-token burn |
| `fraud_proof.validate_ada_return_to_fraud_prover` | `…/utils.ak:270` | min-ADA refund at a redeemer-given output index |
| `fraud_proof.get_singleton_ct_token` | `…/utils.ak:291` | thread token name from own input |
| `fraud_proof.validate_output_to_fraud_prover` | `…/utils.ak:301` | output pays a pubkey credential |
| `no_active_operators_minted` | `validators/active-operators.ak:363` | net-zero mint under a policy |
| `tx_interval_contained` | `validators/scheduler.ak:242` | validity range fully inside `[start,end]` |
| `validate_output_to_initial_fraud_proof_step` | `validators/computation-thread.ak:148` | exact output shape: datum + 1 token + `flatten(value).length == 2` |

## 1.2 Recurring validation idioms

### auth-nft-check (3 flavours, all in one helper file)

```aiken
// lib/midgard/common/utils.ak:141
pub fn get_authentic_input_with_policy_at(
  inputs: List<Input>, nft_policy_id: PolicyId, input_index: Int,
) -> Input {
  expect Some(Input { output: Output { value: input_value, .. }, .. } as found_input) =
    list.at(inputs, input_index)
  expect (input_nft_policy_id, _, 1) =
    get_single_asset_from_value_apart_from_ada(input_value)
  expect input_nft_policy_id == nft_policy_id
  found_input
}
```

The strongest variant additionally requires *the address's payment credential to equal
the NFT policy id* — a self-minted beacon that cannot be forged at another address:

```aiken
// lib/midgard/common/utils.ak:179
pub fn get_authentic_input_of(
  inputs: List<Input>, address_script_hash: ScriptHash,
  nft_token_name: ByteArray, input_index: Int,
) -> Input {
  expect Some(Input { output: Output {
      address: Address { payment_credential: Script(refs_script_hash), .. },
      value: refs_value, .. }, .. } as found_ref_input) = list.at(inputs, input_index)
  let refs_nft_triplet = get_single_asset_from_value_apart_from_ada(refs_value)
  expect and {
      refs_script_hash == address_script_hash,
      refs_nft_triplet == (address_script_hash, nft_token_name, 1),
    }
  found_ref_input
}
```

Used to read the protocol-wide config ("hub oracle") — the single indirection point
every other script goes through:

```aiken
// lib/midgard/hub-oracle.ak:40
pub fn get_datum(
  reference_inputs: List<Input>, hub_oracle_script_hash: ScriptHash, hub_ref_input_index: Int,
) -> Datum {
  expect Input { output: Output { datum: InlineDatum(hub_datum_data), .. }, .. } =
    utils.get_authentic_input_of(
      reference_inputs, hub_oracle_script_hash, asset_name, hub_ref_input_index)
  expect hub_datum: Datum = hub_datum_data
  hub_datum
}
```

### one-shot / init-deinit gated by a companion NFT

Midgard does not use a `TxOutRef` one-shot for its list roots. Instead every list's
`Init` is gated on the hub-oracle NFT being minted in the *same* transaction, which is
itself the one-shot:

```aiken
// validators/state-queue.ak:56
when redeemer is {
  Init -> and {
      quantity_of(self.mint, hub_oracle_script_hash, hub_oracle_asset_name) == 1,
      unordered.init(node_in_outputs, node_mint, policy_id),
    }
  Deinit -> and {
      quantity_of(self.mint, hub_oracle_script_hash, hub_oracle_asset_name) == -1,
      unordered.deinit(node_outputs, node_mint, policy_id),
    }
```

### state-machine step: exactly-one-in / exactly-one-out, by redeemer index

```aiken
// validators/settlement-queue.ak:57
// 2. The spent input must be reproduced as a settlement node with a resolution claim.
let Output { address: input_address, value: input_value, .. } =
  utils.get_own_input_at(tx.inputs, own_out_ref, node_input_index)
expect Some(Output { address: cont_output_address, value: cont_output_value,
                     datum: InlineDatum(cont_output_datum), .. }) =
  tx.outputs |> list.at(node_output_index)
expect input_address == cont_output_address
expect input_value == cont_output_value
// TODO: inefficient but implicitly safe
expect output_node_data == NodeData { ..input_node_data, resolution_claim: output_resolution_claim }
```

Note the last line: **rebuild the expected datum from the input datum with only the
intended field changed, then compare wholesale.** Midgard's own comment calls it
"inefficient but implicitly safe". Binocular independently reinvents this (§4.3).

### linked-list insert/remove proofs

```aiken
// validators/state-queue.ak:99
expect
  unordered.append_unsafe(
    parsed_header_node_datum.key,
    header_node_output_index,
    previous_header_node_output_index,
    node_in_outputs, node_outputs, node_mint,
  )
// (5) The key field of header_node must be the hash of its data field.
expect
  parsed_header_node_datum.key == Key(
    crypto.sha2_256(parsed_header_node_block_data_as_bytearray),
  )
```

Non-membership by *ordered* list covering — used to prove an operator is not already
registered/active/retired:

```aiken
// validators/registered-operators.ak:103
ordered.prove_is_not_member(
  parsed_hub_oracle_datum.active_operators,
  ordered.Key(key_to_prepend),
  active_operator_ref_input.output,
),
```

Root/tail proofs, used to bound the queue:

```aiken
// validators/state-queue.ak:232
expect unordered.prove_is_root_node(policy_id, confirmed_state_node_input.output)
expect unordered.prove_is_root_node(policy_id, confirmed_state_node_output)
...
// validators/state-queue.ak:394
and {
  fraud_proof_block_hash == fraudulent_operator,
  unordered.prove_is_last_node(policy_id, removed_node_input.output),
}
```

### withdraw-zero-forward (merkelized validator)

Heavy MPF proof verification is moved out of the spending script into a staking
validator; the spending script only checks that the *withdrawal redeemer* equals the
inputs it wants proved.

```aiken
// lib/midgard/common/utils.ak:349
pub fn plutarch_phas(
  mpf_root: ByteArray, key: Data, value: Data,
  membership_proof: mpf.Proof, redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Bool {
  merkelized_validator.generic_delegated_validation(
    staking_validator: env.plutarch_phas_validator_hash,
    withdraw_redeemer_validator: fn(phas_validator_redeemer: Data) -> Bool {
      expect [ redeemer_root_data, redeemer_key_data, redeemer_value_data,
               redeemer_proof_data, .. ] = builtin.un_list_data(phas_validator_redeemer)
      let mpf_root_data: Data = mpf_root
      let proof_data: Data = membership_proof
      and {
        redeemer_root_data == mpf_root_data,
        redeemer_key_data == key,
        redeemer_value_data == value,
        redeemer_proof_data == proof_data,
      }
    },
    redeemers: redeemers,
  )
}
```

Note `builtin.un_list_data` + positional `Data` comparison — no typed decoding at all.

### spend-forwards-to-mint

The whole spending validator of a linked-list node is one call: all real logic lives in
the minting policy, and the spend only checks that the mint happened.

```aiken
// validators/state-queue.ak:26
validator spend(fraud_proof_catalog_mint_script_hash: ByteArray) {
  spend(_datum, _redeemer, _input, self: Transaction) {
    let tokens = tokens(self.mint, fraud_proof_catalog_mint_script_hash)
    unordered.list_state_transition(tokens)
  }
  else(_) { fail }
}
```

Same shape at `validators/registered-operators.ak:25` and
`validators/settlement-queue.ak:43`.

### cross-script coordination via redeemer indices ("read my sibling's redeemer")

This is Midgard's characteristic move: rather than re-deriving another script's facts,
a validator points at the *other script's redeemer* by index, asserts the purpose, and
trusts the sibling to have validated its own arguments.

```aiken
// validators/state-queue.ak:271
// Note: we trust the redeemer arguments provided to settlement_queue
// because we expect it to check them.
expect Some(settlement_queue_redeemer_pair) =
  self.redeemers |> list.at(settlement_queue_redeemer_index)
expect settlement_queue_redeemer_pair.1st == Mint(settlement_queue_script_hash)
expect settlement_queue.AppendSettlementNode { key_to_add, .. }: settlement_queue.MintRedeemer =
  settlement_queue_redeemer_pair.2nd
expect key_to_add == header_node_key
```

and the generic helper:

```aiken
// lib/midgard/common/utils.ak:338
pub fn get_redeemer_at(
  redeemers: Pairs<ScriptPurpose, Redeemer>, expected_purpose: ScriptPurpose, redeemer_index: Int,
) -> Redeemer {
  expect Some(redeemer_pair) = redeemers |> list.at(redeemer_index)
  expect redeemer_pair.1st == expected_purpose
  redeemer_pair.2nd
}
```

Bidirectional example — the state queue asserts the active-operator node is being spent
with the right redeemer *and* the active-operators script asserts the state queue is
minting with the right redeemer (`validators/active-operators.ak:63-88`).

### validity-range checks (deadline, maturity, containment)

```aiken
// validators/state-queue.ak:143
// (9) The `end_time` of `header_node` must match the transaction's time-validity upper bound.
expect Finite(valid_to) = self.validity_range.upper_bound.bound_type
expect header_node_block_data.end_time == valid_to
```

```aiken
// validators/state-queue.ak:240
// (3) `header_node` must be mature --- the lower bound of the transaction
//     validity interval meets or exceeds the sum of the `end_time` field of
//     `header_node` and the Midgard `maturity_duration` protocol parameter.
expect Finite(valid_from) = self.validity_range.lower_bound.bound_type
expect valid_from >= end_time + maturity_duration
```

```aiken
// validators/scheduler.ak:242
fn tx_interval_contained(validity_range: ValidityRange, start_time: Int, end_time: Int) {
  expect Finite(valid_from) = validity_range.lower_bound.bound_type
  expect Finite(valid_to) = validity_range.upper_bound.bound_type
  start_time <= valid_from && valid_to <= end_time
}
```

Deadline recorded *into* a datum from the validity range (so it cannot be backdated):

```aiken
// validators/registered-operators.ak:91
registered_operator_data.activation_time == valid_to + registration_duration,
```

### must-send-value-to-address-with-datum (payout check)

```aiken
// lib/midgard/user-events.ak:80  (validate_refund)
expect and {
    // 1. The specified address must be the recepient.
    output_address == refund_address,
    // 2. The whole value must get refunded (excluding the NFT).
    output_value == (own_value |> assets.add(event_nft_policy_id, nonce_asset_name, -1)),
    // 3. The attached datum must be as specified.
    output_datum == refund_datum,
  }
```

The `value |> assets.add(policy, name, -1)` trick — express "the same value minus the
beacon" as a value expression rather than a per-asset comparison — recurs everywhere
(`deposit.ak:97`, `withdrawal.ak:101-105`, `tx-order.ak:85`).

Escrow hand-off does it in both directions in one expression:

```aiken
// validators/user-events/withdrawal.ak:100
expect
  output_value == (
    own_value
      |> assets.add(withdrawal_policy_id, burn_asset_name, -1)
      |> assets.add(escrow_policy_id, burn_asset_name, 1)
  )
```

### protocol-fee-cut / slashing

Midgard enforces the operator slash *through the transaction fee*, so the burned bond
goes to the block producer, not the prover:

```aiken
// validators/active-operators.ak:254
self.fee >= slashing_penalty,
```

Also `validators/registered-operators.ak:226`. Bond size is enforced at registration
with a bare-ADA quantity check:

```aiken
// validators/registered-operators.ak:92
quantity_of(registered_node_output.value, "", "") == required_bond,
```

### merkle / MPT membership

Midgard uses `aiken-lang/merkle-patricia-forestry` (`mpf.Proof`), and always through
the withdraw-0 indirection (`plutarch_phas` / `plutarch_pexcludes`, §1.2). Real usage:

```aiken
// validators/user-events/deposit.ak:107
plutarch_phas(deposits_root, deposit_event_id, deposit_event_info, membership_proof, redeemers),
```

```aiken
// lib/midgard/user-events.ak:93   (refund of an event never included in a block)
SkippedEvent { non_membership_proof } -> and {
    plutarch_pexcludes(events_root, event_id, non_membership_proof, redeemers),
    inclusion_time <= settlement_end_time,
    inclusion_time >= settlement_start_time,
  }
```

The plain binary-Merkle side lives in the fraud-proof steps
(`lib/midgard/fraud-proof/common/utils.ak:80/104/128`).

## 1.3 Anti-double-satisfaction and tagging/nonce/uniqueness

**(a) `singular_utxo_indexer` — one input paired with one output, both named by the
redeemer.** All three user-event spend validators are wrapped in it, so a transaction
cannot satisfy two event UTxOs with one payout:

```aiken
// validators/user-events/deposit.ak:76
singular_utxo_indexer.spend(
  fn(_in_ix, input, spend_purpose, _out_ix, output) { … },
  purpose, input_index, output_index, own_out_ref, tx,
)
```

**(b) Per-event nonce = hash of the seed outref.** Every user event gets a token whose
*asset name* is derived from a consumed `OutputReference`, making it globally unique and
un-replayable:

```aiken
// lib/midgard/user-events.ak:54
pub fn out_ref_to_nonce(out_ref: OutputReference) -> AssetName {
  cbor.serialise(out_ref) |> blake2b_256
}
```

```aiken
// lib/midgard/user-events.ak:210
// Grab output reference of the specified nonce input.
expect Some(Input { output_reference: nonce_input_out_ref, .. }) = inputs |> list.at(nonce_input_index)
let nonce = out_ref_to_nonce(nonce_input_out_ref)
// 1. This event NFT must be minted with a quantity of 1.
expect assets.quantity_of(mint, own_policy, nonce) == 1
…
// 4. Event's ID must be the same as the specified spent input.
expect event_id == nonce_input_out_ref_data
```

**(c) The staking-credential uniqueness registry — the most unusual trick in the
corpus.** For each event, a *parameterised staking script* is derived from the nonce,
and the event NFT can only be minted in a transaction that **registers** that stake
credential (and only burned in one that **deregisters** it). The ledger allows a given
credential to be registered at most once at a time, so registration itself is the
uniqueness lock, and it is reusable after deregistration.

```aiken
// lib/midgard/user-events.ak:222
let expected_witness_script_hash =
  parameter_validation.apply_prehashed_param(
    version: 3, prefix: witness_script_prefix, param: nonce)
// 2. Validate this script hash is being registered.
// 3. Validate the policy ID passed in its redeemer is correct.
expect validate_witness_redeemer(
    expected_witness_script_hash: expected_witness_script_hash,
    witness_redeemer_index: witness_registration_redeemer_index,
    for_registration: True, event_policy_id: own_policy, redeemers: redeemers)
```

```aiken
// lib/midgard/user-events/witness.ak:20
pub fn validate_witness_redeemer(…) -> Bool {
  expect Some(witness_redeemer_pair) = redeemers |> list.at(witness_redeemer_index)
  let present_witness_script_hash =
    if for_registration {
      expect Publish { certificate: RegisterCredential { credential: Script(h), .. }, .. } =
        witness_redeemer_pair.1st
      h
    } else {
      expect Publish { certificate: UnregisterCredential { credential: Script(h), .. }, .. } =
        witness_redeemer_pair.1st
      h
    }
  let expected_witness_redeemer: Data = MintOrBurn { target_policy: event_policy_id }
  and {
    present_witness_script_hash == expected_witness_script_hash,
    witness_redeemer_pair.2nd == expected_witness_redeemer,
  }
}
```

The witness script itself binds the certificate direction to the mint sign:

```aiken
// validators/user-events/witness.ak:22
MintOrBurn { target_policy } -> {
  let mint_qty = tx.mint |> assets.quantity_of(target_policy, nonce)
  when cert is {
    RegisterCredential { .. } -> mint_qty == 1
    UnregisterCredential { .. } -> mint_qty == -1
    _ -> False
  }
}
```

and even offers a *proof-of-non-registration* by register-then-unregister in one tx
(`validators/user-events/witness.ak:31-61`).

**(d) On-chain derivation of a parameterised script hash.** Rather than applying a
parameter off-chain and hard-coding the result, Midgard reconstructs the applied
script's CBOR and hashes it:

```aiken
// lib/midgard/common/parameter-validation.ak:11
const postfix: ByteArray = #"0001"
pub fn apply_prehashed_param(version: Int, prefix: ByteArray, param: ByteArray) -> ScriptHash {
  builtin.integer_to_bytearray(True, 1, version)
    |> bytearray.concat(prefix)
    |> bytearray.concat(param)
    |> bytearray.concat(postfix)
    |> blake2b_224
}
```

(`prefix` is the constant CBOR prefix of the compiled witness script, hard-coded at
`lib/midgard/user-events/witness.ak:17`.)

**(e) Redeemer-carried tag binding two scripts to the same UTxO:**

```aiken
// validators/user-events/withdrawal.ak:109
expect escrow.MintRedeemer { withdrawal_utxo_out_ref }: escrow.MintRedeemer =
  utils.get_redeemer_at(redeemers, Mint(escrow_policy_id), escrow_mint_redeemer_index)
expect withdrawal_utxo_out_ref == own_out_ref
```

**(f) Computation-thread token as a serialised program counter.** The fraud-proof
machine's token name is `fraud_category_key ++ block_hash`, and each step forwards it to
the *next* step's script address:

```aiken
// validators/computation-thread.ak:61
let token_to_mint_asset_name =
  bytearray.concat(parsed_fraud_proof_catalogue_node_datum_key,
                   parsed_frauded_state_queue_node_app_data.utxos_root)
and {
  quantity_of(self.mint, policy_id, token_to_mint_asset_name) == 1,
  utils.quantity_of_policy_id(self.mint, policy_id) == 1,   // and nothing else
  …
}
```

```aiken
// lib/midgard/fraud-proof/common/utils.ak:171
// (5) Validate output goes to next validator
expect Script(hash) = output_to_next_step.address.payment_credential
expect hash == next_validator_hash
// (6) Validate CT token forwarding and no other minting/burning
expect Pair(ct_token_asset_name, 1) ==
  get_singleton_asset_with_policy(output_to_next_step.value, ct_token_policy_id)
```

and the token name is bound back to the disputed block:

```aiken
// lib/midgard/fraud-proof/common/utils.ak:50
// Get CT token and check match with the block hash
expect drop(ct_token_asset_name, n: 4) == block_hash
```

## 1.4 Efficiency tricks

**Direct `Data` destructuring instead of typed decoding** — the most reused one:

```aiken
// lib/midgard/ledger-state.ak:86
pub fn unsafe_event_to_id_data(event_datum_data: Data) -> Data {
  expect [id_data, ..] = builtin.unconstr_fields(event_datum_data)
  id_data
}

pub fn unsafe_event_to_key_value_pair(event_datum_data: Data) -> Pair<Data, Data> {
  expect [id_data, info_data, ..] = builtin.unconstr_fields(event_datum_data)
  Pair(id_data, info_data)
}
```

Call sites explicitly justify skipping the decode because authenticity was already
established by an NFT:

```aiken
// validators/settlement-queue.ak:212
// Unsafe coercion is fine here as we have already validated the
// authenticity of the UTxO.
let Pair(event_key, event_value) = ledger_state.unsafe_event_to_key_value_pair(event_datum_data)
```

Partial structural coercion of a foreign object (an L2 output supplied in the redeemer):

```aiken
// validators/user-events/withdrawal.ak:208
// Unsafely destructuring `l2_output_data`, assuming it is structured as an `Output`.
expect [l2_output_address_data, l2_output_value_data, ..] = builtin.unconstr_fields(l2_output_data)
expect l2_output_address: Address = l2_output_address_data
…
// Coerce the second data under `l2_output_data` into a `ValuePairs`.
// `Value` is not allowed as its an opaque type.
expect l2_output_value: ValuePairs = l2_output_value_data
```

**`ValuePairs` — a datum-safe mirror of `Value`.** Because `Value` is opaque and its
`Data` shape is not storage-stable, Midgard declares
`pub type ValuePairs = Pairs<PolicyId, Pairs<AssetName, Int>>`
(`lib/midgard/common/types.ak:12`) for datums, plus comparison helpers
(`authentic_value_has_tokens`, `compare_authentic_value_with_target`,
`lib/midgard/common/utils.ak:248,309`).

**Parallel two-list fold, no `zip` allocation:**

```aiken
// lib/midgard/common/utils.ak:264
pub fn zip_foldr(self: List<a>, bs: List<b>, acc: c, with: fn(a, b, c) -> c) -> c {
  when self is {
    [] -> acc
    [x, ..xs] -> when bs is {
        [] -> acc
        [y, ..ys] -> { let c = zip_foldr(xs, ys, acc, with); with(x, y, c) }
      }
  }
}
```

**Exact value shape via `flatten` length** (cheap "ADA + exactly one token" assertion):

```aiken
// validators/computation-thread.ak:160
let correctValue = and {
    quantity_of(output_to_init_step.value, own_hash, token_asset_name) == 1,
    list.length(flatten(output_to_init_step.value)) == 2,
  }
```

**Single-pass fold over the value's policy list, computing two results at once**
(counting tokens and validating the beacon in one traversal):

```aiken
// validators/user-events/deposit.ak:188
let output_deposit_nft_is_valid, total_token_count <-
  list.foldl2(
    output_deposit_value |> assets.to_dict |> dict.to_pairs, False, 0,
    fn(policy_tokens_pair, beacon_found, token_count_so_far, return) {
      if !beacon_found && policy_tokens_pair.1st == own_policy {
        expect [output_deposit_nft_tn_qty] = policy_tokens_pair.2nd |> dict.to_pairs
        expect output_deposit_nft_tn_qty == Pair(l2_id, 1)
        return(True, token_count_so_far)
      } else {
        return(beacon_found, token_count_so_far + dict.size(policy_tokens_pair.2nd))
      }
    })
```

**Input-index hints everywhere.** Every redeemer in Midgard carries indices instead of
letting the script search: `MintRedeemer.CommitBlockHeader` alone carries five
(`lib/midgard/state-queue.ak:146-153`). The safety pattern is always "index + assert":

```aiken
// lib/midgard/common/utils.ak:87
pub fn get_own_input_at(inputs: List<Input>, own_out_ref: OutputReference, input_index: Int) -> Output {
  expect Some(Input { output: own_utxo, output_reference: spent_own_out_ref }) =
    list.at(inputs, input_index)
  expect (own_out_ref == spent_own_out_ref)?
  own_utxo
}
```

**Raw byte-level key handling** — comparing/incrementing keys as bytes/ints without
decoding structures:

```aiken
// validators/fraud-proof/catalogue.ak:65
let new_node_key_as_int = bytearray.to_int_big_endian(new_node_key)
let old_node_key_as_int = bytearray.to_int_big_endian(old_node_key)
…
bytearray.length(new_node_key) == 4,
if ordered.prove_is_root_node(policy_id, old_node_input.output) { new_node_key_as_int == 0 }
else { and { old_node_key_as_int < 4095, new_node_key_as_int == old_node_key_as_int + 1 } },
```

```aiken
// validators/scheduler.ak:209  (direct builtin, avoids a typed comparison)
expect builtin.less_than_equals_bytearray(datum.operator, root_node_link_as_bytearray)
```

**Idioms present (Midgard):**
`auth-nft-check`, `one-shot-mint` (companion-NFT form), `single-script-input`,
`output-to-self-with-datum`, `withdraw-zero-forward`, `spend-forwards-to-mint`,
`validity-range-check`, `input-index-hint`, `data-level-compare`, `merkle-proof`
(MPF membership + non-membership), `linked-list-node` (ordered + unordered),
`min-ada`, `protocol-fee-cut` (slashing via `tx.fee`), `tag-with-txoutref`,
`no-double-satisfaction`, `cross-script-redeemer-assert`,
`stake-credential-uniqueness-registry`, `onchain-script-hash-derivation`,
`datum-rebuild-and-compare`, `exact-value-shape`.

---

# 2. Hydrozoa — `/Users/nau/projects/lantr/hydrozoa/cardano-onchain`

Scalus. Two validators (`DisputeResolutionValidator`, `RuleBasedTreasuryValidator`)
implementing the "rule-based regime" fallback of an L2 head: peers vote on the latest
committed state, votes are tallied by contracting a linked list, then the treasury is
resolved and evacuated against a KZG/BLS accumulator.

## 2.1 Helpers Hydrozoa defines itself

| Name | Signature | Purpose | File:line |
|---|---|---|---|
| `Value.containsCurrencySymbol` | `(Value)(PolicyId) -> Boolean` | any token of this policy present (skips the leading ADA entry) | `…/lib/cardano/scalus/ledger/api/ValueExtensions.scala:17` |
| `Value.containsExactlyOneAsset` | `(Value)(PolicyId, TokenName, BigInt) -> Boolean` | value is ADA + exactly `amount` of exactly that one token | `ValueExtensions.scala:29` |
| `Value.onlyNonAdaAsset` | `Value -> (PolicyId, TokenName, BigInt)` | the unique non-ADA asset, or `fail` | `ValueExtensions.scala:51` |
| `Value.unary_-` | `Value -> Value` | negate (for burn maps) | `ValueExtensions.scala:74` |
| `TxOut.inlineDatumOfType[T]` | `(TxOut)(using FromData[T]) -> T` | inline datum or fail | `…/ledger/api/TxOutExtensions.scala:20` |
| `ByteString.< <= > >= at take slice drop` | builtin wrappers | ordering + slicing sugar over `ByteString` | `…/ledger/api/ByteStringExtensions.scala:1-33` |
| `findRegimeReference` | `(TxInfo, PolicyId) -> RuleBasedRegimeDatum` | locate the config UTxO by CIP-67-prefixed beacon under `headMp` | `…/plutus/RuleBasedTreasuryScript.scala:145` |
| `maxVote` | `(VoteStatus, VoteStatus) -> VoteStatus` | vote-precedence join used by the tally fold | `…/plutus/DisputeResolutionScript.scala:635` |
| `Unresolved.resolve` / `Resolved.evacuate` | datum transitions | single-source the *only* legal datum transitions; validators build the expected output from them | `…/state/TreasuryState.scala:51,65` |
| `checkMembership` / `getG2Commitment` / `getFinalPolyScalus` | BLS12-381 | KZG subset-membership proof over evacuation keys | `RuleBasedTreasuryScript.scala:520,494,486` |
| `verifySignatures` / `verifyCoilSignatures` | local `@tailrec` | position-aligned multisig + sparse quorum multisig | `DisputeResolutionScript.scala:279,302` |

## 2.2 Idioms

### auth-nft-check with a CIP-67 prefix (beacon by *label*, not by exact name)

```scala
// RuleBasedTreasuryScript.scala:135
def cip67BeaconTokenPrefix: ByteString = hex"01349900"
// CIP-67 prefix of the HRWT (Hydrozoa regime witness token), tag 4798
def cip67RegimeTokenPrefix: ByteString = hex"012be4e0"

// RuleBasedTreasuryScript.scala:145
def findRegimeReference(tx: TxInfo, headMp: PolicyId): RuleBasedRegimeDatum =
    tx.referenceInputs
        .find(i =>
            i.resolved.value.toSortedMap.get(headMp) match
                case Some(tokens) =>
                    tokens.toList match
                        case List.Cons(tokenNameAndAmount, tail) =>
                            tail.isEmpty
                            && tokenNameAndAmount._2 == BigInt(1)
                            && tokenNameAndAmount._1.take(4) == cip67RegimeTokenPrefix
                        case _ => false
                case None => false
        )
        .getOrFail(RegimeReferenceNotFound)
        .resolved
        .inlineDatumOfType[RuleBasedRegimeDatum]
```

The doc comment states the trust argument explicitly: *"The token policy is the
authentication: only the head multisig script can mint under `headMp`, and it mints
exactly one HRWT."*

### no-double-satisfaction: "no other input carries this head's policy"

Hydrozoa's anti-DS technique is a *negative* filter over inputs keyed by policy, not by
address or txid. The comment records why an earlier txid-based filter was wrong.

```scala
// DisputeResolutionScript.scala:178  (Vote branch)
// Bound the transaction to a single ballot box by token identity (a txid filter
// misses co-spent boxes once ratcheting diverges a box's source tx from fallback).
require(
  tx.inputs
      .filter(i =>
          (i.outRef !== ownRef)
              && i.resolved.value.containsCurrencySymbol(headMp)
      )
      .isEmpty,
  VoteOnlyOneVoteUtxoIsSpent
)
```

```scala
// DisputeResolutionScript.scala:445  (Tally branch)
// No other input may hold any token of this head's policy (contCs). This
// prevents co-spending unrelated ballot boxes or the treasury in the same Tally tx.
require(
  tx.inputs
      .filter(i =>
          (i.outRef !== continuingInputId) && (i.outRef !== removedInputId)
              && (i.resolved.value.containsCurrencySymbol(contCs))
      )
      .isEmpty,
  NoOtherInputs
)
```

Same shape again in the `Abstain` branch (`:593`).

### single-continuing-output, found by value equality

```scala
// DisputeResolutionScript.scala:343
val voteOutput = tx.outputs.filter(o => o.value === voteInput.value) match
    case List.Cons(o, tail) =>
        require(tail.isEmpty, VoteVoteOutputExists)
        o
    case _ => fail(VoteVoteOutputExists)
require(voteOutput.address === voteInput.address, VoteVoteOutputExists)
```

`filter … then require(tail.isEmpty)` (rather than `find`) is the deliberate
"exactly one" idiom — Binocular states the same rule explicitly (§4.2).

### reference-script rejection as a DoS guard (unusual, worth stealing)

```scala
// DisputeResolutionScript.scala:351
// Reject an attached reference script — would bloat the utxo and could push
// downstream Tally / Resolve over the tx-size limit (denial of evacuation).
voteOutput.referenceScript match
    case None    => ()
    case Some(_) => fail(VoteOutputNoScriptRef)
```

Repeated at `:528` (Tally) and `:619` (Abstain). Midgard does the same thing by
destructuring `reference_script: None` in the output pattern
(`validators/user-events/deposit.ak:81`).

### linked-list contraction (tally by pairwise merge)

```scala
// DisputeResolutionScript.scala:436
// The key field of removedInput must be greater than the key field and equal to the
// link field of continuingInput.
val continuingDatum = continuingInput.inlineDatumOfType[VoteDatum]
val removedDatum = removedInput.inlineDatumOfType[VoteDatum]
require(
  removedDatum.key > continuingDatum.key && removedDatum.key == continuingDatum.link,
  KeyLinkFieldsDoNotMatch
)
…
// DisputeResolutionScript.scala:544
// link of continuingOutput inherits removedInput's link (linked-list contraction).
require(continuingOutputDatum.link == removedDatum.link, LinkCheck)
// key of continuingOutput is preserved from continuingInput.
require(continuingOutputDatum.key === continuingDatum.key, KeyCheck)
```

The `Continuing` / `Removed` redeemer is the *role selector* for the two identical
script inputs — the same script runs twice, each invocation asserting the other input:

```scala
// DisputeResolutionScript.scala:405
val ownInput = tx.inputs.find(_.outRef === ownRef).get
val otherInput: TxInInfo =
    tx.inputs.filter(i =>
        i.resolved.address === ownInput.resolved.address && (i.outRef !== ownRef)
    ) match
        case List.Cons(other, empty) =>
            require(empty.isEmpty, TwoVotingInputsExpected)
            other
        case _ => fail(VotingInputsNotFound)
```

### min-ADA / fee handling on list contraction

Merging two UTxOs into one must not fail merely because the tx fee exceeded one side's
ADA — the check is written as a clamped residual:

```scala
// DisputeResolutionScript.scala:512
// continuingOutput must have: same address, combined tokens, and ADA at least
// continuingInput.ADA + max(0, removedInput.ADA - tx.fee). Computing residualAda
// this way avoids failing the tx if tx.fee exceeds removedInput's ADA (the
// alternative `require(removedInput.value.getLovelace >= tx.fee)` is overly
// strict and would block otherwise-valid tallies).
val residualAda = {
    val residualAda = removedInput.value.getLovelace - tx.fee
    if residualAda > 0 then residualAda else BigInt(0)
}
val continuingOutput = tx.outputs
    .filter(o =>
        o.address === continuingInput.address
            && o.value.onlyNonAdaAsset === (continuingInput.value + removedInput.value).onlyNonAdaAsset
            && o.value.getLovelace >= continuingInput.value.getLovelace + residualAda
    ) match …
```

### validity-range: deadline before / deadline after

```scala
// DisputeResolutionScript.scala:261   (voting must end before the deadline)
tx.validRange.to.boundType match {
    case IntervalBoundType.Finite(toTime) =>
        require(toTime <= treasuryDatum.deadlineVoting, VoteTimeValidityCheck)
    case _ => fail(VoteTimeValidityCheck)
}
// DisputeResolutionScript.scala:498   (tally only after it)
tx.validRange.from.boundType match {
    case IntervalBoundType.Finite(fromTime) =>
        require(treasuryDatum.deadlineVoting <= fromTime, TallyOnlyAfterVotingDeadline)
    case _ => fail(TallyValidityStartRequired)
}
```

### ratchet / monotonic version (replay protection without a nonce)

```scala
// DisputeResolutionScript.scala:192
case VoteStatus.Voted(_, prevVersionMinor) =>
    // Open phase: any multisigned SEC can ratchet this box, but only with
    // strictly higher versionMinor (foundation I8).
    require(voteRedeemer.sec.versionMinor > prevVersionMinor, VoteRatchetNotMonotonic)
```

### multisig over a serialised datum

```scala
// DisputeResolutionScript.scala:270
val msg = voteRedeemer.sec.toData |> serialiseData
require(regimeDatum.headPeers.length == voteRedeemer.headMultisig.length, VoteMultisigCheck)
```

Two verifier shapes: a position-aligned "all peers must sign" recursion (`:279`) and a
sparse `List[Option[Signature]]` quorum counter that stops at `coilQuorum` (`:302-333`).
The source carries a note to the Scalus team that `List[(CoilPeerId, Signature)]` would
be cheaper on wire — a concrete stdlib request.

### datum-transition functions as the single source of truth

```scala
// TreasuryState.scala:47
extension (self: RuleBasedTreasuryDatum.Unresolved)
    def resolve(evacuationActive: MembershipProof, versionMinor: BigInt)
        : RuleBasedTreasuryDatum.Resolved =
        RuleBasedTreasuryDatum.Resolved(
          headMp = self.headMp,
          evacuationActive = evacuationActive,
          version = (self.versionMajor, versionMinor)
        )
```

used by the validator as:

```scala
// RuleBasedTreasuryScript.scala:251
val expected = unresolvedDatum.resolve(commitment, versionMinor)
require(treasuryOutputDatum.version === expected.version, ResolveVersionCheck)
require(treasuryOutputDatum.evacuationActive === expected.evacuationActive, ResolveUtxoActiveCheck)
require(treasuryOutputDatum.headMp === expected.headMp, ResolveTreasuryInputOutputHeadMp)
```

### positional output convention (redeemer-free index hint)

```scala
// RuleBasedTreasuryScript.scala:303
// The beacon token should be preserved
// By contract, we require:
//   - The change utxo is position zero
//   - the treasury utxo in position one
//   - the tail be evacuatees
val List.Cons(_, List.Cons(treasuryOutput, evacuationOutputs)) = tx.outputs: @unchecked
```

followed by the *redirect guard* that a previous revision was missing:

```scala
// RuleBasedTreasuryScript.scala:313
// The continuing treasury output must stay at the treasury script address. The
// checks below only constrain its beacon, datum and total value — not where it
// goes; without this an Evacuate could redirect the beacon and the entire treasury
// value to an arbitrary address (cf. the Resolve branch, which pins the address).
require(treasuryOutput.address === treasuryInput.address, EvacuateTreasuryWrongAddress)
```

### value conservation across a fan-out

```scala
// RuleBasedTreasuryScript.scala:421
val evacuatedValue = evacuationOutputs.foldLeft(Value.zero)((acc, o) => acc + o.value)
val valueIsPreserved = treasuryInput.value === (treasuryOutput.value + evacuatedValue)
require(valueIsPreserved, EvacuateValueShouldBePreserved)
```

### "must make progress" anti-DoS

```scala
// RuleBasedTreasuryScript.scala:329
// An Evacuate must make progress (remove at least one utxo). A zero-evacuatee tx
// would be a permissionless no-op that just re-creates the treasury, enabling a
// UTxO-contention DoS against legitimate evacuations…
require(!evacuationKeys.isEmpty, EvacuateMustMakeProgress)
```

### reference-input authentication *by outref range* (trusted-setup ladder)

```scala
// RuleBasedTreasuryScript.scala:365
val setupRefInput = tx.referenceInputs !! setupRefInputIdx
val ladderAnchor = findRegimeReference(tx, headMp).setupG2Ladder
require(
  setupRefInput.outRef.id === ladderAnchor.id
      && setupRefInput.outRef.idx < 7,
  EvacuateSetupNotAuthenticated
)
```

A degenerate KZG setup would trivially satisfy the pairing check, so the setup UTxO must
be one of seven outputs of a recorded anchor transaction.

### burn-all-tokens deinit (inclusion, not equality)

```scala
// RuleBasedTreasuryScript.scala:461
val headTokensMint = (-tx.mint).toSortedMap.get(headMp).getOrFail(DeinitTokensNotBurned)
require(
  headTokensInput.toList.forall(tokenNameAndAmount =>
      headTokensMint.get(tokenNameAndAmount._1) match
          case Some(burned) => burned == tokenNameAndAmount._2
          case None         => false
  ),
  DeinitTokensNotBurned
)
```

Plus a hard-coded "empty accumulator" constant with an explicit stdlib request:

```scala
// RuleBasedTreasuryScript.scala:475
// TODO: comparing as bytestrings is more efficient, we want to have this constant in Scalus
require(
  utxosActive === hex"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb",
  DeinitTreasuryShouldBeEmpty
)
```
(that is the BLS12-381 G1 generator/identity compressed encoding).

### Scalus-specific gotcha recorded in the source

```scala
// VoteState.scala:61
// Explicit givens (rather than `derives` clauses on the types) so the derived instances are
// direct members of this `@Compile` object and their SIR is emitted for on-chain use…
// A clause-derived instance lands in the type's companion, whose SIR the on-chain linker
// cannot resolve (fails at script-build time, not Scala compile time).
given FromData[VoteDatum] = FromData.derived
given ToData[VoteDatum] = ToData.derived
```

**Idioms present (Hydrozoa):**
`auth-nft-check` (CIP-67 prefix form), `single-script-input`,
`output-to-self-with-datum`, `validity-range-check`, `input-index-hint` (`!!` on
reference inputs; positional output convention), `linked-list-node` (key/link,
contraction), `min-ada` (fee-aware residual), `no-double-satisfaction`
(policy-scoped input filter), `merkle-proof` (KZG/BLS accumulator instead of Merkle),
`datum-rebuild-and-compare` (via transition functions), `no-reference-script-guard`,
`monotonic-ratchet`, `value-conservation`, `must-make-progress`.
Absent: `one-shot-mint`, `withdraw-zero-forward`, `spend-forwards-to-mint`,
`protocol-fee-cut`.

---

# 3. AdaStream — `/Users/nau/projects/lantr/adastream/src/contract.scala`

Small and old-style (`scalus.ledger.api.v3`, pre-`cardano.onchain` package). Two
validators: a fraud-provable bond and a plain HTLC. Value/NFT/state-machine idioms are
entirely absent; its contribution is **crypto primitives + raw `Data` access**.

| Name | Signature | Purpose | File:line |
|---|---|---|---|
| `integerToByteString` | `BigInt -> ByteString` | positive-only wrapper over the builtin | `contract.scala:53` |
| `xorBytes` | `(BigInt, BigInt) -> BigInt` | byte XOR by binary long-division (pre-`xorByteString` fallback) | `contract.scala:57` |
| `customXor` | `(ByteString, ByteString) -> ByteString` | equal-length XOR, hand-rolled | `contract.scala:66` |
| `xor` | `(ByteString, ByteString) -> ByteString` | the builtin version that replaced it | `contract.scala:83` |
| `verifyMerkleInclusionProof` | `(Data, ByteString, ByteString, BigInt, ByteString) -> Boolean` | binary Merkle path fold, index bit selects sibling side | `contract.scala:88` |
| `verifyPreimage` | `(ByteString, ByteString) -> Boolean` | `sha2_256` preimage | `contract.scala:109` |
| `verifyFraudProof` | 9 args | combined: wrong-chunk-hash + signed claim + preimage + Merkle | `contract.scala:112` |

### merkle-proof (the plain binary form, over `BuiltinList[Data]`)

```scala
// contract.scala:88
inline def verifyMerkleInclusionProof(
    merkleProof: Data, encryptedChunk: ByteString, chunkHash: ByteString,
    chunkIndex: BigInt, encId: ByteString
): Boolean =
    val encryptedChunkAndChunkHashHash = sha2_256(appendByteString(encryptedChunk, chunkHash))
    def loop(index: BigInt, curHash: ByteString, siblings: BuiltinList[Data]): ByteString =
        if siblings.isEmpty then curHash
        else
            val sibling = unBData(siblings.head)
            val nextHash =
                if index % 2 == BigInt(0)
                then sha2_256(appendByteString(curHash, sibling))
                else sha2_256(appendByteString(sibling, curHash))
            loop(index / 2, nextHash, siblings.tail)
    val merkleRoot = loop(chunkIndex, encryptedChunkAndChunkHashHash, unListData(merkleProof))
    merkleRoot == encId
```

The proof is carried as a bare `Data` (`// List of ByteString`, `contract.scala:43`)
and walked with `unListData` / `unBData` — no typed list decode.

### data-level partial decode of the script context (efficiency)

```scala
// contract.scala:161
def bondContractValidator(ctxData: Data): Unit = {
    val bondAction = ctxData.field[ScriptContext](_.redeemer).to[BondAction]
    val info = ctxData.field[ScriptContext](_.scriptInfo)
    val infoPair = info.toConstr
    if infoPair.fst == BigInt(1) then // SpendingScript
        val BondConfig(passwordHash, encryptedId, serverPubKey, serverPubKeyHash) = info
            .field[ScriptInfo.SpendingScript](_.datum)
            .to[Option[BondConfig]]
            .getOrFail("No datum")
        require(bondContractCheck(…,
            // get PubKeyHash as a ByteString from the first signatory
            // NOTE: we assume that the first signatory is the server
            ctxData.field[ScriptContext](_.txInfo.signatories).toList))
}
```

Two things here: `.field[T](_.path)` projects into `Data` without materialising the
whole `ScriptContext`, and the purpose is dispatched by comparing the raw constructor
tag (`infoPair.fst == 1`) instead of decoding `ScriptInfo`.

The signatory check then avoids even a list search:

```scala
// contract.scala:196
// verify that the signatory is the server PubKeyHash from the BondConfig
val verifySignature = signatures.head.toByteString == serverPubKeyHash
```

### validity-range-check (HTLC expiry)

```scala
// contract.scala:231
val expired = {
    val txtime = txInfoData.field[TxInfo](_.validRange.from.boundType).to[IntervalBoundType]
    txtime match
        case IntervalBoundType.Finite(txtime) => expiration < txtime
        case _                                => false
}
val signedByOwner = {
    val signaturePubKeyHashData = txInfoData.field[TxInfo](_.signatories).toList.head
    signaturePubKeyHashData == ownerPubKeyHash
}
require(expired && expired, "HTLC is not expired")   // NB: duplicated conjunct in source
require(signedByOwner, "HTLC is not signed by the owner")
```

**Idioms present (AdaStream):**
`merkle-proof`, `validity-range-check`, `data-level-compare` (raw `Data` field
projection + constructor-tag dispatch), `hash-preimage-lock`, `signed-claim`
(ed25519 over a concatenated message).
Absent: everything token/NFT/state-machine related.

---

# 4. Binocular — `/Users/nau/projects/lantr/binocular/src/main`

Scalus. A Bitcoin light-client oracle (`BitcoinValidator`) plus the Bifrost bridge
scripts. Seven `@Compile` objects:
`oracle/BitcoinValidator.scala`, `watchtower/TreasuryMovementValidator.scala`,
`watchtower/TransactionVerifierValidator.scala`, `watchtower/PegOutVerifier.scala`,
`bitcoin/BitcoinHelpers.scala`, `bitcoin/ReorgDiagnostics.scala`, `TimeFmt.scala`.
(The peg-in/peg-out/bridge-state scripts referenced in comments are Aiken and live
outside this checkout — see `TreasuryMovementValidator.scala:270`.)

## 4.1 Helpers Binocular defines itself

### `bitcoin/BitcoinHelpers.scala` — a Bitcoin-serialisation library in UPLC

| Name | Line | Purpose |
|---|---|---|
| `calculateBlockProof` | `:70` | chainwork contribution `2^256 / (target+1)` |
| `blockHeaderHash` | `:75` | double-SHA256 of the 80-byte header |
| `compactBitsToTarget` / `targetToCompactBits` / `targetToCompactBitsV2` / `targetToCompactByteString` | `:85,:108,:146,:169` | nBits ↔ target |
| `getMedianTimePast` | `:202` | median of last 11 timestamps |
| `merkleRootFromInclusionProof` | `:209` | binary Merkle fold, index bit selects side |
| `readVarInt` | `:226` | Bitcoin VarInt → `(value, nextOffset)` |
| `parseCoinbaseTxScriptSig` / `parseBlockHeightFromScriptSig` / `getBlockHeightFromCoinbaseTx` | `:235,:249,:255` | BIP-34 height extraction |
| `getTxHash` / `stripWitnessData` / `isWitnessTransaction` | `:265,:272,:269` | txid from a segwit-serialised tx |
| `skipTxIns` / `skipTxOuts` / `skipTxIn` / `skipTxOut` | `:307,:314,:321,:326` | cursor advancement without materialising |
| `outputValueSat` / `firstInputOutpoint` / `findPegInInputIndex` | `:292,:339,:356` | targeted field reads |
| `findWitnessSectionOffset` / `skipOneWitness` / `witnessStackSize` / `witnessItem` / `isKeyPathWitness` / `isValidScriptPathWitness` / `spentViaLeaf` | `:378,:471,:493,:565,:520,:556,:591` | taproot witness inspection |
| `insertReverseSorted` | `:596` | ordered insert into a small sorted list |
| `getNextWorkRequired` / `calculateNextWorkRequired` | `:629,:652` | Bitcoin difficulty retarget |

### `oracle/BitcoinValidator.scala`

| Name | Line | Purpose |
|---|---|---|
| `accumulateBlock` | `:391` | fold one validated block into a traversal context (height, MTP window, retarget) |
| `insertAscending` / `insertionSort` | `:430,:438` | ordered insert / insertion sort for the 11-timestamp MTP window |
| `validateBlock` | `:456` | PoW + MTP + futurity + `bad-diffbits` + continuity |
| `computeChainwork` / `validateAndCollectBlocks` / `validateAndInsert` / `bestChainPath` / `splitPromotable` / `promoteAndGC` / `applyPromotions` / `computeUpdate` | `:540,:570,:633,:869,:920,:974,:1073,:1128` | fork-tree state machine |
| `forkTreeBlockCount` | `:894` | datum-size envelope guard |

### `watchtower/TreasuryMovementValidator.scala`

| Name | Line | Purpose |
|---|---|---|
| `OutputDatum.of[A]` (extension) | `:229` | inline-datum decode; `inline` **on purpose** — see §4.4 |
| `isTwoRootCommitment` | `:281` | `OP_RETURN`-tagged commitment output, length **and** prefix |
| `committedRoots` | `:303` | the two MPF roots from *exactly one* commitment output |
| `allInputOutpoints` | `:320` | parse all 36-byte outpoints of a raw BTC tx |
| `allOutputs` | `:338` | parse all `(scriptPubKey, amount)` outputs |
| `findOracleInput` | `:359` | oracle UTxO by script hash **and** oracle NFT |
| `tmInputCount` | `:385` | count inputs at own script address (anti-DS) |

### `watchtower/PegOutVerifier.scala`

| Name | Line | Purpose |
|---|---|---|
| `scanTm` | `:91` | single forward pass over a raw BTC tx proving "spends X and pays Y exactly Z" |

## 4.2 Idioms

### one-shot-mint (the textbook form)

```scala
// oracle/BitcoinValidator.scala:1403
inline override def mint(param: Data, redeemer: Data, policyId: PolicyId, tx: TxInfo): Unit = {
    val params = param.to[BitcoinValidatorParams]
    val minted = tx.mint.tokens(policyId).toData
    if minted == SortedMap.singleton(ByteString.empty, BigInt(1)).toData then
        // ensure we spend the one-shot TxOutRef
        tx.inputs
            .find(_.outRef.toData == params.oneShotTxOutRef.toData)
            .getOrFail("Must consume one-shot utxo")
        // Verify oracle output contains the NFT at the specified index
        val outputIndex = redeemer.to[BigInt]
        val oracleOutput = tx.outputs.at(outputIndex)
        val expectedValue = Value.unsafeFromList(List((policyId, List(ByteString.empty -> 1))))
        require(
          oracleOutput.value.withoutLovelace.toData == expectedValue.toData,
          "Oracle output must contain NFT and no other tokens"
        )
        // Verify oracle output goes to this script's address (policyId == script hash)
        val expectedAddress = Address(Credential.ScriptCredential(policyId), Option.None)
        require(oracleOutput.address.toData == expectedAddress.toData,
          "Oracle output must go to script address without staking")
    else
        require(minted == SortedMap.singleton(ByteString.empty, BigInt(-1)).toData,
          "can only mint 1 or burn 1 SP NFT")
}
```

Everything compared as `Data` (`.toData ==`), and the policy id doubles as the script
hash so the NFT can only land at the oracle address.

### state-machine step (continuing output found by address + NFT)

```scala
// oracle/BitcoinValidator.scala:1267
// Find continuing output: address match + oracle NFT
val continuingOutput = outputs
    .find(out =>
        out.address.toData == ownInput.address.toData
            && out.value.quantityOf(policyId, ByteString.empty) == BigInt(1)
    )
    .getOrFail("No continuing output with oracle NFT found")

// NFT preservation
require(
  ownInput.value.withoutLovelace.toData == continuingOutput.value.withoutLovelace.toData,
  "Non-ADA tokens must be preserved"
)
// ADA value can only increase (prevents draining oracle UTxO)
require(
  continuingOutput.value.lovelaceAmount >= ownInput.value.lovelaceAmount,
  "ADA value can only increase"
)
// Verify output datum matches computed chainState
val providedOutputDatum = continuingOutput.datum.toData
val expectedOutputDatum = OutputDatum.OutputDatum(computedState.toData).toData
require(providedOutputDatum == expectedOutputDatum,
  "Computed state does not match provided output datum")
```

This is the cleanest statement of the pattern in the corpus:
**recompute the next state as a pure function, then compare the whole serialised
datum**, plus "tokens preserved / ADA monotone".

### oracle datum freshness (staleness → close/reset)

```scala
// oracle/BitcoinValidator.scala:1295
case OracleAction.CloseOracle =>
    // 1. Staleness check: last confirmed block timestamp must be > closureTimeout ago
    require(
      intervalEndInSeconds - chainState.ctx.timestamps.head > params.closureTimeout,
      "Oracle is not stale"
    )
    // 2. Owner authorization
    require(tx.isSignedBy(params.owner), "Not signed by oracle owner")
    // 3. NFT must be burned
    require(
      tx.mint.tokens(policyId).toData == SortedMap.singleton(ByteString.empty, BigInt(-1)).toData,
      "Must burn oracle NFT"
    )
```

### bounded validity window (so recorded deltas are trustworthy)

```scala
// oracle/BitcoinValidator.scala:1208
val intervalStartMs = tx.validRange.from.finiteOrFail("Must have finite interval start")
val intervalEndMs = tx.validRange.to.finiteOrFail("Must have finite interval end")
require(intervalEndMs - intervalStartMs <= MaxValidityWindow, "Validity interval too wide")
```

with ~25 lines of comment justifying the choice of `validRange.to` as "now"
(`:1213-1238`) — the aging check is shown to be reference-invariant.

### datum shape validation on an owner-supplied state (anti-brick)

```scala
// oracle/BitcoinValidator.scala:1349
// 4. The replacement state must be a structurally valid ChainState: every field
//    is forced so a malformed datum fails HERE, not on the next spend (which
//    would brick the oracle …)
require(newState.confirmedBlocksRoot.length == BigInt(32), "confirmedBlocksRoot must be 32 bytes")
require(newState.ctx.currentBits.length == BigInt(4), "currentBits must be 4 bytes")
require(newState.ctx.timestamps.length == MedianTimeSpan, "timestamps must have MedianTimeSpan entries")
require(
  newState.ctx.timestamps.forall(ts => ts > BigInt(0) && ts <= intervalEndInSeconds + MaxFutureBlockTime),
  "timestamps must be positive and within the futurity bound"
)
require(forkTreeBlockCount(newState.forkTree) <= params.maxBlocksInForkTree,
  "Fork tree exceeds maxBlocksInForkTree")
```

The comments spell out that `forall` / `length` are being used **as forcing
operations** on a lazily-decoded structure.

### merkle-proof + MPF membership together

```scala
// watchtower/TreasuryMovementValidator.scala:430
case TmSpendRedeemer.Confirm(proof) =>
    // spec [CTM-1] recompute the txid from the witness-stripped serialization — never trust the caller.
    val txid = BitcoinHelpers.getTxHash(signedBtcTx)
    // spec [CTM-3] the block is in the oracle's confirmed-blocks trie.
    val oracleState = findOracleInput(tx.referenceInputs, oracleScriptHash).datum.of[ChainState]
    val blockHash = BitcoinHelpers.blockHeaderHash(proof.blockHeader)
    MPF(oracleState.confirmedBlocksRoot).verifyMembership(blockHash, blockHash, proof.blockMpfProof)
    // spec [CTM-2] the header … commits to txid at txIndex.
    val computedRoot = BitcoinHelpers.merkleRootFromInclusionProof(
      proof.txMerkleProof, txid, proof.txIndex)
    require(computedRoot == proof.blockHeader.merkleRoot, "TM tx not in block merkle root")
```

### config-NFT runtime indirection (breaks a parameterisation cycle)

```scala
// watchtower/TreasuryMovementValidator.scala:467
// spec [CTM-28] the singleton policy comes from the Config reference input
// at RUNTIME ([PAR-1]): the bridge_state script takes THIS script's hash as
// its own parameter, so a compile-time link would be a cycle.
val cfgOut = tx.referenceInputs
    .find(refIn => refIn.resolved.value.quantityOf(configNftPolicy, configNftName) == BigInt(1))
    .getOrFail("TM confirm: no config reference input")
    .resolved
val bssPolicy = cfgOut.datum.of[ConfigDatum].bridgeStatePolicy
```

Structurally identical to Midgard's hub oracle: **one NFT-authenticated config UTxO
carrying every other script's policy id / address.** Two independent protocols invented
the same indirection.

### exactly-one-of by `filter`, never `find`

```scala
// watchtower/TreasuryMovementValidator.scala:303
// `filter` then match, NOT `find`: `find` stops at the first commitment and would silently
// accept a TM carrying a second one.
outs.filter(out => isTwoRootCommitment(out.scriptPubKey)) match
    case ScalusList.Cons(only, ScalusList.Nil) => …
    case ScalusList.Nil => fail("TM confirm: missing two-root commitment")
    case _              => fail("TM confirm: multiple two-root commitments")
```

### tagged OP_RETURN commitment parsing (length **and** prefix)

```scala
// watchtower/TreasuryMovementValidator.scala:277
/** Is this `scriptPubKey` a two-root commitment? Length AND prefix, so a short script cannot
  * slice past its end and a 71-byte payment script cannot masquerade as one. …*/
def isTwoRootCommitment(scriptPubKey: ByteString): Boolean =
    scriptPubKey.length == TwoRootCommitmentScriptLength &&
        scriptPubKey.slice(0, TwoRootCommitmentPrefixLength) == TwoRootCommitmentPrefix
```

### timestamp anchored to the validity range (no backdating)

```scala
// watchtower/TreasuryMovementValidator.scala:581
val txHappenedBefore = tx.validRange.to.finiteOrFail(
  "TM mint: validity range upper bound must be finite")
// The tx cannot be included after `txHappenedBefore`, so requiring
// `created == txHappenedBefore` makes `created` a guaranteed upper bound on the real
// posting time: the GC grace period … can start late but never early, and cannot be backdated.
require(record.created == txHappenedBefore,
  "TM mint: created field must be equal to `tx.validRange.to`")
```

and consumed as:

```scala
// watchtower/TreasuryMovementValidator.scala:543
val timeout = record.created + GcGraceMs
// spec [CTM-8] the validity interval lies ENTIRELY after the boundary.
require(tx.validRange.isEntirelyAfter(timeout), "TM GC: grace period has not elapsed")
require(tx.isSignedBy(record.creator), "TM GC: not signed by the record's creator")
```

### min-ADA reclaim / garbage collection

`GcGraceMs = BigInt(30) * 24 * 3600 * 1000` (`:239`, with a note that the `Int`
literal product would overflow) gates a creator-only reclaim of a dead record's min-ADA
— a rare on-chain treatment of min-ADA as a first-class resource.

## 4.3 Anti-double-satisfaction — the sharpest example in the corpus

```scala
// watchtower/TreasuryMovementValidator.scala:373
/** Count the transaction inputs sitting at the TM script address … A legal TM spend —
  * Confirm or GC — spends EXACTLY ONE TM record ([CTM-17]); both branches of [[spend]] require this.
  *
  * Why: the TM NFT has an empty asset name and no one-shot seed, so `(policy, "")` is fungible
  * across posts — permissionless posting lets the SAME `signedBtcTx` be posted as two
  * `Unconfirmed` records, each bearing the token. Spending two TM records in one tx runs this
  * validator once per input; every invocation sees the same transaction-wide −1 mint, so only
  * ONE token is burned, and ledger value-conservation forces the second token to escape to an
  * attacker output with a fabricated `Unconfirmed` datum — a forged post that skipped the mint
  * checks. Requiring one TM input per spend closes the escape on both paths. */
def tmInputCount(inputs: ScalusList[TxInInfo], tmScriptHash: ByteString): BigInt =
    inputs.count(_.resolved.address.credential === Credential.ScriptCredential(tmScriptHash))
```

used together with the transaction-wide burn:

```scala
// watchtower/TreasuryMovementValidator.scala:416
require(tmInputCount(tx.inputs, tmScriptHash) == BigInt(1),
  "TM spend: exactly one TM-script input per tx")
require(tx.mint.quantityOf(tmScriptHash, ByteString.empty) == BigInt(-1),
  "TM spend: must burn the TM NFT")
```

This is the canonical "a per-input validator sees transaction-wide facts, so N inputs
can share one satisfaction" bug, with the fix stated as a rule.

**Constructor-tag pinning** — because on-chain `FromData` for a case class is an erased
retag, the tag must be checked explicitly or an attacker-shaped datum passes:

```scala
// watchtower/TreasuryMovementValidator.scala:568
// Pin the datum's Constr TAG to 0. A case-class decode is an erased retag with no tag
// check, and every harvester … keys history by `Constr 0` records — a wrong-tag record
// would be mintable and confirmable yet invisible to every reader.
val rawDatum = tmOut.datum match
    case OutputDatum.OutputDatum(d) => d
    case _                          => fail("TM mint: NFT output datum must be inline")
require(unConstrData(rawDatum).fst == BigInt(0),
  "TM mint: NFT output datum is not an UnconfirmedTm record")
```

**Rebuild-the-whole-datum-and-compare**, for the same reason:

```scala
// watchtower/TreasuryMovementValidator.scala:515
// spec [CTM-27] rebuild the WHOLE expected datum and compare the whole
// OutputDatum. On-chain FromData is an erased retag (no tag or arity
// check), so field-wise reads would also accept `Constr 5 [root, junk]` at
// the singleton address — attacker-chosen, since confirming is permissionless.
val exp = OutputDatum.OutputDatum(
  BridgeState(
    spiRoot = roots._1, cpoRoot = roots._2,
    treasuryUtxoId = txid ++ hex"00000000",
    treasuryAmount = outs.head.amount
  ).toData
)
require(exp === bssOut.datum, "TM confirm: singleton datum is not the attested state")
```

**"No output at my own address"** — the record is retired, not recreated:

```scala
// watchtower/TreasuryMovementValidator.scala:458
require(
  !tx.outputs.exists(_.address.credential === Credential.ScriptCredential(tmScriptHash)),
  "TM confirm: no output may sit at the TM address"
)
```

**Chain linkage as a replay lock** — a re-posted stale record is permanently
unconfirmable because the head it chains from is already spent:

```scala
// watchtower/TreasuryMovementValidator.scala:490
// spec [CTM-18] the TM spends the confirmed head. This is what makes
// re-posting an OLD TM permanently unconfirmable …
require(
  allInputOutpoints(signedBtcTx).head == bssIn.datum.of[BridgeState].treasuryUtxoId,
  "TM confirm: BTC tx does not spend the confirmed head"
)
```

## 4.4 Efficiency tricks

**`.toData ==` instead of structural equality** — used pervasively for addresses,
values, datums, outrefs and mint maps (`BitcoinValidator.scala:1270,1277,1288-1292,
1305-1308,1410-1414,1421,1427`). One `equalsData` beats a recursive typed comparison.

**Whole-`Value` comparison by serialised form, with lovelace stripped:**

```scala
// oracle/BitcoinValidator.scala:1276
require(
  ownInput.value.withoutLovelace.toData == continuingOutput.value.withoutLovelace.toData,
  "Non-ADA tokens must be preserved"
)
```

**Hand-decoded `ScriptContext`** (no typed decode at all) in the smallest validator:

```scala
// watchtower/TransactionVerifierValidator.scala:164
def validate(scData: Data): Unit = {
    val sc = unConstrData(scData).snd
    val txInfoData = sc.head
    val redeemer = sc.tail.head
    val scriptInfo = unConstrData(sc.tail.tail.head)
    if scriptInfo.fst == BigInt(1) then
        val txOutRef = scriptInfo.snd.head.to[TxOutRef]
        val datum = scriptInfo.snd.tail.head.to[Option[Datum]]
        val txInfo = txInfoData.to[TxInfo]
        spend(datum, redeemer, txInfo, txOutRef)
    else fail("Invalid script context")
}
```

Note `TreasuryMovementValidator.scala:652-659` records that this workaround is now
obsolete: *"This used to hand-decode via `unConstrData`/`unBData` — a workaround from
before Scalus V3 lowering made `to`/`toData` no-ops on the structural script-context
types; the straightforward form now compiles to the same field projections."*

**Positional redeemer as a bare `List<Data>`, decoded by `unListData` + skipping:**

```scala
// watchtower/PegOutVerifier.scala:68
val items = unListData(ctx.redeemer)
val treasuryUtxoId = unBData(items.head)
val afterTreasury = items.tail
val destinationAddress = unBData(afterTreasury.head)
// index 2 (peg_out_utxo_id) is not needed for the produced check – skip it.
val afterPegOutId = afterTreasury.tail.tail
val pegOutAmount = unIData(afterPegOutId.head)
val rawTx = unBData(afterPegOutId.tail.head)
```

**Single forward pass, no intermediate lists** — explicitly chosen over reusing two
existing helpers:

```scala
// watchtower/PegOutVerifier.scala:56
// The raw TM tx is walked exactly ONCE (inputs then outputs, in a single forward pass) rather
// than reusing [[TreasuryMovementValidator]]'s `allInputOutpoints` + `allOutputs` – those would
// walk the input region twice (the second time via `skipTxIns` inside `allOutputs`) and allocate
// two intermediate lists. [[scanTm]] streams over the bytes, short-circuits, and allocates nothing.
def scanTm(rawTx, treasuryUtxoId, destinationSpk, pegOutAmount): Boolean = {
    …
    def scanInputs(remaining: BigInt, offset: BigInt, foundTreasury: Boolean): (Boolean, BigInt) = …
    def scanOutputs(remaining: BigInt, offset: BigInt, foundPay: Boolean): Boolean = …
    treasurySpent && scanOutputs(outsNumAndOffset._1, outsNumAndOffset._2, false)
}
```

**Byte-cursor parsing over `slice` + a returned offset** — the whole `BitcoinHelpers`
`skipTxIns` / `skipTxOuts` / `readVarInt` family returns *offsets*, never sub-objects
(`bitcoin/BitcoinHelpers.scala:226,307,314,321,326`).

**Ordered insert instead of sort, for tiny lists:**

```scala
// oracle/BitcoinValidator.scala:429
/** Insert element into an ascending-sorted list, maintaining ascending order. */
def insertAscending(x: BigInt, sorted: List[BigInt]): List[BigInt] = sorted match
    case Nil                  => Cons(x, Nil)
    case Cons(h, t) if x <= h => Cons(x, sorted)
    case Cons(h, t)           => Cons(h, insertAscending(x, t))

/** Sort a list of BigInts in ascending order using insertion sort. Efficient for small
  * fixed-size lists (e.g. 11 timestamps for MTP calculation). */
def insertionSort(xs: List[BigInt]): List[BigInt] =
    xs.foldLeft(List.empty[BigInt])((sorted, x) => insertAscending(x, sorted))
```

**Fold one block into a context in a single branch** (retarget handled inline, no second
pass):

```scala
// oracle/BitcoinValidator.scala:391
def accumulateBlock(ctx: TraversalCtx, block: BlockSummary, powLimit: BigInt): TraversalCtx = {
    val newHeight = ctx.height + 1
    val newTimestamps = Cons(block.timestamp, ctx.timestamps)
    if newHeight % DifficultyAdjustmentInterval == BigInt(0) then … else ctx.copy(…)
}
```

**Scalus-specific gotcha worth its own stdlib note** — a generic non-`inline` datum
decoder cannot be used across `@Compile` boundaries:

```scala
// watchtower/TreasuryMovementValidator.scala:223
/** Decode an inline datum as `A`, failing on a missing/hashed datum. … `inline` so the
  * `FromData[A]` derivation expands at the call site — a non-inline generic would reference the
  * companion's `derived$FromData` module, which is not `@Compile`d for externally-defined types
  * like [[ConfigDatum]] and `ChainState`. */
extension (d: OutputDatum) {
    inline def of[A: FromData]: A = d match
        case OutputDatum.OutputDatum(datum) => datum.to[A]
        case _                              => fail("Expected inline datum")
}
```

**Idioms present (Binocular):**
`auth-nft-check`, `one-shot-mint`, `single-script-input`,
`output-to-self-with-datum`, `validity-range-check` (bounded window, entirely-after,
anchored timestamp), `input-index-hint` (`outputs.at(outputIndex)`,
`referenceInputs.at(bridgeStateRefInputIndex)`), `data-level-compare` (pervasive),
`merkle-proof` (binary Merkle + MPF membership), `min-ada` (GC reclaim),
`no-double-satisfaction` (input-count rule), `tag-with-txoutref` (one-shot seed;
`treasuryUtxoId` chain linkage), `datum-rebuild-and-compare`, `constr-tag-pinning`,
`config-nft-indirection`, `datum-shape-validation`, `no-output-at-own-address`.
Absent: `linked-list-node`, `withdraw-zero-forward` (present only as a *retired*
scheme — `PegOutVerifier.scala:29-54` documents the old
`stake_validator.validate_withdraw` delegation), `protocol-fee-cut`,
`spend-forwards-to-mint`.

---

# 5. Cross-repo synthesis — what to promote into a Scalus stdlib

Ranked by how many of the four repos reimplement it by hand.

| Rank | Candidate | Midgard | Hydrozoa | AdaStream | Binocular |
|---|---|---|---|---|---|
| 1 | **Authenticated UTxO lookup by beacon NFT** (`findByNft(inputs, policy, name)`, prefix and policy-only variants, ref-input variants, "policy == address script hash" variant) | ✔ ×3 helpers | ✔ (CIP-67 prefix) | – | ✔ ×2 |
| 2 | **`exactlyOne` / `theOnly` combinator** (filter-then-assert-tail-empty; `find` is a footgun) | ✔ | ✔ ×4 | – | ✔ (with the rationale spelled out) |
| 3 | **Value shape assertions**: `onlyNonAdaAsset`, `singleAssetOf(policy)`, `quantityOfPolicy`, `flatten.length == n`, `withoutLovelace` compare | ✔ ×6 | ✔ ×3 | – | ✔ |
| 4 | **State-machine step**: own input by `ownRef`, continuing output by address+NFT, value preserved / ADA monotone, expected datum rebuilt and compared | ✔ | ✔ | – | ✔ |
| 5 | **Anti-double-satisfaction primitives**: `countInputsAtOwnScript == 1`, `noOtherInputCarriesPolicy(p)`, "no output at own address" | ✔ (`singular_utxo_indexer`) | ✔ | – | ✔ |
| 6 | **Validity-range helpers**: `finiteFrom/finiteTo`, `isEntirelyAfter/Before`, `containedIn(a,b)`, `maxWidth(w)`, `deadlineFromRange` | ✔ ×4 | ✔ ×3 | ✔ | ✔ ×4 |
| 7 | **Merkle / MPF proofs**: binary inclusion fold, MPF membership **and non-membership** | ✔ (MPF, both) | ✔ (KZG/BLS instead) | ✔ (binary) | ✔ (both) |
| 8 | **Linked-list-in-UTxO**: `NodeDatum{key,link}`, init/deinit/append/prepend/insert/remove, `proveIsMember`, `proveIsNotMember`, `proveIsRoot`, `proveIsLast` | ✔ (external lib) | ✔ (hand-rolled contraction) | – | – |
| 9 | **One-shot mint tied to a `TxOutRef`** and `outRefToNonce = blake2b_256(serialise(outRef))` token names | ✔ (nonce form) | – | – | ✔ (classic form) |
| 10 | **Payout check**: `paysTo(address, value, datum)` incl. the `value + add(policy,name,-1)` "same minus beacon" idiom | ✔ ×4 | ✔ | – | – |
| 11 | **Config-NFT indirection** (one oracle UTxO holding every other script's policy/address; breaks compile-time cycles) | ✔ (hub oracle) | ✔ (regime utxo) | – | ✔ (ConfigDatum) |
| 12 | **Redeemer-index hints with assertion** (`inputAt(i) asserting outRef`, `redeemerAt(i) asserting purpose`) | ✔ (everywhere) | ✔ (`!!`) | – | ✔ (`.at`) |
| 13 | **Datum integrity**: constructor-tag pinning, arity/field forcing, whole-datum `Data` comparison | – | – | – | ✔ ×3 |
| 14 | **Withdraw-0 / merkelized validator** delegation | ✔ | – | – | (retired) |
| 15 | **Datum-safe `Value` mirror** (`ValuePairs = Pairs<PolicyId, Pairs<AssetName,Int>>`) for storing values in datums | ✔ | – | – | – |
| 16 | **min-ADA / fee arithmetic**: fee-aware residual on merge, min-ADA GC reclaim, slashing via `tx.fee >= penalty` | ✔ (fee slash, bond) | ✔ (residual) | – | ✔ (GC) |
| 17 | **Bitcoin/foreign-chain byte parsing** (VarInt cursors, tx walking) | – | – | – | ✔ (large) |
| 18 | **Raw-`Data` escape hatches**: `unconstrFields`, positional list redeemers, `field[T](_.path)` projections, constructor-tag dispatch | ✔ | – | ✔ | ✔ |

Two Scalus-specific findings that are not idioms but stdlib/compiler asks, both
recorded in the source by the authors:

- `VoteState.scala:61` — `derives FromData` on a type puts the instance in the type's
  companion, whose SIR the on-chain linker cannot resolve; explicit `given` inside the
  `@Compile` object is required.
- `TreasuryMovementValidator.scala:223` — a generic (non-`inline`) datum-decoding
  helper references `derived$FromData` of a non-`@Compile`d companion and fails; the
  helper must be `inline` so the derivation expands at the call site.

And two explicit feature requests found in comments:

- `DisputeResolutionScript.scala:296` — "TODO (Scalus team): the `List[Option[Signature]]`
  encoding bloats wire size … a sparser encoding like `List[(CoilPeerId, Signature)]`
  … Worth evaluating once cost benchmarks are in place."
- `RuleBasedTreasuryScript.scala:475` — "TODO: comparing as bytestrings is more
  efficient, we want to have this constant in Scalus" (BLS12-381 G1 identity).
