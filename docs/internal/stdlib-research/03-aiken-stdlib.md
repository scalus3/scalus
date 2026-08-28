# Aiken ecosystem API inventory (for the Scalus on-chain stdlib design)

Research note. Goal: know exactly what an Aiken contract author gets out of the box, so the
new Scalus "smart contract standard library" can match or beat it.

## 1. Provenance and method

All numbers below come from real source, cloned at these commits (2026-08-26):

| Repo | Version | Commit | Path in scratch |
|---|---|---|---|
| `aiken-lang/stdlib` | `main` (post `v3.1.0`, compiler `v1.1.21`) | `41f3c823f8d36dc81c285f34c8c6e545e2fffbfd` | `aiken/stdlib` |
| `sidan-lab/vodka` | `0.1.23` (compiler `v1.1.9`) | `1b5f32c1227b18c7580260e4a0c313b05d32db05` | `aiken/vodka` |
| `Anastasia-Labs/aiken-design-patterns` | `v1.8.0` (compiler `v1.1.23`) | `dab059406035ed30a9464211822b1e5520d21cfc` | `aiken/aiken-design-patterns` |
| `aiken-lang/fuzz` | `main` (compiler `v1.1.17`) | `06874926ec70747f3fc4e2b9364ee9e1393441cc` | `aiken/fuzz` |
| `Cardano-Fans/acca`, `SundaeSwap-finance/aicone`, `logicalmechanism/assist`, `aiken-lang/merkle-patricia-forestry` | `main` | shallow | `aiken/*` |

Downstream corpora cloned for usage measurement: `cardano-miners/fortuna`,
`SundaeSwap-finance/sundae-contracts`, `Anastasia-Labs/smart-handles`,
`jpg-store/contracts-v3`, `minswap/minswap-dex-v2`, `MeshJS/mesh`.

Declarations were extracted mechanically (`pub fn` / `pub type` / `pub const` / `pub opaque
type`, signature joined across lines, first `///` line as the one-line semantics). The
Scalus side was extracted the same way from
`scalus-core/shared/src/main/scala/scalus/cardano/onchain/plutus/**` (31 files, ~1340 public
declarations).

Legend for the "Kind" column:

- **P** = low-level primitive (data-structure or arithmetic building block)
- **H** = high-level validation helper (knows about transactions, values, credentials)

Note: the Aiken stdlib splits nearly every partial function into a total version returning
`Option`/`Bool` and an `expect_*` version that **fails** the script. The `expect_*` family is
not just ergonomics: Aiken documents each as "more efficient", because the failing variant
skips constructing the `Option` wrapper. This is a deliberate, pervasive API convention and is
the single biggest structural difference from the Scalus prelude today.

Totals (measured): stdlib ships **502 public declarations across 34 modules**, of which
**406 are `pub fn`**.

---

## 2. `aiken-lang/stdlib`

### 2.1 `cardano/transaction` (`lib/cardano/transaction.ak`)

Types: `Transaction`, `Input`, `Output`, `OutputReference`, `Datum` (`NoDatum |
DatumHash(DataHash) | InlineDatum(Data)`), `Redeemer = Data`, `ValidityRange = Interval`,
`TransactionId = Hash<Blake2b_256, Transaction>`, `ScriptPurpose` (`Mint | Spend | Withdraw |
Publish{at,certificate} | Vote | Propose{at,proposal_procedure}`).

`Transaction` fields: `inputs, reference_inputs, outputs, fee: Lovelace, mint: Value,
certificates, withdrawals: Pairs<Credential, Lovelace>, validity_range, extra_signatories,
redeemers: Pairs<ScriptPurpose, Redeemer>, datums: Dict<DataHash, Data>, id, votes,
proposal_procedures, current_treasury_amount, treasury_donation`.

| Signature | Kind | Semantics |
|---|---|---|
| `find_input(inputs: List<Input>, output_reference: OutputReference) -> Option<Input>` | H | Find an input by its `OutputReference` (the "own input" idiom). |
| `resolve_input(inputs: List<Input>, output_reference: OutputReference) -> Output` | H | Same, but **fails** when not found; returns the resolved `Output` directly. |
| `find_datum(outputs: List<Output>, datums: Dict<DataHash, Data>, datum_hash: DataHash) -> Option<Data>` | H | Look up datum in the witness set first, then scan inline datums re-hashing with `blake2b_256(serialise_data(..))`. |
| `find_script_outputs(outputs: List<Output>, script_hash: ScriptHash) -> List<Output>` | H | All outputs paying to that script hash. |
| `placeholder: Transaction` (const) | H | Empty transaction for tests; `id` all zeros, `validity_range == interval.everything`. |

Companion compare modules: `transaction/output_reference.compare`,
`transaction/script_purpose.compare` (both `-> Ordering`, needed because ledger fields are
sorted by these orders).

### 2.2 `cardano/assets` (`lib/cardano/assets.ak`) – 37 decls

Types: `Lovelace = Int`, `PolicyId = ByteArray` (28 bytes), `AssetName = ByteArray` (0..32),
`opaque Value` (invariant: never stores a zero quantity). Constants `ada_policy_id = ""`,
`ada_asset_name = ""`.

| Signature | Kind | Semantics |
|---|---|---|
| `from_asset(policy_id, asset_name, quantity: Int) -> Value` | P | Single-asset value. |
| `from_asset_list(xs: Pairs<PolicyId, Pairs<AssetName, Int>>) -> Value` | P | From arbitrary list; fails on duplicates/zero. |
| `from_ascending_pairs(xs: Pairs<PolicyId, Dict<AssetName, Int>>) -> Value` | P | From already-sorted pairs. |
| `from_lovelace(quantity: Int) -> Value` | P | Ada-only value. |
| `zero: Value` (const) | P | Empty value. |
| `add(self, policy_id, asset_name, quantity: Int) -> Value` | P | **Add** (not set) a signed quantity of one token. |
| `merge(left, right) -> Value` | P | Pointwise sum of two values. |
| `difference(left, right) -> Value` | P | Pointwise subtraction (added in v3.1.0). |
| `negate(self) -> Value` | P | Negate every quantity, Ada included. |
| `is_zero(self) -> Bool` | P | No assets and no Ada. |
| `quantity_of(self, policy_id, asset_name) -> Int` | P | Quantity of one asset (0 if absent). |
| `expect_quantity_of(self, policy_id, asset_name) -> Int` | P | Same, **fails** if absent. |
| `lovelace_of(self) -> Int` | P | Specialized `quantity_of` for Ada. |
| `expect_lovelace_of(self) -> Int` | P | Same, **fails** if no Ada present. |
| `tokens(self, policy_id) -> Dict<AssetName, Int>` | P | All tokens under one policy. |
| `policies(self) -> List<PolicyId>` | P | All policies with non-zero tokens. |
| `restricted_to(self, mask: List<PolicyId>) -> Value` | H | Subset of the value limited to the given policies. |
| `without_lovelace(self) -> Value` | H | Drop Ada. |
| `expect_tail(self) -> Value` | P | Drop first policy; fails if empty. |
| `contains(self: Dict<AssetName,Int>, subset: Dict<AssetName,Int>) -> Bool` | H | Token-list containment (on the tokens dict, not on `Value`). |
| `has_nft(self, policy, asset_name) -> Bool` | H | Value carries exactly 1 of that NFT; other assets tolerated. |
| `has_nft_strict(self, policy, asset_name) -> Bool` | H | Same, but no other assets under that policy. |
| `has_any_nft(self, policy) -> Bool` | H | Value carries some NFT from that policy. |
| `has_any_nft_strict(self, policy) -> Bool` | H | Same, strict about the policy's other assets. |
| `match(left: Value, right: Data, assert_lovelace: fn(Lovelace, Lovelace) -> Bool) -> Bool` | H | Efficient structural compare of two values with custom Ada handling. |
| `expect_match(...) -> Bool` | H | Failing variant of `match`. |
| `match_assets(left: Value, right: Data) -> Bool` | H | `match` ignoring lovelace on both sides. |
| `expect_match_assets(left, right) -> Bool` | H | Failing variant. |
| `flatten(self) -> List<(PolicyId, AssetName, Int)>` | P | 3-tuple list. |
| `flatten_with(self, with: FlattenStrategy<result>) -> List<result>` | P | Flatten with keep/discard callback. |
| `reduce(self, start: result, with: fn(PolicyId, AssetName, Int, result) -> result) -> result` | P | Single-pass fold over all assets. |
| `to_dict(self) -> Dict<PolicyId, Dict<AssetName, Int>>` | P | Nested dict view. |
| `to_pairs(self) -> Pairs<PolicyId, Dict<AssetName, Int>>` | P | Associative-list view. |

`cardano/assets/strategy`: `FlattenStrategy<result>`, `KeepResult`, `DiscardResult`,
`triple()`.

### 2.3 `cardano/address` (`lib/cardano/address.ak`)

Types: `Credential { VerificationKey(Hash<Blake2b_224, VerificationKey>) |
Script(Hash<Blake2b_224, Script>) }`, `Address { payment_credential,
stake_credential: Option<Referenced<Credential>> }`, `Referenced<a> { Inline(a) |
Pointer{slot_number, transaction_index, certificate_index} }`, `StakeCredential`,
`PaymentCredential`.

| Signature | Kind | Semantics |
|---|---|---|
| `from_script(script: Hash<Blake2b_224, Script>) -> Address` | H | Script address, no delegation. |
| `from_verification_key(vk: Hash<Blake2b_224, VerificationKey>) -> Address` | H | Enterprise address, no delegation. |
| `with_delegation_key(self, vk) -> Address` | H | Set/reset the delegation part to a key hash. |
| `with_delegation_script(self, script) -> Address` | H | Set/reset the delegation part to a script hash. |
| `credential.compare(left, right) -> Ordering` | P | Ledger credential order (`Script` sorts **below** `VerificationKey`). |

### 2.4 `cardano/certificate`, `cardano/governance`, `cardano/script_context`

- `certificate.ak`: `StakePoolId`, `Certificate` (all Conway variants),
  `Delegate { DelegateBlockProduction | DelegateVote | DelegateBoth }`,
  `DelegateRepresentative { Registered | AlwaysAbstain | AlwaysNoConfidence }`. Types only,
  no functions.
- `governance.ak`: `ProposalProcedure`, `GovernanceAction`, `Vote`, `GovernanceActionId`,
  `ProtocolVersion`, `Constitution`, `Mandate`, `Voter`. Types only.
  `governance/voter.compare(left, right) -> Ordering`.
- `governance/protocol_parameters.ak`: an **opaque** `ProtocolParametersUpdate` with **30
  typed getters**, each `-> Option<T>`: `min_fee_coefficient`, `min_fee_constant`,
  `max_block_body_size`, `max_transaction_size`, `max_block_header_size`,
  `stake_credential_deposit`, `stake_pool_deposit`, `stake_pool_retirement_horizon`,
  `desired_number_of_stake_pools`, `stake_pool_pledge_influence`, `monetary_expansion`,
  `treasury_expansion`, `min_stake_pool_cost`, `min_utxo_deposit_coefficient`, `cost_models`,
  `script_execution_prices`, `max_transaction_execution_units`,
  `max_block_execution_units`, `max_value_size`, `collateral_percentage`,
  `max_collateral_inputs`, `stake_pool_operator_voting_thresholds`,
  `delegate_representative_voting_thresholds`, `min_constitutional_committee_size`,
  `max_constitutional_committee_mandate`, `governance_proposal_lifetime`,
  `governance_proposal_deposit`, `delegate_representative_deposit`,
  `delegate_representative_max_idle_time`, `reference_scripts_tier_fee_initial_factor`.
  All **H**: this is the only ergonomic way to inspect a `ParameterChange` governance action.
- `script_context.ak`: `ScriptContext { transaction, redeemer, info }`,
  `ScriptInfo` (like `ScriptPurpose` but `Spending` carries `datum: Option<Data>`). Types only.

### 2.5 `aiken/interval` – 19 decls

Types: `Interval { lower_bound: IntervalBound, upper_bound: IntervalBound }`,
`IntervalBound { bound_type: IntervalBoundType, is_inclusive: Bool }`,
`IntervalBoundType { NegativeInfinity | Finite(Int) | PositiveInfinity }`.

| Signature | Kind | Semantics |
|---|---|---|
| `after(lower_bound: Int) -> Interval` | P | `[lb, +inf]`. |
| `entirely_after(lower_bound: Int) -> Interval` | P | `(lb, +inf]`. |
| `before(upper_bound: Int) -> Interval` | P | `(-inf, ub]`. |
| `entirely_before(upper_bound: Int) -> Interval` | P | `(-inf, ub)`. |
| `between(lb, ub) -> Interval` | P | `[lb, ub]`. |
| `entirely_between(lb, ub) -> Interval` | P | `(lb, ub)`. |
| `empty: Interval` (const) | P | Contains nothing. |
| `everything: Interval` (const) | P | `(-inf, +inf)`. |
| `contains(self, elem: Int) -> Bool` | P | Point membership. |
| `is_empty(self) -> Bool` | P | Contains no value (computed, not identity). |
| `is_entirely_after(self, point: Int) -> Bool` | H | Whole interval strictly after point. |
| `is_entirely_before(self, point: Int) -> Bool` | H | Whole interval strictly before point. |
| `includes(self, other: Interval) -> Bool` | P | `other` fully inside `self`. |
| `hull(iv1, iv2) -> Interval` | P | Smallest interval containing both. |
| `intersection(iv1, iv2) -> Interval` | P | Largest interval inside both. |
| `to_string(self) -> String` | P | Debug rendering. |

### 2.6 `aiken/collection/list` – 60 decls (all **P**)

Construct: `push`, `range(from,to)`, `repeat(elem, n_times)`.

Inspect: `all`, `any`, `at -> Option<a>`, `count(predicate)`, `find`, `find_map`, `has`,
`head`, `is_empty`, `index_of`, `last`, `length`.

Failing variants: `expect_any`, `expect_at`, `expect_find`,
`expect_find_map(self, select: fn(a, fn(b)->b, fn()->b) -> b) -> b`, `expect_has`,
`expect_head`, `expect_index_of`, `expect_last`.

Modify: `delete`, `drop`, `drop_while`, `filter`, `filter_map`, `init -> Option<List<a>>`,
`partition -> (List<a>, List<a>)`, `slice(from,to)`, `span(n) -> (List<a>, List<a>)`,
`tail -> Option<List<a>>`, `take`, `take_while`, `unique`.
Failing: `expect_delete`, `expect_drop`, `expect_init`, `expect_tail`, `expect_take`.

Transform: `flat_map`, `for_each(do: fn(a) -> Void) -> Void`,
`indexed_map(with: fn(Int, a) -> result)`, `map`, `map2`, `map3`, `reverse`,
`sort(compare: fn(a,a) -> Ordering)`, `unzip(List<(a,b)>) -> (List<a>, List<b>)`.

Combine: `concat`, `difference`, `zip`.

Reduce: `foldl`, `foldr`, `reduce(self, zero: b, with: fn(b, a) -> b)`,
`indexed_foldr(with: fn(Int, a, result) -> result)`, plus the double-accumulator
CPS folds `foldl2` / `foldr2` with `Fold2<a, b, result> = fn(a, b) -> result`.

### 2.7 `aiken/collection/dict` – 37 decls (all **P**)

`opaque Dict<key, value>` = `Pairs<ByteArray, value>` kept sorted by key. Keys are always
`ByteArray` (the phantom `key` param is a tag only).

Construct: `empty`, `singleton(key, value)`, `from_pairs`, `from_ascending_pairs`,
`from_ascending_pairs_with(xs, predicate)`.

Query: `contains(self, subset, compare)`, `find(self, value) -> Option<ByteArray>`,
`get -> Option<value>`, `get_or_else(key, or_else: fn() -> value)`, `has_key`, `is_empty`,
`keys`, `size`, `values`.
Failing: `expect_contains -> Void`, `expect_find`, `expect_get`, `expect_has_key -> Void`.

Modify: `delete`, `filter(with: fn(ByteArray, value) -> Bool)`, `insert`,
`insert_with(.., with: UnionStrategy)`, `map(with: fn(ByteArray, a) -> b)`,
`pop(key) -> (Option<value>, Dict)`, `union`,
`union_with(left, right, with: UnionStrategy)`,
`difference_with(left, right, with_both: UnionStrategy, with_right: InsertStrategy)`.
Failing: `expect_delete`, `expect_pop`, `expect_tail`.

Reduce: `foldl`, `foldr`, `foldl2`, `foldr2`, `to_pairs`.

`collection/dict/strategy` (**P**) – the combinator vocabulary that makes `union_with` usable:
types `UnionStrategy<key,value>`, `InsertStrategy<key,value>`, `KeepValue`, `DiscardValue`;
functions `discard()`, `keep()`, `negate()`, `never()`, `difference()`,
`difference_if_non_zero()`, `expect_no_duplicate()`, `keep_left()`, `keep_right()`, `sum()`,
`sum_if_non_zero()`.

### 2.8 `aiken/collection/pairs` – 33 decls (all **P**)

Unsorted associative list (`Pairs<key, value>`), used directly for `withdrawals`, `redeemers`,
`votes`.

Query: `get_all`, `get_first`, `get_last`, `find_all`, `find_first`, `find_last`, `has_key`,
`keys`, `values`.
Failing: `expect_get_first`, `expect_get_last`, `expect_find_first`, `expect_find_last`,
`expect_has_key`.

Modify: `delete_all`, `delete_first`, `delete_last`,
`insert_by_ascending_key(self, key, value, compare)`,
`insert_with_by_ascending_key(.., compare, with)`,
`repsert_by_ascending_key(..)` (insert-or-replace), `map`.
Failing: `expect_delete_first`, `expect_delete_last`.

Streaming extraction (added v3.1.0, the "walk a sorted ledger list once" idiom):
`pop_until(self, until: fn(key,value) -> Bool) -> (Option<Pair<key,value>>, Pairs)`,
`pop_until_and_then(.., return)`, `pop_until_key(self, until: key)`,
`pop_until_key_and_then(..)`, plus `expect_pop_until`, `expect_pop_until_and_then`,
`expect_pop_until_key`, `expect_pop_until_key_and_then`.

Reduce: `foldl`, `foldr`.

### 2.9 `aiken/crypto` (20 decls, **P**)

Types `VerificationKey`, `VerificationKeyHash`, `Script`, `ScriptHash`, `Signature`,
`DataHash`, `Hash<alg, a>`; phantom algorithms `Blake2b_224`, `Blake2b_256`, `Keccak_256`,
`Sha2_256`, `Sha3_256`.
Functions: `blake2b_224`, `blake2b_256`, `keccak_256`, `sha2_256`, `sha3_256`,
`verify_ecdsa_signature(key, msg, sig) -> Bool`, `verify_ed25519_signature`,
`verify_schnorr_signature`.

Sub-modules: `crypto/bitwise` (`opaque State<t>`, `add_bits/add_int/add_state`,
`sub_*`, `mul_*`, `scale`, `scale2`, `neg`, `to_int`, `from_int` – a generic modular-arithmetic
kernel), `crypto/int224` and `crypto/int256` (21 decls each: `from_bytearray_big_endian`,
`from_bytearray_little_endian`, `from_int`, `scale`, `scale2`, `add`, `add_bytes`, `add_int`,
`mul*`, `neg`, `sub*`, `to_bytearray_big_endian`, `to_bytearray_little_endian`, `to_int`),
`crypto/bls12_381` (`DomainSeparationTag`, `domain_separation_tag_{nul,aug,pop}`,
`public_key_size = 48`, `signature_size = 96`), `bls12_381/g1` and `/g2` (`generator`, `zero`,
`compress`, `decompress`, `equal`, `add`, `neg`, `sub`, `scale`, `hash_to_group`),
`bls12_381/pairing` (`miller_loop`, `mul`, `final_exponentiation`),
`bls12_381/scalar` (24 decls: `field_prime`, `field_size`, `from_bytes`,
`from_bytes_little_endian`, `from_int`, `scale`, `scale2`, `add*`, `div*`, `mul*`, `neg`,
`recip`, `sub*`, `to_int`, `to_bytes`, `to_bytes_little_endian`).

### 2.10 `aiken/math` (11, **P**) and `aiken/math/rational` (25, **P**)

`math`: `abs`, `clamp(self, min, max)`, `gcd`, `is_sqrt(self, x)`, `log(self, base)`, `log2`,
`max`, `min`, `pow(self, e)`, `pow2(e)`, `sqrt -> Option<Int>`.

`math/rational`: `opaque Rational`; `from_int`, `new(num, den) -> Option<Rational>`, `zero`,
`numerator`, `denominator`, `abs`, `negate`, `reciprocal -> Option`, `reduce`, `add`,
`div -> Option`, `mul`, `sub`, `compare`, `compare_with(left, with: fn(Int,Int)->Bool, right)`,
`arithmetic_mean(List<Rational>) -> Option`, `geometric_mean(l, r) -> Option`, `ceil`, `floor`,
`pow(x, y: Int) -> Option`, `proper_fraction -> (Int, Rational)`, `round`, `round_even`,
`truncate`.

### 2.11 `aiken/option` (10, **P**)

`is_none`, `is_some`, `and_then`, `choice(List<Option<a>>) -> Option<a>`,
`flatten(Option<Option<a>>)`, `map`, `map2`, `map3`,
`or_try(self, compute_default: fn() -> Option<a>) -> Option<a>` (lazy),
`or_else(self, default: a) -> a`.

### 2.12 `aiken/primitive/*` and `aiken/cbor`

`primitive/bytearray` (26, **P**): `Byte`, `from_int_big_endian(self, size)`,
`from_int_little_endian`, `from_string`, `push`, `at`,
`index_of(self, bytes) -> Option<(Int, Int)>`, `is_empty`, `length`, `test_bit(self, ix)`,
`drop`, `slice(start, end)`, `take`, `concat`, `compare -> Ordering`,
`foldl(zero, with: fn(Int, result) -> result)`, `foldr`, `reduce`, `to_int_big_endian`,
`to_int_little_endian`, `to_string`, `to_hex`, `starts_with(self, prefix)`, `and_bytes`,
`or_bytes`, `xor_bytes` (each `(left, right, pad_end: Bool)`).

`primitive/int` (5, **P**): `compare`, `from_bytearray_big_endian`,
`from_bytearray_little_endian`, `from_utf8 -> Option<Int>`, `to_string`.

`primitive/string` (5, **P**): `from_bytearray`, `from_int`, `concat`,
`join(list: List<String>, delimiter: String)`, `to_bytearray`.

`aiken/cbor` (3, **P**): `diagnostic(self: Data) -> String`,
`deserialise(bytes) -> Option<Data>`, `serialise(self: Data) -> ByteArray`.

---

## 3. `Anastasia-Labs/aiken-design-patterns` v1.8.0

Seven patterns. Prose below is condensed from the repo README.

### 3.1 Stake validator (withdraw-zero trick) – `stake_validator`

Problem: spend logic runs once per script input; heavy checks get paid for N times. Move the
real logic into a withdrawal (stake) script that runs exactly once, and make the spend endpoint
a cheap "did the stake script run?" check.

| Signature | Semantics |
|---|---|
| `validate_withdraw(withdraw_script_hash: ScriptHash, redeemers: Pairs<ScriptPurpose, Redeemer>, withdraw_redeemer_index: Int, withdraw_redeemer_validator: fn(Redeemer) -> Bool) -> Bool` | Locate the `Withdraw` redeemer at the given index and validate it. |
| `validate_withdraw_with_amount(.., withdrawals: Pairs<Credential, Lovelace>, withdrawal_index: Int, validator: fn(Redeemer, Lovelace) -> Bool) -> Bool` | Same, plus the withdrawn quantity. |
| `validate_withdraw_minimal(withdraw_script_hash, withdrawals, withdrawal_index) -> Bool` | Cheapest: only prove the stake script is being executed. |

### 3.2 UTxO indexers – `singular_utxo_indexer`, `multi_utxo_indexer`

Problem: pairing one script input to its continuing output(s) is `O(n*m)` if done by search.
Pass the indices in the redeemer and validate positionally instead.

| Signature | Semantics |
|---|---|
| `singular_utxo_indexer.one_to_one(input_index, output_index, own_ref, inputs, outputs, double_satisfaction_prevented: Bool, validation_logic: fn(Input, Output) -> Bool) -> Bool` | One input to one output by index; `own_ref` proves the index really is this script's input. |
| `singular_utxo_indexer.one_to_many(input_index, output_indices: List<Int>, own_ref, inputs, outputs, double_satisfaction_prevented, input_collective_outputs_validator: fn(Input, List<Output>) -> Bool, input_output_validator: fn(Input, Int, Output) -> Bool) -> Bool` | One input to a set of outputs; collective check runs once, per-output check runs N times. |
| `multi_utxo_indexer.one_to_one_no_redeemer(indices: Pairs<Int,Int>, spending_script_hash, inputs, outputs, validation_logic: fn(Int, Input, Int, Output) -> Bool) -> Bool` | Many pairs at once, redeemers ignored. |
| `multi_utxo_indexer.one_to_one_with_redeemer(indices, spending_script_hash, stake_script_hash, inputs, outputs, redeemers, spend_redeemer_coercer_and_stake_credential_extractor: fn(Data) -> (a, Credential), validation_logic: fn(Int, Input, a, Int, Output) -> Bool) -> Bool` | Many pairs, coupled to a withdraw-0 script, giving each pair its own redeemer. |

The `double_satisfaction_prevented: Bool` argument is a deliberate API wart – a required
reminder that the library does not solve double satisfaction for you.

### 3.3 Transaction-level validator minting policy – `tx_level_minter`

Problem: same as the stake validator, but coupling spend to the **mint** endpoint instead.

- `validate_mint(mint_script_hash: PolicyId, mint: Value, redeemers, mint_redeemer_index: Int, mint_validator: fn(Redeemer, Dict<AssetName, Int>) -> Bool) -> Bool`
- `validate_mint_minimal(mint_script_hash: PolicyId, mint: Value) -> Bool`

### 3.4 Validity range normalization – `validity_range_normalization`

Problem: `ValidityRange` admits meaningless and redundant encodings (exclusive integer bounds,
`Finite` + `is_inclusive: False`, inverted ranges).

- `type NormalizedTimeRange { ClosedRange{lower,upper} | FromNegInf{upper} | ToPosInf{lower} | Always | InvalidRange }`
- `normalize_time_range(validity_range: ValidityRange) -> NormalizedTimeRange`

### 3.5 Merkelized validator – `merkelized_validator`

Problem: script size limit (and the 200 KiB reference-script cap plus exponential fees). Move
expensive computation into a withdrawal script and import only its result.

- `type ComputationRedeemer<a, b> { input, output }`, `type ValidationRedeemer<a>`
- `delegated_compute(function_input: a, staking_validator: ScriptHash, redeemers, redeemer_index: Int, input_data_coercer: fn(Data) -> a, output_data_coercer: fn(Data) -> b) -> b`
- `delegated_validation(function_input: a, staking_validator, redeemers, redeemer_index, input_data_coercer) -> Bool`
- `computation_withdrawal_wrapper(redeemer: ComputationRedeemer<a,b>, function: fn(a) -> b) -> Bool`
- `validation_withdrawal_wrapper(redeemer: ValidationRedeemer<a>, validation: fn(a) -> Bool) -> Bool`

### 3.6 Parameter validation – `parameter_validation` (+ `/advanced`)

Problem: proving on-chain that a given script hash is "the instance of my parameterized script
applied to *this* parameter", without shipping the whole script.

Base module (hashed, fixed-length parameters):
`apply_param(version: Int, prefix: ByteArray, param: ByteArray) -> ScriptHash`,
`apply_prehashed_param(..)`, plus `_2`/`_3` arities of both;
redeemer types `ParameterizedRedeemer<p, r>`, `ParameterizedRedeemer2`,
`ParameterizedRedeemer3`, `Parameter<p>`, `Parameter2`, `Parameter3`; wrappers
`wrapper(hashed_parameter, parameter_serialiser, outer_redeemer, validator_function)`,
`wrapper_2`, `wrapper_3`, `wrapper_no_redeemer`, `wrapper_no_redeemer_2`,
`wrapper_no_redeemer_3`.

Advanced module (arbitrary `Data` parameter):
`advanced.apply_param(version: Int, flat_prefix_without_parameter_header: ByteArray, parameter: Data) -> ScriptHash`
– serialises the parameter canonically, handles variable-length CBOR and Flat chunking, and
rebuilds the applied script hash. Needs a generated prefix constant per script
(`env/default.ak` carries 25 such constants for tests).

### 3.7 Linked list – `linked_list` (+ `/advanced`, `/nested`)

Problem: on-chain collections do not fit in a datum. Spread them across authenticated UTxOs,
each holding ADA, exactly one list NFT, an inline datum, and a `Link` to its successor.

Reader aliases `ElementEval<r>`, `RootEval<r>`, `NodeEval<r>` are partially-applied
configurations finalized by `run_element_with`, `run_root_with`, `run_node_with`.

Core operations (base module, 42 decls): `init`, `deinit`, `insert_ascending`,
`insert_descending`, `append_unordered`, `prepend_unordered`, `remove`, `fold_from_root`,
`spend_for_adding_or_removing_an_element`, `spend_for_updating_elements_data`,
`get_element_info`, `get_root_element_info`, `get_node_element_info`,
`validate_singular_authentic_input`, `validate_dual_authentic_inputs`.
Each structural operation takes an `additional_validations` callback
(`OrderedInsertValidation`, `AppendValidation`, `PrependValidation`, `RemoveValidation`,
`FoldValidation`, `UpdateValidation`) so the application logic layers on top of the
structural checks.

`advanced` (31 decls) adds reference-script visibility and tolerates permitted same-policy
mint/burn changes and foreign-credential inputs.
`nested` (29 decls) adds `Root`/`InnerRoot`/`Node` two-level lists with
`insert_into_inner_list`, `insert_inner_list_ascending`, `insert_inner_list_descending`.

### 3.8 `aiken-design-patterns/utils` – the incidental grab-bag

Reusable pieces (**H** unless noted) that Anastasia had to write because stdlib lacks them:
`get_withdraw_scripts_redeemer_at`, `utxo_is_spent(inputs, utxo_out_ref)`, `sort_inputs`,
`resolve_output_reference` (copied verbatim from Fortuna),
`get_single_asset_from_value_apart_from_ada(v, return: Scott3<...>)`,
`get_lovelace_and_single_nft_name(value, nft_policy_id, return)`,
`authentic_input_is_reproduced_unchanged(auth_symbol, optional_auth_name, in_utxo, out_utxo)`,
`find_index_of_first_script_input(inputs)`, `sum_of_squares` (P).
Plus 15 `Fuzzer` generators for `Value`/`Output`/`Input` used by its own tests.

---

## 4. `sidan-lab/vodka` 0.1.23

Two halves, both re-exported from umbrella modules `cocktail` and `mocktail`.

### 4.1 `cocktail` – on-chain validation utilities (all **H**)

`cocktail/vodka_inputs` (15):

| Signature | Semantics |
|---|---|
| `input_inline_datum(input: Input) -> a` | Extract and coerce the inline datum of an input. |
| `only_input_datum_with(inputs, policy: PolicyId, name: AssetName) -> a` | Inline datum of the single input carrying that asset. |
| `inputs_at(inputs, address) -> List<Input>` | Filter by address. |
| `inputs_with(inputs, policy, name) -> List<Input>` | Filter by asset. |
| `inputs_with_policy(inputs, policy) -> List<Input>` | Filter by policy. |
| `inputs_at_with(inputs, address, policy, name) -> List<Input>` | Filter by address + asset. |
| `inputs_at_with_policy(inputs, address, policy) -> List<Input>` | Filter by address + policy. |
| `i_at`, `i_with`, `i_with_policy`, `i_at_with`, `i_at_with_policy` | The same five as **predicates** `fn(Input) -> Bool`, for reuse with `group_inputs`. |
| `inputs_token_quantity(inputs, token: (PolicyId, AssetName)) -> Int` | Total quantity across inputs. |
| `group_inputs(inputs, group: fn(Input) -> Bool) -> (List<Input>, List<Input>)` | One-pass partition. |
| `group_inputs_2(inputs, group1, group2) -> (List<Input>, List<Input>, List<Input>)` | One-pass three-way partition. |

`cocktail/vodka_outputs` (13): the exact mirror – `output_inline_datum`, `outputs_at`,
`outputs_with`, `outputs_with_policy`, `outputs_at_with`, `outputs_at_with_policy`, predicates
`o_at`/`o_with`/`o_with_policy`/`o_at_with`/`o_at_with_policy`, `group_outputs`,
`group_outputs_2`.

`cocktail/vodka_value` (12):
`value_length(value) -> Int`,
`get_all_value_to(outputs, address) -> Value`,
`get_all_value_from(inputs, address) -> Value`,
`get_all_value_to_script(outputs, script_hash) -> Value`,
`get_all_value_from_script(inputs, script_hash) -> Value`,
`get_all_value_to_cred(outputs, cred) -> Value`,
`get_all_value_from_cred(inputs, cred) -> Value`,
`value_geq(greater: Value, smaller: Value) -> Bool`,
`value_policy_info(value, policy) -> Option<(ByteArray, ByteArray, Int)>`,
`value_tokens(value) -> List<(PolicyId, AssetName, Int)>`,
`inputs_value(inputs) -> Value`, `outputs_value(outputs) -> Value`.

`cocktail/vodka_mints` (4):
`check_policy_only_burn(mint, policy) -> Bool`,
`policy_only_minted_token(mint, policy, name, quantity) -> Bool`,
`only_minted_token(mint, policy, name, quantity) -> Bool`,
`token_minted(mint, policy, name, quantity) -> Bool`.

`cocktail/vodka_extra_signatories` (3):
`key_signed(extra_signatories, key) -> Bool`,
`one_of_keys_signed(extra_signatories, keys) -> Bool`,
`all_key_signed(extra_signatories, keys) -> Bool`.

`cocktail/vodka_validity_range` (2):
`valid_after(validity_range, required_timestamp) -> Bool`,
`valid_before(validity_range, required_timestamp) -> Bool`.

`cocktail/vodka_address` (5): `compare_script_address`, `compare_address`,
`address_payment_key(address) -> Hash<Blake2b_224, ByteArray>`,
`address_pub_key(address) -> Option<VerificationKeyHash>`,
`address_script_hash(address) -> Option<ScriptHash>`.

`cocktail/vodka_redeemers` (3):
`redeemer_from(redeemers, inputs, output_reference, input_address) -> Option<Data>`,
`withdrawal_redeemer(redeemers, withdrawal_script_hash) -> Option<Data>`,
`compare_output_reference(x, y) -> Ordering`.

`cocktail/vodka_withdrawals` (1):
`withdrawal_script_validated(withdrawals, withdrawal_script_hash) -> Bool`.

`cocktail/vodka_certificate` (7): `register_stake_certificate(certificates, credential)`,
`unregister_stake_certificate`, `register_drep_certificate(certificates, credential, deposit)`,
`unregister_drep_certificate(.., refund)`,
`delegate_stake_certificate(certificates, credential, stake_pool)`,
`delegate_vote_certificate(certificates, credential, delegate_representative)`,
`delegate_stake_and_vote_certificate(..)` – all `-> Bool`.

`cocktail/vodka_crypto` (2): `verify_signatures(keys, msg, sigs) -> Bool`,
`verify_pub_keys(keys, pkhs) -> Bool`.

`cocktail/vodka_converter` (2, **P**): `convert_int_to_bytes(i) -> ByteArray` (decimal
"stringify"), `get_number_digit(i) -> Int`.

`cip` (6): CIP-68 prefixes `cip68_100_prefix` `#"000643b0"`, `_222` `#"000de140"`, `_333`
`#"0014df10"`, `_444` `#"001bc280"`; `drop_cip68_prefix(asset_name)`, `cip68_100`, `cip68_222`,
`cip68_333`, `cip68_444`; `type CIP68Metadata`.

### 4.2 `mocktail` – on-chain test transaction builder (29 + 19 decls)

A fluent builder, every step gated on a `condition: Bool` so negative tests are one-liners.

Builder: `mocktail_tx() -> MocktailTx`, then
`tx_in(tx, condition, tx_hash, tx_index, amount, address)`, `tx_in_inline_datum(tx, cond, datum)`,
`tx_out(tx, cond, address, amount)`, `tx_out_inline_datum(tx, cond, datum)`,
`mint(tx, cond, quantity, policy_id, token_name)`,
`ref_tx_in(..)`, `ref_tx_in_inline_datum(..)`,
`invalid_before(tx, cond, time)`, `invalid_hereafter(tx, cond, time)`,
`required_signer_hash(tx, cond, key)`,
`script_withdrawal(tx, cond, script_hash, withdrawal_amount)`,
`withdrawal_redeemer_value(tx, cond, redeemer)`,
`complete(tx) -> Transaction`.

Direct `Transaction` manipulators (also condition-gated): `add_input`, `add_reference_input`,
`add_output`, `set_fee`, `add_mint`, `add_certificate`, `add_withdrawal`,
`add_extra_signatory`, `add_redeemer`, `add_datum`, `set_transaction_id`.

Deterministic mock values, all indexed by an `Int` "variation" so tests get distinct but
reproducible hashes: `mocktail/virgin_key_hash` (`mock_key_hash`, `mock_policy_id`,
`mock_pub_key_hash`, `mock_script_hash`, `mock_stake_key_hash`, `mock_script_stake_key_hash`),
`mocktail/virgin_address` (`mock_verfication_key_credential`, `mock_pub_key_address`,
`mock_script_credential`, `mock_script_address`, `mock_pub_key_stake_cred`,
`mock_script_stake_cred`), `mocktail/virgin_output_reference` (`mock_tx_hash`,
`mock_utxo_ref(variation, output_index)`), `mocktail/virgin_outputs` (`mock_output`,
`mock_pub_key_output`, `mock_script_output`), `mocktail/virgin_validity_range`
(`mock_interval(lower: Option<Int>, upper: Option<Int>)`).

---

## 5. Other libraries in the ecosystem

Source: `aiken-lang/awesome-aiken` README, verified by cloning.

### 5.1 `aiken-lang/fuzz` – property testing (169 decls)

`aiken/fuzz` (57, **P**): `bool`, `constant`, `byte`, `bytearray`, `bytearray_between`,
`bytearray_fixed`, `data`, `int`, `int_between`, `int_at_least`, `int_at_most`, `list`,
`list_between`, `list_at_least`, `list_at_most`, `list_with_elem`, `pick`, `set`,
`set_between`, `set_at_least`, `set_at_most`, `set_with_elem`, `tuple`..`tuple9`,
`and_then`, `both`, `either`..`either9`, `map`..`map9`, `one_of`, `option`, `sublist`,
`subset`, `such_that`, `label`, `label_when`, `label_if`.

`cardano/fuzz` (77, **H**) – the piece Scalus has no analogue for: domain generators for the
entire ledger vocabulary. `address`/`address_with`, `credential`, `script`, `script_hash`,
`verification_key`, `verification_key_hash`, `stake_credential`, `inline`, `pointer`,
`asset_name`, `policy_id`, `lovelace`, `value(self: Value)` / `value_with(...)` (extend an
existing value), `certificate` and one generator per Conway certificate variant (~20),
`delegate*`, `delegate_representative`, `stake_pool_id`, `transaction_id`, `datum`,
`datum_hash`, `inline_datum`, `no_datum`, `input`/`input_with`, `output`/`output_with`,
`output_reference`/`output_reference_with`, `reference_script`, `withdrawals`,
`withdrawals_extending`, `withdrawals_with`. Each `X()` has an `X_with(...)` variant taking
sub-fuzzers.

`aiken/fuzz/scenario` (35, **H**): state-machine scenario generation.
`run(scenario: List<Transaction>, script, mint, spend, withdraw, publish, vote)`,
`ok(initial_state, step)`, `ko(initial_state, step)`, `report_coverage`, `check_coverage`,
`classify`, handler adapters `into_mint_handler` .. `into_vote_handler`, and the
`fork`/`fork_if`/`fork2`/`fork3`/`fork4` (+ `_and_then`) family for weighted branching.

### 5.2 `Cardano-Fans/acca` – "extensions to the standard library"

`acca/list` (35, **P**): `force_at`, `force_head`, `force_tail`, `reduce_left`, `reduce_right`,
`first`, `non_empty`, `prepend`, `append`, `take_until`, `reject`, `without`, `without_all`,
`grouped(n)`, `bi_diff`, `sliding(n)`, `indices`, `index_of`, `max(compare)`, `min(compare)`,
`sum`, `product`, `combinations(n)`, `permutations`, `count_by`, `count`, `flatten`, `resolve`,
`indexed_foldl`, `indexed_reduce`, `indexed_all`, `indexed_any`, `indexed_filter`,
`indexed_partition`, `indexed_find`.
`acca/either` (14, **P**): full `Either<a,b>` with `left`, `right`, `contains`, `fold`,
`to_option`, `to_list`, `is_left`, `is_right`, `exists`, `swap`, `map`, `map_right`,
`map_left`.
`acca/ordering` (8, **P**): `compare`, `compare_option`, `reduce`, `force_reduce`,
`compare_tuple`, `compare_tuple_left`, `compare_pair_left`, `to_string` – the "compose several
comparators" toolkit.
`acca/collections/{mt, hash_tree, hashed_list, stack}`: Merkle tree (`from_list`, `root`,
`get_proof`, `is_member`, `verify_proof`, `equals`, `size`, `is_empty`, `to_list`), a hash tree,
and a functional `Stack` (`new`, `from_list`, `push`, `pop`, `size`, `peek`, `is_empty`).
`acca/{address, constraint, datums, dict, math, option, predicate, string, time, validators}`
(**H**): `find_input_address`, `must_be_signed_by(transaction, pkh)`, `is_inline_datum`,
`dict.sort`, `min/max` with comparator, `gcd`, `force_get`, `flat_map`, `not(predicate)`,
`must_start_after(range, lower_bound)`, `must_start_before`,
`own_validator_script_address_hash(inputs, output_reference)`,
`any_output_contains_own_validator_address(inputs, outputs, output_reference)`.

### 5.3 `SundaeSwap-finance/aicone` – `sundae/multisig`

Small but very widely reused: `type MultisigScript` (signature / all-of / any-of / at-least /
before / after / script / withdrawal composition) plus
`satisfied(script, signatories, valid_range, withdrawals) -> Bool` and
`satisfied_payload(script, payload, signatures: List<(VerificationKey, Signature)>, valid_range, withdrawals) -> Bool`.
This is the de-facto on-chain authorization DSL for the Sundae stack.

### 5.4 `logicalmechanism/assist` – "a collection of specialized Aiken functions"

Structured exactly like the helper list a real contract author accumulates:

- `validation/find` (13, **H**): `first_input_index`, `first_input_txid`,
  `input_by_ref(inputs, out_ref) -> Input` (fails), `input_by_addr`, `output_by_addr`,
  `first_output_datum`, `output_datum_by_addr`, `output_by_addr_value`, `output_by_value`,
  `stake_reward_by_sc(withdraws, stake_credential) -> Int`,
  `redeemer_by_ref(redeemers, out_ref) -> Data`, `output_datum_by_nft(outputs, pid, tkn)`,
  `input_by_nft(inputs, pid, tkn)`.
- `validation/count` (7, **H**): `inputs_by_datum(inputs, amount)`, `inputs_by_vkh`,
  `outputs_by_vkh`, `inputs_by_addr(inputs, addr, amount)`, `outputs_by_addr`,
  `outputs_by_datum`, `single_input_with_bypass(inputs, this_addr, those_addrs)`.
- `validation/payout` (2, **H**): `exact(payout_address, payout_value, outputs)`,
  `at_least(payout_address, payout_value, outputs)`.
- `cardano/tx` (6, **H**): `output_reference(tx_id_hash, idx)`, `verify_signature(vks, vk)`,
  `verify_multisig(sigs, vks, minimum)`, `is_spending_input(inputs, out_ref)`,
  `not_being_spent_from(validator_hashes, inputs)`,
  `total_token_amount(inputs, pid, tkn, threshold)`.
- `cardano/value` (8, **H**): `from_token`, `from_tokens`, `multiply(val, n)`,
  `contains(total, target)`, `compute_hash(target)` (merklized value digest),
  `unique_token_name(txid, idx, prefix, personal)`, `prove_nft(total, pid)`,
  `prove_exact_nft(total_value, nft_pid, nft_tkn)`.
- `cardano/{addresses, datum, minting, certificates}`, `types/{token, wallet, moment, cip68,
  prefixes, registry}`, `maths/{boolean, circuits, constants, routines}`, `tests/fake_tx`
  (16 fake-tx builders, an older/simpler `mocktail`).

### 5.5 `aiken-lang/merkle-patricia-forestry`

`opaque MerklePatriciaForestry` with `from_root(root)`, `empty`, `is_empty`,
`has(self, key, value, proof)`, `miss(self, key, proof)`, `insert(self, key, value, proof)`,
`delete(self, key, value, proof)`, `update(self, key, proof, old_value, new_value)`,
`root(self)`; plus `helpers` and `merkling` internals.

### 5.6 Also listed by awesome-aiken (not inventoried in depth)

`ilap/bls` (high-level BLS12-381), `Anastasia-Labs/aiken-linked-list`,
`Anastasia-Labs/aiken-trie`, `aiken-extra/*` (test/debug helpers),
`aiken-lang/setup-aiken` (CI action).

---

## 6. Which functions actually get used

Measured, not guessed: call-sites counted over the six downstream contract repos plus
vodka / design-patterns / fuzz (doc comments stripped).

### 6.1 stdlib, top of the aggregate

`fuzz.*` combinators are excluded from this table: they belong to a separate library, and
almost all of the hits come from design-patterns' own test suite (`fuzz.and_then` alone scores
163, which would otherwise top the list). Field accesses such as `address.payment_credential`
are counted by the same regex and are marked as such.

| Rank | Function | Calls | Where |
|---|---|---|---|
| 1 | `assets.from_lovelace` | 122 | sundae 84, ADP 37 |
| 2 | `bytearray.concat` | 115 | ADP 71, fortuna 31, sundae 7 |
| 3 | `assets.add` | 115 | sundae 89, ADP 25 |
| 4 | `assets.from_asset` | 71 | ADP 40, sundae 31 |
| 5 | `dict.to_pairs` | 65 | fortuna 40, ADP 13 |
| 6 | `assets.merge` | 51 | ADP 45, sundae 6 |
| 7 | `cbor.serialise` | 42 | ADP 34, sundae 7 |
| 8 | `list.filter` | 41 | minswap 14, vodka 12, fortuna 8 |
| 9 | `address.payment_credential` (field) | 37 | 6 repos |
| 10 | `option.or_else` | 36 | sundae 34 |
| 11 | `list.length` | 35 | 5 repos |
| 12 | `list.at` | 33 | ADP 18, sundae 11 |
| 13 | `list.concat` | 28 | mesh 12, vodka 7 |
| 14 | `bytearray.drop` / `bytearray.take` | 27 / 25 | ADP (parameter-validation CBOR surgery) |
| 15 | `list.foldr` | 25 | vodka 11, mesh 7 |
| 16 | `list.find` | 23 | mesh 9, fortuna 7 |
| 17 | `list.has` | 23 | vodka 8, minswap 4 |
| 18 | `interval.between` | 22 | sundae 22 |
| 19 | `list.any` | 21 | vodka 7, fortuna 5 |
| 20 | `dict.get` | 20 | ADP 14 |
| – | `assets.quantity_of` | 19 | sundae |
| – | `assets.zero` / `without_lovelace` / `tokens` / `flatten` | 16 / 15 / 13 / 11 | ADP + sundae |
| – | `transaction.placeholder` | 9 | mesh 4, sundae 4 (tests) |

Read: the hot path is `Value` construction and arithmetic, `List` traversal, `Dict` lookup,
and `ByteArray` slicing. `interval` shows up only in sundae, but heavily.

### 6.2 The helpers people reach for by name

Unqualified name counts across the six downstream repos (mesh dominates the vodka numbers):

| Name | Calls | Library |
|---|---|---|
| `mock_policy_id` | 209 | vodka/mocktail |
| `mock_utxo_ref` | 186 | vodka/mocktail |
| `mock_pub_key_address` | 115 | vodka/mocktail |
| `mocktail_tx` | 71 | vodka/mocktail |
| `placeholder` | 54 | stdlib `transaction` |
| `mock_script_address` | 51 | vodka/mocktail |
| `value_geq` | 46 | vodka/cocktail |
| `get_all_value_to` | 38 | vodka/cocktail |
| `key_signed` | 37 | vodka/cocktail |
| `inputs_at` | 32 | vodka/cocktail |
| `find_input` | 29 | stdlib `transaction` |
| `outputs_at` | 16 | vodka/cocktail |
| `check_policy_only_burn` | 14 | vodka/cocktail |
| `only_minted_token` / `all_key_signed` | 11 each | vodka/cocktail |
| `one_of_keys_signed` / `inputs_with` | 8 each | vodka/cocktail |
| `get_all_value_from` | 6 | vodka/cocktail |
| `valid_after` | 4 | vodka/cocktail |

Two conclusions that matter for the Scalus design:

1. **The single most-used third-party API in the ecosystem is a test transaction builder.**
   `mocktail` + `virgin_*` mocks outrank every validation helper by 3-5x. Deterministic,
   variation-indexed mock hashes and a condition-gated builder are what contract authors
   actually type all day.
2. Among *validation* helpers, the winners are dead simple: `value_geq`,
   `get_all_value_to(outputs, address)`, `key_signed`, `inputs_at`/`outputs_at`,
   `check_policy_only_burn`, `only_minted_token`. None of these are in stdlib. That is exactly
   why vodka exists, and it is the shortest list of things a Scalus high-level API must have.

---

## 7. Gap table: Aiken to Scalus

Scope checked: `scalus-core/shared/src/main/scala/scalus/cardano/onchain/plutus/**`, plus
`scalus.uplc.builtin.{Builtins, ByteString, Bitwise}` for primitives that Scalus exposes as
builtins, plus the JVM-only `scalus-design-patterns` module where relevant.

### 7.1 `cardano/transaction`

| Aiken | Scalus today | Status |
|---|---|---|
| `find_input(inputs, out_ref)` | `v1/v2/v3.Utils.findInput`, `TxInfo.findOwnInput` | EXISTS |
| `resolve_input(inputs, out_ref) -> Output` (fails) | `v3 TxInfo.findOwnInputOrFail(outRef, message)` returns `TxInInfo` | PARTIAL (v3 only, returns the input not the output) |
| `find_datum(outputs, datums, hash)` | `v2.Utils.findDatum`, `v2/v3 TxInfo.findOwnDatum` | EXISTS |
| `find_script_outputs(outputs, hash)` | `Utils.findScriptOutputs`, `TxInfo.findOwnScriptOutputs` | EXISTS |
| `placeholder` | `v1/v2/v3 TxInfo.placeholder` | EXISTS |
| `output_reference.compare`, `script_purpose.compare` | `given Ord[TxOutRef]`, `Ord[ScriptPurpose]` | EXISTS |
| – | Scalus extras with no Aiken analogue: `findOwnInputsByCredential`, `findOwnOutputsByCredential`, `findOwnInputs(pred)`, `findOwnOutputs(pred)`, `isSignedBy`, `getValidityStartTime`, `Utils.getAdaFromInputs/Outputs` | Scalus ahead |

### 7.2 `cardano/assets` vs `scalus...plutus.v1.Value`

| Aiken | Scalus today | Status |
|---|---|---|
| `from_asset(p,n,q)` | `Value(cs, tn, v)` | EXISTS |
| `from_lovelace(q)` | `Value.lovelace(v)` | EXISTS |
| `zero` | `Value.zero` | EXISTS |
| `from_asset_list` | `Value.fromList` / `unsafeFromList` | EXISTS |
| `from_ascending_pairs` | `Value.fromStrictlyAscendingListWithNonZeroAmounts`, `unsafeFromSortedMap` | EXISTS |
| `merge` / `difference` / `negate` | `plus`/`+`, `minus`/`-`, `negate`/`unary_-` | EXISTS |
| – | `Value.multiply(v, factor)` / `*` | Scalus ahead |
| `add(self, p, n, q)` (**adds**) | `insertCoin(cs, tn, amount)` **sets** the amount | PARTIAL – semantics differ; `v + Value(p,n,q)` is the workaround |
| `quantity_of` | `quantityOf` | EXISTS |
| `expect_quantity_of` | – | MISSING |
| `lovelace_of` | `getLovelace` / `lovelaceAmount` | EXISTS |
| `expect_lovelace_of` | – | MISSING |
| `tokens(policy)` | `tokens(cs)` | EXISTS |
| `policies` | `policyIds` | EXISTS |
| `flatten` | `flatten` | EXISTS |
| `flatten_with(strategy)` | – | MISSING |
| `reduce(start, with)` | – | MISSING (only via `toSortedMap.foldLeft`) |
| `to_dict` / `to_pairs` | `toSortedMap` / `pairs` / `tokenPairs` | EXISTS |
| `without_lovelace` | `withoutLovelace` | EXISTS |
| `restricted_to(mask)` | – | MISSING |
| `is_zero` | `isZero` / `nonZero` | EXISTS |
| `contains(tokensDict, subset)` | `containsAtLeast(other: Value)` (Value-level) | PARTIAL |
| `has_nft`, `has_nft_strict`, `has_any_nft`, `has_any_nft_strict` | `hasOnly(cs, tn, amount)` only | PARTIAL |
| `match`, `expect_match`, `match_assets`, `expect_match_assets` | `Value.eq`, `nonEq`, `given valueEq` | PARTIAL – no Ada-aware/asset-only compare |
| `expect_tail` | – | MISSING |
| `strategy.triple()` and `FlattenStrategy` | – | MISSING |
| – | `Value.isPositive`, `valueFromDataWithValidation`, `toLedgerValue`, `showDebug` | Scalus ahead |

### 7.3 `cardano/address`

| Aiken | Scalus today | Status |
|---|---|---|
| `from_verification_key(vk)` | `Address.fromPubKeyHash` | EXISTS |
| `from_script(hash)` | `Address.fromScriptHash` | EXISTS |
| `Address.from_credential` | `Address.fromCredential` | EXISTS |
| `with_delegation_key(self, vk)` | – | MISSING |
| `with_delegation_script(self, hash)` | – | MISSING |
| `credential.compare` | `given Ord[Credential]` in `v1/Contexts.scala` L487 | **PARTIAL / divergent** – see the callout below |
| – | `Credential.pubKeyOption`, `Credential.scriptOption` | Scalus ahead |

> **Incidental finding: `Ord[Credential]` disagrees with ledger order.**
> Aiken sorts `Script` **below** `VerificationKey`
> (`stdlib/lib/cardano/address/credential.ak`: `Script(_) -> ... _ -> Less`), and
> `stdlib/lib/cardano/transaction.ak` documents this explicitly on the `withdrawals` field:
> *"Withdrawals are ordered by ascending Credential. Yet, note that `Script` credentials are
> treated as **lower values** than `VerificationKey` credentials."* This matches cardano-ledger,
> where `Credential = ScriptHashObj | KeyHashObj` and `ScriptHashObj` is the first constructor.
>
> Scalus's `given Ord[Credential]` uses the opposite (Plutus-Data constructor) order:
> `PubKeyCredential` is `Order.Less` than `ScriptCredential`.
>
> Why this may matter: `v3.TxInfo.withdrawals: SortedMap[Credential, Lovelace]` is decoded with
> `sortedMapFromData`, which calls `unsafeFromList` and therefore keeps the ledger's ordering
> without re-sorting. `SortedMap.get` short-circuits on ordering
> (`prelude/SortedMap.scala` L625: `case Order.Less => None`). If the decoded list really is in
> ledger order (Script keys first), a `get` for a `PubKeyCredential` would compare `Less` against
> the first Script key and return `None` for a key that is present. The same reasoning applies to
> `redeemers: SortedMap[ScriptPurpose, Redeemer]` and `votes: SortedMap[Voter, ...]`, whose
> Aiken counterparts also have dedicated `compare` modules for exactly this reason.
>
> Not verified by execution here – this is a research observation, and it should be confirmed
> with a test against a real `ScriptContext` before being treated as a defect.

### 7.4 `cardano/certificate`, `governance`, `script_context`

| Aiken | Scalus today | Status |
|---|---|---|
| `Certificate`, `Delegate`, `DelegateRepresentative`, `StakePoolId` | `v3.TxCert`, `v3.Delegatee`, `v3.DRep` | EXISTS |
| `ProposalProcedure`, `GovernanceAction`, `Vote`, `Voter`, `GovernanceActionId`, `ProtocolVersion`, `Constitution`, `Committee` | all present in `v3/Contexts.scala` | EXISTS |
| `governance/voter.compare` | `given Ord[Voter]` | EXISTS |
| `protocol_parameters.*` – 30 typed getters over `ProtocolParametersUpdate` | `type ChangedParameters = Data` (opaque) | **MISSING** – biggest single-module gap |
| `ScriptContext`, `ScriptInfo` | `v3.ScriptContext`, `v3.ScriptInfo` | EXISTS |

### 7.5 `aiken/interval`

| Aiken | Scalus today | Status |
|---|---|---|
| `after`, `before`, `between`, `entirely_before`, `entirely_between` | `Interval.after/before/between/entirelyBefore/entirelyBetween` | EXISTS |
| `entirely_after(lb)` | – (no `entirelyAfter` anywhere in scalus-core) | MISSING |
| `everything` / `empty` | `Interval.always` / `Interval.never` | EXISTS |
| `contains(elem)` | `contains(time)` | EXISTS |
| `is_entirely_after` / `is_entirely_before` | `isEntirelyAfter` / `isEntirelyBefore` | EXISTS |
| `is_empty` (computed) | `isNever` (identity check against the canonical `never`) | PARTIAL |
| `includes(other)` | – | MISSING |
| `hull` / `intersection` | `Interval.hull` / `Interval.intersection` | EXISTS |
| `to_string` | – (there is `Show`, not an `Interval` renderer) | MISSING |
| – | `isEntirelyBetween`, `isOpenInterval`, `nonNever`, `IntervalBound.min/max`, `finite(default)`, `finiteOrFail` | Scalus ahead |

### 7.6 `aiken/collection/list` vs `prelude.List`

| Aiken | Scalus today | Status |
|---|---|---|
| `push`, `range`, `repeat` | `prepended`/`+:`, `List.range`/`rangeUntil`, `List.fill` | EXISTS |
| `all`, `any`, `count`, `find`, `find_map`, `has`, `head`, `is_empty`, `index_of`, `last`, `length` | `forall`, `exists`, `count`, `find`, `findMap`, `contains`, `headOption`, `isEmpty`, `indexOfOption`, `lastOption`, `length` | EXISTS |
| `at -> Option` | `get(index)` (Option) and `at(index)` (fails) | EXISTS |
| `expect_at`, `expect_head`, `expect_last`, `expect_tail` | `!!`/`at`, `head`, `last`, `tail` (these fail) | EXISTS (different naming convention) |
| `expect_any`, `expect_find`, `expect_find_map`, `expect_has`, `expect_index_of`, `expect_delete`, `expect_drop`, `expect_init`, `expect_take` | – | MISSING |
| `delete`, `drop`, `drop_while`, `filter`, `filter_map`, `take`, `take_while`, `unique` | `deleteFirst`, `drop`, `dropWhile`, `filter`/`filterNot`, `filterMap`, `take`, `takeWhile`, `distinct` | EXISTS |
| `init -> Option` | `init` (fails) | PARTIAL |
| `partition -> (List, List)` | – | MISSING |
| `slice(from, to)` | – (compose `drop`+`take`) | MISSING |
| `span(n) -> (List, List)` | – | MISSING |
| `flat_map`, `for_each`, `map`, `reverse`, `sort` | `flatMap`, `foreach`, `map`, `reverse`, `sort`/`quicksort` | EXISTS |
| `indexed_map`, `indexed_foldr` | – | MISSING |
| `map2` | `List.map2(a, b)(f)` | EXISTS |
| `map3` | – | MISSING |
| `unzip` | – | MISSING |
| `zip` | `zip` | EXISTS |
| `concat`, `difference` | `++`/`concat`/`appendedAll`, `diff` | EXISTS |
| `foldl`, `foldr`, `reduce` | `foldLeft`, `foldRight` | EXISTS |
| `foldl2` / `foldr2` (CPS double accumulator) | – | MISSING |
| – | `groupBy`, `groupMap`, `groupMapReduce`, `dropRight`, `takeRight`, `isDefinedAt`, `flatten`, `asScala` | Scalus ahead |

### 7.7 `aiken/collection/dict` vs `prelude.SortedMap` / `AssocMap`

| Aiken | Scalus today | Status |
|---|---|---|
| `empty`, `singleton`, `from_pairs`, `from_ascending_pairs` | `SortedMap.empty/singleton/fromList/fromStrictlyAscendingList/unsafeFromList` | EXISTS |
| `from_ascending_pairs_with(xs, predicate)` | – | MISSING |
| `get`, `has_key`, `is_empty`, `keys`, `size`, `values` | `get`, `contains`, `isEmpty`, `keys`, `size`/`length`, `values` | EXISTS |
| `get_or_else(key, or_else: fn() -> value)` | `get(k).getOrElse(v)` (eager) | PARTIAL |
| `expect_get`, `expect_has_key`, `expect_find`, `expect_contains`, `expect_delete`, `expect_pop`, `expect_tail` | `getOrFail(key, message)`, `at(key)` | PARTIAL |
| `find(value) -> Option<key>` | `find(predicate)` | PARTIAL |
| `contains(self, subset, compare)` | – | MISSING |
| `delete`, `filter`, `insert`, `map` | `delete`, `filter`/`filterKeys`/`filterNot`, `insert`, `mapValues` | EXISTS |
| `insert_with(.., with: UnionStrategy)` | – | MISSING |
| `pop(key) -> (Option<value>, Dict)` | – | MISSING |
| `union` | `SortedMap.union -> SortedMap[A, These[B,C]]` | PARTIAL (different shape) |
| `union_with(.., strategy)` | `SortedMap.unionMap(lhs, rhs, f: These[B,C] => D)` | EXISTS (equivalent) |
| `difference_with(.., with_both, with_right)` | – | MISSING |
| `foldl`, `foldr` | `foldLeft`, `foldRight` | EXISTS |
| `foldl2`, `foldr2` | – | MISSING |
| `to_pairs` | `toList` / `toPairList` | EXISTS |
| whole `dict/strategy` module (11 combinators) | – | **MISSING** |

### 7.8 `aiken/collection/pairs` vs `prelude.PairList`

| Aiken | Scalus today | Status |
|---|---|---|
| `keys`, `values`, `map`, `foldl`, `foldr` | `toList`+`map`, `mapValues`, `map`, `foldLeft`, `foldRight` | PARTIAL (`keys`/`values` not direct) |
| `get_first`, `get_last`, `get_all`, `has_key` | – (only `find(pred)`) | MISSING |
| `find_first`, `find_last`, `find_all` | `find` (first match by predicate) | PARTIAL |
| `delete_first`, `delete_last`, `delete_all` | – | MISSING |
| `insert_by_ascending_key`, `insert_with_by_ascending_key`, `repsert_by_ascending_key` | – | MISSING |
| `pop_until`, `pop_until_key` (+ `_and_then`, + `expect_*`) – 8 functions | – | **MISSING** |
| all 5 `expect_*` query variants | – | MISSING |
| – | `findMap`, `filterNot`, `unsafeToSortedMap`, `unsafeToAssocMap` | Scalus ahead |

### 7.9 `aiken/crypto`

| Aiken | Scalus today | Status |
|---|---|---|
| `blake2b_224`, `blake2b_256`, `keccak_256`, `sha2_256`, `sha3_256` | `Builtins.*` (plus `ripemd_160`, which Aiken lacks) | EXISTS |
| `verify_ed25519_signature`, `verify_ecdsa_signature`, `verify_schnorr_signature` | `Builtins.verifyEd25519Signature`, `verifyEcdsaSecp256k1Signature`, `verifySchnorrSecp256k1Signature` | EXISTS |
| `Hash<alg, a>` phantom-typed digests | untyped `ByteString` | MISSING (type-safety feature) |
| `bls12_381/g1`, `/g2`, `/scalar`, `/pairing` | `prelude/bls12_381/{G1,G2,Scalar}` | EXISTS |
| `bls12_381` DSTs and size constants | – | MISSING |
| `crypto/bitwise` generic modular `State<t>` | `uplc.builtin.Bitwise` (raw builtins only) | PARTIAL |
| `crypto/int224`, `crypto/int256` (21 fns each) | – | MISSING |
| – | `crypto/tree/MerkleTree`, `IncrementalMerkleTree`, `crypto/trie/MerklePatriciaForestry`, `FusedMerklePatriciaForestry`, `crypto/accumulator/{G1,G2}Accumulator`, `Poly` | Scalus ahead (MPF is a separate library in Aiken) |

### 7.10 `aiken/math`, `math/rational`, `option`, `primitive/*`, `cbor`

| Aiken | Scalus today | Status |
|---|---|---|
| `math.{abs, clamp, gcd, is_sqrt, log, log2, max, min, pow, pow2, sqrt}` | `Math.{abs, clamp, gcd, isSqrt, log, log2, max, min, pow, exp2, sqrt}` + `BigInt` extensions | EXISTS (`sqrt` returns `BigInt`, not `Option`) |
| `math/rational` – 25 fns | `Rational(numerator, denominator)` + `isZero`, `normalize`, `Ord`, codecs | **PARTIAL** – no `add`/`sub`/`mul`/`div`/`ceil`/`floor`/`round`/`round_even`/`truncate`/`reciprocal`/`abs`/`negate`/`pow`/`compare_with`/`proper_fraction`/means |
| `option.{is_none, is_some, and_then, flatten, map, or_else}` | `isEmpty`, `isDefined`, `flatMap`, `flatten`, `map`, `getOrElse` | EXISTS |
| `option.or_try(compute_default: fn() -> Option)` | `orElse(alternative: Option[B])` (by-value) | PARTIAL |
| `option.choice(List<Option<a>>)` | – | MISSING |
| `option.map2`, `option.map3` | – | MISSING |
| – | `getOrFail(message)`, `orFail(message)`, `filter`, `filterNot`, `contains`, `exists`, `forall`, `find`, `asScala` | Scalus ahead |
| `bytearray.{slice, take, drop, at, length, is_empty, concat, to_hex}` | `ByteString` extensions + `Builtins.sliceByteString`/`indexByteString`/`lengthOfByteString`/`appendByteString`, `toHex` | EXISTS |
| `bytearray.{from_int_big_endian, from_int_little_endian, to_int_big_endian, to_int_little_endian}` | `ByteString.fromBigIntBigEndian/LittleEndian`, `Builtins.byteStringToInteger` | EXISTS |
| `bytearray.{and_bytes, or_bytes, xor_bytes}` | `Builtins.andByteString`/`orByteString`/`xorByteString` | EXISTS |
| `bytearray.starts_with(self, prefix)` | – | MISSING |
| `bytearray.index_of(self, bytes) -> Option<(Int,Int)>` | – | MISSING |
| `bytearray.test_bit(self, ix)` | – | MISSING |
| `bytearray.push(self, byte)` | – | MISSING |
| `bytearray.{foldl, foldr, reduce}` over bytes | – | MISSING |
| `bytearray.compare` | `given Ord[ByteString]` | EXISTS |
| `bytearray.{from_string, to_string}` | `Builtins.encodeUtf8`/`decodeUtf8` | EXISTS |
| `int.{compare, from_bytearray_*, to_string}` | `Ord[BigInt]`, `Builtins.byteStringToInteger`, `Prelude.showBigInt`/`showByteStringBigInt` | EXISTS |
| `int.from_utf8 -> Option<Int>` | – | MISSING |
| `string.{from_bytearray, from_int, concat, to_bytearray}` | `Builtins.decodeUtf8`, `showBigInt`, `appendString`, `encodeUtf8` | EXISTS |
| `string.join(list, delimiter)` | – | MISSING |
| `cbor.serialise` | `Builtins.serialiseData` | EXISTS |
| `cbor.diagnostic(Data) -> String` | `Show` instances, `Value.showDebug` | PARTIAL |
| `cbor.deserialise(bytes) -> Option<Data>` | – | MISSING |

### 7.11 `aiken-design-patterns` vs `scalus-design-patterns` (JVM-only module)

| Aiken pattern | Scalus today | Status |
|---|---|---|
| `stake_validator.{validate_withdraw, validate_withdraw_with_amount, validate_withdraw_minimal}` | `scalus.patterns.StakeValidator.{spend, spendMinimal, withdraw}` | EXISTS |
| `singular_utxo_indexer.{one_to_one, one_to_many}` | `scalus.patterns.UtxoIndexer.{oneToOne, oneToMany, validateInput}` | EXISTS |
| `multi_utxo_indexer.{one_to_one_no_redeemer, one_to_one_with_redeemer}` | `UtxoIndexer.{multiOneToOneNoRedeemer, multiOneToOneWithRedeemer}` | EXISTS |
| `tx_level_minter.{validate_mint, validate_mint_minimal}` | `TransactionLevelMinterValidator.{spend, spendMinimal}` | EXISTS |
| `validity_range_normalization.normalize_time_range` | `NormalizedInterval.{normalize, tryNormalize, normalizedInterval, tryNormalizedInterval}` | EXISTS (Scalus adds a total `Option` variant) |
| `merkelized_validator.{delegated_compute, delegated_validation, computation_withdrawal_wrapper, validation_withdrawal_wrapper}` | `MerkelizedValidator.{getStakeRedeemer, verifyAndGetRedeemer}` | PARTIAL – no typed `ComputationRedeemer` wrapper pair |
| `parameter_validation.{apply_param*, wrapper*}` (18 decls) | `ParameterValidation.computeScriptHashV1/V2/V3` (off-chain) + `ParameterValidationOnChain.{verifyScriptCredential, verifyAddressScript, findOutputsToScript, isExpectedScript}` | PARTIAL – different (arguably better) approach; no `wrapper_2/_3`, no `advanced.apply_param` Flat-prefix trick |
| `linked_list` base (42 decls) | `scalus.patterns.LinkedList.{init, deinit, insert, appendUnordered, prependUnordered, remove, removeHead, requireListTokensMintedOrBurned, validateElementUpdate}` | PARTIAL – no `fold_from_root`, no `run_*_with` reader config |
| `linked_list/advanced` (31 decls) | – | MISSING |
| `linked_list/nested` (29 decls) | – | MISSING |
| `utils.*` (9 helpers + 15 fuzzers) | – | MISSING |

Caveat: `scalus-design-patterns` is a **JVM-only** sbt module, so JS/Native contract authors
cannot use it today. Aiken's equivalents are plain library code available everywhere.

### 7.12 `vodka/cocktail` – the biggest structural gap

Essentially **none** of vodka's 60+ validation helpers exist in Scalus, and these are the
most-typed functions in the ecosystem (section 6.2).

| Aiken (vodka) | Scalus today | Status |
|---|---|---|
| `inputs_at`, `inputs_with`, `inputs_with_policy`, `inputs_at_with`, `inputs_at_with_policy` | `TxInfo.findOwnInputsByCredential`, `findOwnInputs(pred)` | PARTIAL – credential only, no asset/policy filters |
| `outputs_at`, `outputs_with`, `outputs_with_policy`, `outputs_at_with`, `outputs_at_with_policy` | `findOwnOutputsByCredential`, `findOwnOutputs(pred)`, `findOwnScriptOutputs` | PARTIAL |
| `i_at`/`i_with`/`o_at`/... predicate factories | – | MISSING |
| `group_inputs`, `group_inputs_2`, `group_outputs`, `group_outputs_2` | – | MISSING (no `List.partition` either) |
| `input_inline_datum`, `output_inline_datum` | `v2.OutputDatum.inlineOrFail[A]`, `inlineOrFail[A](message)` | EXISTS |
| `only_input_datum_with(inputs, policy, name)` | – | MISSING |
| `inputs_token_quantity(inputs, token)` | – | MISSING |
| `value_geq(greater, smaller)` | `Value.containsAtLeast(other)` | EXISTS (Scalus additionally rejects negative amounts, and lowers to the CIP-153 `valueContains` builtin at PV11) |
| `get_all_value_to(outputs, address)` / `_from` / `_to_script` / `_from_script` / `_to_cred` / `_from_cred` | – | **MISSING** (2nd most-used vodka helper) |
| `inputs_value(inputs)`, `outputs_value(outputs)` | `Utils.getAdaFromInputs/Outputs` (Ada only) | PARTIAL |
| `value_length`, `value_policy_info`, `value_tokens` | `Value.flatten`, `policyIds`, `tokens` | PARTIAL |
| `key_signed`, `one_of_keys_signed`, `all_key_signed` | `TxInfo.isSignedBy(pubKeyHash)` | PARTIAL – no any-of / all-of |
| `check_policy_only_burn`, `only_minted_token`, `policy_only_minted_token`, `token_minted` | – | **MISSING** (mint validation has no helpers at all) |
| `valid_after`, `valid_before` | `Interval.isEntirelyAfter/Before`, `TxInfo.getValidityStartTime` | PARTIAL – needs the "validity range proves T" framing |
| `withdrawal_script_validated(withdrawals, hash)` | – | MISSING (StakeValidator covers it indirectly) |
| `redeemer_from(...)`, `withdrawal_redeemer(...)` | `StakeValidator`/`MerkelizedValidator` internals | PARTIAL |
| `address_payment_key`, `address_pub_key`, `address_script_hash` | `Credential.pubKeyOption`, `Credential.scriptOption` | PARTIAL – nothing at `Address` level |
| `compare_address`, `compare_script_address` | `given Ord[Address]` | EXISTS |
| 7 `vodka_certificate.*` predicates | – | MISSING |
| `verify_signatures`, `verify_pub_keys` | – | MISSING |
| `convert_int_to_bytes`, `get_number_digit` | `Prelude.showByteStringBigInt` | PARTIAL |
| CIP-68 prefixes + `cip68_100/222/333/444` + `drop_cip68_prefix` | – | MISSING |

### 7.13 `vodka/mocktail` and `aiken-lang/fuzz` – testing

| Aiken | Scalus today | Status |
|---|---|---|
| `mocktail_tx()` fluent, condition-gated builder (14 steps) | `TxInfo.placeholder` + manual `copy(...)`; `scalus-testkit` has off-chain `Emulator`/`TxBuilder` | MISSING (no on-chain `TxInfo` builder DSL) |
| `virgin_key_hash.mock_*` / `virgin_address.mock_*` / `mock_utxo_ref` / `mock_output` / `mock_interval` – deterministic variation-indexed mocks | – | **MISSING** (this is the single most-used API in the whole corpus) |
| 11 `Transaction` manipulators (`add_input`, `set_fee`, ...) | `case class .copy` | PARTIAL |
| `aiken/fuzz` (57 generic fuzzers) | ScalaCheck / `scalus-testkit` generators | PARTIAL (different ecosystem, not on-chain) |
| `cardano/fuzz` (77 ledger-domain fuzzers, each with a `_with` variant) | some arbitraries exist in `scalus-testkit` | PARTIAL – worth an explicit audit |
| `aiken/fuzz/scenario` (35: `ok`/`ko`/`run`/`fork*`/coverage) | – | MISSING |

---

## 8. Summary of what "match or beat" means concretely

Ordered by measured downstream demand.

1. **A mock/test-transaction API.** `mocktail` + `virgin_*` outranks every other third-party
   API by 3-5x. Deterministic variation-indexed hashes, a condition-gated fluent builder, and
   `TxInfo`-level manipulators. Scalus has none of this on the on-chain side.
2. **Transaction query helpers (vodka `cocktail`).** `inputs_at`/`outputs_at` +
   asset/policy filters, predicate factories, `group_inputs/outputs`,
   `get_all_value_to/from(_script/_cred)`, `key_signed`/`one_of_keys_signed`/`all_key_signed`,
   the four mint checks, `valid_after`/`valid_before`, `withdrawal_script_validated`.
3. **The `expect_*` convention.** Aiken pairs almost every partial function with a failing,
   cheaper variant, and downstream code uses them. Scalus has this only sporadically
   (`at`, `head`, `getOrFail`, `findOwnInputOrFail`, `inlineOrFail`). Decide whether to adopt
   it systematically, since it is both an ergonomics and a budget decision.
4. **`Value` matching and NFT predicates.** `match`/`match_assets` (Ada-aware structural
   compare), `has_nft`/`has_any_nft` (+ strict), `restricted_to`, `reduce`, and a correct
   additive `add`.
5. **`Pairs` streaming extraction.** `pop_until` / `pop_until_key` (+ `_and_then` and
   `expect_*`): the idiomatic way to walk a sorted `withdrawals`/`redeemers`/`votes` list in
   one pass. Nothing equivalent exists on `PairList`.
6. **Dict/union strategies.** The 11-combinator `strategy` module is what makes `union_with`
   and `difference_with` usable; `SortedMap.unionMap` exists but has no vocabulary.
7. **`math/rational`.** Scalus `Rational` is a data class with no arithmetic. DEX-style code
   needs the full 25-function surface.
8. **Small `List`/`ByteArray` holes** used constantly downstream: `partition`, `span`,
   `slice`, `unzip`, `indexed_map`, `indexed_foldr`, `map3`; `starts_with`, `index_of`,
   `push`, `test_bit`, byte-level folds.
9. **`protocol_parameters` getters** (30 functions). Required for any governance validator that
   inspects a `ParameterChange`.
10. **Design patterns parity and portability.** Finish `linked_list` (fold-from-root,
    `advanced`, `nested`) and the merkelized-validator typed wrappers, and consider moving
    `scalus-design-patterns` out of JVM-only so JS/Native contracts can use it.
11. **Property-testing generators.** `cardano/fuzz`'s 77 ledger generators and the
    `fuzz/scenario` state-machine harness have no Scalus counterpart.
