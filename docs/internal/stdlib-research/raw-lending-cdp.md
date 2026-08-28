# Raw research: lending / CDP / PoW protocols — what real validators re-implement by hand

Source-code survey for the Scalus "smart contract standard library" design.
Read-only analysis of five local clones. All line references are `file:line` against the
checked-out sources under
`.../scratchpad/repos/{lenfi,indigo,butane,liqwid-plutarch-extra,fortuna}`.

| Repo | Protocol | Language | Rough size analysed |
|---|---|---|---|
| `lenfi` | Lenfi / Aada — P2P lending pools | Aiken (V2 script context) | ~3.3k lines |
| `indigo` | Indigo — synthetics / CDP | PlutusTx Haskell (V2) | ~2.1k lines of on-chain lib + contracts |
| `butane` | Butane — stablecoin / CDP | Aiken (V2, `Pairs`-era) | ~4.6k lines |
| `liqwid-plutarch-extra` | Liqwid Labs — *utility library* | Plutarch | 48 modules |
| `fortuna` | Fortuna TUNA — proof of work | Aiken | ~3.9k lines |

---

## 0. Executive shape of the finding

Every one of these codebases independently re-implements the same ~25 helpers. The
overlap is near total. The four biggest clusters are:

1. **Find / resolve UTxOs** — own input, input-by-oref, input-by-NFT, unique input with
   token, reference input with token, continuing output, output-by-index.
2. **Value predicates** — `quantity_of`, "has exactly one token of this class", "value
   has ONLY these tokens plus ADA" (dust-attack guard), `without_lovelace ==`.
3. **Datum/redeemer plumbing** — inline-datum extraction + typed parse, "expected datum"
   construction and `Data`-level equality, cross-script redeemer lookup by `ScriptPurpose`.
4. **Money math** — rationals with truncation, basis-point percentages, fixed-point
   decimals, interest accrual over a *step function* of historical rates, collateral
   ratio / health factor, protocol fee tiers.

And two structural patterns that every protocol invented separately:

- **withdraw-0 forwarding** (spend/mint validators degenerate to
  `withdrawals.contains(myStakeCred)`, with all logic in one staking validator run once).
- **index/oref hints in the redeemer** (`continuing_output: Int`, `pool_oref:
  OutputReference`, `params_idx: Int`) to turn O(n) scans into O(1) lookups.

---

# 1. Lenfi / Aada (Aiken)

## 1.1 Helpers the protocol defines itself

| Name | Signature | Purpose | file:line |
|---|---|---|---|
| `get_ref_token_datum` | `(List<Input>, AssetClass) -> Option<ReferenceTokenDatum>` | Find reference input holding exactly 1 of an NFT, parse its inline datum | `lenfi/lib/aada/utils.ak:32` |
| `validate_mint_nft` | `(MintedValue, PolicyId, AssetName, Int) -> Bool` | Assert the policy minted **exactly** `[(name, amt)]` and nothing else | `lenfi/lib/aada/utils.ak:52` |
| `validate_mint_nft_few_per_policy` | same | Same, but `list.has` (allows other names in the policy) | `lenfi/lib/aada/utils.ak:65` |
| `output_has_quantity_with_address` | `(Output, Address, PolicyId, AssetName, Int) -> Bool` | Address match + `quantity_of >= amount` | `lenfi/lib/aada/utils.ak:79` |
| `get_outputs_by_nft` | `(List<Output>, PolicyId, AssetName) -> List<Output>` | Filter outputs containing >0 of a token | `lenfi/lib/aada/utils.ak:93` |
| `get_input_by_nft` | `(List<Input>, PolicyId, AssetName) -> Input` | Find the input holding exactly 1 of the token (fails otherwise) | `lenfi/lib/aada/utils.ak:105` |
| `authorized_by_credential` | `(List<PubKeyHash>, Dict<StakeCredential,Int>, Credential) -> Bool` | Uniform "is this credential authorising?" — signature for pubkey, **withdraw-0** for script | `lenfi/lib/aada/utils.ak:123` |
| `safe_div` | `(Rational, Rational) -> Option<Rational>` | `mul(left, reciprocal(right))`; works around an Aiken codegen bug | `lenfi/lib/aada/utils.ak:137` |
| `retrieve_oracle_data` | `(List<Input>, AssetClass, Dict<ScriptPurpose,Redeemer>, Int) -> Option<OraclePriceFeed>` | Locate oracle UTxO by NFT, read the **oracle's own withdraw redeemer**, enforce `valid_to >= tx upper bound` | `lenfi/lib/aada/utils.ak:144` |
| `id_from_utxo` | `(OutputReference) -> ByteArray` | `blake2b_256(serialise_data(oref))` — canonical one-shot token name | `lenfi/lib/aada/nft.ak:38` |
| `check_uniqueness` | `(NFTAction, List<Input>, MintedValue, PolicyId) -> Bool` | One-shot mint: name derived from a consumed oref **and** that oref is in inputs | `lenfi/lib/aada/nft.ak:43` |
| `cred_validator` | `(Credential, Dict<StakeCredential,Int>, List<PubKeyHash>) -> Bool` | Duplicate of `authorized_by_credential` (they wrote it twice) | `lenfi/lib/aada/nft_pointers.ak:15` |
| `nft_validator` | `(PolicyId, AssetName, List<Input>, Dict<..>, List<..>) -> Bool` | "Governance NFT holder approves": find the NFT in ref inputs, then authorise by *its* payment credential | `lenfi/lib/aada/nft_pointers.ak:29` |
| `calculate_interest_amount` | `(Int, Int, Int, Int) -> Option<Rational>` | Simple interest `amount*rate*Δt / (year_ms * 1e6)` | `lenfi/lib/aada/finance.ak:6` |
| `get_expected_deposit_amount` | `(Int,Int,Int,Int) -> Option<Rational>` | LP share: `lp * (balance + lent_out) / total_lp` | `lenfi/lib/aada/finance.ak:17` |
| `token_b_received_from_seling_token_a` | `(Rational, Int, Int) -> Int` | Constant-product AMM swap out, 0.3% fee (`997/1000`) | `lenfi/lib/aada/finance.ak:28` |
| `token_b_needed_to_purchase_token_a` | `(Rational, Int, Int) -> Int` | Inverse AMM quote | `lenfi/lib/aada/finance.ak:42` |
| `calculate_health_factor` | `(Int, Int, Int) -> Option<Rational>` | `collateral * 1e12 / (threshold * debt)` | `lenfi/lib/aada/finance.ak:71` |
| `check_is_overcollaterized` / `check_is_undercollaterized` | `(Int, Int, Int) -> Bool` | HF compared against `1_000_000` | `lenfi/lib/aada/finance.ak:83` / `:104` |
| `get_interest_rates` | `(InterestParams, Int, Int, Int) -> Int` | Two-slope utilisation curve (Aave-style kink) | `lenfi/lib/aada/finance.ak:122` |
| `get_platform_fee` | `(CollateralDatum, PlatformFeeDetails) -> Int` | 3-tier fee by utilisation rate | `lenfi/validators/collateral.ak:49` |

## 1.2 Recurring validation idioms

### Own-input resolution from `Spend` purpose (written 6+ times)

```aiken
// lenfi/validators/pool.ak:33
expect Spend(output_ref) = purpose
expect Some(Input {
  output: Output { address: this_address, value: pool_input_value, .. }, ..
}) = list.find(inputs, fn(inp) -> Bool { inp.output_reference == output_ref })
expect ScriptCredential(this_script_hash) = this_address.payment_credential
let pool_nft_policy: PolicyId = this_script_hash    // policy == own script hash
```

Repeated near-verbatim at `collateral.ak:103/172`, `pool_config.ak:32`,
`delayed_merge.ak:14`, `order_contract.ak:38`.

### "State machine step": output-index hint + full transition check

```aiken
// lenfi/validators/pool.ak:69
let validate_transition =
  fn(pool_output: pool.OutputIndex, f_delta: fn(Int, Int, Int) -> Bool) -> Bool {
    expect Some(pool_output) = outputs |> list.at(pool_output)     // index hint
    expect InlineDatum(contract_datum) = pool_output.datum
    expect pool.Datum { params: output_params, balance: output_balance, .. } = contract_datum
    ...
    let dust_attack_check =
      without_lovelace(expected_value_with_input) == without_lovelace(pool_output.value)
    let valid_pool_output_address = pool_output.address == this_address
    ...
    and { valid_pool_token_tx?, value_change_check?, f_delta_check?,
          params_check?, output_is_positive?, dust_attack_check?,
          valid_pool_output_address? }
  }
```

Note the shape: `expected_value = input_value + delta`, then compare
`without_lovelace(expected) == without_lovelace(actual)`. That is the exact-value
check that Scalus should expose as one call.

### Auth NFT check via reference input + credential forwarding

```aiken
// lenfi/lib/aada/nft_pointers.ak:29
pub fn nft_validator(nft_policy, nft_name, reference_inputs, withdrawals, extra_signatories) -> Bool {
  when reference_inputs
    |> list.find(fn(inp) { quantity_of(inp.output.value, nft_policy, nft_name) == 1 })
  is {
    Some(Input { output: Output { address: Address { payment_credential, .. }, .. }, .. }) ->
      payment_credential |> cred_validator(withdrawals, extra_signatories)
    _ -> False
  }
}
```

The NFT is a *pointer to whoever currently owns it* — governance can be a pubkey today
and a DAO script tomorrow with no script change.

### One-shot mint tied to consuming a TxOutRef

```aiken
// lenfi/lib/aada/nft.ak:38
pub fn id_from_utxo(consumed_utxo: OutputReference) -> ByteArray {
  hash.blake2b_256(builtin.serialise_data(consumed_utxo))
}

// lenfi/lib/aada/nft.ak:49
MintNFT(consumed_utxo, _) -> {
  let nft_name = id_from_utxo(consumed_utxo)
  utils.validate_mint_nft(mint, own_policy, nft_name, 1) && list.any(
    inputs, fn(input) { input.output_reference == consumed_utxo })
}
BurnNFT(nft_name) -> utils.validate_mint_nft(mint, own_policy, nft_name, -1)
```

### Withdraw-0 stake-validator forwarding (batch of N inputs, one validation run)

```aiken
// lenfi/validators/delayed_merge.ak:13
fn spend(_datum: MergeDatum, _r, ctx: ScriptContext) {
  expect ScriptContext { transaction: Transaction { inputs, withdrawals, .. },
                         purpose: Spend(own_oref) } = ctx
  expect Some(own_input) = list.find(inputs, fn(input) { input.output_reference == own_oref })
  let withdraw_cred = Inline(own_input.output.address.payment_credential)
  // Can spend ALL THE merge inputs if ALL THE withdrawals are valid
  dict.has_key(withdrawals, withdraw_cred)
}
```

The paired `withdraw_validate` (`delayed_merge.ak:27`) then filters *all* inputs at its own
credential and folds their datums into totals, cross-checking against the pool redeemer:

```aiken
// lenfi/validators/delayed_merge.ak:69
let total_repay_amount =
  list.foldl(merges, 0, fn(input, total) {
    expect InlineDatum(merge_datum) = ...
    expect merge_datum: MergeDatum = merge_datum
    expect merge_datum.repay_amount > 0
    merge_datum.repay_amount + total
  })
...
let loan_amt_check = loan_amount == total_loan_amount
let repay_amt_check = repay_amount == total_repay_amount
```

### Spend-forwards-to-mint and mint-forwards-to-publish (cross-script redeemer coupling)

```aiken
// lenfi/validators/pool.ak:246 — the pool checks the *collateral minting policy's* redeemer
expect ScriptCredential(collateral_policy) = params.collateral_address.payment_credential
expect Some(collateral_mint_redeemer) = redeemers |> dict.get(Mint(collateral_policy))
expect collateral_mint_redeemer: CollateralNFTRedeemerLists = collateral_mint_redeemer
let valid_collateral_exists =
  collateral_mint_redeemer.mints |> list.any(fn(el) {
    let MintNFTElem(pool_utxo, _) = el
    pool_utxo == output_ref })
```

```aiken
// lenfi/validators/pool.ak:335 — the minting policy reads the *stake* script's Publish redeemer
expect Some(stake_validator_redeemer) =
  dict.get(redeemers, Publish(CredentialDelegation(out_stake_credential,
                                                   pool_mint_details.initial_pool_delegation)))
expect withdraw_validator_redeemer: pool_stake.StakeRedeemer = stake_validator_redeemer
expect pool_stake.CreatePool(_withdraw_validator_oref) = withdraw_validator_redeemer
```

### Self-referential identity: token name == script hash

```aiken
// lenfi/validators/pool.ak:333
expect ScriptCredential(stake_hash) = stake_cred
// Pool NFT name is stake_hash
let correct_quantity_output = quantity_of(value, own_script, stake_hash) == 1
```

And symmetrically in the stake validator (`pool_stake.ak:68`):
`quantity_of(pool_input.output.value, pool_nft_policy, own_hash) == 1`.

### Stake-validator "withdraw the rewards into the pool" fee enforcement

```aiken
// lenfi/validators/pool_stake.ak:77
expect Some(current_withdrawal_amount) = dict.get(withdrawals, withdraw_credentials)
let current_withdrawal_fee_adjusted = current_withdrawal_amount - 2_000_000
expect Some(raw_pool_redeemer): Option<Data> = redeemers |> dict.get(Spend(pool_oref))
expect Wrapped(pool_redeemer): WrappedRedeemer<pool.Redeemer> = raw_pool_redeemer
expect pool.PayFee(fee_amount, ..) = continuing_action
...
let amount_check = fee_amount >= current_withdrawal_fee_adjusted   // allow keeping 2 ADA
```

### Oracle freshness + n-of-m signature threshold

```aiken
// lenfi/lib/aada/utils.ak:161
when loan_tokens_price.data is {
  Pooled(data) ->
    if data.valid_to >= transaction_valid_to { Some(Pooled(data)) }
    else { trace @"oracle is expired"; None }
  Aggregated(data) -> ... same ...
}
```

```aiken
// lenfi/validators/oracle_validator.ak:43
let unique_signatures = list.unique(redeemer.signatures)
let valid_signatures =
  list.foldl(unique_signatures, 0, fn(redem, valid_signatures) -> Int {
    expect Some(verification_key) = list.at(verification_keys, redem.key_position)  // index hint
    if builtin.verify_ed25519_signature(
         verification_key, builtin.serialise_data(redeemer.data), redem.signature)
    { valid_signatures + 1 } else { valid_signatures }
  })
valid_signatures >= threshold
```

Three separate stdlib primitives here: `list.unique` dedup (replay guard), key **index
hints** in the redeemer, and signature verification over `serialise_data(payload)`.

### Payout to address with datum + protocol fee cut

```aiken
// lenfi/validators/collateral.ak:144
let platform_fee_paid =
  if platform_fee_amount_int > 0 {
    list.any(outputs, fn(p_output) {
      if p_output.address == platform_fee_details.platform_fee_collector_address {
        expect InlineDatum(output_datum) = p_output.datum
        expect constructed_datum: ByteArray = output_datum
        and {
          (quantity_of(p_output.value, datum.loan_cs.policy_id,
                       datum.loan_cs.asset_name) >= platform_fee_amount_int)?,
          (constructed_datum == borrower_tn)?,   // fee output tagged with borrower token
        }
      } else { False }
    })
  } else { True }
```

Note the fee output is **tagged with the borrower's unique token name** — this is an
anti-double-satisfaction tag on the fee payout itself.

### Liquidation math + leftovers output

```aiken
// lenfi/validators/collateral.ak:278
expect Some(liquidation_fee) =
  rational.new((collateral_value - value_to_repay) * platform_fee_details.liquidation_fee,
               1_000_000)
let liquidation_fee =
  if (liquidation_fee |> rational.truncate) < min_liquidation_fee {
    rational.from_int(min_liquidation_fee)
  } else { liquidation_fee }
let borrower_compensation_in_ada =
  rational.from_int(collateral_value - value_to_repay) |> rational.sub(liquidation_fee)
```

The leftovers output must carry a datum that *is* the burn ticket
(`collateral.ak:346`) — `LeftoversDatum = AssetClass{policy_id: collateral_script_hash,
asset_name: borrower_tn}` — and the leftovers validator is one line:

```aiken
// lenfi/validators/leftovers.ak:10
fn leftovers(datum: LeftoversDatum, _r: Void, ctx: ScriptContext) {
  let AssetClass { policy_id, asset_name } = datum
  utils.validate_mint_nft(ctx.transaction.mint, policy_id, asset_name, -1)
}
```

### Validity-range width bound (anti time-manipulation)

```aiken
// lenfi/validators/collateral.ak:105
expect Finite(lower_bound) = validity_range.lower_bound.bound_type
expect Finite(upper_bound) = validity_range.upper_bound.bound_type
let valid_validity_range = upper_bound - lower_bound <= 3600000   // 1 hour
```

Interest is charged to `upper_bound` and loan start recorded as `lower_bound`
(`collateral.ak:610`, `deposit_time: lower_bound`) — worst case for the borrower, bounded
by the window width.

## 1.3 Anti-double-satisfaction and tagging (Lenfi)

**(a) Order UTxO tags the payout with its own `OutputReference`.** This is the single
most reusable idiom found in the whole survey:

```aiken
// lenfi/validators/order_contract.ak:96
let real_output = {
  expect Spend(this_oref) = ctx.purpose
  let expected_datum =
    when order_datum is {
      NoDatum -> InlineDatum(this_oref)      // <-- unique tag: this order's own oref
      _ -> order_datum
    }
  Output { address, value: value.add(value, lp_asset.policy_id, lp_asset.asset_name,
                                     lp_assets_minted),
           datum: expected_datum, reference_script: None }
}
let user_received_value = ctx.transaction.outputs |> list.any(fn(out) { out == real_output })
```

Because two different orders produce two different `this_oref` datums, one output can
never satisfy two orders.

**(b) Mutual tagging: the pool redeemer must name this order.**

```aiken
// lenfi/validators/order_contract.ak:46
order.Process { pool_oref, additional_data } -> {
  expect Some(raw_pool_redeemer): Option<Data> = dict.get(redeemers, Spend(pool_oref))
  expect Wrapped(pool.Redeemer { action, order, .. }) = raw_pool_redeemer
  let pool_nft_found = quantity_of(pool_output_value, datum.pool_nft_cs.policy_id,
                                   datum.pool_nft_cs.asset_name) == 1
  let pool_tagged_this = order == Some(this_oref)     // <-- pool names exactly one order
  and { pool_nft_found?, pool_tagged_this?, lambda(additional_data, action)? }
}
```

`pool.Redeemer` carries `order: Option<OutputReference>` (`types/pool.ak:22`), so only
one order may be processed per pool interaction. That is a *global* single-satisfaction
constraint enforced from the counterparty side.

**(c) Explicit `tag: Option<OutputReference>` field in the collateral datum.**

```aiken
// lenfi/validators/collateral.ak:696
let tag_check =
  tag |> option.map(fn(oref) { list.any(inputs, fn(in) { in.output_reference == oref }) })
      |> option.or_else(True)
```

and the order contract requires `collateral_datum.tag == Some(this_oref)` plus
`|> list.length == 1` (`order_contract.ak:272-314`): *exactly one* collateral output may
carry this order's tag.

**(d) Batch validation with a monotonic input cursor (single pass, no double-counting).**
This is the crown jewel of the Lenfi mint policy:

```aiken
// lenfi/validators/collateral.ak:512
let num_minted_check = dict.size(own_mints) == list.length(mints) + list.length(burns)

// lenfi/validators/collateral.ak:519
let mints_are_valid =
  list.foldl(mints, Some(inputs),
    fn(collateral_mint: MintNFTElem, curr_inputs_opt: Option<List<Input>>) -> Option<List<Input>> {
      expect Some(curr_inputs) = curr_inputs_opt
      let MintNFTElem(pool_utxo, collateral_output_idx) = collateral_mint
      ...
      let pruned_inputs =
        curr_inputs |> list.drop_while(fn(in) { in.output_reference != pool_utxo })
      expect [ Input(_, Output { datum: InlineDatum(raw_pool_datum), .. }), .. ] = pruned_inputs
      ...
      if valid { pruned_inputs |> list.tail } else { None }   // <-- cursor advances past it
    })
```

Three separate guarantees in one fold:
- the accumulator `Option<List<Input>>` short-circuits to `None` on any failure;
- `drop_while` + `list.tail` means each redeemer element consumes a *distinct, strictly
  later* input — no input can back two mints;
- `num_minted_check` proves the redeemer list accounts for **every** token minted under
  the policy, so nothing is minted silently.

The burn side uses the same trick over the minted-token list (`collateral.ak:738`).

**(e) `Data`-level datum comparison instead of field-by-field.**

```aiken
// lenfi/validators/collateral.ak:599
let expected_collateral_datum: Data = CollateralDatum { pool_nft_name, loan_cs, loan_amount,
  pool_config, collateral_cs, collateral_amount: borrowed_collateral_amount, interest_rate,
  lent_out: pool_lent_out, balance: pool_balance, deposit_time: lower_bound, borrower_tn,
  oracle_collateral_asset, oracle_loan_asset, tag }
...
let datum_check = actual_datum_output == expected_collateral_datum   // one equalsData
```

## 1.4 Efficiency tricks (Lenfi)

- **Dust-attack guard via `flatten` arity match**, cheaper than iterating the value:

  ```aiken
  // lenfi/validators/collateral.ak:616
  let dust_attack_check =
    when flatten(collateral_value) is {
      [(_, _, _)] -> True
      [(_, _, _), (_, _, _)] -> True
      _ -> False
    }
  // "Collateral output can have 2 assets at most (ADA + maybe token)"
  ```
  Also `pool.ak:357` (≤3 assets), `liquidity_token.ak:37` (must be exactly 2 or 3).

- **`without_lovelace(a) == without_lovelace(b)`** as the whole non-ADA value equality
  (`pool.ak:89`) — one comparison instead of per-asset arithmetic.

- **Whole-`Output` structural equality** (`order_contract.ak:119, 189, 241, 270, 348`):
  construct the expected `Output` record and `list.any(outputs, _ == expected)`. Compiles
  to a single `equalsData` on a constructed value.

- **Index hints everywhere**: `pool.OutputIndex = Int` (`types/pool.ak:6`) is a field of
  every redeemer variant; `list.at(outputs, idx)`.

- **`expect [(name, qty)] = dict.to_list(tokens(mint, policy))`** — exact-shape pattern
  match on the minted map instead of lookups (`pool.ak:355`, `liquidity_token.ak:52`,
  `pool_config.ak:47`).

## 1.5 Idioms present (Lenfi)

`auth-nft-check`, `one-shot-mint`, `tag-with-txoutref`, `no-double-satisfaction`,
`single-script-input`, `output-to-self-with-datum`, `withdraw-zero-forward`,
`spend-forwards-to-mint`, `validity-range-check`, `input-index-hint`, `output-index-hint`,
`data-level-compare`, `min-ada` (implicit via `without_lovelace`), `protocol-fee-cut`,
`ratio-math`, `oracle-freshness`, `n-of-m-signature-threshold`, `dust-attack-arity-guard`,
`burn-to-unlock`, `batch-cursor-fold`, `exact-mint-accounting`.

---

# 2. Indigo Protocol (PlutusTx Haskell)

Indigo marks every on-chain helper with `{-# INLINEABLE #-}`, so
`src/Indigo/Utils/Helpers.hs` **is** their standard library. It is 495 lines of exactly
the functions this research is looking for.

## 2.1 The helper module, exhaustively

| Name | Signature | Purpose | file:line |
|---|---|---|---|
| `oneSecond` / `oneDay` / `oneYear` | `POSIXTime` | Time constants | `Utils/Helpers.hs:84,88,92` |
| `daysDifference` | `POSIXTime -> POSIXTime -> Integer` | Whole-day delta | `:96` |
| `lovelacesAmount` | `Value -> Integer` | ADA-only projection | `:100` |
| `unitValue` | `AssetClass -> Value` | `assetClassValue ac 1` | `:104` |
| `hasUnitValue` | `Value -> AssetClass -> Bool` | Exactly 1 token | `:108` |
| `isSpendingUnitValue` | `TxInfo -> AssetClass -> Bool` | `hasUnitValue . valueSpent` | `:112` |
| `hasPositiveValue` | `AssetClass -> Value -> Bool` | `>= 1` | `:116` |
| `parseDatum` | `FromData a => Datum -> Maybe a` | Typed decode | `:120` |
| `serializeDatum` | `ToData a => a -> Datum` | Typed encode | `:124` |
| `getInlineDatum` | `OutputDatum -> Maybe Datum` | Inline-only extraction | `:128` |
| `hasExpectedInlinedDatum` | `ToData a => TxOut -> a -> Bool` | **Data-level** datum compare | `:134` |
| `usesSpendRedeemer` | `ToData a => TxInfo -> TxOutRef -> a -> Bool` | Assert another input's redeemer equals a value — raw `BuiltinData` compare | `:140` |
| `spendRedeemer` | `FromData a => TxInfo -> TxOutRef -> a` | Read another input's redeemer, typed | `:148` |
| `findInlinedDatumFromOutput`(`'`) | `FromData a => TxOut -> a` / `Maybe a` | Inline datum + parse | `:156` / `:163` |
| `valueWithin` | `TxInInfo -> Value` | `txOutValue . txInInfoResolved` | `:168` |
| `findOwnInput'` | `ScriptContext -> TxInInfo` | Partial `findOwnInput` | `:172` |
| `isAuthOutput` | `AssetClass -> TxOut -> Bool` | Output carries the auth token | `:176` |
| `noContinuingOutputs` | `ScriptContext -> Bool` | Position is closed | `:180` |
| `hasUniqueInputWithToken` | `AssetClass -> TxInfo -> Bool` | **Exactly one** input with the token | `:184` |
| `findUniqueInputWithToken` | `AssetClass -> TxInfo -> TxOut` | idem, returns it | `:192` |
| `findUniqueInputWithPositiveAmtOfTokens` | `AssetClass -> TxInfo -> TxOut` | idem, `>= 1` | `:203` |
| `findUniqueInputWithTokenRef` | `AssetClass -> TxInfo -> (TxOutRef, TxOut)` | idem, keeps the oref (needed for tagging) | `:212` |
| `findUniqueInputWithPositiveAmtOfTokensRef` | same | | `:225` |
| `findUniqueReferenceInputWithToken` | `AssetClass -> TxInfo -> TxOut` | Reference-input variant | `:236` |
| `findUniqueOutputFromCurrentScript` | `AssetClass -> ScriptContext -> TxOut` | Exactly one continuing output with token | `:245` |
| `findUniqueOutputFromOtherScripts` | `AssetClass -> TxInfo -> TxOut` | | `:253` |
| `checkOwnOutput` | `ToData a => ScriptContext -> a -> Value -> Bool` | Continuing output with datum+value | `:265` |
| `getContinuingOutputsNoStaking` | `ScriptContext -> [TxOut]` | Like `getContinuingOutputs` but **rejects staking creds** | `:288` |
| `checkOwnOutputNoStaking` | | Same, no staking credential allowed | `:304` |
| `checkOwnOutputAdaGeq` | | Same, ADA-tolerant | `:326` |
| `checkOutput` | `Datum -> Value -> TxOut -> Bool` | Core: value residual must be **only ADA ≤ minAdaTxOut** | `:337` |
| `checkOutputAdaGeq` | same | Residual may be any non-negative ADA | `:355` |
| `checkOutputDatum` | `ToData a => a -> TxOut -> Bool` | | `:372` |
| `checkOutputFromOtherScripts` | `TxInfo -> ValidatorHash -> a -> Value -> Bool` | Payout to another script | `:383` |
| `checkOutputFromOtherScriptsWithStakingCredential` | | Staking-cred tolerant variant | `:404` |
| `checkOutputFromOtherScriptsAdaGeq` | | | `:426` |
| `validityTimeInInterval` | `TxInfo -> Interval POSIXTime -> Bool` | `interval `contains` txValidRange` | `:445` |
| `findAllInputsFromScript` | `ValidatorHash -> TxInfo -> [TxOut]` | | `:450` |
| `findAllOutputsToAddress` | `ValidatorHash -> TxInfo -> [TxOut]` | Rejects staking creds | `:460` |
| `valueOfAssetCls'` / `valueOfAssetCls` | `Value -> ... -> Value` | Project a Value onto one asset class | `:469` / `:474` |
| `getTokenName` | `AssetClass -> TokenName` | | `:479` |
| `optimizeUPLC` | `HasUPLC a => a -> a` | Plutonomy pass, `TraceRemove` in release | `:484` |
| `filterMap` | `(b -> Bool) -> (a -> b) -> [a] -> [b]` | **Single-pass** map+filter with a bang | `Utils/Utils.hs:8` |
| `getProtocolFeePercentage` | `AssetClass -> TxInfo -> OnChainDecimal` | Read fee % from gov reference input | `Contracts/Helpers.hs:20` |
| `payProtocolFeeCorrectly` | `ValidatorHash -> TxInfo -> Integer -> Bool` | Collector input value + fee must go back to collector | `Contracts/Helpers.hs:31` |

Fixed-point money type (`Data/Decimal.hs`):

| Name | Signature | Purpose | file:line |
|---|---|---|---|
| `decimalUnit` | `Integer = 1_000_000` | Scale | `Data/Decimal.hs:36` |
| `OnChainDecimal` | `newtype { getOnChainInt :: Integer }` | 6-dp fixed point | `:64` |
| `MultiplicativeSemigroup (*)` | | `x*y / 1e6` | `:96` |
| `DivideSemigroup divide` | `a -> a -> a` | `x*1e6 / y` | `:104` |
| `decimal2Integer` | `OnChainDecimal -> Integer` | Truncate | `:45` |

## 2.2 Recurring idioms

### "Must send value to address with datum" tolerating min-ADA — the reference implementation

```haskell
-- indigo/src/Indigo/Utils/Helpers.hs:337
checkOutput :: V2.Datum -> Value -> TxOut -> Bool
checkOutput d val TxOut {txOutValue, txOutDatum = V2.OutputDatum outDatum} =
  case Value.flattenValue (txOutValue - val) of
    [] -> True
    [(symbol, token, amount)] ->
      symbol == Ada.adaSymbol
        && token == Ada.adaToken
        && amount >= 0
        && amount <= Ada.getLovelace Ledger.minAdaTxOut
    _ -> trace "Value mismatch in checkOutput" False
    && (d == outDatum)
```

Subtract the expected value, flatten the residual, and allow *only* a bounded ADA
surplus. This is exactly the primitive that Cardano developers keep rewriting badly.

### Auth-token position lookup (thread token) and uniqueness

```haskell
-- indigo/src/Indigo/Utils/Helpers.hs:184
hasUniqueInputWithToken token info =
  case filterMap predicate V2.txInInfoResolved (V2.txInfoInputs info) of
    [_] -> True
    _   -> False
  where predicate = isAuthOutput token
```

### Anti-double-satisfaction via leader/follower redeemer tagging

Indigo's CDP merge lets one "leader" input validate and forces every other input at the
same script to be spent with a redeemer that names the leader:

```haskell
-- indigo/src/Indigo/Contracts/CDP/OnChain.hs:557
&& traceIfFalse
  "All other CDP inputs are spent with MergeAuxiliary redeemer"
  ( all
      ( \input ->
          Helpers.usesSpendRedeemer info (V2.txInInfoOutRef input) (MergeAuxiliary ownRef)
            && V2.txOutAddress (V2.txInInfoResolved input)
                 == Ledger.scriptHashAddress ownValHash )
      otherCDPInputs )
...
-- indigo/src/Indigo/Contracts/CDP/OnChain.hs:576
&& traceIfFalse
  "A single output has total value from all input CDPs (inc. auth tokens)"
  (V2.txOutValue cdpOutput == totalCDPValue)
&& traceIfFalse "Nothing minted/burnt" (V2.txInfoMint info == mempty)
```

`ownRef` comes from the `Spending` purpose (`OnChain.hs:597`). The follower redeemer
carries the leader's `TxOutRef`, so two leaders cannot claim the same followers.

### Oracle freshness

```haskell
-- indigo/src/Indigo/Contracts/CDP/OnChain.hs:58
getIAssetPrice (MkOracleAssetNFT oAssetNFT) info = (isExpired, odPrice)
  where
    oracleInput = Helpers.findUniqueReferenceInputWithToken oAssetNFT info
    MkOracleDatum {odPrice, odExpiration} = Helpers.findInlinedDatumFromOutput oracleInput
    isExpired = Ledger.ivTo (V2.txInfoValidRange info) >= Ledger.upperBound odExpiration
```

The oracle validator itself pins the feed timestamp into the tx validity window with a
bias, and requires the new expiry to be in the future but not too far:

```haskell
-- indigo/src/Indigo/Contracts/Oracle/OnChain.hs:50
&& traceIfFalse "Price must be positive" (price > zero)
&& traceIfFalse "Expiration time is not properly set"
     ( isExpirationProperlySet
         && Helpers.validityTimeInInterval info
              (Interval.interval (now - opBiasTime) (now + opBiasTime)) )
...
-- :77
isExpirationProperlySet =
  case Ledger.ivTo $ Spooky.txInfoValidRange info of
    Ledger.UpperBound (Ledger.Finite currentTimeApprox) _ ->
      currentTimeApprox + opExpirationTime >= odExpiration outputDatum
        && currentTimeApprox <= odExpiration outputDatum
    _ -> False
```

### Collateral ratio and protocol fee

```haskell
-- indigo/src/Indigo/Contracts/CDP/Common.hs:251
overCollaterized colAmt debtAmt debtPrice ratio =
  P.fromInteger colAmt * 100 * collateralPrice
    >= ratio * P.fromInteger debtAmt * debtPrice

-- indigo/src/Indigo/Contracts/CDP/Common.hs:262
protocolFee percentage collateralBefore collateralAfter =
  if collateralAfter >= collateralBefore then zero
  else getOnChainInt $
         OnChainDecimal (collateralBefore - collateralAfter) * percentage `divide` 100
```

```haskell
-- indigo/src/Indigo/Contracts/Helpers.hs:31
payProtocolFeeCorrectly collectorValHash info fee =
  fee == zero
    || Helpers.checkOutputFromOtherScripts info collectorValHash ()
         (feeInputValue <> Ada.lovelaceValueOf fee)
  where
    feeInput = case Helpers.findAllInputsFromScript collectorValHash info of
      [o] -> o
      _   -> traceError "Must spend 1 input from collector script"
```

Note the "spend the collector UTxO and hand it back with `+fee`" pattern — the collector
accumulates without needing its own accounting.

## 2.3 Efficiency: `Spooky` — deferred `BuiltinData` field decoding

This is Indigo's headline optimisation and the strongest "direct Data manipulation"
exhibit in the survey. Every single field of their vendored `ScriptContext` is wrapped in
`Spooky` (a `BuiltinData` newtype from `plutus-tx-spooky`, `indigo.cabal:110`), and the
accessors are `unSpooky . field`:

```haskell
-- indigo/src/Indigo/Utils/Spooky.hs:823
data TxInfo = TxInfo
  { txInfoInputs'          :: Spooky [TxInInfo],
    txInfoReferenceInputs' :: Spooky [TxInInfo],
    txInfoOutputs'         :: Spooky [TxOut],
    txInfoFee'             :: Spooky Value,
    txInfoMint'            :: Spooky Value,
    txInfoDCert'           :: Spooky [DCert],
    txInfoWdrl'            :: Spooky (Map StakingCredential Integer),
    txInfoValidRange'      :: Spooky POSIXTimeRange,
    txInfoSignatories'     :: Spooky [PubKeyHash],
    txInfoRedeemers'       :: Spooky (Map ScriptPurpose Redeemer),
    txInfoData'            :: Spooky (Map DatumHash Datum),
    txInfoId'              :: Spooky TxId }

-- indigo/src/Indigo/Utils/Spooky.hs:786
txOutValue :: TxOut -> Value
txOutValue = unSpooky . txOutValue'
```

Fields you never touch are never decoded. `Indigo/Utils/Spooky/Helpers.hs` then mirrors
the whole helper module against the Spooky types (`Spooky/Helpers.hs:6-29`).

Two more efficiency notes:

- `usesSpendRedeemer` compares **raw `BuiltinData`** rather than decoding
  (`Spooky/Helpers.hs:56`):
  ```haskell
  Just re -> Ledger.getRedeemer re == PlutusTx.toBuiltinData expectedRedeemer
  ```
- `filterMap` fuses filter and map in one pass with a strict binding
  (`Utils/Utils.hs:8`); every `find*` helper is written in terms of it.
- `optimizeUPLC` runs Plutonomy with `TraceRemove` in non-debug builds
  (`Utils/Helpers.hs:484`).

## 2.4 Idioms present (Indigo)

`auth-nft-check`, `single-script-input` (`hasUniqueInputWithToken`),
`output-to-self-with-datum` (`checkOwnOutput*`), `min-ada` (`checkOutput` residual rule),
`data-level-compare` (`hasExpectedInlinedDatum`, `usesSpendRedeemer`),
`no-double-satisfaction` (leader/follower `MergeAuxiliary ownRef`),
`redeemer-cross-check`, `validity-range-check`, `oracle-freshness`, `ratio-math`
(`OnChainDecimal`), `protocol-fee-cut`, `lazy-data-fields` (Spooky),
`no-staking-credential-variants`, `single-pass-filter-map`.

Notably **absent**: `withdraw-zero-forward`, `input-index-hint`, `merkle-proof` —
Indigo is V2-era and scans lists.

---

# 3. Butane (Aiken, CDP / stablecoin)

Butane is architecturally the most advanced of the Aiken repos: **all logic lives in
withdraw-0 staking validators**; the spend and mint scripts are two-line pointers.

## 3.1 Helpers the protocol defines itself

| Name | Signature | Purpose | file:line |
|---|---|---|---|
| `list_at` | `(List<a>, Int) -> a` | Direct `head_list`/`tail_list` index, no `Option` | `butane/lib/butane/unsafe.ak:3` |
| `unsome` | `(Option<a>) -> a` | Partial unwrap | `butane/lib/butane/unsafe.ak:11` |
| `lcm` | `(Int, Int) -> Int` | | `butane/lib/butane/utils.ak:18` |
| `contains_interval` | `(Interval<Int>, Interval<Int>) -> Bool` | `intersection(self, i) == i` | `utils.ak:23` |
| `finite_interval_range` | `(Interval<Int>) -> Int` | Width, fails if unbounded | `utils.ak:28` |
| `check_price_feed_validity` | `(Interval, Interval) -> Bool` | Feed window < 1 day **and** contains tx window | `utils.ak:36` |
| `until_input_from` | `(Credential, List<Input>) -> List<Input>` | Advance input cursor to the next input at a credential | `utils.ak:46` |
| `gov_has_lock_token` | `(Input, ScriptHash) -> Bool` | | `utils.ak:54` |
| `mints_nothing_here` | `(Value, PolicyId) -> Bool` | Policy minted nothing | `utils.ak:61` |
| `only_mints_this` | `(Value, PolicyId, AssetName, Int) -> Bool` | Minted map for policy is exactly `[Pair(n,a)]` | `utils.ak:66` |
| `until_zero` | `(Int, a, fn(a)->a) -> a` | Bounded repeat combinator | `utils.ak:70` |
| `compare_asset_classes` | `(AssetClass, AssetClass) -> Ordering` | Lexicographic policy-then-name | `utils.ak:78` |
| `list_insert_at` | `(List<a>, Int, a) -> List<a>` | | `utils.ak:88` |
| `sorted_list_is_unique` | `(List<a>) -> Bool` | Adjacent-pair uniqueness on a sorted list | `utils.ak:126` |
| `calculate_fee_percent` | `(List<(PosixTime,Int)>, PosixTime, PosixTime) -> Int` | Integrate a **step function of historical rates** back to CDP open time | `utils.ak:151` |
| `calculate_earnings_percent` | same shape | Staking-side twin | `utils.ak:184` |
| `fake_to_real_out` | `(FakeOutput) -> Output` | Rebuild a `Value` from a blueprint-friendly nested list | `utils.ak:220` |
| `get_state_delta` | 14 args + continuation | Compute mint/fee deltas for one CDP action | `utils.ak:234` |
| `get_treasury_share` | `(Value, Int, Rational) -> Value` | Skim excess collateral above CR into treasury | `utils.ak:286` |
| `is_minimal_denom` | `(List<Int>, Int) -> Bool` | gcd fold — weights are in lowest terms | `utils.ak:312` |
| `is_reserved_asset` | `(AssetName) -> Bool` | Reject names colliding with reserved prefixes | `utils.ak:316` |
| `authorization_check` | 8 args | Unified owner check: pubkey / spend-token / withdraw-from / **delegated signature** | `utils.ak:326` |
| `not_withdrawing_from` | `(Pairs<StakeCredential,Int>, StakeCredential) -> Bool` | | `utils.ak:393` |
| `withdraws_zero` | `(Pairs<StakeCredential,Int>, StakeCredential) -> Bool` | `get_first(..) == Some(0)` | `utils.ak:400` |
| `to_monodatum` / `to_pricefeedredeemer` | `(Data) -> T` | Single-site typed coercions | `utils.ak:407` / `:412` |
| `params_from_refs` | `(List<Input>, ByteArray) -> List<ParamsData>` | Scan reference inputs for prefix-tagged params tokens | `utils.ak:417` |
| `stake_cred_from_hash` | `(ByteArray) -> StakeCredential` | | `utils.ak:445` |
| `find_input_with_credential` | `(List<Input>, Credential) -> Option<Input>` | | `utils.ak:449` |
| `find_input` | `(Data, List<Input>) -> Input` | Compare orefs as **`Data`**, not typed | `butane/validators/upgradeable.ak:12` |
| `get_collateral_finances` | `(prices, denom, Value, assets, weights, wdenom, maxprops, synth, callback) -> a` | CR + health factor in one pass | `butane/lib/butane/prices.ak:10` |
| `do_get_borrowing_capacity(_2)` | CPS folds | Walk `Value` × sorted asset list in lockstep | `prices.ak:43` / `:77` |
| `get_asset_price_then` | CPS | Match one asset against the sorted parameter lists | `prices.ak:151` |

Constants worth stealing (`butane/lib/butane/types.ak:12-46`): `max_min_ada = 2_000_000`,
`bp_precision = 10_000`, `milliseconds_in_year/week/day`, plus the reserved token-name
prefixes `params_prefix = "p_"`, `debt_prefix = "d_"` and lock-token names.

## 3.2 Recurring idioms

### Everything is a withdraw-0 staking validator

```aiken
// butane/validators/pointers.ak:6
validator(upgradeable_script_hash: Referenced<Credential>) {
  fn spend(_datum: Data<types.MonoDatum>, _redeemer: Data, ctx: ScriptContext) -> Bool {
    has_key(ctx.transaction.withdrawals, upgradeable_script_hash)
  }
}
validator(upgradeable_script_hash: Referenced<Credential>, _salt: Int) {
  fn mint(_redeemer: Data, ctx: ScriptContext) -> Bool {
    has_key(ctx.transaction.withdrawals, upgradeable_script_hash)
  }
}
```

`_salt: Int` exists purely to grind a script hash that sorts early in the withdrawals
list (`butane/validators/upgradeable.ak:24`: *"Arbitrary number for mining to ensure that
the upgradeable validator comes before the price feeds validator"*).

The real validator dispatches by redeemer and, crucially, **guards the other purposes**:

```aiken
// butane/validators/synthetics.ak:59
let no_certs = fn() {
  let mint_stake_cred: Data = Inline(ScriptCredential(mint_script_hash))
  let certificate <- list.all(certificates)
  // Can't do any certificate action that uses the mint script (delegator is always the first field)
  ( builtin.un_constr_data(certificate).2nd |> builtin.head_list() ) != mint_stake_cred
}
let zero_withdrawals = fn() {
  utils.not_withdrawing_from(withdrawals, Inline(ScriptCredential(mint_script_hash)))
}
```

That `un_constr_data(...).2nd |> head_list()` reads field 0 of *any* certificate
constructor without decoding the certificate type — a direct-`Data` trick.

### Ordered, cursor-based state machine over inputs (the anti-double-satisfaction core)

```aiken
// butane/lib/butane/subvalidators/cdp_script.ak:43
when spends is {
  [] -> {
    // Not spending anything else from the state script without validation
    expect {
      let i <- list.all(inputs)
      i.output.address.payment_credential != ScriptCredential(state_script_hash)
    }?
    callback(outputs, x_mint, x_btn_delta, x_fee, x_lock_mints)
  }
  [types.SpendAction { spend_type: this_action, params_idx, fee_type }, ..remaining_actions] -> {
    ...
    expect [ Input { output: Output { datum: input_datum, value: cdp_value, .. },
                     output_reference: this_oref }, ..remaining_inputs ]
      = utils.until_input_from(ScriptCredential(state_script_hash), inputs)
```

Each redeemer action consumes the next state-script input in order; the base case proves
no state input was left unvalidated. Outputs are consumed with the same discipline —
`builtin.head_list(outputs)` for the produced output and
`pass_in(remaining_inputs, builtin.tail_list(outputs), ...)` to advance
(`cdp_script.ak:252`, `:363`, `:454`, `:489`).

### Global mint accounting: expected mint map must equal actual, exactly

```aiken
// butane/lib/butane/subvalidators/cdp_script.ak:836
let state_mint_after_insertion =
  if state_lock_mints == 0 { state_mint }
  else { state_mint |> dict.insert(types.cdp_lock_token_name, state_lock_mints) }
and {
  // Mint here is correct
  (state_mint_after_insertion == ( minted_value |> value.tokens(mint_script_hash) ))?,
  // Mint btn is correct
  (( minted_value |> value.quantity_of(fee_token_pid, fee_token_name) ) == state_btn_delta)?,
  // Fees are correct
  valid_fee_output?,
}
```

A `StateDelta { mint, btn_delta, fee, lock_mints }` (`types.ak:109`) accumulates across
every action in the transaction; a single equality at the end makes double-satisfaction
structurally impossible — you cannot mint one synthetic and count it twice.

### Fee output must be an exact address+datum+value

```aiken
// butane/lib/butane/subvalidators/cdp_script.ak:804
let valid_fee_output = or {
  state_fee == value.zero(),
  {
    let expected_fee_address = Address {
      payment_credential: ScriptCredential(state_script_hash),
      stake_credential: Some(Inline(ScriptCredential(mint_script_hash))) }
    and {
      { let pol, tok, qty, acc <- value.reduce(actual_fee_utxo_value, True)
        (acc && value.quantity_of(state_fee, pol, tok) <= qty)? },
      (actual_fee_utxo_address == expected_fee_address)?,
      (actual_fee_utxo_datum == InlineDatum(
         types.TreasuryDatum { treas: types.TreasuryFromFees }))?,
    }
  } }
```

### "Cannot add tokens to the output" — value subset check via `value.reduce`

```aiken
// butane/lib/butane/subvalidators/cdp_script.ak:326
{
  let pol, tok, qty, acc <- value.reduce(
    leftover_value |> value.add(mint_script_hash, types.cdp_lock_token_name, -1), True)
  acc && value.quantity_of(cdp_value_no_lock, pol, tok) >= qty
}
```

Repeated at `:434`, `:511`. This is "output ⊆ input" as a single fold — a genuinely
reusable primitive that no stdlib currently offers.

### Unified authorisation, including delegated (meta-transaction) signatures

```aiken
// butane/lib/butane/utils.ak:336
when verifier is {
  types.AuthorizingDirectly(cv2) ->
    when cv2 is {
      types.AuthorizedWithExtraSigs -> {
        expect types.AuthorizeWithPubKey(hash, _) = fcredential
        extra_signatories |> list.has(hash) }
      types.AuthorizedWithInputsOref(oref) -> {
        expect types.AuthorizeWithConstraint(types.MustSpendToken(asset)) = fcredential
        let inp = unsafe.unsome(inputs |> list.find(fn(inp) { inp.output_reference == oref }))
        quantity_of(inp.output.value, asset.policy_id, asset.asset_name) >= 1 }
      types.AuthorizedWithWithdrawal -> {
        expect types.AuthorizeWithConstraint(types.MustWithdrawFrom(stake_cred)) = fcredential
        withdrawals |> pairs.has_key(stake_cred) }
    }
  types.AuthorizingOtherWithSignature { other, sub_verifier, signature } -> {
    expect types.AuthorizeWithPubKey(_, key) = fcredential
    let types.ConstraintCredential { utxo, interval, constraint } = other
    ...
    and {
      (utxo == this_oref)?,                    // signature is bound to THIS utxo
      (interval_lb <= valid_from)?,
      (interval_ub >= valid_to)?,
      builtin.verify_ed25519_signature(key, builtin.serialise_data(other), signature)?,
      authorization_check(... recursive ...)?,
    } }
}
```

The `ConstraintCredential { utxo, interval, constraint }` (`types.ak:87`) is a signed
permit scoped to one UTxO and one time window — a replay-proof off-chain delegation.

### Price feeds: signature over serialised data, validity window containment

```aiken
// butane/validators/price_feed.ak:17
list.all(redeemer, fn(feed) {
  let types.Feed { data, extra: signature } = feed
  builtin.verify_ed25519_signature(
    verification_key, builtin.serialise_data(data), signature)?
})
```

```aiken
// butane/lib/butane/utils.ak:36
pub fn check_price_feed_validity(pf_validity, tx_validity) {
  and {
    (finite_interval_range(pf_validity) < types.milliseconds_in_day)?,
    contains_interval(pf_validity, tx_validity)?,
  }
}
```

Feeds are then zipped positionally against the params reference inputs
(`cdp_script.ak:746`):

```aiken
let price_feeds_list: List<types.PriceFeed> = {
  let types.Feed { data: p_data, .. }, types.ParamsData { synthetic: params_synth, .. }
    <- list.map2(price_feed_redeemer, param_list)
  expect and {
    utils.check_price_feed_validity(p_data.validity, validity_range)?,
    (p_data.synthetic == params_synth)?,
  }
  p_data }
```

### Interest as an integral over a historical step function

```aiken
// butane/lib/butane/utils.ak:159
pub fn do_calculate_fee_percent(acc_percent, prev_time, interest_rates, cdp_start_time) {
  let (time_neg, rate) = builtin.head_list(interest_rates)
  let time = -time_neg                                // negated so the list sorts descending
  if time < cdp_start_time {
    acc_percent + rate * (prev_time - cdp_start_time)
  } else {
    do_calculate_fee_percent(acc_percent + rate * (prev_time - time), time,
                             builtin.tail_list(interest_rates), cdp_start_time)
  }
}
```

`ActiveParams.interest_rates` holds the 71 most recent `(timestamp, rate)` pairs with a
global-max sentinel at timestamp 0 (`types.ak:263`, `:46`). Times are stored **negated**
so the ledger's natural ordering gives newest-first.

### Collateral ratio & health factor in one sorted pass

```aiken
// butane/lib/butane/prices.ak:178
expect [asset_name, ..] = asset_list
if s == asset_name {
  expect [price, ..] = price_list
  let numerator = qty * price
  continue_with(
    debt,
    unweighted_capacity + numerator,
    borrowing_capacity + math.min(
      // First case: the asset is not limited by the max proportion
      numerator * weights_denom / builtin.head_list(weights_list) / prices_denom,
      // Second case: the asset is limited by the max proportion
      debt * builtin.head_list(max_proportion_list) / types.bp_precision),
    builtin.tail_list(asset_list), builtin.tail_list(price_list), ...
  )
} else { get_asset_price_then(s, ..., builtin.tail_list(asset_list), ...) }
```

The `Value` is already sorted, and `collateral_assets`/`weights`/`max_proportions` are
required to be sorted the same way, so the whole valuation is one merge-join with no
lookups. Final assembly (`prices.ak:37`):

```aiken
let cr = unsafe.unsome(rational.new(unweighted_capacity, p_dom * synth_amount))
let hf = unsafe.unsome(rational.new(capacity, synth_amount))
callback(cr, hf)
```

### min-ADA bound and exact non-ADA value

```aiken
// butane/lib/butane/subvalidators/treasury.ak:520
(value.without_lovelace(change_debt_value) == value.from_asset(
   mint_script_hash, types.gov_lock_token_name, 1))?,
(value.lovelace_of(change_debt_value) <= types.max_min_ada)?,
```

### Reserved token-name prefixes (one policy, many namespaced token families)

```aiken
// butane/lib/butane/utils.ak:316
pub fn is_reserved_asset(asset: AssetName) -> Bool {
  or {
    (asset == types.cdp_lock_token_name)?,
    (asset == types.gov_lock_token_name)?,
    (asset == types.staking_lock_token_name)?,
    (bytearray.take(asset, types.params_prefix_length) == types.params_prefix)?,
    (bytearray.take(asset, types.debt_prefix_length) == types.debt_prefix)?,
  }
}

// butane/lib/butane/utils.ak:424 — discovering params UTxOs by prefix
when value.tokens(ref_input_value, own_hash) |> dict.to_pairs is {
  [Pair(k, _)] ->
    if bytearray.take(k, types.params_prefix_length) == types.params_prefix {
      let params_synthetic_name = bytearray.drop(k, types.params_prefix_length)
      ...
```

### One-shot init + 28-byte token-name discipline

```aiken
// butane/validators/upgradeable.ak:49
types.InitMint -> {
  expect list.any(inputs, fn(inp) { inp.output_reference == init_utxo })
  expect [Pair(name, minted_amount)] =
    mint |> value.from_minted_value |> value.tokens(own_policy) |> dict.to_pairs
  and { minted_amount == 1, builtin.length_of_bytearray(name) == 28 }
}
types.Upgrade -> {
  expect [Pair(name1, minted_amount1), Pair(name2, minted_amount2)] = ...
  or { and { minted_amount1 == 1, minted_amount2 == -1,
             builtin.length_of_bytearray(name1) == 28 },
       and { minted_amount1 == -1, minted_amount2 == 1,
             builtin.length_of_bytearray(name2) == 28 } }
}
```

The token name **is** a script hash, so the pointer script resolves the current logic
script by reading the token name off a reference input (`upgradeable.ak:34`):

```aiken
let input = find_input(redeemer, reference_inputs)
expect [Pair(token_name, _)] = input.output.value |> value.tokens(own_hash) |> dict.to_pairs
let withdraw_cred = Inline(ScriptCredential(token_name))
pairs.has_key(withdrawals, withdraw_cred)
```

## 3.3 Idioms present (Butane)

`withdraw-zero-forward` (universal), `spend-forwards-to-withdraw`,
`mint-forwards-to-withdraw`, `upgradeable-pointer-script`, `one-shot-mint`,
`no-double-satisfaction` (ordered cursor + global mint equality),
`exact-mint-accounting`, `output-subset-of-input`, `input-index-hint`,
`output-index-hint`, `positional-output-consumption`, `oracle-freshness`,
`signed-price-feed`, `validity-range-check`, `interval-containment`, `min-ada`,
`protocol-fee-cut`, `ratio-math`, `basis-point-math`, `step-function-interest`,
`sorted-merge-join`, `asset-name-prefix-namespace`, `data-level-compare`,
`direct-builtin-list-access`, `delegated-signature-permit`, `cert-guard`.

---

# 4. liqwid-plutarch-extra — a shipped utility library

This repo is the closest existing thing to the library Scalus wants. The **module list is
the answer**; every module below is a candidate stdlib area.

## 4.1 Full module inventory

Cardano-domain modules (the ones that matter most):

| Module | Contents (exported names) |
|---|---|
| `Plutarch.Extra.ScriptContext` | `paddressFromScriptHash`, `paddressFromPubKeyHash`, `pownTxOutRef`, `pownTxInfo`, `ptryOwnValue`, `pownMintValue`, `ptryOwnInput`, `pisTokenSpent`, `pisUTXOSpent`, `pvalueSpent`, `ptxSignedBy`, `pfindTxInByTxOutRef`, `pscriptHashFromAddress`, `pisScriptAddress`, `pisPubKey`, `pfindOutputsToAddress`, `pfindOwnInput`, `pfromPDatum`, `presolveOutputDatum`, `ptryResolveOutputDatum`, `pfromOutputDatum`, `ptryFromOutputDatum`, `ptryFromDatumHash`, `ptryFromInlineDatum`, `scriptHashToTokenName`, `pscriptHashToTokenName`, `ptryFromRedeemer` |
| `Plutarch.Extra.Value` | `passetClassDataValue(T)`, `psingleValue(')`, `psingleValueT'`, `pvalue`, `pvaluePositive`, `padaOf`, `passetClassValueOf(')(T)`, `pmatchValueAssets`, `psplitValue`, `psymbolValueOf(')`, `precValue`, `pelimValue`, `pbyClassComparator(')`, `pbySymbolComparator`, `phasOnlyOneTokenOfCurrencySymbol`, `phasOneTokenOfCurrencySymbol`, `phasOneTokenOfAssetClass` |
| `Plutarch.Extra.AssetClass` | `AssetClass`/`PAssetClass`/`PAssetClassData`, `passetClass(T)`, `adaClass`/`padaClass`/`isAdaClass`, `emptyTokenNameData`, `psymbolAssetClass(T)`, `pconstantClass(T)`, `ptoScottEncoding`, `pfromScottEncoding`, `pviaScottEncoding` |
| `Plutarch.Extra.ExtendedAssetClass` | `ExtendedAssetClass`/`PExtendedAssetClass`, `pextendedAssetClassValueOf(')`, `peqClasses`, `punsafeToAssetClass(Data)` |
| `Plutarch.Extra.StateThread` | `withStateThread`, `pwithStateThread` |
| `Plutarch.Extra.MultiSig` | `MultiSig`/`PMultiSig`, `mkMultiSig`, `validatedByMultisig`, `pvalidatedByMultisig`, `PMultiSigContext`, `pmultiSigContext` |
| `Plutarch.Extra.Time` | `PFullyBoundedTimeRange`, `pgetFullyBoundedTimeRange`, `fullyBoundedTimeRangeFromValidRange`, `passertFullyBoundedTimeRange`, `pisWithinTimeRange`, `pisTimeRangeWithin`, `ptimeRangeDuration` |
| `Plutarch.Extra.Script` | `applyArguments` |
| `Plutarch.Extra.IsData` | `ProductIsData`, `EnumIsData`, `PlutusTypeEnumData`, `PlutusTypeDataList`, `pmatchEnum`, `pmatchEnumFromData`, `DerivePConstantViaDataList/Enum` |

Data-structure and math modules:

| Module | Contents |
|---|---|
| `Plutarch.Extra.List` | `preplicate`, `pfromList`, `pmapMaybe`, `pdeleteFirstBy`, `ptryDeleteFirstBy`, `pdeleteFirst`, `pfindJust`, `plookupAssoc`, `phandleList`, `precListLookahead`, `ptryElimSingle`, `plistEqualsBy`, `pisSingleton`, `pfromSingleton`, `ptryFromSingleton` |
| `Plutarch.Extra.Map` | `ptryLookup`, `plookupGe`, `pkeysEqual`, `pkeysEqualUnsorted`, `pupdate`, `padjust`, `pfoldMapWithKey`, `pfoldlWithKey`, `phandleMin`, `punsortedMapFromFoldable`, `psortedMapFromFoldable`, `pkeys`, `pkvPairKey`, `pkvPairValue`, `pkvPairLt` |
| `Plutarch.Extra.Ord` | `POrdering`, `PComparator`, `pmax`/`pmin`, `pfromOrd(By)`, `pmapComparator`, `preverseComparator`, `pcompareBy`/`pequateBy`/`pleqBy`/`plessThanBy`/`pgeqBy`/`pgreaterThanBy`/`pmaxBy`/`pminBy`, `pleqMapBy`/`plessThanMapBy`/`pgeqMapBy`/`pgreaterThanMapBy`, `pleqValueBy`/`plessThanValueBy`/`pgeqValueBy`/`pgreaterThanValueBy`, `pisSortedBy`, `pallUnique(By)`, `ptryAllUnique(By)`, `ptryMerge(By)`, `psort(By)`, `pnubSort(By)`, `pinsertUniqueBy` |
| `Plutarch.Extra.Rational` | `PRationalNoReduce`, `pnoReduce`, `preduce'`, `mulTruncate`, `mulDivTruncate`, `divTruncate`, `mulRational`, `divRational`, `pliftTaggedRational`, `(#%)` |
| `Plutarch.Extra.FixedDecimal` | `FixedDecimal`/`PFixedDecimal` (type-level exponent), `fixedNumerator`/`fixedDenominator`, `emul`/`ediv`, `toFixedZero`/`fromFixedZero`, `convertExp`, `ptoRational`, `punsafeMkFixedDecimal` |
| `Plutarch.Extra.Fixed` | `PFixed`, `DivideSemigroup`, `DivideMonoid`, `fixedToAdaValue`, `fromPInteger`, `toPInteger` |
| `Plutarch.Extra.ExchangeRate` | `type (:>)` (phantom currency tag), `exchangeFrom(Truncate)`, `exchangeTo(Truncate)` |
| `Plutarch.Extra.Numeric` | `peven`, `(#^)` (exponentiation) |
| `Plutarch.Extra.Tagged` | `PTagged`, `pretag` (phantom-typed quantities) |
| `Plutarch.Extra.Bool` | `pcompare`, `pcond`, `passert` |
| `Plutarch.Extra.Field` | `pletAll`, `pletAllC` (bind every record field once) |

Functional-programming scaffolding (less relevant to Scalus, listed for completeness):
`Applicative`, `Bind`, `Boring`, `Category`, `Comonad`, `Const`, `Deriving`, `Function`,
`Functor`, `Identity`, `Monoid`, `Optics`, `Profunctor`, `Record`, `Star`, `State`, `Sum`,
`TermCont`, `These`, `Traversable`, `Tuple`, `PPrelude`, `Orphans`,
`Numeric.Additive`. Tooling: `Compile`, `Precompile`, `DebuggableScript`.

## 4.2 Key implementations worth copying

### `withStateThread` — the canonical one-shot mint combinator

```haskell
-- liqwid-plutarch-extra/src/Plutarch/Extra/StateThread.hs:26
withStateThread mp ref = plam $ \red ctx -> pletAll ctx $ \ctx' ->
  pletFields @'["inputs", "mint"] (getField @"txInfo" ctx') $ \txInfo ->
    pmatch (getField @"purpose" ctx') $ \case
      PMinting thisPolicy ->
        pif (uniqueStateTokenMint (pfield @"_0" # thisPolicy) . getField @"mint" $ txInfo)
          ( pif (pany # (hasUniqueInput # ref) # getField @"inputs" txInfo)
              (mp # red # ctx)
              (ptraceError "stateThread: Unique input not found") )
          (ptraceError "stateThread: Not minting a unique state token")
      _ -> ptraceError "stateThread: Not a minting script purpose"

-- :59
uniqueStateTokenMint thisPolicy mint =
  let singleEmptyToken = ppairDataBuiltin # pdata (pconstant "") # pdata 1
   in ptryFromSingleton # pto (pfromJust #$ plookup # thisPolicy # pto mint)
        #== singleEmptyToken
```

A *decorator* over an existing minting policy: mints exactly one token named `""` under
this policy and consumes `ref`. Scalus should ship exactly this signature.

### Token name == script hash, as a first-class safe coercion

```haskell
-- liqwid-plutarch-extra/src/Plutarch/Extra/ScriptContext.hs:443
scriptHashToTokenName :: ScriptHash -> TokenName
scriptHashToTokenName = coerce

-- :446 (haddock)
-- | Safely convert a 'PScriptHash' into a 'PTokenName'. This can be useful for tagging
--   tokens for extra safety.
pscriptHashToTokenName = punsafeCoerce
```

### Value predicates that distinguish "only" from "at least"

```haskell
-- liqwid-plutarch-extra/src/Plutarch/Extra/Value.hs:575
phasOnlyOneTokenOfCurrencySymbol = phoistAcyclic $
  plam $ \cs vs ->
    psymbolValueOf # cs # vs #== 1
      #&& (plength #$ pto $ pto $ pto vs) #== 1

-- :613 (contrast)
phasOneTokenOfCurrencySymbol = phoistAcyclic $ plam $ \cs vs -> psymbolValueOf # cs # vs #== 1
```

The haddock explicitly warns about the difference (`Value.hs:590`, `:608`) — this is the
inexact-value-check bug class, encoded as two differently-named functions.

### Value eliminators that avoid `flatten`

```haskell
-- liqwid-plutarch-extra/src/Plutarch/Extra/Value.hs:427
pelimValue whenCons whenNil xs = phandleMin (pto xs) whenNil $ \k v kvs ->
  phandleMin v whenNil $ \vk vv rest ->
    whenCons k vk vv . pcon . PValue $
      pif (AssocMap.pnull # rest) kvs (AssocMap.pinsert # k # rest # kvs)
```

`precValue` (`Value.hs:450`) is the raw-builtin-list version — it walks the
`BuiltinList (BuiltinPair (AsData CurrencySymbol) (AsData (Map TokenName Integer)))`
directly, never materialising a flattened list.

`psplitValue` (`Value.hs:365`) exploits the ledger's sort order to peel the ADA entry off
the front in O(1):

> "In cases where we know that a `PValue` contains Ada, such as in a `PScriptContext`,
> then this will function will split the Ada value - since the Ada entry comes first."

### Rational without gcd reduction

```haskell
-- liqwid-plutarch-extra/src/Plutarch/Extra/Rational.hs:65
pnoReduce :: Term s PRational -> Term s PRationalNoReduce
preduce'  :: Term s PRationalNoReduce -> Term s PRational

-- :167
mulDivTruncate = phoistAcyclic $ plam $ \x num denom -> pdiv # (num * x) # denom
-- :153
mulTruncate = ... mulDivTruncate # x # num # pto denom
-- :179
divTruncate = ... mulDivTruncate # x # pto denom # num
```

`PRationalNoReduce` defers the (expensive) `gcd` normalisation; `mulRational`/
`divRational` scale without reducing, with an explicit haddock warning about numerator
blow-up (`Rational.hs:188-196`). `mulTruncate`/`divTruncate` are the "apply a rate to an
integer amount" primitives every protocol needs — currently hand-rolled everywhere else.

### N-of-M multisig as data

```haskell
-- liqwid-plutarch-extra/src/Plutarch/Extra/MultiSig.hs:41
data MultiSig = MultiSig [PubKeyHash] Integer
-- :83  (smart constructor refuses impossible thresholds)
mkMultiSig pkhs minSigs
  | length pkhs < fromIntegral minSigs = Nothing
  | otherwise = Just . MultiSig pkhs $ minSigs
-- :188
pvalidatedByMultisig = ... plam $ \multi ctx -> ...
  getField @"minSigs" multiF
    #<= (plength #$ pfilter # (pflip # pelem # sigs) # getField @"keys" multiF)
```

### Fully-bounded time range as a type

```haskell
-- liqwid-plutarch-extra/src/Plutarch/Extra/Time.hs:64
pgetFullyBoundedTimeRange :: Term s (PPOSIXTimeRange :--> PMaybe PFullyBoundedTimeRange)
-- :117
passertFullyBoundedTimeRange :: Term s (PString :--> PPOSIXTimeRange :--> PFullyBoundedTimeRange)
-- :133
pisWithinTimeRange = plam $ \time ctr ->
  pmatch ctr $ \(PFullyBoundedTimeRange lb ub) -> lb #<= time #&& time #<= ub
-- :153
pisTimeRangeWithin = plam $ \lb' ub' ctr ->
  pmatch ctr $ \(PFullyBoundedTimeRange lb ub) -> lb' #<= lb #&& ub #<= ub'
-- :171
ptimeRangeDuration :: Term s (PFullyBoundedTimeRange :--> PPOSIXTime)
```

Every deadline check in every other repo is a hand-rolled instance of one of these four.
Turning `Interval POSIXTime` into a *proved-finite* type once, at the top of the
validator, is the pattern.

### Ord module: sorting/uniqueness/merging as reusable comparators

`ptryMergeBy`, `pisSortedBy`, `pallUniqueBy`, `pnubSortBy`, `pinsertUniqueBy`
(`Plutarch/Extra/Ord.hs` export list) plus `PComparator` combinators
(`pfromOrdBy`, `pmapComparator`, `preverseComparator`) and Value/Map-specific orderings
(`pleqValueBy`, `pleqMapBy`). This is precisely the toolkit that Butane's
`sorted_list_is_unique` and Lenfi's `list.unique` re-invent by hand.

## 4.3 Idioms present (liqwid-plutarch-extra)

`one-shot-mint` (`withStateThread`), `auth-nft-check` (`pisTokenSpent`,
`phasOneTokenOfAssetClass`), `single-script-input` (`ptryOwnInput`, `ptryFromSingleton`),
`output-to-self-with-datum` (`pfindOutputsToAddress` + `ptryFromInlineDatum`),
`data-level-compare` (`ptryFromRedeemer`), `validity-range-check` (`Time` module),
`ratio-math` (`Rational`, `FixedDecimal`, `Fixed`, `ExchangeRate`),
`n-of-m-signature-threshold` (`MultiSig`), `tag-token-with-script-hash`
(`scriptHashToTokenName`), `phantom-typed-quantities` (`Tagged`, `:>`),
`value-elimination-without-flatten` (`pelimValue`, `precValue`, `psplitValue`),
`sorted-merge`/`uniqueness` (`Ord`).

No `withdraw-zero-forward`, no `input-index-hint`, no `merkle-proof`, no explicit
`no-double-satisfaction` combinator — the library is a level below protocol composition.

---

# 5. Fortuna (TUNA proof of work, Aiken)

## 5.1 Helpers the protocol defines itself

| Name | Signature | Purpose | file:line |
|---|---|---|---|
| `resolve_output_reference` | `(List<Input>, OutputReference) -> Output` | Own-input lookup, raw `head_list`/`tail_list` recursion | `fortuna/lib/fortuna/utils.ak:6` |
| `list_at` | `(List<a>, Int) -> a` | Index via builtins; negative index diverges → tx fails | `fortuna/lib/fortuna/utils.ak:21` |
| `quantity_of` | `(Value, PolicyId, AssetName) -> Int` | Dict-based, avoids `flatten` | `fortuna/lib/fortuna/utils.ak:45` |
| `value_has_nft_and_lovelace` | `(Value, PolicyId, AssetName) -> Bool` | Value is **exactly** {ADA, 1×NFT}; exploits ledger sort order | `fortuna/lib/fortuna/utils.ak:60` |
| `get_inline_datum` | `(Datum) -> Data` | | `fortuna/lib/fortuna/utils.ak:124` |
| `integer_to_bytes` | `(Int, ByteArray) -> ByteArray` | Big-endian base-256 encode for token names | `fortuna/lib/fortuna/utils.ak:133` |
| `format_found_bytearray` | `(ByteArray) -> (Int, Int)` | Extract (difficulty, leading zeros) from a hash | `fortuna/lib/fortuna.ak:12` |
| `find_first_nonzero_byte` | `(ByteArray, Int) -> Int` | Nibble-precision leading-zero count | `fortuna/lib/fortuna.ak:38` |
| `value_has_only_master_and_lovelace` | `(Value, PolicyId) -> Bool` | Exact-shape master-token check | `fortuna/lib/fortuna.ak:66` |
| `value_contains_master_token` | `(Value, PolicyId) -> Bool` | Policy's token map is exactly `[(master, 1)]` | `fortuna/lib/fortuna.ak:87` |
| `get_difficulty_adjustment` | `(Int) -> (Int, Int)` | Clamped ratio, max 4×/¼ | `fortuna/lib/fortuna.ak:96` |
| `get_new_difficulty` | `(Int, Int, Int, Int) -> (Int, Int)` | Re-target with carry across leading-zero boundaries | `fortuna/lib/fortuna.ak:110` |
| `calculate_interlink` | `(List<Data>, Data, Int, Int, Int, Int) -> List<Data>` | NIPoPoW interlink vector maintenance | `fortuna/lib/fortuna.ak:140` |
| `expect_first` | `(Pairs<k,v>, k) -> v` | Assoc lookup that fails instead of returning `Option` | `fortuna/lib/fortunav2.ak:284` |
| `quantity_of` (v2) | `(Pairs<PolicyId,Dict<AssetName,Int>>, PolicyId, AssetName) -> Int` | Hand-written double loop on the already-unwrapped pairs | `fortuna/lib/fortunav2.ak:136` |
| `tokens` / `tokens2` / `loop_tokens` | `(Value, PolicyId) -> Pairs<AssetName,Int>` | Failing vs non-failing policy projection | `fortunav2.ak:296` / `:304` / `:311` |
| `flip_hash` | `(ByteArray) -> ByteArray` | Bitwise complement — derives the "anti" governance token name | `fortunav2.ak:117` |
| `count_votes` | `(List<Input>, PolicyId, AssetName, Option<StakeCredential>) -> Int` | Tally TUNA held in reference inputs | `fortunav2.ak:242` |
| `vote` | `(Bool, OutputReference, Transaction, TunaUpgradeProcess) -> Bool` | Governance state step | `fortunav2.ak:164` |
| `calculate_emission` | (in `hardfork.ak`) | Halving emission schedule | `fortuna/lib/hardfork.ak` |

Prefix constants (`fortunav2.ak:20-28`): `big_tuna_prefix = "TUNA"` (len 4),
`counter_prefix = "COUNTER"` (len 7), `nominated_prefix = "NOMA"`.

## 5.2 Recurring idioms

### Exactly one continuing output + exact output value

```aiken
// fortuna/validators/tunav1.ak:160
// Spend(0) requirement: Contract has only one output going back to itself
expect [own_output] = list.filter(outputs, fn(output: Output) { output.address == in_address })
...
// Spend(6) requirement: Output has only master token and ada
expect fortuna.value_has_only_master_and_lovelace(out_value, own_validator_hash)?
```

```aiken
// fortuna/validators/new_spend.ak:128
let expected_output_value =
  value.from_asset(tunav2_minting_policy,
                   bytearray.concat(fortunav2.big_tuna_prefix, own_script_hash), 1)
    |> value.add(tunav2_minting_policy,
                 bytearray.concat(fortunav2.counter_prefix, block_number_as_bytes), 1)
...
// Spend(7) requirement: Output has same tokens as input
expected_output_value == value.without_lovelace(out_value),
```

### Exact-value shape using ledger map ordering (avoids `flatten`)

```aiken
// fortuna/lib/fortuna/utils.ak:60
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
```

The tests at `utils.ak:106` and `:115` are `fail` tests proving extra tokens are rejected.

### Deterministic "current time" from a bounded validity range

```aiken
// fortuna/validators/tunav1.ak:181
let averaged_current_time = ( upper_range - lower_range ) / 2 + lower_range
...
// Spend(1) requirement: Time range span is 3 minutes or less and inclusive
expect and {
    !upper_is_inclusive?,
    lower_is_inclusive?,
    (upper_range - lower_range <= 180000)?,
  }
```

Note it also pins **inclusivity** of both bounds — otherwise the midpoint is ambiguous.

### One-shot genesis mint with entropy from the consumed oref

```aiken
// fortuna/validators/tunav1.ak:73
expect list.any(inputs, fn(input) { input.output_reference == utxo_ref })?
let bootstrap_hash = utxo_ref |> builtin.serialise_data() |> sha2_256() |> sha2_256()
...
// Mint(3): Mints master token   // Mint(4): master goes to the only script output
expect fortuna.value_contains_master_token(mint, own_policy)?
expect fortuna.value_has_only_master_and_lovelace(value, own_policy)?
expect output_state: State = output_datum
let initial_state = State { block_number: 0, current_hash: bootstrap_hash,
                            leading_zeros: 5, target_number: 65535, epoch_time: 0,
                            current_posix_time: averaged_current_time, extra: 0,
                            interlink: [] }
(initial_state == output_state)?      // whole-datum structural compare
```

Cheaper variant in the hardfork script — hash the **first** input rather than searching:

```aiken
// fortuna/lib/hardfork.ak:138
// Use blake hash since it's faster and this is unrelated to Proof of Work
let own_token_name =
  builtin.head_list(inputs).output_reference |> builtin.serialise_data |> blake2b_256
expect [Pair(minted_token, 1)] =
  mint |> value.from_minted_value |> value.tokens(own_policy) |> dict.to_pairs
```

### Proof of work over a serialised redeemer/datum struct

```aiken
// fortuna/validators/new_spend.ak:91
let target = TargetState { nonce, block_number, epoch_time, current_hash,
                           leading_zeros, target_number,
                           miner: blake2b_256(builtin.serialise_data(miner)) }
let found_bytearray = target |> builtin.serialise_data() |> sha2_256() |> sha2_256()
let (found_target_number, found_leading_zeros) = fortuna.format_found_bytearray(found_bytearray)
...
or { found_leading_zeros > leading_zeros,
     and { found_leading_zeros == leading_zeros, found_target_number < target_number } },
```

### Merkle Patricia Forestry proof (append-only block index)

```aiken
// fortuna/validators/new_spend.ak:222
// Spend(13) requirement: Check output merkle is correct
insert(
  merkle_root |> from_root,
  found_bytearray |> blake2b_256,
  found_bytearray,
  merkle_proof_list,
) == from_root(out_merkle),
```

with `use aiken/merkle_patricia_forestry.{Proof, from_root, insert}`
(`new_spend.ak:7`) and `Proof` carried in the redeemer (`new_spend.ak:25`).

### Datum/redeemer size caps (griefing protection)

```aiken
// fortuna/validators/tunav1.ak:307
//Spend(12) requirement: Check output extra field is within a certain size
expect (builtin.length_of_bytearray(builtin.serialise_data(extra)) <= 512)?
```

```aiken
// fortuna/validators/new_spend.ak:229
// Spend(14) requirement: Data size doesn't exceed 1/4 tx size
builtin.serialise_data(miner)
  |> builtin.length_of_bytearray()
  |> builtin.less_than_equals_integer(4096),
```

### Miner may be an NFT held by *any* validator — programmable mining pools

```aiken
// fortuna/validators/new_spend.ak:154
when miner is {
  Pkh(signer, _) -> list.has(transaction.extra_signatories, signer)
  Nft { policy: nft_policy, name: nft_name, output_index, .. } -> {
    let quantity =
      utils.list_at(outputs, output_index).value      // <-- output index hint
        |> value.to_dict |> dict.to_pairs |> quantity_of(nft_policy, nft_name)
    quantity == 1
  }
}
// comment at :149: "Nfts can come from any input, even validators
//                   So any validator logic can be enforced to run along with fortuna"
```

### Cross-script "this action can only run once" coupling

```aiken
// fortuna/lib/fortunav2.ak:42
let fork_purpose = tx.WithdrawFrom(Inline(ScriptCredential(fork_script_hash)))
let fork_redeemer = expect_first(tx.redeemers, fork_purpose)
// By looking at the other contract redeemer action that can only run once,
// We can ensure this action only runs once.
expect HardFork { lock_output_index }: NftForkAction = fork_redeemer
// We can assume the next output is the one we look at
let tunav2_output = list_at(outputs, lock_output_index + 1)
```

```aiken
// fortuna/validators/new_spend.ak:236
Upgrade -> {
  let upgrade_rdmr = expect_first(redeemers, Mint(tunav2_minting_policy))
  expect FinalizeNomination { .. }: TunaAction = upgrade_rdmr
  True
}
```

### Prefix-namespaced token families in one policy, with counter tokens

```aiken
// fortuna/validators/tunav2.ak:109 — expected mint list built in *ledger sort order*
let expected_mint = {
  let tail = if halving_exponent > 32 { [] }
             else { [Pair(fortuna.token_name, initial_payout / pow2(halving_exponent))] }
  // Here we burn the previous counter and mint the new one
  // Thus ensuring the minting policy is always aware of the current block number
  if expected_in_token_name |> builtin.less_than_bytearray(expected_out_token_name) {
    [Pair(expected_in_token_name, -1), Pair(expected_out_token_name, 1), ..tail]
  } else {
    [Pair(expected_out_token_name, 1), Pair(expected_in_token_name, -1), ..tail]
  }
}
```

```aiken
// fortuna/validators/tunav2.ak:82 — reading the counter back out, exploiting sort order
expect [Pair(token1, 1), Pair(token2, 1)] = spend_value |> tokens(own_policy)
let in_token_name =
  // By ledger rules we should only hit this first since token names are lexographically sorted
  if token2 == big_tuna_name { token1 }
  else if token1 == big_tuna_name { token2 }
  else { fail @"Big Tuna not found in input value" }
```

```aiken
// fortuna/validators/tunav2.ak:264 — prefix classification on burn
let tuna_and_master_tokens_burned = fn(name) { or {
    and { builtin.slice_bytearray(0, big_tuna_length, name) == fortuna.token_name,
          or { builtin.length_of_bytearray(name) == 4,
               builtin.length_of_bytearray(name) == 32 } },
    builtin.slice_bytearray(0, counter_length, name) == fortunav2.counter_prefix,
  } }
```

The **counter token** idea is worth highlighting: minting policies cannot read datums, so
Fortuna encodes the current block number *into a token name* that is burned and re-minted
every block, giving the minting policy O(1) access to the state counter.

### "Account for every input at my own address"

```aiken
// fortuna/lib/hardfork.ak:120
// Needed to ensure all validator spends from this script hash are accounted for
let script_inputs = list.filter(inputs, fn(input) { input.output.address == own_address })
...
expect [Input { output: lock_input, .. }] = script_inputs           // exactly one   (:153)
...
expect [script_input1, script_input2] = script_inputs               // exactly two   (:174)
```

### Governance: token-weighted votes from reference inputs, deadline pinned

```aiken
// fortuna/lib/fortunav2.ak:224
and {
  (quantity_of(in_value |> value.to_dict |> dict.to_pairs, own_hash,
               bytearray.concat(nominated_prefix, script_hash)) == 1)?,
  (in_address == out_address)?,
  (upper_range <= deadline)?,
  (value.without_lovelace(in_value) == value.without_lovelace(out_value))?,
  if for { votes_in_tx > for_count } else { votes_in_tx > against_count }?,
  (expected_datum == out_datum)?,       // whole-Datum structural compare
}
```

## 5.3 Idioms present (Fortuna)

`one-shot-mint`, `tag-with-txoutref` (bootstrap hash), `single-script-input`
(`expect [x] = filter(own address)`), `output-to-self-with-datum`,
`validity-range-check` (+ inclusivity pinning + midpoint time),
`input-index-hint`, `output-index-hint`, `data-level-compare` (whole-`State` and
whole-`Datum` equality), `merkle-proof` (MPF `insert`), `exact-value-shape`
(`value_has_only_master_and_lovelace`), `exact-mint-accounting` (expected mint list in
sort order), `asset-name-prefix-namespace`, `counter-token-as-state`,
`redeemer-cross-check` (`expect_first(redeemers, Mint(policy))`), `datum-size-cap`,
`nft-as-programmable-authority`, `direct-builtin-list-access`, `token-weighted-vote`.

No `withdraw-zero-forward` in the mining path (Fortuna's spend script does the work),
no `ratio-math` beyond integer difficulty re-targeting, no `oracle-freshness`,
no `protocol-fee-cut`.

---

# 6. Cross-repo summary: idioms present

Canonical short names, per repo. `Y` = present with quoted evidence above.

| Idiom | lenfi | indigo | butane | liqwid | fortuna |
|---|---|---|---|---|---|
| `auth-nft-check` | Y | Y | Y | Y | Y |
| `one-shot-mint` | Y | – | Y | Y | Y |
| `tag-with-txoutref` | Y | Y | Y | – | Y |
| `no-double-satisfaction` | Y | Y | Y | – | – |
| `single-script-input` | Y | Y | Y | Y | Y |
| `output-to-self-with-datum` | Y | Y | Y | Y | Y |
| `withdraw-zero-forward` | Y | – | Y | – | – |
| `spend-forwards-to-mint` | Y | – | Y (to-withdraw) | – | Y |
| `validity-range-check` | Y | Y | Y | Y | Y |
| `input-index-hint` | Y | – | Y | – | Y |
| `output-index-hint` | Y | – | Y | – | Y |
| `data-level-compare` | Y | Y | Y | Y | Y |
| `merkle-proof` | – | – | – | – | Y |
| `min-ada` | Y (via `without_lovelace`) | Y (`checkOutput`) | Y (`max_min_ada`) | – | Y (`without_lovelace`) |
| `protocol-fee-cut` | Y | Y | Y | – | – |
| `ratio-math` | Y | Y | Y | Y | – |
| `oracle-freshness` | Y | Y | Y | – | – |
| `n-of-m-signature-threshold` | Y | – | Y (single key) | Y | – |
| `exact-mint-accounting` | Y | – | Y | – | Y |
| `exact-value-shape` / dust guard | Y | Y | Y | Y | Y |
| `asset-name-prefix-namespace` | – | – | Y | – | Y |
| `tag-token-with-script-hash` | Y | – | Y | Y | Y |
| `upgradeable-pointer-script` | Y | Y (VersionRegistry) | Y | – | Y (nomination) |
| `batch-cursor-fold` | Y | – | Y | – | – |
| `sorted-merge-join` | – | – | Y | Y | Y |
| `step-function-interest` | – | – | Y | – | – |
| `lazy-data-fields` | – | Y (Spooky) | – | – | – |
| `direct-builtin-list-access` | – | – | Y | – | Y |
| `datum-size-cap` | – | – | – | – | Y |
| `delegated-signature-permit` | – | – | Y | – | – |
| `counter-token-as-state` | – | – | – | – | Y |

---

# 7. Notes for the stdlib design (observations, not recommendations)

- **`find*` variants multiply by three axes**: (inputs | reference inputs | outputs) ×
  (by oref | by token | by address | by index) × (`Option` | fail-fast | "exactly one").
  Indigo has 8 of the 24 cells; Lenfi has 5; Butane 4. A stdlib that generates the grid
  once would delete hundreds of lines per protocol.
- **"Exactly one" is the safety-critical variant.** Every repo has a comment or a `fail`
  test explaining that `>= 1` is exploitable. `phasOnlyOneTokenOfCurrencySymbol` vs
  `phasOneTokenOfCurrencySymbol` (`liqwid .../Value.hs:575` / `:613`) is the clearest
  articulation.
- **Two competing anti-double-satisfaction schools appear**:
  (a) *tag the output* with the consumed `TxOutRef` (Lenfi `order_contract.ak:101`),
  (b) *aggregate accounting* — accumulate an expected mint/fee delta across all actions
  and assert one final equality (Butane `cdp_script.ak:845`, Indigo `OnChain.hs:576`).
  (b) is strictly stronger but requires the "all logic in one withdraw-0 script"
  architecture.
- **The ordered-cursor fold** (Lenfi `collateral.ak:519`, Butane `cdp_script.ak:97`) is
  the shared mechanism for batch validation. It needs three stdlib pieces: a monotonic
  input cursor (`until_input_from` / `drop_while` + `tail`), a short-circuiting
  accumulator, and a terminal "nothing left unaccounted" assertion.
- **Money types**: three different fixed-point schemes appear (`OnChainDecimal` at 1e6,
  basis points at 1e4, `PFixedDecimal` with a type-level exponent) plus two rational
  flavours (reducing / non-reducing). The non-reducing rational
  (`PRationalNoReduce`) exists purely because `gcd` is expensive on-chain.
- **Value handling is where the budget goes.** Every repo avoids `flatten` in hot paths:
  Fortuna hand-writes a double loop over `Pairs` (`fortunav2.ak:136`), Butane writes a
  CPS merge-join (`prices.ak:151`), liqwid provides `pelimValue`/`precValue`, Indigo
  projects with `valueOfAssetCls`. `flatten` survives only as a *cheap arity check*
  (Lenfi's dust guard).
