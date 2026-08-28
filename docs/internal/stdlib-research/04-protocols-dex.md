# What production Cardano DEX/orderbook contracts re-implement by hand

Research note for the Scalus "smart contract standard library" design. Goal: find the helper
functions and validation idioms that every real DEX/AMM/orderbook on Cardano writes itself,
because no standard library provides them.

Everything below is quoted verbatim from real source. Every `file:line` reference was read
directly (`awk`/`sed`/`grep` on the checkout), not reconstructed from memory.

---

## 1. Corpus

| Protocol | Language | Repo | Commit | Scope read |
|---|---|---|---|---|
| **SundaeSwap v3** | Aiken | `SundaeSwap-finance/sundae-contracts` | `be33466b` | `lib/shared.ak`, `lib/calculation/*`, `validators/{pool,order,stake}.ak`, `validators/oracle.ak` (head only), `lib/types/settings.ak`, `lib/calculation/InputSorting.md` |
| **Minswap v2** | Aiken | `minswap/minswap-dex-v2` | `c3293ad4` | `lib/amm_dex_v2/{utils,pool_validation,order_validation}.ak`, `validators/{pool_validator,order_validator}.ak` |
| **WingRiders v2** | Plutarch | `WingRiders/dex-v2-contracts` | `280a9e89` | `src/Plutarch/Util.hs`, `src/Plutus/Util.hs`, `src/DEX/{Pool,Request}.hs`, `src/DEX/Types/Pool.hs` |
| **Splash** (ex-Spectrum) | Plutarch + Aiken | `splashprotocol/splash-core` | `f10d6784` | `plutarch-validators/PExtra/*`, `WhalePoolsDex/PContracts/{PApi,PPool,PSwap}.hs`, `validators_v2/{lib,validators}/**.ak` |
| **Spectrum Finance** | Plutarch | `spectrum-finance/cardano-dex-contracts` | `c25c8ab7` | `cardano-dex-contracts-onchain/ErgoDex/PContracts/{PSwap,PDeposit,PRedeem}.hs`, `PExtra/List.hs` |
| **Genius Yield DEX** | Plutarch (v1) + Aiken (v2) | `geniusyield/dex-contracts-api` | `421b673b` | `geniusyield-onchain/src/GeniusYield/OnChain/DEX/{PartialOrder,Utils}.hs`, `dex-v2-contracts/{validators,lib}/**.ak` |
| **JPG Store v3** | Aiken | `jpg-store/contracts-v3` | `de7365cb` | `lib/jpg/utils.ak`, `validators/ask.ak` |
| **cardano-swaps** | Aiken | `fallen-icarus/cardano-swaps` | `520ea1d2` | `aiken/lib/cardano_swaps/**`, `aiken/validators/one_way_swap.ak` |
| **Bodega Market** | Aiken | `bodega-market-smart-contracts` | `c1b85d6d` | `validators/predictions.ak`, `lib/bodega/{types,utils}.ak` |
| **Intersect treasury** | Aiken | `treasury-contracts` (SundaeSwap/Intersect) | `dea9e526` | `lib/utilities.ak`, `lib/logic/vendor/withdraw.ak` |
| **Midgard L2** | Aiken | `al-ft/midgard` | `9a2b636c` | `onchain/aiken/lib/midgard/common/utils.ak`, `aiken.toml` |
| **CIP-113 tokens** | Aiken | `cip113-programmable-tokens` | `556133b5` | `src/programmable-tokens-onchain-aiken/lib/utils.ak` |
| *(baseline)* `anastasia-labs/aiken-design-patterns` | Aiken | v1.8.0 | `dab05940` | `lib/aiken-design-patterns/*` |
| *(baseline)* `aiken-lang/stdlib` | Aiken | main | — | `lib/cardano/{transaction,assets}.ak` |

**Not analysed.** TeddySwap: confirmed by its own blog and by the `teddy-swap` GitHub org that
it was "built using the open-source smart contracts from Spectrum Finance, previously known as
ErgoDex" — analysing it would double-count Spectrum, so it is excluded.
MuesliSwap, Levvy, Optim, Indigo: repos not resolved in one search round; **no claims made
about them** in this report.

**The baseline that makes the gap concrete.** `aiken-lang/stdlib`'s
`lib/cardano/transaction.ak` exposes exactly four transaction-level functions:
`find_input`, `resolve_input`, `find_datum`, `find_script_outputs`. Everything else in the
ranked table below is a gap that twelve protocols each filled themselves.

**The ecosystem already tried to library-ise this.** Midgard's `onchain/aiken/aiken.toml`
declares:

```toml
[[dependencies]]
name = "anastasia-labs/aiken-design-patterns"
version = "v1.2.0"
source = "github"
```

`aiken-design-patterns` ships exactly the patterns found by hand in the DEXes: UTxO indexers,
the withdraw-zero staking trick, validity-range normalisation, merkelised validators.

---

## 2. Ranked table: helper/idiom → protocols that hand-roll it → proposed Scalus API

Counts are over the 12 protocols in section 1 (design-patterns/stdlib excluded from the count,
noted separately).

Every **Count** equals the number of protocols named in its row. `aiken-design-patterns` and
`aiken-lang/stdlib` are never counted — where they also ship the helper it is noted in
parentheses, outside the count.

| # | Helper / idiom | Count | Protocols | In aiken-stdlib? | Proposed one-line Scalus API |
|---|---|---|---|---|---|
| 1 | **Redeemer-carried input/output index → resolve UTxO** | **8** | Sundae, Minswap, WingRiders, Splash, Spectrum, Bodega, Midgard, JPG *(also design-patterns)* | No | `tx.spendingWith(redeemer.inIx, redeemer.outIx) { (in, out) => ... }` — asserts `in.outRef == ownRef` |
| 2 | **Unrolled `list.at` / skip-N-at-a-time indexing** | **5** | Sundae (15/7), Minswap (10), JPG (10), WingRiders (5), cip113 (2) | Only naive `list.at` | `List.dropFast(n)` / `List.atFast(n)` with a compiler-generated unrolled skip |
| 3 | **Continuing output: same address + NFT + preserved datum fields** | **8** | Sundae, Minswap, WingRiders, Splash, Spectrum, cardano-swaps, treasury, Bodega | No | `tx.singleContinuingOutput(ownAddr, threadToken)` returning `(Output, D)` |
| 4 | **NFT-marked config / registry / license UTxO lookup (reference input or input)** | **6** | Sundae, Minswap, GY, treasury, Midgard, Bodega | No | `tx.referenceInputWithNft[D](policy, name): D` |
| 5 | **Withdraw-zero staking-script presence check** | **6** | Sundae, Minswap, Splash, GY v2, cardano-swaps, treasury *(also design-patterns)* | No | `tx.withdrawalPresent(scriptHash)` and `stakeValidator { ... }` builder |
| 6 | **Extract / assert the single non-ADA asset on a UTxO** | **5** | Midgard, JPG, Sundae (`has_exact_token_count`), Splash (`pValueLength`), cip113 (`count_unique_tokens`) *(also design-patterns)* | Partly (`has_nft_strict`) | `value.singleAssetApartFromAda: (PolicyId, AssetName, BigInt)` |
| 7 | **Validity range → finite bounds / current-time approximation** | **7** | Minswap, Bodega, WingRiders, Sundae (`contains_interval`), cardano-swaps, GY v2 (`is_after`/`is_before`), treasury (`interval_length_at_most`) *(also design-patterns)* | `interval` module only | `tx.timeWindow: (Long, Long)`, `tx.approxNow(maxWidth)` |
| 8 | **Multi-method authorization (signature \| spend-script \| withdraw-script \| mint-script)** | **5** | Minswap, Sundae (`sundae/multisig`), treasury, GY v2 (`multisig.ak`), cardano-swaps (`staking_credential_approves`) | No | `Authorization` ADT + `auth.satisfiedBy(tx)` |
| 9 | **Inline-or-hash datum resolution** | **4** | Sundae (`datum_of`), Minswap (`must_find_script_datum`), Splash (`DatumHashMap`), cip113 (`expect_inline_datum`) | `find_datum` (partial) | `output.datumAs[D](tx.datums): D` |
| 10 | **Value preservation with fee / "oil" ADA tolerance** | **5** | Minswap (`min_pool_ada`), WingRiders (`pvalueOfWithOilCheck`), treasury (`equal_plus_min_ada`), Sundae, Splash | No | `Value.equalPlusMinAda(expected, actual)`, `Value.quantityOfNetOfOil(...)` |
| 11 | **Index-uniqueness enforcement over a batch** | **3** | Sundae (bitmask), Minswap (byte-set), WingRiders (`pcontainsDuplicate`) *(design-patterns uses ascending order instead)* | No | `Indices.checkUnique(flags, i): BigInt` (bitmask) + `List.strictlyAscending` |
| 12 | **Asset-class ordering / canonical pair sort** | **4** | Sundae (`compare_asset_class`), Minswap (`sorted_asset`), cardano-swaps, cip113 (`bytearray_lt`) | No | `AssetClass.compare` + `Ordering` given |
| 13 | **Datum equality by serialised hash** | **4** | Minswap (`is_valid_datum`), Midgard (`verify_hash_32`), WingRiders (`phashDatum`), Sundae (`cbor.serialise` for signing) | `builtin.serialise_data` only | `Data.hashBlake2b256`, `output.datumMatchesHash(h)` |
| 14 | **Count/filter script inputs (own vs. foreign)** | **6** | Sundae (`count_orders`), Minswap (`has_only_pool_and_author`), WingRiders (`pcountScriptInputs`), treasury (`ensure_compliant_scripts`), Splash (`checkInputsQty`), Spectrum (`checkInputsQty`) | No | `tx.scriptInputCount`, `tx.assertOnlyScripts(allowed)` |
| 15 | **Zip / positional fold over two lists (in ↔ out)** | **4** | Splash (`left_biased_zip_validate`), Midgard (`zip_foldl`/`zip_foldr`), Minswap (`zip_with`), WingRiders (`pfoldInputsWithTwoOutputs`) | No | `List.zipFold(as, bs, z)(f)` with early exit |
| 16 | **Redeemer lookup by script purpose / index** | **3** | Sundae (`pairs.foldl` over redeemers), Midgard (`get_redeemer_at`), WingRiders (`ptryTxOutRefRedeemer`) *(also design-patterns)* | No | `tx.redeemerFor(purpose)` / `tx.redeemerAt(i, purpose)` |
| 17 | **Bounded `Value` shape check ("nothing else on this UTxO")** | **5** | Sundae (`has_expected_pool_value`), Minswap (`estimate_pool_out_value ==`), Splash (`pValueLength`), treasury (≤4 assets), WingRiders (`pcountOfUniqueTokens`) | No | `Value.hasExactly(expected)` / `value.assetCount` |
| 18 | **"Verify, don't compute" (check a witness instead of computing)** | **4** | Sundae (`is_sqrt`, one-less swap check), Splash (`correctOut #<= relaxedOut`), Spectrum, Minswap (`compare_swap_output_value`) | n/a — design principle | Documented pattern + `Math.isSqrt(n, root)` |
| 19 | **CIP-68 label prefixing for thread/LP/ref tokens** | **2** | Sundae (`pool_ref_name`/`pool_nft_name`/`pool_lp_name`), Midgard | No | `Cip68.refName/nftName/ftName(ident)` |
| 20 | **Deterministic ident from first spent input (NFT uniqueness)** | **3** | Sundae, GY v2 (`populate_nft_set`), cip113 | No | `OutputRef.toIdent(ref): ByteString` (hash + truncate) |

Note on row 1: Genius Yield v2 is deliberately **not** counted there. Its comment says
"(UTXO indexer)" but the position is a fixed *convention* (`list.head(self.outputs)`), not an
index carried in the redeemer — it belongs to strategy F in section 3.

---

## 3. Double-satisfaction countermeasures — six distinct strategies

This is the most important finding. There is **no single accepted defence**; every protocol
picks one (or two) of six mutually independent strategies, and each re-implements it.

`aiken-design-patterns` explicitly refuses to solve it — its own README says:

> **[!NOTE]** Neither of the singular UTxO indexer patterns provides protection against the
> [double satisfaction](https://github.com/Plutonomicon/plutonomicon/blob/.../vulnerabilities.md#double-satisfaction)
> vulnerability, as this can be done in multiple ways depending on the contract.

and encodes that refusal in the *type signature* of the helper
(`aiken-design-patterns/lib/aiken-design-patterns/singular-utxo-indexer.ak:20-38`):

```rs
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

### Strategy A — redeemer carries indices ("UTxO indexer")

The redeemer names which input is "self" and which output is its continuation. The validator
must then check `ownRef == inputs[ix].outRef`, or an attacker points at someone else's UTxO.

**Splash / stable pool** — `splash-core/validators_v2/validators/stable_pool/pool.ak:41-45,71,94`:

```rs
    expect Spend(pool_in_utxo_reference) = purpose
    let PoolRedeemer { pool_in_ix, pool_out_ix, action } = redeemer

    // ==================================== POOL INPUT ==================================== //
    expect Some(pool_input) = list.at(inputs, pool_in_ix)
...
    expect Some(pool_out_output) = list.at(outputs, pool_out_ix)
...
    // 1. Pool input is valid:
    let valid_pool_input: Bool = pool_in_utxo_reference == pool_output_reference
```

**Splash / Spectrum orders** — three indices in one redeemer,
`splash-core/plutarch-validators/WhalePoolsDex/PContracts/PSwap.hs:80-117`:

```haskell
    redeemer <- pletFieldsC @'["poolInIx", "orderInIx", "rewardOutIx", "action"] redeemer'
    ...
    rewardOut   <- tlet $ pelemAt # rewardOutIx # outputs
    rewardValue <- tlet $ getRewardValue' # rewardOut # rewardPkh # stakePkh

    poolIn'   <- tlet $ pelemAt # poolInIx # inputs
    ...
    selfIn'   <- tlet $ pelemAt # orderInIx # inputs
    ...
    PSpending selfRef' <- tmatch (pfromData $ getField @"purpose" ctx)
    let
        selfIdentity =
            let selfRef   = pfromData $ pfield @"_0" # selfRef'
                selfInRef = pfromData $ getField @"outRef" selfIn
             in selfRef #== selfInRef -- check that orderInIx points to the actual order
```

**Bodega Market** — every redeemer variant is a bundle of indices
(`bodega-market-smart-contracts/lib/bodega/types.ak:57-76`):

```rs
pub type PredictionRedeemer {
  PredApply {
    own_input_idx: Int,
    own_output_idx: Int,
    license_idx: Int,
    pos_indices: List<(Int, Int)>,
  }
  ...
}
```

used at `validators/predictions.ak:34-70`:

```rs
      PredApply { own_input_idx, own_output_idx, license_idx, pos_indices } -> {
        ...
        expect Some(own_input) = list.at(inputs, own_input_idx)
        expect Some(own_output) = list.at(outputs, own_output_idx)
        ...
            fn(curr, acc) {
              let (in_idx, out_idx) = curr
              ...
              expect Some(in) = list.at(inputs, in_idx)
              expect Some(out) = list.at(outputs, out_idx)
```

**WingRiders v2** — the pool redeemer carries a pool index, an agent index, and a list of
`(index, data)` pairs for every request
(`wingriders-v2/src/DEX/Types/Pool.hs:174-189`):

```haskell
data PPoolRedeemer (s :: S)
  = PEvolve
      ( Term
          s
          ( PDataRecord
              '[ "poolLocation" ':= PInteger
               , "agentLocation" ':= PInteger
               , -- List of request locations and their data.
                 ...
                 "requestLocations" ':= PBuiltinList (PAsData (PBuiltinPair (PAsData PInteger) (PAsData PData)))
               ]
          )
      )
```

**Minswap v2** packs the order permutation into a `ByteArray`
(`minswap-dex-v2/validators/pool_validator.ak:166-172, 239-255`):

```rs
    let PoolBatchingRedeemer {
      batcher_index,
      orders_fee,
      input_indexes,
      pool_input_indexes_opt,
      vol_fees,
    } = redeemer
...
    // Currently, transaction inputs will be sorted by TxId and TxIndex of UTxO.
    // We have to calculate indexes of orders inputs sorting by the ASC created time
    // on the off-chain and on-chain will sort the TxIns by the indexes
    // Input Indexes in parameter will be the index indexes of @order_inputs
    let sorted_order_inputs =
      bytearray.foldr(
        input_indexes,
        [],
        fn(idx, ips) { list.push(ips, utils.list_at_index(order_inputs, idx)) },
      )
```

**Genius Yield DEX v2** names the pattern in a comment
(`geniusyield-dex/dex-v2-contracts/validators/order.ak:56-57`):

```rs
            // fee output "agreed" to be first output if output list is non-empty (UTXO indexer)
            expect Some(fee_output) = list.head(self.outputs)
```

**Midgard** wraps it as a named helper
(`midgard/onchain/aiken/lib/midgard/common/utils.ak:86-96`):

```rs
/// Helper function for accessing spending UTxO using its input index.
pub fn get_own_input_at(
  inputs: List<Input>,
  own_out_ref: OutputReference,
  input_index: Int,
) -> Output {
  expect Some(Input { output: own_utxo, output_reference: spent_own_out_ref }) =
    list.at(inputs, input_index)
  expect (own_out_ref == spent_own_out_ref)?
  own_utxo
}
```

### Strategy B — index uniqueness (the batch must not process one order twice)

Strategy A alone is not enough for batchers: a malicious batcher can list index 1 twenty
times and satisfy one tiny order twenty times, stealing the other nineteen.

**SundaeSwap v3** solves it with an arbitrary-precision-integer bitmask, and documents the
trick at length in `sundae-contracts/lib/calculation/InputSorting.md`:

> A malicious scooper could include the right number of entries in the redeemer, but omit some
> orders and repeat others. [...] the key innovation is: we implement the uniqueness check by
> taking advantage of UPLC arbitrary size integers, and emulating bitwise operations with
> arithmetic! [...] if we add a power of two, and that bit is already set, it results in a
> carry operation. The number overall will be larger, but the number if we mask off any
> "higher" bits [...] will decrease.

`sundae-contracts/lib/calculation/shared.ak:71-122`:

```rs
/// Take in `uniqueness_flags`, which is treated as a bit vector, and flip the `index`th bit
/// Fail if it was already set
/// See InputSorting.md for a full explanation
pub fn check_and_set_unique(uniqueness_flags: Int, index: Int) -> Int {
  expect index >= 0
  let bit = small_pow2(index)
  let bit_shifted = 2 * bit
  let flag_set = uniqueness_flags + bit
  expect flag_set % bit_shifted > uniqueness_flags % bit_shifted
  ...
  flag_set
}
```

`small_pow2` is itself a hand-rolled optimisation with a byte-array lookup table
(`lib/calculation/shared.ak:127-152`):

```rs
pub fn small_pow2(exponent: Int) -> Int {
  // A small bytestring, containing all the powers of two that can fit in a single byte, saves us a an expensive multiplication
  let single_byte_powers = #[1, 2, 4, 8, 16, 32, 64, 128]
  if exponent < 8 {
    builtin.index_bytearray(single_byte_powers, exponent)
  } else if exponent < 16 {
    256 * builtin.index_bytearray(single_byte_powers, exponent - 8)
  ...
```

**Minswap v2** implements the same set with a mutable-looking byte array
(`minswap-dex-v2/lib/amm_dex_v2/utils.ak:379-412`):

```rs
fn do_is_unique_bytearray_unsorted(
  self: ByteArray,
  acc: ByteArray,
  index: Int,
  length: Int,
) {
  if index >= length {
    True
  } else {
    let value_as_index = builtin.index_bytearray(self, index)
    let acc_value = builtin.index_bytearray(acc, value_as_index)
    if acc_value == 1 {
      False
    } else {
      let left_slice = builtin.slice_bytearray(0, value_as_index, acc)
      let righ_slice = builtin.slice_bytearray(value_as_index + 1, 64, acc)
      let new_acc =
        builtin.append_bytearray(
          left_slice,
          builtin.cons_bytearray(1, righ_slice),
        )
      do_is_unique_bytearray_unsorted(self, new_acc, index + 1, length)
    }
  }
}
```

called at `validators/pool_validator.ak:193-194`:

```rs
        // Input indexes must not be empty list and be unique
        utils.is_unique_bytearray_unsorted(input_indexes),
```

**WingRiders v2** does an O(n²) membership scan plus a count identity
(`wingriders-v2/src/DEX/Pool.hs:788-797` and `:321-328`):

```haskell
pcontainsDuplicate :: Term s (PBuiltinList (PAsData (PBuiltinPair (PAsData PInteger) (PAsData a))) :--> PBool)
pcontainsDuplicate = plam $ \l -> (pfix # plam f) # l # (pnil @PList)
  where
    f recur l xs =
      pelimList
        ( \y ys -> plet (pfromData $ pfstBuiltin # pfromData y) \location ->
            pif (pelem # location # xs) ptrue (recur # ys # (pcons # location # xs))
        )
        pfalse
        l
```

```haskell
    noRequestLeftOut =
      pand'List
        [ inputCount #== (2 + plength # requestLocations)
        , inputCount #> 2
        ]
    -- As we use requestsLocations to reconstruct inputs in correct order,
    -- it is necessary that requestsLocations is injective mapping
    noRequestDuplicated = pnot # (pcontainsDuplicate # requestLocations)
```

**aiken-design-patterns** takes a cheaper route: force strictly ascending indices, which makes
duplicates impossible without a set
(`lib/aiken-design-patterns/multi-utxo-indexer.ak:38-47`):

```rs
            if i == in1 && in1 > in0 && out1 > out0 {
              expect Some(out_utxo) = outputs |> list.at(out1)
              if validation_logic(in1, input, out1, out_utxo) {
                return(rest_of_indices, in1, out1, next_i)
              } else {
                fail @"Validation failed"
              }
            } else {
              fail @"Input and output indices must be in ascending orders"
            }
```

and in `singular-utxo-indexer.ak:72-79`:

```rs
    fn(curr_ix, prev_ix, outputs_acc, return) {
      if curr_ix < prev_ix {
        ...
      } else {
        fail @"Output indices must be in ascending order"
      }
    },
```

### Strategy C — datum tag: hash your own `OutputReference` into the payout datum

Makes a payout output usable by *exactly one* spending input.

**JPG Store v3** — `jpg-store-v3/validators/ask.ak:45-55`:

```rs
        expect Spend(out_ref) = purpose

        // for double satisfaction
        let datum_tag =
          out_ref
            |> serialise_data
            |> blake2b_256
            |> InlineDatum

        let Datum { payouts, .. } = datum

        // Find the `outputs` that correspond to `payouts`.
        let payout_outputs = find_payout_outputs(outputs, payout_outputs_offset)
```

The tag is then required on the *first* payout output only, and `NoDatum` on the rest
(`lib/jpg/utils.ak:203-207` and `:243-247`):

```rs
  expect [first_output, ..rest_outputs] = outputs

  let Output { address: output_address, value, datum, .. } = first_output

  expect datum == datum_tag
```

```rs
  expect datum == NoDatum
```

**cardano-swaps** does the same with a typed field instead of a hash: the continuing output's
datum must equal the input's datum *except* `prev_input == Some(input_ref)`
(`cardano-swaps/aiken/lib/cardano_swaps/one_way_swap/utils.ak:489-502`):

```rs
  let req_datum =
    SwapDatum(
      beacon_id,
      pair_beacon,
      offer_id,
      offer_name,
      offer_beacon,
      ask_id,
      ask_name,
      ask_beacon,
      swap_price,
      Some(input_ref),
      expiration,
    ) |> InlineDatum
```

with the intent stated at `aiken/validators/one_way_swap.ak:86-99`:

```rs
        //      b) The swap datum must be exactly the same as the input's except the prev_input
        //         must be == Some(input_ref).
        ...
        // Requirement 2 guarantees that beacons from other trading pairs cannot be combined into
        // one output UTxO; all swaps must get their own swap UTxOs.
```

### Strategy D — strict input cardinality / no foreign scripts

**Spectrum Finance** — the comment is literally the vulnerability name
(`spectrum-dex-contracts/cardano-dex-contracts-onchain/ErgoDex/PContracts/PSwap.hs:124-133`):

```haskell
    let 
        strictInputs = -- ensure double satisfaction attack is not possible
            let inputsLength = plength # inputs
             in inputsLength #== 2
        minSatisfaction = minOutput #<= quoteAmount -- configured minimal output is satisfied
        fairPrice = validPrice # quoteAmount # poolValue # base # quote # baseAmount # feeNum

    pure $
        pmatch action $ \case
            Apply -> poolIdentity #&& selfIdentity #&& strictInputs #&& minSatisfaction #&& fairExFee #&& fairPrice
```

The identical block appears in `PDeposit.hs:109` and `PRedeem.hs:106`, and survives verbatim
into Splash (`splash-core/plutarch-validators/WhalePoolsDex/PContracts/PSwap.hs:132-141`). It
is also codified as a helper in
`spectrum-dex-contracts/.../ErgoDex/PContracts/PApi.hs` / Splash `PApi.hs:122-126`:

```haskell
checkInputsQty :: Term s (PBuiltinList (PAsData PTxInInfo) :--> PBool)
checkInputsQty = phoistAcyclic $
    plam $ \inputs ->
        let inputsLength = plength # inputs
         in inputsLength #== 2
```

**Intersect treasury** names the function after the attack
(`treasury-contracts/lib/utilities.ak:33-53`):

```rs
// To prevent double satisfaction, we disallow any inputs from *other* scripts
// ONLY the treasury and vendor scripts are allowed on the inputs
pub fn ensure_compliant_scripts(
  inputs: List<Input>,
  registry: ScriptHashRegistry,
) {
  expect
    inputs
      |> list.all(
          fn(input) {
            when input.output.address.payment_credential is {
              // However, someone needs to pay fees, so verification key credentials are allowed
              VerificationKey(_) -> True
              Script(_) -> or {
                  input.output.address.payment_credential == registry.treasury,
                  input.output.address.payment_credential == registry.vendor,
                }
            }
          },
        )
}
```

**Minswap v2** — `lib/amm_dex_v2/pool_validation.ak:536-575`:

```rs
pub fn has_only_pool_and_author(
  inputs: List<Input>,
  pool_address: Address,
  pool_author: PoolAuthorizationMethod,
) -> Bool {
  let Address { payment_credential: pool_payment_cred, .. } = pool_address
  // Having single pool input
  expect [_] =
    inputs
      |> list.filter(...)
  // All inputs does not contain other scripts except for the Pool and Author 
  inputs |> list.all(...)
}
```

**SundaeSwap v3** uses a *count* rather than a whitelist: every script input other than the
pool is assumed to be an order, and the batch must process exactly that many
(`sundae-contracts/lib/shared.ak:115-159`):

```rs
// We need to count the number of input orders to enforce that every order is "considered"
// however, we don't know the order script address (indeed, there could be many types!);
// and for efficiency, we don't want to do an equality comparison each time anyway
// So, we use a clever trick, and count the number of *script* addresses, and subtract the constant below.
...
const exact_non_order_script_inputs = 1
...
pub fn count_orders(tx_inputs: List<Input>) -> Int {
  when tx_inputs is {
    // Note: by using  -exact_non_order_script_inputs for the base case,
    // it's equivalent to subtracting at the end
    [] -> -exact_non_order_script_inputs
    [input, ..rest] ->
      when input.output.address.payment_credential is {
        Script(_) -> count_orders(rest) + 1
        _ -> count_orders(rest)
      }
  }
}
```

enforced in `validators/pool.ak:132, 221`:

```rs
        let real_order_count = count_orders(inputs)
...
        expect simple_count + strategy_count == real_order_count
```

and reinforced at `lib/calculation/process.ak:409-413`:

```rs
      // It's important that we fail if we ever try to process a UTXO from a wallet address
      // This is a bit unfortunate, because it means we can't support processing orders directly out of a users wallet
      // but is important, because we rely on this to check that every order is processed.
      // If we didn't do this check, a scooper could include a UTXO from their wallet, and leave a *real* order un-processed, and steal those users funds.
      expect is_script(order.address.payment_credential)
```

### Strategy E — one NFT per order

**Genius Yield** mints a unique NFT per order, derived from a spent `OutputReference`, so the
order UTxO is globally unique and its continuation is identifiable.
`geniusyield-dex/dex-v2-contracts/validators/order.ak:41-46, 65-71`:

```rs
      // NB: it is not possible that two NFTs go to the same output because NFT asset name is put into datum
      Some(OutputReferenceInt {
        output_ref: check_spent_oref, // UTXO to ensure uniqueness
        amount: order_count,          // number of tokens to be minted
      }) -> {
        let nft_set = populate_nft_set(check_spent_oref, order_count, dict.empty) // (unordered) set of all NFTs to identify orders
```

```rs
        and {
          gy_transaction.utxo_consumed(self.inputs, check_spent_oref)?,

          (order_count > 0)?,
          (order_count < 256)?,

          (nft_set == new_mint_tokens)?,
```

with the naming scheme at `lib/geniusyield/dex_v2/order.ak:99-106`:

```rs
/// Populate the NFT set corresponding to given output reference & starting count. Note that given `count` should be greater than zero.
pub fn populate_nft_set(
...
  // NFT name scheme: sha256(txId || ix) truncated to 28 bytes,
  // then prefix a 1-byte counter to support multi-mint (<256 enforced).
```

The v1 Plutarch validator keeps the NFT in the datum and burns it on completion
(`geniusyield-onchain/src/GeniusYield/OnChain/DEX/PartialOrder.hs:97-98, 138`):

```haskell
        -- Succeed immediately if there is no NFT.
        pguardC' (pcon PUnit) $ pvalueOf # valueIn # nftSymbol # nftTkName #== 1
...
                pguardC "NFT not burnt" $ nftMintAmt #== -1
```

### Strategy F — positional 1:1 zip of inputs to outputs

**Splash** — the batch witness zips inputs against outputs by position, with a datum "tag"
field discriminating which inputs it owns
(`splash-core/validators_v2/validators/orders/limit_order.ak:72-84, 269-291`):

```rs
validator {
  fn batch_witness(_: Data, ctx: ScriptContext) -> Bool {
    expect WithdrawFrom(_) = ctx.purpose
    let tx = ctx.transaction
    left_biased_zip_validate(
      tx.inputs,
      tx.outputs,
      tx.extra_signatories,
      True,
      validation_step,
    )?
  }
}
```

```rs
fn left_biased_zip_validate(
  lhs: List<a>,
  rhs: List<b>,
  ctx: ctx,
  acc: Bool,
  predicate: fn(a, b, ctx) -> Bool,
) -> Bool {
  when lhs is {
    [] -> acc
    [x, ..xs] ->
      when rhs is {
        [] -> False
        [y, ..ys] ->
          left_biased_zip_validate(xs, ys, ctx, acc && predicate(x, y, ctx), predicate)
      }
  }
}
```

with the per-pair "is this mine?" tag check at `:94-101`:

```rs
  when self.datum is {
    InlineDatum(datum) -> {
      let (_, fields) = builtin.un_constr_data(datum)
      let tag = builtin.head_list(fields)
      let this_tag: Data = this_tag
      if tag == this_tag {
        expect conf: LimitOrderConfig = datum
        let successor = output
```

**Minswap v2** consumes `all_outputs` head-first, one output per order, in the same order as
the (index-sorted) order inputs (`lib/amm_dex_v2/order_validation.ak:1330, 1691-1707`):

```rs
        expect [output, ..rest_outputs] = all_outputs
```

```rs
  when rest_inputs is {
    [] -> new_state
    _ ->
      apply_orders(
        ...
        order_inputs: rest_inputs,
        all_outputs: rest_outs,
        orders_fee: rest_batcher_fees,
        pool_state: new_state,
      )
  }
```

**SundaeSwap v3** does the same, relying on the fact that outputs are *not* reordered by the
ledger while inputs *are* (`lib/calculation/InputSorting.md`):

> Instead, each order corresponds to 1 (or in one particular corner case, 0) outputs. And,
> since the outputs *are not* reordered by the script context, we can safely just recursively
> traverse these in order [...]

**Genius Yield v2** states the same contract as a documented assumption
(`dex-v2-contracts/lib/geniusyield/dex_v2/order.ak:346`):

```rs
/// Case where we are filling multiple orders in a single transaction. We assume that the subsequence of input orders in the list of `Input` correspond to subsequence of the "relevant" outputs in the `Output` list. Thus off-chain transaction builder would need to ensure that order output ordering is in sync with order input ordering.
```

---

## 4. Efficiency tricks worth stealing

### 4.1 Unrolled list skipping (6 protocols)

The single most-copied optimisation. Everyone hand-unrolls `tail_list` because `list.at` is
O(n) with per-step recursion overhead.

**SundaeSwap** skips 15, then 7 (`lib/calculation/shared.ak:17-59`):

```rs
/// Efficiently skip past the first `idx` elements of `inputs`, returning the remaining tail
/// This is a demonic ungodly black magic: we manually un-roll the loops, using builtin.tail_list
/// to skip past many items in a row to avoid the overhead of recursing, subtracting, or doing a bounds check
///
/// The number of items we skip over is chosen carefully based on the number of orders we expect
/// i.e. we want to roughly skip half the orders each time, so we skip 15, then 7, otherwise fall back to simple iteration
pub fn unsafe_fast_index_skip_with_tail(inputs: List<a>, idx: Int) -> List<a> {
  if idx >= 15 {
    unsafe_fast_index_skip_with_tail(
      // This is so wild
      inputs
        |> builtin.tail_list
        |> builtin.tail_list
        ... (15 times)
      idx - 15,
    )
  } else if idx >= 7 { ... } else {
    unsafe_fast_index_with_tail(inputs, idx)
  }
}
```

**Minswap** and **JPG Store** both skip 10 with a shared-looking helper
(`minswap-dex-v2/lib/amm_dex_v2/utils.ak:190-225`, `jpg-store-v3/lib/jpg/utils.ak:23-68`):

```rs
pub fn list_at_index(outputs: List<a>, payout_outputs_offset: Int) -> a {
  if payout_outputs_offset >= 10 {
    outputs
      |> skip_10_items
      |> list_at_index(payout_outputs_offset - 10)
  } else {
    list_at_index_step(outputs, payout_outputs_offset)
  }
}
```

JPG's comment states the payoff:

```rs
/// We use a heuristic function combined with the given 
/// redeemer offset to jump 10 at a time towards first output to validate.
/// This saves a massive amount in budget allowing us to support 40+ inputs.
/// If the wrong offset is given then the script will fail or cost more.
```

**WingRiders** skips 5 (`wingriders-v2/src/Plutarch/Util.hs:812-818`):

```haskell
pelemAtOptimized :: PIsListLike l a => Term s (PInteger :--> l a :--> a)
pelemAtOptimized = phoistAcyclic $
  pfix #$ plam $ \recur n xs ->
    pif
      (n #>= 5)
      (recur # (n - 5) #$ ptail #$ ptail #$ ptail #$ ptail #$ ptail # xs)
      (pif (n #== 0) (phead # xs) (pelemAt # (n - 1) # (ptail # xs)))
```

**CIP-113** skips 2 (`lib/utils.ak:73-81`).

### 4.2 Amortised traversal — never restart the list unless you must

SundaeSwap threads two lists (whole + remaining) so a monotonic index sequence costs O(n) total
(`lib/calculation/process.ak:394-406`):

```rs
      // Then, we identify where to find the inputs; in particular, to avoid "starting from the beginning" every single time
      // when indices are monotonic through the list, we can just continue to advance through the list
      // so, all_inputs will always contain the full list of inputs
      // while remaining_inputs will just contain the ones "after" the last one we processed.
      // So, here, we check if we can continue down this path, or if we need to start from the beginning again
      let next_input_list =
        if idx >= prev_index {
          unsafe_fast_index_skip_with_tail(remaining_inputs, idx - prev_index)
        } else {
          unsafe_fast_index_skip_with_tail(all_inputs, idx)
        }
```

WingRiders does the same in `porder` (`src/DEX/Pool.hs:768-786`), with the same last-index
trick:

```haskell
porder inputs locations =
  go # inputs # pnil # 999 # locations
  where
    go =
      pfix #$ plam \recur all' rest last' locs' ->
        ...
                  (pif (i #< last') (pdrop' # i # all') (pdrop' # (i - last' - 1) # rest))
```

### 4.3 Continuation-passing instead of tuples/records

SundaeSwap states the cost model explicitly (`validators/pool.ak:146-147`):

```rs
        // Note: this abomination is brought to you by the fact that constructing and destructuring structs
        // is expensive, so it's cheaper to have **massive** lambdas / continuations
```

`process_order` takes 22 scalar parameters plus a continuation
(`lib/calculation/process.ak:101-143`), and `pool_input_to_state` returns 12 values through a
continuation (`:25-44`). The same reasoning drives the up-front destructure at
`validators/pool.ak:62-76`:

```rs
    // First, we destructure the transaction right upfront, because field access is O(n),
    // and we want access to these fields with just a single pass over the transaction
    // This will be a common pattern throughout the scripts
    // (in fact, I really want a compiler optimization that detects and lifts repeated field accesses into a destructure)
```

**This is a direct Scalus opportunity**: the comment asks for exactly the compiler optimisation
Scalus could provide (CSE of repeated `Data` field access), removing the need for CPS-by-hand.

### 4.4 Single-pass fold over a `Value` instead of building a `Value`

SundaeSwap checks the whole pool UTxO in one fold and compares tuples
(`validators/pool.ak:618-647`):

```rs
  if pool_policy_a == ada_policy_id {
    let actual =
      list.foldl(
        assets.flatten(output_value),
        // (token count, lovelace amount, token b amount, pool nft amount)
        (0, 0, 0, 0),
        fn(asset, acc) {
          let token_count = acc.1st + 1
          if asset.1st == pool_policy_a {
            (token_count, acc.2nd + asset.3rd, acc.3rd, acc.4th)
          } else if asset.1st == pool_policy_b && asset.2nd == pool_asset_name_b {
            (token_count, acc.2nd, acc.3rd + asset.3rd, acc.4th)
          } else {
            expect asset == (pool_script_hash, pool_nft_name(identifier), 1)
            (token_count, acc.2nd, acc.3rd, acc.4th + 1)
          }
        },
      )
    ...
    // Rather than constructing a value directly (which can be expensive)
    // we can just compare the expected token count and amounts with a single pass over the value
    expected == actual
```

Minswap instead builds the expected `Value` once and compares by equality
(`lib/amm_dex_v2/pool_validation.ak:163-181`):

```rs
  let estimate_pool_in_value =
    value.zero()
      |> value.add(authen_policy_id, utils.pool_auth_asset_name, 1)
      |> value.add(asset_a_policy_id, asset_a_asset_name, value_reserve_a_in)
      |> value.add(asset_b_policy_id, asset_b_asset_name, value_reserve_b_in)
      |> value.add(authen_policy_id, lp_asset_name, remaining_liquidity_supply_in)
      |> value.add(ada_policy_id, ada_asset_name, utils.min_pool_ada)
  ...
  expect and {
      estimate_pool_in_value == pool_in_value,
      estimate_pool_out_value == pool_out_value,
      remaining_liquidity_supply_out - remaining_liquidity_supply_in == pool_in_total_liquidity - pool_out_total_liquidity,
    }
```

Minswap also has a hand-written fast path that avoids `Value` arithmetic entirely for ADA
pairs (`lib/amm_dex_v2/order_validation.ak:33-45`):

```rs
// Ad hoc solution for predicting order output value.
// This function only handles ADA pairs and has strict asset requirements in the value.
fn get_optimized_swap_output_value(
  ...
) -> Option<SortedValueList> {
```

falling back to the general path only when the shape does not match (`:161-201`).

Splash bypasses `Value` lookup entirely by relying on ADA being the first policy and scanning
at most `max_policies` entries (`splash-core/validators_v2/lib/splash/value_ext.ak:5-48`):

```rs
pub fn lovelace_of(val: value.Value) -> Int {
  let pol = val |> value.to_dict |> dict.to_list |> builtin.head_list
  let tok = pol.2nd |> dict.to_list |> builtin.head_list
  tok.2nd
}
...
pub fn quantity_of(
  val: List<(PolicyId, dict.Dict<ByteArray, Int>)>,
  pol: PolicyId,
  tn: ByteArray,
  max_policies: Int,
) -> Int {
  expect max_policies > 0
```

SundaeSwap's settings lookup makes the same structural assumption
(`lib/types/settings.ak:88-101`):

```rs
pub fn find_settings_datum(
  reference_inputs: List<Input>,
  settings_policy_id: PolicyId,
) -> SettingsDatum {
  // Assume the settings input is at the head of the list.
  let settings_output = builtin.head_list(reference_inputs).output
  let settings_value_list = dict.to_pairs(assets.to_dict(settings_output.value))
  expect [_, Pair(found_policy_id, settings_dict)] = settings_value_list
```

### 4.5 Verify a witness instead of computing it

SundaeSwap never computes a square root; it checks one supplied off-chain
(`lib/shared.ak:161-164`):

```rs
// Taken from unmerged PR: https://github.com/aiken-lang/stdlib/pull/73/files
pub fn is_sqrt(self: Int, x: Int) -> Bool {
  x * x <= self && ( x + 1 ) * ( x + 1 ) > self
}
```

used at `validators/pool.ak` in `CreatePool`:

```rs
        expect
          shared.is_sqrt(coin_a_amt_sans_protocol_fees * coin_b_amt, initial_lq)
```

The same style shows in `swap_takes`, which proves optimality by checking "one unit less would
give strictly less" instead of searching (`lib/calculation/swap.ak:98-114`):

```rs
  // We need to make sure that the user is getting the most efficient swap
  // So we check what they would receive with one less unit of the token they're giving
  ...
  let one_less =
    ( give_takes_numerator - pool_take_times_difference ) / (
      give_takes_denominator - difference
    )
  // And that *must* give strictly less takes; this means that the user is getting the most efficient order possible
  expect one_less < give_takes
```

Splash/Spectrum use the same "relaxed output" trick to avoid exact division
(`splash-core/plutarch-validators/WhalePoolsDex/PContracts/PSwap.hs:190-198`):

```haskell
validPrice =
    plam $ \quoteAmount poolValue treasuryX treasuryY poolX base quote baseAmount feeNum ->
        let relaxedOut    = quoteAmount + 1
            ...
            correctOut    = pdiv # (reservesQuote * baseAmount * feeNum) # (reservesBase * feeDen + baseAmount * feeNum)
         in correctOut #<= relaxedOut
```

and the AMM invariant is checked as a cross-multiplied inequality, never as `x*y >= k`
(`WhalePoolsDex/PContracts/PPool.hs:372-379`):

```haskell
                                dxf = dx * (feeNum - tFeeNum)
                                dyf = dy * (feeNum - tFeeNum)

                                validSwap =
                                    pif
                                        (zero #< dx)
                                        (-dy * (rx0 * feeDen' + dxf) #<= ry0 * dxf)
                                        (-dx * (ry0 * feeDen' + dyf) #<= rx0 * dyf)
```

### 4.6 Withdraw-zero: run heavy logic once, not per input

SundaeSwap's order validator is 69 lines total because all real logic lives in a stake script
(`sundae-contracts/validators/order.ak:22-27, 49-62`):

```rs
/// - Scoop: the order is processed as a batch against a pool.
///   Instead of re-checking the conditions in every single script, we instead use a clever trick
///   We look for the stake_script_hash in the list of withdrawals!
///   This means that the stake script must run, and it can always run, because withdrawals of 0 tokens are allowed!
///   That stake_script then checks that a pool NFT is present on the UTXO, where most of the subtle and important logic is implemented.
```

```rs
      Scoop ->
        pairs.foldl(
          transaction.withdrawals,
          False,
          fn(withdrawal, _amt, acc) {
            when withdrawal is {
              // TODO: we could make this more efficient by CBOR encoding the `Script` into the `stake_script_hash` parameter
              // or perhaps even the whole withdrawal list to compare it all at once!
              // and just doing a direct equaltiy comparison. Since this runs for each order, this could represent significant savings!
              Script(script) -> acc || script == stake_script_hash
              _ -> acc
            }
          },
        )
```

and the stake script itself (`validators/stake.ak:9-43`) does the one-time pool-NFT check,
identifying the NFT by its CIP-68 label byte rather than a name comparison:

```rs
        let is_pool_nft =
          fn(
            ...
            kvp: Pair<ByteArray, Int>,
          ) {
            // we use the 2nd byte here, because the first byte is zero as a "parenthesis"
            builtin.index_bytearray(kvp.1st, 1) == 0xd
          }
```

Minswap uses the identical shape — the order script is a one-line withdrawal check
(`minswap-dex-v2/validators/order_validator.ak:25-29`):

```rs
      ApplyOrder -> {
        let Transaction { withdrawals, .. } = transaction
        // validate that an Order can be spent if there's a `Order Batching` validator in the `withdrawals`
        dict.has_key(withdrawals, pool_batching_credential)
      }
```

and so do Splash (`limit_order.ak:56-57`: `True -> dict.has_key(tx.withdrawals, witness)?`) and
Genius Yield v2 (`dex-v2-contracts/validators/order.ak:102-105`):

```rs
    when redeemer is {
      CancelOrder -> pairs.has_key(withdrawals, cancel_credential)
      FillOrder -> pairs.has_key(withdrawals, fill_credential)
    }
```

`aiken-design-patterns` packages this as `stake_validator.validate_withdraw*`
(`lib/aiken-design-patterns/stake-validator.ak:27-77`).

### 4.7 Delegating to the pool by pointing at its input index

WingRiders' request validator is *just* a proof that the pool script will run
(`wingriders-v2/src/DEX/Request.hs:61-77`):

```haskell
{- |
  Apply a request against a liquidity pool.

  Safety: We have to verify that the transaction contains a pool validator,
          which takes over responsibility of checking request outputs.
  We use a following proxy check to attestate that `validatePool` will be run
  - `poolIdx` should contain a hint of which input is the pool input
  - we verify this input has the correct validator hash
  - therefore the transaction has to run a pool evolve or one of the other pool paths
  - only the pool evolve path allows any requests to be present

   Note that this logic also delegates checking of deadlines, correct datum contents to the pool due to efficiency.
-}
pvalidateApply :: Term s (PScriptHash :--> PInteger :--> PScriptContext :--> PBool)
pvalidateApply = plam $ \poolHash poolIdx scriptContext ->
  let delegateeValidatorHash = pextractNthInputValidatorHash # scriptContext # poolIdx
   in ptraceIfFalse "raL" (delegateeValidatorHash #== poolHash)
```

---

## 5. Per-protocol helper inventories (the "no stdlib has this" evidence)

### 5.1 WingRiders v2 — `src/Plutarch/Util.hs` (818 lines, ~70 helpers)

The single largest hand-rolled standard library in the corpus. Selected signatures, all read
from the file:

| Function | Line | Purpose |
|---|---|---|
| `passertSingleton :: PIsListLike list a => Text -> Term s (list a :--> a)` | 261 | Exactly-one-element extraction with a custom error |
| `passertDoubleton` | 265 | Exactly-two-element extraction |
| `passertSingleSpecificInput` | 275 | Exactly one input at script hash holding `(cs, tn) == 1` |
| `pvalueOfInputs :: (PCurrencySymbol :--> PTokenName :--> PBuiltinList PTxInInfo :--> PInteger)` | 290 | Sum an asset across all inputs |
| `phashDatum :: PIsData a => Term s (a :--> PDatumHash)` | 295 | `blake2b_256 . serialiseData . pforgetData . pdata` |
| `pgetInput` | 309 | Resolve `PTxOutRef` to `PTxOut` |
| `pcountAllScriptInputs` | 376 | Count script-credential inputs |
| `pcountScriptInputs` / `pcountScriptOutputs` | 388 / 401 | Count by script hash |
| `ptryUniqueScriptTxOut` / `ptryUniqueScriptTxInInfo` | 414 / 424 | Single continuing output/input at a script |
| `ptxOutHasAssociatedToken` | 432 | Output holds one token whose name == its own validator hash |
| `pfindScriptOutputs` / `pfindScriptOutputsWithAddress` | 436 / 454 | Collect (value, datum) at a script |
| `plowerBoundCurrentTimeApproximation` | 478 | Validity-range lower bound as "now" |
| `ptryLookup` | 490 | `PMap` lookup or error |
| `pforallValue` / `pvalueIsSubsetOf` | 544 / 555 | Per-asset predicate over a `Value` |
| `pextractNthInputValidatorHash` / `pextractNthTxInput` | 570 / 579 | Redeemer-index resolution |
| `pisTokenSpent` | 589 | Any input holds ≥1 of an asset |
| `pmustFindOwnInput` | 607 | Own-input lookup |
| `phaveSameStakingCredentials` | 639 | Stake-credential preservation |
| `pfiniteTxValidityRangeTimestamps` | 655 | Finite bounds or error |
| `pvalueOfWithOilCheck` | 670 | Asset quantity net of min-ADA "oil", with guard |
| `pisTxValidityRangeShortEnough` | 712 | Bound the validity window |
| `ptryTxOutRefRedeemer` | 726 | Redeemer for a given spending `TxOutRef` |
| `pfoldInputsWithTwoOutputs` | 786 | Index-driven batch fold over (input, output, next-output) |
| `pelemAtOptimized` | 812 | Unrolled indexing |

`src/Plutus/Util.hs:41-42` even re-implements ceiling division:

```haskell
-- | NOTE: doesn't work for negative numbers because of the mod
divideCeil :: Integer -> Integer -> Integer
divideCeil a b = div a b + if (mod a b > 0) then 1 else 0
```

### 5.2 Midgard — `lib/midgard/common/utils.ak` (411 lines)

Notable because it is an L2, not a DEX, yet it re-implements the same set — and several
functions are named after their Plutarch originals (`plutarch_phas:349`,
`plutarch_pexcludes:381`), showing direct cross-ecosystem copying.

| Function | Line | Purpose |
|---|---|---|
| `is_hereafter` / `is_herebefore` | 20 / 29 | Interval comparisons |
| `validate_mint` | 37 | Exact mint check |
| `safe_list_last` / `safe_list_head` / `safe_list_init` | 47 / 52 / 57 | Failing list accessors |
| `quantity_of_policy_id` | 67 | Sum all tokens under a policy |
| `get_own_hash` / `get_own_address` | 71 / 79 | Own script hash/address from the spent input |
| `get_own_input_at` | 87 | Redeemer-index own input (Strategy A) |
| `has_signed` | 107 | Signature check |
| `get_single_asset_from_value` | 116 | Exactly one asset |
| `get_single_asset_from_value_apart_from_ada` | 127 | Exactly ADA + one asset |
| `get_authentic_input_with_policy_at` | 141 | Beacon input by policy + index |
| `get_authentic_input_with_nft_at` | 159 | Beacon input by exact NFT + index |
| `get_authentic_input_of` | 179 | Beacon whose policy == its own script hash |
| `verify_hash_32` / `verify_hash_28` | 233 / 238 | `blake2b(cbor.serialise(data)) == h` |
| `authentic_value_has_tokens` | 248 | `Value` matches a datum-stored `ValuePairs` plus NFT |
| `zip_foldr` / `zip_foldl` | 264 / 284 | Parallel two-list folds |
| `compare_authentic_value_with_target` | 309 | Per-quantity predicate against a datum target |
| `get_redeemer_at` | 338 | Redeemer by purpose + index |

### 5.3 SundaeSwap v3 — `lib/shared.ak` + `lib/calculation/shared.ak`

| Function | File:line | Purpose |
|---|---|---|
| `datum_of(datums, output) -> Option<Data>` | `lib/shared.ak:43` | Inline-or-hash datum resolution |
| `spent_output(tx, out_ref) -> Output` | `lib/shared.ak:56` | Own input |
| `own_input_index(tx, out_ref) -> Int` | `lib/shared.ak:65` | Own input *index* (for cross-script redeemer matching) |
| `get_input_with_token(inputs, idx, token)` | `lib/shared.ak:88` | Indexed input holding a token |
| `count_orders(tx_inputs) -> Int` | `lib/shared.ak:144` | Count script inputs minus known non-orders |
| `is_sqrt(self, x) -> Bool` | `lib/shared.ak:162` | Verify an off-chain sqrt |
| `has_exact_token_count(val, count)` | `lib/shared.ak:177` | Asset-count bound on a `Value` |
| `pool_ref_name` / `pool_nft_name` / `pool_lp_name` | `lib/shared.ak:217/222/227` | CIP-68 label prefixes |
| `fees_in_legal_range(fees)` | `lib/shared.ak:309` | Basis-point range check |
| `unsafe_fast_index_skip_with_tail` | `lib/calculation/shared.ak:23` | Unrolled skip |
| `check_and_set_unique(flags, index) -> Int` | `lib/calculation/shared.ak:74` | Bitmask duplicate detection |
| `small_pow2(exponent) -> Int` | `lib/calculation/shared.ak:127` | Table-driven `2^n` |
| `contains_interval(outer, inner)` | `lib/calculation/strategy.ak:14` | Interval containment (for signed strategies) |

**Oracle validator** (`validators/oracle.ak`, 350 lines; only the head was read). It is a
price-snapshot SFT rather than a feed. Two points worth carrying into a stdlib design
(`validators/oracle.ak:16-31, 48-57`):

```rs
// The oracle script holds an oracle token, and a snapshot of the pool price at the *end* of some scoop.
// This allows other protocols to build integrations that read the pool price (for some confidence interval) without worrying about contention
...
// It's important to use the price at the *end* of the scoop, or at the beginning, rather than just using the price
// "at the time" the order was processed. If we expose the pool price mid-stream, then it is easy to sandwich the order between two others.
```

```rs
      list.all(
        transaction.outputs,
        fn(output) {
          assets.quantity_of(
            output.value,
            own_script_hash,
            shared.oracle_sft_name(),
          ) == 0
        },
      ),
```

The second block is a reusable primitive: "no output anywhere carries this token" (a burn
assertion expressed over outputs rather than over `mint`).

`lib/calculation/strategy.ak:72-104` is also the only signed-message ("oracle-ish") pattern in
the corpus, and flags the serialisation risk itself:

```rs
      // And finally, use cbor.serialise and check that the signature is valid
      // TODO: is this at risk if cbor.serialise changes? is there a way for us to get the raw bytes of the data?
      // NOTE: it's really important that the signature is for the *whole execution* here; otherwise
      // you could replay that signature over some other strategy order with the same signing key
      let strategy_bytes = cbor.serialise(execution)
      expect Some(signature) = signature
      expect verify_ed25519_signature(signer, strategy_bytes, signature)
```

### 5.4 Minswap v2 — `lib/amm_dex_v2/utils.ak`

| Function | Line | Purpose |
|---|---|---|
| `sorted_asset(a, b) -> Bool` | 65 | Canonical pair ordering |
| `is_ada_asset(pid, name)` | 91 | ADA test |
| `compute_lp_asset_name(...)` | 95 | Deterministic LP token name (`sha3_256` twice) |
| `must_find_script_inline_datum` / `must_find_script_datum` | 125 / 132 | Failing datum resolution |
| `dict_must_get` | 145 | Failing dict lookup ("rewrite dict.get / `None -> fail`") |
| `list_at_index` / `skip_10_items` | 190 / 213 | Unrolled indexing |
| `zip_with(a, b, c, f)` | 245 | Three-list zip with length equality |
| `compare_list_length(a, b)` | 265 | Length equality without computing lengths |
| `is_unique_bytearray_unsorted` | 405 | Duplicate detection over a byte set |
| `must_get_current_time_approximation` | 482 | Midpoint of validity range, window ≤ 10 min |
| `must_get_finite_start_validity` | 496 | Finite lower bound |
| `value_to_list` | 505 | `Value` → nested assoc list |
| `authorize_pool_license` / `authorize_order_license` | 518 / 550 | Signature \| spend-script \| withdraw-script \| mint-script |

### 5.5 Splash / Spectrum — `PExtra/*` and `splash/*`

`PExtra` is an explicit "things Plutarch does not give us" module:
`API.hs` (`assetClass:101`, `assetClassValue:110`, `assetClassValueOf:119`,
`getContinuingOutputs:128`, `pfindOwnInput:149`, `findOwnInput:159`, `mustPayToPubKey:182`,
`outputPaysTo:193`, `ptryFromData:213`, `pValueLength:217`), plus `List.hs` (`psort`,
`preverse`, `pexists`, `pelemAt`, and a full merge-sort *and* tim-sort implementation,
lines 14-129), `Ada.hs`, `Integer.hs` (`podd`, `peven`, `ppow`, `pexp'`, `psquare`),
`Maybe.hs`, `Pair.hs`, `PTriple.hs`, `Monadic.hs`.

`WhalePoolsDex/PContracts/PApi.hs` adds the protocol-level ones: `containsSignature:70`,
`getRewardValue':74`, `getStakeHash:89`, `getInputValue:106`, `checkPoolNft:113`,
`checkInputsQty:122`, `ownCurrencySymbol:128`, `pmin:67`.

Their Aiken port re-does the same in `validators_v2/lib/splash/`: `value_ext.ak`
(`lovelace_of:5`, `lovelace_of_tail:11`, `quantity_of:19`), `plutus.ak` (`Asset`, `mk_asset`,
`DatumHashMap`), `rational.ak`.

### 5.6 Genius Yield DEX v2 — `lib/geniusyield/dex_v2/`

`transaction.ak`: `utxo_consumed:17`, `find_marked_ref_input:25`.
`order.ak`: `oracle_timestamp_valid:45`, `net_from_gross:54`, `ceil_mul_ratio:69`,
`floor_mul_ratio:85`, `populate_nft_set:100`, `verify_outputs:139`, `verify_output:194`,
`comp_percent_maker_fee:246`, `is_after:305`, `is_before:318` (both marked "Code is similar to
standard library's `is_entirely_after`, but includes the given point as well"),
`order_can_be_filled:330`, `traverse_fill_inputs_outputs:347`.
Plus `types/{rational,sorted_list,multisig,order,assets}.ak` and `option.ak`, `address.ak`.

### 5.7 JPG Store v3 — `lib/jpg/utils.ak`

`find_payout_outputs:23`, `skip_10_items:56`, `check_marketplace_payout:81`,
`check_payouts:198`, `check_payouts_aux:242`. Only five helpers, but four of the five exist
purely to work around missing stdlib primitives (indexed drop, `Value` shape assertion,
positional payout matching).

Note the hand-inlined fee approximation in `validators/ask.ak:74-76`, chosen because exact
arithmetic was too expensive:

```rs
          // This approximates the marketplace fee given only the payouts to a very high degree.
          // For a payouts in excess of 100k ada the error is less than 40000 lovelace.
          let marketplace_fee = payouts_sum * 50 / 49 / 50
```

### 5.8 cardano-swaps — `lib/cardano_swaps/`

`common/utils.ak`: `trace_if_false:17`, `error_if_false:26`, `staking_credential_approves:35`,
`compare_asset_config:59`, `has_beacon_script_minting_execution:69`,
`has_beacon_script_staking_execution:76`.
`one_way_swap/utils.ak`: `parse_datum:28`, `get_upper_bound:43`, `generate_pair_beacon:54`,
`generate_offer_beacon:71`, `generate_ask_beacon:83`, `extract_ask_and_offer_quantity:95`,
`valid_swap_output:232`, `beacon_destination_check:324`, `swap_output_value:393`,
`valid_swap:472`. The two-way variant duplicates all of it (`two_way_swap/utils.ak`, 591 lines
vs 574) — a strong argument for a parameterised stdlib helper.

`staking_credential_approves` is a good candidate for a stdlib primitive
(`common/utils.ak:35-57`):

```rs
pub fn staking_credential_approves(
  swap_addr: Address, 
  withdrawals: Dict<StakeCredential, Int>,
  extra_signatories: List<Hash<Blake2b_224, VerificationKey>>
) -> Bool {
  let Address(_,staking_cred) = swap_addr
  when staking_cred is {
    None -> True
    Some(Inline(VerificationKeyCredential(skey))) -> {
      list.has(extra_signatories,skey)
    }
    Some(svh) -> {
      dict.has_key(withdrawals,svh)
    }
  }
}
```

### 5.9 Intersect treasury — `lib/utilities.ak`

`find_script_hash_registry:13`, `ensure_compliant_scripts:35`, `ensure_no_ref_scripts:57`,
`outputs_of:65`, `equal_plus_min_ada:72`, `greater_than_or_equal_to:80`, `value_sum:94`,
`payout_sum:119`, `interval_length_at_most:167`.

`equal_plus_min_ada` (`:69-77`) is the cleanest statement of the min-ADA tolerance problem
that every protocol hits:

```rs
// Check that the assets are identical, but the lovelace of `actual` is *at least* as much as `expected`
// For example, if comparing inputs to outputs, this allows the ADA to increase if it needs to to cover minUTxO
// but doesn't allow it to decrease, and doesn't allow arbitrary other tokens to be added
pub fn equal_plus_min_ada(expected: Value, actual: Value) -> Bool {
  and {
    without_lovelace(expected) == without_lovelace(actual),
    lovelace_of(expected) <= lovelace_of(actual),
  }
}
```

`ensure_no_ref_scripts` (`:55-61`) records a fee-based DoS the stdlib should help with:

```rs
// Because reference scripts have an exponential (if small) effect on the fee,
// we disallow reference scripts on the outputs, to prevent someone from being annoying
```

and `payout_sum` (`:140-154`) shows why bounded sizes matter:

```rs
                    // Benchmarking shows that if we allow more than 4 tokens in the value, we can exceed execution units
                    // on the sweep / withdraw operations quickly, leaving funds locked forever;
```

### 5.10 CIP-113 programmable tokens — `lib/utils.ak`

`expect_inline_datum:17`, `find_input:23`, `must_find_input:31`, `has_currency_symbol:42`,
`count_unique_tokens:60`, `has_signatory:65`, `elem_at:73`, `bytearray_lt:84`,
`apply_hashed_parameter:91`, `is_programmable_token_registration:117`, `has_key:142`,
`sum_output_values:168`, `merge_raw_values:203`, `match_assets:214`, `value_contains:241`.

`has_currency_symbol:42-46` shows another structural assumption everyone makes:

```rs
/// Check if a currency symbol is present in a value. Fail loudly if missing
pub fn has_currency_symbol(haystack: Value, needle: PolicyId) -> Bool {
  // Fast skip first pair which is always ada
  let haystack = haystack |> assets.to_dict |> dict.to_pairs |> tail_list
  do_has_currency_symbol(haystack, needle)
}
```

---

## 6. Concrete recommendations for the Scalus stdlib

Ordered by (protocol count × severity of getting it wrong).

1. **Ship a UTxO-indexer API with the own-ref assertion built in.** Nine of twelve protocols
   hand-roll it, and the assertion `ownRef == inputs[ix].outRef` is the part that is easy to
   forget. Follow `aiken-design-patterns`' lead and make the double-satisfaction gap explicit
   in the signature or docs, since the library cannot close it.

2. **Ship all six double-satisfaction strategies as named, documented combinators**, not one
   blessed one. `Tag.ofOwnRef(ownRef)`, `Batch.uniqueIndices`, `Tx.onlyScripts(allowed)`,
   `Tx.exactInputCount(n)`, `Batch.zipInOut`, `Nft.perOrder`. Naming them is half the value:
   Spectrum's comment `-- ensure double satisfaction attack is not possible` and Intersect's
   `ensure_compliant_scripts` show that authors already think in these terms.

3. **Make indexed list access fast in the compiler, not in user code.** Six protocols
   hand-unroll `tail_list` by 15/10/5/2. A Scalus `List.drop`/`List.at` that the SIR lowering
   unrolls would delete this entire category of code and its off-by-one risk.

4. **Do CSE on repeated `Data` field access.** SundaeSwap's own comment asks for it
   (`validators/pool.ak:65`: *"I really want a compiler optimization that detects and lifts
   repeated field accesses into a destructure"*), and 22-parameter continuation-passing
   functions exist only because it is missing. This is a Scalus differentiator, not a library
   feature.

5. **`Value` API: add shape assertions, not just arithmetic.** `assetCount`,
   `singleAssetApartFromAda`, `hasExactly(expected)`, `equalPlusMinAda`, `quantityOfNetOfOil`.
   Five protocols check "nothing else is on this UTxO" and each writes its own fold.

6. **First-class withdraw-zero support.** Six protocols use it; a `stakeValidator { }` builder
   plus `tx.withdrawalPresent(hash)` (and the pre-encoded-`Credential` comparison SundaeSwap
   asks for at `validators/order.ak:55-57`) covers all observed uses.

7. **Reference-input-by-NFT lookup.** Six protocols implement "find the settings/config/registry
   UTxO", and three of them cut corners (`builtin.head_list(reference_inputs)`) for cost. A
   fast, correct primitive removes the temptation.

8. **Validity-range helpers with the safety rails.** Finite-bounds extraction, midpoint "now"
   with a maximum window (Minswap caps at 10 minutes), interval containment, and the
   `NormalizedTimeRange` shape from `aiken-design-patterns/validity-range-normalization.ak`.

9. **An `Authorization` ADT** covering signature / spend-script / withdraw-script / mint-script,
   satisfied against a `Transaction`. Minswap, Sundae (`sundae/multisig`), Intersect and Genius
   Yield each ship their own.

10. **Document "verify, don't compute" as a first-class pattern**, with `Math.isSqrt` and a
    worked "relaxed inequality instead of exact division" example. It is the highest-leverage
    budget technique in the corpus and it is invisible to anyone reading the AMM maths.
