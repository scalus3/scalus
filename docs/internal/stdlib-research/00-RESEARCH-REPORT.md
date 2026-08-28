# 00 — Master research report: the Scalus on-chain stdlib API

**Status:** synthesis of the eight research studies in this directory. 2026-08-26.
**Inputs:** `01-scalus-examples.md`, `02-scalus-existing-api.md`, `03-aiken-stdlib.md`,
`04-protocols-dex.md`, `05-protocols-other.md`, `06-pitfalls.md`,
`07-efficiency-constraints.md`, `08-scalus-in-the-wild.md`.

**Fixed design frame (owner decisions, not re-litigated here):**

- The API extends the existing `extension (self: TxInfo)` surface in
  `scalus.cardano.onchain.plutus.v3` (`Contexts.scala`), plus extensions on `TxOut`,
  `Value`, `Interval`, `List`, and new small types in that package.
- Naming: `findX(...): Option[A]` and `inline findXOrFail(..., inline message: String = "..."): A`.
  Fail-fast is the PRIMARY form; `Option` variants only where callers genuinely branch.
- PlutusV3 only (PV11 default target).

**Evidence-count convention.** Counts are *distinct codebases hand-rolling the operation*,
drawn only from what the source reports state; corpora are: 35 in-repo validators (01),
12 DEX protocols (04), 15 non-DEX protocols + 7 libraries (05), 6 independent Scalus
projects in the wild (08), Aiken library demand measurements (03). Library packagings
(vodka, aiken-design-patterns, liqwid) are cited as demand evidence but not counted as
"hand-rolls" unless the source report counted them. Pitfall IDs are from 06 §2; cost
rules `Rn`/`In`/`§n` are from 07.

---

## Part 1 — Master evidence table

Ranked by evidence count × pitfall severity (severity from 06 §2 scores). Verdict
policy: **CORE** = ships in the first cut of the high-level surface (34 operations);
**EXTENDED** = second wave or separate module, with reason; **REJECT** = not shipped,
with reason.

Naming note applying to the whole table: obligations are `requireX(...): Unit` (or
return the checked value, precedent `findOwnInputOrFail`); finders are
`findX: Option` / `findXOrFail`. Indexed accessors (`inputAt`, `outputAtWithToken`)
follow the prelude `List.at` fail-fast precedent and deliberately have **no** Option
twin — a mismatched index has no legitimate None-branch (see Part 2(f)).

| # | Operation | Evidence (codebases) | Reports | Pitfalls neutralized | Cost basis | Verdict |
|---|---|---|---|---|---|---|
| 1 | `tx.findContinuingOutputOrFail(ownInput: TxInInfo): TxOut` — **exactly one** output at the **full** input address; `findContinuingOutput: Option[TxOut]` for the at-most-one case | **27+**: 14 in-repo, 6 spellings (01 §P05); 8 DEX (04 §2 r3: Sundae, Minswap, WingRiders, Splash, Spectrum, cardano-swaps, treasury, Bodega); 12 protocols "output-to-self" (05 §7); 5 wild (08 §4 r4: B,C,H,V,starter) | 01,04,05,06,08 | VP-3 (s5), AU-4 (s3), DS-1 partial, RS-3 | single tail-rec scan, no filter list, no Option in OrFail form (07 R5,R6,I1); full-`Address` compare = one `equalsData` (I7) | **CORE** |
| 2 | `tx.requireContinuing(ownInput, value: Value, datum: D): TxOut` — fused: #1 + whole-`Value` `===` + whole-datum inline `===` | fusion of rows 1,7,8; 01 §6 ranks the fused form #2 ("the four checks are always written together and one is usually weakened"); 06 §3 `continuing(...)` | 01,05,06 | VP-1 (s5), VP-2 (s5), VP-3, DT-1 (s4), DT-3, AU-4 | 2 × `equalsData` + 1 address compare on top of row 1 (07 I7, §4.4) | **CORE** |
| 3 | `tx.requireSignedBy(pkh)`; row includes keeping `isSignedBy` | **26**: 20 in-repo, 5 spellings (01 §P01); 6 wild, 4 hand-rolled (08 §4 r10); vodka `key_signed` = 37 downstream calls (03 §6.2) | 01,02,03,08 | AU-2 partial (s4); kills `signatories.head` misuse (01 P01 hazard; 08 §5 #2) | `contains` is intrinsic `equalsData` scan, no Option, no Eq closure (07 I2) | **CORE** |
| 4 | `tx.requireMintExactly(policy, expected: SortedMap[TokenName, BigInt])` + sugar `requireMintsOne(policy, name)`, `requireBurnsAll(policy, name, qty > 0)` | **18+**: 9 in-repo + 5 idioms (01 §P10, 02 §C.13); 9 protocols exact-mint (05 §7); 4 wild (08 §4 r18); vodka's 4 mint fns (03 §4.1) | 01,02,03,05,06,08 | **MI-1 (s5, the Minswap ~$195M incident, 06 §5)**, MI-3 (s4) | `mint.tokens(p)` + one `equalsData`; `hasOnly` measured ~35% cheaper than map compare (07 §2.5, CHANGELOG:23) | **CORE** |
| 5 | `List.findUniqueOrFail(p, msg): A` / `findUnique(p): Option[A]` — single pass: find first match, keep scanning to prove no second | **~10 codebases / ~55 sites**: 4 wild ~40 sites (08 §4 r1); 4 in-repo idioms 15+ sites (02 §C.10); WingRiders `passertSingleton`, Liqwid only-one distinction, Binocular "filter-then-match NOT find" rule (05 §8.1) | 01,02,05,08 | AU-1 support, DS support; kills the silent-second-match `find` bug (08 §2.3 binocular :304-306) | 1 traversal, 0 allocations vs `filter.length` 2 traversals + k `mkCons` (07 I1, R8) | **CORE** |
| 6 | `tx.findInputWithTokenOrFail(policy, name): TxInInfo` and `tx.findReferenceInputWithTokenOrFail(policy, name): TxInInfo` — **exactly-one** semantics | **13 protocols** auth-nft (05 §7 top row); 11 in-repo (01 §P15); 6 DEX config-NFT (04 §2 r4); 3 wild ×12 sites (08 §4 r7) | 01,04,05,06,08 | **AU-1 (s5)**, AU-5 | `quantityOf` = `lookupCoin` builtin, 13x cheaper than a map walk (07 §2.5); uniqueness scan per row 5 | **CORE** |
| 7 | `value.requireEquals(expected)` is just `===` (no new op) — the shipped op is `Value.equalPlusMinAda(expected, actual): Boolean` + `requireEqualPlusMinAda` — tokens exactly equal, ADA of `actual` ≥ | **10 protocols** min-ADA family (05 §7); treasury `equal_plus_min_ada` verbatim (05 §2.2); binocular tokens-exact/ADA-monotone ×2 (08 §2.3); 5 DEX oil/min-ada (04 §2 r10) | 04,05,08 | VP-1, VP-2, **VP-6 (min-ADA griefing)** | `withoutLovelace` = `insertCoin` builtin + one `equalsData`; `getLovelace` = `lookupCoin` (07 §2.3) | **CORE** |
| 8 | `out.requireInlineDatum[T: ToData](expected: T)` — whole-datum equality, and `out.inlineDatumOrFail[T]` / `out.inlineDatum[T]: Option[T]` (TxOut-level accessor) | equality: 12 in-repo (01 §P08), 4 wild ×13 sites (08 §4 r5), 9 protocols (05 §7); accessor: 18 in-repo (01 §P04), 5 wild ×20 sites (08 §4 r2) | 01,05,06,08 | DT-1 (s4), DT-3; whole-datum compare also rejects wrong-tag/arity junk that field reads accept (08 §2.3 binocular CTM-27) | one `equalsData` (07 I7); accessor = existing `inlineOrFail` re-hung on TxOut | **CORE** |
| 9 | `tx.inputAt(idx: BigInt, expectedRef: TxOutRef): TxInInfo` — index + binding check fused | **15 sites in-repo** (02 §C.8 "strongest single stdlib candidate"); 8 DEX strategy-A (04 §3A); 10 protocols (05 §7); Midgard names it `get_own_input_at` (04 §3A) | 01,02,04,05,06 | AU-3, IX-3, DS-5 soundness half | PV11 `at` = `dropList`+`nullList`+`headList`, constant (07 §3.3, R12); one `equalsData` for the binding | **CORE** |
| 10 | `tx.requireAfter(deadline)` / `tx.requireBefore(deadline)` — obligation forms of `isEntirelyAfter/Before`, fail closed on unbounded | 14 in-repo, 6 time primitives (01 §P09); 12 protocols (05 §7); 5 wild (08 §4 r11); vodka `valid_after/before` (03 §4.1) | 01,03,05,06,08 | **TI-1 (s5)** — the `getValidityStartTime`-returns-0 live footgun (06 §TI-1; Vault + TwoPartyEscrow bugs, 01 §P09) | interval predicate, no scalar projection; O(1) | **CORE** |
| 11 | `tx.requireSingleOwnInput(ownInput: TxInInfo)` — count of inputs at ownInput's payment credential === 1 | 5 in-repo, needed in ~20 (01 §P18, `UnfixedAuction` is the exploit demo); 11 protocols single-script-input (05 §7); Spectrum/Splash `checkInputsQty` (04 §3D); 2 wild (08 §4 r17) | 01,04,05,06,08 | **DS-1 (s5, rank-1 pitfall)** — countermeasure C1; scaladoc must state it is instance-scoped (DS-2 caveat, 06) | one `foldLeft` count over inputs, no allocation (07 I1) | **CORE** |
| 12 | `ownRef.asDatumTag: OutputDatum` (blake2b_256 ∘ serialiseData, inline) + `tx.requirePaidTagged(addr: Address, value, tag)` (Credential overload with reward-redirect caveat, see Part 2(c)) | 4 protocols DS-4 (05 §7.1: JPG, cardano-swaps, Midgard, Binocular); JPG v3 quoted (04 §3C); Helios/Aiken document it as the pattern (06 DS-1 prior art) | 04,05,06 | **DS-1/DS-2** — the only payout mechanism safe under batching and across instances (06 DS-2); `Address`-first per AU-4 fix #3 | 1 `serialiseData` + 1 `blake2b_256` + 1 datum `equalsData` per payout | **CORE** |
| 13 | `tx.ownScriptHash(ownRef): ScriptHash` — own input's script credential or fail | 10 in-repo, 3 spellings (01 §P20; the 9-line destructure ×6, 02 §C.12); 3 wild ×10 sites (08 §4 r3); Midgard `get_own_hash` (04 §5.2) | 01,02,04,08 | AU-3 | `findOwnInputOrFail` + `scriptOption.getOrFail`; no new traversal | **CORE** |
| 14 | `tx.requireSpends(ref: TxOutRef)` — one-shot seed consumed | 6 in-repo (01 §P13); 8 protocols one-shot-mint (05 §7); 3 wild (08 §4 r13); Hydra, Lenfi `check_uniqueness` (05 §2.6, §4b.1) | 01,05,06,08 | **MI-2 (s5)** — EditableNft critical finding (seed never bound) | direct tail recursion with `equalsData` per element — spec: NOT `exists` (07 I2 Option tax, R5) | **CORE** |
| 15 | `ref.deriveTokenName: TokenName` (blake2b_256 ∘ serialiseData of a TxOutRef) — shared on-/off-chain | 2 in-repo (01 §P14); Lenfi `id_from_utxo`, GY `populate_nft_set`, cip113 (04 §2 r20 = 3); treasury oneshot (05 §2.6) | 01,04,05,06 | MI-2 (canonical derivation, on/off-chain agree) | 2 builtins | **CORE** |
| 16 | `tx.valuePaidTo(addr: Address): Value` and `tx.valueSpentFrom(addr: Address): Value` — full multi-asset sums, full-address match; `Credential` overloads for pkh-only datums, scaladoc'd staking-redirect-unsafe (see Part 2(c)) | 8+6 in-repo (01 §P16, §P17, 8 hand-rolled folds §C.14); vodka `get_all_value_to` = 38 calls, #2 helper (03 §6.2); 9 protocols (05 §7); 2 wild (08 §4 r16); treasury `value_sum` with its explicit stake policy flag (05 §2.3) | 01,02,03,05,08 | VP-2 (full Value, not lovelace — existing `getAdaFrom*` are the ADA-only trap, 01 §P17); AU-4 payout variant (06 fix #3: payout helpers take `Address`); scaladoc carries the DS-1 warning (06 prior-art `valuePaidTo`) | one fold, `unionValue` builtin per matching element (07 §2.3) | **CORE** |
| 17 | `tx.requireSignedByAll(keys)` / `tx.requireSignedByAny(keys)` | Betting `\|\|` chain, TwoPartyEscrow loop (01 §P01); hydrozoa hand-rolled multisig ×2 (08 §2.1); vodka `all_key_signed`/`one_of_keys_signed` 11+8 calls (03 §6.2) | 01,03,08 | AU-2 partial | one pass over signatories per key or one pass total (spec: direct recursion, not `exists`, 07 I2) | **CORE** |
| 18 | `tx.requireAuthorizedBy(cred: Credential)` — pubkey ⇒ in signatories; script ⇒ withdrawal present (withdraw-zero) | 4 protocols stake-credential-approves (05 §7: amaru, cip113, cardano-swaps, Lenfi `authorized_by_credential` quoted §4b.1); Minswap auth methods (04 §2 r8) | 04,05,06 | **AU-2** ("signature ≠ authorization when the authority is a script", 06 §9 V014 note) | one signatories scan or one `SortedMap.get` on withdrawals | **CORE** |
| 19 | `value.uniqueTokenOf(policy): (TokenName, BigInt)` — exactly one token under policy, else fail | 5 in-repo (01 §P27); 2 wild ×6 sites (08 §4 r15); ADP `get_single_asset…`, amaru `expect_nft`, Hydra `hasST` (05 §1.4) | 01,05,08 | AU-1, MI-5; kills the Betting/LinkedList griefing shapes (01 §P27 hazards) | `tokens(p)` + one match on the pair list; one-shot tuple return acceptable (07 B7 targets per-step fold tuples) | **CORE** |
| 20 | `value.singleAssetApartFromAda: (PolicyId, TokenName, BigInt)` | 12 protocols value-shape family (05 §7); Midgard, JPG, Sundae, Splash, cip113 (04 §2 r6); hydrozoa `onlyNonAdaAsset` (08 §2.1); Fortuna `value_has_nft_and_lovelace` (05 §4b.3) | 04,05,08 | AU-1, RS-1 | ADA is the first `Value` entry on-chain — pop head, match tail; never `flatten` (07 I5; 05 §4 "never flatten a Value") | **CORE** |
| 21 | `tx.referenceDatumByToken[T: FromData](policy, name): T` — find ref input by NFT (exactly one) + inline datum + decode, fused | 8 protocols config-NFT indirection (05 §7); 6 DEX (04 §2 r4); DID + PriceBet oracle reads (01 §P22); hydrozoa `findRegimeReference`, binocular `findOracleInput` ×2 divergent copies (08 §2.1, §2.3) | 01,04,05,06,08 | **AU-1 (s5)** — enforces the auth+decode pairing that binocular's copy-paste divergence broke (08 §2.3 cat3) | row 6 + `inlineOrFail`; decode free under V3 (07 §1.6) | **CORE** |
| 22 | `tx.validityLowerBoundOrFail: PosixTime` / `tx.validityUpperBoundOrFail: PosixTime` — finite bound or fail, no defaulting | 3 spellings in-repo (01 §P09 N14/N15); 5 wild interval destructure ×10 (08 §4 r11); Minswap `must_get_finite_start_validity`, WingRiders `pfiniteTxValidityRangeTimestamps` (04 §5) | 01,04,08 | **TI-1** — the replacement that lets `getValidityStartTime` be deprecated | one match on the bound; no Option | **CORE** |
| 23 | `tx.requireValidityWidthAtMost(ms: BigInt)` | binocular + vela both hand-roll it (08 §2.3); Minswap 10-min cap, treasury `interval_length_at_most` 36h (04 §2 r7; 05 §1.5); Butane price-feed width < 1 day (05 §4b.2) | 04,05,08 | **TI-1** — bounds the attacker-chosen window, "no current Cardano stdlib offers it" (06 TI-1) | two bound matches + one compare | **CORE** |
| 24 | `tx.requireWithdrawalFrom(scriptHash)` — withdraw-zero presence | 2 in-repo (01 §P23); 6 DEX (04 §2 r5); 7 protocols (05 §7); vodka `withdrawal_script_validated` (03 §4.1) | 01,03,04,05 | PU-2; ⚠ gated on the `Ord[Credential]` verification (Part 4 F1) | one `SortedMap.get` on withdrawals | **CORE** |
| 25 | `tx.withdrawalRedeemerOrFail[R: FromData](scriptHash): R` | duplicated inside scalus-design-patterns itself (StakeValidator vs MerkelizedValidator, 02 §C.6); ADP `get_withdraw_scripts_redeemer_at`, vodka `withdrawal_redeemer` (05 §1.1, §5.5) | 02,03,05,06 | **PU-1** — "it ran" + "with THIS redeemer" must be one call (06 PU-1); same `Ord[Credential]` gate | `SortedMap.get` on redeemers + free decode (07 §1.6) | **CORE** |
| 26 | `tx.spendRedeemerOrFail[R: FromData](ref: TxOutRef): R` — cross-purpose redeemer read | vela ×11 sites, cosmex (08 §4 r8); Sundae/Midgard/WingRiders redeemer-by-purpose (04 §2 r16); Indigo's critical audit finding was exactly the missing form of this (06 §5 Oct-2022) | 04,05,06,08 | **PU-1 (s5)** | `SortedMap.get` keyed by `ScriptPurpose.Spending(ref)` | **CORE** |
| 27 | `tx.outputAtWithToken(idx, policy, name): TxOut` / `tx.referenceInputAtWithToken(idx, policy, name): TxInInfo` — index for lookup, token for authentication | 5 wild projects — the dominant perf workaround (08 §3.5 table); binocular mint redeemer *is* an output index (08 §2.3) | 05,06,08 | IX-3 (output indices have zero ledger guarantee), AU-1 | `at` = `dropList` const + `lookupCoin` (07 §2.3, §3.3) | **CORE** |
| 28 | `List.singleOrFail(msg): A` — exactly-one element of an already-filtered list | vela `oneOrFail` used **~30×**, the most-used user helper in the wild corpus (08 §2.2); starter template, binocular, hydrozoa spellings (08 §3.4) | 08 | same family as row 5 | one match on the cons cell | **CORE** |
| 29 | `List[BigInt].requireStrictlyAscending(msg)` — monotone index lists (BigInt-concrete, not generic) | Crowdfunding (01 §P26); ADP multi-indexer enforces it (04 §3B); Sundae bitmask / Minswap byte-set / WingRiders O(n²) all solve the same problem (04 §3B = 3 protocols + ADP) | 01,04,05,06 | **IX-1 (s5)** — Crowdfunding critical finding (zip truncation + duplicate indices) | one fold, BigInt-concrete so compares are `lessThanInteger` not `equalsData` (07 I9) | **CORE** |
| 30 | `tx.requireOnlyBurnsUnder(policy)` — at least one entry, all negative | 5 in-repo, 3 verbatim copies (01 §P12); vodka `check_policy_only_burn` = 14 calls (03 §6.2) | 01,03 | MI-3; closes the empty-map-`forall`-vacuous latent hole (01 §P12 hazard) | `tokens(p)` + one pass | **CORE** |
| 31 | `value.requireAdaOnly(msg)` | 3 in-repo (01 §P25) — the *mitigation* only 3 of 8 lovelace-only contracts apply | 01,06 | **VP-2 (s5)** mitigation — declares the ADA-only assumption once instead of implicitly per comparison (06 VP-2 fix 2) | `withoutLovelace` = `insertCoin` builtin + `isZero` (07 §2.3) | **CORE** |
| 32 | `Math.divCeil(a, b)` / `Math.divFloor(a, b)` (+ `mulDiv(a, b, c, rounding)`) | WingRiders `divideCeil`, GY `ceil_mul_ratio`/`floor_mul_ratio`, liqwid `mulTruncate` family, JPG fee approx = 4 protocols (04 §5.1, §5.6; 05 §4 ratio-math) | 04,05,06 | **AR-1** — rounding-direction exploitation; named direction instead of bare `/` | 2-3 integer builtins | **CORE** |
| 33 | `tx.findOwnInputsByCredential` stays as-is; NEW: `tx.requireNoContinuingOutput(ownInput)` | 4 in-repo, 3 spellings (01 §P30) | 01 | closes the `size==0` / `isEmpty` / `<=1` divergence | one filtered-emptiness scan (isEmpty, not length — 07 I14) | **CORE** |
| 34 | Sugar `tx.requireNoMintUnder(policy)` = `requireMintExactly(policy, SortedMap.empty)` | Butane `mints_nothing_here`, LinkedList update-path (05 §4b.2; 02 §B.8) | 02,05,06 | MI-1 family completeness ("this policy must not move") | one `tokens(p).isEmpty` | **CORE** |
| 35 | `MultisigScript` ADT (`Signature/AllOf/AnyOf/AtLeast/Before/After/Script`) + `satisfied(tx)` | 4 protocols via aicone (05 §2.1: treasury, amaru, + hydrozoa & Lenfi hand-roll equivalents); 5 DEX multi-method auth (04 §2 r8) | 04,05 | AU-2; the `Script` leaf is withdraw-zero-as-delegation | pure data ADT (no closures — stays Data-encodable, 07 A2), recursion over the tree | **EXTENDED** — port aicone near-verbatim; sizable, second wave |
| 36 | `tx.requireSamePaymentCredential(ownInput, out)` — the *named* loose continuing check | vela house style ~15 sites (08 §3.2); `findOwnOutputsByCredential` users (01 §P06) | 01,06,08 | AU-4 made LOUD: the opt-out must be typed out and scaladoc'd staking-unsafe | one credential `equalsData` | **EXTENDED** — exists only to make row 1's default safe without stranding real demand |
| 37 | `tx.continuingOutputs(ownInput): List[TxOut]` — legit N>1 | AMM-style multi-continuation cases (01 §P05) | 01 | — | filter | **EXTENDED** — rare vs exactly-one |
| 38 | Indexed-pair coverage combinator (walks the *input list*, asserts every own-credential input covered, ascending, per-pair `=> Unit` block) | ADP multi-indexer + existing `UtxoIndexer.multiOneToOneNoRedeemer` which 06 endorses as the model (06 IX-2); 4 protocols batch-cursor (05 §7) | 04,05,06 | **IX-2 (missed-input), DS-5** | one input walk (07 R8) | **EXTENDED** — lives in the patterns module, upgraded per R1 (Part 4) |
| 39 | `tx.requireOnlyScriptInputsFrom(allowed: List[Credential])` | treasury `ensure_compliant_scripts`, Minswap `has_only_pool_and_author`, Sundae count variant (04 §3D = 3) | 04,05 | DS-1 C5 | one input walk | **EXTENDED** — closed-protocol niche |
| 40 | `tx.countInputsAt(cred): BigInt` / `scriptInputCount` | 6 DEX (04 §2 r14); 2 wild (08 §4 r17) | 04,08 | DS support | one count fold | **EXTENDED** — `count` composition, name aids review |
| 41 | `List.zipExact(other)` — fails on length mismatch | Crowdfunding zip-truncation critical (06 IX-1); Minswap `zip_with` with length check, Midgard `zip_foldl` (04 §2 r15) | 04,05,06 | IX-1 | one paired walk; no truncating zip in high-level surface (06 IX-1) | **EXTENDED** |
| 42 | `List.requireDistinct` | Crowdfunding (01 §P26) | 01 | IX-1 | O(n²) — prefer `requireStrictlyAscending` (07 R11) | **EXTENDED** — warn loudly, ascending is the primary |
| 43 | `value.quantityOfOrFail(policy, name)` / vela `existingQuantityOf` | 4 wild, 12 sites (08 §4 r6); Aiken `expect_quantity_of` gap (03 §7.2) | 03,08 | fail-fast completeness | PV11 `lookupCoin` makes `require(quantityOf === n)` one cheap line already | **EXTENDED** — thin; ship for Aiken parity + discoverability, low priority |
| 44 | `value.hasToken(policy, name): Boolean` (Aiken `has_nft`) | Aiken gap table (03 §7.2) | 03 | — | `quantityOf === 1` one-liner | **EXTENDED** — discoverability only |
| 45 | `Value.of((policy, name, qty)*)` ergonomic builder for exact comparisons | 2 wild whole-value construction (08 §4 r20); Minswap estimate-value style (04 §4.4) | 04,08 | VP-1 | constructors exist; builder is sugar | **EXTENDED** |
| 46 | `value.assetCount: BigInt` | Sundae `has_exact_token_count`, Splash `pValueLength`, treasury ≤4, WingRiders = 5 protocols (04 §2 r17) | 04,05 | RS-1 bounded-value | one flatten-free count walk | **EXTENDED** |
| 47 | `tx.mintRedeemerOrFail[R](policy): R` | 6 protocols spend-forwards-to-mint (05 §7) | 05 | PU-1 | `SortedMap.get` | **EXTENDED** |
| 48 | `tx.requireRanWith[R: ToData](purpose, expected)` | 06 PU-1 proposal; vela's 11-site match-on-redeemer (08 §2.2) | 05,06,08 | PU-1 | rows 25/26 + one `equalsData` | **EXTENDED** — sugar over CORE rows |
| 49 | `Interval.containsInterval(other)` | Sundae `contains_interval`, Butane freshness (04 §5.3, 05 §4b.2) | 04,05 | DE-2 oracle freshness | bound compares | **EXTENDED** |
| 50 | Promote `NormalizedInterval` from scalus-design-patterns into the stdlib | ADP pattern; TI-2 fix (06); Aiken parity (03 §7.11) | 03,06 | TI-2 | already implemented | **EXTENDED** — a move, not new code (Part 4) |
| 51 | `Cip68` object (label100/222/333/444, apply/strip) | 2 in-repo (01 §P28); vodka cip module (03 §4.1); Sundae/Midgard labels (04 §2 r19); **zero of 15 analyzed protocols on-chain** (05 §7 obs 1) | 01,03,04,05 | — | byte concat | **EXTENDED** — standard + cheap, but measured demand is minting-tooling, not validators |
| 52 | `Rational` arithmetic suite (add/mul/div/ceil/floor/compare-by-cross-multiplication) | Aiken 25-fn module vs Scalus data-class-only (03 §7.10); 4 protocols ratio-math (05 §7); Pricebet forced into `RationalEq.equals` (01 §P08) | 01,03,05 | AR-1 | cross-multiplication, no gcd on-chain (liqwid `PRationalNoReduce` precedent, 05 §4) | **EXTENDED** — separate math workstream; keep `rationalNoEq` compile-error (correct: structural === would be wrong) |
| 53 | `value.tokenWithPrefix(policy, prefix)` with uniqueness guard | DID (01 §P28); Butane `params_from_refs` (05 §4b.2) | 01,05 | AU-1 (first-match ambiguity hazard, 01 §P28) | prefix compare per token | **EXTENDED** |
| 54 | `out.requireNoReferenceScript` / `tx.requireNoReferenceScriptsOnOutputs` | hydrozoa ×3 (08 §4 r23); treasury + Midgard = 3 protocols (05 §7) | 05,08 | RS-7 (exploited on mainnet Jun 2024, 06 §5) | one Option isEmpty per output | **EXTENDED** |
| 55 | `requireDatumMatchesValue(claimed: BigInt, out, policy, name)` | AMM datum-reserves-vs-value critical shape (06 VP-1(c)) | 06 | VP-1(c) | one `lookupCoin` + compare | **EXTENDED** |
| 56 | Oracle freshness reader (`referenceDatumByToken` + interval containment + width cap composed) | 5 protocols oracle-freshness (05 §7); Charli3 idiom (05 §6); Butane strongest form (05 §4b.2) | 05,06 | DE-2 | composition of rows 21+23+49 | **EXTENDED** — worked example first, API if it stabilizes |
| 57 | `signedPayload(domain, instance, nonce, body)` domain-separated signing payload | Sundae strategy signing + Butane/hydrozoa permits (04 §5.3; 05 §7 delegated-signature = 2) | 04,05,06 | AU-7 | serialiseData + concat | **EXTENDED** |
| 58 | `List.partition(p)` (CPS-shaped impl, no per-step tuple) | Aiken gap (03 §7.6); vodka `group_inputs` (03 §4.1) | 03 | — | naive foldr-with-tuple-acc violates 07 B7; needs reshaped impl | **EXTENDED** |
| 59 | `List.takeAtMostOrFail(n, msg)` bounded traversal | treasury + Fortuna hard caps (05 §7 bounded-traversal = 2) | 05,06 | RS-1/RS-2 ExUnits-bricking | one counted walk | **EXTENDED** |
| 60 | ByteString ordering/slice sugar (`<`,`take`,`drop`,`slice`,`startsWith`) | hydrozoa + binocular (08 §4 r21); Aiken gaps (03 §7.10) | 03,08 | — | direct builtins | **EXTENDED** — prelude-level, adjacent to this package |
| 61 | `ProtocolParametersUpdate` 30 typed getters | Aiken's biggest single-module gap (03 §7.4) | 03 | PU-4 support | Data field walks | **EXTENDED** — governance-validator niche |
| 62 | `requirePositive(x, name)` | 8 in-repo (01 §P24) | 01 | AR-2 | `require(x > 0)` is already one line | **EXTENDED** — marginal; uniform message only |
| 63 | `tx.requireMintOneShot(policy, name, seed)` fused (rows 4+14+15) | 01 §6 r12 | 01 | MI-1+MI-2 co-presence | composition | **EXTENDED** — sugar |
| 64 | Unrolled list skipping (`dropFast`/`atFast`) | 5-6 protocols hand-unroll by 15/10/5/2 (04 §4.1) | 04 | — | **already solved**: PV11 `at`/`drop` lower to the `dropList` builtin, constant cost (07 §3.3). Assumes PV11 target, which is the fixed constraint | **REJECT** — document that the PV11 intrinsic replaces the entire hand-unrolling genre |
| 65 | `Data.sameAs(other)` / documented `.toData ==` idiom | 05 §8 r2 | 05,07 | — | superseded: `===` *is* `equalsData` for Data-backed types (07 I7) | **REJECT** — see Part 2(a); ship documentation, not API |
| 66 | `requireDatumChangedOnly(self, cont)(fields…)` macro | 5 in-repo files use field-wise "must not change" (01 §P08c) | 01,06 | DT-1 | — | **REJECT** — the safe primitive is `old.copy(changed = …)` + row 8 whole-datum equality (06 DT-1); field-wise is the fails-open strategy being eliminated, a macro would legitimize it |
| 67 | Opaque types with **validating `FromData`** (`AscendingIndices`, `Positive`) | 06 IX-1/AR-2 proposals | 06,07 | — | **unsound under V3**: `fromData` is erased to identity (07 §1.6), decoder-embedded validation never runs — see Part 2 register entry (h) | **REJECT** as decoder-validation; the invariants ship as explicit `require*` (rows 29, 62) |
| 68 | `preimage.hashesTo(hash)` | 3 in-repo (01 §P21) | 01 | — | `sha2_256(p) === h` is already one line | **REJECT** |
| 69 | `mustAdvance(old, next)` monotonic ratchet | 2 protocols (05 §7) | 05 | DE-5 | `require(next > old)` one line | **REJECT** |
| 70 | Commit-reveal `Commitment` type | 06 DE-4 | 06 | DE-4 | — | **REJECT** for API; document as pattern |
| 71 | `Value.restrictedTo(mask)` / `flatten_with(strategy)` / `reduce` | Aiken gap rows (03 §7.2) | 03 | — | flatten-family is the anti-pattern (07 I5, D17); strategy objects store closures (07 A2/I23) | **REJECT** |
| 72 | `TxOut.datumOrFail[T]` resolving datum **hashes** via `tx.data` | proofspace only (08 §4 r12, 1 project, pre-inline-datum vintage) | 08 | DT-3 | — | **REJECT** — V3/inline-datum era; `findOwnDatum` covers the residue |
| 73 | `tx.requireNoOutputCarries(policy, name)` (burn-assert over outputs) | Sundae oracle only (04 §5.3) | 04 | — | full output walk | **REJECT** — single-protocol |
| 74 | BLS12-381 `Scalar` field type | hydrozoa donation (08 §2.1) | 08 | — | — | **REJECT for this API** — belongs in the crypto module, take the donation there |
| 75 | On-chain mock-tx builder + deterministic mocks (mocktail/virgin_*) | **the #1 most-used third-party API in the Aiken ecosystem, 3-5x over any validation helper** (03 §6.2, §8.1) | 03 | EV-2 adjacent | — | **REJECT for this deliverable** (off-chain testkit, not the on-chain surface) — but flagged as the highest-leverage companion workstream (Part 5 note) |
| 76 | Ledger-domain fuzz generators (77 fns) + scenario harness | Aiken `cardano/fuzz` (03 §5.1) | 03 | DETECT-class pitfalls (AU-2, DE-1, DE-3) | — | **REJECT for this deliverable** — testkit workstream |
| 77 | `List.forall2`/`zipAll` multisig walk | hydrozoa workaround ×1 (08 §2.1) | 08 | — | — | **REJECT** — `zipExact` (row 41) covers it |
| 78 | `lovelacePaidTo(cred): BigInt` as a separate helper | 6 of 8 payout sites compare lovelace only (01 §P16) | 01 | **anti-goal**: VP-2 | — | **REJECT** — `valuePaidTo(cred).getLovelace` exists; a named lovelace-only sum would *institutionalize* the token-stripping bug (01 §P07/P17 hazards) |
| 79 | Salt-grinding / withdrawal-position tricks, `expect_tail`, head-of-value fast paths | Butane, Splash, cip113 structural assumptions (04 §4.4, §5.10) | 04 | VP-5 risk | rely on undocumented ordering (06 §8 unverified) | **REJECT** — do not bless representation-order assumptions beyond "ADA is first" (which row 20 uses and the ledger guarantees) |

**Implementation-shape rule carried by every `require*`/`find*OrFail` row:** direct
tail recursion or `contains`-style intrinsic scans — never `exists`/`find(...).isDefined`
internally (07 I2: 326K–565K cpu fixed Option tax per call, ≈ 85–158 lovelace), never
`filter(...).length` (07 I1), never a per-step tuple accumulator (07 B7), and every
callback parameter is `=> Unit`, not `=> Boolean` (06 R1 / EV-1).

---

## Part 2 — Contradiction register

### (a) `a.toData == b.toData` in production vs "`===` already lowers to `equalsData`"

**Position 1 (08):** production Scalus code hand-writes `toData` equality — vela 9×,
binocular 2×, cosmex 1× (08 §3.3) — and binocular documents a *security* reason:
"on-chain `FromData` is an erased retag (no tag or arity check), so field-wise reads
would also accept `Constr 5 [root, junk]`" (08 §2.3, TreasuryMovementValidator:515-533).
**Position 2 (07):** for any Data-backed type, `a === b` already lowers to
`equalsData(toData a, toData b)`; the two spellings pin to byte-identical budgets
(901 mem / 1,653,665 cpu, ValueTest.scala:1619 vs :1628; 07 I7, rule C13).

**Resolution: 07 is right on mechanism; 08's sites are explained, not contradicted.**
Three distinct reasons produce the `toData` spelling in the wild, each with its own answer:

1. *Cost belief* ("`===` is field-by-field") — false under the default V3 backend;
   the API documentation states `===` as the canonical cheap form and never ships an
   `equalsData` wrapper (07 rule C13; the `optimize-contract` skill's O016/O020 are
   stale on this point and must be fixed, 07 §6 verdict table).
2. *Missing `Eq` instance* (Vault `Status`, enum cases, `Rational`) — the real gap.
   Fix by shipping/deriving `Eq` where sound. For `Rational` the missing `Eq` is
   **deliberately correct**: on-chain `===` is structural, and structural num/den
   comparison is not rational equality (02 §A.13) — keep `rationalNoEq`, keep
   `RationalEq.equals` as the explicit cross-multiplied form.
3. *Security: whole-datum comparison* — the property binocular wants (junk-rejecting
   comparison) is a property of comparing the **whole constructed expected datum**, and
   `===` on the decoded values provides exactly the same whole-tree `equalsData`. Row 8
   (`requireInlineDatum[T](expected)`) encodes it; the API documents that field-wise
   datum checks are the fails-open shape (06 DT-1) and whole-value comparison is the
   default.

**What the API documents:** "Write `===`. Never write `a.toData == b.toData`; it
compiles to the identical term. If `===` does not compile, the type is missing an `Eq`
instance — derive one, unless the type (like `Rational`) has value-vs-structure
semantics, in which case the explicit comparator is the point."

### (b) `Option`-returning finders: existing style (02) + Aiken style (03) vs the Option tax (07)

**Position 1 (02, 03):** the existing Scalus high-level layer is almost entirely
`find…: Option`/`List` shaped (02 §A.0 observation); Aiken pairs every partial function
with an `expect_*` sibling and downstream code uses both (03 §1 note, §8.3).
**Position 2 (07):** `Option` always allocates a `constrData(0, mkCons(x, mkNilData()))`
and no pass ever folds `Case` over a literal `Constr` (07 §3.7, rule B6); the tax is
measured at 326,483 cpu (miss) / 564,996 cpu (hit) per call ≈ 85–158 lovelace (07 §4.1).

**Resolution: both are right, at different granularities — the tax is fixed per call,
not per element.** At list length 20 the Option overhead of a single `find` fades to
2% (07 §4.1); a one-off `findContinuingOutput: Option` in a validator that genuinely
branches on absence is fine. What is *not* fine is Option inside per-element machinery
(`exists` = `find(p).isDefined` pays the full tax on every call and is not intrinsic;
07 I2) or Option as the *only* form, which produced the bare-`.get` epidemic in the
wild (hydrozoa ×4, binocular ×3; 08 §5 #9, #13).

**Where `Option` is still justified — the complete list:**

1. **At-most-one semantics with a real None-continuation**: `findContinuingOutput`
   (SimpleTransfer's full-withdrawal ⇒ no continuing output, 01 §P30), `findUnique`.
2. **Ledger-shaped optionality**: the V3 spend datum is `Option[Datum]` by CIP-69;
   `Credential.pubKeyOption`/`scriptOption` mirror a genuine two-case domain.
3. Everywhere else the OrFail form is the only high-level form; the Option twin exists
   for the two categories above and is documented as the secondary form. This satisfies
   the fixed naming constraint ("fail-fast is PRIMARY") and the Aiken parity goal
   simultaneously — Aiken's own docs call `expect_*` "more efficient" for the same
   allocation reason (03 §1).

### (c) Three incompatible continuing-output address checks

**Positions (08 §3.2):** full `address ===` (hydrozoa, binocular, cosmex);
`address.credential ===` only (vela ~15 sites, proofspace); reconstructed
`Address(cred, None)` (vela Bootstrap, binocular mint, in-repo Auction family via
`Address.fromScriptHash`, 01 §P06). **06 AU-4** classifies credential-only as a reward
hijack (franken addresses, real disclosures vs Atomic Swap / TradingTent) and notes it
also *breaks the DS-1 "one input at this address" guard*; severity confirmed by the
in-repo Vesting/Betting comments (01 §P06).

**Resolution — the API names the safe one and makes the loose one loud:**

- `tx.findContinuingOutputOrFail(ownInput)` (row 1) compares the **full `Address`**,
  staking part included. This is the *only* operation named "continuing".
- The credential-only check exists as a *separately named* op,
  `requireSamePaymentCredential` (row 36), whose scaladoc states the staking-hijack
  consequence — real demand exists (vela's whole house style) so it must not be
  impossible, only unmistakable (06 R2: unsafe is a named argument, never an omission).
- Address **reconstruction** (`Address.fromScriptHash(h)` compared against outputs) is
  never used by any high-level helper and its scaladoc gains the "no staking part"
  warning (Part 4). The constructor itself stays — it is legitimate for building
  enterprise addresses — but it disappears from the continuing-output story because the
  helpers compare against `ownInput.resolved.address`, which needs no reconstruction
  (06 AU-3/AU-4 fixes, delivered through API shape rather than deletion).

What the API makes hard to get wrong: with row 1 there is nothing to reconstruct and no
credential to extract — the wrong comparisons stop being written because the right one
is shorter.

**The payout-side variant of the same split.** The three-way divergence recurs on
outputs paying *parties*, not the script: Auction matches the reconstructed enterprise
`Address.fromPubKeyHash` (misses real wallet addresses with a staking part), Lottery
matches the credential only (01 §4 N23), and the in-house PaymentSplitter finding is
"payee outputs matched on payment credential only → staking-credential redirect of
rewards" (06 AU-4). Per 06 AU-4 fix #3 ("payout helpers take `Address`, never
`Credential`"), the payout family (`valuePaidTo`, `valueSpentFrom`,
`requirePaidTagged`) is **`Address`-first** — full match, safe whenever the datum
stores a full address, mirroring treasury's `value_sum` stake-policy flag (05 §2.3).
`Credential` overloads exist for the common pkh-only-datum case (the payee's staking
choice is then genuinely theirs), with the reward-redirect caveat in the scaladoc.

### (d) Six anti-double-satisfaction mechanisms vs a mandatory `OwnInputPolicy`

**Positions:** 04 §3 and 05 §7.1 document six structurally different mechanisms (input
whitelist; single-script-hash + equal redeemers; exactly-one filtered; output tagged
with spent `TxOutRef`; index hints + coverage; global aggregate accounting), and
`aiken-design-patterns` explicitly declines to solve it, shipping only a
`double_satisfaction_prevented: Bool` reminder (04 §3). 06 proposes a mandatory
`OwnInputPolicy` sum type as an abstract member of a new `SpendingValidator` trait, with
no default (06 §3, DS-1).

**Is it workable given the `Validator` trait shape (v3/Validator.scala:9-88)?
No — not inside this deliverable's constraints.** Three blockers:

1. The fixed constraints allow extensions on `TxInfo`/`TxOut`/`Value`/`Interval`/`List`
   and new *small types* — not a new validator framework or changes to the three
   existing traits.
2. Adding an abstract member to `Validator` (or interposing a guard before `spend` in
   `validateScriptContext`) is source-breaking for every existing implementer — all 22+
   in-repo validators and every wild project — against the repo's compatibility policy.
   The three traits are also triplicated copy-paste (02 §A.2/C.3); changing the contract
   means changing all three.
3. The members are `inline def`s dispatched in an inline match; injecting a
   policy-selected guard there is a redesign of the trait, not an addition.

**Fallback (adopted):** deliver the same six mechanisms as *named combinators* on the
allowed surface, so "which DS strategy is this validator using?" becomes a grep, not a
type. The mapping from 06's enum to shipped operations:

| `OwnInputPolicy` case (06) | Shipped as | Row |
|---|---|---|
| `Exclusive` | `tx.requireSingleOwnInput(ownInput)` (scaladoc: instance-scoped, does not stop DS-2) | 11 |
| `TaggedOutputs` | `ownRef.asDatumTag` + `tx.requirePaidTagged(cred, value, tag)` (tag = `TxOutRef`, globally unique ⇒ also closes DS-2) | 12 |
| `Indexed` | `tx.inputAt(idx, ownRef)` + coverage combinator (walks inputs, IX-2 closed) | 9, 38 |
| `Aggregated` | `tx.valueSpentFrom(cred)` / `tx.valuePaidTo(cred)` folds | 16 |
| DS-1 whitelist / DS-2 variants | `tx.requireOnlyScriptInputsFrom(allowed)` | 39 |
| `Unchecked(why)` | absence of all of the above — the security-review skill flags a spending validator using `valuePaidTo`/payout checks with none of rows 11/12/38/39 present |

The *mandatory-choice* property (06's whole point: "no policy" impossible to express)
cannot be delivered by extensions; it is deferred to a possible future opt-in
`SpendingValidator` layer and raised as Open Question 2. Until then the delivery is
LOUD-by-naming plus review tooling, which is exactly where `aiken-design-patterns`
landed — but with real named mechanisms instead of a boolean reminder.

### (e) "Exactly one" as first-class vs `filter`-then-match vs `find`

**Positions:** 05 §8.1 argues "exactly one" must be a first-class result kind of the
find-grid (Liqwid distinguishes `phasOnlyOneTokenOfCurrencySymbol` from the ≥1 form);
01/02/08 document ~55 sites of `filter`-then-match across four spellings (02 §C.10,
08 §4 r1); 08 documents `find` as a silent bug for this purpose, in binocular's own
words: "`find` stops at the first commitment and would silently accept a TM carrying a
second one" (08 §2.3).

**Resolution — the primitive is a single-pass find-then-prove-no-second scan:**
`List.findUniqueOrFail(p, msg): A` (row 5) finds the first match, *continues scanning*
to assert no second match, and returns the element — one traversal, no intermediate
list, no Option. `filter`-then-match is rejected as the implementation (2 traversals +
k `mkCons`, 07 I1); `find` is retained in the prelude but its scaladoc gains the
binocular warning and the high-level TxInfo finders (rows 1, 6) are all built on the
unique scan. `List.singleOrFail` (row 28) covers the "already filtered, assert
singleton" tail of the demand (vela's 30-site `oneOrFail`). The deliberate cost note:
the uniqueness scan always walks the whole list — that *is* the security property being
paid for; callers who genuinely want first-match keep `find` and own the risk.

### (f) Redeemer-carried index APIs (04/05) vs index lists as a pitfall class (06 IX-1)

**Positions:** 04 (8 of 12 DEXes) and 05 (10 protocols) show index-in-redeemer is *the*
standard cost optimization, and 08 §3.5 shows it is the dominant workaround in Scalus
projects too. 06 ranks index handling as pitfall IX-1 (s5) with three sub-bugs
(duplicates, length mismatch/zip truncation, out-of-range) plus IX-2 (missed input) and
IX-3 (ordering assumptions).

**Reconciled design — an index is a hint, the binding is the API:**

1. Every indexed accessor **fuses the binding**: `tx.inputAt(idx, expectedRef)` fails
   unless `inputs.at(idx).outRef === expectedRef` (the two-line idiom that appears 15×
   in-repo, 02 §C.8, becomes one call). Outputs have no `outRef` to bind, so indexed
   output access is only offered fused with an authentication:
   `tx.outputAtWithToken(idx, policy, name)` (row 27) — matching the observed wild
   pattern "index for lookup, token for authentication" (08 §3.5).
2. Index *lists* are validated by explicit calls: `requireStrictlyAscending` (row 29,
   kills duplicates + gives determinism), `zipExact` (row 41, kills truncation);
   `List.at`'s existing PV11 out-of-range guards kill the third sub-bug (06 IX-1 notes
   "Scalus's `at` already fails on out-of-range — keep it").
3. Batch coverage (IX-2) is the framework's job in the coverage combinator (row 38),
   which walks the **input list**, not the index list — the model being the existing
   `UtxoIndexer.multiOneToOneNoRedeemer`, which 06 itself endorses as already correct.
4. Bare `tx.outputs.at(i)` with no binding stays available at the List layer (06 VP-3
   wanted it unreachable; the constraints keep the prelude intact) — the altitude
   change is the documented signal: high-level = bound access, prelude = you own the
   check.

### (g) No tuples from folds, no closures in public types (07) — who violates, and reshapes

**The rules (07 B7, A2/I23; plus 06 R1):** per-step tuple accumulators lower to
`ProdDataList` build/tear-down per element (why `dropRight` costs ~4× `drop`); a stored
closure silently forces a type out of the Data world (loses `equalsData`, free
`fromData`, every intrinsic); `Boolean`-returning callbacks are the EV-1 footgun.

**Violations found in the other reports' proposals, and their reshapes:**

| Proposal | Violation | Reshape |
|---|---|---|
| `tx.timeWindow: (Long, Long)` (04 r7), `requireBoundedRange → (PosixTime, PosixTime)` (06 TI-1) | tuple return | two scalar accessors (`validityLowerBoundOrFail` / `UpperBoundOrFail`, row 22) + `requireValidityWidthAtMost` (row 23); no pair needed |
| vodka `group_outputs_2` 3-way partition, `group_inputs` (03 §4.1) | `foldr` with tuple-of-lists accumulator = n tuple allocations | not ported as-is; `List.partition` (row 58) only with a CPS/accumulator-free implementation |
| `value.singleAssetApartFromAda: (PolicyId, TokenName, BigInt)` (05 r5), `uniqueTokenOf` | one-shot tuple return | **allowed** — B7 targets per-step fold accumulators; a single return-position tuple is one allocation. Where it matters, an `inline` CPS overload (`inline def …(inline k: (PolicyId, TokenName, BigInt) => A): A`) erases even that — scalac inlining expands the lambda before the plugin sees it (**verify at implementation time**; Aiken's `Scott3` continuation is the precedent, 05 §1.4) |
| `cursorFold[A](start)(step)(finish)` batch fold (05 r24) | closure-heavy, tuple accumulators likely | delivered as the coverage combinator (row 38) with a `=> Unit` per-pair block and scalar cursor |
| 06 `CertPolicy.Custom(rule: (TxCert, TxInfo) => Unit)` | **closure stored in an enum case** → forces UplcConstr repr (07 I23) | no callback-carrying enum; certificate policy stays the plugin's default-fail + explicit `certify` implementations |
| Aiken `FlattenStrategy`/`UnionStrategy` combinator vocabulary (03 §2.7) | strategy objects are stored closures | rejected (row 71) |
| existing `UtxoIndexer`/`StakeValidator`/`TransactionLevelMinterValidator`/`MerkelizedValidator` callbacks | `=> Boolean` (EV-1) | migrate to `=> Unit` (Part 4) |
| `MultisigScript` (row 35) | none — pure-data ADT, no closures | port as-is; it *must* stay Data-encodable because it lives in datums |

### (h) *(found in synthesis)* Validating-`FromData` invariant types (06) vs `FromData` erasure (07)

**Position 1 (06):** IX-1 and AR-2 propose opaque types (`AscendingIndices`,
`IndexPairs`, `Positive`) "whose `FromData` instance REJECTS" bad input, so invalid
redeemers never reach user code.
**Position 2 (07 §1.6):** under the default `SirToUplcV3Lowering` backend, the linker
rewrites every `fromData`/`toData` application to `UniversalDataConversion` and the
lowering emits the **identity** — no decoder code exists at all. A validating `FromData`
would be silently skipped on-chain while running on the JVM: the worst possible
divergence (exactly the EV-2 class).

**Resolution: 07 wins outright; this also matches the repo's own prior T9 decision**
(keep the lazy no-op `fromData`, validation is an explicit opt-in `expect`). Invariants
are established by explicit `require*` calls at the use site (rows 29, 41, 62), never
by decoding. Any future `Trusted[D]`/validated-decode design must be built on an
explicit deep-check entry point, not on `FromData` instances.

### (i) *(found in synthesis)* "Only whole-sub-map mint assertions" (06) vs demand for weaker mint checks (03/05, 01)

**Position 1 (06 MI-1):** the stdlib should expose *no* `quantityOf`-based mint
assertion at all; whole-sub-map `requireMintExactly` is the only form (Minswap
incident; the Plutus team's own note that the sum-over-names helper "is perhaps
responsible for more critical vulnerabilities than any other utility function").
**Position 2 (03/05):** vodka ships four strictness levels and they are heavily used
(`policy_only_minted_token`, `token_minted`, …); EditableNft deliberately uses the weak
`quantityOf === -1` on its spend path while the mint path is strict (01 §P11).

**Resolution:** the *assertion* family is whole-sub-map only: `requireMintExactly`,
`requireMintsOne`, `requireBurnsAll` (positive qty, negated internally — MI-3 sign
safety), `requireOnlyBurnsUnder`, `requireNoMintUnder` (rows 4, 30, 34). The weak
checks stay expressible through the **query** layer (`mint.quantityOf(p, n)`) plus a
hand-written `require` — deliberately less convenient than the safe form, per the 06 §6
lesson that "a safe helper next to an unsafe sibling with a similar name is not a safe
API". No `requireMint(policy, name, qty)` that ignores other names ships (open
question 7 records the residual disagreement).

---

## Part 3 — Layer map

Four layers over the existing surface. Each CORE row lives in exactly one layer.

### L0 — Prelude collections (existing package, hardened)

*Responsibility:* representation-honest data operations; nothing knows about
transactions. *New ops:* `findUniqueOrFail`/`findUnique`, `singleOrFail`,
`requireStrictlyAscending`, (`zipExact`, `partition` — EXTENDED). *Types introduced:*
none. *Excludes:* anything touching `TxInfo`; any combinator relying on fusion or the
optimizer (07 R24: design for `optimizeUplc = false`); any per-step tuple accumulator.
This layer is also where the anti-pattern documentation lives (`filter().length` →
`count`; `exists` tax; `AssocMap` → `SortedMap`; append → prepend).

### L1 — Query layer (`TxInfo`/`TxOut`/`Value` extensions returning values)

*Responsibility:* locate and decode transaction facts, fail-fast primary.
*Ops:* `ownScriptHash`, `inputAt`, `outputAtWithToken`, `referenceInputAtWithToken`,
`findInputWithTokenOrFail`, `findReferenceInputWithTokenOrFail`,
`referenceDatumByToken[T]`, `inlineDatumOrFail[T]`/`inlineDatum[T]`,
`valuePaidTo`, `valueSpentFrom`, `uniqueTokenOf`, `singleAssetApartFromAda`,
`validityLowerBoundOrFail`/`UpperBoundOrFail`, `withdrawalRedeemerOrFail`,
`spendRedeemerOrFail`, `deriveTokenName`. *Types:* none new. *Excludes:* any
unauthenticated foreign-state reader — there is no `findReferenceInputAtAddress`-style
op; foreign state is reached by token or not at all (06 AU-1). Option twins exist only
per Part 2(b)'s two categories.

### L2 — Obligation layer (`require*`, `Unit`-returning, message-carrying)

*Responsibility:* one named call per security obligation; every default is the safe
one. *Ops:* `requireSignedBy`/`ByAll`/`ByAny`, `requireAuthorizedBy`,
`requireMintExactly` + sugar, `requireOnlyBurnsUnder`, `requireNoMintUnder`,
`requireAfter`/`requireBefore`, `requireValidityWidthAtMost`, `requireSpends`,
`requireSingleOwnInput`, `requireWithdrawalFrom`, `requireAdaOnly`,
`requirePaidTagged`, `requireEqualPlusMinAda`, `requireNoContinuingOutput`,
`Math.divCeil/divFloor`. *Types:* none new (message strings, `inline`). *Excludes:*
`Boolean`-returning callbacks anywhere (06 R1); any lovelace-only comparison helper
(VP-2); any minimum-payment helper without the DS warning naming (06 VP-4).

### L3 — Continuation facade (the "safe by default" layer)

*Responsibility:* the state-machine step as one concept. *Ops:*
`findContinuingOutputOrFail`/`findContinuingOutput`, `requireContinuing(ownInput,
value, datum)` — full address + whole value + whole datum in one call —
`requireSamePaymentCredential` (the named opt-out), `continuingOutputs` (the named
N>1 form), `ownRef.asDatumTag`. *Types introduced:* none required in v1 (an
`OwnInput`-style bundle is Open Question 4). *Excludes deliberately:* partial variants
— there is no continuing check that constrains datum but not value; anyone needing an
unusual shape drops to L1 (`findContinuingOutputOrFail` returns the `TxOut`) and writes
explicit requires, which is a visible altitude change in review.

### L4 — Patterns module (EXTENDED wave; today's `scalus-design-patterns`, upgraded)

*Responsibility:* multi-script architectures and opinionated machinery:
`MultisigScript`, indexed-pair coverage combinator, `NormalizedInterval` (promoted),
withdraw-zero validators, oracle-freshness composition, Cip68, Rational suite.
*Prerequisite fixes:* Boolean → Unit callbacks (R1), cross-platform (it is JVM-only
today, 03 §7.11). *Excludes:* mock/test builders and fuzz generators — those are the
testkit workstream (03 §8 items 1, 11), not on-chain code.

**Moving between layers.** L3/L2 names are the review vocabulary: a validator written
against them reads as its own security argument. Dropping to L1 is normal (queries
compose); dropping to L0/raw `TxInfo` fields means "I own the binding and uniqueness
checks" and is what the security-review skill keys on. Every L2/L3 scaladoc states
which L1 calls it expands to, so the escape hatch is always documented in place.

---

## Part 4 — What Scalus already has: keep / rename / deprecate / delete / fix

### Keep as-is (and document harder)

| Item | Reason |
|---|---|
| `findOwnInputOrFail` (v3:960) | the canonical fail-fast finder; 23 in-repo users (01 §P02) |
| `OutputDatum.inlineOrFail` (v2:82/94) | correct + compile-time-checked receiver; row 8 re-hangs it on `TxOut` |
| `Value.hasOnly` (v1:942) | "exactly the whole-sub-map check MI-1 needs" (06 §6) — **promote to the headline mint API**, `requireMintExactly` sugar builds on it |
| `Value.quantityOf`, `containsAtLeast`, `insertCoin`, `withoutLovelace`, `+/-/*` | CIP-153 builtin-backed, 13–75x wins (07 §2.5); route everything through them (07 D16) |
| `Interval.isEntirelyAfter/Before` | fail-closed on unbounded — the correct time primitives (06 TI-1); rows 10 wrap them |
| `IntervalBound.finiteOrFail` | correct; rows 22 wrap it |
| `List.at`/`!!` PV11 guards | out-of-range fails; keep (06 IX-1); PV11 `dropList` also obsoletes hand-unrolled skipping (row 64) |
| `List.count`, `filterMap`, `findMap` | the fused single-pass precedents (07 R8) — document as the replacements for `filter().length` / `map.filter` |
| `SortedMap` (vs `AssocMap`) | early-exit `get`, linear `union` (07 E20) |
| `PairList` + zero-cost `toPairList` | ~4 vs ~12 builtins/element (07 E21) |
| Plugin default-`fail` for unimplemented purposes | closes PU-1/PU-3/PU-4 by default — **document as a deliberate security property**, it is currently undocumented (06 §6 Scalus row) |
| `Math.isSqrt` | already ships the "verify, don't compute" primitive (04 §6.10; 02 §A.14) |
| `UtxoIndexer.multiOneToOneNoRedeemer` coverage logic | both-direction IX-2 coverage is already right (06 IX-2) — keep the algorithm, fix the callback type |
| `AssocMap` | keep for ledger-shape compat, scaladoc "prefer SortedMap; O(n·m) union" (07 I11) |
| `Rational` without `Eq` (`rationalNoEq` compile error) | deliberately correct — structural `===` would be wrong (Part 2(a)) |

### Rename / re-position

| Item | Action | Reason |
|---|---|---|
| `Value.lovelaceAmount` (v1:719) | rename to an unsafe-marked name (e.g. `unsafeLovelaceHead`) or fold behind docs; keep the fast path | it silently returns *another token's* amount when ADA is absent (02 §A.6, C.14) — but it is 1 machine step vs 895K cpu for `getLovelace` (07 §4.4), so it must survive as a named unsafe fast path, not as an innocent-looking sibling |
| `Utils.getAdaFromInputs/Outputs` (v3:1161/1179) | keep for compat, scaladoc: ADA-only; point to `valueSpentFrom`/`valuePaidTo` | direct cause of the P07/P17 token-stripping hazards (01 §P17) |
| `findOwnScriptOutputs`, `findOwnOutputsByCredential` | keep, scaladoc: payment-credential-only, staking part ignored; point to `findContinuingOutputOrFail` | the middle row of the 08 §3.2 split; AU-4 |
| `Address.fromScriptHash` / `fromPubKeyHash` | keep constructors; scaladoc: produces `stakingCredential = None` — never compare against real outputs | AU-4 mechanism (06); 01 §P06/P16 hazards. Deletion (06's ask) breaks legitimate enterprise-address construction |
| doc comments `getOwnInputsByCredential`/`getOwnOutputsByCredential` (v3:1017, 1036) | fix to match the actual `findOwn…` names | 02 §A.1 naming mismatch |
| `Value.tokens` doc comment (v1:896-898) | fix (shows `quantityOf` returning a SortedMap) | 02 §A.6 |

### Deprecate

| Item | Action | Reason |
|---|---|---|
| **`TxInfo.getValidityStartTime`** (v3:1102) | `@deprecated("use validityLowerBoundOrFail / requireAfter", "1.1.1")` | **live footgun #1** (06 TI-1, ELIMINATE, rank 8): returns `BigInt(0)` on an unbounded bound; caused the Vault finding and the TwoPartyEscrow deposit-time bug (01 §P09); 4 in-repo users to migrate. The security-review skill's V013 misses exactly this case (06 §9) |
| `IntervalBound.finite(default)` | soft-deprecate in high-level docs (keep in prelude) | same invent-a-value trap (06 TI-1); `finiteOrFail` is the sanctioned form |
| `TxInfo.findOwnInput` (Option twin) | keep but document as low-level | Option-sibling prominence produced bare-`.get` misuse in every wild project (08 §5; 06 §6 lesson 1) |

### Fix (bugs and R1 migrations — the two live footguns from 06 are here)

| Item | Action | Reason |
|---|---|---|
| **Boolean-returning callbacks** in `UtxoIndexer.oneToOne/oneToMany/multi*`, `StakeValidator.spend/withdraw`, `TransactionLevelMinterValidator.spend`, `MerkelizedValidator` | migrate to `=> Unit` blocks (new overloads, deprecate old) | **live footgun #2** (06 R1 / EV-1): Boolean callbacks invite short-circuited `&&` chains and discard failure messages |
| `StakeValidator.spendMinimal` scaladoc | add the PU-1 warning ("proves it ran, not with which redeemer") | 06 PU-1 — the `_minimal` sibling is the unsafe one character away |
| `Eq[DCert]`, `Eq[ScriptPurpose]` shadowed-binder self-comparison (v1:310-341, 743-760) | fix bodies | wrong off-chain results (02 §C.20); harmless on-chain only by accident |
| **`Ord[Credential]` vs ledger order** (03 §7.3 callout) | **verify with a real `ScriptContext` test before implementing rows 24/25/26** — if the decoded `withdrawals`/`redeemers` maps are in ledger order (Script < VerificationKey) while `Ord` says the opposite, `SortedMap.get` can return `None` for present keys | unverified but, if real, silently breaks every withdraw-zero helper this API ships; gate CORE rows 24–26 on it |
| `ParameterizedValidator.propose` default body inconsistency (Validator.scala:163-169) | align with the (correct) abstract-everywhere convention | 02 §A.2 |
| `optimize-contract` skill O016/O020 ("use `equalsData` instead of `===`") | fix — stale, `===` already is `equalsData` | 07 §6 verdict table |
| security-review skill | add the 12 missing classes 06 §9 lists (VP-1, VP-2, MI-2, IX-2, EV-1, AU-7, PU-3/4, DE-4, RS-7, VP-5, VP-6); re-frame V004 (no integer overflow on-chain; it is rounding + sign + Int64 serialisation) | 06 §9 |
| `scalus-design-patterns` JVM-only | move to shared cross-platform | JS/Native authors cannot use any pattern today (03 §7.11) |
| Companion-derived `FromData`/`ToData` not linkable; non-inline generic decode helpers | compiler/linker fix | forces `inline def of[A]` and explicit-given workarounds in the two most sophisticated wild codebases (08 §3.6) |
| Lowering: generic `===` behind a type variable emits `equalsData` not `equalsInteger` (2.1x) | lowering improvement (already a known follow-up) | 07 I9, SORTEDMAP findings |
| `List.exists` | consider an intrinsic (drop the Option like `contains` does) | measured full Option tax on every call (07 §4.1) |

### Delete

Nothing. Every deletion candidate (06 wanted `getValidityStartTime` and
`Address.fromScriptHash` gone) is converted to deprecate/document per the
backwards-compatibility policy; MiMa governs the actual surface.

---

## Part 5 — Open questions for the project owner

1. **Fused vs à-la-carte continuation check.** Ship `requireContinuing(ownInput,
   value, datum)` with *required* value+datum args as the headline (06's `continuing`,
   no partial variant), or only the returning `findContinuingOutputOrFail` + separate
   requires? **Recommendation: both, fused form documented as the default** — 01 §6
   ranks the fused form #2 because "the four checks are always written together and one
   is usually weakened", while cosmex's `expectNewState` (08 §2.4) shows users build the
   fused form themselves when given only pieces.

2. **A future `SpendingValidator` layer with mandatory `OwnInputPolicy`** — pursue as a
   new opt-in trait (next to, not replacing, `Validator`) in a later milestone, or
   commit to extensions-only permanently? **Recommendation: defer but keep the door
   open**; re-evaluate when CIP-112 `Observe` lands, since the forwarding trait would
   then swap withdraw-zero for `Observe` invisibly (06 PU-2). The DS toolkit (Part 2(d))
   ships now either way.

3. **`AssetClass` (and opaque `Beacon`) as new small types**, or keep two-parameter
   `(PolicyId, TokenName)` signatures? **Recommendation: two-param now** — matches the
   entire existing `Value` surface (`quantityOf(cs, tn)`), zero migration, and the
   `ValidatorHash`-used-as-`PolicyId` confusion (02 §D.6) is a type-alias problem this
   API cannot fix piecemeal; introduce `AssetClass` only as part of a deliberate
   newtype sweep. Evidence both ways: 04 r12 (4 protocols want canonical ordering),
   06 MI-5/AU-1 want the pairing enforced.

4. **Preamble bundle** `tx.ownInputContext(ownRef)` returning a small
   `OwnInput(input, address, scriptHash, value)` record (one allocation) vs individual
   accessors only? **Recommendation: individual accessors** (`ownScriptHash`, existing
   `findOwnInputOrFail`) — allocation-free, and 07 §1.3's scope-keyed field-access
   memoisation already shares the spine within a scope; revisit with a measured pin if
   the bundle proves ≤ a few machine steps.

5. **Where exactly do Option twins ship?** Everywhere (full Aiken-style pairing) vs
   only the two justified categories from Part 2(b) (at-most-one semantics;
   ledger-shaped optionality)? **Recommendation: the two categories only** — the
   corpus shows Option-primary APIs produce bare-`.get` misuse (08 §5), and the fixed
   constraint already makes fail-fast primary. Cost basis: 07 §4.1.

6. **`getValidityStartTime`: hard-deprecate now** (`@deprecated(..., "1.1.1")`, warning
   on every use) vs docs-only until the replacement ships? **Recommendation:
   hard-deprecate in the same PR that adds `validityLowerBoundOrFail`** — it has
   already caused two in-corpus bugs (01 §P09) and the migration is mechanical for the
   4 in-repo users.

7. **Weak mint assertions**: is a named `requireMint(policy, name, qty)` that does
   *not* pin other token names allowed into EXTENDED (vodka `token_minted` demand,
   EditableNft's deliberate weak spend-path check), or excluded from the assertion
   vocabulary entirely? **Recommendation: exclude** (Part 2(i)) — the weak check stays
   one `require` + `quantityOf` away, and MI-1 is the ecosystem's costliest incident
   class; if it ever ships, the name must state the gap
   (`requireMintsAtLeastIgnoringOtherNames`-grade unpleasantness, per 06 VP-4's
   naming doctrine).

8. **`valuePaidTo` + a minimum-payment obligation**: ship `requirePaidAtLeast` under a
   deliberately unpleasant DS-warning name (06 VP-4's
   `payAtLeastRequiresExclusive` doctrine), or omit the obligation form and ship only
   the `valuePaidTo` query + safe `requirePaidTagged`? **Recommendation: omit the
   `atLeast` obligation in v1** — every DS-1 incident shape in the corpus routes
   through an at-least payout check (06 DS-1); the query + tagged form covers the
   legitimate cases, and `>=` remains writable explicitly by whoever accepts the risk.

*Roadmap note outside this API's scope:* the single most-used third-party API in the
comparable ecosystem is a **deterministic mock-transaction builder** for on-chain-style
tests (mocktail/virgin_*, 3–5x usage over any validation helper — 03 §6.2). Whatever
ships here should be followed by its testkit counterpart, or adoption of the validation
layer will lag for want of a way to test it.
