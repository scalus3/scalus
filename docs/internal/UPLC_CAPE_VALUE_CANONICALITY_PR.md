# Prepared upstream PR: canonical `Value` key order in the fixture builder

**Status: PREPARED, NOT SUBMITTED.** The branch `fix/canonical-value-ordering` in the local clone
`/Users/nau/projects/lantr/UPLC-CAPE` carries two commits, ready for review and manual submission:

- `d807877` `tests: emit canonical Value key order in the fixture builder` (the fix,
  `lib/Cape/Tests.hs`)
- `b8b165a` `submissions(linear_vesting): re-measure under canonical fixture Value order`
  (regenerated `metrics.json` for all 11 committed `linear_vesting` submissions; no `.uplc` or
  `metadata.json` touched)

This document is the PR body. It supersedes the old issue draft
(`UPLC_CAPE_VALUE_CANONICALITY_ISSUE_DRAFT.md`, now a pointer to this file): the root-cause
attribution below corrects the draft (the bug lives in `lib/Cape/Tests.hs`'s `buildValue` +
plutus-tx's Data-backed `AssocMap` union semantics, not in `ScriptContextBuilder.hs`), and the
work is now a fix PR, not an issue.

## How to submit (user runs these; none were run by the preparing task)

```bash
cd /Users/nau/projects/lantr/UPLC-CAPE
git log --oneline main..fix/canonical-value-ordering     # review: 2 commits
git diff main...fix/canonical-value-ordering             # review the full diff

# Direct branch push (if you have write access) ...
git push origin fix/canonical-value-ordering
# ... or fork flow:
# gh repo fork IntersectMBO/UPLC-CAPE --remote --remote-name fork
# git push fork fix/canonical-value-ordering

gh pr create --repo IntersectMBO/UPLC-CAPE \
  --base main --head fix/canonical-value-ordering \
  --title "fix: emit canonical Value key order in the fixture builder" \
  --body-file docs/internal/UPLC_CAPE_VALUE_CANONICALITY_PR.md   # paste from "PR body" below
```

(When pasting, use only the "PR body" section below - the parts above this line are Scalus-internal
context.)

---

# PR body

## Problem

The Haskell test-fixture builder emits non-canonical `Value` key order for **any**
`{lovelace, assets}` value spec that mixes a native asset with lovelace: the custom policy's
currency symbol appears **before** ADA's empty-bytestring policy in the outer map - the reverse of
canonical (strictly ascending byte-lexicographic) order. Real ledger-produced `Value`s are always
canonically sorted, and CIP-0153's `unValueData` builtin rejects non-canonical encodings outright
instead of normalising them. So any submission - in any language - whose validator decodes a
fixture-provided multi-asset `Value` with `unValueData` fails with a machine error even though the
validator logic is correct.

Observed against `scenarios/linear_vesting/cape-tests.json`'s `successful_partial_unlock` baseline
(the only committed scenario using multi-asset values): all 4 `partial_unlock_*` measurement tests
of a CIP-0153-based submission abort identically with

```
The machine terminated because of an error, either from a built-in function or from an explicit use of 'error'.
Caused by: unValueData
             (Map
                [ ( B #dddddddddddddddddddddddddddddddddddddddddddddddddddddddd
                , Map [(B #76657374, I 1000)] )
                , (B #, Map [(B #, I 2000000)]) ])
```

The outer key order is `[0xdd..dd (28-byte custom policy), "" (ADA)]`. The empty bytestring is the
lexicographically smallest possible key, so a valid encoding must list ADA's entry first; this
value is in exactly reversed order. Both the spending input's value (quantity 1000) and the
continuing output's value (quantity 900) are affected - the same builder code path builds each.

## Root cause

`buildValue` (`lib/Cape/Tests.hs`) combines the lovelace entry with the resolved asset entries as

```haskell
pure $ foldl' (<>) adaValue assetValues
```

over `PlutusLedgerApi.Data.V3.Value` - the *Data-backed* ledger `Value`. That type's `Semigroup`
makes no ordering guarantee:

- `instance Semigroup Value` is `unionWith (+)`
  (`plutus-ledger-api-1.45.0.0` / `1.65.0.0`, `src/PlutusLedgerApi/V1/Data/Value.hs:331-333` /
  `302-304`), and `unionWith`'s `unionVal` delegates to `Map.union` over the Data-backed
  `PlutusTx.Data.AssocMap` (`Value.hs:431` / `:402`).
- `PlutusTx.Data.AssocMap.union` (`plutus-tx-1.45.0.0` / `1.65.0.0`,
  `src/PlutusTx/Data/AssocMap.hs:335-393`) computes
  ``res = goLeft ls `safeAppend` goRight rs``, and `safeAppend` folds the left map's entries into
  the right map's list with `insert'` (`AssocMap.hs:146-164`), which **appends a missing key at the
  end of the list** (its `nilCase`).

So for the disjoint-key case here, `adaValue <> assetValue` produces the *right* operand's keys
first and the left operand's keys last: `[0xdd.., ""]`. Empirical confirmation on the production
pin (plutus-core 1.45, `cabal repl lib:cape`):

```haskell
ghci> B.builtinDataToData (toBuiltinData (foldl' (<>) ada [asset]))
Map [(B "\221\221...\221", Map [(B "vest", I 1000)]), (B "", Map [(B "", I 2000000)])]
```

The same code is present in plutus-tx 1.45 and 1.65, so both the production and preview measure
binaries are affected. This is not a plutus-tx bug: the Data-backed `AssocMap` documents itself as
an unordered association list, and *on-chain* nothing ever needs to reorder it. The bug is using an
insertion-ordered union to *construct* a transaction-context `Value` that the real ledger would
have produced in canonical order.

## Why canonical order is required (not a quirk of one compiler)

`unValueData`'s implementation (`plutus-core-1.65.0.0`, `src/PlutusCore/Value.hs:469-499`)
delegates to `buildValueWith` (`Value.hs:543+`), whose documented contract is:

> It fails unless the following conditions are met: currency symbols are strictly ascending, token
> names are strictly ascending, every quantity is within bounds, no zero quantity.

Key comparison is newtype-derived from `ByteString`'s lexicographic `Ord` (`Value.hs:81-82`), under
which the empty bytestring sorts first. The builtin deliberately does not normalise - it is a cheap
O(n) parse that assumes the canonical form every real ledger-constructed `Value` has. Every
CIP-0153 consumer shares this behaviour, so every language/compiler that lowers value lookups to
`unValueData` (Plinth, Scalus, ...) hits this failure on the fixture-built values. The fixture JSON
itself is fine; only the JSON-to-`Data` conversion is at fault.

## Fix

`buildValue` now re-sorts the folded result into canonical order before returning it
(`canonicalValue` in `lib/Cape/Tests.hs`): the `Value`'s `Data` encoding is unpacked, the outer
currency-symbol map and (defensively) each inner token-name map are sorted into strictly ascending
byte-lexicographic key order, and the result is repacked. The fold is kept, so duplicate-key
merging semantics are unchanged; sorting a 1-entry map (every lovelace-only value) is a no-op.
There is no canonicalising constructor in the Data-backed ledger API to delegate to, hence the
explicit sort. `buildValue` is the single `Value`-construction point for script contexts, so this
covers inputs, outputs, and any future value spec.

## Blast radius: re-measured metrics (second commit)

Canonical order changes how many steps map-walking validators take to reach the vesting token's
policy (it now sorts after ADA instead of first), so measured costs of all committed
`linear_vesting` submissions shift by roughly +2..8% cpu. All executions still succeed - no
submission's result flips. Regenerated `metrics.json` (summed over measurement evaluations):

| Submission | cpu sum old -> new | mem sum old -> new | total fee (lovelace) old -> new |
|---|---:|---:|---:|
| Plinth_1.45.0.0_Unisay | 245,143,090 -> 264,713,442 | 741,040 -> 786,880 | 85,483 -> 89,539 |
| Plinth_1.45.0.0_Unisay_plain | 740,659,238 -> 760,101,590 | 2,662,974 -> 2,708,014 | 250,376 -> 254,376 |
| Plinth_1.61.0.0_Unisay_preview | 467,388,202 -> 478,224,546 | 1,723,409 -> 1,739,417 | 153,195 -> 154,900 |
| Plinth_1.64.0.0_Unisay | 220,599,042 -> 238,505,394 | 584,608 -> 620,048 | 65,598 -> 68,934 |
| Plinth_1.64.0.0_Unisay_plain | 624,764,959 -> 642,799,311 | 2,143,120 -> 2,179,360 | 192,239 -> 195,630 |
| Plinth_1.64.0.0_Unisay_preview | 416,838,446 -> 427,674,790 | 1,534,593 -> 1,550,601 | 138,161 -> 139,866 |
| Plinth_1.65.0.0_Unisay | 191,890,594 -> 201,238,122 | 512,166 -> 543,862 | 58,343 -> 60,846 |
| Plinth_1.65.0.0_Unisay_plain | 583,020,959 -> 600,159,311 | 1,882,220 -> 1,912,860 | 173,470 -> 176,474 |
| Plinth_1.65.0.0_Unisay_preview | 140,515,320 -> 143,737,440 | 441,234 -> 459,098 | 48,971 -> 50,234 |
| Scalus_0.18.2_Unisay | 307,452,349 -> 328,355,965 | 873,596 -> 950,908 | 93,619 -> 99,587 |
| Scalus_0.18.2_Unisay_preview | 224,680,257 -> 234,107,961 | 653,903 -> 700,543 | 70,130 -> 73,501 |

Only `metrics.json` files are touched; every `.uplc` and `metadata.json` is byte-identical. No
other scenario's metrics change: `linear_vesting` is the only committed scenario whose fixtures use
`assets` (grep `'"assets"'` under `scenarios/`), and lovelace-only values take `buildValue`'s
untouched `[] -> pure adaValue` branch. Spot-checked by re-measuring `htlc/Plinth_1.65.0.0_Unisay`
and `two_party_escrow/Scalus_0.18.2_Unisay` with the fixed builder: byte-identical `metrics.json`.

## Verification

- `cabal test`: 117 examples, 0 failures.
- `cape submission verify` green on all 7 current-track `linear_vesting` submissions
  (`Plinth_1.45/1.64/1.65 x {default, _plain}`, `Scalus_0.18.2_Unisay`), plus `metrics.schema.json`
  / `metadata.schema.json` validation for the 4 re-measured `_preview` submissions.
- `treefmt` (fourmolu): no reformatting needed.

## How to reproduce

Before the fix (on `main`):

```bash
nix develop --command cabal repl lib:cape
ghci> :set -XOverloadedStrings
ghci> import qualified PlutusLedgerApi.Data.V3 as V3
ghci> import qualified PlutusTx.Builtins as B
ghci> let ada = V3.singleton V3.adaSymbol V3.adaToken 2000000
ghci> let asset = V3.singleton "dddddddddddddddddddddddddddddddddddddddddddddddddddddddd" "vest" 1000
ghci> B.builtinDataToData (V3.toBuiltinData (ada <> asset))
Map [(B "\221...\221", Map [(B "vest",I 1000)]), (B "", Map [(B "",I 2000000)])]   -- reversed
```

With the fix, `buildValue`'s output for the same spec is
`Map [(B "", ...), (B "\221...\221", ...)]` - canonical. End to end: measure any
`linear_vesting` submission whose validator uses `unValueData` (e.g. a Scalus 1.1.0 build with its
default CIP-0153 `Value` lowering) with `measure-preview`; before the fix its 4 `partial_unlock_*`
tests abort in `unValueData`, after the fix all 29 tests pass.
