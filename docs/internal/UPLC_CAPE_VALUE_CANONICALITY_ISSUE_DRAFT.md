# Superseded: see UPLC_CAPE_VALUE_CANONICALITY_PR.md

This issue draft is superseded (2026-08-25). The bug it described - the UPLC-CAPE fixture builder
emitting non-canonical `Value` key order, rejected by the real `unValueData` - has been root-caused
and **fixed on a prepared PR branch** instead of filed as an issue:

- PR body, root cause with citations, blast radius, and the exact submit commands:
  `docs/internal/UPLC_CAPE_VALUE_CANONICALITY_PR.md`
- Branch: `fix/canonical-value-ordering` in `/Users/nau/projects/lantr/UPLC-CAPE` (2 commits: the
  `lib/Cape/Tests.hs` fix + regenerated `metrics.json` for all 11 committed `linear_vesting`
  submissions). Not pushed; the user reviews and submits.

One correction to what this draft said: the faulty conversion is `buildValue` in
`lib/Cape/Tests.hs` (folding `<>` over the Data-backed `PlutusLedgerApi.Data.V3.Value`, whose
`AssocMap`-based Semigroup appends the left operand's keys last), not
`lib/Cape/ScriptContextBuilder.hs`. The Scalus-side interim workaround
(`Options.valueBuiltins = false` in `LinearVestingContract.scala`) has been reverted.
