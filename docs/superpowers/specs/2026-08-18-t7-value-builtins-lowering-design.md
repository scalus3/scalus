# T7 Phase 1: Lower prelude Value through CIP-153 builtins (design)

Status: approved 2026-08-18. Implements the first phase of T7 from
`docs/internal/CODEGEN_IMPROVEMENT_PLAN.md`.

## Goal

When compiling for PV11 (vanRossem), lower the hot `plutus.v1.Value`
operations to the CIP-153 MaryEraValue builtins
(`lookupCoin`/`unionValue`/`scaleValue`/`valueContains`) instead of the
linked SIR pair-list walks. PV10 output stays byte-identical to today.

## Evidence (measured 2026-08-18, 5 policies x 2 tokens, PV11 mainnet costs)

Per-operation cpu, prelude vs builtin (see
`scalus-core/jvm/src/test/scala/scalus/uplc/eval/ValueBuiltinsBudgetTest.scala`):

| op | prelude | builtin | ratio |
|---|---:|---:|---:|
| quantityOf / lookupCoin | 12.7M | 0.99M | 13x |
| plus / unionValue | 125.9M | 4.1M | 31x |
| multiply / scaleValue | 58.0M | 3.3M | 18x |
| containment / valueContains | 110.3M | 1.5M | 75x |

`unValueData` costs ~3.6M for that value, so conversion amortizes against
even a single operation. `fromData[Value]`/`toData` are already free
(identity) under the V3 pipeline, so the conversion into `BuiltinValue` is
the only new fixed cost.

## Decisions (user-approved)

1. **Default ON at PV11**, with an `Options` opt-out flag. The strict
   CIP-153 validation that comes with `unValueData` is treated as a
   feature (free deep datum validation, T9 direction), not a regression.
2. **Staged delivery.** Phase 1 = method intrinsics only (this spec).
   Phase 2 = a `BuiltinValueBacked` representation, driven by Phase 1
   measurements (sketch at the end).
3. **New API:** `Value.containsAtLeast` is added, with a portable body
   whose semantics mirror `valueContains` exactly on every PV.

## Semantic change at PV11 (documented, opt-out)

`unValueData` (CIP-153, mirrors `PlutusCore.Value.buildValueWith`,
implementation `scalus-core/.../uplc/eval/BuiltinValueOps.scala:200`)
REQUIRES canonical form and fails otherwise:

- currency symbols and token names strictly ascending (no duplicates),
- no zero quantities, no empty inner maps,
- keys at most 32 bytes,
- quantities within +-(2^127).

`unionValue`/`scaleValue` additionally fail when a resulting quantity
leaves the 128-bit range; `valueContains` fails when either side holds a
negative amount. Today's prelude walks tolerate all of that.

Consequence: at PV11 with the flag on, a lowered `Value` operation applied
to a malformed (non-canonical) Value makes the script fail where it
previously succeeded. Ledger-produced ScriptContext values are canonical;
datum-embedded values are user-controlled and are exactly the values that
now get validated for free. The changelog and the `Options` scaladoc must
state this.

Opt-out: `Options.valueBuiltins = false` restores the portable lowering at
any PV.

## Design

### Gating

- New field `valueBuiltins: Boolean = true` on `scalus.compiler.Options`.
- Effective condition: `valueBuiltins && targetProtocolVersion >= vanRossemPV`.
- Mechanism: the provider module `ValueIntrinsicsV11` is included in the
  `intrinsicModules` map handed to the lowering only when the flag is on
  (wired where `IntrinsicResolver.defaultIntrinsicModules` is consumed:
  `SirToUplcV3Lowering.scala:161-162`, `UplcPipeline.scala:67-68`). An
  absent provider makes `LoweringContext.findProviderBinding` return
  `None`, and `IntrinsicResolver.tryResolve/tryResolveFull` silently fall
  back to the linked SIR body.
- PV gating uses the registry entry's `minProtocolVersion = 11`, the
  pattern proven by `BuiltinListOperationsV11`
  (`IntrinsicResolver.scala:157-160`).
- MiMa: adding a case-class field to `Options` changes
  `apply`/`copy`/`unapply` signatures. Check how post-1.0.0 fields (e.g.
  `cseIterations`, `cceEnabled`) were handled; add explicit overloads or
  MiMa filters as that precedent dictates.

### The intrinsic module

New file
`scalus-core/shared/src/main/scala/scalus/compiler/intrinsics/ValueIntrinsics.scala`:

- `@Compile object ValueIntrinsicsV11` with defs whose simple names and
  arity match the compiled `Value$` methods exactly (the resolver matches
  `s"$providerModule.$methodName"` and requires
  `countTopLambdas(body) == argCount`).
- `ValueReprRules`: per-method `ReprRule` map (mandatory for
  `WildcardRepr` entries). `Value` results report
  `OneElementWrapper(PackedDataMap)`; `BigInt`/`Boolean` results use their
  default representations.
- Registry entry in `IntrinsicResolver.registry`:
  `"scalus.cardano.onchain.plutus.v1.Value$" ->
  List((WildcardRepr, 11, "scalus.compiler.intrinsics.ValueIntrinsicsV11$",
  ValueReprRules.rules, NoArgConvert))`.
- Bodies use the free `Value <-> Data` relabel (the runtime bytes of a
  `Value` ARE the Data map, `OneElementWrapperEmitter`), via
  `IntrinsicHelpers.typeProxy`/`typeProxyRepr`, around the builtins from
  `scalus.uplc.builtin.Builtins`.

### Operation mapping

| Value$ method | PV11 lowering | notes |
|---|---|---|
| `quantityOf(v, cs, tn)` | `lookupCoin(cs, tn, unValueData(v))` | also serves `getLovelace` (defined via `quantityOf`) |
| `plus(a, b)` | `valueData(unionValue(a', b'))` | `a' = unValueData(a)` etc. |
| `minus(a, b)` | `valueData(unionValue(a', scaleValue(-1, b')))` | no subtract builtin |
| `multiply(v, factor)` | `valueData(scaleValue(factor, v'))` | |
| `negate(v)` | `valueData(scaleValue(-1, v'))` | |
| `containsAtLeast(a, b)` (new) | `valueContains(a', b')` | |

Explicitly NOT lowered in Phase 1:

- `eq`/`===`: `given valueEq = Eq.structural(...)`, so `===` already
  compiles to `equalsData` on the Data bytes - cheap; direct `Value.eq`
  calls keep the zero-tolerant walk (different semantics from structural
  equality on non-canonical values).
- `flatten`, `policyIds`, `tokens`, `withoutLovelace`, `isPositive`,
  `lovelaceAmount`: no builtin counterpart; they keep the pair-list walk.
- `toData`/`fromData`: already identity relabels under V3.
- `zero`, `apply`, `lovelace`, `fromList` variants: construction is
  already a cheap `mapData`; `insertCoin`-based construction is marginal.
  Revisit in Phase 2.

Chained operations pay a `valueData` + `unValueData` roundtrip between
steps in Phase 1. Accepted: even with the roundtrip each step stays >=10x
cheaper than the current walk. Phase 2 removes it.

### New API: `Value.containsAtLeast`

Extension on `Value` in `plutus/v1/Value.scala`:
`a.containsAtLeast(b)` is true when for every (policy, token, amount) in
`b`, `a` holds at least that amount. The portable body must mirror
`valueContains` semantics EXACTLY, including failing (throwing) when
either side contains a negative amount, so PV10 and PV11 agree. Scaladoc
documents the canonical-form requirement and the PV11 lowering.

## Validation

1. **Lowering tests** (pattern:
   `compiler/sir/lowering/IntrinsicResolverTest.scala`): at PV11 the
   lowered term contains `LookupCoin`/`UnionValue`/... builtins; at PV10,
   and at PV11 with `valueBuiltins = false`, it does not and is unchanged.
2. **Differential semantics tests**: property tests generating canonical
   Values, asserting prelude walk == builtin lowering results for every
   mapped op; explicit tests that malformed Data (zeros, unsorted, dup
   keys, oversized keys) fails at PV11 and still passes the portable
   lowering.
3. **Budgets**: extend `ValueBuiltinsBudgetTest` to compile the prelude
   methods themselves at PV11 and assert they now hit ~builtin costs.
   Re-pin corpus budgets with `scripts/update-budgets.py`, on BOTH
   compiler generations (`ScalaCompilerVersion.baseline` dual pins).
4. **Full `sbtn ci`** including MiMa; changelog entry describing the
   behavior change and the opt-out.

## Risks

- `Options` binary compatibility (see Gating).
- Example/validator budget churn: any PV11-compiled script using Value
  ops changes cost and script hash; expect broad re-pins.
- Behavior change on malformed Values (documented, flag-gated).
- Intrinsic authoring subtleties: extension-method desugaring can insert
  evidence params between `self` and explicit params; verify each target
  method's compiled arity (none of the mapped methods use context bounds,
  but verify against the compiled SIR).

## Phase 2 sketch (separate spec later)

`BuiltinValueBacked` product representation + a
`SirTypeUplcConvertingGenerator` for the `Value` type (name-keyed PV11
dispatch beside the `BuiltinArray` arm in
`SirTypeUplcGenerator.scala:374-382`), `ArgReprConvertRule`s so intrinsic
args convert via the memoized per-variable representation cache
(`VariableLoweredValue.otherRepresentations`), match/select support,
equality routing in `LoweringEq`, and the
`PrimitiveRepresentation.Constant.defaultUni` fix for `BuiltinValue`
(`LoweredValueRepresentation.scala:1624` maps it to `DefaultUni.Data`).
Values then stay native across op chains and cross the Data boundary once.
