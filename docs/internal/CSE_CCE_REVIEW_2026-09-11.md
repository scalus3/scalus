# Code review: `worktree-cce-bit-pricing`, range `c8a247859..d315bb9b0`

Reviewed 2026-09-11 at HEAD `d315bb9b0`. Eight commits, 55 files. Every finding below was
reproduced against HEAD with sbt; the numbers in each Evidence line are measured, not estimated.
The review changed nothing in the tree.

This file is the work list for the fix pass. Sections:

1. Constraints for the fix pass
2. Ranked findings (F1 to F16), each with Where / What / Evidence / Fix / Verify
3. Items outside the ranked list (still live)
4. Refuted during review (do not chase)
5. Suggested order of work

Commits in range, oldest first:

| SHA | Subject |
|-----|---------|
| `914f51ad9` | fix: price CCE by encoded bits and actual occurrence count |
| `7175acb94` | fix: price inlining after constant propagation |
| `934397022` | fix: preserve CSE evaluation regions and sharing profitability |
| `27ffbfdfb` | docs: document CSE design, measurements and future work |
| `e86461aba` | feat: public RefScriptFee tiered fee API in scalus-core |
| `78b223522` | refactor: delegate all tier-fee duplicates to RefScriptFee |
| `8a7501730` | perf: optimize Knights and report HTLC fees |
| `d315bb9b0` | refactor: configure RefScriptFee with protocol parameters |

## 1. Constraints for the fix pass

Environment:

- Work in `.claude/worktrees/cce-bit-pricing` on branch `worktree-cce-bit-pricing`.
- Do not run sbt in the primary checkout `/Users/nau/projects/lantr/scalus`. A live sbtn server
  there collides with a second instance.
- Run sbt inside the nix devshell: `nix develop --command sbt ...`. Outside it JDK 25 replaces the
  pinned JDK 21 and `FromDataDerivationTest` falsely fails.
- The `plutus-conformance` symlink must exist in the worktree (see CLAUDE.md).
- One sbt run at a time. For long runs add `-Dsbt.supershell=false -Dsbt.log.noformat=true` and
  redirect output to a file.

Gate before each commit:

```
sbt scalafmtAll
sbt "scalusJVM/testOnly scalus.uplc.transform.*"
sbt scalusExamplesJVM/clean scalusExamplesJVM/test
```

Example validator tests JIT from SIR; incremental builds lie, so `clean` first. Budget pins live
in scalus-examples tests. When an ExUnits or size assertion moves, explain the cause in the commit
message before updating the pin. Some pins are per Scala version
(`ScalaCompilerVersion.baseline(pre38, since38)`); re-measure those on 3.3.8 and 3.8.4.

Commit rules:

- Conventional prefix (`fix:`, `perf:`, `test:`, `docs:`), one or two paragraphs.
- Explicit pathspec: `git add <files>`. Never `git add -A`.
- No co-author trailers, no "generated with" footers.
- No em dashes anywhere. Use an en dash or a comma.

Test rule: for every new test, first run it against the unfixed code and confirm it fails with the
mutation named in the finding's Verify line. Then fix. A test that cannot fail is worse than none;
this range already contains one (F10).

## 2. Ranked findings

Severity: S1 = wrong output or crash on a reachable path. S2 = measured regression against master,
or compile-time blowup. S3 = test cannot fail, docs wrong, duplication. S4 = style.

| # | Sev | Where | One line |
|---|-----|-------|----------|
| F1 | S1 | `Inliner.scala:187` | tryPartialEval folds shared constants with no size guard: 1042 B to 3094 B |
| F2 | S2 | `Inliner.scala:226` | retained Const let re-traverses the body: 2^k, and CSE emits that shape |
| F3 | S1 | `CommonContextExtraction.scala:617` | hole under Delay makes the lazy leaf strict (CCE, off by default) |
| F4 | S2 | `CommonSubexpressionElimination.scala:98` | uniqueBinders blocks sharing of any lambda-containing term |
| F5 | S1 | `CommonSubexpressionElimination.scala:377` | termBits throws on BLS12-381 constants |
| F6 | S2 | `SharingCost.scala:24` | zero credit for avoided evaluation; Escrow +4.3% steps |
| F7 | S2 | `CommonSubexpressionElimination.scala:109` | collect-apply-repeat is O(rounds x n x depth): 6.8 s at 400 groups |
| F8 | S3 | mdx:65, cse-placement.md:52/82, CHANGELOG.md:55 | docs contradict code; CHANGELOG hides default-build growth |
| F9 | S3 | `KnightsTest.scala:458` | benchmark algorithm changed, Plutus reference budget did not |
| F10 | S3 | `CceProfitabilityTest.scala:45` | assertion is tautological |
| F11 | S3 | `CommonContextExtraction.scala:485` | second copy of the exchange rate with a different step model |
| F12 | S3 | `CommonSubexpressionElimination.scala:375` | termBits reimplements Flat[Term].bitSize on a false premise |
| F13 | S3 | `RefScriptFee.scala:35` | public fee() throws above 460,800 bytes |
| F14 | S3 | `CommonSubexpressionElimination.scala:377` | Data constants CBOR-encoded once per CCE template |
| F15 | S3 | `CompareWithLeaderboard.scala:48` | tier-fee dedup commit missed this copy |
| F16 | S4 | `CommonContextExtraction.scala:39`, `Inliner.scala:67` | em dashes in new scaladoc |

Paths below are relative to the repo root. `Inliner.scala`, `CommonContextExtraction.scala`,
`CommonSubexpressionElimination.scala`, `SharingCost.scala`, `TermAnalysis.scala` live in
`scalus-core/shared/src/main/scala/scalus/uplc/transform/`. Their tests live in
`scalus-core/shared/src/test/scala/scalus/uplc/transform/`.

### F1. Inliner: tryPartialEval grows scripts (S1)

Where: `Inliner.scala:187-200` (`tryPartialEval`), fed by the `case c: Const` arm at `:226`.

What: `tryPartialEval` substitutes let-bound constants into any subterm whose free variables are
all known constants, then accepts whatever `Const` `PartialEvaluator.tryEval` returns. Nothing
bounds the result size. A fold over a shared constant copies the constant's payload into every
site, which undoes the sharing decision the pass just made.

Evidence (default pipeline):

| Input | Before | After |
|-------|--------|-------|
| `let c = <1024-byte bytestring> in Constr(0, [appendByteString c c, c])` | 1042 B | 3094 B |
| same with `c` used 4 times | 1042 B | +2055 B, and the let is still kept |

The fold builds a 2048-byte constant, `c` drops to one use and is inlined as well. The same
happens for `serialiseData`, `constrData` and `consByteString` over CSE-shared constants. Master
leaves both inputs unchanged. The branch's own doc records the direction:
`docs/design/cse-measurements.md:7-8`, bilinear-accumulator 588 to 607 bytes.

Fix: accept a folded result only when it does not grow the term:

```scala
PartialEvaluator.tryEval(candidate) match
    case Some(result) if termBits(result) <= termBits(term) => result
    case _                                                  => term
```

Use the same bit function SharingCost uses (see F12). Consider also crediting the avoided
evaluation (F6) so a fold that removes a builtin call but adds bytes is priced instead of only
size-gated.

Verify: new `InlinerTest` cases. (a) The 1024-byte `appendByteString` example: after
`DeBruijn.deBruijnTerm`, `Flat[Term].bitSize(after) <= Flat[Term].bitSize(before)`. (b) The
4-use variant. (c) An existing small fold (`addInteger 1 2` to `3`) still folds. Mutation: remove
the guard; (a) fails.

### F2. Inliner: 2^k re-traversal of retained constant lets (S2)

Where: `Inliner.scala:217-226`. `go(f, constants)` at `:217` already optimized the lambda body.
The arm `case c: Const => go(originalBody, constants.updated(name, c))` at `:226` then optimizes
the original body a second time with the extended environment.

Evidence: k nested retained constant lets over 64-byte constants, one Inliner pass:

| k | wall-clock |
|---|-----------|
| 12 | 39 ms |
| 16 | 294 ms |
| 18 | 1.09 s |
| 20 | 4.12 s |

Doubles per level. The same chain with non-`Const` arguments takes 0.1 ms. The new CSE produces
exactly this shape: on `lam ctx. Constr(0, [bs1 x3 ... bsm x3, ctx])` it emits m nested constant
lets at the root, and the following Inliner pass took 2.0 / 21.5 / 273 ms for m = 8 / 12 / 16.
Extrapolated: m = 24 about 1 minute per pass, m = 28 about 15 minutes, and the pipeline runs the
Inliner 5 to 7 times per compile.

Fix: match the syntactic redex before recursing into `f`, and traverse the body exactly once:

```scala
case Apply(LamAbs(name, body, lamAnn), arg, ann) =>
    val inlinedArg = go(arg, constants)
    val env = inlinedArg match
        case c: Const => constants.updated(name, c)
        case _        => constants
    val inlinedBody = go(body, env)
    // continue with the existing occurrence analysis on
    // LamAbs(name, inlinedBody, lamAnn) and inlinedArg
```

Keep the general `Apply(f, arg)` arm for the case where `f` becomes a `LamAbs` only after
optimization. In that arm do not re-run `go` on the original body; if a constant environment is
needed, run it once on the already optimized body.

Verify: `InlinerTest` with k = 22 nested constant lets under `failAfter(2.seconds)` (current
implementation: about 16 s). Plus: output equality with the current implementation on the existing
`InlinerTest` inputs. Mutation: restore the double traversal; the timing test fails.

### F3. CCE: hole under Delay makes the leaf strict (S1, CCE is off by default)

Where: `CommonContextExtraction.scala:615-623`, the `Delay` arm of `decomposeImpl`. The `Force`
arm (`:605-613`) is safe because `Force` is strict in its argument. `LamAbs` is excluded at
`:588`. `Case` alternatives are already excluded (`:645-650`). `Delay` is the only lazy position
that still receives a hole.

What: a template `lam h. force (ifThenElse c (delay h) (delay 0))` rewrites each site into
`[__cce_... LEAF]`, so LEAF is evaluated before the guard instead of after it.

Evidence (probe at HEAD): three sites `force (ifThenElse c (delay LEAF_i) (delay 0))` with leaves
`headList xs`, `unIData d`, `lengthOfByteString bs`. CCE extracts the 81-bit template (3
occurrences, +21.7 bits). With `c = false` and `xs = []`:

| | Result |
|---|---|
| before CCE | `(constr 0 0 0 0)` |
| after CCE | FAIL on `headList []` |

Homogeneous leaves do NOT reproduce it: CCE then prefers a deeper safe hole. A test with identical
leaves passes and proves nothing; the test must use three different leaves.

Fix, conservative (one line): add `_: Delay` to the exclusion at `:588` so no decomposition puts
a hole under a `Delay`. Optional refinement: keep the `Delay` arm but keep only decompositions
whose leaf is a value form (`TermAnalysis.isValueForm`, `TermAnalysis.scala:132`); evaluating a
value form eagerly cannot fail. Measure the 23-blueprint corpus with `cceEnabled = true` before and
after (`scripts/cse-corpus.py`). CCE is off by default, so no pinned budget should move.

Verify: `CommonContextExtractionTest`: the three-site heterogeneous term, evaluated with the CEK
before and after CCE with `c = false`, `xs = []`; assert equal results. Mutation: remove
`_: Delay` from the exclusion.

### F4. CSE: uniqueBinders blocks sharing of lambda-containing terms (S2)

Where: `CommonSubexpressionElimination.scala:98-104`. `TermKey` compares binder names through
`~=~` (`Term.scala:353-355`).

What: `uniqueBinders` renames the second of two same-named binders before grouping. The comment
at `:90-95` intends this for bodies: `e(x)` under two different `lam x` must not be grouped, which
is correct. But the whole lambdas `lam x. e(x)` and `lam x_cse. e(x_cse)` are alpha-equal, were
shared by the old CSE, and after renaming are structurally different, so they can never be grouped.

Evidence: `LamAbs(map, LamAbs(xs, Constr(0, [e, e])))` with
`e = [map (lam x [addInteger x 1]) xs]` (85 bits, `savingLovelace(e, 2) = 78.6 > 0`) returns
unchanged: zero `__cse_` binders. The control with the lambda replaced by `[addInteger g 1]` IS
shared. CSE at `c8a247859` shared `e`. Any duplicated `xs.map(f)` or fold call,
`Case(xs, [lam h (lam t ...)])` or `Delay(lam ...)` written twice now ships twice and runs twice.
`CHANGELOG.md:54` says "All term kinds are eligible for CSE".

Fix: make grouping alpha-aware instead of pre-renaming. Key each subterm by its de Bruijn form:
bound variables by index, free variables by name. Two closed-under-binder lambdas then hash equal;
two bodies referring to different enclosing binders hash different. Keep `uniqueBinders` only if
the scope machinery needs unique names after grouping; do not let it decide equality. Check that
`DeBruijn.deBruijnTerm` on a subterm with free variables tolerates unbound names (index 0 default),
or write a small structural hash that carries a local binder environment.

Verify: new CSE test with the `map` example above: assert one `__cse_` binder and CEK-equal
results before and after. Mutation: switch the key back to name-based `~=~`.

### F5. termBits throws on BLS12-381 constants (S1, unlikely from @Compile code)

Where: `CommonSubexpressionElimination.scala:377`,
`case Const(c, _) => TermTagBits + flatConstant.bitSize(c)`. Reached from
`SharingCost.savingLovelace` for every repeated `Const` in CSE and every CCE template.

Evidence: `Constr(0, [Const(BLS12_381_G1_Element g), Const(same g)])` through the public
`CommonSubexpressionElimination()(term)` or `V3Optimizer` aborts with
`Flat encoding is not supported for bls12_381_G1_element: use PrepareForSerialization transformer`
(`DefaultUni.blsStubFlat`). `PrepareForSerialization` only runs at `Program.scala:277`, after the
optimizer. The old CSE skipped every `Const`; the old CCE counted nodes. Reachable through
`UplcParser` (`(con bls12_381_G1_element ...)`) or `Constant.lift` of a CEK-produced value.
`PartialEvaluator` refuses to fold to BLS, so `@Compile` code is unlikely to hit it.

Fix: in `termBits`, treat constants the encoder cannot serialize as never profitable (return a
large sentinel, or add `Constant.flatBitSizeOption` returning `None` for BLS and map `None` to
"do not share").

Verify: CSE test with two equal BLS G1 constants: no exception; term unchanged or shared. Mutation:
remove the guard.

### F6. SharingCost: zero credit for avoided evaluation (S2)

Where: `SharingCost.scala:24-29`.

What: `savingBits = (uses - 1) * termBits - uses * VarBits - 2 * TermTagBits`, minus a fixed
`bindingFee`. Nothing credits the evaluation that sharing avoids. Any two-use duplicate under 32
flat bits is refused regardless of its runtime cost. The old rule shared every non-work-free
repeat.

Evidence: `Constr(0, [[length xs], [length xs]])`: `Apply(Var, Var)` is 28 bits, `savingBits = -4`,
so the call runs twice. For a 10-element list that is about 100 CEK steps, about 115 lovelace per
transaction, against about 7.5 lovelace of size "saved". The test "should extract duplicated
Force of non-builtin" was flipped to "should not share two Force nodes when the binding saves no
bits". In this diff the third pin in `EscrowTest` (package `scalus.examples.escrow`) moved from
`ExUnits(102784, 46_412_776)` to `ExUnits(105345, 48_401_763)`, +4.3% steps, while its other two
pins improved.

Fix: pick one and record it in the CHANGELOG.

- (a) Credit evaluation:
  `savingLovelace = savingBits * pricePerBit - bindingFee + (uses - 1) * stepsPerEval(term) * lovelacePerStep`
  where `stepsPerEval` is the static CEK step count of the shared expression's spine (Apply, Force,
  Builtin nodes; not the builtin's own cost, which needs a profile). This is the shape the spec
  `docs/superpowers/specs/2026-09-10-cse-cce-placement-and-refactoring-design.md` section 2.1.3
  asks for, so a future profile can supply measured counts.
- (b) Keep size-only pricing, restore the old floor "share every non-work-free repeat with two or
  more uses", and state the trade-off.

Verify: the flipped test decides. With (a): restore "should extract duplicated Force of
non-builtin" and add `[length xs]` twice, assert shared. Re-measure all three Escrow pins; the
third should come back down.

### F7. CSE collect-apply-repeat cost (S2)

Where: `CommonSubexpressionElimination.scala:109-135` (loop) and `collect` at `:151`.

What: after every single extraction, `collect` reruns over the whole tree. Each run builds an
uncached structural-hash `TermKey` for every node, including leaves that can never pay, plus
`termBits` per group. Cost is O(rounds x n x depth) instead of the old single count pass plus an
O(n) recount.

Evidence: m distinct shareable expressions, three uses each, one CSE pass:

| m | wall-clock |
|---|-----------|
| 50 | 27 ms |
| 100 | 130 ms |
| 200 | 853 ms |
| 400 | 6.8 s |

Log-log slope rises from 2.3 to 3.0. Real scripts today: `auction_1-2` (9.6k nodes) 259 ms for 5
rounds, and CSE runs 2 to 3 times per compile. Becomes seconds to minutes with hundreds of
profitable groups.

Fix: compute hash and bits bottom-up once per `collect` (memo by node identity); skip nodes whose
bits cannot pay even at unbounded uses; extract all independent winners in one round instead of
one per round. Two candidates are independent when neither's occurrences lie inside the other's.
Extract those together, then recollect.

Verify: timing test with m = 400 under `failAfter(2.seconds)`. Output identical to the current
implementation on the transform test corpus and on the 23 blueprints (hashes unchanged, or each
change explained).

### F8. Docs contradict code; CHANGELOG hides default-build growth (S3)

Where and what:

- `scalus-site/content/smart-contract-optimisations/uplc-optimiser-pipeline.mdx:65` still states
  the old Inliner "Multiple" rule (at most 64 bits) and omits the CSE, CCE and second-CSE phases
  (`OptimizerPipelines.scala:57-66`).
- `docs/design/cse-placement.md:52` and `:82` say "CCE's pricing is unchanged". Commit `914f51ad9`
  replaced node-count `MinTemplateSize` with `extractionSavingBits` and `MinTemplateBits`.
- `CHANGELOG.md:55-57` says "builds retaining error traces can grow". `generateErrorTraces = true`
  is the default. `docs/design/cse-measurements.md:271-275` measures HelloCardano 268 to 395 bytes
  (+47%) and AMM deposit fee 402,649 to 445,799 lovelace (+10.7%).

Fix: update all three. In the CHANGELOG state the traced-build growth range and point to
`Options.release` or `generateErrorTraces = false`. Do this after F1 lands and re-measure
HelloCardano and AMM first: F1 is a plausible cause of the traced growth (error-trace strings are
shared constants that `tryPartialEval` can fold into every site). If so, the CHANGELOG line
changes again.

Verify: `scripts/cse-corpus.py` plus the HelloCardanoTest and AMM pins after F1.

### F9. Knights benchmark changed, Plutus reference did not (S3)

Where: `scalus-examples/jvm/src/test/scala/scalus/benchmarks/KnightsTest.scala:458-470`
(`descendants`: `descAndNo` reused, `insertionSort` instead of quicksort), and the
`compareBudgetWithReferenceValue` calls at `:95-98` and `:211-214`. Commit `8a7501730`.

What: the benchmark program changed while `refBudget = ExUnits(160_204421L, 54958_831939L)` is
the unchanged plutus-benchmark algorithm (quicksort, descendants recomputed). The pinned 4x4
budget fell from `ExUnits(146486004, 30533045087)` to `ExUnits(116736784, 24795623419)`, -19%
steps, from the algorithm change and not from the compiler.
`docs/design/knights-htlc-measurements.md` quotes the resulting ratio as a Scalus-vs-Plutus figure.

Fix: keep the reference-shaped program for the comparison (restore quicksort and recomputed
descendants in the program passed to `compareBudgetWithReferenceValue`) and add the optimized
variant as a separate test with its own pin. Or re-measure the Plutus reference with the same
source change and cite the plutus commit. Update `knights-htlc-measurements.md` to say which.

Verify: by inspection the comparison program equals the Plutus reference algorithm; say so in the
test scaladoc with the plutus file path.

### F10. CceProfitabilityTest cannot fail (S3)

Where: `scalus-examples/jvm/src/test/scala/scalus/examples/CceProfitabilityTest.scala:42-50`.

What: the `saved=` value parsed from CCE logs is `extractionSavingBits(nOcc, templateBits)`. The
log line at `CommonContextExtraction.scala:337` is emitted only inside the guard at `:255-257`
that already evaluated the same call `> 0`. So `losers.isEmpty` is always true. It was a valid RED
once, before the guard and the log were aligned.

Fix: assert on the term, not the log. For each extraction assert
`termBits(after) < termBits(before)` (thread the before/after pair out of CCE, or pin
`cce.logs.size` and the total bit delta per validator). See also the locale item in section 3:
under a comma-decimal locale the regex `saved=(-?[0-9.]+)` matches nothing and the test passes
vacuously a second way.

Verify: mutation: replace the body of `extractionSavingBits` with `100.0`; the new test must fail.

### F11. CCE keeps a second copy of the exchange rate (S3)

Where: `CommonContextExtraction.scala:485-524` (`referenceParams`, `LovelacePerStep`,
`LovelacePerBit`, `StepBits`, `MinTemplateBits`, `extractionSavingBits`) versus
`SharingCost.scala:16-22`.

What: both read `CardanoInfo.mainnet` and hand-compute lovelace, with different step models (CCE:
3 x applyCost; SharingCost: apply + lam + var). Same package, same pipeline run
(`cce |> inliner |> cse |> inliner`), no test ties them. The "every CEK step costs the same at
PV11" assumption is stated twice and enforced nowhere.

Fix: expose `lovelacePerBit`, `lovelacePerStep`, `lovelace(ExBudget)` and `stepBits` from
`SharingCost`; make CCE consume them. Keep `MinTemplateBits` derived, and declare it after
`StepBits`: object vals initialise in order, and a forward reference silently yields 17 instead of
40.

Verify: one test asserting CCE's n = 2 break-even equals the value computed from SharingCost's
constants.

### F12. termBits reimplements the encoder on a false premise (S3)

Where: `CommonSubexpressionElimination.scala:360-392`.

What: the scaladoc says `Flat[Term].bitSize` throws on unassigned de Bruijn indices.
`NamedDeBruijn.index` defaults to 0 and the encoder throws only for a negative index
(`Term.scala`, the `Var` encoder), which only `DeBruijn.deBruijnTerm` produces on a scoping error.
Measured on optimizer inputs, encoder versus `termBits`: `vr"x"` 12/12, a lambda 47/47, a CCE
template containing `__CCE_HOLE__` 43/43, a Data-constant application 89/89, no throw. The only
real divergence is the deliberate fixed 12-bit `Var` (the encoder uses 12 or 20 bits by index).

Fix: delegate to `Flat[Term].bitSize` with a `Var` override (a wrapper that counts every `Var` at
12 bits and everything else through the encoder). Any encoder change (Constr tag, list framing,
constant sizes) then cannot desynchronise pricing from what the chain charges. If the copy stays,
rewrite the scaladoc to state the true reasons: fixed `Var` width and the hole sentinel.

Verify: property test: for de Bruijn'd terms with all indices below 128,
`termBits == Flat[Term].bitSize`.

### F13. RefScriptFee.fee throws above 460,800 bytes (S3)

Where: `scalus-core/shared/src/main/scala/scalus/cardano/ledger/RefScriptFee.scala:31-39`.

What: `go` accumulates an exact `NonNegativeInterval`. The reduced numerator grows like 6^k and
`NonNegativeInterval.reduceBigInt` throws once it no longer fits a `Long`. The method is public,
`@static`, MiMa-stable, and guards only `sizeInBytes >= 0`. Simulated with the same
reduce-after-each-op arithmetic: `fee(460800, 15)` returns 49,196,799; `fee(460801, 15)` throws
(18 full strides plus one byte). A larger `costPerByte` overflows earlier. `MinTransactionFee`
never reaches it because the ledger caps reference-script bytes per transaction at 204,800.

Fix: either `require(sizeInBytes <= MaxRefScriptSizePerTx)` with the constant named and
documented, or accumulate in `BigInt` / `BigDecimal` and floor once at the end, as the ledger
formula does.

Verify: `fee(460801, 15)` returns a value; `fee(204800, 15)` equals the existing pinned ledger
value.

### F14. Data constants CBOR-encoded once per CCE template (S3)

Where: `CommonSubexpressionElimination.scala:377` calls `flatConstant.bitSize`, which for
`Constant.Data` CBOR-encodes the value with no memoisation. CCE prices every single-hole
decomposition before the `MinTemplateBits` filter.

Evidence: a 2102-byte `Data.List` under 15 distinct applications: 120 CBOR encodes per CCE pass
(3.1 ms). Inside a 4-arm `Constr`: 604 encodes (26 ms). CSE encodes once per repeated group per
round. Master priced in node count and never encoded.

Fix: memoise constant bits by identity within a pass (an `IdentityHashMap`), or compute Data sizes
once in the bottom-up pass from F7.

Verify: the F7 timing test covers it. Optional: an encode counter in a test.

### F15. CompareWithLeaderboard missed by the dedup commit (S3)

Where: `scalus-examples/jvm/src/main/scala/scalus/examples/cape/CompareWithLeaderboard.scala:42-55`.

What: commit `78b223522` "delegate all tier-fee duplicates to RefScriptFee" missed this copy:
`tierSize = 25L * 1024`, `price * 1.2`, hard-coded 15 lovelace per byte, and literal
`0.0577` / `0.0000721` execution prices. When mainnet's `minFeeRefScriptCostPerByte` or the ex-unit
prices change, the leaderboard comparison and the `RefScriptFee`-based pins (`HtlcCapeTest`,
`EqualsDataVsTypedComparisonTest`) disagree by construction.

Fix: `RefScriptFee.fee(size, CardanoInfo.mainnet.protocolParams.minFeeRefScriptCostPerByte)` and
the ex-unit fee from the same params. If CAPE's ceiling rounding must be kept for leaderboard
parity, keep it and say so in a comment.

Verify: the leaderboard comparison numbers for current submissions do not change.

### F16. Em dashes in new scaladoc (S4)

`CommonContextExtraction.scala:39` and `Inliner.scala:67`. The other hits in `Inliner.scala`
(`:62-65`, `:130`) are pre-existing. Replace with an en dash or a comma. Both render into the
published Unidoc.

## 3. Items outside the ranked list (still live)

- `cseIterations = 0` no longer disables CSE once `cceEnabled = true`:
  `OptimizerPipelines.scala:66` runs `cse.apply` unconditionally after CCE. Gate it on
  `cseIterations > 0`, or document that CCE implies one CSE pass.
- Locale-dependent log format: `CommonContextExtraction.scala:337` uses
  `f"...$savedBits%.1f"`, which follows the JVM default locale. Under a comma-decimal locale the
  `CceProfitabilityTest` regex matches nothing. Format with `Locale.ROOT` or without `f`.
- Dead `@deprecated` forwarders at `CommonSubexpressionElimination.scala:280-289`: kept for MiMa.
  Add MiMa filters and delete, or keep them with a comment naming the version that removes them.
- `termSize` at `CommonSubexpressionElimination.scala:394-402` has no callers since the bit
  pricing change. Delete.
- CCE `isSkippable` rescue for `Force(Builtin)` and `Force(Force(Builtin))`
  (`CommonContextExtraction.scala:449-467`) is dead under `MinTemplateBits = 40`: those templates
  are 15 and 19 bits and are filtered earlier. Delete the arms, or add a test proving they fire.
- `scripts/cse-corpus.py:51` hardcodes `target/scala-3.3.8`. Take the Scala version from an
  argument, or glob `target/scala-*`.

## 4. Refuted during review (do not chase)

- `@static` with `using` on `RefScriptFee` compiles on JS and Native.
- `NonNegativeInterval(12, 10)` equals the old `1.2` multiplier exactly.
- `CardanoInfo.mainnet` is embedded at compile time; `SharingCost` does no runtime file read.
- Post-CCE CSE re-hoisting through the `__cce_` prefix exemption in `referencesPartialBuiltin`
  (`CommonContextExtraction.scala:437`) is moot at HEAD: region placement blocks the hoist (probe:
  0 extractions, before equals after). The exemption is still accidental; tighten it when working
  on F11.

## 5. Suggested order of work

Do now. Each step unblocks measurement of the next:

1. F16, F10, F3 (conservative fix). Mechanical. About one hour together.
2. F2, then F1. Both in `Inliner.scala`. About half a day with tests.
3. F4. About half a day. Re-measure the 23 blueprints afterwards.
4. F6. Decide (a) or (b), about two hours. Re-pin Escrow.
5. F8. After steps 2 to 4 land, because the numbers will move.

Later:

6. F7 and F14 together (one bottom-up pass), then F12, then F11.
7. F5, F13, F15, and the section 3 items.

After each of steps 2 to 4: run `scripts/cse-corpus.py` and record the 23-blueprint total in
`docs/design/cse-measurements.md` with the commit SHA.
