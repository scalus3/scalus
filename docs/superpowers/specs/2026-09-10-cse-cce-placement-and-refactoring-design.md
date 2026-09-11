# CSE/CCE: binding placement and refactoring

Date: 2026-09-10. Status: historical design; CSE placement is superseded by
[CSE binding placement](../../design/cse-placement.md). The implementation uses Plutus's
ancestor-only rule and trace contract. The all-branches rule and shared cost model below
remain deferred, as does the CCE refactoring.

Scope agreed with the owner: **Phase 0 (test oracle), Phase 1 (placement rule), Phase 3
(collect-apply-repeat), Phase 4 (naming and dedup)**. The generic traversal (Phase 2) is
described under "Deferred" for continuity but is not part of this work.

---

## 1. Motivation

`CommonSubexpressionElimination` (564 lines) and `CommonContextExtraction` (724 lines)
are 40% of the code in `scalus/uplc/transform/`, and the hardest to reason about. Three
structural problems, in order of importance.

### 1.1 The placement rule is more permissive than either reference compiler

Both passes place the new binding at `longestCommonPrefix` of the occurrence paths
(`CommonSubexpressionElimination.scala:149`, `:240`; `CommonContextExtraction.scala:159`,
`:259`). The LCP of two paths in *different* branches is their parent, which need not be
a point where the expression was evaluated at all. The binding can therefore land
"earlier" than any original occurrence.

Because of that, a cluster of compensating checks is needed to decide when the move is
acceptable: `crossesConditional`, `unsafeCaseCrossing`, `referencesPartialBuiltin`,
`shapePartialBuiltins`, `partialBuiltinVarPrefixes`, plus two `startsWith("__")` name
heuristics. That cluster has two maintenance problems:

- It is a **hand-maintained list** that has drifted from `DefaultFun.isTotal`, which is
  the authoritative source and is already correct. Nothing mechanically links them and
  no test asserts consistency, so a newly added builtin is not covered by default.
- It decides on the wrong axis. `crossesConditional` is an `exists` over occurrence
  paths, so it cannot distinguish "the expression is in *some* branches" from "it is in
  *every* branch", and it then discriminates by *which builtin is involved* rather than
  by whether the expression is evaluated on all paths from the bind point.

Neither reference compiler needs this machinery. Plutus
(`UntypedPlutusCore/Transform/Cse.hs:344-367`) merges occurrences only along
**ancestor-or-self**, so every candidate is anchored at a real occurrence, and it
therefore has no purity, free-variable or shadowing checks in CSE at all. Aiken restricts
hoisting to closed constant partial applications
(`crates/uplc/src/optimize/shrinker.rs:2615`), which makes the question vacuous.

### 1.2 `Path` is not a path

`private type Path = Vector[Int]`, but the `Int`s are IDs from a **stateful counter**
allocated during a traversal, re-created from zero four times per pass. Inserting one
binding renumbers every path after it, so stored paths are invalidated by any edit
earlier in pre-order. `CommonContextExtraction` carries an explicit "Path ID
synchronization invariant" comment acknowledging the fragility.

This is the sole reason for the `re*` vocabulary: `reCount`, `reCollect`,
`reIsSafeToHoist`, `reVarsInScope`, `reFreshId`, `rePathIdToVar`, `reConditionalPathIds`,
`reSafe`, `reCrossesConditional`, `reUnsafeCaseCrossing`, `reDistinctLeaves`. Every
candidate triggers a full re-traversal and a full re-derivation of its safety checks,
including the rejected majority.

### 1.3 The tests cannot fail

The two properties that claim to check semantic preservation are vacuous. Their generator
emits only `Add`/`Multiply`/`Subtract` over integer constants, so no `Case`, `Delay`,
`Error` or lambda is ever produced, and the `case (None, None) => succeed` arm is
unreachable. `Term.evaluate` uses `NoBudgetSpender`, so a divergent generated term would
hang CI. ScalaTest's default `minSuccessful` is 10, not 100.

No CEK-conformance test exists for either pass, though `LetChainRegroupCekConformanceTest`
is a good in-repo model.

---

## 2. Design

### 2.1 Placement

Replace LCP-plus-guards with an explicit **down-safety** condition, in the Lazy Code
Motion sense: is the candidate evaluated on every path forward from the bind point?

```
bindAt(p) is admissible iff  p is itself an occurrence
                          || every path from p reaches an occurrence
```

Both clauses are down-safety, so a binding only ever *relocates* an evaluation that was
going to happen anyway. The number of evaluations is unchanged and the only new cost is
the fixed `let` framing, which means **no cost model and no execution profile is needed
to decide admissibility**.

- The **first** clause is Plutus's ancestor-or-self rule.
- The **second** covers the case where an expression appears in *every* branch of a
  `Case`. Exactly one branch is selected and evaluated, so the expression is evaluated
  exactly once either way and the binding may be placed above the `Case`. **Plutus misses
  this**: `combinePaths` never merges siblings, so occurrences spread across all branches
  stay at count 1 and are never considered. Two branches that both destructure the same
  datum produce this shape, which is common in validators, so this clause is expected
  to carry most of the practical value.

For a `Delay` the second clause never holds, since the delay may not be forced.

Worked examples:

| Situation | Admissible above the `Case`? |
|---|---|
| candidate in 2 of 3 branches | no; bind at the shallowest occurrence instead |
| candidate in 3 of 3 branches | yes |
| forced builtins | already hoisted by `ForcedBuiltinsExtractor` in phase 1 |

Adopting this deletes `shapePartialBuiltins`, `partialBuiltinVarPrefixes`,
`referencesPartialBuiltin`, `crossesConditional`, `unsafeCaseCrossing` and both
`startsWith("__")` heuristics, in CSE and, through the shared import, in CCE. Roughly 120
lines, replaced by a branch-coverage check on the occurrence set.

### 2.1.1 Why speculation is deliberately excluded

LLVM's rule (`GVN.cpp`) is "speculatable **or** always executed", where speculatable means
the computation cannot fail. That third option is intentionally not adopted here.

Speculation is the only admissibility rule that changes the *number* of evaluations: an
expression present in some but not all branches goes from 0-or-1 evaluations to always 1.
Pricing that needs the cost of the expression weighted by how often the skipping paths
are taken, and the optimizer has no execution profile. Correctness would be satisfied;
profitability would be a guess.

It also costs little to leave out. The one candidate class where speculation clearly pays
is forced builtins, and those are already hoisted by `ForcedBuiltinsExtractor` in phase 1,
before CSE runs, at a bounded known cost because the hoisted term is a value. CSE's
remaining candidates are predominantly accessor chains built from partial builtins, which
are not speculatable in any case.

Note that `isSkippable` deliberately carves `Force(Builtin)` and `Force(Force(Builtin))`
out of the generic value-form skip, because sharing them saves 3 and 7 bits plus 1 and 2
CEK steps per use respectively. That carve-out is why deleting `ForcedBuiltinsExtractor`
entirely measured at only ~0.5%: CSE picks up what the extractor's occurrence count did
not reach.

Consequence: CSE performs no purity analysis at all, matching Plutus.

### 2.1.2 `isSkippable` does two unrelated jobs

`isSkippable` is the first filter applied when collecting candidates. It currently mixes
a **profitability** rule with a **safety** rule, and the split is worth making explicit.

*Profitability.* Sharing costs 8 bits of `let` framing plus a 12-bit `Var` per use, so
`saving_bits = (n-1)·bits(e) − 8 − 12n`, which at two occurrences needs `bits(e) > 32`.
The skipped shapes (`Var` 12 bits, unapplied `Builtin` 11, small `Const` 18, `Error` 4)
are all correctly below that. But the rule is a syntactic whitelist rather than the
computation, so it also skips shapes that clear the bar comfortably: large constants
(measured headroom: Auction 75 B, Crowdfunding 92 B) and unsaturated builtin
applications, which are exactly what Aiken's `builtin_curry_reducer` shares and what
Plutus's `CommuteFnWithConst` normalises argument order to expose. Plutus makes this an
explicit dial (`cse-which-subterms`: `ExcludeWorkFree` or `AllSubterms`) rather than a
hardcoded list.

Replacing the list with the computation is tempting but only half-safe: the byte side is
exact and static, while the step side depends on *execution* count, not occurrence count.
A forced builtin inside a loop body has an effective count far above its syntactic one,
which is what `ForcedBuiltinsExtractor`'s "occurrences inside a lambda count double"
heuristic stands in for. A purely static model would understate exactly those cases.
Out of scope here; recorded so it is not mistaken for an oversight.

*Safety.* `containsError` is a placement concern, not a profitability one: UPLC
application is strict, so binding a term evaluates it when the `let` is entered, which
can fire an `Error` earlier than the original would.

Under the down-safety rule this is no longer needed for success/failure equivalence, since
the bind point is somewhere the term was going to be evaluated anyway. It is **not**
fully redundant, though: in the all-branches case the evaluation moves from inside the
selected branch to before the `Case`, so an erroring term can pre-empt a `trace` that the
branch would have emitted first. Script validity is unchanged; the logs are not. Settle
this with a conformance test that compares trace logs (as
`LetChainRegroupCekConformanceTest` already does) rather than by argument, and only then
decide whether `containsError` can be dropped.

### 2.1.3 A shared, profile-ready cost model

CCE already prices extractions in lovelace, derived from `CardanoInfo` and
`CekMachineCosts`. CSE has no model at all. Unify them, and put the seam where a future
profile-guided Autotune can inject measured data without touching either pass.

`extractionSavingBits` currently hardcodes one assumption:

```scala
(n - 1) * skeletonBits - framingBits - StepBits * (3 * n + 3)
```

The `3n + 3` is `Σ_sites 3·count(site) + 3·count(bind)` with every count set to 1. So the
hook is a parameter substitution, not a redesign.

```scala
/** Marginal per-transaction cost of a rewrite, in lovelace. Lifted out of CCE.
  * Derived from CardanoInfo and CekMachineCosts; the only fractional quantity here. */
object RewriteCost:
    def lovelace(deltaBits: Int, deltaSteps: Double): Double

/** Net lovelace saving of extracting a term. Positive means worth doing.
  *
  * `occurrenceCounts` and `bindCount` are ''expectations'': under a weighted profile a
  * site that runs in some scenarios and not others has a fractional count. Without a
  * profile every count is 1.0. `stepsPerEval` is a CEK step tally, so it is an `Int`.
  */
def savingLovelace(
    occurrenceCounts: Seq[Double],
    bindCount: Double,
    bits: Int,
    stepsPerEval: Int
): Double
```

Counts are **parameters, not a lookup**. There is deliberately no `ExecutionProfile`
trait and no key, because no usable key exists today:

| Candidate key | Why it does not work |
|---|---|
| `ScalusSourcePos` | many-to-many with nodes; optimizer-created nodes have positions filled from neighbours afterwards |
| occurrence `Path` | internal to the pass; a CEK profile cannot produce one |
| `Term`, structurally | conflates distinct occurrences of the same term |

Supplying real counts requires a **stable site id threaded from SIR through lowering into
the UPLC annotation**, which is the enabling work recorded in
`PGO_SGD_OPTIMIZATION_RESEARCH.md` and a separate piece of work. Passing counts as plain
parameters defers that decision instead of baking a known-bad key into the API. The
passes call this with `Seq.fill(n)(1.0)` and `1.0`; a future caller with a profile
supplies measured values by whatever means it has.

The saving then depends on placement, which the down-safety rule already distinguishes:

```
ancestor-or-self:  Σ_inner count(i)·(stepsPerEval(e) − 1)  −  3·count(bind)
all-branches:      −3·count(bind)        // always a step cost, a byte win only
```

Two things a static model cannot do, and a profile can. **Counts**: whether the inner
sites actually run. **Per-evaluation cost**: without a profile `stepsPerEval` has to be
approximated by node count, which understates precisely the cases sharing is most
valuable for, since a saturated `sha2_256` is few nodes carrying a large,
argument-size-dependent builtin cost. `ProfilingData` carries `(mem, cpu, count)` per
entry, making this measured rather than estimated.

Wiring an actual profile is out of scope. Only the parameterisation is in scope, and it
is worth doing now because the alternative is re-deriving the saving formula later.

*Immediate benefit, before any profile exists.* With all counts at 1.0 this replaces
`isSkippable`'s profitability whitelist with the computation, so large constants and
unsaturated builtin applications are judged on their bits instead of their shape, and the
profitability half separates cleanly from the `containsError` safety half (§2.1.2).

### 2.2 Grouping

The mechanical change is in how occurrences are grouped, not only where the binding
lands. `addCount` (`CommonSubexpressionElimination.scala:73-78`) is a flat linear scan
over `(TermKey, Path)` pairs. It becomes a merge that folds counts along ancestor chains
and treats sibling clusters as a unit, so the surviving entry is always a legal bind
point under 2.1.

### 2.3 Collect-apply-repeat (Phase 3)

```
var t = term
while true:
    val cands = collect(t)
    cands.filter(admissible && profitable).maxByOption(_.bits) match
        case None    => return t
        case Some(c) => t = applyOne(t, c)
```

One collect per **successful** extraction, instead of one full re-collect per
**candidate**. Strictly fewer traversals than today. Deletes the entire `re*` vocabulary,
about 78 lines.

It also makes the staleness class fixed in `854491c2a` structurally impossible, because
scoring and applying now happen against the same term. That commit's re-check can be
removed as part of this.

---

### 2.4 Phase 4: naming, then dedup

Two halves with different risk profiles. Order matters: each earlier phase shrinks what
the later ones have to touch, so doing them in the wrong order means refactoring code
that is about to be deleted.

#### 2.4a Globally unique binders

Rename every binder to a globally unique name on entry, as Plutus does
(`Cse.hs:226`, `rename` before anything else) and as Aiken does with its scoped-binder
interner re-run after every phase (`interner.rs:9-25`).

This is what lets the scope machinery go: `isSafeToHoist`, `varsInScope`, `pathIdToVar`
and the free-variable and shadowing checks all exist because a hoisted binding might
capture or be captured. With unique binders that is impossible by construction, which is
precisely why Plutus's CSE has no such checks at all. Estimated 40 to 60 lines.

**It should be hash-neutral, and that is checkable.** Flat encoding de Bruijn-indexes
first (`Program.flatEncoded`), and `Flat[Term]` encodes `Var` by index while dropping
`LamAbs` names entirely, so names never reach the bytes. The corpus harness from 4.1
verifies this directly: if any hash moves, a pass is depending on a name, which is itself
the finding.

That dependency exists today, which is why this must come **after** Phase 1. The
`startsWith("__")` heuristics in `referencesPartialBuiltin` make optimizer behaviour
depend on spelling, so renaming before Phase 1 deletes them would change behaviour. After
Phase 1 names are cosmetic, and `__cse_` / `__cce_` survive only as debug labels.

#### 2.4b Deduplicate what remains

Six helpers are byte-identical across the two files today (`diff` returns empty):
`longestCommonPrefix` (13 lines), `collectNames` (14), `isAncestorOrSelf` (2),
`varsInScope` (2), `type Path`, and `insertLetAtPath` (41, differing by one comment). Two
more are identical modulo renames: `isSafeToHoist` and `freshCseName`/`freshCceName`.

**The set shrinks as the earlier phases land**, which is why this goes last:

| Helper | Fate |
|---|---|
| `varsInScope`, `isSafeToHoist` | deleted by 2.4a |
| `longestCommonPrefix` | survives; still computes the bind point for the all-branches case |
| `isAncestorOrSelf` | survives, and becomes load-bearing in the merge rule (2.2) |
| `insertLetAtPath`, `collectNames`, `type Path` | survive |
| `freshCseName` / `freshCceName` | collapse into one shared `freshName` |

So the dedup is roughly 60 to 70 lines rather than the 97 measured today, and it lands on
a stable target instead of a moving one.

---

## 3. Phase 0: the oracle (hard gate)

Nothing above is verifiable until this exists. No change to placement or grouping is
merged before it is in place and green.

| Item | Rationale |
|---|---|
| `Observation` enum (`Value` / `Failed` / `OutOfBudget`) | `Result.alphaEq` requires equal budgets, which is exactly what the optimizer changes, and equal exception *messages*, which embed terms so a rename breaks them |
| `RestrictingBudgetSpender` in property tests | The budget is our clock; `NoBudgetSpender` lets one divergent term hang CI |
| `isStrictIn`, ported from Plutus's `∈↓` | Decidable, needs no evaluation, works on open and over-budget terms. Descend into `Var`, both sides of `Apply`, `Force`, `Constr` args, the `Case` **scrutinee** and `Let` bodies; not into `LamAbs`/`Delay` bodies. Extend with the all-branches rule from 2.1, which Plutus's version lacks |
| `validateCse(before, after)`, ported from `UCSE` | Translation validation: for each introduced binding, check it is strict in the body and that back-substitution relates to the original. The strongest oracle available, and it makes the placement rule provable rather than merely tested |
| CEK-conformance tests for both passes | Model on `LetChainRegroupCekConformanceTest`: outcome, failure class and trace logs compared under a budget cap, with an anti-vacuity guard |
| Generator fixes | Frequency weights; `Constr`/`Case` arity from `Gen.choose(0, 4 min sz)` rather than a hardcoded 3; a **3-name pool** so shadowing is actually generated; a seeded builtin environment so terms compute something |
| `minSuccessful` set explicitly | One line |
| **Well-scopedness property**: `DeBruijn.deBruijnTerm(opt(t))` must not throw | A capture or an escaped variable becomes a loud failure instead of a wrong script. Aiken uses this round-trip as a hard gate twice in its pipeline (`shrinker.rs:2461-2463`, `:2551-2553`). Cheap, and it guards every later phase, so it belongs here rather than with the interner in 2.4a |

Properties, cheapest first. P1 to P3 need no evaluation.

| # | Property |
|---|---|
| P1 | `validateCse(t, CSE(t))` succeeds |
| P2 | `opt(opt(t)) ~=~ opt(t)` (exists, keep) |
| P3 | no new free variables (exists, keep) |
| P4 | `observe(opt(t), B) == observe(t, B)` on rich closed terms |
| P5 | `changed == (t ~!=~ opt(t))` |
| P6 | per-pass cost bound: CSE on budget, CCE on `termBits` |

---

## 4. Measurement

### 4.1 Required before merge

Whole-corpus blueprint diff: capture `hash` and `compiledCode` length for all 23
generated blueprints, apply the change, rebuild, diff. Report hashes changed, total bytes
moved, and budget deltas for every validator with pinned `ExUnits`.

Expect the placement change to move **many** hashes. A stricter rule extracts less in
some shapes and more in others, so scripts may grow or shrink per validator. This must be
quantified rather than assumed, and it belongs in the CHANGELOG.

### 4.2 Open questions to settle with numbers

1. **Does the all-branches clause earn its keep?** Sweep two variants on the full corpus:
   ancestor-or-self alone, and with all-branches. Prior expectation is that all-branches
   carries most of the value, since it covers sibling branches destructuring the same
   datum. If it buys nothing, drop it and match Plutus exactly.
2. **Should `cceEnabled` default to true?** Measured on 3 CAPE validators after the three
   commits on this branch: htlc -381, two_party_escrow -1725, linear_vesting -135
   lovelace per transaction. Extend to all 23 before deciding. Owner's call.

---

## 5. Deferred

- **Phase 2, one traversal.** A `mapChildrenF[F[_]: Applicative]` primitive plus open
  recursion and a Stratego-style combinator layer. 39 hand-rolled exhaustive `Term`
  matches exist in the transform layer today, 13 of them full rewrites. Plutus writes
  congruence once in 12 lines (`Core/Plated.hs:56-76`) and 20 passes use it. Estimated
  -200 to -300 lines. Independent of this work.
- **Not doing:** Kiama (measured 34-46x hand-written, unmaintained since 2023, JVM-only,
  and `Constructor.newInstance` cannot construct Scala 3 `enum` cases); recursion schemes
  and `Fix` (real allocation cost in Scala); Trees That Grow (GHC's own record: +20%
  compile time, 596 type instances, unfinished after 9 years); replacing the pair with an
  e-graph (Cranelift measured 23% compile-time overhead for ~0.1% gain, and binders blow
  e-graphs up); Aiken's `> 2` occurrence threshold (we already price in lovelace);
  SMT verification of the passes.

---

## 6. Risks

| Risk | Mitigation |
|---|---|
| A stricter rule loses sharing and scripts grow | The all-branches clause recovers the common case, which is where a uniform ancestor-or-self rule would lose. Measure the full corpus before merging |
| Many script hashes change at once | Expected; quantify and document. Deliberately done in one step rather than split across releases |
| Placement lands without a working oracle | Phase 0 is a hard gate |
| Forced-builtin sharing regresses | `ForcedBuiltinsExtractor` runs first and takes the global cases; `isSkippable`'s carve-out keeps the rest as candidates. Verify on the corpus |
| Phase 3 changes extraction order and therefore output | Verify with the corpus diff, not by reasoning |
| Renaming binders (2.4a) changes output | Should be impossible: flat de Bruijn-indexes first and drops binder names, so names never reach the bytes. The corpus harness checks it directly, and any moved hash means a pass is reading a name |
| Deleting the scope checks (2.4a) allows a capture | The well-scopedness property in Phase 0 turns a capture into a loud test failure, and unique binders make it impossible by construction |

---

## 7. Order of work

1. Phase 0 oracle, in full, including the well-scopedness property.
2. Corpus measurement harness (4.1), so every later step is measured identically.
3. Phase 1 placement, measured.
4. Shared cost model and the count parameterisation (2.1.3), with all counts at 1.0.
   Measured: the only intended behaviour change is that `isSkippable`'s profitability
   whitelist becomes a computation.
5. Phase 3 collect-apply-repeat, measured; remove the now-redundant re-check from
   `854491c2a`.
6. Phase 4a globally unique binders, and delete the scope machinery. Expected
   hash-neutral; if the corpus harness disagrees, a pass is depending on a name.
7. Phase 4b dedup what survives, into one shared object.
8. Settle 4.2 and decide the `cceEnabled` default.

Each step is ordered by what it makes possible or unnecessary for the next:

- Step 4 after step 3, because the saving formula depends on which placement was chosen.
- Step 6 after step 3, because the `startsWith("__")` heuristics make behaviour depend on
  spelling until Phase 1 deletes them.
- Step 7 last, because steps 3 and 6 delete roughly a third of what would otherwise be
  deduplicated.

## Sources

Code: `CommonSubexpressionElimination.scala:73-78,149,240`,
`CommonContextExtraction.scala:159,259`, `TermAnalysis.scala:287-343`,
`DefaultFun.scala:1429-1497`, `OptimizerPipelines.scala:27-80`.
Plutus `1.63.0.0`: `UntypedPlutusCore/Transform/Cse.hs:215-467`,
`UntypedPlutusCore/Purity.hs:121-281`, `Core/Plated.hs:56-76`,
`plutus-metatheory/src/VerifiedCompilation/UCSE.lagda.md`,
`plutus-metatheory/src/Untyped/Strictness.lagda.md`.
Aiken `v1.1.23`: `crates/uplc/src/optimize/shrinker.rs:1900-1935,2557-2771`.
LLVM: `lib/Transforms/Scalar/GVN.cpp` (`performScalarPRE`), LangRef `speculatable`.
Related: `docs/internal/UPLC_SIZE_COMPRESSION_RESEARCH.md`,
`docs/internal/PGO_SGD_OPTIMIZATION_RESEARCH.md`.
