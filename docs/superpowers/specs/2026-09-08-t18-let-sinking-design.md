# T18: let-sinking

Design for `CODEGEN_IMPROVEMENT_PLAN.md` task **T18** ("Let-sinking of multi-use
top-level bindings into using branches"), deferred by the T5 design
(`2026-08-31-t5-let-chain-regrouping-design.md`, section 7) until T5 was
measured.

Status: designed 2026-09-08, not yet implemented.

## 1. Decision

Build **T18 as a new `LetSinking` pass, flag-gated**, and flip its default
together with `letChainRegroup` in one baseline re-pin.

Two things make it worth building despite a small measured win (section 3):

- It is **monotone**. Sinking never adds a machine step and never adds a node,
  so unlike T5 it needs no threshold, no arity heuristic, and no size/step
  trade-off. That is what makes it safe to turn on by default.
- It is **smaller than the T5 design assumed**. That document routed the
  `TermPaths` extraction (folding the placement machinery out of
  `CommonSubexpressionElimination` and `CommonContextExtraction`) through T18.
  It turns out T18 does not need it — see section 4.

The reason to batch the default flip with T5's: flipping either one re-pins
every `ExUnits` baseline by hand, because `update-budgets` cannot rewrite `Coin`
fees, `assertResult`, or size pins. That manual pass costs the same whether one
pass changed or two.

## 2. What T18 does

The linker and the earlier passes leave bindings above the code that uses them.
When every use of a binding sits inside one branch of a `Case`, the binding's
right-hand side is still built on every path, including the paths that never
reach that branch. T18 moves the binder down to the branch.

```
-- before: rhs is built whether or not branch 1 is taken
[(lam x (case s [b0, <uses of x>, b2])) rhs]

-- after: rhs is built only when branch 1 is taken
(case s [b0, [(lam x <uses of x>) rhs], b2])
```

The transformation is the **`Many` case of something the `Inliner` already does
for `OnceGuarded`**. `Inliner.analyzeOccurrence` classifies a variable used
exactly once under a `Delay`, a `Case` branch, or a lambda body as `OnceGuarded`
and substitutes the right-hand side into that position, which sinks it. For two
or more uses it classifies `Many` and declines, because substituting would
duplicate the right-hand side. T18 gets the same effect for `Many` by relocating
the binder instead of duplicating the value.

That is also why the residual candidates are all multi-use: the single-use ones
have already been handled.

### Cost model

A binding costs **2 machine steps** for its own `Apply` and `LamAbs`, plus
whatever the right-hand side costs to evaluate — **3 in total when the
right-hand side is a lambda**, which is the common case. Sinking moves all of
that behind a branch test:

- On an execution that enters the branch: identical cost, just later.
- On an execution that does not: the whole 3 steps are saved.

At 100 mem / 16,000 cpu per step and mainnet prices, 3 steps is 300 mem /
48,000 cpu, about 21 lovelace.

This matches the plan's own measurement under T18: removing the last user of
`find` from `linear_vesting` cut the `FullUnlock` path — which never called it —
by 300 mem / 48,000 steps, exactly 3 steps.

### Why it cannot lose

- **Steps:** the sunk work is a subset of the original work on every path, so
  the step count is `<=` pointwise. There is no execution on which sinking costs
  more. This is the property that removes the need for a threshold.
- **Nodes:** the node multiset is unchanged. The binder moves; nothing is
  copied.

Script *bytes* are near-neutral rather than provably neutral, because flat
encodes de Bruijn indices as variable-length naturals. Sinking a binder deeper
shortens the references to it and removes one level from everything between the
old and new position, so the expectation is a small win, but it is not a
theorem. Section 6 measures it.

## 3. Measured evidence

Method: compile each validator under its production `Options`, undo
`CaseConstrApply` to recover the let structure, then for every binding with two
or more uses walk down from its body through positions entered at most once,
following the unique child that holds all the uses (section 4 gives the
descent). A binding is **sinkable** when that walk crosses at least one `Case`
branch boundary. The probe is ~140 lines and is reconstructible from section 4;
it is not kept in the repo.

Measured 2026-09-08 on master `f1081633d`:

| validator | nodes | multi-use lets | sinkable | pure rhs | cross-branch | depth |
|---|---:|---:|---:|---:|---:|---:|
| cape/linear_vesting | 678 | 29 | 0 | 0 | 2 | – |
| cape/htlc | 572 | 26 | 2 | 2 | 4 | 1 |
| cape/two_party_escrow | 817 | 28 | 4 | 3 | 3 | 2 |
| escrow | 1052 | 35 | 6 | 6 | 5 | 1 |
| betting | 2059 | 79 | 5 | 5 | 17 | 2 |
| auction | 2329 | 95 | 4 | 4 | 13 | 2 |
| editable_nft | 1036 | 38 | 2 | 2 | 8 | 1 |
| linked_list | 4056 | 136 | 5 | 5 | 9 | 1 |
| upgradeable_proxy | 664 | 29 | 2 | 2 | 5 | 2 |
| payment_splitter | 1353 | 57 | 5 | 4 | 9 | 1 |
| **total** | | **552** | **35** | **33** | **75** | |

Reading it:

- **33 bindings across ten validators** are sinkable with a pure right-hand
  side: 2 to 6 per validator, none on `linear_vesting`. At 3 steps each that is
  6 to 18 steps per validator, or 40 to 125 lovelace, on the executions that
  skip the branch. Expect about half of that in practice, since a two-way
  dispatch skips the branch on roughly half its paths.
- **The purity guard is nearly free.** It blocks 2 of 35. Both are computed
  right-hand sides rather than lambdas: `[(builtin unIData) xIn]` in
  `two_party_escrow` and a `Value.getLovelace` call in `payment_splitter`. Both
  can fail, so blocking them is correct — sinking would turn a script that
  always fails into one that fails only on some paths.
- **`linear_vesting` now has zero candidates**, although it is the validator the
  plan's T18 evidence was measured on. The prelude workaround landed in the
  meantime: `findOwnInputOrFail` and friends became `inline` helpers with local
  recursion, which keeps the loop's letrec inside the calling branch. T18's
  remaining value is therefore for user code and for helpers that have not been
  rewritten that way — it removes the reason to keep writing the workaround, on
  top of the steps it saves.
- **Sink depth is 1 or 2.** No binding in the corpus can sink past more than two
  branch boundaries.
- **75 bindings spread over two or more branches** of the first `Case` they
  reach. Those need duplication, which costs bytes; they are out of scope
  (section 7).

## 4. Algorithm

T18 is a single top-down rewrite. At every let it tries to sink, recursing into
the result.

```scala
private def go(term: Term): Term = term match
    case Apply(LamAbs(x, body, lamAnn), rhs, applyAnn) if rhs.isPure =>
        sink(x, go(body), rhs, lamAnn, applyAnn, spineLam = true)
            .getOrElse(Apply(LamAbs(x, go(body), lamAnn), go(rhs), applyAnn))
    ...
```

`sink` descends while the uses of `x` stay together, and re-wraps the binder at
the point where they stop being together:

```scala
// spineLam: whether a bare LamAbs here has its body entered at most once —
// true in the function position of an Apply (every beta-redex, so every let)
// and in a Case branch, false in an argument position and under Delay.
private def descend(name: String, t: Term, spineLam: Boolean): Option[Term => Term]
```

The once-entered rules, which are what keep the pass monotone:

| position | entered at most once | sink through |
|---|---|---|
| `Case` branch (and its leading lambdas) | yes | yes — this is where the win is |
| `Case` scrutinee | yes | yes, no win |
| body of an applied lambda (a let) | yes | yes, no win |
| either operand of an `Apply` | yes | yes, no win |
| `Force` operand | yes | yes, no win |
| `Constr` field | yes | yes, no win |
| body of a lambda in value position | **no** — once per call | **never** |
| `Delay` body | **no** — once per force | **never** |

The last two rows are the whole safety argument for steps. A lambda in value
position is a closure, a callback, or a fixpoint; sinking a binding into it
rebuilds the right-hand side on every call. A `Delay` body re-runs on every
`force`, since UPLC does not memoise.

**No global path table is needed.** The T5 design assumed T18 would need the
`TermPaths` extraction, on the model of `CommonSubexpressionElimination`, which
builds a `(TermKey, Path)` occurrence table and merges to a longest common
prefix. CSE needs that because its occurrences are scattered across the tree.
T18's binder moves along a single root-to-node path and stops at the first node
where the uses branch — the descent computes that lowest common ancestor
incrementally, in one pass, with the shadowing check accumulated on the way
down. CSE's machinery can still be factored out later for CCE's benefit; it is
not a prerequisite here.

### Safety conditions

1. **`rhs.isPure`.** Sinking delays evaluation. A right-hand side that can error
   or trace would fire on fewer paths after the move. `TermAnalysis.isPure`
   already encodes this; it blocks 2 of 35 candidates.

   Note it also blocks self-application fixpoints, because the analysis is
   context-free: the fixpoint body `[f f]` is a `Var` applied to a `Var`, which
   `isPure` cannot classify. That does not cost anything on this corpus (all 33
   surviving candidates are ordinary bindings), so admitting fixpoints is
   deliberately left out — see section 7.

2. **No shadowing between the origin and the sink point.** The free variables of
   `rhs` are necessarily in scope deeper down, since sinking only descends, so
   the only hazard is a binder on the way that rebinds one of them. Accumulate
   the binders crossed during the descent and stop before any that intersects
   `rhs.freeVars`.

3. **`x` itself not shadowed.** Handled by the use-counting, which already
   respects shadowing.

## 5. Pipeline placement

New phase between CCE and `LetChainRegroup`, followed by an `Inliner` run —
sinking can turn a multi-use binding into a single-use one inside the branch,
which the inliner then removes:

```scala
val sunk =
    if letSinking then withCce |> letSink.apply |> inliner.apply
    else withCce
```

T18 before T5 so that T5 forms its runs on the final positions.

New option, declared last in `Options` so it does not shift the positional
accessors above it, with the same three MiMa filters T5 needed
(`Options.this`, `.copy`, `.apply`):

```scala
letSinking: Boolean = false
```

## 6. Testing

- `LetSinkingTest` (shared) — shape tests: sinks into a branch; does not sink
  through a value-position lambda; does not sink through a `Delay` body; does
  not sink an impure right-hand side; does not sink when uses span two branches;
  stops before a shadowing binder; pass is wired in both flag states.
- `LetSinkingCekConformanceTest` (jvm) — generated terms evaluated before and
  after, asserting **same result and `budget_after <= budget_before`**. The
  inequality is the monotonicity claim from section 2, and it is a stronger
  property than T5's conformance test could assert (T5 trades steps for bytes,
  so it only checks equality of results). Include the same vacuity guards T5
  uses: assert that the generator actually produces terms the pass rewrites, and
  terms that get cheaper.
- Corpus: compile the ten validators with the flag on and off; compare
  `program.cborByteString.size` and the pass's log lines. Real `ExUnits` come
  from flipping the default and reading the pinned-budget failures in
  `scalusExamplesJVM/testOnly scalus.examples.cape.*`. Section 2's byte claim is
  confirmed or corrected here.

## 7. Out of scope

- **Duplicating a binding into several branches.** The 75 cross-branch bindings
  need the right-hand side copied per using branch, which costs script bytes and
  therefore reintroduces exactly the size-versus-steps threshold T5 had to
  measure. If it is ever built it should reuse
  `docs/internal/CASE_CONSTR_COST_MODEL.md`, not invent a second cost model.
- **Sinking fixpoints.** Would need a narrow `isFixpointValue` predicate for the
  self-application shape (`[(lam s [s s]) (lam s (lam x ...))]` reduces to a
  lambda in two steps and cannot fail, so it is a value construction even though
  `isPure` says otherwise). Worth zero on this corpus. Revisit if a corpus shows
  fixpoint candidates.
- **Impure sinking**, which needs the plan's T6 `relaxedEvaluationOrder`.
- **`TermPaths` extraction.** Still worth doing for CSE and CCE, but section 4
  removes it from T18's critical path.

## 8. Risks

- **Interaction with T5.** Sinking a binding out of a top-level chain shortens
  that chain's run. A run dropping from 5 to 4 falls below
  `LetChainRegroup.MinRunSize` and stops being grouped, losing 3 steps on
  *every* execution to save 3 steps on the skipped ones — a net loss. The sunk
  binding may or may not join a run at its new position. This is the one way the
  combination can regress, and it is why both flags must be measured together in
  section 6's corpus run, not just individually.
- **The win is path-dependent.** The 33 candidates are an upper bound: a binding
  whose branch is taken on every real execution saves nothing. This is the same
  caveat that the case-constr census ran into, and the same answer applies —
  the honest number comes from profiling, not from counting nodes.
- **Script bytes.** Claimed near-neutral, not proven. If the corpus disagrees,
  the pass stays off and this document records why.
