# BooleanOptimizer: what it does, and what it is worth

Findings from fixing and enabling `scalus.compiler.sir.transform.BooleanOptimizer`, 2026-09-01.
Re-measure with `scalusExamplesJVM/testOnly scalus.examples.BooleanOptimizerImpactTest`.

## What it is

A SIR-to-SIR boolean simplification pass, run from `UplcPipeline` whenever `options.optimizeUplc`
is set - the same switch that enables `StaticArgumentTransformation`. Until 2026-09-01 it was dead
code: nothing but its own test referenced it.

Which rules it may and may not apply is documented in the pass's own scaladoc. The short version:
`And`/`Or`/`Not` all lower to `IfThenElse`, so the first operand is always evaluated and the second
only conditionally. A rule that drops an evaluated operand can delete an `Error` or a `trace`, and
is therefore illegal even when its truth table agrees. That asymmetry is why `And(false, b)` folds
but `And(a, false)` must not.

## Only one rule pays

Across the 22 example contracts, exactly one rule fires:

| rule | hits |
|---|---|
| `if !c then t else f` => `if c then f else t` | 34 |
| everything else (De Morgan, constant folding, ...) | 0 |

Two reasons the rest are dead, both worth knowing before adding a rule:

- **Constant folding never fires.** `scalac` folds boolean constants before the plugin sees the
  tree.
- **Every `a == b` rule is unreachable.** `AnnotationsDecl` is a case class carrying `pos`, so
  structural equality on two SIR subtrees includes their source positions. Two textual occurrences
  of the same expression never compare equal. That kills idempotence, complementation, and
  identical-`if`-branch detection on plugin-produced SIR.

All 34 sites come from `!`, `!==`, or `require(!x, ...)`, and they concentrate in two places:

- `prelude/Prelude.scala:110` - `inline def require` expands to `if requirement then () else throw`,
  so a negated argument creates `If(Not(...))`. Sites are attributed to the inline body, not to the
  contract line that called it.
- `v1/Value.scala:75`, `:1133`, `:1198` - `Value.apply` and the `Value` union inner loop, both
  `if x !== BigInt(0)`.

## What it is worth

**Size**, 22 contracts, `Options.releaseUntagged`:

| | raw (UPLC optimizer off) | optimized |
|---|---|---|
| total | 58 218 -> 58 087 B (-131 B) | 31 938 -> 31 864 B (-74 B, -0.23%) |
| contracts changed | 13 of 22 | 11 of 22 |
| best single contract | Escrow -25 B | Escrow -17 B (-1.59%) |

**Budget.** Every executed `if !c` site costs exactly **200 memory units and 32 000 CPU steps**,
one PV11 `Case` node, so a test improves by that times the number of swap sites on its path. 93
pinned assertions moved and **none moved upward**: 79 on the Scala 3.3.x LTS, plus 14 `since38`
values measured on 3.8.4. Per-test improvement is -0.03% to -0.29% of CPU steps on contracts, and
-0.63% in aggregate on the standard-library microbenchmarks, where one saved `Case` is a larger
share of a short run.

`scalus-core` needed no `since38` change, so the pass removes the same nodes on both compiler
generations.

**Cost:** one linear SIR pass, and **a changed script hash for every `optimizeUplc = true` compile**
(`Options.release` included). `Options.default` is unaffected.

## Follow-up worth considering

Move the one rule that pays down into the UPLC optimizer. `Not` does not exist in UPLC - at PV11 it
is `Case(c, [False, True])` - so the same rewrite is a `Case(Case(c, [K1, K2]), [a, b])` peephole in
`V3Optimizer`. Placed after the `Inliner`, it would also catch `if !c` shapes that inlining creates,
which a SIR-level pass cannot see because linked standard-library functions are not inlined at SIR
level. That extra upside is a hypothesis; the SIR-level numbers above are measured.

Do not delete the pass without a replacement: `require(!x, ...)` and `if x !== y` are idiomatic
Scalus, so the pattern keeps appearing.
