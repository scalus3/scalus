# Mutual recursion support (T2 follow-up) - design

Date: 2026-08-04
Status: approved
Scope: top-level mutually recursive defs in `@Compile` objects. Local defs in
`compile {}` blocks get a clear error only (plugin two-pass compilation is a
follow-up).

## 1. Verified current state (probed on master)

- **No Z combinator remains anywhere in lowering.** `LetRecLoweredValue` (V3)
  and `BaseSimpleLowering` (Scott/SoP) use self-application unconditionally.
  `Expr.ZTerm`/`Z`/`z`/`rec` (`Expr.scala:44-55`) are a public hand-written
  UPLC DSL API, not compiler output. The `zCombinatorNeeded` vars are
  deprecated no-ops kept for MiMa.
- **"Polymorphic recursion corner cases" do not exist.** That concern applies
  to typed fixpoints; UPLC is untyped and Scalus erases types at lowering, so
  self-application covers every recursion shape Z did. The plan text in
  `CODEGEN_IMPROVEMENT_PLAN.md` T2 was speculative and will be corrected.
- **Mutual recursion is broken today, with confusing errors:**
  - Local mutual defs in `compile {}` fail at Scala compile time: the forward
    reference becomes a phantom `ExternalVar` in a fake module
    ("Bad symbolic reference ... Module `..._$_$sir` not found").
  - Top-level mutual defs link into broken *nested* single-binding lets
    (`let isOdd = ...isEven... in let isEven = ...`); lowering fails with
    "External variable ... not found in the scope".
  - The `sys.error("Mutually recursive bindings are not supported")` branches
    (`Lowering.scala` rec case, `BaseSimpleLowering.scala`) are dead code: no
    producer emits a multi-binding rec `SIR.Let`.

## 2. Chosen approach

SIR-level elimination (peers-as-params), zero backend encoding changes:

1. Linker groups top-level SCCs into multi-binding rec `SIR.Let`s.
2. A new SIR pass `MutualRecursionElimination` rewrites each group into
   nested single-binding lets that the existing T2 self-application
   machinery lowers as-is, in both backends.
3. The plugin reports a clear error for local forward references.

Rejected alternatives:

- Backend dispatcher (Aiken cyclic encoding: self-applied body +
  Scott-encoded chooser, `gen_uplc.rs` `FunctionVariants::Cyclic`): ~4-5
  extra applies per cross-call vs ~1-2 here, two backend implementations,
  and Aiken uses the v1 fixpoint shape that broke our PartialEvaluator
  constant folding in T2.
- Rewrite inside the linker without materializing groups: least code, but
  untestable in isolation and hand-built SIR still hits `sys.error`.

## 3. Linker: SCC grouping (`SIRLinker.link`)

- After `traverseAndLink`, build a dependency graph over `globalDefs`: for
  each linked binding, the free `ExternalVar`/`Var` names that are keys of
  `globalDefs`.
- Run Tarjan SCC.
- Size-1 SCCs: emit exactly as today (`foldRight`, completion order). No
  output churn for existing code, so no budget-pin churn.
- Non-trivial SCCs: merge all members into the earliest member's slot,
  bindings in completion order, and emit one
  `SIR.Let(bindings, acc, LetFlags.Recursivity, ...)`. `SIR.Let` and the
  flat serialization already support `List[Binding]`.
- `RemoveRecursivity` (called at the end of linking) only matches
  single-binding lets and passes groups through unchanged.

## 4. `MutualRecursionElimination` (new SIR pass)

Location: `scalus-core/shared/.../compiler/sir/MutualRecursionElimination.scala`,
next to `RemoveRecursivity`. Invoked at the entry of both
`SirToUplcV3Lowering` and `BaseSimpleLowering`, so linker output, hand-built
and deserialized SIR all work. The `sys.error` branches stay as guards.

Rewrites every rec `Let` with 2+ bindings, anywhere in the tree.

### Transform

For a group `f1..fN` in binding order, emit nested lets: outermost `fNp`,
then `f(N-1)p`, ..., `f2p`, innermost `f1`, then the body.

- `fip = λf1...f(i-1). rhs_i'` for `i >= 2`. Lambda params reuse the
  **original names** (full top-level names; unique, cannot be shadowed).
  The let binding gets a **fresh name** `fi$mutrec`.
- `f1 = rhs_1'` under its original name.
- Reference rewrite `E(j)` inside `rhs_i'` (and in `f1`'s rhs and the body
  with `i = 1`), applied to both `Var` and `ExternalVar` occurrences:
  - `j < i`: the param var `fj`
  - `j = i` (self): `fip f1 ... f(i-1)` - plain self-recursion, lowered by
    the existing T2 self-application encoding
  - `j = i + 1` (adjacent forward): `fjp E(1) ... E(j-1)`, a call-site
    chain of plain vars - unchanged from the naive scheme
  - `j >= i + 2` (far forward): **not** expanded recursively at every call
    site. Each member binds `fk` (for each such target `k`) once, via a
    non-recursive, eta-expanded let: `let fk = lambda $eta. (fkp E(1) ...
    E(k-1)) $eta`. The eta-lambda defers the fixpoint application - a
    strict direct binding (`let fk = fkp E(1) ... E(k-1)`) would apply the
    fixpoint at member entry and diverge. Call sites inside `rhs_i'` then
    just reference the plain var `fk`.
- Types: `fip.tp = Fun(tp_1, ... Fun(tp_(i-1), tp_i))`; intermediate
  `Apply` node types come from peeling `Fun`.
- Rec flag per emitted let: `LetFlags.Recursivity` iff
  `RemoveRecursivity.isRecursive(fip-name, rhs_i')` - e.g. a member with
  only cross-calls (like `oddp`) comes out non-rec.
- Guard: every member's rhs must be a `LamAbs` (parameterless defs already
  arrive as `Unit -> T` lambdas from the plugin). A cyclic group of values
  is rejected with a clear exception.

### Example

```
letrec isEven = λn. if n==0 then true  else isOdd(n-1)
   and isOdd  = λn. if n==0 then false else isEven(n-1)
in isEven 4
=>
let    isOdd$mutrec = λisEven. λn. if n==0 then false else isEven(n-1)  // non-rec
letrec isEven       = λn. if n==0 then true  else isOdd$mutrec isEven (n-1)
in isEven 4
```

One fixpoint per group. Total growth is O(N^2) per member: measured on a
10-member distance-9 mutual-recursion group, this encoding produces 499 AST
nodes versus 1377 for naive recursive expansion of far-forward references
(the old exponential-in-distance shape that the eta-expanded once-only
binding above replaces).

## 5. PrettyPrinter: multi-binding lets

`PrettyPrinter.pretty(sir, style)` currently dies with
`sys.error("Multiple bindings not supported")` (`PrettyPrinter.scala:208-209`)
on any multi-binding `Let`. Non-rec multi-binding lets are already legal in
lowering, and the linker will now emit rec groups, so implement both:

- Non-rec, 2+ bindings: stack the bindings under one `let`, then `in`:
  ```
  let a: T1 = e1
      b: T2 = e2
  in ...
  ```
- Rec, 2+ bindings: render the group with the existing `fun` style, joined
  by `and`:
  ```
  fun isEven(n): (Int -> Boolean) =
      ...
  and fun isOdd(n): (Int -> Boolean) =
      ...
  in ...
  ```

The `sys.error` case is removed entirely.

## 6. Plugin: clear error for local mutual recursion

In `SIRCompiler`, where a forward reference to a local def currently becomes
a phantom `ExternalVar` (symbol owner is not a module class and the name is
not in `env`): report a real compiler error - "forward reference to local
def `X`: local mutual recursion is not supported in compile blocks; move the
functions to a `@Compile` object".

## 7. Tests (TDD)

- `MutualRecursionEliminationTest` (unit, hand-built SIR): groups of size 2
  and 3; assert rewritten structure and CEK evaluation result.
- PrettyPrinter: `sir.show` on a rec group and on a non-rec multi-binding
  let renders without error; asserted in the end-to-end test.
- `MutualRecursionTest` (end-to-end, grows out of the probe): top-level
  `isEven`/`isOdd` through V3 and both simple backends, evaluated result plus
  a budget ceiling; a 3-function cycle; a member that is both self- and
  cross-recursive; a group where the body calls only one member.
- Plugin negative test: local mutual defs produce the new error message.
- Full `jvm/test` and `sbtn mima` green; acyclic linker output is unchanged
  so no budget re-pinning is expected.

## 8. Docs and compatibility

- Correct `CODEGEN_IMPROVEMENT_PLAN.md` T2 text (no Z kept, no polymorphic
  recursion corner case; mark mutual recursion done, drop the dead-code
  `Lowering.scala:588` reference).
- Out of scope: `pendingTopLevelLetRecs` mutually-recursive-sums limitation
  (`SirToUplcV3Lowering.scala:56`) - LoweredValue-level, noted as future
  work; plugin two-pass block compilation for local mutual defs.
- MiMa: additions only, zero filters.
