# One traversal for UPLC `Term`

Date: 2026-10-27. Status: implemented, steps 1 to 6 done (2026-10-30). Amended 2026-10-30: the
whole-tree combinators recurse on the JVM stack, see `[TT-14]`.

Origin: "Phase 2, one traversal" in the Deferred section of
[CSE/CCE: binding placement and refactoring](2026-09-10-cse-cce-placement-and-refactoring-design.md).
This spec replaces that entry. Scope: UPLC `Term` only. SIR is out of scope.

---

## 1. Motivation

The UPLC layer has 46 exhaustive `Term` matches in 17 files. About 30 of them walk the
tree the same way and differ only in what they do at one node. Examples of the same walk
written again: `PartialEvaluator.containsTrace`, `CommonContextExtraction.containsError`,
`CommonContextExtraction.containsHole`, `ScalusTag.containsMarkedError`,
`Term.collectBuiltins`, `TermAnalysis.freeVars`.

Three problems follow:

- A new `Term` case means edits in about 30 places. Each place is a chance to forget a
  child, for example the scrutinee of a `Case`.
- Every walk recurses on the JVM stack. Our builds hide this with `-Xss64M`
  (`.jvmopts`, `build.sbt:601`, `build.sbt:919`). A user runs with the JVM default of
  about 1 MB, and Scala.js runs on the browser stack. A deep let-chain that passes our
  tests can overflow for them. This spec does not fix it yet, see `[TT-14]` and Section 7.
- The child walk exists in one good form already, but it is private. The private
  `children` and `mapChildren` in `CommonSubexpressionElimination.scala:296-330` keep
  identity (`eq`) for unchanged subtrees, which the CSE caches need. Other passes copy
  every node.

Plutus writes the child walk once in 12 lines (`Core/Plated.hs:56-76`), and 20 passes
use it. This spec does the same for Scalus, with one extra goal from the owner: **low
allocation**. No stack overflow on deep terms was a second goal; it is deferred, see `[TT-14]`.

## 2. Definitions

| Term | Meaning |
|---|---|
| **node** | One `Term` value. |
| **child** | A direct sub-`Term` of a node. `Var`, `Const`, `Builtin` and `Error` have no children. |
| **child order** | The order of children defined in `[TT-1]`. |
| **whole-tree combinator** | A method of `TermTraversal` that walks every node below its argument: `visit`, `foreach`, `exists`, `foldLeft`, `transformUp`, `transformDown`, `freeVars`. |
| **one-level combinator** | A method of `TermTraversal` that touches only the children of its argument: `foreachChild`, `foreachChildIndexed`, `mapChildren`, `mapChildrenIndexed`. |
| **identity preservation** | A rewrite returns the same object (`eq`) when no child changed. |
| **pass** | A function in `scalus.uplc` or `scalus.uplc.transform` that walks a `Term`. |
| **golden output** | The text that `PipelineDeterminismChild.render()` prints on master before this work starts. |

## 3. API

**Purpose:** give every pass one shared, fast walk over `Term`.
**Who:** passes in `scalus.uplc` and `scalus.uplc.transform`.
**Trigger:** a pass needs to visit or rewrite the children of a `Term`.

**Structure.** File `scalus-core/shared/src/main/scala/scalus/uplc/internal/TermTraversal.scala`:

```scala
package scalus.uplc.internal

object TermTraversal {
    extension (t: Term) {
        // One level.
        inline def foreachChild(inline f: Term => Unit): Unit
        inline def mapChildren(inline f: Term => Term): Term
        inline def foreachChildIndexed(inline f: (Term, Int) => Unit): Unit
        inline def mapChildrenIndexed(inline f: (Term, Int) => Term): Term

        // Whole tree. Recursive on the JVM stack.
        inline def visit(inline enter: Term => Boolean)(inline exit: Term => Unit): Unit
        inline def foreach(inline f: Term => Unit): Unit
        inline def exists(inline p: Term => Boolean): Boolean
        inline def foldLeft[A](z: A)(inline f: (A, Term) => A): A
        def nodeCount: Int
        inline def transformUp(inline f: Term => Term): Term
        inline def transformDown(inline f: Term => Term): Term
    }

    def freeVars(t: Term): Set[String]
}
```

The combinators are extension methods, so a call reads `t.mapChildren(go)`. A file imports
the ones it uses by name, for example `import scalus.uplc.internal.TermTraversal.{exists,
mapChildren}`. They do not become members of the public `Term` type. `freeVars` stays a plain
method: `TermAnalysis` already provides a `freeVars` extension, and two would be ambiguous.

**Why `scalus.uplc.internal`:** MiMa ignores this package (`build.sbt:491`), and the
interop style rules do not apply to it. We do not know the final combinator set yet. This
package lets us change it without a MiMa filter.

### 3.1 Checks enforced here

**Child order**

- Every combinator MUST give children in this order: `LamAbs` body; `Apply` function,
  then argument; `Force` body; `Delay` body; `Constr` arguments from first to last;
  `Case` scrutinee, then branches from first to last. `[TT-1]`
- `mapChildrenIndexed` and `foreachChildIndexed` MUST pass index 0 to the first child and
  count up by one in child order. `[TT-2]`

**Why:** `[TT-1]` is the order every pass uses today. CSE candidate order and fresh names
depend on it, so a different order changes script bytes. `[TT-2]` keeps the existing CSE
path numbering: `Apply` 0 = function, 1 = argument; `Case` 0 = scrutinee, 1..n = branches.

*Example, illustrative.* For `Apply(f, Case(s, [b0, b1]))`, `foreach` visits
`Apply`, `f`, `Case`, `s`, `b0`, `b1`. The CSE path of `b1` is `[1, 2]`.

**Traversal shape**

- `visit` MUST call `enter` on a node before it calls `enter` on any child of that node. `[TT-3]`
- `visit` MUST NOT visit the children of a node when `enter` returns `false` for it. `[TT-4]`
- `visit` MUST call `exit` on a node exactly once, after it visits all children of that
  node, and only if `enter` returned `true` for it. `[TT-5]`
- `foreach` MUST call `f` on every node in pre-order. `[TT-6]`
- `exists` MUST return `true` as soon as `p` returns `true`. `[TT-7]`
- `exists` MUST NOT call `p` on any node after the first match. `[TT-21]`
- `foldLeft` MUST apply `f` to nodes in pre-order. `[TT-8]`
- `transformUp` MUST rewrite all children of a node before it calls `f` on that node. `[TT-9]`
- `transformDown` MUST call `f` on a node, then walk the children of the result of `f`. `[TT-10]`
- `transformUp` and `transformDown` MUST call `f` at most once per node. `[TT-11]`

**Why:** `[TT-5]` lets a pass keep scope state without recursion. `freeVars` pushes a
binder on `enter` and pops it on `exit`. `ForcedBuiltinsExtractor.count` counts lambda
depth the same way. `[TT-11]` keeps the combinators predictable. A pass that needs a
fixpoint loops itself.

*Example, illustrative.* `transformDown` with `f` = "replace `Force(Delay(x))` by `x`"
on `Force(Delay(Force(Delay(c))))`. `f` turns the root into `Force(Delay(c))`. The walk
then goes into the children of that result, so it visits `Delay(c)` and `c`. It does not
call `f` on the new root again. The result is `Force(Delay(c))`, not `c`.

**Identity preservation**

- `mapChildren`, `mapChildrenIndexed`, `transformUp` and `transformDown` MUST return the
  input node (`eq`) when every child result is `eq` to the old child and `f` returns its
  argument. `[TT-12]`
- A rewrite MUST allocate a new `List` for `Constr` arguments or `Case` branches only
  when at least one element changed. `[TT-13]`

**Why:** the CSE size and key caches and `Inliner.cachedFreeVars` are keyed by object
identity. A copy of an unchanged subtree empties those caches.

*Example, illustrative.* `transformUp(t)(identity)` returns `t`. A rewrite of one leaf at
depth 100 allocates 100 new nodes on the path to the root and no others.

**Allocation and stack**

- `[TT-14]` *Withdrawn 2026-10-30.* It said: each whole-tree combinator MUST use an
  explicit stack in a growable array, not JVM recursion.
- `[TT-15]` *Withdrawn 2026-10-30.* It set the initial size of that array to 64.
- A whole-tree combinator MUST NOT allocate per visited node. Exceptions: nodes that a
  rewrite rebuilds, and the result of `freeVars`. `[TT-16]`
- `[TT-17]` *Withdrawn 2026-10-30.* It said: each whole-tree combinator MUST finish on a
  term 1,000,000 levels deep, on a JVM thread with a 256 KB stack.
- One-level combinators MUST NOT allocate a `List` of children. `[TT-18]`
- Each whole-tree combinator MUST recurse on the JVM stack, one frame per tree level, as
  the hand-written walks do today. `[TT-22]`

**Why:** `[TT-18]` is why the API has no `children: List[Term]` method. `[TT-22]` keeps the
stack behaviour of today's passes: a term that does not overflow now does not overflow
after the migration.

*Implementation status of `[TT-14]`, `[TT-15]`, `[TT-17]`.* Step 1 built the explicit
stack (commit `2b6497586`), and it passed the 1,000,000-level test. Step 2 measured it
against JVM recursion (commit `61bffcccf`, us/op on auction_1-1 / future-increase-margin-5
/ stablecoin_1-5):

| Case | Recursive | Explicit stack | Explicit stack B/op |
|---|---|---|---|
| count all nodes | 10.6 / 35.6 / 43.6 | 16.8 / 51.6 / 62.8 | 1.9K to 3.9K, recursive 0 |
| `containsTrace` | 1.18 / 1.15 / 1.01 | 1.24 / 1.26 / 1.26 | 824, recursive 0 |
| `CaseConstrApply` | 20.0 / 63.5 / 76.1 | 43.6 / 141 / 170 | |

The explicit stack failed `[GT-7]` on every case except `freeVars`. The owner decided to
keep recursion for now. A depth-limited hybrid is the candidate fix, see Section 7.

**Inlining**

- Every combinator except `freeVars` SHOULD be `inline` with `inline` function
  parameters. `[TT-19]`
- When the JMH gate `[GT-4]` shows a whole-tree `inline` combinator slower than the
  hand-written code, the implementer MUST remove `inline` from that combinator only and
  measure again. `[TT-20]`

**Why:** `inline` places the callback body inside a local recursive function at each call
site. A shared non-inline function sees about 20 different lambdas, so its `Function1` call becomes megamorphic. The cost
is larger bytecode at each call site, and `[TT-20]` catches the case where that cost wins.

### 3.2 Checks delegated elsewhere

None.

### 3.3 Not in the API

- `children: List[Term]`. It allocates a list. See `[TT-18]`.
- `mapChildrenF[F[_]: Applicative]`. Through cats, every node would allocate an `Id` or
  `Eval` wrapper, and no pass needs effects.
- A fixpoint rewrite. See `[TT-11]`.
- Java and Kotlin entry points. Scala 3 emits no bytecode for an `inline def` with
  `inline` parameters (checked with `javap` on Scala 3.3.7), so Java cannot call this
  API. Deferred, see Section 7.

## 4. Migration

Each phase is one commit or a short series of commits. Phases run in order A, B, C,
because each phase is lower risk than the next.

- Every migrated pass MUST produce the same result as before, including child order
  `[TT-1]`, the stop point of `exists` `[TT-7]` `[TT-21]`, and identity `[TT-12]`. `[MG-1]`
- The implementer MUST delete the old hand-written walk in the same commit that migrates
  it. `[MG-2]`
- The private `children` and `mapChildren` in `CommonSubexpressionElimination` MUST be
  deleted, and their callers MUST use `TermTraversal`. `[MG-3]`

### 4.1 Phase A: queries

| Today | Becomes |
|---|---|
| `PartialEvaluator.containsTrace`, `PartialEvaluator.hasReducibleOp` | `exists` |
| `CommonContextExtraction.containsError`, `containsHole`, `referencesPartialBuiltin` | `exists` |
| `ScalusTag.containsMarkedError` | `exists` |
| `Term.collectBuiltins`, `TermAnalysis.collectNames`, CCE `collectTemplateFunctions` | `foreach` |
| CSE `size`, CSE `termSize` | `foldLeft` |
| `ForcedBuiltinsExtractor.count` | `visit` |
| `Inliner.analyzeOccurrence` | `visit` |
| `TermAnalysis.freeVars`, `Inliner.cachedFreeVars` | `TermTraversal.freeVars` |

`hasReducibleOp` looks only at the root and at `Constr` arguments. The implementer
decides whether `exists` with `enter` returning `false` keeps that shape. If not, it
stays hand-written.

`Inliner.cachedFreeVars` caches per node by identity. It MAY keep its cache and use
`visit` to fill it, instead of calling `TermTraversal.freeVars`. `[MG-4]`

*Implementation status, Phase A (2026-10-30).* The table above was written before each
walk was read in full. Done differently:

| Walk | Done | Why |
|---|---|---|
| `PartialEvaluator.hasReducibleOp` | Stays hand-written | It looks only at the root and at `Constr` arguments, not the whole tree. |
| CCE `referencesPartialBuiltin` | Stays hand-written | It skips `LamAbs` and `Delay` bodies and returns `true` for any `Apply(Var, _)`. `exists` cannot skip a subtree. |
| CCE `collectTemplateFunctions` | Stays hand-written | It walks the `Apply` spine only. |
| CSE `size` | Stays hand-written | A cached, bottom-up `Option[Int]`, not a fold over nodes. |
| `Inliner.analyzeOccurrence` | Recursive `go(t, guarded)`, `foreachChild` for the default case | A `Case` scrutinee is unguarded and its branches are guarded. `visit` cannot tell children apart. |
| `Inliner.cachedFreeVars` | `visit` fills the identity cache, per `[MG-4]` | Keeps the per-pass cache that avoids quadratic queries. |
| `Term.usesConstrOrCase` | `exists` | Not in the table above, same walk. |

### 4.2 Phase B: simple rewrites

| Today | Becomes |
|---|---|
| `CaseConstrApply.go` | `transformDown` |
| `PrepareForSerialization.transform` | `transformDown` |
| CCE `replaceHole`, CCE `substituteChildren` | `transformDown` or `mapChildren` |
| `TermSanitizer.sanitizeTerm` | `transformUp` |
| `StrictIf.go`, `LetChainRegroup.go`, `EtaReduce.etaReduce` | `transformUp` if the pass is a pure local rewrite, else `mapChildren` |

*Example, illustrative.* `PrepareForSerialization.transform` becomes:

```scala
private def transform(term: Term): Term = term.transformDown {
    case Const(c, a) if containsBLSElement(c.tpe) => transformBLSConstant(c, a)
    case t                                       => t
}
```

`CaseConstrApply.go` matches on the whole `Apply` spine, not one node. The implementer
MUST check that `transformDown` sees the same spine before migrating it. `[MG-5]`

*Implementation status, Phase B (2026-10-30).*

| Walk | Done | Why |
|---|---|---|
| `PrepareForSerialization.transform`, CCE `replaceHole` | `transformUp`, not `transformDown` | `transformDown` would walk into the replacement term. |
| `TermSanitizer.sanitizeTerm` | `transformDown` | Fresh names depend on pre-order. `transformUp` would rename a body before its binder. |
| CCE `substituteChildren` | `mapChildren` and `mapChildrenIndexed` | Path IDs are allocated in the same order as before. |
| `LetChainRegroup.go`, `EtaReduce.etaReduce` | `mapChildren` default case | Both carry an environment, the Phase C pattern. |
| `StrictIf.go` | Own recursion, `mapChildren` default case | `transformUp` checks the pattern on the rebuilt node. A `Force` around an `if` made strict then matches as a second lazy `if`. Caught by `StrictIfTest` "transforms lazy if inside Force". |
| `CaseConstrApply.go` | Own recursion with a spine-length check, `mapChildren` default case | Changed 2026-10-31 from `transformDown`. `transformDown` walked the new `Case` and `Constr` and built both again once their children changed. With the linear `Term.applyToList` it measured 5 to 8% slower than the old code; own recursion is 7 to 13% faster. |

Behaviour change, approved by the owner on 2026-10-30. The old `CaseConstrApply` rebuilt
spines of one or two arguments without annotations. The old `StrictIf` rebuilt a lazy `if`
that stays lazy without annotations. Both now keep the original nodes, so source positions
survive. Script bytes do not change. Tests: `CaseConstrApplyTest` and `StrictIfTest`,
"keeps ... source position".

### 4.3 Phase C: passes with an environment

These passes keep their own recursion. Only the default case changes:

```scala
case _ => t.mapChildren(go(_, env))
```

- `Inliner.go` (both), CCE `go`, CCE `collectPass`, CCE `reCollect`
- CSE `uniqueBinders` and the CSE path walk, through `mapChildrenIndexed`
- `DeBruijn.process`, `DeBruijn.go`, `ScalusTag.tagFirstError`, `ForcedBuiltinsExtractor.go`

These passes stay recursive, like the combinators `[TT-22]`.

*Implementation status, Phase C (2026-10-30).*

| Walk | Done | Why |
|---|---|---|
| `Inliner.substitute` (inner `go`), `ForcedBuiltinsExtractor.go`, `DeBruijn.deBruijnTerm` | `mapChildren` default case | As planned. `deBruijnTerm` measured equal or faster, with 4% less allocation. |
| `DeBruijn.fromDeBruijnTerm` | Stays | Every `Var` is renamed, so every node above one is rebuilt, and the identity checks only cost time. JMH, 4 forks, auction_1-1: hand-written 18.73 ± 0.34 us, `mapChildren` 19.12 ± 0.28 us. Fails `[GT-7]`. |
| CSE `uniqueBinders`, CSE `extract` | `mapChildren`, `mapChildrenIndexed` | As planned. The private `children` and `mapChildren` are deleted `[MG-3]`. |
| CSE `collect` | `foreachChildIndexed` | Added to the API: an indexed walk without a rewrite. `children(t).zipWithIndex` allocated two lists per node. |
| CCE `collectPass`, `reCollect` | `foreachChild`, `foreachChildIndexed` | A `Delay` body and each `Case` branch still get a fresh path ID, in the same order. |
| CCE `insertLetAtPath` | `mapChildren` default case | Not in the plan, same pattern. |
| `Inliner.go` | Stays | It has no default case: each case passes different parameters. |
| `ScalusTag.tagFirstError` | Stays | It stops at the first `error`. `mapChildren` cannot stop part-way. |
| `DeBruijn.checkScope` | Stays | It does not look inside `Constr` or `Case`, as Plutus does. |

### 4.4 Stays hand-written

| Code | Why |
|---|---|
| `Cek` | Hot VM path with its own frames. |
| Flat `bitSize`, `encode`, `decode` in `Term.scala` | Serialization hot path. |
| `Term.toString`, the pretty printer, `TermNaming.describeTerm` | Each case prints different text. |
| `Term.α_==`, `Term.~=~` | They walk two terms at once. |
| `TermAnalysis.isPure`, `TermAnalysis.isValueForm`, CSE `structuralHash`, CCE `decomposeImpl` | Each case has different logic. A combinator saves nothing. |
| `Term.annotation`, `Term.withAnnotation` | One level, no children. |

### 4.5 Phase D: walks outside the passes

Added 2026-10-31. The census for Phases A to C covered only `scalus-core/shared/src/main`.
The owner found more walks in tests and other modules; a search of every tracked Scala file
then found about 50. This phase covers the ones that walk a `Term`.

- `TermTraversal` MUST provide `nodeCount`, the number of nodes of a term. `[MG-6]`
- Every copy of a node count MUST call `nodeCount`. `[MG-7]`
- A walk that duplicates `Term.usesConstrOrCase` or `Term.collectBuiltins` MUST call that
  method. `[MG-8]`

**Why:** five places counted nodes, each with its own recursive match: CSE `termSize`,
JIT `StagingCompilerThread.nodeCount`, `BooleanOptimizerImpactTest`,
`CceImpactMeasurementTest` and `MpfCostAnalysisTest`. `nodeCount` is an extension in
`TermTraversal`, not a member of `Term`: it reads the same, and `Term`'s public API, which
must stay binary compatible, does not grow. CSE `termSize` stays as a forwarder, because
released bytecode has it.

| Walk | Done |
|---|---|
| The five node counts | `nodeCount` `[MG-7]` |
| `UplcPipelineTest.containsCaseOrConstr`, `EcdCapeTest.builtins` | `usesConstrOrCase`, `collectBuiltins` `[MG-8]` |
| `UplcSourceMap.mayCollideWithMarkers`, `UplcSourceMap.hasSourceInfo` | `exists` |
| JIT `ConstantPool` collection | `foreach`; pre-order keeps the constant indices |
| `DropLists.apply` (verification), `EtaReduceCekConformanceTest.caseConstrToApply` | `mapChildren` default case |
| `CseCekConformanceTest.nonLetNodes` | `foreachChild` default case |
| Test queries: `hasCaseOnDataShape`, `containsBuiltin`, `hasErrorNode`, `containsDropList`, `allLocated` | `exists` |
| Test collections: `collectFunctionNames`, `collectVars`, `listElemTypes`, `SourcePositionCoverageTest` node count, `MpfCostAnalysisTest.countBuiltins` | `foldLeft`, `foreach` |
| Test folds: `countMarkers`, `maxVarIndex` | `foldLeft` |

`IntrinsicResolverTest.containsBuiltin` and `DropListTest.containsDropList` did not look
inside `Constr` or `Case`, the failure mode named in Section 1. `exists` looks everywhere.

Stays hand-written:

| Code | Why |
|---|---|
| `EtaReduceCekConformanceTest.legacyEtaReduce` | A frozen copy of the old pass, kept as the reference. |
| `CseTranslationValidation.isStrictIn` | Each case has different evaluation logic. |
| `PlutusConformanceTest.isClosed` | Carries the binder depth. Unlike `DeBruijn.checkScope`, it looks inside `Constr` and `Case`. |
| `JitCodegenProbe.depth` | A maximum over children, not a fold. |
| `UplcSourceMap.render`, `UplcSourceMap.index`, JIT `genCode`, `UplcBlaster.term` | Each case emits different text or code. |
| Cek `discharge*` | Walks `CekValue`, not `Term`. |

Not `Term` walks, though they match the same constructor names: SIR passes
(`MutualRecursionElimination`, `SIR.size`, `LetFloating`, `BooleanOptimizer`,
`StaticArgumentTransformation`) and Scala macro trees (`UtxoQueryMacros`, `UtxoFlowMacros`,
`PropMacro`).

## 5. Gates

**Output**

- Before Phase A, the implementer MUST save the golden output from master in a temporary
  file outside git. `[GT-1]`
- After each phase, `PipelineDeterminismChild.render()` MUST print text identical to the
  golden output. `[GT-2]`
- `TermAnalysisCekConformanceTest`, `EtaReduceCekConformanceTest` and
  `CseTranslationValidation` MUST pass after each phase. `[GT-3]`

**Why:** `[GT-2]` covers more than 40 example validators byte for byte. Any change in
child order or a missed child shows as a diff.

**Performance**

- A new JMH benchmark `bench/src/main/scala/scalus/uplc/TermTraversalBenchmark.scala`
  MUST compare old and new code on the `.flat` scripts of `BenchData`. `[GT-4]`
- `[GT-4]` MUST cover `freeVars`, `containsTrace`, `CaseConstrApply` and the full
  `V3Optimizer` pipeline. `[GT-5]`

*Implementation status of `[GT-5]` (2026-10-31).* `containsTrace` is private, so the
benchmark could only time a copy of it, which can drift from production. It now times
`Term.usesConstrOrCase`, a public `exists` query, through the production method. The
benchmark scripts have no `constr` or `case`, so this query walks the whole tree.
- The implementer MUST copy each old implementation into the benchmark file before
  deleting it, so one JMH run compares both. `[GT-6]`
- For each case, the new mean time MUST be inside the error bars of the old mean, or
  lower. `[GT-7]`
- For each case, the new `gc.alloc.rate.norm` from `-prof gc` MUST NOT be higher than
  the old value. `[GT-8]`
- The final commit message MUST include the JMH table. `[GT-9]`

*Example, illustrative.* Old `containsTrace`: 12.0 ± 0.4 us, 0 B/op. New: 12.3 ± 0.3 us,
0 B/op. Both rules pass: 12.3 is inside 12.0 ± 0.4, and 0 B/op is not higher. New 12.6 us
fails `[GT-7]`.

**Unit tests** (`scalus-core/shared/src/test/scala/scalus/uplc/internal/TermTraversalTest.scala`)

- Each rule `[TT-1]` to `[TT-13]` MUST have a test that cites its ID. `[GT-10]`
- A property test MUST compare every whole-tree combinator with a plain recursive
  reference on generated terms. The generator MUST produce every `Term` case. `[GT-11]`
- `[GT-12]` *Withdrawn 2026-10-30* with `[TT-17]`. It required a JVM test of `[TT-17]`.
- `[GT-13]` *Withdrawn 2026-10-30* with `[TT-17]`. It required the same test on Scala.js at
  100,000 levels.

**Why:** the 2026-09-10 spec found CSE properties whose generator never produced
`Case`, `Delay`, `Error` or a lambda, so the property could not fail. `[GT-11]` closes
that gap for this work.

**Build**

- `sbtn quick` MUST pass with 0 compiler warnings after each phase. `[GT-14]`

## 6. Risks

| Risk | Mitigation |
|---|---|
| A migrated pass misses a child or changes order | `[GT-2]` diffs the bytes of every example. |
| `inline` grows bytecode and slows the JIT | `[TT-20]` with `[GT-4]`. |
| A rewrite copies unchanged subtrees and empties identity caches | `[TT-12]`, tested by `[GT-10]`, measured by `[GT-8]`. |
| An iterative `transformUp` is slower than recursion | Happened. See the implementation status note under `[TT-14]`. |

## 7. Deferred

- **Java and Kotlin entry points.** Non-inline methods with `java.util.function` types
  and `java.util.stream` names (`forEach`, `anyMatch`, `fold`, `walk`, `rewriteUp`,
  `rewriteDown`), each a one-line call to the `inline` version. Test file:
  `scalus-core/jvm/src/test/java/scalus/uplc/internal/JavaTermTraversalInterop.java`.
- **Stack safety.** All walks recurse on the JVM stack `[TT-22]`. The candidate fix is a
  hybrid: recurse up to a fixed depth, then hand the subtree to an explicit stack. Normal
  terms then keep the recursive speed. The explicit-stack code is in commit `2b6497586`.
  The same follow-up covers the Phase C passes.
- **SIR traversal.** The same idea for `scalus.compiler.sir`.

## 8. Order of work

1. Write `TermTraversal` and `TermTraversalTest`. Save the golden output `[GT-1]`. Done.
2. Write `TermTraversalBenchmark` with the old implementations `[GT-6]`. Done. After the
   `[TT-14]` amendment, re-run it on the recursive combinators.
3. Phase A. Run `[GT-2]`, `[GT-3]`, `[GT-14]`.
4. Phase B. Same gates.
5. Phase C. Same gates.
6. Run the full JMH benchmark. Put the table and the line count in the final commit
   message `[GT-9]`. Done 2026-10-30.

*Implementation status of step 6.* The first branch run of `V3Optimizer` failed `[GT-7]`:
231.2 ± 8.9 ms against 222.5 ± 2.7 ms on master, measured in the same session. Two
allocations from Phase A caused it. `Inliner.cachedFreeVars` started each union from
`Set.empty`, so a one-child node copied its child's set instead of sharing it.
`TermTraversal.freeVars` created a `HashMap` on every call, also for the small terms that
`EtaReduce` passes. After both fixes: 219.6 ± 4.1 ms (3 forks) and 7.4% less allocation
than master.

7. Phase D (2026-10-31). Gates: tests of every module touched (core, examples, JIT,
   verification), `[GT-2]`, `[GT-14]`.
