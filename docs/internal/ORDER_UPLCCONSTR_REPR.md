# `Order` as a native UPLC `constr`

Status: landed. Branch `order-uplcconstr`, rebased on master `a414a1141`. Measured 2026-08-27 on
Scala 3.3.8 / PV11 / `Options.release`.

`scalus.cardano.onchain.plutus.prelude.Order` now carries
`@UplcRepr(UplcRepresentation.UplcConstr)`. Its values are native `constr N []` terms instead of
`Data`, so matching on a comparison result is a single `case` with no `Data` round trip.

## What changed in the generated UPLC

`(a <=> b) match { case Less => 1; case Equal => 2; case Greater => 3 }`

Before:

```
(case [(force (force (builtin fstPair))) [(builtin unConstrData)
      (case [(builtin lessThanInteger) a b]
          (case [(builtin lessThanInteger) b a] (con data (Constr 1 []))
              (con data (Constr 2 [])))
        (con data (Constr 0 [])))]] (con integer 1) (con integer 2) (con integer 3))
```

After:

```
(case (case [(builtin lessThanInteger) a b]
        (case [(builtin lessThanInteger) b a] (constr 1) (constr 2)) (constr 0))
  (con integer 1) (con integer 2) (con integer 3))
```

Every comparison used to build a `Data.Constr` and then read its tag back out. That cost
`unConstrData` (24 588 CPU) plus `fstPair` (141 895 CPU) plus two forces and two applies, on top of
the comparison itself. All of it is gone.

Micro-benchmarks, CPU steps:

| shape | before | after | Δ |
|---|---:|---:|---:|
| 3-way match, `Less` | 595 873 | 333 390 | −44.0% |
| 3-way match, `Equal`/`Greater` | 737 163 | 474 680 | −35.6% |
| `.isLess` | 595 873 | 333 390 | −44.0% |
| `ifEqualThen` chain (2 comparisons) | 1 604 936 | 1 063 970 | −33.7% |
| `Ord[Data]` on `I 1` vs `I 2` | 2 458 111 | 2 227 628 | −9.4% |

Script size for those shapes drops 36% (56 → 36 flat bytes for the 3-way match).

## The lowering gap this exposed

`Order` is the first *multi-constructor enum* to use `@UplcRepr(UplcConstr)`. Every earlier user
(`Tile`, `ChessSet`, `SolutionEntry`, `Queue`) is a single case class, where the annotated symbol
and the `ConstrDecl` are the same thing.

`SirTypeUplcGenerator.apply` reads the annotation from two places:

- `SIRType.SumCaseClass(decl, _)` reads `decl.annotations` – the enum's `DataDecl`. Finds
  `UplcConstr`, returns `SumCaseUplcConstrEmitter`.
- `SIRType.CaseClass(constrDecl, _, optParent)` reads only `constrDecl.annotations`, never
  `optParent`. For `Order.Less` that is empty, so it falls through to `ProductCaseEmitter`.

Conversions dispatch on the value's *type*, not its representation
(`SumDispatch.toRepresentation` picks the typegen from `SirTypeUplcGenerator(input.sirType)`). So
the same conversion succeeded or failed depending on how the value happened to be typed:

```
(a <=> b)   sirType = Order        repr = SumUplcConstr(0->…, 1->…, 2->…)   -> sumCaseImpl      OK
Order.Less  sirType = Order$.Less  repr = ProdUplcConstr(0, [])             -> ProductCaseEmitter FAIL
```

`(a <=> b) === Order.Less` needs both sides at the same representation, so it threw:

```
LoweringException: Unsupported conversion for Order$.Less
  from ProdUplcConstr(0,List()) to SumUplcConstr(TreeMap(0 -> …, 1 -> …, 2 -> …))
```

This hit `Ord[NormalizedInterval]` in `scalus-design-patterns`.

### Fix

`ProductCaseEmitter.emitConvert` gained the missing arm. `SumUplcConstrOps` already owns this
conversion (`prodUplcConstrToSumUplcConstr`); only the routing edge was absent.

```scala
case (_: ProdUplcConstr, _: SumCaseClassRepresentation.SumUplcConstr) =>
    SumUplcConstrOps.emitConvert(input, representation, pos)
```

### Rejected: propagating the annotation to variants in the plugin

Copying the sealed parent's `@UplcRepr` onto each variant's `ConstrDecl`
(`SIRCompiler.makeConstrDecl`, `SIRTyper.makeCaseClassConstrDecl`) looks like the root fix, so it
was tried and measured. **It does not fix the crash.** `ProductCaseUplcConstrEmitter.emitConvert`
resolves TypeVar inputs and then delegates to `ProductCaseEmitter.emitConvert`, landing on the same
missing arm; with a concrete `ProdUplcConstr` input the resolve step is a no-op. Applied on top of
the lowering arm it produced byte-identical budgets, so it was dropped.

The variant/sum annotation asymmetry therefore remains: `defaultRepresentation(Order.Less)` still
returns `ProdDataConstr`. Nothing depends on it today. A future caller that asks for a *variant*
type's default representation on an annotated enum would need the plugin change.

## Measured effect on real workloads

63 pinned baselines moved. All 63 moved down; none regressed.

| workload | CPU before | CPU after | ΔCPU | ΔMem |
|---|---:|---:|---:|---:|
| `SortedMap.delete` | 1 170 776 | 908 293 | −22.4% | −12.6% |
| `SortedMap.get` | 1 309 043 | 1 046 560 | −20.1% | −11.1% |
| `SortedMap.insert` | 1 579 138 | 1 316 655 | −16.6% | −9.0% |
| `SortedMap.getOrFail` / `at` | 10 757 748 | 9 803 816 | −8.9% | −4.8% |
| `List.quicksort` | 9 530 315 | 8 742 866 | −8.3% | −4.6% |
| `List.groupMapReduce` | 874 033 | 810 033 | −7.3% | −7.4% |
| `SortedMap` ToData/FromData | 7 277 141 | 6 752 175 | −7.2% | −4.8% |
| `SortedMap.length` / `size` | 8 485 388 | 7 960 422 | −6.2% | −4.0% |
| `SortedMap.find` | 9 700 330 | 9 175 364 | −5.4% | −3.4% |
| `SortedMap.filter` | 10 500 096 | 9 975 130 | −5.0% | −3.2% |
| Clausify F4 | 41 969 044 053 | 40 125 180 053 | −4.4% | −2.6% |
| `SortedMap.union` | 14 318 018 | 13 793 052 | −3.7% | −2.4% |
| `SortedMap.foldLeft` | 16 184 793 | 15 659 827 | −3.2% | −2.2% |
| Auction: end with winner | 57 767 333 | 56 339 860 | −2.5% | −2.4% |
| PaymentSplitter (optimized) spend | 13 443 300 | 13 212 817 | −1.7% | −1.1% |
| EditableNFT burn | 27 227 427 | 26 766 461 | −1.7% | −1.1% |
| Betting: oracle payout | 38 988 054 | 38 527 088 | −1.2% | −0.9% |
| Pricebet: player wins | 27 365 804 | 27 135 321 | −0.8% | −0.6% |
| `Value` +/− multi-asset cancel | 28 226 132 | 27 995 649 | −0.8% | −0.4% |
| Knights 100_8x8 | 145 298 597 251 | 145 217 006 269 | −0.1% | −0.1% |

Script size: the V1 and V2 minting-policy pins dropped 706 → 686 flat bytes (−2.8%).

The gain tracks how much of a workload is comparison. `SortedMap` lookups are mostly `Ord`, so they
gain most. Knights is dominated by list and board work, so it barely moves.

## Safety notes

- `Order` has no `ToData`/`FromData` instance. It is never serialized into a datum or redeemer, so
  no on-chain encoding changes.
- `SumCaseUplcConstrEmitter.canBeConvertedToData` is `true` and `defaultDataRepresentation` is
  `DataConstr`, so the `Data` form is still reachable when something demands it (for example an
  `Order` held in a `Data`-encoded container).
- The Scott and SoP lowering backends ignore `@UplcRepr`; both were checked and are unaffected.
- `constr`/`case` availability is not a new constraint. `SirToUplcV3Lowering` already emits `case`
  for booleans, so V1/V2 targets already required the Scott backend.

## Follow-up spotted, not done

`(a <=> b) === Order.Less` lowers to a generic recursive structural-equality helper
(`_sumEq_…Order…`) rather than the single `(case order True False False)` it could be. Comparing a
sum against a literal nullary constructor is a missed peephole in `LoweringEq`. It affects any
`@UplcRepr(UplcConstr)` enum, not just `Order`.

## Reproducing

`scalus-core/jvm/src/test/scala/scalus/compiler/sir/lowering/OrderReprLoweringTest.scala` pins the
lowering against the exact expected term – it parses the UPLC above and compares with `α_==` on the
De Bruijn form, so variable-name churn does not matter but any structural change does. It also
covers the `ProdUplcConstr -> SumUplcConstr` regression and checks the Scott and SoP backends.

Baselines were re-pinned on both Scala 3.3.8 (`pre38`) and Scala 3.8.4 (`since38`). Ten pins are
dual: Clausify F1-F5, Knights 100_4x4/6x6/8x8, and the two Auction budgets. Use
`++3.8.4; scalusJVM/test; scalusExamplesJVM/test` for the 3.8.4 side – never the `jvm` aggregate,
which pulls in modules that do not cross-build to 3.8.4.
