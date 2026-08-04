# T2b-simple: self-application recursion in the simple backends (design)

Date: 2026-08-04. Branch: `feature/codegen-improvements`.
Follow-up to `2026-08-03-t2-self-application-recursion-design.md` (V3 backend).

## Goal

Apply the T2 self-application recursion encoding to the legacy simple
backends (`BaseSimpleLowering`, shared by `ScottEncodingLowering` and
`SumOfProductsLowering`), removing the Z combinator there too.

## Scope decisions (carried over from T2)

- Default on, no option flag; validated by the corpus + budget re-pin.
- Mutual recursion stays out of scope (`BaseSimpleLowering.scala:613` TODO).
- `ExprBuilder.ZTerm`/`Z`/`z`/`rec` stay (public API).

## Encoding

Single site: the `flags.isRec` case of `BaseSimpleLowering.lowerInner`
(`:600-609`). A repo grep confirms no other `__Z` emitters.

Current: `(λf. body) (__Z (λf. rhs))`. New (T2 v2 shape):

```scala
case SIR.Let(Binding(name, tp, rhs) :: Nil, body, flags, _) if flags.isRec =>
    val selfApply = Term.Apply(Term.Var(NamedDeBruijn(name)), Term.Var(NamedDeBruijn(name)))
    val rhsSelfApp = Inliner.substitute(lowerInner(rhs), name, selfApply)
    val fixpoint = Term.Apply(Term.LamAbs(name, selfApply), Term.LamAbs(name, rhsSelfApp))
    Term.Apply(Term.LamAbs(name, lowerInner(body)), fixpoint)
```

The fixpoint stays a closed argument-position subterm, preserving
Inliner + PartialEvaluator constant folding (the T2 lesson).

## Safety: non-unique names

Unlike V3's `uniqueVarName` ids, simple-backend names come straight from
SIR and can shadow. `Inliner.substitute` is shadow-correct: it stops at
any lambda rebinding `name` (including a nested letrec of the same name,
whose encoding wraps everything in `LamAbs(name, ...)`), and its
alpha-renaming branch cannot fire because the replacement's only free
variable is `name` itself.

## Cleanup + binary compatibility

- Delete the root `__Z` binding in `lower()` (`:47-50`) and the flag
  write in the letrec case.
- `zCombinatorNeeded` is a `protected var` on a public class: keep it as
  a deprecated no-op (same MiMa treatment as
  `LoweringContext.zCombinatorNeeded`); zero MiMa filters.

## Validation (TDD order)

1. RED: extend `SelfApplicationRecursionTest` with Scott and
   SumOfProducts variants - no `__Z` in output, correct results, budget
   below the measured Z baseline - plus a shadowing test (nested
   recursive def reusing the outer name), the one new risk.
2. GREEN: implement; run the full corpus.
3. Re-pin budgets: `scripts/update-budgets.py` + the known manual tail:
   `ExprSizeAndBudgetTest."Recursion cost"` (Scott pin cpu=128352,
   mem=702 per recursion, and its lovelace/USD derivations),
   `MintingPolicyExampleTest."Optimized"` (672 B). Grep tests for golden
   UPLC text containing `__Z`. Re-measure both Scala generations.
4. `mima` clean.

Expected: the same 6-machine-steps-per-recursive-call saving as T2;
improves PV10-era (Plomin) output where the simple backends are used.
