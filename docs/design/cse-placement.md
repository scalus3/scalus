# CSE binding placement

CSE remains enabled with its existing iteration defaults. The implementation follows the
ancestor-or-self rule in Plutus 1.63.0.0. It shares partial computations as well as total ones,
without speculative evaluation or merging sibling branches.

## Contract

The contract is Plutus's CSE contract: preserve results and success/failure, allowing trace
count, trace order and failure diagnostics to change. Execution budgets are measured separately;
equal outcomes under an identical finite budget are not a general equivalence property.

Inputs use named variables, as in the existing optimizer pipeline. Every binder receives a
globally unique name before collection, preserving existing names where they are already unique.
Fresh names avoid every input name, including free variables. Renaming alone returns the original
term when there is no extraction.

## Placement invariant

A region ends at a lambda body, delay body or Case branch. Immediately applied lambda bodies
remain in their parent's region, just as in Plutus. Occurrences merge only when their regions
are ancestor-related. Every resulting group therefore has an actual occurrence in its shallowest
region; an occurrence in each sibling branch is insufficient.

Within a group, the binding is placed at the structural least common ancestor of its occurrences.
Candidate keys normalize internal binders for alpha-equivalence while retaining the unique
identities of free references. Different captures therefore remain distinct. Since every occurrence is inside
the scope of each variable it references, their structural common ancestor is inside that scope
too. This achieves the innermost placement of Plutus's `placeCseBinding`, including descent into
immediately applied lambda bodies.

Paths contain child positions. Each round collects from the current term and applies exactly one
extraction, using those paths in a single substitution-and-insertion walk. Later rounds collect
again. No traversal counter has to be synchronized after an edit. Candidates are ranked by their
estimated lovelace saving; ties use deterministic traversal order, not names or hash-map iteration order.

## Candidate selection

Every structurally repeated term is a candidate, including constants, delayed values and forced
builtins. There is no work-free filter or builtin exception. Candidates must pass the placement
invariant above. `SharingCost` ranks them by positive estimated net lovelace savings:

```
savedBits = (n - 1) * termBits(expr) - n * VarBits - 8
savedLovelace = savedBits * referencePricePerBit - bindingExecutionFee
bindingExecutionFee = applyFee + lambdaFee + varFee
```

For a one-node value, n evaluations become n variable lookups plus one retained value, Apply and
LamAbs. The extra work is three node evaluations. For larger expressions the estimate gives no
credit for avoided computation. It assumes one execution of the binding and uses the repository's
mainnet reference prices at the first reference-script tier; it does not predict execution frequency
or total transaction fees. CCE separately prices one-hole templates in encoded bits and charges
its additional execution steps, as described below.

Each round chooses the greatest positive net saving and recollects after applying it. A positive
net saving requires positive bit savings, so decreasing additive `termBits` ensures termination.
The bit estimate assumes minimum-width variable indices and worst-case byte-array padding.
Marginal extractions can therefore increase actual serialized size. This affects profitability,
not semantic safety. A regression test covers this at an index-width boundary.

The inliner shares the same cost calculation. It keeps exact occurrence counts, including guarded
uses and excluding shadowed uses. For multiple uses, only variables, constants and builtins are
eligible for duplication, and only when the estimated sharing saving is zero or negative. The
single-use safety rules are unchanged. Opposite decisions at the same threshold prevent it from
undoing profitable constant sharing. Before pricing a constant binding, the inliner simplifies
its body using known constants from enclosing bindings. It substitutes these constants only into
closed evaluation candidates: successful evaluation replaces the expression with its result;
failed, trace-containing or still-open expressions keep their original shared variables. Lambda
shadowing removes the enclosing constant from scope. Occurrence counting and profitability run
on the simplified body, so sharing does not hide opportunities for constant folding.

`IfThenElse` needs no special branch handling. It receives evaluated arguments, and lowering puts
branch bodies in delays. Their bodies remain separate regions. The whole delayed values can be
shared without forcing them, even when sharing their bodies across sibling regions is forbidden.

The combined fee estimate does not guarantee lower CPU, memory or total transaction fees. Sharing a large constant can introduce extra work each time its scope executes. Small but
expensive repeated calls can be rejected when their binding adds bits. Execution budgets are
measured separately. Greedy choices and subsequent optimizer passes also mean that the final
pipeline output need not be smaller than a different CSE implementation's output.

The legacy `isSkippable`, `containsError`, shape-builtin list and variable-prefix checks
live in `CommonContextExtraction` and serve only CCE. Deprecated forwarding methods preserve
the emitted 1.1.0 methods for binary compatibility.

## Validation

`CseTranslationValidation` is a test-only port of Plutus's UCSE translation relation: congruence,
or a let whose variable is strict in its body and whose back-substitution relates to the input.
It works on de Bruijn indices and does not reuse the optimizer's names, paths or grouping.
It is an independent executable check, not a machine-checked proof of this Scala implementation.
Cycle detection and a work limit keep invalid recursive outputs from hanging the validator;
exhausting that limit rejects validation rather than accepting an unverified rewrite.

Bounded CEK tests compare values (including observations of returned closures) and success/failure.
They exercise Case, Delay, lambdas, repeated binder names, traces and partial builtins. Negative
oracle tests reject speculation and capture. Seeded coverage checks require actual rewrites,
successful evaluations and failures. Budget exhaustion is detected separately; it is not accepted
as evidence that two terms are equivalent.

The existing cross-JVM tests check deterministic output. `scripts/cse-corpus.py` snapshots and
compares all generated blueprint hashes and compiled sizes after `scalusExamplesJVM/blueprint`.

CCE has its own bit-based profitability model. For `n` occurrences it credits the duplicated
skeleton, subtracts the application/variable framing, and charges `3n + 3` additional CEK steps
using the repository's mainnet reference prices. Its minimum template size follows from this
formula. It assumes each site executes once, so this is a heuristic rather than a guarantee of
lower fees. Decomposition stops at `Delay` boundaries to keep deferred leaves from becoming
eager arguments. Its legacy filters live in its companion object.

The speculative/all-branches CSE placement rules from the earlier design are not implemented.

## References

- [Plutus CSE, 1.63.0.0](https://github.com/IntersectMBO/plutus/blob/1.63.0.0/plutus-core/untyped-plutus-core/src/UntypedPlutusCore/Transform/Cse.hs)
- [Strictness relation](https://github.com/IntersectMBO/plutus/blob/1.63.0.0/plutus-metatheory/src/Untyped/Strictness.lagda.md)
- [UCSE translation relation](https://github.com/IntersectMBO/plutus/blob/1.63.0.0/plutus-metatheory/src/VerifiedCompilation/UCSE.lagda.md)
- [Plutus discussion: binding overhead when sharing forced builtins](https://github.com/IntersectMBO/plutus/pull/5634#discussion_r1402541979)
- [Ledger reference-script fee calculation](https://github.com/IntersectMBO/cardano-ledger/blob/master/eras/conway/impl/src/Cardano/Ledger/Conway/Tx.hs)
