# Fix Status for Cosmex Shadowing Bug

## Current Status: Partial Fix Attempted

We added shadowing detection in `PatternMatchingCompiler.createLeaf()` but the test still fails with a different error.

## What We Fixed

Added collision detection in line 127-135 of `PatternMatchingCompiler.scala`:

```scala
if m.contains(nameInfo.name) then
    report.error(
      s"Variable '${nameInfo.name}' appears in multiple pattern match scopes...",
      p.pos
    )
    m // Keep the first (outer) binding
else
    m + (nameInfo.name -> cb.name)
```

## Why It's Not Working Yet

The shadowing detection at the leaf level doesn't catch this particular case because:

1. The two `signedSnapshot` variables might be in **different rows** of the decision tree
2. The nested matches from inline functions might be compiled separately
3. The collision happens during decision tree construction/flattening, not within a single leaf

## Original Error

```
CaclulateApplyTypeException: Cannot calculate apply type for 
SignedSnapshot -> ... to Snapshot, function type does not match argument type
```

## Current Error After Fix

```
LoweringException: Unsupported representation ProdDataConstr for match expression
```

The error changed and occurs at a different line, suggesting our fix has some effect, but doesn't fully resolve the issue.

## Next Steps

1. **Deep Investigation:** Trace exactly how nested matches from inline functions are merged into the decision tree
2. **Scope Tracking:** Implement proper scope tracking in `BindingNameInfo` as described in Option 3 of BUG_FIX_PROPOSAL.md
3. **Renaming Strategy:** Automatically rename bindings at decision tree construction time, not just at leaf level

## Recommendation

Revert the partial fix and implement the full solution with scope tracking:
- Add `scopeDepth: Int` to `BindingNameInfo`
- Track scope depth when creating column bindings during `insertConstructorPatternsIntoGroup`
- Use qualified names in binding maps: `s"${name}@${scopeDepth}"`

This requires more comprehensive changes but will properly handle Scala's scoping rules.
