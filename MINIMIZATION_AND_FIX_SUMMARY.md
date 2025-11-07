# Cosmex Shadowing Bug: Complete Analysis

## Achievement

Successfully minimized test case from **952 lines → 96 lines (89.9% reduction)**

## The Bug

**Location:** `scalus-plugin/src/main/scala/scalus/PatternMatchingCompiler.scala:122-129`  
**Method:** `PatternMatchingContext.createLeaf`

### Root Cause

When Scalus compiles nested pattern matches (especially from inline functions), it flattens all pattern bindings into a single decision tree. If variable names appear in multiple pattern scopes, the binding map construction overwrites earlier bindings:

```scala
m + (nameInfo.name -> cb.name)  // Overwrites if name exists!
```

### Example

```scala
action match
  case Close(signedSnapshot) =>              // Creates: signedSnapshot -> col_0
    handleClose(state, signedSnapshot, txOut)

inline def handleClose(..., newSignedSnapshot: SignedSnapshot, ...) =
  newSignedSnapshot match
    case SignedSnapshot(signedSnapshot, _, _) =>  // Creates: signedSnapshot -> col_5 (OVERWRITES!)
      validSignedSnapshot(newSignedSnapshot)
```

Result: Any reference to `signedSnapshot` resolves to `col_5` (Snapshot type) instead of `col_0` (SignedSnapshot type).

## Key Insight From Your Question

The Scala code is **actually valid**! There's no real shadowing because:
- Outer `signedSnapshot` is passed as parameter `newSignedSnapshot`
- Inner `signedSnapshot` is a new variable in nested scope
- Code references `newSignedSnapshot` (parameter), not `signedSnapshot`

The bug is in **Scalus's compilation strategy**, not the user's code.

## Proposed Solutions

### Immediate Fix (Recommended)

Add collision detection in `createLeaf()`:

```scala
if m.contains(nameInfo.name) then
    report.error(
      s"Variable '${nameInfo.name}' appears in multiple pattern match scopes...",
      p.pos
    )
    m  // Keep first binding
else
    m + (nameInfo.name -> cb.name)
```

**Pros:** Simple, safe, immediate deployment  
**Cons:** Users must rename variables in valid Scala code

### Long-term Fix (Your Suggestion!)

Add scope tracking to `BindingNameInfo`:

```scala
case class BindingNameInfo(
    name: String,
    scalaName: Option[String],
    tp: SIRType,
    scopeDepth: Int = 0,  // Track nesting level
    aliases: Set[BindingNameInfo] = Set.empty
)
```

Use qualified names: `s"${name}@${scopeDepth}"` in binding maps.

**Pros:** Handles Scala scoping correctly, no user code changes  
**Cons:** More complex, requires refactoring

## Files

- `ContractStep4.scala` - Minimal reproduction (96 lines)
- `CompileCosmexStep4Test.scala` - Test case
- `BUG_FIX_PROPOSAL.md` - Detailed fix proposal
- `MINIMIZATION_AND_FIX_SUMMARY.md` - This file

## Recommendation

1. **Immediate:** Implement error detection (Option 2) for Scalus 1.x
2. **Long-term:** Implement scope tracking (Option 3) for Scalus 2.0

Your insight about renaming is excellent - it's the proper solution that respects Scala's scoping rules!
