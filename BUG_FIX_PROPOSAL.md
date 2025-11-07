# Bug Fix Proposal: Variable Shadowing in Pattern Matching

## Bug Location

**File:** `scalus-plugin/src/main/scala/scalus/PatternMatchingCompiler.scala`  
**Method:** `PatternMatchingContext.createLeaf` (lines 111-132)

## The Problem

The bug occurs when inline functions contain nested pattern matches that use the same variable names as outer pattern matches. When the inline function is expanded, the variable bindings from different pattern match scopes get mixed together.

### What Actually Happens

In our minimal test case:

```scala
action match
  case Close(signedSnapshot) =>              // signedSnapshot: SignedSnapshot
    handleClose(state, signedSnapshot, txOut)

inline def handleClose(..., newSignedSnapshot: SignedSnapshot, ...) =  
  newSignedSnapshot match                   // Nested match creates NEW bindings
    case SignedSnapshot(signedSnapshot, _, _) =>  // signedSnapshot: Snapshot
      validSignedSnapshot(newSignedSnapshot)
```

**Key Insight:** In regular Scala, this works fine because:
1. `signedSnapshot` from outer match is passed as `newSignedSnapshot` parameter
2. The inner pattern creates a NEW variable `signedSnapshot` (different scope)
3. The reference `newSignedSnapshot` correctly points to the parameter

**But in Scalus compilation**, when both matches are compiled into a single decision tree (due to inlining), the column bindings get merged:

1. Outer match creates: `signedSnapshot -> column_X` (SignedSnapshot type)
2. Inner match creates: `signedSnapshot -> column_Y` (Snapshot type)  
3. When building the leaf binding map with `m + (nameInfo.name -> cb.name)`, the second entry **OVERWRITES** the first
4. Result: `newSignedSnapshot` in the action resolves using the wrong binding map entry

### The Core Issue in `createLeaf`

```scala
val binding = row.patterns.zip(columnBinding).foldLeft(Map.empty[String, String]) {
    case (m, (p, cb)) =>
        p.optNameInfo match {
            case None => m
            case Some(nameInfo) =>
                m + (nameInfo.name -> cb.name)  // BUG: Overwrites if name already exists!
        }
}
```

When pattern variables from nested matches (that were inlined) have the same name, the map silently overwrites the earlier binding, causing variables in the action to resolve to the wrong column.

## Why This Happens

The issue is that Scalus compiles nested matches (especially from inlined functions) into a **flattened** decision tree where all pattern bindings are collected into a single flat list of columns. If two patterns at different nesting levels use the same variable name, they end up as separate columns but with name collision in the binding map.

**Example:**
- Column 0: `signedSnapshot` (from outer match, type: SignedSnapshot)
- Column 5: `signedSnapshot` (from nested match in inlined function, type: Snapshot)
- Binding map after fold: `{"signedSnapshot" -> "column_5_name"}` (WRONG! Should be column_0)

## Proposed Fix Options

### Option 1: Automatic Renaming (Recommended)

When building the binding map, detect collisions and rename the **older** binding to preserve Scala's inner-scope-wins semantics:

```scala
val binding = row.patterns.zip(columnBinding).foldLeft(Map.empty[String, String]) {
    case (m, (p, cb)) =>
        p.optNameInfo match {
            case None => m
            case Some(nameInfo) =>
                if m.contains(nameInfo.name) then
                    // Collision detected! Rename the OLD binding to preserve it
                    val oldColumnName = m(nameInfo.name)
                    val renamedOldName = s"${nameInfo.name}$$outer${oldColumnName.hashCode.abs}"
                    // Add renamed old binding and new binding
                    m - nameInfo.name + (renamedOldName -> oldColumnName) + (nameInfo.name -> cb.name)
                else
                    m + (nameInfo.name -> cb.name)
        }
}
```

However, this requires tracking which variables come from which scope when compiling actions. The action compilation happens BEFORE the decision tree is built, so we'd need to refactor when/how variables are resolved.

**Better approach:** Detect collisions during pattern parsing and rename earlier in the pipeline, OR track scope information in BindingNameInfo.

### Option 2: Detect and Report Error (Simpler, Immediate Fix)

```scala
val binding = row.patterns.zip(columnBinding).foldLeft(Map.empty[String, String]) {
    case (m, (p, cb)) =>
        p.optNameInfo match {
            case None => m
            case Some(nameInfo) =>
                if m.contains(nameInfo.name) then
                    report.error(
                      s"Variable '${nameInfo.name}' appears in multiple pattern match scopes. " +
                      s"When inline functions contain nested matches, variable names must be unique across all nesting levels. " +
                      s"Please use a different variable name (e.g., '${nameInfo.name}2' or 'inner${nameInfo.name.capitalize}').",
                      p.pos
                    )
                    m  // Keep the first binding (outer scope)
                else
                    m + (nameInfo.name -> cb.name)
        }
}
```

### Option 3: Track Scopes in BindingNameInfo

Add scope tracking to `BindingNameInfo`:

```scala
case class BindingNameInfo(
    name: String,
    scalaName: Option[String],
    tp: SIRType,
    scopeDepth: Int = 0,  // NEW: track nesting level
    aliases: Set[BindingNameInfo] = Set.empty
)
```

Then use qualified names in binding maps: `s"${nameInfo.name}@${nameInfo.scopeDepth}"` to distinguish variables from different scopes.

## Recommendation

### For Immediate Fix: Option 2 (Error Detection)

**Option 2** is recommended for immediate deployment because:
1. ✅ Simple, surgical fix (5-7 lines of code)
2. ✅ Prevents silent incorrect compilation
3. ✅ Clear error message guides users to fix their code
4. ✅ Minimal risk - only adds error detection
5. ✅ Works correctly when there's no collision
6. ✅ Can be implemented and tested immediately

The error message should explain that this is a Scalus limitation and suggest renaming one of the variables.

### For Long-term Fix: Option 3 (Scope Tracking)

**Option 3** should be implemented in Scalus 2.0 to properly handle Scala's scoping rules:
1. Add `scopeDepth` to `BindingNameInfo`  
2. Track scope depth when creating column bindings
3. Use qualified names in binding maps when collisions occur
4. Allows valid Scala code to compile without modification

This requires more extensive refactoring but provides the correct long-term solution.

## Test Case

After the fix:
- `CompileCosmexStep4Test` should report: "Variable 'signedSnapshot' appears in multiple pattern match scopes"
- Users can fix by renaming: `case SignedSnapshot(snapshot, _, _)` or `case SignedSnapshot(ss, _, _)`

## Long-term Solution

For Scalus 2.0, consider **Option 2** (qualified names) to allow shadowing like regular Scala does, but this requires more extensive refactoring of the pattern matching compiler.

