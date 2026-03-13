# Intrinsic Providers Design

## Overview

This design enables representation-specific optimizations for methods like `List.isEmpty` by allowing the compiler to substitute optimized UPLC builtin implementations based on the runtime representation.

### Problem Statement

`List.isEmpty` compiles to a match expression:

```scala
def isEmpty: Boolean = this match
    case Nil => true
    case Cons(_, _) => false
```

Which generates UPLC:
```
(force (chooseList list (delay True) (delay False)))
```

But for `SumDataList` representation, this could be simply:
```
(nullList list)
```

### Solution

Provide representation-specific intrinsic implementations that the compiler substitutes during lowering when the representation is known.

---

## Implementation Status

| Component | Status |
|-----------|--------|
| `Module.anns` field, SIR version bump to (5,0) | **DONE** |
| `SIRModuleAnnotation` marker trait | **DONE** |
| `typeProxy` / `typeProxyRetData` functions | **DONE** |
| `BuiltinListOperations` / `BuiltinListOperationsV11` provider objects | **DONE** |
| `compiledModules()` plugin-intercepted function | **DONE** |
| `IntrinsicResolver` with hardcoded registry | **DONE** |
| Type variable substitution via `SIRUnify` | **DONE** |
| Protocol-version-aware provider selection | **DONE** |
| Integration in `Lowering.lowerNormalApp` | **DONE** |
| `ForcedBuiltinsExtractor` threshold fix | **DONE** |
| Test framework error comparison fix | **DONE** |
| Tests (`IntrinsicResolverTest`, budget updates) | **DONE** |

### What Was NOT Implemented (Design Changes)

1. **No `@IntrinsicProviders` annotation** — the original design proposed an annotation on `List` companion object. Instead, the mapping is **hardcoded** in `IntrinsicResolver.registry`. This avoids modifying `List.scala` and simplifies the implementation.

2. **No annotation-driven module discovery** — instead, a `compiledModules()` function is intercepted by the plugin to load provider modules at compile time.

3. **No `PackedListOperations` or `DataConstrListOperations`** — Phase 1 only implements `SumDataList` representation. Other representations are future work.

---

## Architecture (As Implemented)

### Key Finding: Cannot Access `.sirModule` Directly

The `sirModule` val is added to `@Compile` objects by `SIRPreprocessor` (runs after PostTyper phase). This means `.sirModule` is **NOT visible** during type-checking of files in the same compilation unit.

**Implication:** `IntrinsicResolver` cannot call `BuiltinListOperations.sirModule` directly on clean builds. Instead, we use `compiledModules()` — a stub function intercepted by the plugin at a later phase, which generates tree-level references to `.sirModule` that work at runtime.

```scala
// This FAILS on clean build:
lazy val modules = Map(
    BuiltinListOperations.sirModule.name -> BuiltinListOperations.sirModule
)

// This WORKS — plugin intercepts and generates correct tree:
lazy val modules = scalus.compiler.compiledModules(
    "scalus.compiler.intrinsics.BuiltinListOperations",
    "scalus.compiler.intrinsics.BuiltinListOperationsV11"
)
```

### Component Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                    Compilation Pipeline                          │
│                                                                 │
│  1. Plugin compiles @Compile objects → sirModule/sirDeps vals    │
│  2. Plugin intercepts compiledModules() → SIRLinker.readModules  │
│  3. IntrinsicResolver.defaultIntrinsicModules loaded lazily      │
│  4. Modules threaded to SirToUplcV3Lowering → LoweringContext    │
│  5. Lowering calls IntrinsicResolver.tryResolve per application  │
│  6. Resolver substitutes provider body, re-lowers with cached arg│
└─────────────────────────────────────────────────────────────────┘
```

---

## Provider Objects

**File:** `scalus-core/shared/src/main/scala/scalus/compiler/intrinsics/ListIntrinsics.scala`

```scala
package scalus.compiler.intrinsics

@scalus.Compile
object BuiltinListOperations {
    import scalus.compiler.intrinsics.IntrinsicHelpers.*
    import scalus.uplc.builtin.Builtins.*

    def isEmpty[A](self: List[A]): Boolean =
        nullList(typeProxy[scalus.uplc.builtin.BuiltinList[scalus.uplc.builtin.Data]](self))

    def head[A](self: List[A]): A =
        typeProxyRetData[A](
          headList(typeProxy[scalus.uplc.builtin.BuiltinList[scalus.uplc.builtin.Data]](self))
        )

    def tail[A](self: List[A]): List[A] =
        typeProxy[List[A]](
          tailList(typeProxy[scalus.uplc.builtin.BuiltinList[scalus.uplc.builtin.Data]](self))
        )
}

@scalus.Compile
object BuiltinListOperationsV11 {
    import scalus.compiler.intrinsics.IntrinsicHelpers.*
    import scalus.uplc.builtin.Builtins.*

    def drop[A](self: List[A], n: BigInt): List[A] =
        typeProxy[List[A]](
          dropList(n, typeProxy[scalus.uplc.builtin.BuiltinList[scalus.uplc.builtin.Data]](self))
        )
}
```

### typeProxy vs typeProxyRetData

**File:** `scalus-core/shared/src/main/scala/scalus/compiler/intrinsics/Intrinsics.scala`

Two functions for different use cases:

- **`typeProxy[T](x)`** — preserves the argument's representation. Used when the underlying UPLC term already has the right shape (e.g., casting `List[A]` to `BuiltinList[Data]` when both are SumDataList).

- **`typeProxyRetData[T](x)`** — marks the result as having the **default data representation** for type `T`. Used when a builtin returns raw `Data` that needs to be interpreted as type `T` (e.g., `headList` returns `Data`, which should be unpacked as `BigInt` via `unIData`).

**Critical fix:** `typeProxyRetData` originally used hardcoded `PrimitiveRepresentation.PackedData`, which failed for non-primitive types like `TxOut` (need `ProdDataConstr`). Fixed to use `lctx.typeGenerator(app.tp).defaultDataRepresentation(app.tp)`.

---

## IntrinsicResolver

**File:** `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/IntrinsicResolver.scala`

### Hardcoded Registry

```scala
private val ListModule = "scalus.cardano.onchain.plutus.prelude.List$"
private val BuiltinListOps = "scalus.compiler.intrinsics.BuiltinListOperations$"
private val BuiltinListOpsV11 = "scalus.compiler.intrinsics.BuiltinListOperationsV11$"

private val registry: Map[String, List[(String, Int, String)]] = Map(
    ListModule -> List(
        (SumDataListRepr, 0, BuiltinListOps),      // all protocol versions
        (SumDataListRepr, 11, BuiltinListOpsV11),   // vanRossemPV (11+) only
    )
)
```

**Note on `$` suffix:** Module names include `$` because dotc's `symbol.fullName` for `object Foo` returns `"pkg.Foo$"` (module class). Both `Module.name` and `ExternalVar.moduleName` use this form.

### Resolution Flow

1. Extract module name and method name from the function SIR node
2. Look up registry entries for the target module (early return if not found)
3. Map the argument's `LoweredValueRepresentation` to a string name
4. Filter entries matching representation AND `targetProtocolVersion >= minVersion`
5. For each matching provider (highest version first), find the method binding
6. Substitute `self` with actual argument SIR (+ type variable substitution)
7. Cache the already-lowered argument in `precomputedValues`
8. Re-lower the substituted body, remove cache entry

### Type Variable Substitution

Provider bodies have unresolved type variables (e.g., `A` in `head[A]`). When substituting `self → actual_argument`, we also substitute type variables using `SIRUnify.topLevelUnifyType`:

```scala
SIRUnify.topLevelUnifyType(param.tp, arg.tp, SIRUnify.Env.empty) match
    case SIRUnify.UnificationSuccess(env, _) => env.filledTypes
    case _ => Map.empty
```

The `substituteVarAndTypes` method walks the entire SIR tree, replacing both the expression variable and all type occurrences via `SIRType.substitute`.

### Multi-Parameter Providers (drop)

`drop(self, n)` compiles as curried `LamAbs(self, LamAbs(n, body))`. The resolver substitutes only `self` (the outermost lambda), returning `LamAbs(n, body[self := arg])`. The call site's outer `Apply` handles the remaining `n` argument.

---

## Plugin: compiledModules Interception

**File:** `scalus-plugin/src/main/scala/scalus/compiler/plugin/Plugin.scala`

The `compiledModules` function in `scalus.compiler.compiler` is a stub that throws at runtime. The plugin's `ScalusPhase.transformApply` intercepts calls and replaces them with:

```scala
SIRLinker.readModules(SIRModuleWithDeps.list(
    SIRModuleWithDeps(Module1.sirModule, Module1.sirDeps),
    SIRModuleWithDeps(Module2.sirModule, Module2.sirDeps),
    ...
))
```

This generates tree-level references to `.sirModule` and `.sirDeps` (added by `SIRPreprocessor`) that resolve correctly at runtime.

---

## Lowering Integration

**File:** `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/Lowering.scala`

In `lowerNormalApp`, after lowering the argument:

```scala
if lctx.intrinsicModules.nonEmpty then
    IntrinsicResolver.tryResolve(app.f, app.arg, arg, app.tp, app.anns.pos) match
        case Some(result) => return result
        case None => // fall through to normal apply
```

The resolver caches the already-lowered argument in `lctx.precomputedValues` (keyed by SIR identity) so that re-lowering the substituted body finds it without recomputation.

---

## ForcedBuiltinsExtractor Change

Intrinsics introduce single-use forced builtins (e.g., `force(builtin nullList)` appears once per `isEmpty` call). The extractor was modified to only extract builtins used **2+ times** (threshold raised from 1), avoiding unnecessary let-binding overhead for single-use builtins.

---

## Budget Impact

| Operation | Before | After | Change |
|-----------|--------|-------|--------|
| `tail` (single element) | 405K steps | 16K steps | **-96%** |
| `diff` | 6.25M steps | 6.03M steps | **-4%** |
| CAPE accept | 27.5K mem | 27.3K mem | **-1%** |
| `v + Value.zero` | 28.97M steps | 29.01M steps | +0.2% |
| Knights 4x4 | 56.645B steps | 56.644B steps | -0.002% |

Most operations get cheaper. Small regressions (~0.3%) occur for constant empty list operations where `nullList` builtin has slightly more overhead than constant-folded match.

---

## Error Handling

| Situation | Behavior |
|-----------|----------|
| No intrinsic modules loaded | Normal compilation (no intrinsics checked) |
| Module not in registry | Normal compilation (early return) |
| Representation not matched | Normal compilation (fallback) |
| Provider module not found | Normal compilation (try next provider) |
| Method not in provider | Normal compilation (try next provider) |
| Provider binding not a LamAbs | **LoweringException** |

---

## Constraints on Provider Implementations

1. **Only use builtins and `typeProxy`/`typeProxyRetData`** — no method calls to prevent complex resolution chains
2. **Simple and direct** — emit efficient UPLC, not complex logic
3. **`self` must appear exactly once** in the body (currently not enforced, but avoids argument duplication)
4. **Match the method name** of the original extension method

---

## Future Work

### Phase 2: More Representations
- `PackedSumDataList` — needs `unListData` wrapping
- `DataConstr` — needs `unConstrData` + field extraction

### Phase 3: More Types
- `Option`, `Either`, `Map`, `PairList` intrinsics
- User-defined types with custom intrinsics

### Phase 4: Annotation-Driven Registry
- Replace hardcoded registry with `@IntrinsicProviders` annotation on target modules
- Automatic provider discovery from module annotations

### Optimization Opportunities
- Cache substitution results for same provider + same type arguments
- Pre-filter registry at context creation time based on protocol version
