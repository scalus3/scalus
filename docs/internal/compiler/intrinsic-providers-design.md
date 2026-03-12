# Intrinsic Providers Design

## Overview

This design enables representation-specific optimizations for methods like `List.isEmpty` by allowing the compiler to substitute optimized implementations based on the runtime representation.

### Problem Statement

Currently, `List.isEmpty` compiles to a match expression:

```scala
def isEmpty: Boolean = this match
    case Nil => true
    case Cons(_, _) => false
```

Which generates complex UPLC:
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

## Design

### 1. New Annotations

**File:** `scalus-core/shared/src/main/scala/scalus/compiler/internal/Intrinsics.scala`

```scala
package scalus.compiler.internal

import scala.annotation.StaticAnnotation

/** Marks a type as having representation-specific intrinsic providers.
  *
  * @param providers Map from representation name to provider object.
  *                  Provider objects contain methods with optimized implementations.
  *
  * Example:
  * {{{
  * @IntrinsicProviders(Map(
  *   "SumDataList" -> BuiltinListOperations,
  *   "DataConstr" -> DataConstrListOperations
  * ))
  * enum List[+A]:
  *   case Nil
  *   case Cons(head: A, tail: List[A])
  * }}}
 */
final class IntrinsicProviders(providers: Map[String, AnyRef]) extends StaticAnnotation

/** Compile-time type and representation cast. Zero runtime cost.
  *
  * Tells the compiler to view the value as having type T with
  * the representation appropriate for T.
  *
  * Example:
  * {{{
  * def isEmpty[A](self: List[A]): Boolean =
  *   Builtins.nullList(typeProxy[BuiltinList[Data]](self))
  * }}}
  */
inline def typeProxy[T](x: Any): T = x.asInstanceOf[T]
```

---

### 2. SIR Changes

**File:** `scalus-core/shared/src/main/scala/scalus/compiler/sir/SIR.scala`

```scala
// Bump version for structural change
val SIRVersion: (Int, Int) = (5, 0)

case class Module(
    version: (Int, Int),
    name: String,
    linked: Boolean,
    requireBackend: Option[String],
    defs: List[Binding],
    anns: AnnotationsDecl  // NEW - required field
) {
    /** Get intrinsic providers: representation name -> provider module name */
    def intrinsicProviders: Map[String, String] = {
        anns.data.collect {
            case (key, SIR.Const(Constant.String(value), _, _))
                if key.startsWith("intrinsicProviders:") =>
                key.stripPrefix("intrinsicProviders:") -> value
        }
    }
}
```

**Note:** Users must recompile all modules with the new version. No backward compatibility handling.

---

### 3. Provider Object Implementations

**File:** `scalus-core/shared/src/main/scala/scalus/prelude/ListIntrinsics.scala`

Provider objects define the same methods as the original extension methods, with `self` as the first parameter. The compiler substitutes `self` with the actual argument at the call site.

```scala
package scalus.prelude

import scalus.builtin.Builtins.*
import scalus.builtin.{BuiltinList, Data}
import scalus.compiler.internal.typeProxy

/** Intrinsic implementations for List with SumDataList/SumPairDataList representation.
  *
  * IMPORTANT: Provider methods should only use builtins and typeProxy.
  * Avoid calling other methods to prevent complex resolution chains.
  *
  * Methods must match the signature of the original extension methods in List companion.
  */
@scalus.Compile
object BuiltinListOperations {

    def isEmpty[A](self: List[A]): Boolean =
        nullList(typeProxy[BuiltinList[Data]](self))

    def head[A](self: List[A]): A =
        typeProxy[A](headList(typeProxy[BuiltinList[Data]](self)))

    def tail[A](self: List[A]): List[A] =
        typeProxy[List[A]](tailList(typeProxy[BuiltinList[Data]](self)))
}

/** Intrinsic implementations for List with PackedSumDataList representation */
@scalus.Compile
object PackedListOperations {

    def isEmpty[A](self: List[A]): Boolean =
        nullList(unListData(typeProxy[Data](self)))

    def head[A](self: List[A]): A =
        typeProxy[A](headList(unListData(typeProxy[Data](self))))

    def tail[A](self: List[A]): List[A] =
        typeProxy[List[A]](listData(tailList(unListData(typeProxy[Data](self)))))
}

/** Intrinsic implementations for List with DataConstr representation */
@scalus.Compile
object DataConstrListOperations {

    // List as DataConstr: Constr(0, []) for Nil, Constr(1, [head, ...tail]) for Cons

    def isEmpty[A](self: List[A]): Boolean = {
        val pair = unConstrData(typeProxy[Data](self))
        equalsInteger(fstPair(pair), 0)
    }

    def head[A](self: List[A]): A = {
        val pair = unConstrData(typeProxy[Data](self))
        typeProxy[A](headList(sndPair(pair)))
    }

    def tail[A](self: List[A]): List[A] = {
        val pair = unConstrData(typeProxy[Data](self))
        typeProxy[List[A]](constrData(fstPair(pair), tailList(sndPair(pair))))
    }
}
```

Note: Provider methods can use either regular method syntax or extension syntax - the compiler plugin sees both uniformly as methods of the companion object.

---

### 4. Usage on List

**File:** `scalus-core/shared/src/main/scala/scalus/prelude/List.scala`

Note: `isEmpty` is an extension method defined in the `List` companion object, not a method on the enum itself. The `@IntrinsicProviders` annotation goes on the companion object.

```scala
package scalus.prelude

import scalus.compiler.internal.IntrinsicProviders

enum List[+A]:
    case Nil
    case Cons(head: A, tail: List[A])

@Compile
@IntrinsicProviders(Map(
    "SumDataList" -> BuiltinListOperations,
    "SumPairDataList" -> BuiltinListOperations,
    "PackedSumDataList" -> PackedListOperations,
    "DataConstr" -> DataConstrListOperations
))
object List {
    // ... other methods ...

    extension [A](self: List[A]) {

        // Original implementation (fallback when no intrinsic matches)
        def isEmpty: Boolean = self match
            case Nil        => true
            case Cons(_, _) => false

        def head: A = self match
            case Cons(h, _) => h
            case Nil        => throw new NoSuchElementException("head of empty list")

        // ... rest of extension methods
    }
}
```

---

### 5. Compiler Plugin Changes

**File:** `scalus-plugin/src/main/scala/scalus/compiler/plugin/SIRCompiler.scala`

#### 5.1 New Symbols

```scala
private val intrinsicProvidersAnnot = requiredClass("scalus.compiler.internal.IntrinsicProviders")
private val typeProxyMethod = requiredMethod("scalus.compiler.internal.typeProxy")
```

#### 5.2 Compile typeProxy as Cast with Annotation

```scala
private def compileExpr2(env: Env, tree: Tree): AnnotatedSIR = tree match {
    // ... existing cases ...

    // Handle typeProxy[T](x)
    case Apply(TypeApply(fun, List(targetType)), List(arg))
        if fun.symbol == typeProxyMethod =>

        val compiledArg = compileExpr(env, arg)
        val targetSirType = sirTypeInEnv(targetType.tpe, tree.srcPos, env)
        val anns = mkAnns(tree.srcPos, env) + ("typeProxy" -> SIR.Const.bool(true))

        SIR.Cast(compiledArg, targetSirType, anns)

    // ... rest of cases ...
}
```

#### 5.3 Process @IntrinsicProviders When Creating Module

```scala
private def createModule(sym: Symbol, bindings: List[Binding]): Module = {
    val moduleName = sym.fullName.toString

    // Check for @IntrinsicProviders annotation
    val intrinsicData: Map[String, SIR] =
        sym.getAnnotation(intrinsicProvidersAnnot) match {
            case Some(annot) =>
                parseIntrinsicProviders(annot).map { case (repr, providerSym) =>
                    s"intrinsicProviders:$repr" -> SIR.Const(
                        Constant.String(providerSym.fullName.toString),
                        SIRType.String,
                        AnnotationsDecl.empty
                    )
                }
            case None => Map.empty
        }

    val anns = AnnotationsDecl(
        pos = SIRPosition.fromSrcPos(sym.srcPos),
        data = intrinsicData
    )

    Module(
        version = SIRVersion,
        name = moduleName,
        linked = false,
        requireBackend = None,
        defs = bindings,
        anns = anns
    )
}

private def parseIntrinsicProviders(annot: Annotation): Map[String, Symbol] = {
    // Parse Map("SumDataList" -> BuiltinListOperations, ...) from annotation
    // Returns Map[String, Symbol] - representation name to provider object symbol
    annot.argument(0) match {
        case Some(Apply(_, args)) =>
            args.collect {
                case Apply(_, List(Literal(Constant(repr: String)), provider)) =>
                    repr -> provider.symbol
            }.toMap
        case _ => Map.empty
    }
}
```

---

### 6. Lowering Changes

#### 6.1 Module Loading and Caching

**File:** `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/LoweringContext.scala`

```scala
class LoweringContext {
    // ... existing fields ...

    private val moduleCache: mutable.Map[String, Option[Module]] = mutable.Map.empty
    private val providersCache: mutable.Map[String, Map[String, String]] = mutable.Map.empty
    private val bindingCache: mutable.Map[(String, String), Option[Binding]] = mutable.Map.empty

    /** Load a module by name from SIRCompiled annotations on classpath */
    def loadModule(moduleName: String): Option[Module] = {
        moduleCache.getOrElseUpdate(moduleName, {
            SIRModuleLoader.load(moduleName)
        })
    }

    /** Get intrinsic providers for a module (cached) */
    def getIntrinsicProviders(module: Module): Map[String, String] = {
        providersCache.getOrElseUpdate(module.name, module.intrinsicProviders)
    }

    /** Find a binding by method name in a module (cached) */
    def findBinding(moduleName: String, methodName: String): Option[Binding] = {
        bindingCache.getOrElseUpdate((moduleName, methodName), {
            loadModule(moduleName).flatMap { module =>
                module.defs.find(_.name.endsWith(s".$methodName"))
            }
        })
    }
}
```

#### 6.2 Intrinsic Resolution

**File:** `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/IntrinsicResolver.scala`

```scala
package scalus.compiler.sir.lowering

import scalus.compiler.sir.*

object IntrinsicResolver {

    /** Try to resolve an intrinsic for the given external var and argument.
      * Returns the substituted provider body if intrinsic exists, None otherwise.
      * Throws LoweringException if provider is specified but cannot be loaded.
      */
    def resolve(
        externalVar: SIR.ExternalVar,
        arg: SIR,
        argRepr: LoweredValueRepresentation
    )(using ctx: LoweringContext): Option[SIR] = {

        // 1. Load target module (e.g., scalus.prelude.List)
        val targetModule = ctx.loadModule(externalVar.moduleName)
            .getOrElse(return None)  // No module = no intrinsics = normal path

        // 2. Get intrinsic providers from module
        val providers = ctx.getIntrinsicProviders(targetModule)
        if providers.isEmpty then return None  // No providers = normal path

        // 3. Get representation name
        val reprName = representationName(argRepr)

        // 4. Check if representation is in providers map
        providers.get(reprName) match {
            case None =>
                None  // Representation not covered = normal path (expected)

            case Some(providerModuleName) =>
                // Provider specified - MUST work or error
                val providerModule = ctx.loadModule(providerModuleName)
                    .getOrElse(throw LoweringException(
                        s"Cannot load intrinsic provider module: $providerModuleName",
                        externalVar.anns.pos
                    ))

                val methodName = externalVar.name.split('.').last

                val binding = ctx.findBinding(providerModuleName, methodName)
                    .getOrElse(throw LoweringException(
                        s"Intrinsic method '$methodName' not found in provider: $providerModuleName",
                        externalVar.anns.pos
                    ))

                // Get type arguments from the call for substitution
                val typeSubst = extractTypeSubstitution(externalVar, arg)

                // Substitute self parameter and type parameters
                val substituted = substituteParameter(binding.value, "self", arg, typeSubst)

                Some(substituted)
        }
    }

    private def representationName(repr: LoweredValueRepresentation): String = {
        repr match {
            case SumCaseClassRepresentation.SumDataList => "SumDataList"
            case SumCaseClassRepresentation.SumPairDataList => "SumPairDataList"
            case SumCaseClassRepresentation.PackedSumDataList => "PackedSumDataList"
            case SumCaseClassRepresentation.DataConstr => "DataConstr"
            case other => other.getClass.getSimpleName.stripSuffix("$")
        }
    }

    private def extractTypeSubstitution(
        externalVar: SIR.ExternalVar,
        arg: SIR
    ): Map[String, SIRType] = {
        // Extract type arguments from the call
        // e.g., List[Int].isEmpty -> A = Int
        arg.tp match {
            case SIRType.SumCaseClass(_, typeArgs) if typeArgs.nonEmpty =>
                Map("A" -> typeArgs.head)  // Assumes single type param named A
            case _ => Map.empty
        }
    }

    private def substituteParameter(
        body: SIR,
        paramName: String,
        replacement: SIR,
        typeSubst: Map[String, SIRType]
    ): SIR = {
        body.transform {
            case v: SIR.Var if v.name == paramName => replacement
            case c: SIR.Cast =>
                c.copy(tp = substituteType(c.tp, typeSubst))
            // Handle types in other nodes as needed
        }
    }

    private def substituteType(tp: SIRType, subst: Map[String, SIRType]): SIRType = {
        tp match {
            case tv @ SIRType.TypeVar(name, _, _) =>
                subst.getOrElse(name, tp)
            case SIRType.SumCaseClass(decl, typeArgs) =>
                SIRType.SumCaseClass(decl, typeArgs.map(substituteType(_, subst)))
            case SIRType.CaseClass(decl, typeArgs, parent) =>
                SIRType.CaseClass(decl, typeArgs.map(substituteType(_, subst)), parent)
            case SIRType.Fun(from, to) =>
                SIRType.Fun(substituteType(from, subst), substituteType(to, subst))
            case _ => tp
        }
    }
}
```

#### 6.3 Integration in Lowering

**File:** `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/SirLowering.scala`

```scala
def lowerApply(apply: SIR.Apply)(using ctx: LoweringContext): LoweredValue = {
    apply.f match {
        case ev: SIR.ExternalVar =>
            // Try intrinsic resolution first
            val argLowered = ctx.lower(apply.arg)

            IntrinsicResolver.resolve(ev, apply.arg, argLowered.representation) match {
                case Some(intrinsicBody) =>
                    // Lower the substituted intrinsic body
                    ctx.lower(intrinsicBody)
                case None =>
                    // Normal lowering
                    lowerApplyNormal(apply)
            }

        case _ =>
            lowerApplyNormal(apply)
    }
}
```

#### 6.4 Lowering Cast with typeProxy

```scala
def lowerCast(cast: SIR.Cast)(using ctx: LoweringContext): LoweredValue = {
    val loweredExpr = ctx.lower(cast.term)

    val isTypeProxy = cast.anns.data.get("typeProxy") match {
        case Some(SIR.Const(Constant.Bool(true), _, _)) => true
        case _ => false
    }

    if isTypeProxy then
        // Zero-cost representation view change
        val targetGenerator = ctx.typeGenerator(cast.tp)
        val targetRepr = targetGenerator.defaultRepresentation(cast.tp)

        // Just change type view, underlying term stays the same
        TypeViewLoweredValue(
            underlying = loweredExpr,
            sirType = cast.tp,
            representation = targetRepr,
            pos = cast.anns.pos
        )
    else
        // Normal cast lowering
        lowerCastNormal(cast, loweredExpr)
}
```

#### 6.5 TypeViewLoweredValue

**File:** `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/LoweredValue.scala`

```scala
/** LoweredValue that changes type/representation view without conversion */
class TypeViewLoweredValue(
    underlying: LoweredValue,
    override val sirType: SIRType,
    override val representation: LoweredValueRepresentation,
    override val pos: SIRPosition
) extends LoweredValue {

    override def termInternal(gctx: TermGenerationContext): Term =
        underlying.termInternal(gctx)

    override def show: String =
        s"TypeView(${underlying.show} as ${sirType.show})"
}
```

---

## Complete Flow Example

### Input Scala

```scala
val list: List[Int] = Cons(1, Nil)
val empty = list.isEmpty
```

### After Compiler Plugin (SIR)

```scala
// List module:
Module(
    version = (5, 0),
    name = "scalus.prelude.List",
    linked = false,
    requireBackend = None,
    defs = [...],
    anns = AnnotationsDecl(
        pos = ...,
        data = Map(
            "intrinsicProviders:SumDataList" -> Const(String("scalus.prelude.BuiltinListOperations")),
            "intrinsicProviders:DataConstr" -> Const(String("scalus.prelude.DataConstrListOperations"))
        )
    )
)

// Usage compiles to:
Apply(
    ExternalVar("scalus.prelude.List", "scalus.prelude.List.isEmpty", ...),
    Var("list", List[Int]),
    Boolean,
    ...
)

// BuiltinListOperations.isEmpty compiles to:
Binding(
    name = "scalus.prelude.BuiltinListOperations.isEmpty",
    tp = ...,
    value = Apply(
        ExternalVar("scalus.builtin.Builtins", "nullList", ...),
        Cast(
            Var("self", List[A]),
            BuiltinList[Data],
            anns = {"typeProxy" -> Const(Bool(true))}
        ),
        Boolean
    )
)
```

### During Lowering (SumDataList representation)

```
1. See: Apply(ExternalVar("List.isEmpty"), listVar)

2. Load List module

3. Get intrinsicProviders: {
     "SumDataList" -> "scalus.prelude.BuiltinListOperations",
     "DataConstr" -> "scalus.prelude.DataConstrListOperations"
   }

4. Current representation is SumDataList

5. Look up provider: "scalus.prelude.BuiltinListOperations"

6. Load BuiltinListOperations module, find isEmpty binding

7. Substitute self -> listVar:
   Apply(
     nullList,
     Cast(listVar, BuiltinList[Data], {typeProxy: true})
   )

8. Lower Cast with typeProxy:
   - Target type: BuiltinList[Data]
   - Current repr: SumDataList (compatible)
   - Result: TypeViewLoweredValue (zero cost, same underlying term)

9. Lower Apply(nullList, ...):
   - Compile to builtin application

10. Result UPLC: (nullList listVar)
```

### Without Intrinsics (fallback)

If representation not in providers map:

```
Match(listVar, [Nil -> true, Cons -> false])
  ↓
(force (chooseList listVar (delay True) (delay False)))
```

---

## Error Handling

| Situation | Behavior |
|-----------|----------|
| No `@IntrinsicProviders` on module | Normal compilation |
| Has providers but repr not in map | Normal compilation |
| Repr in map, provider module not found | **LoweringException** |
| Repr in map, method not in provider | **LoweringException** |

---

## Constraints on Provider Implementations

Provider methods should:

1. **Only use builtins and `typeProxy`** - avoid calling other methods to prevent complex resolution chains
2. **Be simple and direct** - the goal is to emit efficient UPLC, not complex logic
3. **Match the signature** of the original method (same parameters, same return type)

---

## Summary of Changes

| File | Change |
|------|--------|
| `SIR.scala` | Add `anns` field to `Module`, bump version to (5,0) |
| `SIR.scala` | Add `intrinsicProviders` helper method on `Module` |
| `Intrinsics.scala` (new) | Define `@IntrinsicProviders` and `typeProxy` |
| `ListIntrinsics.scala` (new) | Provider implementations |
| `List.scala` | Add `@IntrinsicProviders` annotation |
| `SIRCompiler.scala` | Compile `typeProxy` as Cast with annotation |
| `SIRCompiler.scala` | Process `@IntrinsicProviders` when creating Module |
| `IntrinsicResolver.scala` (new) | Resolution logic |
| `SirLowering.scala` | Check intrinsics in `lowerApply` |
| `SirLowering.scala` | Handle `typeProxy` Cast in `lowerCast` |
| `LoweredValue.scala` | Add `TypeViewLoweredValue` |
| `LoweringContext.scala` | Add module loading, caching, binding lookup |

---

## Performance Considerations

Caching is implemented for:
- Module loading (`moduleCache`)
- Intrinsic providers parsing (`providersCache`)
- Binding lookup (`bindingCache`)

Substitution happens per call site (different arguments), so cannot be cached.

---

## Future Extensions

This design can be extended to:

1. **Other types** - `Option`, `Either`, `Map`, etc.
2. **More methods** - any method that has representation-specific optimal implementations
3. **User-defined types** - users can add `@IntrinsicProviders` to their own types
