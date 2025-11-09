---
sidebar_position: 3
---
# Advanced usage

## Direct access to ScriptContext fields

Converting full `ScriptContext` to Scott-encoded lambdas is not always necessary. Often you just need to access a few fields. In this case, you can use the `fieldAsData` macro to extract a field from a class represented as a `Data` object.

```scala mdoc:compile-only
import scalus.*, Compiler.*, builtin.{Data, Builtins, ByteString}, Builtins.*, ByteString.*
import scalus.ledger.api.v3.*

val sir = compile:
    def validator(ctxData: Data) =
        // this generates headList(...headList(sndPair(unConstrData(ctxData)))) code
        // to extract the signatories field from the ScriptContext
        val signatories = fieldAsData[ScriptContext](_.txInfo.signatories)(ctxData)
        // or like this, which is equivalent
        val signatories2 = ctxData.field[ScriptContext](_.txInfo.signatories)
        val sigs = unListData(signatories)
        // or like this, which is equivalent
        val sigs2 = signatories2.toList
        unBData(sigs.head) == hex"deadbeef"
        // same as above
        sigs2.head.toByteString == hex"deadbeef"
```

## Inlining constants

Scalus can inline constants in the script.

```scala mdoc:compile-only
import scalus.*, Compiler.*, builtin.{Data, Builtins, ByteString}, Builtins.*, ByteString.*

inline def validator(inline pubKeyHash: ByteString)(datum: Data, redeemer: Data, ctxData: Data) =
    verifyEd25519Signature(pubKeyHash, datum.toByteString, redeemer.toByteString)
val script = compile:
    validator(hex"deadbeef")
```

generates the following `SIR`:

```ocaml
{λ datum redeemer ctxData ->
  verifyEd25519Signature(#deadbeef, unBData(datum), unBData(redeemer))
}
```

## Conditional code generation using macros

```scala mdoc:compile-only
import scalus.*, Compiler.*, builtin.Data, builtin.Builtins
// the `dbg` macro will generate `trace` calls only if the `debug` flag is set to `true`
inline def dbg[A](msg: String)(a: A)(using debug: Boolean): A =
    inline if debug then Builtins.trace(msg)(a) else a

inline def validator(using debug: Boolean)(datum: Data, redeemer: Data, ctxData: Data) =
    dbg("datum")(datum)

val releaseScript = compile(validator(using false))
// {λ datum redeemer ctxData -> datum }
val debugScript = compile(validator(using true))
// {λ datum redeemer ctxData -> trace("datum", datum) }
```

Here, the `releaseScript` will not contain any `trace` calls, while the `debugScript` will contain them.

## Pretty-printing Scalus Intermediate Representation (SIR)

Scalus Intermediate Representation (SIR) is a low-level representation of the script that is used by the Scalus compiler.
It's a good idea to print the SIR to understand what the compiler has generated.

```scala mdoc:compile-only
import scalus.*, scalus.compiler.sir.SIR
val sir: SIR = ??? // from the previous example
println(sir.show) // pretty-print the SIR
println(sir.showHighlighted) // pretty-print the SIR with colorized syntax highlighting
```

## SIR optimizations

Scalus Intermediate Representation (SIR) can be optimized. Currently,
the only optimization is the `RemoveRecursivity` optimization that inlines `let` expressions.

```scala mdoc:compile-only
import scalus.*, scalus.compiler.sir.*
val sir: SIR = ??? // from the previous example
val optimized = BooleanOptimizer.optimize(sir)
// or using the `|>` operator
val optimized2 = sir |> BooleanOptimizer.optimize
```

## UPLC optimizations

Scalus can also optimize the UPLC representation of the script. The `EtaReduce` optimization removes unnecessary lambdas.

```scala mdoc:compile-only
import scalus.uplc.Term, scalus.uplc.transform.*
val uplc: Term = ???
val opt = EtaReduce(uplc)
```
