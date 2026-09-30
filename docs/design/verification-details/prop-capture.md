# Statement capture

How the statement syntax of [the verification overview](../verification-overview.md) (§3.2, §4)
becomes a runtime `Prop`. Code: `scalus-verification/src/main/scala/scalus/verify/PropMacro.scala`,
`Prop.scala` (`Props`) and `FunctionMacro.scala`. Tests: `PropTest.scala`.

## Why a macro

The Scalus plugin compiles `compile(...)` calls to SIR in its own phase, which runs after
`inlining`, where macros expand:

```
typer → posttyper → [ScalusPrepare] → pickler → inlining (macros) → firstTransform → [Scalus] → patternMatcher
```

A macro never sees SIR. The plugin never sees a statement's logical structure, because after
inlining that structure is ordinary code that builds `Prop` values. So capture splits the work.
The macros behind `Props` build the skeleton (`Prop.Forall`, `Prop.Call`, …) as ordinary Scala.
For each leaf they emit a `compile(...)` call, and the plugin compiles it like a hand-written
one. The plugin needs no changes.

## Inlining order

The compiler inlines from the inside out: dotty's `Inlining` phase transforms a call's arguments
before it inlines the call. In

```scala
forAll[BigInt](x => Prop(x > 0) ==> call(Math.abs, x)(r => r == x))
```

the macros of `Prop(...)`, `call(...)` and the Boolean `r == x` expand first, while `x` and `r`
are still parameters of the enclosing lambdas. `forAll`'s macro expands last, and it sees a
lambda whose body is already code that builds a `Prop`. This order decides the design:

- a leaf is expanded first, sees free references to parameters of enclosing lambdas, and closes
  over them;
- a binder is expanded after its body, checks that the body no longer uses its parameter, and
  keeps the body.

## Leaves

A leaf is an expression of the statement that becomes SIR.

| Syntax | Leaf |
|---|---|
| `Prop(b)`, or a `Boolean` where a `Prop` is expected (`booleanToProp`) | the test `b` |
| `denotes(e)` | `e` |
| `equal(a, b)` | `a` and `b` |
| `call(f, arg)(…)`, `whenReturns(f, arg)(…)` | `arg` |
| `existsLet(w)(…)` | the witness `w` |

`PropMacro.leaf` collects the binder variables the expression uses: identifiers of parameters of
anonymous functions that the expression does not define itself, in order of first use.

- With none, the leaf is `compile(e)`.
- Otherwise the macro builds the lambda `(v1, …, vn) => e` and emits `compile` of that lambda.
  The plugin cannot compile an expression with free variables, so a leaf is never compiled open.

At runtime `Props.openVariables` takes the compiled lambda apart:

1. It removes the `n` parameters.
2. It renames their occurrences in the body to the names of the statement variables they stand
   for (`SIR.renameFreeVars`).
3. The compiler may have put definitions around the lambda: a `Let` of module definitions when
   the body uses `@Compile` methods, and `Decl`s for data types. These move inside, around the
   body.

References to `@Compile` methods stay `ExternalVar`s. A backend resolves them from its function
table; `UplcBlaster` links them ([uplc-blaster.md](uplc-blaster.md#linking)).

## Binders

The binders are `forAll` (one to three parameters, outermost first), `exists`, `existsLet`, and
the result of `call` or `whenReturns`. A call's argument uses the enclosing scope; its result
is in scope only in the continuation. `PropMacro.binders` does four things:

1. It removes `Inlined` wrappers without bindings, type ascriptions and empty blocks, and requires
   a lambda literal with the expected number of parameters.
2. It reports a compile error at every remaining use of a parameter in the body (next section).
3. It builds one `PropExpr.Ident` per parameter, with its name, an id and its SIR type.
4. It keeps the body, which, once the inner macros have expanded, builds the rest of the `Prop`.

The SIR type comes from compiling the identity lambda `(value: T) => value` and reading the type
of its parameter (`Props.variableType`), so the plugin decides the type exactly as it would for
any other code. `Quantifiable[T]` restricts which types can be bound, and carries no data.

## Names

A leaf and its binder see the same symbol, so both derive the variable's name from it: the
parameter's name and the source offset of its declaration, as in `x_1234`
(`PropMacro.variableName`). Two binders called `x` in one statement therefore get different
names. The `id` of an `Ident` packs the declaration's start and end offsets.

These names stay the same while the source file is unchanged, but they change when code above the
statement moves. Nothing stores them yet; a proof cache keyed on a statement's content (overview
§5.4) will have to rename the variables to canonical names first.

## What is rejected, and why

- **A binder variable used outside a leaf.** Once the inner macros have expanded, a binder's body
  should not use its parameter. A use that remains is at the `Prop` level, as in
  `forAll[Boolean](flag => if flag then p else q)`. There the shape of the statement depends on
  the variable's value, which is outside the fragment (overview §4.1). The error message
  suggests `Prop(if flag then a else b)` for a Boolean conditional.
- **A binder's lambda inside an inline expansion with bindings.** When an `inline def` with
  parameters that are not `inline` wraps a statement, its expansion is an `Inlined` node that
  binds those parameters as local values, and the lambda's body may use them. A statement cannot
  keep them, so this is a compile error, not a statement that is silently wrong.
- **More than three parameters in one `forAll`.** Nest `forAll`s instead.

## Function references

`FunctionRef(Math.clamp)` and `FunctionDef(Math.clamp)` take the function's name from the method
behind an eta-expanded reference (`FunctionMacro.qualifiedName`). The name is the method's
`fullName`, which is also the name SIR gives it: `…prelude.Math$.clamp`.

The macro accepts only a lambda whose body calls one method of a `@Compile` object with the
lambda's own parameters, in order. Any other lambda is a different function, and naming it after
the method it calls would misstate what a statement is about. For any other function, including
an `inline def`, `FunctionDef.named("abs", (x: BigInt) => Math.abs(x))` gives a synthetic name
without dots.

There are overloads for one to three parameters. A call to a function of several parameters
passes them as a tuple written out in the call, as in `(x, lo, hi)`. `FunctionDef.apply` and
`FunctionDef.named` compile the function with `UplcBlaster.options`, and keep its SIR and UPLC
program as representations.
