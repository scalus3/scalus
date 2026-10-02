# Statement capture

How the statement syntax of [the verification overview](../verification-overview.md) (§3.2, §4)
becomes a runtime `Prop`. Code: `scalus-verification/src/main/scala/scalus/verify/PropMacro.scala`,
`Prop.scala` (`Props`) and `FunctionMacro.scala`. Tests: `PropTest.scala`. What a captured
statement means is in [statement semantics](prop-semantics.md).

## Two front ends

- **Macros (implemented).** A statement written in test code is ordinary Scala, which the plugin
  never compiles as a whole. The macros behind `Props` split it into its skeleton and its leaves,
  and send each leaf to `compile`. The sections up to "Function references" describe them.
- **SIR reification (proposed).** A statement or specification that the plugin compiles anyway,
  inside a `@Compile` object or one `compile { … }` block, can be read from its SIR, with the
  combinators as Boolean pseudo-functions of a marker object.
  See [statements in SIR](#statements-in-sir-boolean-pseudo-functions-proposed). It is how
  specifications written in a function's body reach the verifier, together with
  [function tables](#function-tables-from-compile-objects-proposed) read from `@Compile` objects.

Both produce the same `Prop`.

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

the macros of `Prop(...)` and `call(...)` expand first, while `x` and `r` are still parameters of
the enclosing lambdas; `call`'s body `r == x` becomes its one test. `forAll`'s macro expands last,
and it sees a lambda whose body is already code that builds a `Prop`. This order decides the
design:

- a leaf is expanded first, sees free references to parameters of enclosing lambdas, and closes
  over them;
- a binder is expanded after its body, checks that the body no longer uses its parameter, and
  keeps the body.

## Leaves

A leaf is an expression of the statement that becomes SIR.

| Syntax | Leaf |
|---|---|
| a binder's body of type `Boolean` | the whole body, one test |
| `Prop(b)`, or a `Boolean` operand of a connective (`booleanToProp`) | the test `b` |
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
is in scope only in the continuation. A body has the type `Prop | Boolean`. `PropMacro.binders`
does three things:

1. It removes `Inlined` wrappers without bindings, type ascriptions and empty blocks, and requires
   a lambda literal with the expected number of parameters.
2. It builds one `PropExpr.Ident` per parameter, with its name, an id and its SIR type.
3. It turns the body into the binder's statement, by the body's type:
   - **`Boolean`**: the whole body is one test, compiled as a leaf. It may use the binder's
     parameters and the enclosing binders' variables anywhere, so an `if`, a `match` or a local
     `val` needs no wrapper.
   - **`Prop`**: the body, once the inner macros have expanded, builds the rest of the statement.
     The macro reports a compile error at every remaining use of a parameter (next section), and
     stops there.
   - **Both**, an `if` or `match` with a statement in one branch and a Boolean in another: a
     compile error.

A Boolean operand of a connective, as in `(lo <= hi) ==> call(...)`, becomes a test through the
implicit conversion `booleanToProp`. `Prop(b)` is only needed to make a Boolean a separate test
on purpose: `!Prop(t)` holds when `t` fails, while a Boolean body `!t` is one test that fails with
it.

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

- **A binder variable used outside a leaf of a statement body.** Once the inner macros have
  expanded, a statement body should not use its parameter. A use that remains is at the `Prop`
  level, as in `forAll[Boolean](flag => if flag then p else q)` with statements `p` and `q`. There
  the shape of the statement depends on the variable's value, which is outside the fragment
  (overview §4.1). The error message suggests stating the cases with `==>`, as
  `(flag ==> p) && (!flag ==> q)`, and points out that an `if` over Boolean tests is one test.
- **A body that is a statement in one branch and a Boolean in another.** Its type is
  `Prop | Boolean`, and it is neither one test nor a statement.
- **A binder's lambda inside an inline expansion with bindings.** When an `inline def` with
  parameters that are not `inline` wraps a statement, its expansion is an `Inlined` node that
  binds those parameters as local values, and the lambda's body may use them. A statement cannot
  keep them, so this is a compile error, not a statement that is silently wrong.
- **More than three parameters in one `forAll`.** Nest `forAll`s instead.
- **A statement inside an `@Compile` object, class or trait.** The plugin compiles that code, and
  would meet the Scala code the macros build `Prop` values with. Until statements are read from
  SIR ([below](#statements-in-sir-boolean-pseudo-functions-proposed)), the macros reject them
  there, and point to stating them outside compiled code. Only enclosing classes are checked:
  `@Compile` sits on objects, classes and traits, and a statement inside a
  `compile { … }` block is not detected.

## Contracts

`Props.contract(f)(expects, ensures)` and `Props.totalContract` (`PropMacro.contract`) take two
lambda literals, `expects = (x, lo, hi) => …` and `ensures = (x, lo, hi) => r => …`, and build

```
∀ x lo hi. expects ==> whenReturns(f, (x, lo, hi))(r => ensures)
```

with a total `call` for `totalContract`. The quantified variables are the parameters of
`expects`, whose types the overload fixed. The call is built as `call` builds one, with the
argument `(x, lo, hi)` made from references to them and `ensures`' inner lambda `r => …` as its
continuation. The bodies of both lambdas become statements as a binder's body does: a `Boolean` is
one test, and a statement must use the parameters only in its leaves.

The two lambdas declare their parameters separately, so `ensures`' `lo` is another symbol than
`expects`' `lo`, with another name. `ensures`' leaves were compiled before the contract macro
expands, because expansion goes from the inside out, and they are already closed over its own
names. So the macro cannot rename them in the tree. It emits `Props.renameVariables` instead, which
renames `ensures`' variables after those of `expects` in the built statement at runtime, in its
SIR and its binders.

`contract.returnsWhen(args => c)` and `contract.failsWhen(args => c)` add a guarantee. The
clause is built as the standalone statement `∀ args. c ==> succeeds(f, args)`, or `fails`, with
its own variables. `Contract.withClause` then renames them after the contract's and puts the
clause under the contract's precondition, at runtime, for the same reason as above.

The macro returns a `Contract`: the statement, with the function and totality it already knows.
`Verifier.contract(name, contract)` records `Origin.Contract(f, total)` from it, so a statement
that is not a contract cannot be declared as one; `Verifier.contracts(f)` lists them.

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

## Statements in SIR: Boolean pseudo-functions (proposed)

The macros are needed only because test code is never compiled by the plugin as a whole. Code
the plugin does compile can carry a statement in its SIR, with the combinators as pseudo-functions
of a marker object. `UniversalDataConversion` already works this way: an `@Compile` object whose
methods throw if called; a call to one compiles to an `ExternalVar` application in SIR, and the
linker and the lowering recognise it by name.

```scala
@Compile
object Logic {   // statements; each method throws if called
    def forAll[A](body: A => Boolean): Boolean
    def exists[A](body: A => Boolean): Boolean
    def implies(premise: Boolean, conclusion: Boolean): Boolean
    def denotes[A](value: A): Boolean
    def holds(test: Boolean): Boolean                     // a test kept separate, as Prop(t)
    def whenReturns[R](value: R)(body: R => Boolean): Boolean
}

@Compile
object Spec {    // specifications in a function's body, prop-semantics.md §7
    def expects(condition: Boolean): Unit
    extension [A](body: A) def ensuring(condition: A => Boolean): A
}
```

`forAll[BigInt](x => Math.abs(x) >= 0)`, compiled by the plugin, is

```
Apply(ExternalVar(Logic$.forAll), LamAbs(x: Integer, ≥(Apply(ExternalVar(Math$.abs), x), 0)))
```

The skeleton is Boolean-typed SIR, so `Prop` needs no SIR type. A reifier walks the SIR and builds
the `Prop` the macros build:

- `Logic.forAll(λx. b)` is `Forall(x, reify(b))`, with the binder's `SIRType` from the lambda's
  parameter; `exists` likewise. SIR keeps Scala's parameter names, which nested lambdas can
  repeat, so the reifier renames binders apart.
- `implies`, `denotes` and `holds` are `Implies`, `Denotes` and a separate `Bool` test.
  `whenReturns` is a partial call.
- `&&`, `||`, `!` and `if` are statement connectives when an operand contains a `Logic` call. An
  `if` becomes `(c ∧ p) ∨ (¬c ∧ q)`, with `¬c` inside the test, so a failing condition makes both
  branches false.
- `let y = e in p` around a statement is `existsLet(e)(y => p)`. The macros reject this form
  (overview §4.1).
- Any other subterm is a test: a maximal subterm without `Logic` calls, whose free variables are
  the enclosing binders. It is `PropExpr.SIRExpr` as it is, with no closed lambda to open and no
  runtime renaming.
- A call of a function in the function table is an application inside a test, which the tactic
  links as it does now. Target syntax, `f(x)` for a known `f`, needs nothing more.

**What changes, compared with the macros.**

- **No tricks around inlining order.** Leaves are no longer compiled as closed lambdas, so
  `openVariables` and the contract macro's `renameVariables` go away.
- **Statements can live in `@Compile` objects.** That is where in-body specifications are.
- **Types no longer separate tests from statements.** In SIR everything is `Boolean`, so the rule
  above decides, as [prop-semantics.md §4](prop-semantics.md#4-connectives) requires. A `Logic`
  call where a value is expected, as in a function's argument, is reported by the reifier or by
  the lowering; scalac cannot catch it. `@compileTimeOnly` would catch a `Logic` call left in JVM
  code, but the methods of an `@Compile` object are JVM code too. So it fits only statements
  written inside `compile { … }`, which the plugin replaces.
- **Lowering.** A statement is never lowered as a whole, only its tests, which contain no `Logic`
  calls, so the lowering rejects `Logic` calls with a clear error. `Spec` clauses sit in code that
  is lowered, so the linker or the lowering drops them first, and the bytes do not change. This
  is the one core change, of the same kind as `UniversalDataConversion`'s special cases. The
  earlier branch made its `spec.requires` an `inline` no-op instead, and the inliner erased it before
  the plugin ever saw it, so nothing could read it.

**Why the overview's §4.3 found this route expensive.** It assumed that the combinators had to
be plugin intrinsics and that `Prop` needed a SIR type. Pseudo-functions need neither: the plugin
compiles calls to them as calls to any `@Compile` method, and the skeleton stays `Boolean`.

**Rejected: a SIR type for `Prop`**, with its constructors in SIR. `Prop` holds SIR terms and
Scala values, so it cannot be an `@Compile` type; it would need its own encoding and constructor
intrinsics, for nothing that Boolean pseudo-functions do not give.

**Recommendation.**

1. Build the reifier for specifications in `@Compile` code, and for statements in `@Compile`
   objects.
2. Keep the macros for statements in test code until the reifier exists.
3. Then let a `statement { … }` entry compile its block with `compileInline` and reify it. That
   retires the closed-lambda and renaming machinery, and leaves one front end.

## Function tables from `@Compile` objects (proposed)

A verifier needs a `FunctionDef` per function, with its SIR, UPLC and signature, and the function's
in-body specification. Today each is registered by hand, `FunctionDef(Math.clamp)`, which compiles
one function with `PlutusV3.compile` where it is written.

**What already exists.**

- The plugin stores each `@Compile` object's SIR in the object: `sirModule: Module` and
  `sirDeps: List[SIRModuleWithDeps]`.
- A `Module` is a list of `Binding(name, tp, value)`. The names are the fully qualified names
  calls use, and the types are `SIRType`s, so a binding's arity is the number of its `Fun`
  layers. Bindings carry no annotations of their own.
- `compiledModules("scalus.….Math")` is a plugin intrinsic that returns such modules at compile
  time. `sirModule` is also reachable by reflection, as `SecondaryParamListCaseClassTest` does.
- An `inline def` has no binding: it is expanded where it is called.
- scalus-core cannot refer to `FunctionDef`, which lives in scalus-verification.

**Options.**

1. **The plugin generates a function table** next to `sirModule`. Being in scalus-core, it could
   only emit core types: names, types, SIR, the specification's SIR. `sirModule` already holds
   those, so this option reduces to the next one, plus redundant generated code.
2. **Read `sirModule` at runtime**, with no plugin change. `FunctionTable.fromModule(module, deps)`
   makes one `FunctionDef` per binding:
   - its SIR, and an arity from its type;
   - its UPLC program and signature, compiled on first use: link `ExternalVar(binding)` against
     the module and its dependencies with `SIRLinker`, then lower with `UplcBlaster.options`;
   - its specification, from the binding's SIR: the leading `Spec.expects` calls and the
     trailing `ensuring`, reified as above.

   It needs runtime access to `sirDeps`, by reflection or by an intrinsic like `compiledModules`
   that also returns dependencies, and an entry point for linking at runtime.
3. **A macro over the object's type** that lists its non-inline methods and expands to
   `FunctionTable(FunctionDef(Math.clamp), FunctionDef(Math.gcd), …)`. It needs no plugin change
   and no runtime linking, because each entry is compiled as today. It reads no specifications,
   and keeps the limit of three parameters.

**Recommendation: option 2.** It has one source of truth, the SIR the plugin already stores. It
covers every binding, generic ones included, and the in-body specifications. Option 3 is a cheap
interim step if function tables are needed before the reifier exists.
