# UPLC JIT refactoring plan

**Superseded** by `docs/superpowers/specs/2026-10-19-uplc-jit-simplification-design.md`
(branch `worktree-jit-simplify`): one backend (mincont and hybrid are deleted), CEK values and
builtins, `UplcCompiler` with a CEK fallback (`withFallback`). Section 2.5 (comments in generators) still applies; the rest is history.
The current design is summarized in `scalus-uplc-jit-compiler/docs/DESIGN.md`.

Status: implemented on branch `worktree-jit-refactor-plan` (2026-10-18). Base: master
`92cb5634a` (2026-10-18), which includes the `optimize-jit-lambda-inlining` session:

| Commit | Change |
|---|---|
| `c786bee01` | applied builtins bind each argument once; budget equals CEK |
| `6e3520f2a` | mincont generates subterms with the owner of their splice |
| `184c04646` | `StagingCompilerThread`: one compile thread, compiler replaced every 50 000 nodes |
| `a87d311da` | nativestack hoists constants into a per-script `consts` array |
| `92cb5634a` | nativestack inlines let-bindings `[(lam x b) a]` while a method has < 20 terms |

Goal: remove duplication in `scalus-uplc-jit-compiler` without losing eval speed or compile speed.
Every step must keep the JIT tests of steps 1 and 1b green and must not slow vm-bench or
`JitEvalProbe`.

**Scope: builtin semantics variant E only** (PlutusV3 and later, at protocol version 11 and
later). Variant D (PlutusV1/V2 at PV11) is excluded too: it differs from E (`ConsByteString`
wraps instead of throwing, `Builtin.scala:237-241`). With one variant the JIT needs no variant
parameter, its API stays `(Logger, BudgetSpender, MachineParams) => Any`, every hand-written
builtin is simply variant-E code, and the variant-E conformance corpus checks all of them. Callers
evaluate other scripts with the CEK. `JitRunner` documents the scope.

## 1. Findings

### 1.1 Builtins are written 3 times

| File | Lines | Per-builtin content |
|---|---|---|
| `BuiltinSnippets.scala` | 880 | curried form: cost + semantics |
| `BuiltinAppliedGenerator.scala` | 1320 | staged applied form: the same cost + semantics, plus Apply/Builtin step spends |
| `BuiltinEmitter.scala` | 340 | 3 `match` tables + `isSupported1/2` |

183 of the 206 `spendBudget(` calls in the module are in the first two files.

Defects found while reading:

- The JIT cannot compile 47 of the 101 builtins: `emitBuiltin` throws "not yet supported" for all
  BLS12-381 (17), all Value builtins (`LookupCoin`, `InsertCoin`, `UnionValue`, `ValueContains`,
  `ValueData`, `UnValueData`, `ScaleValue`), the bitwise builtins, `IntegerToByteString`,
  `ByteStringToInteger`, `ExpModInteger`, `Keccak_256`, `Blake2b_224`, `Ripemd_160`, `DropList`
  and the array builtins. Any script that uses one of them, for example through `getLovelace`
  (which compiles to `lookupCoin` at PV11), cannot be JIT-compiled.
- `MkCons` (in `isSupported2`) and `HeadList`, `TailList`, `NullList` (in `isSupported1`) are
  polymorphic. Valid UPLC applies them as `[(force (builtin f)) x]`, so
  `UplcTermHelper.isApplyBuiltin1/2` never matches them. These entries are dead.
- The JIT costs `appendString`, `equalsString` and `encodeUtf8` with `MemoryUsage.memoryUsageString`
  (UTF-16 length). Variant E uses `StringByByteLengthCostingFun` (UTF-8 byte length,
  `Builtin.scala:73,400-421`), so non-ASCII strings cost differently. Not yet tested.
- `BuiltinSnippets.constrData` uses `tag.longValue`; check against `Builtins.constrData` range rules.

### 1.2 Constants are converted 4 times

| Converter | Used by | Note |
|---|---|---|
| `ConstantGenerator.constantToExpr` | nobody | `MlResult` -> `sys.error` |
| `nativestack/JIT.scala` private copy + `ToExpr[Data]`, `ToExpr[ByteString]` | nativestack | now feeds the `consts` array |
| `mincont/JIT.scala` private copy (identical) | mincont | no hoisting: constants rebuilt on every evaluation |
| `RuntimeHelper.uplcToJitAny` (runtime) | hybrid | throws on `Unit`, `Array`, BLS, `BuiltinValue` |

`a87d311da` moved hoisted constants out of the evaluation function, but the `consts` initializer is
still staged code built by `constantToExpr`. A large constant (an applied script context) still
becomes one big JVM method: compile time goes up, and the 64 KB method limit is still reachable.

### 1.3 The two backends drift

| Feature | nativestack | mincont |
|---|---|---|
| constant hoisting | yes | no |
| let inlining | yes | no |
| `genCode` parameters | 7 (`term, env, logger, budget, params, nativeStackContext, consts`) | 5 |
| per-term overhead | none | `termDesc` string built for every term, used only when `debugMode` (hard-coded `false`) |

Other duplication and dead code:

- `enum FunType` is declared in both `JIT.scala` files and never used.
- Three hand-written `Term` walks: `nodeCount` (`StagingCompilerThread`), `collectConstants`
  (nativestack), `UplcTermHelper`. Each lists the term cases again (out of scope, section 5).
- The step-spend pattern `$budget.spendBudget(Step(StepKind.X), $params.machineCosts.xCost, Nil)`
  appears 22 times in the two `JIT.scala` files.
- `env.find(_._1 == name.name)` is a linear search per `Var` at compile time.
- `src/main/scala/scalus/uplc/eval/mincont/JIT.scala.bak` is stale and calls the old API.
- `JitVsCekCheck` is a `main` tool over a directory of `.flat` scripts, not a test, so "keep it
  green" cannot be checked automatically.

## 2. Target design

### 2.1 Every builtin compiles: hand-written fast path or CEK bridge

There is no "unsupported" builtin and no `isSupported`. Each builtin has one of two
implementations, chosen when the code is generated.

**A. `BuiltinImpls` (about 50 builtins): the hot builtins (integers, bytestrings, `Data`) and all
15 polymorphic builtins.** Polymorphic arguments can be JIT closures or `ConstrValue`s, which
have no `Constant` form, so these cannot use the bridge: `IfThenElse`, `ChooseUnit`, `Trace`,
`FstPair`, `SndPair`, `ChooseList`, `MkCons`, `HeadList`, `TailList`, `NullList`, `ChooseData`,
and the 4 new ones `DropList`, `LengthOfArray`, `ListToArray`, `IndexArray`.

Plain Scala, uncurried, typed, with the same first 3 parameters for all. Semantics and costing are
variant E; the conformance suite (step 1b) checks every method.

```scala
object BuiltinImpls {
    import MemoryUsage.memoryUsageInteger

    private inline def spend2(
        b: BudgetSpender, fun: DefaultFun, cf: DefaultCostingFun[TwoArguments],
        mx: CostingInteger, my: CostingInteger
    ): Unit = b.spendBudget(BuiltinApp(fun), cf.calculateCostFromMemory(Seq(mx, my)), Nil)

    private inline def spendConst(b: BudgetSpender, fun: DefaultFun, cf: ConstCostingFun): Unit =
        b.spendBudget(BuiltinApp(fun), cf.constantCost, Nil)

    def addInteger(l: Logger, b: BudgetSpender, p: MachineParams, x: BigInt, y: BigInt): BigInt =
        spend2(b, AddInteger, p.builtinCostModel.addInteger, memoryUsageInteger(x), memoryUsageInteger(y))
        x + y

    def headList(l: Logger, b: BudgetSpender, p: MachineParams, xs: List[Any]): Any =
        spendConst(b, HeadList, p.builtinCostModel.headList)
        xs.head

    def trace(l: Logger, b: BudgetSpender, p: MachineParams, s: String, a: Any): Any =
        spendConst(b, Trace, p.builtinCostModel.trace)
        l.log(s)
        a
}
```

Hand-written methods call the typed `MemoryUsage.memoryUsageInteger`, `memoryUsageByteString`,
`memoryUsageData`, ... directly, so `MemoryUsageJit` (a runtime type `match` per argument) is
deleted. It needs no `Tuple2` or `ConstrValue` case: the bridge costs with the CEK memory usage,
list, pair and polymorphic builtins have constant cost, and a `ConstrValue` reaches a builtin only
in an ill-typed script, where the generated cast fails as the CEK fails.

The string builtins (`appendString`, `equalsString`, `encodeUtf8`) go to the bridge: it applies
the variant-E byte-length costing of finding 1.1 with no JIT code.

**B. `CekBridge` (all other monomorphic builtins, no per-builtin code).** The `TypeScheme` gives
the `DefaultUni` of each argument, so JIT values convert exactly. Pairs are `Tuple2` on both sides
(section 2.2), so no pair conversion is needed.

```scala
object CekBridge {
    /** The variant-E runtimes of all builtins, indexed by `DefaultFun.ordinal`. */
    final class Runtimes(val params: MachineParams, val byOrdinal: Array[BuiltinRuntime])

    /** The last runtimes used (fast path: a reference compare), then a map by cost model, so
      * alternating `MachineParams` do not rebuild the about 100 runtimes on every call.
      */
    def builtinsFor(params: MachineParams): Runtimes = ...

    /** The argument types of each builtin, from its type scheme, indexed by ordinal. */
    private val argTypes: Array[Array[DefaultUni]] = ...

    def call(builtins: Runtimes, ordinal: Int, l: Logger, b: BudgetSpender, args: Array[Any]): Any = {
        val rt = builtins.byOrdinal(ordinal)
        val cek = ArraySeq.tabulate(args.length)(i =>
            CekValue.VCon(fromValue(argTypes(ordinal)(i), args(i))))
        b.spendBudget(BuiltinApp(DefaultFun.fromOrdinal(ordinal)), rt.costFunction.calculateCost(cek*), Nil)
        rt.f(l, cek) match
            case CekValue.VCon(c) => toValue(c)
            case other            => throw JitEvaluationFailure(s"$fun returned $other")
    }
}
```

`fromValue`/`toValue` are `Constant.fromValue`/`Constant.toValue` plus the BLS12-381 ML result,
which both reject: no script can write an ML result constant, but `bls12_381_millerLoop` returns
one and `bls12_381_mulMlResult` and `bls12_381_finalVerify` take it.

The generated call looks the runtimes up at the call site, `CekBridge.call(CekBridge.builtinsFor(params),
28, logger, budget, Array[Any](a1, a2))`: usually a volatile read and a reference compare (the
last runtimes used); on a miss a map keyed by cost model, so evaluations that alternate between
two `MachineParams` do not rebuild the runtimes on every call. A prologue
`val builtins` (the first design) is captured by every lambda of the script, and each captured
variable adds names to the constant pool of the generated class; large mincont scripts are near
its 65 535-entry limit (step 8). The argument types are a static table in `CekBridge`, computed
once from the type schemes, so the generated call passes only the ordinal.

The bridge is correct by construction: CEK semantics and CEK costing functions, including
`StringByByteLengthCostingFun`, `ValueMaxDepthCostingFun` and `DataNodeCountCostingFun`. A new
monomorphic Plutus builtin works in the JIT with no JIT code. It also serves as the reference in
tests for the hand-written methods. Cost: one `CekValue` + `Constant` wrapper per argument and
result; BLS, hashes and Value ops are expensive anyway. If a bridged builtin is hot, move it to
`BuiltinImpls`.

**Metadata and dispatch.**

```scala
private def implName(bn: DefaultFun): String =
    val s = bn.toString
    s.head.toLower +: s.tail                           // Sha2_256 -> sha2_256, as in SIRBuiltins

// true = term argument, false = force; every builtin lists its forces first
private def argKinds(bn: DefaultFun): List[Boolean] =
    Meaning.allBuiltins.getBuiltinRuntime(bn).argKinds.toList   // private[uplc]: visible here

private val implemented: Set[String] = BuiltinImpls.getClass.getMethods.map(_.getName).toSet
private def hasImpl(bn: DefaultFun): Boolean = implemented.contains(implName(bn))

private def callBuiltin(bn: DefaultFun, args: List[Expr[Any]])(using GenCtx, Quotes): Expr[Any] =
    if hasImpl(bn) then callImpl(bn, args) else callBridge(bn, args)
```

A misspelled method name fails silently: `hasImpl` is false and the builtin goes to the bridge,
which is slower for a hot builtin and breaks for a polymorphic one (`fromValue` of a closure). A
test guards the mapping: all 15 polymorphic builtins have a method; every public `BuiltinImpls`
method maps to a `DefaultFun`; each method has 3 + (number of term arguments) parameters.

`callImpl` builds the static call by name with the reflection API of the staging `Quotes`; the
casts come from the method type:

```scala
private def callImpl(bn: DefaultFun, args: List[Expr[Any]])(using ctx: GenCtx, q: Quotes): Expr[Any] = {
    import q.reflect.*
    val module = Symbol.requiredModule("scalus.uplc.jit.BuiltinImpls")
    val method = module.methodMember(implName(bn)).head
    val MethodType(_, paramTypes, _) = Ref(module).tpe.memberType(method): @unchecked
    val castArgs = args.zip(paramTypes.drop(3)).map { (arg, tpe) =>
        tpe.asType match
            case '[t] => '{ $arg.asInstanceOf[t] }.asTerm
    }
    val fixed = List(ctx.logger.asTerm, ctx.budget.asTerm, ctx.params.asTerm)
    Apply(Select(Ref(module), method), fixed ++ castArgs).asExprOf[Any]
}
```

Sketch; check the exact reflection API in step 6. Fallback if name lookup is fragile in the
staging class loader: one hand-written line per `BuiltinImpls` method,
`case AddInteger => '{ BuiltinImpls.addInteger($l, $b, $p, ${a(0)}.asInstanceOf[BigInt], ...) }`.

**Unapplied form (step 6).** Replaces `BuiltinSnippets`. The first design generated the curried
closure, one lambda per force and term argument, for every occurrence:

```scala
def curry(kinds: List[Boolean], bound: List[Expr[Any]])(using Quotes): Expr[Any] = kinds match
    case false :: rest => '{ () => ${ curry(rest, bound) } }              // a force
    case true :: rest  => '{ (a: Any) => ${ curry(rest, bound :+ 'a) } }  // a term argument
    case Nil           => callBuiltin(bn, bound)
```

It put too many lambdas into the generated class: mincont failed on stablecoin_1-1 with "Class
too large" (step 8). Now precompiled code builds the closures (`JitCurry`) around one uncurried
call lambda per hand-written builtin and script, which sits in a slot of the constant array
(`ConstantPool.initCalls` fills it once per compiled script); a bridged builtin needs no lambda:

```scala
// (builtin addInteger), its call lambda in slot 7:
JitBudget.spendBuiltin(budget, params)
JitCurry.curry(consts(7).asInstanceOf[JitCurry.Call], 0, logger, budget, params)
// (builtin appendString):
JitBudget.spendBuiltin(budget, params)
JitCurry.bridged(CekBridge.builtinsFor(params), 28, logger, budget)
```

**Applied form `[(force^k (builtin f)) a1 .. an]`, for every builtin (step 7).** Replaces
`isApplyBuiltin1/2WithSimpleArg(s)` and adds the forced builtins (`headList`, `ifThenElse`, ...):

```scala
object AppliedBuiltin {
    def unapply(t: Term): Option[(DefaultFun, Int, List[Term])] = {
        def apps(t: Term, args: List[Term]): (Term, List[Term]) = t match
            case Term.Apply(f, a, _) => apps(f, a :: args)
            case other               => (other, args)
        def forces(t: Term, n: Int): (Term, Int) = t match
            case Term.Force(b, _) => forces(b, n + 1)
            case other            => (other, n)
        val (head, args) = apps(t, Nil)
        forces(head, 0) match
            case (Term.Builtin(bn, _), k)
                if argKinds(bn) == List.fill(k)(false) ++ List.fill(args.size)(true)
                    && args.forall(UplcTermHelper.isSimpleTerm) =>
                Some((bn, k, args))
            case _ => None
    }
}

// args are generators, not finished Exprs: each runs inside the splice of its `val` (rule 7)
def emitApplied(bn: DefaultFun, forces: Int, args: List[Quotes ?=> Expr[Any]])(using ctx: GenCtx, q: Quotes): Expr[Any] = {
    // bind each argument once (keeps the c786bee01 fix)
    def bind(rest: List[Quotes ?=> Expr[Any]], bound: List[Expr[Any]])(using Quotes): Expr[Any] = rest match
        case a :: tail => '{ val v = ${ a }; ${ bind(tail, bound :+ 'v) } }
        case Nil       => callBuiltin(bn, bound)
    '{
        // CEK order: the Apply, Force and Builtin steps come before the argument steps;
        // one static call spends them all
        JitBudget.spendSaturated(${ ctx.budget }, ${ ctx.params }, ${ Expr(args.size) }, ${ Expr(forces) })
        ${ bind(args, Nil) }
    }
}
```

Generated code for `[(builtin addInteger) x (con integer 1)]`:

```scala
JitBudget.spendSaturated(budget, params, 2, 0)    // Apply, Apply, Builtin
val v1 = { JitBudget.spendVar(budget, params); x }
val v2 = { JitBudget.spendConst(budget, params); consts(3) }
BuiltinImpls.addInteger(logger, budget, params, v1.asInstanceOf[BigInt], v2.asInstanceOf[BigInt])
```

Step 8: nativestack takes any arguments, not only simple ones: it computes each in place, in
order, which is the CEK order too. Only mincont needs simple arguments (`SimpleArgs`), because any
other argument is a continuation. Without this, `[(force ifThenElse) [lessThanInteger a b] t e]`
(a computed condition, the common case) took the unapplied path through `JitCurry`, and auction_1-1
evaluated 2x slower.

This is the CEK order: for `[[f a1] a2]` it spends Apply, Apply, (Force), Builtin, then the
argument steps, then the builtin cost. The arguments are simple terms, which cannot fail, so moving
their steps after the others changes nothing but the order, and a `RestrictingBudgetSpender` now
stops at the same point as the CEK. Today the applied code spends `applyCost |+| applyCost` in
one call and the argument steps first; the totals are the same.

Size: `BuiltinAppliedGenerator` (1320 lines) and `BuiltinSnippets` (880) are deleted;
`BuiltinEmitter` drops from 340 to about 150 lines with `CekBridge`; `BuiltinImpls` is about 2
lines per hand-written builtin. A new builtin needs 0 lines (bridge) or 1 method (fast path).

Rejected: a compile-time macro that generates the dispatch `match` (it would have to emit staging
quotes, for no gain over reflection), and an sbt source generator from `SIRBuiltins`/`TypeScheme`
(it can generate signatures only; the bodies stay hand-written).

### 2.2 One constant converter, one constant pool

- **Pairs are `Tuple2`; `Constr` values get their own class (JIT only).** `Constant.toValue`
  already returns a `Tuple2` for a pair, and `Data.Map` entries are `(Data, Data)`. The JIT now
  uses `BuiltinPair` for pairs and `Tuple2` for `Constr` values (nativestack
  `Expr.ofTuple(tag -> args)`, mincont `Return((tag, args))`, `CaseHelper.scala:81`
  `case (tag: Long, args: List[Any])`). Swap them:

  ```scala
  final case class ConstrValue(tag: Long, args: List[Any])   // was (tag, args)

  // RuntimeHelper / builtin bodies
  def unMapData(d: Data): List[(Data, Data)] = d match
      case Data.Map(values) => values.toScalaList           // was .map(BuiltinPair.apply)
  def mapData(x: List[(Data, Data)]): Data = Data.Map(PList.from(x)) // was .map(p => (p.fst, p.snd))
  // fstPair / sndPair: x._1 / x._2; mkPairData: (a, b); unConstrData: (tag, args)
  ```

  Gains: no core change (no public behavior change, no CHANGELOG line); `unMapData` and `mapData`
  stop allocating one object per map entry; `CaseHelper` tests a class instead of a typed tuple
  pattern; a `Constr` value can no longer be mistaken for a pair. Files: `CaseHelper` (both
  dispatchers), `Constr` in both backends, `RuntimeHelper.unConstrData/unMapData`, and the
  `mkPairData`, `mapData`, `unMapData`, `fstPair`, `sndPair` bodies in the old builtin files
  (6 one-line edits that step 6/7 later replaces).

  **Do this together with the pool, never after it:** the pool converts with `toValue`, which
  makes pair constants `Tuple2`, while the builtins still expect `BuiltinPair` until this switch.

  Rejected alternative: switch `toValue`/`fromValue`/`flatForUni` to `BuiltinPair`. It changes 3
  public core methods and keeps the per-entry copies; its only reuse gain is `Builtins.fstPair`.
- **One converter (JIT).** `uplcToJitAny` becomes `Constant.toValue`. This also closes the hybrid
  gap: `toValue` covers `Unit`, `Array`, BLS and `BuiltinValue`. Arrays become the `IndexedSeq`
  of the constant, not `Vector`. No JIT builtin reads arrays today; future array builtins take
  `IndexedSeq`.
- Delete `RuntimeHelper.anyUplcConstant`. It has no callers, and it is wrong: `case i: Int` misses
  the `BigInt` integers of the JIT, and every list gets element type `Data`. `Constant.fromValue`
  cannot replace it, because it needs the `DefaultUni` type, which JIT values do not carry (for
  example the element type of an empty list).
- Compute `Array[AnyRef]` at JIT compile time with `uplcToJitAny`. Generate
  `Array[AnyRef] => (Logger, BudgetSpender, MachineParams) => Any` and apply it to the array after
  `staging.run`. The constants then never become bytecode. `jitUplc` keeps its signature.
- Keep `isHoisted`: `Unit`, `Bool`, `String` stay literals (`Expr(value)` directly).
- The two private `constantToExpr` copies and their `ToExpr[Data]`, `ToExpr[ByteString]` are
  deleted (`ConstantGenerator` already goes in step 2).
- Use the same pool in mincont (step 5).

### 2.3 Shared code generation context

```scala
final case class GenCtx(
    logger: Expr[Logger],
    budget: Expr[BudgetSpender],
    params: Expr[MachineParams],
    consts: Expr[Array[AnyRef]],        // step 5
    pool: ConstantPool                  // step 8: which constant or call lambda is in which slot
)
def spendStep(kind: StepKind)(using ctx: GenCtx, q: Quotes): Expr[Unit]   // JitBudget.spendVar(budget, params)
```

Every step spend is one static call of a plain-Scala `JitBudget` method, for example
`JitBudget.spendVar(budget, params)`, never an inlined
`budget.spendBudget(Step(StepKind.Var), params.machineCosts.varCost, Nil)`: the inlined form was
about 64 % of the generated bytecode and allocated a `Step` per spend. `JitBudget` holds one
preallocated `Step` per kind. A let-binding spends `JitBudget.spendLet` (Apply, LamAbs), a saturated
builtin `JitBudget.spendSaturated(budget, params, nApply, nForce)` (Apply x n, Force x k, Builtin, in
CEK order). The budgets stay equal to the CEK's, per category.

- `genCode(term, env)(using GenCtx, Quotes)` in both backends. nativestack adds its
  `NativeStackContext` in a subclass or a second given.
- `env` becomes `Map[String, Expr[Any]]`, or de Bruijn indices from `DeBruijnedProgram`.
- Step 3 introduces `GenCtx` with the first 3 fields; later steps add fields without touching the
  `genCode` signatures again. (Step 4 gives only nativestack the pool, so `consts` sits in its
  `NativeCtx` until step 5 gives mincont the pool too.) No field may add a variable that generated
  lambdas capture (see the bridge note in 2.1).

### 2.4 Plutus conformance suite on the JIT

Run the whole `plutus-conformance` corpus (999 cases: 724 with a term and a budget, 220
`evaluation failure`, 55 `parse error`) through nativestack and mincont, with the same variant-E
machine parameters as `PlutusConformanceJvmTest`. The corpus is variant E, the scope of the JIT.
Hybrid is not run: it is nativestack with a fallback for deep stacks, and corpus cases are shallow.

Reuse, do not copy: `scalusUplcJitCompiler` already depends on `scalus.jvm % "test->test"`, so a
subclass of `PlutusConformanceTest` gets case discovery (`Macros.conformanceEvaluationCasesImpl`),
`readFile`, `path`, `expectationOf`, `ignoredCases` and `plutusVM`. It overrides `check`, which is
already `protected`.

```scala
// scalus-uplc-jit-compiler/src/test/scala/scalus/uplc/jit/JitConformanceTest.scala
abstract class JitConformanceTest(jit: JITImplementation) extends PlutusConformanceTest {

    /** Corpus directories this backend fails today, with the reason. Keys are path prefixes
      * relative to `uplc/evaluation` (for example `builtin/semantics/bls12_381_G1_add`), so one
      * entry covers all cases of one builtin. A listed case that passes fails the suite, so the
      * list only shrinks (step 6 empties the "unsupported builtin" part).
      */
    protected def knownJitFailures: Map[String, String]

    private def knownFailure(name: String): Option[String] =
        knownJitFailures.collectFirst { case (prefix, reason) if name.startsWith(prefix) => reason }

    override protected def check(name: String): Unit = test(name) {
        val code = readFile(s"$path/$name.uplc")
        val outcome = Program.parseUplc(code) match
            case Left(_)        => Outcome.ParseError
            case Right(program) => run(program.term)
        val ok = matches(outcome, name)
        knownFailure(name) match
            case Some(reason) => assert(!ok, s"$name passes now; remove it ($reason)")
            case None         => assert(ok, s"$name: $outcome")
    }

    private def run(term: Term): Outcome = {
        // A compile failure is never an "evaluation failure": an unsupported builtin must not
        // pass the 220 failure cases by accident.
        val fn =
            try jit.compile(term)
            catch case NonFatal(e) => return Outcome.CompileError(e)
        val spender = TallyingBudgetSpender(CountingBudgetSpender())
        try Outcome.Success(fn(NoLogger, spender, plutusVM.machineParams), spender.getSpentBudget)
        catch case NonFatal(e) => Outcome.EvaluationFailure(e)
    }
}

class NativeStackConformanceTest extends JitConformanceTest(JITImplementation.NativeStack) { ... }
class MincontConformanceTest     extends JitConformanceTest(JITImplementation.Mincont) { ... }
```

The base class registers the tests in its constructor (`discoveredCases.foreach(check)`), so
`check` runs before the subclass body. Everything it reads at registration time must be a `def`
or a constructor parameter; `knownJitFailures` is read only inside the test body.

Comparing a result. The JIT returns a JVM value, not a term, so the expected term is turned into
the JIT representation and compared by value:

- `(con c)` -> `Constant.toValue(c)`; `(constr i a*)` -> `ConstrValue(i, a*.map(...))`. Equal
  values and an equal budget pass. BLS elements compare by their compressed bytes.
- Any other expected term (`lam`, `delay`, a partly applied builtin; about 170 cases): the JIT
  result must be a function (`Any => Any` or `() => Any`; a mincont `delay` is
  `Return(() => body)`, `ContinuationJitRepr.scala:27`), and the budget must be equal. The term
  itself cannot be compared; count these cases in the test output.
- `evaluation failure`: the evaluation, not the compile, must throw.
- `parse error`: the parser must reject the program (same parser as the CEK suite).

Until step 4 the JIT uses `BuiltinPair` for pairs and `Tuple2` for `Constr` values, so the
comparison maps through one `normalize` function; step 4 deletes it.

Expected first result: the 47 unsupported builtins fail to compile (one prefix entry per builtin,
reason "unsupported builtin, step 6"), and string costing may differ (finding 1.1). Each known
failure is a finding to fix or explain.

Cost: about 1000 staging compiles per backend. Measure the run time; if it is minutes, run
nativestack in `jvm/test` and mincont only on demand.

### 2.5 Comments in code generators and macros

Staged code is hard to read: one Scala method runs at JIT-compile time and writes code that runs
later, at evaluation time, and the two are interleaved in quotes and splices. Every generator,
splice-heavy helper and macro in this plan is commented to these rules:

1. **Show the generated code.** The doc comment of each generator shows, as a Scala block, what it
   emits for one concrete UPLC input. Example: `emitApplied` shows the code for
   `[(builtin addInteger) x (con integer 1)]`.
2. **Mark the stage.** Say which lines run at JIT-compile time (inspecting the `Term`, choosing an
   implementation, reflection lookups) and which run at evaluation time (inside `'{ ... }`). A
   short `// compile time:` / `// generated:` prefix is enough where a block mixes both.
3. **Explain why, with the CEK reference.** Each budget spend says which CEK step it matches and
   why it sits where it is: for example "Apply step, as `Cek.scala` `computeCek` for `Apply`;
   spent before the argument steps, as the CEK spends it before it computes the arguments".
4. **Explain owners and splices.** Where the order of splices or the `Quotes` passed in matters
   (the owner rule of `6e3520f2a`, binding arguments once from `c786bee01`), say what breaks if it
   changes: "spliced twice, the argument would run and spend its step twice".
5. **Explain reflection.** For `callImpl`, say what tree is built, how the method is found
   (`implName`), where the casts come from, and what fails if a name does not match.
6. **Name the invariants.** For example "forces precede term arguments in every `TypeScheme`",
   "a `ConstrValue` never reaches a builtin in a well-typed script", "variant E only". If code
   depends on one, say so at the place that depends on it.

7. **Owner rule.** Every generator takes the `Quotes` of the splice it is called from and is called
   inside that `${ }`. Never build a child `Expr` first and then splice the finished `Expr` under a
   `val`, a lambda or a deeper quote: staging re-owns (copies with a `TreeTypeMap`) the whole
   subtree at each nesting level, which is quadratic in the nesting depth. Code for subterms is
   passed as generators (`Quotes ?=> Expr[Any]`), as `CaseHelper.genCaseDispatch` does, and
   spliced as `${ gen }`. References (`'logger`, `'budget`, `'v`, `consts`) can be passed as
   finished `Expr`s: re-owning an `Ident` costs nothing. Fixing this in `2eb9f3d61` cut the
   nativestack compile of the 89 plutus_use_cases scripts from 4-197 s to 1.4-8.3 s (auction_1-1:
   20 s -> 1.9 s; 57 % of CPU was `TreeTypeMap`), and in `6e3520f2a` the mincont compile of
   stablecoin_2-3 from 407 s to 12 s.

Example of the expected density:

```scala
/** Generates a saturated builtin call: `[(force^k (builtin f)) a1 .. an]`.
  *
  * For `[(builtin addInteger) x (con integer 1)]` it generates:
  * {{{
  * JitBudget.spendSaturated(budget, params, 2, 0)        // Apply, Apply, Builtin
  * val v1 = { JitBudget.spendVar(budget, params); x }            // a1, runs once
  * val v2 = { JitBudget.spendConst(budget, params); consts(3) }  // a2, runs once
  * BuiltinImpls.addInteger(logger, budget, params, v1.asInstanceOf[BigInt], v2.asInstanceOf[BigInt])
  * }}}
  *
  * The steps are spent in CEK order: the CEK spends Apply, Force and Builtin while it walks down
  * to the builtin, and only then computes the arguments. Each argument is bound to a `val` before
  * use: its code spends its own step, and an implementation reads it twice (cost and result), so
  * splicing it directly would spend twice.
  */
```

Review rule: a generator without a "generated code" example is not done.

## 3. Steps

Each step is one commit. Each keeps the tests of steps 1 and 1b green, and every generator or
macro it adds or changes is commented as section 2.5 requires.

| # | Step | Risk | Estimate |
|---|---|---|---|
| 1 | JIT-shape differential test, JIT vs CEK result and `ExUnits`, nativestack + mincont: applied, unapplied, forced and let-inlined builtins, a large constant, `Constr`/`Case`. Only what the corpus (1b) does not cover. Reuse the comparison of `JitVsCekCheck` and `JitBudgetTest`/`JitBudgetTally`, not a new harness | none | 1 h |
| 1b | Plutus conformance suite on the JIT (section 2.4): `JitConformanceTest`, nativestack + mincont; prefix-keyed known-failure list that can only shrink | none | 3 h |
| 2 | Dead code: `FunType` x2, `JIT.scala.bak`, `ConstantGenerator`, `anyUplcConstant`, `termDesc`/`debugMode` in mincont | none | 30 min |
| 3 | `GenCtx` + `spendStep` (one `JitBudget` call per site); apply to both backends | low | 2 h |
| 4 | Constants, one commit: pairs as `Tuple2` + `ConstrValue` class (JIT only); `uplcToJitAny` = `Constant.toValue`; runtime pool for nativestack; delete the `ToExpr` givens and private copies | low | 3 h |
| 5 | Constant pool in mincont | low | 1 h |
| 6 | Unapplied form: `BuiltinImpls` for hot + polymorphic builtins (4 new: `DropList`, array builtins); `CekBridge` with `builtinsFor` and `GenCtx.builtins`; `emitBuiltin`; the name-mapping test; delete `BuiltinSnippets` | medium | 4 h |
| 7 | Applied form: `AppliedBuiltin` + `emitApplied` for all 101 builtins, in CEK step order; delete `BuiltinAppliedGenerator`, `MemoryUsageJit`, all `isSupported*` | medium | 4 h |
| 8 | Measure: forced builtins on the applied path (new in step 7), bridge cost on BLS/Value scripts, re-tune `MaxInlinedLetTerms`; move hot bridged builtins to `BuiltinImpls` | low | 2 h |

Do 1 and 1b first: they decide whether the string-cost mismatch is real, and they guard every
later step. Every later step re-runs 1b and shrinks its known-failure list or keeps it equal.

Dependencies: step 4 edits the pair bodies in the old builtin files, which steps 6 and 7 then
replace; the pair switch and the pool must stay in one commit (section 2.2). `JitVsCekCheck` is
not a gate; run it on the vm-bench script set before step 1 and after step 8.

## 4. Measurements per step

- `JitEvalProbe <script.flat>`: steady-state eval, JIT vs CEK. Steps 4, 5, 6, 7, 8.
- `JitCodegenProbe`: compile time and live compiler objects. Steps 4, 6, 7.
- vm-bench subset (see the vm-bench memory note): before step 1 and after step 8.

Known trade-offs to watch:

- Step 4: a pool read instead of an inlined literal. Small `BigInt` literals may be faster inline;
  measure before hoisting them.
- Step 6: the unapplied form moves from a precompiled snippet to staged lambdas around a static
  call; check compile time.
- Step 7: a static call replaces inlined `x + y`. HotSpot should inline it; the smaller staged
  code should cut compile time. `MaxInlinedLetTerms = 20` was tuned with the current builtin code
  size, so re-check it after step 7.

## 5. Out of scope

- Variants A-D: PlutusV1/V2 at any protocol version, and PlutusV3 before PV11. Those scripts are
  evaluated with the CEK.
- Let inlining in mincont. It is a performance feature, not de-duplication, and it is hard there:
  a non-simple argument produces a continuation, so `val x = gen(arg)` does not apply.
- Merging nativestack and mincont into one backend with a pluggable return representation.
  Possible after step 3, but the `Return` wrapping and continuation trampoline differ in every case.
- A shared term walk (`Term.foreachSubterm`) for `nodeCount`, `collectConstants` and the
  shape checks in `UplcTermHelper`. A separate refactoring, not part of this plan.
