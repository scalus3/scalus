# T7 Phase 1: Value CIP-153 Builtin Lowering Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** At PV11, lower `plutus.v1.Value` operations (`quantityOf`, `plus`, `minus`, `multiply`, `negate`, new `containsAtLeast`) to the CIP-153 builtins via a new `ValueIntrinsicsV11` intrinsic module, gated on a new `Options.valueBuiltins` flag (default true).

**Architecture:** A `@Compile` provider object registered in `IntrinsicResolver.registry` with `minPV = 11` (the `BuiltinListOperationsV11` pattern). Provider bodies use the free `Value <-> Data` relabel (`typeProxy`) around `unValueData`/builtin/`valueData`. PV10 or flag-off silently falls back to the linked SIR body because either the registry entry is version-filtered or the provider module is absent from `intrinsicModules`.

**Tech Stack:** Scala 3.3, sbt (`sbtn`), ScalaTest + ScalaCheck, Scalus compiler plugin (`compile {}` blocks in tests), MiMa.

**Spec:** `docs/superpowers/specs/2026-08-18-t7-value-builtins-lowering-design.md`

## Global Constraints

- PV10 output must stay byte-identical: any test lowering with `targetProtocolVersion = MajorProtocolVersion.plominPV` must produce no CIP-153 builtin.
- Default `Options()` has `targetProtocolVersion = vanRossemPV`, so the intrinsics fire by default once landed.
- Test files that use `compile {}` must NOT declare a class-level `given Options` with a non-default backend (the plugin resolves `Options` at the compile call site at scalac time and bakes the linker decision; see memory note / ValueBuiltinsBudgetTest scaladoc). Pass Options explicitly at `toUplc` instead: `sir.toUplc(using opts)()`.
- Commit style: conventional commits, no Claude co-author trailer, run `sbtn scalafmtAll` before each commit.
- Commits go directly on `master` (project convention); rebase before push if the user committed in parallel.
- MiMa: intentional binary breaks need a reviewed `mimaBinaryIssueFilters` entry with a comment in `build.sbt` (policy comment at `build.sbt:18-19`).

---

### Task 1: `Value.containsAtLeast` portable API

**Files:**
- Modify: `scalus-core/shared/src/main/scala/scalus/cardano/onchain/plutus/v1/Value.scala` (inside the `extension (v: Value)` block, near `quantityOf` at :767)
- Test: `scalus-core/shared/src/test/scala/scalus/ledger/api/v1/ValueTest.scala`

**Interfaces:**
- Produces: `extension (v: Value) def containsAtLeast(other: Value): Boolean` compiled as `Value$.containsAtLeast` with SIR arity 2 (self, other). Task 3's intrinsic must match this name and arity.
- Semantics (must mirror the CIP-153 `valueContains` builtin, `BuiltinValueOps.scala:137-154`): throws (via `prelude.require`) when EITHER value contains a negative amount; otherwise true iff for every (policy, token, amount) in `other`, `v.quantityOf(policy, token) >= amount`.

- [ ] **Step 1: Write the failing tests**

Add to `ValueTest.scala` (match the file's existing test style; adapt `List`/`ByteString` construction helpers to what the file already imports):

```scala
test("containsAtLeast: superset with larger amounts contains subset") {
    val a = Value.unsafeFromList(
      List(
        (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000)))),
        (hex"aa", List((hex"01", BigInt(5)), (hex"02", BigInt(7))))
      )
    )
    val b = Value.unsafeFromList(
      List((hex"aa", List((hex"01", BigInt(5)))))
    )
    assert(a.containsAtLeast(b))
    assert(!b.containsAtLeast(a))
    assert(a.containsAtLeast(a))
    assert(a.containsAtLeast(Value.zero))
    assert(Value.zero.containsAtLeast(Value.zero))
}

test("containsAtLeast: missing token or smaller amount is not contained") {
    val a = Value.unsafeFromList(List((hex"aa", List((hex"01", BigInt(5))))))
    val more = Value.unsafeFromList(List((hex"aa", List((hex"01", BigInt(6))))))
    val other = Value.unsafeFromList(List((hex"bb", List((hex"01", BigInt(1))))))
    assert(!a.containsAtLeast(more))
    assert(!a.containsAtLeast(other))
}

test("containsAtLeast: negative amounts on either side throw") {
    val neg = Value.unsafeFromList(List((hex"aa", List((hex"01", BigInt(-1))))))
    val pos = Value.unsafeFromList(List((hex"aa", List((hex"01", BigInt(1))))))
    assertThrows[Exception](pos.containsAtLeast(neg))
    assertThrows[Exception](neg.containsAtLeast(pos))
}
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `sbtn "scalusJVM/testOnly scalus.ledger.api.v1.ValueTest"`
Expected: FAIL to compile with `value containsAtLeast is not a member` (compile error counts as the failing state here).

- [ ] **Step 3: Implement the extension**

Add inside the `extension (v: Value)` block in `Value.scala`, with scaladoc:

```scala
        /** Tests whether this `Value` contains at least the amounts in `other`.
          *
          * For every (policy id, token name, amount) entry in `other`, this `Value` must hold at
          * least that amount. Fails (throws) when either value contains a negative amount -
          * mirroring the CIP-153 `valueContains` builtin exactly, so the result is identical on
          * every protocol version.
          *
          * At PV11 (vanRossem) this method lowers to the `valueContains` builtin, which requires
          * both values to be in canonical form (strictly ascending keys, no zero amounts, no
          * empty inner maps, keys at most 32 bytes, amounts within +-(2^127)); a non-canonical
          * value makes the script fail. See `Options.valueBuiltins`.
          *
          * @example
          *   {{{
          *   val a = Value.lovelace(BigInt(1000))
          *   val b = Value.lovelace(BigInt(400))
          *   a.containsAtLeast(b) === true
          *   b.containsAtLeast(a) === false
          *   }}}
          */
        def containsAtLeast(other: Value): Boolean = {
            prelude.require(
              v.toSortedMap.forall { kv => kv._2.forall { tv => tv._2 >= BigInt(0) } },
              "containsAtLeast: negative amount in this value"
            )
            prelude.require(
              other.toSortedMap.forall { kv => kv._2.forall { tv => tv._2 >= BigInt(0) } },
              "containsAtLeast: negative amount in other value"
            )
            other.toSortedMap.forall { kv =>
                kv._2.forall { tv => v.quantityOf(kv._1, tv._1) >= tv._2 }
            }
        }
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `sbtn "scalusJVM/testOnly scalus.ledger.api.v1.ValueTest"`
Expected: PASS (all tests in the file, including the pre-existing ones).

- [ ] **Step 5: Format and commit**

```bash
sbtn scalafmtAll
git add scalus-core/shared/src/main/scala/scalus/cardano/onchain/plutus/v1/Value.scala scalus-core/shared/src/test/scala/scalus/ledger/api/v1/ValueTest.scala
git commit -m "feat(prelude): add Value.containsAtLeast with valueContains semantics"
```

---

### Task 2: `ValueIntrinsicsV11` with `quantityOf` (end-to-end de-risking)

**Files:**
- Create: `scalus-core/shared/src/main/scala/scalus/compiler/intrinsics/ValueIntrinsics.scala`
- Modify: `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/IntrinsicResolver.scala` (module constants near :29-33, `defaultIntrinsicModules` :68-92, imports :134, `registry` :155-190)
- Test: `scalus-core/jvm/src/test/scala/scalus/compiler/sir/lowering/ValueIntrinsicsLoweringTest.scala` (create)

**Interfaces:**
- Consumes: `Builtins.lookupCoin(cs: ByteString, tn: ByteString, v: BuiltinValue): BigInt`, `Builtins.unValueData(d: Data): BuiltinValue`, `IntrinsicHelpers.typeProxy[V](x: Any): V`, `ReprRule`/`ArgReprConvertRule` type aliases (`ListIntrinsics.scala:12-19`).
- Produces: `@Compile object ValueIntrinsicsV11` (module key `scalus.compiler.intrinsics.ValueIntrinsicsV11$`), `object ValueReprRules { val rules: Map[String, ReprRule] }`. Task 3 extends both; Task 4 gates the module's presence.

- [ ] **Step 1: Write the failing lowering test**

Create `ValueIntrinsicsLoweringTest.scala`. NO class-level `given Options` (see Global Constraints):

```scala
package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.cardano.onchain.plutus
import scalus.cardano.onchain.plutus.v1.Value
import scalus.compiler.{compile, Options}
import scalus.uplc.Term
import scalus.uplc.Term.*
import scalus.uplc.builtin.Data.fromData
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.eval.{PlutusVM, Result}

class ValueIntrinsicsLoweringTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    private val pv10 = Options(targetProtocolVersion = MajorProtocolVersion.plominPV)

    /** Canonical 2-policy value as Data. */
    private def valueData: Data = {
        def entry(p: String, amount: Int): (Data, Data) =
            (
              Data.B(ByteString.fromHex(p * 28)),
              Data.Map(
                plutus.prelude.List((Data.B(ByteString.fromString("tok")), Data.I(amount)))
              )
            )
        Data.Map(plutus.prelude.List(entry("aa", 5), entry("bb", 7)))
    }
    private val policyBB = ByteString.fromHex("bb" * 28)
    private val tok = ByteString.fromString("tok")

    private val quantityOfSir = compile { (d: Data, cs: ByteString, tn: ByteString) =>
        fromData[Value](d).quantityOf(cs, tn)
    }

    private def hasBuiltin(t: Term, name: String): Boolean =
        t.show.contains(s"(builtin $name)")

    private def evalInt(t: Term): BigInt = t.evaluateDebug match
        case Result.Success(Term.Const(c, _), _, _, _) =>
            c.asInstanceOf[scalus.uplc.Constant.Integer].value
        case other => fail(s"evaluation failed: $other")

    test("quantityOf lowers to lookupCoin at PV11 and evaluates correctly") {
        val uplc = quantityOfSir.toUplc()
        assert(hasBuiltin(uplc, "lookupCoin"))
        assert(evalInt(uplc $ valueData.asTerm $ policyBB.asTerm $ tok.asTerm) == BigInt(7))
    }

    test("quantityOf keeps the portable lowering at PV10") {
        val uplc = quantityOfSir.toUplc(using pv10)()
        assert(!hasBuiltin(uplc, "lookupCoin"))
        assert(!hasBuiltin(uplc, "unValueData"))
        assert(evalInt(uplc $ valueData.asTerm $ policyBB.asTerm $ tok.asTerm) == BigInt(7))
    }
}
```

Note: if `Constant.Integer` unapply differs, follow `BuiltinValueCompileTest.evalToInteger` (`scalus-core/shared/src/test/scala/scalus/builtin/BuiltinValueCompileTest.scala:458-466`) which pattern-matches `Term.Const(Constant.Integer(i), _)` - prefer that exact form.

- [ ] **Step 2: Run the test to verify it fails**

Run: `sbtn "scalusJVM/testOnly scalus.compiler.sir.lowering.ValueIntrinsicsLoweringTest"`
Expected: first test FAILS on `assert(hasBuiltin(uplc, "lookupCoin"))` (portable lowering emitted); PV10 test may already pass.

- [ ] **Step 3: Create the intrinsic module**

Create `ValueIntrinsics.scala`:

```scala
package scalus.compiler.intrinsics

import scalus.cardano.onchain.plutus.v1.Value
import scalus.compiler.Compile
import scalus.compiler.intrinsics.IntrinsicHelpers.*
import scalus.compiler.sir.lowering.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.{ByteString, Data}

/** PV11 (vanRossem) intrinsic lowerings for `plutus.v1.Value` operations via the CIP-153
  * MaryEraValue builtins. Registered in `IntrinsicResolver.registry` with minPV = 11; at PV10 the
  * linked SIR bodies are used unchanged.
  *
  * Semantics note: `unValueData` requires canonical form (strictly ascending keys, no zero
  * amounts, no empty inner maps, keys <= 32 bytes, amounts within +-(2^127)) and fails otherwise;
  * `unionValue`/`scaleValue` fail on 128-bit overflow. See the design doc
  * `docs/superpowers/specs/2026-08-18-t7-value-builtins-lowering-design.md`.
  */
@Compile
object ValueIntrinsicsV11 {

    def quantityOf(v: Value, cs: ByteString, tn: ByteString): BigInt =
        lookupCoin(cs, tn, unValueData(typeProxy[Data](v)))
}

object ValueReprRules {

    private val defaultOut: ReprRule = (outTp, _, lctx) =>
        typegens.SirTypeUplcGenerator.defaultRepresentation(outTp)(using lctx)

    val rules: Map[String, ReprRule] = Map(
      "quantityOf" -> defaultOut
    )
}
```

- [ ] **Step 4: Register the module**

In `IntrinsicResolver.scala`:

1. Next to the module-name constants (near :29-33):

```scala
    private val ValueModule = "scalus.cardano.onchain.plutus.v1.Value$"
```

and next to the provider constants (near `AssocMapIntrinsicsModule`):

```scala
    private val ValueIntrinsicsV11Module = "scalus.compiler.intrinsics.ValueIntrinsicsV11$"
```

2. Add `"scalus.compiler.intrinsics.ValueIntrinsicsV11"` to the `compiledModules(...)` list in `defaultIntrinsicModules`.

3. Add `ValueReprRules` to the `import scalus.compiler.intrinsics.{...}` line near :134.

4. Add the registry entry:

```scala
      ValueModule -> List(
        (WildcardRepr, 11, ValueIntrinsicsV11Module, ValueReprRules.rules, NoArgConvert)
      ),
```

Do NOT add `ValueModule` to `isIntrinsicDispatchedModule` - `Value` is monomorphic, no TypeVar rewriting needed.

- [ ] **Step 5: Run the test again**

Run: `sbtn "scalusJVM/testOnly scalus.compiler.sir.lowering.ValueIntrinsicsLoweringTest"`
Expected: PASS.

Debugging if the intrinsic does not fire or mis-lowers:
- Enable the resolver trace by lowering with `Options(debug = true)` and look for the trace at `IntrinsicResolver.scala:387-390/:433-438`.
- Arity: `tryResolveFull` requires `countTopLambdas(provider body) == argument count`. `quantityOf` is an extension method: 3 SIR lambdas (self, cs, tn) - the flat 3-param provider def matches (same pattern as `BuiltinListOperationsV11.drop`).
- If `typeProxy[Data](v)` leaves an incompatible representation for the `unValueData` argument (a lowering exception mentioning representations), replace it with `typeProxyRepr[Data](v, UplcRepresentation.PackedData)` - the `Value` runtime bytes are exactly the Data map, so the relabel is free either way. Whichever form works becomes the pattern for Task 3.

- [ ] **Step 6: Run the surrounding suites for regressions**

Run: `sbtn "scalusJVM/testOnly scalus.compiler.sir.lowering.* scalus.uplc.eval.ValueBuiltinsBudgetTest scalus.ledger.api.v1.ValueTest"`
Expected: `ValueBuiltinsBudgetTest` WILL now fail (its "SortedMap strategy" measurements assume the portable walk, which just got 13-75x cheaper at PV11). That is expected and fixed in Task 5 - do not fix it here. Everything else must pass.

- [ ] **Step 7: Format and commit**

```bash
sbtn scalafmtAll
git add scalus-core/shared/src/main/scala/scalus/compiler/intrinsics/ValueIntrinsics.scala scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/IntrinsicResolver.scala scalus-core/jvm/src/test/scala/scalus/compiler/sir/lowering/ValueIntrinsicsLoweringTest.scala
git commit -m "feat(sir): lower Value.quantityOf to lookupCoin at PV11 (T7 phase 1, first intrinsic)"
```

(If `ValueBuiltinsBudgetTest` is red at this commit, note it in the commit body: "ValueBuiltinsBudgetTest temporarily red, re-pinned in the T7 budgets commit".)

---

### Task 3: Remaining operations + differential and strictness tests

**Files:**
- Modify: `scalus-core/shared/src/main/scala/scalus/compiler/intrinsics/ValueIntrinsics.scala`
- Test: `scalus-core/jvm/src/test/scala/scalus/compiler/sir/lowering/ValueIntrinsicsLoweringTest.scala`

**Interfaces:**
- Consumes: `Builtins.unionValue(v1, v2): BuiltinValue`, `Builtins.scaleValue(s: BigInt, v: BuiltinValue): BuiltinValue`, `Builtins.valueContains(v1, v2): Boolean`, `Builtins.valueData(v: BuiltinValue): Data`, `typeProxyRepr[V](x, repr)`.
- Consumes from Task 1: `Value$.containsAtLeast` (arity 2). From Task 2: the proxy form validated in its Step 5, and `ValueReprRules.defaultOut`.
- Produces: provider defs `plus`, `minus`, `multiply`, `negate`, `containsAtLeast` matching the `Value$` methods (`plus(a, b)` :431, `minus(a, b)` :452, `multiply(v, factor)` :491, `negate(v)` :409 - all plain defs; `containsAtLeast` extension, arity 2).

- [ ] **Step 1: Write the failing lowering tests**

Add to `ValueIntrinsicsLoweringTest.scala`. Include a second canonical value for binary ops and a `hasNoCip153Builtins` helper:

```scala
    private def valueData2: Data =
        Data.Map(
          plutus.prelude.List(
            (
              Data.B(ByteString.fromHex("bb" * 28)),
              Data.Map(plutus.prelude.List((Data.B(tok), Data.I(3))))
            )
          )
        )

    private val cip153Names =
        List("lookupCoin", "unionValue", "scaleValue", "valueContains", "insertCoin")

    private def hasNoCip153Builtins(t: Term): Boolean =
        cip153Names.forall(n => !hasBuiltin(t, n))

    private val plusSir = compile { (d1: Data, d2: Data) =>
        import scalus.uplc.builtin.Data.toData
        (fromData[Value](d1) + fromData[Value](d2)).toData
    }
    private val minusSir = compile { (d1: Data, d2: Data) =>
        import scalus.uplc.builtin.Data.toData
        (fromData[Value](d1) - fromData[Value](d2)).toData
    }
    private val multiplySir = compile { (d: Data) =>
        import scalus.uplc.builtin.Data.toData
        (fromData[Value](d) * BigInt(3)).toData
    }
    private val negateSir = compile { (d: Data) =>
        import scalus.uplc.builtin.Data.toData
        (-fromData[Value](d)).toData
    }
    private val containsSir = compile { (d1: Data, d2: Data) =>
        fromData[Value](d1).containsAtLeast(fromData[Value](d2))
    }

    test("plus/minus/multiply/negate/containsAtLeast lower to CIP-153 builtins at PV11") {
        assert(hasBuiltin(plusSir.toUplc(), "unionValue"))
        assert(hasBuiltin(minusSir.toUplc(), "unionValue"))
        assert(hasBuiltin(minusSir.toUplc(), "scaleValue"))
        assert(hasBuiltin(multiplySir.toUplc(), "scaleValue"))
        assert(hasBuiltin(negateSir.toUplc(), "scaleValue"))
        assert(hasBuiltin(containsSir.toUplc(), "valueContains"))
    }

    test("all ops keep the portable lowering at PV10") {
        for sir <- List(plusSir, minusSir, multiplySir, negateSir, containsSir) do
            assert(hasNoCip153Builtins(sir.toUplc(using pv10)()))
    }

    test("PV11 and PV10 lowerings agree on canonical values") {
        def run(t: Term): Term = t.evaluateDebug match
            case Result.Success(r, _, _, _) => r
            case other                      => fail(s"evaluation failed: $other")
        def both(sir: scalus.compiler.sir.SIR, args: Term => Term): Unit =
            assert(run(args(sir.toUplc())) == run(args(sir.toUplc(using pv10)())))
        both(plusSir, u => u $ valueData.asTerm $ valueData2.asTerm)
        both(minusSir, u => u $ valueData.asTerm $ valueData2.asTerm)
        both(multiplySir, u => u $ valueData.asTerm)
        both(negateSir, u => u $ valueData.asTerm)
        both(containsSir, u => u $ valueData.asTerm $ valueData2.asTerm)
        both(quantityOfSir, u => u $ valueData.asTerm $ policyBB.asTerm $ tok.asTerm)
    }

    test("PV11 strict validation: malformed values fail where PV10 succeeds") {
        // zero amount
        val zeroAmount = Data.Map(
          plutus.prelude.List(
            (
              Data.B(ByteString.fromHex("aa" * 28)),
              Data.Map(plutus.prelude.List((Data.B(tok), Data.I(0))))
            )
          )
        )
        // duplicate (thus non-strictly-ascending) policy keys
        val dupKeys = Data.Map(
          plutus.prelude.List(
            (
              Data.B(ByteString.fromHex("aa" * 28)),
              Data.Map(plutus.prelude.List((Data.B(tok), Data.I(1))))
            ),
            (
              Data.B(ByteString.fromHex("aa" * 28)),
              Data.Map(plutus.prelude.List((Data.B(tok), Data.I(2))))
            )
          )
        )
        for bad <- List(zeroAmount, dupKeys) do
            val pv11 = (quantityOfSir.toUplc() $ bad.asTerm $ policyBB.asTerm $ tok.asTerm)
            assert(pv11.evaluateDebug.isInstanceOf[Result.Failure], s"expected PV11 failure: $bad")
            val pv10r =
                (quantityOfSir.toUplc(using pv10)() $ bad.asTerm $ policyBB.asTerm $ tok.asTerm)
            assert(pv10r.evaluateDebug.isInstanceOf[Result.Success])
    }
```

- [ ] **Step 2: Run the tests to verify the new ones fail**

Run: `sbtn "scalusJVM/testOnly scalus.compiler.sir.lowering.ValueIntrinsicsLoweringTest"`
Expected: the new PV11-lowering assertions FAIL (no unionValue/scaleValue/valueContains emitted yet); Task 2's tests still pass.

- [ ] **Step 3: Add the provider defs**

In `ValueIntrinsicsV11`, using the proxy form validated in Task 2 Step 5 (shown here with `typeProxy`; substitute `typeProxyRepr(..., UplcRepresentation.PackedData)` if that was the validated form). Note the `typeProxyRepr[Value](..., UplcRepresentation.ProductCaseOneElement)` on Value-returning bodies; if `ProductCaseOneElement` is not accepted by `interpretReprSIR` at lowering time (LoweringException naming the repr), fall back to declaring the output repr only via `ValueReprRules.valueOut` below and returning `typeProxy[Value](...)`:

```scala
    def plus(a: Value, b: Value): Value =
        typeProxyRepr[Value](
          valueData(
            unionValue(unValueData(typeProxy[Data](a)), unValueData(typeProxy[Data](b)))
          ),
          UplcRepresentation.ProductCaseOneElement
        )

    def minus(a: Value, b: Value): Value =
        typeProxyRepr[Value](
          valueData(
            unionValue(
              unValueData(typeProxy[Data](a)),
              scaleValue(BigInt(-1), unValueData(typeProxy[Data](b)))
            )
          ),
          UplcRepresentation.ProductCaseOneElement
        )

    def multiply(v: Value, factor: BigInt): Value =
        typeProxyRepr[Value](
          valueData(scaleValue(factor, unValueData(typeProxy[Data](v)))),
          UplcRepresentation.ProductCaseOneElement
        )

    def negate(v: Value): Value =
        typeProxyRepr[Value](
          valueData(scaleValue(BigInt(-1), unValueData(typeProxy[Data](v)))),
          UplcRepresentation.ProductCaseOneElement
        )

    def containsAtLeast(v: Value, other: Value): Boolean =
        valueContains(unValueData(typeProxy[Data](v)), unValueData(typeProxy[Data](other)))
```

And in `ValueReprRules`:

```scala
    private val valueOut: ReprRule = (_, _, _) =>
        ProductCaseClassRepresentation.OneElementWrapper(
          ProductCaseClassRepresentation.PackedDataMap
        )

    val rules: Map[String, ReprRule] = Map(
      "quantityOf" -> defaultOut,
      "plus" -> valueOut,
      "minus" -> valueOut,
      "multiply" -> valueOut,
      "negate" -> valueOut,
      "containsAtLeast" -> defaultOut
    )
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `sbtn "scalusJVM/testOnly scalus.compiler.sir.lowering.ValueIntrinsicsLoweringTest"`
Expected: PASS. If the PV11/PV10 agreement test fails on `plus`: check ordering - `binaryOpValues` and `unionValue` both produce ascending keys and drop zero sums, so a mismatch means a proxy/repr bug, not a semantics bug; inspect both result terms with `.show`.

- [ ] **Step 5: Run the intrinsic-adjacent suites**

Run: `sbtn "scalusJVM/testOnly scalus.compiler.* scalus.builtin.*"`
Expected: PASS (except nothing new; `IntrinsicResolverTest` must stay green).

- [ ] **Step 6: Format and commit**

```bash
sbtn scalafmtAll
git add scalus-core/shared/src/main/scala/scalus/compiler/intrinsics/ValueIntrinsics.scala scalus-core/jvm/src/test/scala/scalus/compiler/sir/lowering/ValueIntrinsicsLoweringTest.scala
git commit -m "feat(sir): lower Value plus/minus/multiply/negate/containsAtLeast via CIP-153 builtins at PV11"
```

---

### Task 4: `Options.valueBuiltins` flag + gating

**Files:**
- Modify: `scalus-core/shared/src/main/scala/scalus/compiler/compiler.scala` (Options case class, fields end at `noWarn`)
- Modify: `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/IntrinsicResolver.scala` (add `intrinsicModulesFor`)
- Modify: `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/UplcPipeline.scala` (V3 branch, `intrinsicModules = ...` around :67)
- Modify: `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/SirToUplcV3Lowering.scala` (companion factory around :155-163)
- Modify: `build.sbt` (mimaBinaryIssueFilters if MiMa flags the Options change)
- Test: `scalus-core/jvm/src/test/scala/scalus/compiler/sir/lowering/ValueIntrinsicsLoweringTest.scala`

**Interfaces:**
- Produces: `Options.valueBuiltins: Boolean = true` (new last case-class field); `IntrinsicResolver.intrinsicModulesFor(valueBuiltins: Boolean): Map[String, Module]`.

- [ ] **Step 1: Write the failing test**

```scala
    test("valueBuiltins = false disables the intrinsics at PV11") {
        val off = Options(valueBuiltins = false)
        val uplc = quantityOfSir.toUplc(using off)()
        assert(hasNoCip153Builtins(uplc))
        assert(evalInt(uplc $ valueData.asTerm $ policyBB.asTerm $ tok.asTerm) == BigInt(7))
    }
```

- [ ] **Step 2: Run it - expect a COMPILE failure** (`valueBuiltins is not a member of Options`).

Run: `sbtn "scalusJVM/testOnly scalus.compiler.sir.lowering.ValueIntrinsicsLoweringTest"`

- [ ] **Step 3: Implement the flag and gating**

1. `compiler.scala` - add as the LAST field of `Options`, after `noWarn`:

```scala
    /** When true (the default) and `targetProtocolVersion >= vanRossemPV`, `plutus.v1.Value`
      * operations (`quantityOf`, `+`, `-`, `*`, `negate`, `containsAtLeast`) lower to the CIP-153
      * builtins (`lookupCoin`, `unionValue`, `scaleValue`, `valueContains`). The builtins require
      * values in canonical form (strictly ascending keys, no zero amounts, no empty inner maps,
      * keys <= 32 bytes, amounts within +-(2^127)) and make the script fail otherwise, and
      * `unionValue`/`scaleValue` fail on 128-bit overflow - stricter than the portable lowering,
      * which tolerates malformed values. Set to false to keep the portable lowering at any PV.
      */
    valueBuiltins: Boolean = true
```

2. `IntrinsicResolver.scala` - below `defaultIntrinsicModules`:

```scala
    /** Intrinsic modules honoring `Options.valueBuiltins`: when false, the ValueIntrinsicsV11
      * provider is absent, so `findProviderBinding` returns None and Value ops fall back to the
      * linked SIR bodies.
      */
    def intrinsicModulesFor(valueBuiltins: Boolean): Map[String, Module] =
        if valueBuiltins then defaultIntrinsicModules
        else defaultIntrinsicModules - ValueIntrinsicsV11Module
```

Note: verify the key format - `defaultIntrinsicModules` keys must match `ValueIntrinsicsV11Module` (with trailing `$`). If `compiledModules` keys carry no `$`, adjust the subtraction key accordingly (check by printing `defaultIntrinsicModules.keys` in a scratch test, or by how `findProviderBinding` succeeds in Task 2).

3. `UplcPipeline.scala` V3 branch: `intrinsicModules = IntrinsicResolver.intrinsicModulesFor(options.valueBuiltins),`

4. `SirToUplcV3Lowering.scala` companion factory (the one reading `options.*`): `intrinsicModules = IntrinsicResolver.intrinsicModulesFor(options.valueBuiltins),`

- [ ] **Step 4: Run the test to verify it passes**

Run: `sbtn "scalusJVM/testOnly scalus.compiler.sir.lowering.ValueIntrinsicsLoweringTest"`
Expected: PASS.

- [ ] **Step 5: MiMa**

Run: `sbtn mima`
Expected: reported problems only for `scalus.compiler.Options` (`apply`/`copy`/`<init>`/`copy$default$*` arity change from the new field). Add each reported filter to the existing `mimaBinaryIssueFilters` (near `build.sbt:415` settings block for scalusJVM), each with the policy comment, e.g.:

```scala
      // T7: Options gained the valueBuiltins field (new defaulted last parameter).
      // Source-compatible; binary signatures of the synthetic case-class methods change.
      ProblemFilters.exclude[DirectMissingMethodProblem]("scalus.compiler.Options.apply"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("scalus.compiler.Options.copy"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("scalus.compiler.Options.this"),
```

Only add filters MiMa actually reports. Re-run `sbtn mima` until green.

- [ ] **Step 6: Format and commit**

```bash
sbtn scalafmtAll
git add scalus-core/shared/src/main/scala/scalus/compiler/compiler.scala scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/IntrinsicResolver.scala scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/UplcPipeline.scala scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/SirToUplcV3Lowering.scala build.sbt scalus-core/jvm/src/test/scala/scalus/compiler/sir/lowering/ValueIntrinsicsLoweringTest.scala
git commit -m "feat(compiler): Options.valueBuiltins flag gating the CIP-153 Value lowering (default on)"
```

---

### Task 5: Budgets, benchmark truth, changelog, full CI

**Files:**
- Modify: `scalus-core/jvm/src/test/scala/scalus/uplc/eval/ValueBuiltinsBudgetTest.scala`
- Modify: budget pins across test dirs (via `scripts/update-budgets.py`)
- Modify: `CHANGELOG.md`

**Interfaces:**
- Consumes: `Options.valueBuiltins` from Task 4.

- [ ] **Step 1: Fix ValueBuiltinsBudgetTest's baseline side**

The suite's "SortedMap strategy" exists to measure the PORTABLE walk; after this feature it would silently measure the intrinsics. Compile all its SortedMap-side programs with the flag off, and update the scaladoc:

```scala
    // The SortedMap side measures the PORTABLE lowering; since T7 the prelude ops themselves
    // lower to CIP-153 builtins at PV11 by default, so we must opt out here.
    private val portable = scalus.compiler.Options(valueBuiltins = false)
```

and change every SortedMap-side `.toUplc()` in the file to `.toUplc(using portable)()`. Leave the builtin side unchanged.

- [ ] **Step 2: Add the "prelude ops are now cheap" regression guard**

New test in `ValueBuiltinsBudgetTest.scala`:

```scala
    test("T7: prelude Value ops lower to builtin-level budgets at PV11 by default") {
        val preludeLookup = compile { (d: Data, cs: ByteString, tn: ByteString) =>
            fromData[Value](d).quantityOf(cs, tn)
        }.toUplc()
        val rawBuiltin = compile { (d: Data, cs: ByteString, tn: ByteString) =>
            lookupCoin(cs, tn, unValueData(d))
        }.toUplc()
        val (pr, pb) = runTerm(preludeLookup $ fiveByTwo.asTerm $ lastPolicy.asTerm $ lastToken.asTerm)
        val (rr, rb) = runTerm(rawBuiltin $ fiveByTwo.asTerm $ lastPolicy.asTerm $ lastToken.asTerm)
        assert(pr == rr)
        // within 2x of the hand-written builtin program (allows intrinsic-wrapper overhead)
        assert(pb.steps < rb.steps * 2, s"prelude=${pb.steps} raw=${rb.steps}")
    }
```

- [ ] **Step 3: Run the suite**

Run: `sbtn "scalusJVM/testOnly scalus.uplc.eval.ValueBuiltinsBudgetTest"`
Expected: PASS (the flag-off SortedMap side restores the old measurements exactly).

- [ ] **Step 4: Re-pin corpus budgets**

Run: `python3 scripts/update-budgets.py` (it iterates `sbtn quick` and rewrites `ExUnits(...)` pins; `--dry` first to see the blast radius). Then the known manual tail (see memory: the script cannot parse Coin fees / assertResult / size pins / Knights tolerance / bloxbean expectations - fix those by hand from the test failure output).

IMPORTANT (dual baselines): example suites pin budgets per Scala compiler generation via `ScalaCompilerVersion.baseline(pre38, since38)`. This change alters PV11 Value-op costs, so BOTH generations need re-measuring - run the update on the current compiler, then follow the repo's documented procedure for the other generation (see `scalus-testkit/.../ScalusTest.scala` baseline usage and the KnightsDataTest pattern) or flag the second-generation numbers as needing a maintainer run if the toolchain is not available locally.

- [ ] **Step 5: Changelog**

Add to the unreleased section of `CHANGELOG.md`:

```markdown
- **PV11 Value builtins (CIP-153).** At `targetProtocolVersion >= vanRossemPV`,
  `plutus.v1.Value` operations (`quantityOf`/`getLovelace`, `+`, `-`, `*`, `negate`, and the new
  `Value.containsAtLeast`) lower to the CIP-153 builtins - 13-75x cheaper per operation.
  Behavior change: the builtins validate canonical form (strictly ascending keys, no zero
  amounts, no empty inner maps, keys <= 32 bytes, amounts within +-(2^127)) and fail on
  malformed values that the portable lowering tolerated; `unionValue`/`scaleValue` fail on
  128-bit overflow. Opt out with `Options.valueBuiltins = false`.
```

- [ ] **Step 6: Full verification**

Run: `sbtn quick`, then `sbtn ci`
Expected: green (formatting, all platforms, MiMa, docs). Fix any stragglers the budget script missed.

- [ ] **Step 7: Commit**

```bash
git add -A ':!docs/internal/UPLC_CORRECTNESS_AUDIT.md'
git status  # verify only intended files are staged; unstage anything unrelated (e.g. user's parallel edits)
git commit -m "test: re-pin budgets for CIP-153 Value lowering; changelog for T7 phase 1"
```

---

## Self-review notes

- Spec coverage: gating (Task 4), intrinsic module + registry (Tasks 2-3), operation mapping incl. minus-via-scaleValue (Task 3), containsAtLeast API (Task 1), lowering tests / differential tests / strictness tests (Tasks 2-3), budgets + dual baselines + changelog + MiMa (Tasks 4-5). Phase 2 items are explicitly out of scope.
- The two genuinely uncertain mechanics (exact `typeProxy` vs `typeProxyRepr` form for the Data cast; `ProductCaseOneElement` acceptance in `interpretReprSIR`) are isolated in Task 2 Step 5 and Task 3 Step 3 with concrete fallback forms and the resolver-trace debugging recipe.
- `eq`, `flatten`, `policyIds`, `tokens`, `withoutLovelace`, `zero`, `apply`, `lovelace` are intentionally NOT lowered (spec: "Explicitly NOT lowered in Phase 1").
