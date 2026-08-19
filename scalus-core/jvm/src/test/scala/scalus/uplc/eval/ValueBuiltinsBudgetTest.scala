package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.onchain.plutus
import scalus.cardano.onchain.plutus.prelude.SortedMap
import scalus.cardano.onchain.plutus.v1.Value
import scalus.cardano.ledger.ExUnits
import scalus.compiler.{compile, Options}
import scalus.uplc.Term
import scalus.uplc.Term.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.Data.{fromData, toData}
import scalus.uplc.builtin.{BuiltinValue, ByteString, Data}

/** CIP-0153 BuiltinValue builtins vs SortedMap-based plutus.v1.Value.
  *
  * Two strategies for working with a Value that arrives as Data:
  *   - SortedMap: `fromData[Value]` once, use Value methods (list recursion), `toData` at the end.
  *   - Builtin: `unValueData` once, use CIP-0153 builtins (lookupCoin, unionValue, scaleValue,
  *     valueContains, insertCoin), `valueData` at the end.
  *
  * The SortedMap side exists to measure the PORTABLE lowering, i.e. the list recursion that runs on
  * every protocol version. Since T7 the prelude `Value` operations themselves lower to the CIP-153
  * builtins at PV11 by default, so every SortedMap-side program here is lowered with [[portable]]
  * (`Options(valueBuiltins = false)`); without that opt-out both sides would emit the same builtins
  * and the comparison would degenerate to "a builtin costs what a builtin costs". The "prelude ops
  * are cheap at the PV11 default" claim is guarded by its own test at the bottom of this suite,
  * which deliberately does NOT opt out.
  *
  * Each strategy is compiled as `(data..., n) => result` with the conversions outside an
  * n-iteration recursive loop around the operation. Running at n=0 and n=reps separates the fixed
  * conversion cost from the per-operation cost: the loop overhead is identical on both sides and
  * cancels in the comparison. Break-even = how many operation calls justify paying for
  * unValueData/valueData.
  *
  * IMPORTANT: this file must have NO `given Options` with a non-default backend. The plugin
  * resolves `Options` at each `compile {}` call site at scalac time and bakes the linker decision
  * into the SIR: under the default V3 backend, `fromData`/`toData` applies are replaced by
  * `UniversalDataConversion` and erased to identity (Value's V3 representation IS Data). A Scott
  * given in scope would irreversibly link the real, eager conversion code instead, and
  * `toUplc(backend = ...)` cannot undo it. That is why these tests live in their own suite and not
  * in ExprSizeAndBudgetTest.
  *
  * Measured verdict (5 policies x 2 tokens, PV11 mainnet costs, V3 pipeline, SortedMap side on the
  * portable lowering):
  *   - `fromData[Value]`/`toData` are free (identity), so the SortedMap strategy has near-zero
  *     fixed cost (884K-1.27M cpu), while `unValueData` pays real parsing up front (3.7M-7.1M).
  *   - Every builtin operation is 13-75x cheaper per call than its portable counterpart (perOp
  *     12.7M-125.9M cpu portable vs 1.0M-4.1M builtin), so the builtin strategy still wins from the
  *     first operation: break-even is 1 call on cpu and 0 on memory for every operation.
  */
class ValueBuiltinsBudgetTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM()
    private val prices = CardanoInfo.mainnet.protocolParams.executionUnitPrices

    /** Lowering options for the SortedMap (baseline) side.
      *
      * The SortedMap side measures the PORTABLE lowering; since T7 the prelude ops themselves lower
      * to CIP-153 builtins at PV11 by default, so we must opt out here. This is a plain val, NOT a
      * `given`: the plugin must keep resolving the default `Options` at every `compile {}` call
      * site (see the suite scaladoc), and only the `toUplc` lowering step is redirected.
      */
    private val portable = Options(valueBuiltins = false)

    /** A normalized (sorted, non-zero) Value as Data: `policies` 28-byte policy ids, each with
      * `tokensPerPolicy` tokens of `amount`.
      */
    private def assetsData(policies: Int, tokensPerPolicy: Int, amount: BigInt): Data = {
        val entries = (1 to policies).map { p =>
            val tokens = (1 to tokensPerPolicy).map { t =>
                (Data.B(ByteString.fromString(f"token$t%02d")), Data.I(amount))
            }
            (Data.B(ByteString.fromHex(f"$p%02x" * 28)), Data.Map(plutus.prelude.List.from(tokens)))
        }
        Data.Map(plutus.prelude.List.from(entries))
    }

    private val fiveByTwo = assetsData(5, 2, 100)
    private val fiveByTwoSmaller = assetsData(5, 2, 7)
    private val lastPolicy = ByteString.fromHex("05" * 28)
    private val lastToken = ByteString.fromString("token02")

    private def runTerm(t: Term): (Term, ExUnits) = t.evaluateDebug match
        case Result.Success(result, budget, _, _) => (result, budget)
        case f                                    => fail(s"evaluation failed: $f")

    /** `None` if the builtin strategy's per-op cost is not cheaper; `Some(0)` if it is cheaper even
      * with the conversions; otherwise the least n with fixedB + n*perB <= fixedA + n*perA.
      */
    private def breakEven(fixedA: Long, perA: Long, fixedB: Long, perB: Long): scala.Option[Long] =
        if perB >= perA then scala.None
        else if fixedB <= fixedA then scala.Some(0L)
        else scala.Some((fixedB - fixedA + (perA - perB) - 1) / (perA - perB))

    /** Runs both strategies at n=0 and n=reps, asserts identical results, asserts the builtin
      * per-operation cost and the fee at n=reps are lower, reports fixed (conversion) vs
      * per-operation costs, and returns the (cpu, mem) break-even call counts.
      */
    private def compareValueStrategies(
        name: String,
        sortedMap: Long => Term,
        builtin: Long => Term,
        reps: Long
    ): (scala.Option[Long], scala.Option[Long]) = {
        val (aRes0, a0) = runTerm(sortedMap(0))
        val (bRes0, b0) = runTerm(builtin(0))
        val (aResN, aN) = runTerm(sortedMap(reps))
        val (bResN, bN) = runTerm(builtin(reps))
        assert(aRes0 == bRes0, s"$name: strategies disagree at n=0")
        assert(aResN == bResN, s"$name: strategies disagree at n=$reps")
        val perACpu = (aN.steps - a0.steps) / reps
        val perAMem = (aN.memory - a0.memory) / reps
        val perBCpu = (bN.steps - b0.steps) / reps
        val perBMem = (bN.memory - b0.memory) / reps
        val beCpu = breakEven(a0.steps, perACpu, b0.steps, perBCpu)
        val beMem = breakEven(a0.memory, perAMem, b0.memory, perBMem)
        val feeA = aN.fee(prices).value
        val feeB = bN.fee(prices).value
        info(
          f"$name%-14s SortedMap fixed cpu=${a0.steps}%9d mem=${a0.memory}%7d perOp cpu=$perACpu%9d mem=$perAMem%7d"
        )
        info(
          f"$name%-14s Builtin   fixed cpu=${b0.steps}%9d mem=${b0.memory}%7d perOp cpu=$perBCpu%9d mem=$perBMem%7d"
        )
        info(
          f"$name%-14s fee at n=$reps: SortedMap=$feeA builtin=$feeB; break-even calls cpu=$beCpu mem=$beMem"
        )
        assert(perBCpu < perACpu, s"$name: builtin must cost less CPU per operation")
        assert(perBMem < perAMem, s"$name: builtin must cost less memory per operation")
        assert(feeB < feeA, s"$name: builtin strategy must cost a lower fee at n=$reps")
        (beCpu, beMem)
    }

    test("conversions: unValueData+valueData roundtrip vs fromData[Value]+toData") {
        val sortedMapUplc =
            compile { (d: Data) => fromData[Value](d).toData }.toUplc(using portable)()
        val builtinUplc = compile { (d: Data) => valueData(unValueData(d)) }.toUplc()
        val (aRes, aBudget) = runTerm(sortedMapUplc $ fiveByTwo.asTerm)
        val (bRes, bBudget) = runTerm(builtinUplc $ fiveByTwo.asTerm)
        assert(aRes == bRes)
        info(
          f"roundtrip      SortedMap cpu=${aBudget.steps}%9d mem=${aBudget.memory}%7d fee=${aBudget.fee(prices).value}"
        )
        info(
          f"roundtrip      Builtin   cpu=${bBudget.steps}%9d mem=${bBudget.memory}%7d fee=${bBudget.fee(prices).value}"
        )
        // Under the default V3 pipeline the prelude conversions are erased to identity, while
        // unValueData/valueData do real parsing and re-serialization work.
        assert(aBudget.steps < bBudget.steps, "prelude roundtrip must be (erased to) identity")
        assert(aBudget.memory < bBudget.memory)
        assert(aBudget.fee(prices).value < bBudget.fee(prices).value)
    }

    test("lookupCoin vs Value.quantityOf") {
        val sortedMapUplc = compile { (d: Data, cs: ByteString, tn: ByteString, n: BigInt) =>
            val v = fromData[Value](d)
            def go(i: BigInt, acc: BigInt): BigInt =
                if i == BigInt(0) then acc
                else go(i - 1, acc + v.quantityOf(cs, tn))
            go(n, 0)
        }.toUplc(using portable)()
        val builtinUplc = compile { (d: Data, cs: ByteString, tn: ByteString, n: BigInt) =>
            val v = unValueData(d)
            def go(i: BigInt, acc: BigInt): BigInt =
                if i == BigInt(0) then acc
                else go(i - 1, acc + lookupCoin(cs, tn, v))
            go(n, 0)
        }.toUplc()
        def applied(uplc: Term)(n: Long): Term =
            uplc $ fiveByTwo.asTerm $ lastPolicy.asTerm $ lastToken.asTerm $ n.asTerm
        val (beCpu, beMem) =
            compareValueStrategies(
              "lookup",
              applied(sortedMapUplc),
              applied(builtinUplc),
              reps = 16
            )
        assert(beCpu == scala.Some(1L) && beMem == scala.Some(0L))
    }

    test("unionValue vs Value.plus") {
        val sortedMapUplc = compile { (d1: Data, d2: Data, n: BigInt) =>
            val a = fromData[Value](d1)
            val b = fromData[Value](d2)
            def go(i: BigInt, acc: Value): Value =
                if i == BigInt(0) then acc
                else go(i - 1, acc + b)
            go(n, a).toData
        }.toUplc(using portable)()
        val builtinUplc = compile { (d1: Data, d2: Data, n: BigInt) =>
            val a = unValueData(d1)
            val b = unValueData(d2)
            def go(i: BigInt, acc: BuiltinValue): BuiltinValue =
                if i == BigInt(0) then acc
                else go(i - 1, unionValue(acc, b))
            valueData(go(n, a))
        }.toUplc()
        def applied(uplc: Term)(n: Long): Term =
            uplc $ fiveByTwo.asTerm $ fiveByTwoSmaller.asTerm $ n.asTerm
        val (beCpu, beMem) =
            compareValueStrategies("union", applied(sortedMapUplc), applied(builtinUplc), reps = 16)
        assert(beCpu == scala.Some(1L) && beMem == scala.Some(0L))
    }

    test("scaleValue vs Value.multiply") {
        val sortedMapUplc = compile { (d: Data, n: BigInt) =>
            val v = fromData[Value](d)
            def go(i: BigInt, acc: Value): Value =
                if i == BigInt(0) then acc
                else go(i - 1, acc * BigInt(3))
            go(n, v).toData
        }.toUplc(using portable)()
        val builtinUplc = compile { (d: Data, n: BigInt) =>
            val v = unValueData(d)
            def go(i: BigInt, acc: BuiltinValue): BuiltinValue =
                if i == BigInt(0) then acc
                else go(i - 1, scaleValue(3, acc))
            valueData(go(n, v))
        }.toUplc()
        def applied(uplc: Term)(n: Long): Term = uplc $ fiveByTwo.asTerm $ n.asTerm
        val (beCpu, beMem) =
            compareValueStrategies("scale", applied(sortedMapUplc), applied(builtinUplc), reps = 8)
        assert(beCpu == scala.Some(1L) && beMem == scala.Some(0L))
    }

    test("valueContains vs SortedMap containment") {
        val sortedMapUplc = compile { (d1: Data, d2: Data, n: BigInt) =>
            val a = fromData[Value](d1)
            val b = fromData[Value](d2)
            def go(i: BigInt, acc: Boolean): Boolean =
                if i == BigInt(0) then acc
                else
                    go(
                      i - 1,
                      acc && b.toSortedMap.forall { kv =>
                          kv._2.forall { tv => a.quantityOf(kv._1, tv._1) >= tv._2 }
                      }
                    )
            go(n, true)
        }.toUplc(using portable)()
        val builtinUplc = compile { (d1: Data, d2: Data, n: BigInt) =>
            val a = unValueData(d1)
            val b = unValueData(d2)
            def go(i: BigInt, acc: Boolean): Boolean =
                if i == BigInt(0) then acc
                else go(i - 1, acc && valueContains(a, b))
            go(n, true)
        }.toUplc()
        def applied(uplc: Term)(n: Long): Term =
            uplc $ fiveByTwo.asTerm $ fiveByTwoSmaller.asTerm $ n.asTerm
        val (beCpu, beMem) =
            compareValueStrategies(
              "contains",
              applied(sortedMapUplc),
              applied(builtinUplc),
              reps = 16
            )
        assert(beCpu == scala.Some(1L) && beMem == scala.Some(0L))
    }

    test("insertCoin vs SortedMap insert") {
        val sortedMapUplc = compile { (d: Data, cs: ByteString, tn: ByteString, n: BigInt) =>
            val v = fromData[Value](d)
            def go(i: BigInt, acc: Value): Value =
                if i == BigInt(0) then acc
                else
                    val tokens = acc.toSortedMap.get(cs).getOrElse(SortedMap.empty)
                    go(
                      i - 1,
                      Value.unsafeFromSortedMap(
                        acc.toSortedMap.insert(cs, tokens.insert(tn, BigInt(42)))
                      )
                    )
            go(n, v).toData
        }.toUplc(using portable)()
        val builtinUplc = compile { (d: Data, cs: ByteString, tn: ByteString, n: BigInt) =>
            val v = unValueData(d)
            def go(i: BigInt, acc: BuiltinValue): BuiltinValue =
                if i == BigInt(0) then acc
                else go(i - 1, insertCoin(cs, tn, BigInt(42), acc))
            valueData(go(n, v))
        }.toUplc()
        def applied(uplc: Term)(n: Long): Term =
            uplc $ fiveByTwo.asTerm $ lastPolicy.asTerm $ lastToken.asTerm $ n.asTerm
        val (beCpu, beMem) =
            compareValueStrategies(
              "insert",
              applied(sortedMapUplc),
              applied(builtinUplc),
              reps = 16
            )
        assert(beCpu == scala.Some(1L) && beMem == scala.Some(0L))
    }

    /** Regression guard for T7: the ergonomic prelude call must cost what hand-writing the CIP-153
      * builtin costs, not what the portable list walk costs.
      *
      * Unoptimized (`toUplc()`), the prelude program carries dead let-bindings for the portable
      * `quantityOf`/`SortedMap.get` it no longer calls, so it pays a handful of extra beta-redexes;
      * the 2x bound absorbs that. Optimized (what a real script ships), the two programs are
      * structurally identical and the budgets match exactly.
      */
    test("T7: prelude Value ops lower to builtin-level budgets at PV11 by default") {
        val preludeLookup = compile { (d: Data, cs: ByteString, tn: ByteString) =>
            fromData[Value](d).quantityOf(cs, tn)
        }.toUplc()
        val rawBuiltin = compile { (d: Data, cs: ByteString, tn: ByteString) =>
            lookupCoin(cs, tn, unValueData(d))
        }.toUplc()
        def applied(uplc: Term): Term =
            uplc $ fiveByTwo.asTerm $ lastPolicy.asTerm $ lastToken.asTerm
        val (pr, pb) = runTerm(applied(preludeLookup))
        val (rr, rb) = runTerm(applied(rawBuiltin))
        assert(pr == rr)
        info(
          f"prelude cpu=${pb.steps}%9d mem=${pb.memory}%7d; raw cpu=${rb.steps}%9d mem=${rb.memory}%7d"
        )
        // within 2x of the hand-written builtin program (allows intrinsic-wrapper overhead)
        assert(pb.steps < rb.steps * 2, s"prelude=${pb.steps} raw=${rb.steps}")
        assert(pb.memory < rb.memory * 2, s"prelude=${pb.memory} raw=${rb.memory}")

        // Optimized: no wrapper left at all, so the budgets must be identical.
        val preludeOpt = compile { (d: Data, cs: ByteString, tn: ByteString) =>
            fromData[Value](d).quantityOf(cs, tn)
        }.toUplcOptimized()
        val rawOpt = compile { (d: Data, cs: ByteString, tn: ByteString) =>
            lookupCoin(cs, tn, unValueData(d))
        }.toUplcOptimized()
        val (_, pOptB) = runTerm(applied(preludeOpt))
        val (_, rOptB) = runTerm(applied(rawOpt))
        assert(pOptB == rOptB, s"optimized prelude=$pOptB raw=$rOptB")

        // And the portable lowering it replaced is genuinely more expensive, so the bounds above do
        // not pass merely because the operation is cheap on every path.
        val portableLookup = compile { (d: Data, cs: ByteString, tn: ByteString) =>
            fromData[Value](d).quantityOf(cs, tn)
        }.toUplc(using portable)()
        val (or, ob) = runTerm(applied(portableLookup))
        assert(or == pr)
        assert(pb.steps * 2 < ob.steps, s"prelude=${pb.steps} portable=${ob.steps}")
    }
}
