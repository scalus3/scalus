package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.cardano.onchain.plutus
import scalus.cardano.onchain.plutus.v1.Value
import scalus.compiler.sir.SIR
import scalus.compiler.{compile, Options}
import scalus.uplc.{Constant, Term}
import scalus.uplc.Term.*
import scalus.uplc.builtin.Data.fromData
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.eval.{PlutusVM, Result}

class ValueIntrinsicsLoweringTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    private val pv10 = Options(targetProtocolVersion = MajorProtocolVersion.plominPV)

    /** Canonical 2-policy value as Data. */
    private val valueData: Data = {
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

    /** Second canonical value, single policy `bb`, for the binary operations. */
    private val valueData2: Data =
        Data.Map(
          plutus.prelude.List(
            (
              Data.B(policyBB),
              Data.Map(plutus.prelude.List((Data.B(tok), Data.I(3))))
            )
          )
        )

    /** All seven CIP-153 MaryEraValue builtins; none may appear in a PV10 lowering. */
    private val cip153Names =
        List(
          "lookupCoin",
          "unionValue",
          "scaleValue",
          "valueContains",
          "insertCoin",
          "valueData",
          "unValueData"
        )

    private def hasNoCip153Builtins(t: Term): Boolean =
        cip153Names.forall(n => !hasBuiltin(t, n))

    private val quantityOfSir = compile { (d: Data, cs: ByteString, tn: ByteString) =>
        fromData[Value](d).quantityOf(cs, tn)
    }

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
    private val insertCoinSir = compile { (d: Data, cs: ByteString, tn: ByteString, n: BigInt) =>
        import scalus.uplc.builtin.Data.toData
        fromData[Value](d).insertCoin(cs, tn, n).toData
    }
    private val withoutLovelaceSir = compile { (d: Data) =>
        import scalus.uplc.builtin.Data.toData
        fromData[Value](d).withoutLovelace.toData
    }

    private def hasBuiltin(t: Term, name: String): Boolean =
        t.show.contains(s"(builtin $name)")

    private def evalInt(t: Term): BigInt = t.evaluateDebug match
        case Result.Success(Term.Const(Constant.Integer(i), _), _, _, _) => i
        case other =>
            fail(s"evaluation failed: $other")

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

    test("plus/minus/multiply/negate/containsAtLeast lower to CIP-153 builtins at PV11") {
        assert(hasBuiltin(plusSir.toUplc(), "unionValue"))
        assert(hasBuiltin(minusSir.toUplc(), "unionValue"))
        assert(hasBuiltin(minusSir.toUplc(), "scaleValue"))
        assert(hasBuiltin(multiplySir.toUplc(), "scaleValue"))
        assert(hasBuiltin(negateSir.toUplc(), "scaleValue"))
        assert(hasBuiltin(containsSir.toUplc(), "valueContains"))
    }

    test("insertCoin and withoutLovelace lower to the insertCoin builtin at PV11") {
        assert(hasBuiltin(insertCoinSir.toUplc(), "insertCoin"))
        assert(hasBuiltin(withoutLovelaceSir.toUplc(), "insertCoin"))
    }

    test("all ops keep the portable lowering at PV10") {
        for sir <- List(
              plusSir,
              minusSir,
              multiplySir,
              negateSir,
              containsSir,
              insertCoinSir,
              withoutLovelaceSir
            )
        do assert(hasNoCip153Builtins(sir.toUplc(using pv10)()))
    }

    test("PV11 and PV10 lowerings agree on canonical values") {
        def run(t: Term): Term = t.evaluateDebug match
            case Result.Success(r, _, _, _) => r
            case other                      => fail(s"evaluation failed: $other")
        def both(sir: SIR, args: Term => Term): Unit =
            assert(run(args(sir.toUplc())) == run(args(sir.toUplc(using pv10)())))
        both(plusSir, u => u $ valueData.asTerm $ valueData2.asTerm)
        both(minusSir, u => u $ valueData.asTerm $ valueData2.asTerm)
        both(multiplySir, u => u $ valueData.asTerm)
        both(negateSir, u => u $ valueData.asTerm)
        both(containsSir, u => u $ valueData.asTerm $ valueData2.asTerm)
        both(quantityOfSir, u => u $ valueData.asTerm $ policyBB.asTerm $ tok.asTerm)
        // replace, insert-new, and zero-deletes cases
        both(insertCoinSir, u => u $ valueData.asTerm $ policyBB.asTerm $ tok.asTerm $ 42L.asTerm)
        both(
          insertCoinSir,
          u => u $ valueData.asTerm $ ByteString.fromHex("cc" * 28).asTerm $ tok.asTerm $ 9L.asTerm
        )
        both(insertCoinSir, u => u $ valueData.asTerm $ policyBB.asTerm $ tok.asTerm $ 0L.asTerm)
        // an ada-bearing value, so `withoutLovelace` actually deletes something
        val adaValue = Data.Map(
          plutus.prelude.List(
            (
              Data.B(ByteString.empty),
              Data.Map(plutus.prelude.List((Data.B(ByteString.empty), Data.I(1000))))
            ),
            (Data.B(policyBB), Data.Map(plutus.prelude.List((Data.B(tok), Data.I(3)))))
          )
        )
        both(withoutLovelaceSir, u => u $ adaValue.asTerm)
        both(withoutLovelaceSir, u => u $ valueData.asTerm)
    }

    test("valueBuiltins = false disables the intrinsics at PV11") {
        val off = Options(valueBuiltins = false)
        val uplc = quantityOfSir.toUplc(using off)()
        assert(hasNoCip153Builtins(uplc))
        assert(evalInt(uplc $ valueData.asTerm $ policyBB.asTerm $ tok.asTerm) == BigInt(7))
    }

    test("valueBuiltins = false disables the intrinsics for every Value op at PV11") {
        val off = Options(valueBuiltins = false)
        for sir <- List(
              plusSir,
              minusSir,
              multiplySir,
              negateSir,
              containsSir,
              insertCoinSir,
              withoutLovelaceSir
            )
        do assert(hasNoCip153Builtins(sir.toUplc(using off)()))
    }

    test("PV11 strict validation: insertCoin rejects long keys and out-of-range amounts") {
        val longKey = ByteString.fromHex("ab" * 33)
        val pv11 = insertCoinSir.toUplc()
        val pv10i = insertCoinSir.toUplc(using pv10)()
        // 33-byte policy id, non-zero amount: PV11 fails, PV10 inserts it
        val longKeyArgs =
            (u: Term) => u $ valueData.asTerm $ longKey.asTerm $ tok.asTerm $ 1L.asTerm
        assert(longKeyArgs(pv11).evaluateDebug.isFailure)
        assert(longKeyArgs(pv10i).evaluateDebug.isSuccess)
        // amount outside the signed 128-bit range: PV11 fails, PV10 inserts it
        val bigAmount = Term.Const(Constant.Integer(BigInt(2).pow(128)))
        val bigArgs = (u: Term) => u $ valueData.asTerm $ policyBB.asTerm $ tok.asTerm $ bigAmount
        assert(bigArgs(pv11).evaluateDebug.isFailure)
        assert(bigArgs(pv10i).evaluateDebug.isSuccess)
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
        // canonical shape, but the amount is outside the builtin's 128-bit signed range
        val outOfRange = Data.Map(
          plutus.prelude.List(
            (
              Data.B(ByteString.fromHex("aa" * 28)),
              Data.Map(plutus.prelude.List((Data.B(tok), Data.I(BigInt(2).pow(128)))))
            )
          )
        )
        val quantityOfPv11 = quantityOfSir.toUplc()
        val quantityOfPv10 = quantityOfSir.toUplc(using pv10)()
        val plusPv11Term = plusSir.toUplc()
        val plusPv10Term = plusSir.toUplc(using pv10)()
        for bad <- List(zeroAmount, dupKeys, outOfRange) do
            val pv11 = quantityOfPv11 $ bad.asTerm $ policyBB.asTerm $ tok.asTerm
            assert(pv11.evaluateDebug.isFailure, s"expected PV11 failure: $bad")
            val pv10r = quantityOfPv10 $ bad.asTerm $ policyBB.asTerm $ tok.asTerm
            assert(pv10r.evaluateDebug.isSuccess, s"expected PV10 success: $bad")
            // Same `unValueData` guard on a Task 3 operation, so the strictness is not
            // specific to quantityOf.
            val plusPv11 = plusPv11Term $ bad.asTerm $ valueData2.asTerm
            assert(plusPv11.evaluateDebug.isFailure, s"expected PV11 plus failure: $bad")
            val plusPv10 = plusPv10Term $ bad.asTerm $ valueData2.asTerm
            assert(plusPv10.evaluateDebug.isSuccess, s"expected PV10 plus success: $bad")

        // containsAtLeast: a canonical value holding a negative amount is rejected by the
        // `valueContains` builtin. Both PVs fail by design here (the portable body also
        // requires non-negative amounts), so only the PV11 failure is asserted.
        val negativeAmount = Data.Map(
          plutus.prelude.List(
            (
              Data.B(ByteString.fromHex("aa" * 28)),
              Data.Map(plutus.prelude.List((Data.B(tok), Data.I(-1))))
            )
          )
        )
        val containsNeg = containsSir.toUplc() $ negativeAmount.asTerm $ valueData2.asTerm
        assert(
          containsNeg.evaluateDebug.isFailure,
          "expected PV11 containsAtLeast failure on a negative amount"
        )
    }
}
