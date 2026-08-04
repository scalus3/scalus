package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.onchain.plutus
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}
import scalus.serialization.flat.Flat
import scalus.uplc.Term.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.toData
import scalus.cardano.ledger.ExUnits
import scalus.uplc.builtin.{BuiltinList, ByteString, Data}
import scalus.uplc.{ExprBuilder, NamedDeBruijn, Term}

class ExprSizeAndBudgetTest extends AnyFunSuite {
    private val encoder = summon[Flat[Term]]
    private val boolSize = encoder.bitSize(compile(true).toUplc())
    private val unitSize = encoder.bitSize(compile(()).toUplc())
    private val fun1Uplc = compile((b: Boolean) => b).toUplc()
    private val fun1Size = encoder.bitSize(fun1Uplc)
    private given PlutusVM = PlutusVM.makePlutusV3VM()
    private val params = CardanoInfo.mainnet.protocolParams
    private val prices = params.executionUnitPrices

    // SimpleSirToUplcLowering is used to have stable sizes in terms, not in data representation.
    given Options =
        Options(targetLoweringBackend = TargetLoweringBackend.ScottEncodingLowering)

    test("unit bit size is 10") {
        assert(unitSize == 10)
    }

    test("bool bit size is 11") {
        assert(boolSize == 11)
    }

    test("BigInt bit size is 26") {
        assert(encoder.bitSize(compile(BigInt(123)).toUplc()) == 26)
    }

    test("Var bit size is 12") {
        assert(encoder.bitSize(Term.Var(NamedDeBruijn("a", 1))) == 12)
    }

    test("fun1 bit size is 16") {
        assert(fun1Size == 16)
    }

    test("let bit size is 8") {
        val uplc = compile { val a = true }.toUplc()
        assert(encoder.bitSize(uplc) - unitSize - boolSize == 8)
    }

    test(
      "new scalus.cardano.onchain.plutus.prelude.List.Cons(true, scalus.cardano.onchain.plutus.prelude.List.Nil) size is 63"
    ) {
        val uplc = compile(
          plutus.prelude.List.Cons(true, plutus.prelude.List.Nil)
        ).toUplcOptimized()
        assert(encoder.bitSize(uplc) == 63)
    }

    test(
      "scalus.cardano.onchain.plutus.prelude.List.cons(true, scalus.cardano.onchain.plutus.prelude.List.Nil) size is 63"
    ) {
        val uplc =
            compile(plutus.prelude.List.single(true)).toUplcOptimized()
        assert(encoder.bitSize(uplc) == 63)
    }

    test("scalus.cardano.onchain.plutus.prelude.List.single(true) size is 63") {
        val uplc =
            compile(plutus.prelude.List.single(true)).toUplcOptimized()
        assert(encoder.bitSize(uplc) == 63)
    }

    test("Recursion cost") {
        val ifUplc =
            compile((n: BigInt) => if n == BigInt(0) then BigInt(0) else n - 1).toUplc()
        val ifBudget0 = (ifUplc $ 0.asTerm).evaluateDebug.budget
        val ifBudget1 = (ifUplc $ 1.asTerm).evaluateDebug.budget
        val sir = compile:
            def rec(n: BigInt): BigInt =
                if n == BigInt(0) then BigInt(0)
                else rec(n - 1)
            rec(1000)
        val uplc = sir.toUplc()
        val budget = uplc.evaluateDebug.budget
        val cpu = (budget.steps - 1000 * ifBudget1.steps) / 1000
        val mem = (budget.memory - 1000 * ifBudget1.memory) / 1000
        val params = CardanoInfo.mainnet.protocolParams
        val lovelacePerRecursion = params.executionUnitPrices.priceSteps * cpu +
            params.executionUnitPrices.priceMemory * mem
        // convert to USD assuming 1 ADA = 0.66 USD
        val pricePerRecursionInUSDMilliCents = lovelacePerRecursion.toDouble / 1000000 * 66_000
        assert(cpu == 32256)
        assert(mem == 102)
        assert(lovelacePerRecursion.ceil == 9)
        // Precise value with higher precision price_step representation
        assert(pricePerRecursionInUSDMilliCents == 0.5419298016)
    }

    test("2nd bytestring in a list fee = 126") {
        val bs1 = hex"01"
        val bs2 = hex"02"
        val bs3 = hex"03"
        val bs4 = hex"04"
        val bs5 = hex"05"
        val data = listData(BuiltinList(bs1.toData, bs2.toData, bs3.toData, bs4.toData, bs5.toData))

        val sir = compile { (d: Data) =>
            unBData(headList(tailList(unListData(d))))
        }
        val uplc = sir.toUplc() $ data.asTerm
        val result = uplc.evaluateDebug

        assert(result.success.term == bs2.asTerm)
        assert(result.budget.memory == 1628)
        assert(result.budget.steps == 434988)
        assert(result.budget.fee(prices).value == 126)
    }

    test("2nd bytestring in a packed bytestring fee = 94") {
        val bs1 = hex"01"
        val bs2 = hex"02"
        val bs3 = hex"03"
        val bs4 = hex"04"
        val bs5 = hex"05"
        val packed = (bs1 ++ bs2 ++ bs3 ++ bs4 ++ bs5).toData

        val sir = compile { (d: Data) =>
            sliceByteString(1, 1, unBData(d))
        }
        val uplc = sir.toUplc() $ packed.asTerm
        val result = uplc.evaluateDebug

        assert(result.success.term == bs2.asTerm)
        assert(result.budget.memory == 1336)
        assert(result.budget.steps == 232710)
        assert(result.budget.fee(prices).value == 94)
    }

    test("5th bytestring in a list fee = 211") {
        val bs1 = hex"01"
        val bs2 = hex"02"
        val bs3 = hex"03"
        val bs4 = hex"04"
        val bs5 = hex"05"
        val data = listData(BuiltinList(bs1.toData, bs2.toData, bs3.toData, bs4.toData, bs5.toData))

        val sir = compile { (d: Data) =>
            unBData(headList(tailList(tailList(tailList(tailList(unListData(d)))))))
        }
        val uplc = sir.toUplc() $ data.asTerm
        val result = uplc.evaluateDebug

        assert(result.success.term == bs5.asTerm)
        assert(result.budget.memory == 2624)
        assert(result.budget.steps == 823977)
        assert(result.budget.fee(prices).value == 211)
    }

    test("5th bytestring in a packed bytestring fee = 94") {
        val bs1 = hex"01"
        val bs2 = hex"02"
        val bs3 = hex"03"
        val bs4 = hex"04"
        val bs5 = hex"05"
        val packed = (bs1 ++ bs2 ++ bs3 ++ bs4 ++ bs5).toData

        val sir = compile { (d: Data) =>
            sliceByteString(4, 1, unBData(d))
        }
        val uplc = sir.toUplc() $ packed.asTerm
        val result = uplc.evaluateDebug

        assert(result.success.term == bs5.asTerm)
        assert(result.budget.memory == 1336)
        assert(result.budget.steps == 232710)
        assert(result.budget.fee(prices).value == 94)
    }

    test("2nd int in a list of ints fee = 126") {
        val i1 = BigInt(0)
        val i2 = BigInt(1)
        val intData = listData(BuiltinList(i1.toData, i2.toData))

        val sir = compile { (d: Data) =>
            unIData(headList(tailList(unListData(d))))
        }
        val uplc = sir.toUplc() $ intData.asTerm
        val result = uplc.evaluateDebug

        assert(result.success.term == i2.asTerm)
        assert(result.budget.memory == 1628)
        assert(result.budget.steps == 435590)
        assert(result.budget.fee(prices).value == 126)
    }

    test("2nd int in a bytestring of 64bit packed ints fee = 198") {
        val i1 = BigInt(0)
        val i2 = BigInt(1)
        val packedInts =
            (ByteString.fromBigIntBigEndian(i1, 8) ++ ByteString.fromBigIntBigEndian(i2, 8)).toData

        val sir = compile { (d: Data) =>
            byteStringToInteger(true, sliceByteString(8, 8, unBData(d)))
        }
        val uplc = sir.toUplc() $ packedInts.asTerm
        val result = uplc.evaluateDebug

        assert(result.success.term == i2.asTerm)
        assert(result.budget.memory == 1737)
        assert(result.budget.steps == 1346626)
        assert(result.budget.fee(prices).value == 198)
    }

    // ------------------------------------------------------------------
    // T2 recursion-encoding proof-record: Z combinator vs self-application
    //
    // Z encoding (LetRecLoweredValue before T2):
    //   (λf. body) (Z (λf. rhs))   where Z = λff.(λxx.ff (λvv.xx xx vv)) (λxx.ff (λvv.xx xx vv))
    // Self-application encoding (T2, as emitted by LetRecLoweredValue):
    //   (λf. body) ((λf. f f) (λf. rhs[f := f f]))
    //
    // Measured (PV11 mainnet params): self-application saves exactly 6 machine
    // steps per recursive call (96,000 cpu / 600 mem), is cheaper even at
    // n=0 and when the body uses f several times, and drops the 12-byte Z
    // term from the script. See docs/internal/CODEGEN_IMPROVEMENT_PLAN.md T2.
    // ------------------------------------------------------------------

    import scalus.uplc.DefaultFun.{AddInteger, EqualsInteger, IfThenElse, SubtractInteger}
    import scalus.uplc.TermDSL.given

    /** λn. force(ifThenElse (equalsInteger n 0) (delay 0) (delay (call (subtractInteger n 1)))) */
    private def countdownRhs(call: Term): Term =
        λ("n")(
          !(!IfThenElse $ (EqualsInteger $ vr"n" $ 0.asTerm)
              $ ~0.asTerm
              $ ~(call $ (SubtractInteger $ vr"n" $ 1.asTerm)))
        )

    /** λn. λacc. force(ifThenElse (n == 0) (delay acc) (delay (call (n - 1) (acc + n)))) */
    private def sumRhs(call: Term): Term =
        λ("n", "acc")(
          !(!IfThenElse $ (EqualsInteger $ vr"n" $ 0.asTerm)
              $ ~vr"acc"
              $ ~(call $ (SubtractInteger $ vr"n" $ 1.asTerm) $ (AddInteger $ vr"acc" $ vr"n")))
        )

    /** `(λf. body(f)) (Z (λf. rhs(f)))` - what LetRecLoweredValue emitted before T2 */
    private def zEncoding(rhs: Term => Term, body: Term => Term): Term =
        λ("f")(body(vr"f")) $ (ExprBuilder.ZTerm $ λ("f")(rhs(vr"f")))

    /** `(λf. body(f)) ((λf. f f) (λf. rhs(f f)))` - the T2 encoding */
    private def selfAppEncoding(rhs: Term => Term, body: Term => Term): Term =
        λ("f")(body(vr"f")) $ (λ("f")(vr"f" $ vr"f") $ λ("f")(rhs(vr"f" $ vr"f")))

    private def runTerm(t: Term): (Term, ExUnits) = t.evaluateDebug match
        case Result.Success(result, budget, _, _) => (result, budget)
        case f                                    => fail(s"evaluation failed: $f")

    private def flatSize(t: Term): Int = t.plutusV3.flatEncoded.length

    /** Runs both encodings, asserts identical results and that self-application is cheaper. Returns
      * (zBudget, selfAppBudget).
      */
    private def compareEncodings(
        name: String,
        z: Term,
        selfApp: Term,
        iterations: Long
    ): (ExUnits, ExUnits) = {
        val (zResult, zBudget) = runTerm(z)
        val (sResult, sBudget) = runTerm(selfApp)
        assert(zResult == sResult, s"$name: encodings disagree on the result")
        assert(sBudget.steps < zBudget.steps, s"$name: self-application must cost less CPU")
        assert(sBudget.memory < zBudget.memory, s"$name: self-application must cost less memory")
        assert(flatSize(selfApp) < flatSize(z), s"$name: self-application must be smaller")
        val cpuDelta = zBudget.steps - sBudget.steps
        val memDelta = zBudget.memory - sBudget.memory
        info(
          f"$name%-20s saved cpu=$cpuDelta%11d (${cpuDelta * 100.0 / zBudget.steps}%5.2f%%) mem=$memDelta%8d (${memDelta * 100.0 / zBudget.memory}%5.2f%%) size=${flatSize(z) - flatSize(selfApp)}%3d B"
        )
        (zBudget, sBudget)
    }

    test("T2 proof: self-application beats Z on countdown loop, 6 machine steps per call") {
        for n <- List(0L, 10L, 1000L) do
            compareEncodings(
              s"countdown(n=$n)",
              zEncoding(countdownRhs, f => f $ n.asTerm),
              selfAppEncoding(countdownRhs, f => f $ n.asTerm),
              iterations = n
            )
        // pinned per-call saving, measured at n=100_000 to amortize the entry delta:
        // 6 machine steps/call = 96_000 cpu, 600 mem at PV11 mainnet costs
        val n = 100_000L
        val (zb, sb) = compareEncodings(
          s"countdown(n=$n)",
          zEncoding(countdownRhs, f => f $ n.asTerm),
          selfAppEncoding(countdownRhs, f => f $ n.asTerm),
          iterations = n
        )
        assert((zb.steps - sb.steps) / n == 96_000L)
        assert((zb.memory - sb.memory) / n == 600L)
    }

    test("T2 proof: self-application beats Z on 2-arg sum loop by the same 6 steps per call") {
        val n = 100_000L
        val (zb, sb) = compareEncodings(
          s"sum(n=$n)",
          zEncoding(sumRhs, f => f $ n.asTerm $ 0.asTerm),
          selfAppEncoding(sumRhs, f => f $ n.asTerm $ 0.asTerm),
          iterations = n
        )
        assert((zb.steps - sb.steps) / n == 96_000L)
        assert((zb.memory - sb.memory) / n == 600L)
    }

    test("T2 proof: body referencing f multiple times pays the unrolling once, still cheaper") {
        // body = f 5 + f 3: two uses of the recursive function
        val body: Term => Term = f => AddInteger $ (f $ 5.asTerm) $ (f $ 3.asTerm)
        compareEncodings(
          "two-uses",
          zEncoding(countdownRhs, body),
          selfAppEncoding(countdownRhs, body),
          iterations = 8
        )
    }

}
