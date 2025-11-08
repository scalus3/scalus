package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Builtins.*
import scalus.builtin.Data
import scalus.cardano.ledger.CardanoInfo
import scalus.serialization.flat.Flat
import scalus.uplc.NamedDeBruijn
import scalus.uplc.Term
import scalus.uplc.Term.*
import scalus.uplc.eval.ExBudget.given

import scala.math.Ordering.Implicits.*

class ExprSizeAndBudgetTest extends AnyFunSuite {
    private val encoder = summon[Flat[Term]]
    private val boolSize = encoder.bitSize(compile(true).toUplc())
    private val unitSize = encoder.bitSize(compile(()).toUplc())
    private val fun1Uplc = compile((b: Boolean) => b).toUplc()
    private val fun1Size = encoder.bitSize(fun1Uplc)
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    // SimpleSirToUplcLowering is used to have stable sizes in terms, not in data representation.
    given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.ScottEncodingLowering
    )

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

    test("new prelude.List.Cons(true, prelude.List.Nil) size is 83") {
        val uplc = compile(new prelude.List.Cons(true, prelude.List.Nil)).toUplcOptimized()
        assert(encoder.bitSize(uplc) == 83)
    }

    test("prelude.List.cons(true, prelude.List.Nil) size is 123") {
        val uplc = compile(prelude.List.single(true)).toUplcOptimized()
        assert(encoder.bitSize(uplc) == 123)
    }

    test("prelude.List.single(true) size is 123") {
        val uplc = compile(prelude.List.single(true)).toUplcOptimized()
        assert(encoder.bitSize(uplc) == 123)
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
        val cpu = (budget.cpu - 1000 * ifBudget1.cpu) / 1000
        val mem = (budget.memory - 1000 * ifBudget1.memory) / 1000
        val params = CardanoInfo.mainnet.protocolParams
        val lovelacePerRecursion = params.executionUnitPrices.priceSteps * cpu +
            params.executionUnitPrices.priceMemory * mem
        // convert to USD assuming 1 ADA = 0.66 USD
        val pricePerRecursionInUSDMilliCents = lovelacePerRecursion.toDouble / 1000000 * 66_000
        assert(cpu == 128540)
        assert(mem == 703)
        assert(lovelacePerRecursion.ceil == 50)
        assert(pricePerRecursionInUSDMilliCents == 3.28798668)
    }

    test("equalsInteger(unIData) < equalsData(iData)") {
        given PlutusVM = PlutusVM.makePlutusV3VM()
        val eqDataSIR = compile { (d: Data) => equalsData(d, iData(1)) }
        val eqIntegerSIR = compile { (d: Data) => equalsInteger(unIData(d), 1) }
        val d = iData(1)
        val eqDataUplc = eqDataSIR.toUplc() $ d.asTerm
        val eqIntegerUplc = eqIntegerSIR.toUplc() $ d.asTerm
        val (eqData, eqInteger) = (eqDataUplc.evaluateDebug, eqIntegerUplc.evaluateDebug)
        // here we ensure that it's cheaper to convert the data to an integer and compare with equalsInteger
        // than to convert the integer to data and compare with equalsData
        // this is a prerequisite for the optimizations to be valid
        assert(eqInteger.budget < eqData.budget)
    }

}
