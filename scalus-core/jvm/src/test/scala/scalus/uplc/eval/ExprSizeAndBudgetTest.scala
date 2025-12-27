package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString.hex
import scalus.builtin.Data.toData
import scalus.builtin.{BuiltinList, ByteString, Data}
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.ledger.ExUnits.given
import scalus.serialization.flat.Flat
import scalus.uplc.{Constant, NamedDeBruijn, Term}
import scalus.uplc.Term.*
import scalus.uplc.transform.Inliner

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
        val cpu = (budget.steps - 1000 * ifBudget1.steps) / 1000
        val mem = (budget.memory - 1000 * ifBudget1.memory) / 1000
        val params = CardanoInfo.mainnet.protocolParams
        val lovelacePerRecursion = params.executionUnitPrices.priceSteps * cpu +
            params.executionUnitPrices.priceMemory * mem
        // convert to USD assuming 1 ADA = 0.66 USD
        val pricePerRecursionInUSDMilliCents = lovelacePerRecursion.toDouble / 1000000 * 66_000
        assert(cpu == 128540)
        assert(mem == 703)
        assert(lovelacePerRecursion.ceil == 50)
        // Precise value with higher precision price_step representation
        assert(pricePerRecursionInUSDMilliCents == 3.2888350440000003)
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

    test("2nd bytestring in a list fee = 105") {
        given PlutusVM = PlutusVM.makePlutusV3VM()
        val prices = CardanoInfo.mainnet.protocolParams.executionUnitPrices
        val bs1 = hex"01"
        val bs2 = hex"02"
        val bs3 = hex"03"
        val bs4 = hex"04"
        val bs5 = hex"05"
        val data = listData(BuiltinList(bs1.toData, bs2.toData, bs3.toData, bs4.toData, bs5.toData))

        val sir = compile { (d: Data) =>
            unBData(headList(tailList(unListData(d))))
        }
        val uplc = (sir.toUplc() $ data.asTerm) |> Inliner.apply
        val result = uplc.evaluateDebug

        assert(result.success.term == bs2.asTerm)
        assert(result.budget.memory == 1328)
        assert(result.budget.steps == 386988)
        assert(result.budget.fee(prices).value == 105)
    }

    test("2nd bytestring in a packed bytestring fee = 74") {
        given PlutusVM = PlutusVM.makePlutusV3VM()
        val prices = CardanoInfo.mainnet.protocolParams.executionUnitPrices
        val bs1 = hex"01"
        val bs2 = hex"02"
        val bs3 = hex"03"
        val bs4 = hex"04"
        val bs5 = hex"05"
        val packed = (bs1 ++ bs2 ++ bs3 ++ bs4 ++ bs5).toData

        val sir = compile { (d: Data) =>
            sliceByteString(1, 1, unBData(d))
        }
        val uplc = (sir.toUplc() $ packed.asTerm) |> Inliner.apply
        val result = uplc.evaluateDebug

        assert(result.success.term == bs2.asTerm)
        assert(result.budget.memory == 1036)
        assert(result.budget.steps == 184710)
        assert(result.budget.fee(prices).value == 74)
    }

    test("5th bytestring in a list fee = 191") {
        given PlutusVM = PlutusVM.makePlutusV3VM()
        val prices = CardanoInfo.mainnet.protocolParams.executionUnitPrices
        val bs1 = hex"01"
        val bs2 = hex"02"
        val bs3 = hex"03"
        val bs4 = hex"04"
        val bs5 = hex"05"
        val data = listData(BuiltinList(bs1.toData, bs2.toData, bs3.toData, bs4.toData, bs5.toData))

        val sir = compile { (d: Data) =>
            unBData(headList(tailList(tailList(tailList(tailList(unListData(d)))))))
        }
        val uplc = (sir.toUplc() $ data.asTerm) |> Inliner.apply
        val result = uplc.evaluateDebug

        assert(result.success.term == bs5.asTerm)
        assert(result.budget.memory == 2324)
        assert(result.budget.steps == 775977)
        assert(result.budget.fee(prices).value == 191)
    }

    test("5th bytestring in a packed bytestring fee = 74") {
        given PlutusVM = PlutusVM.makePlutusV3VM()
        val prices = CardanoInfo.mainnet.protocolParams.executionUnitPrices
        val bs1 = hex"01"
        val bs2 = hex"02"
        val bs3 = hex"03"
        val bs4 = hex"04"
        val bs5 = hex"05"
        val packed = (bs1 ++ bs2 ++ bs3 ++ bs4 ++ bs5).toData

        val sir = compile { (d: Data) =>
            sliceByteString(4, 1, unBData(d))
        }
        val uplc = (sir.toUplc() $ packed.asTerm) |> Inliner.apply
        val result = uplc.evaluateDebug

        assert(result.success.term == bs5.asTerm)
        assert(result.budget.memory == 1036)
        assert(result.budget.steps == 184710)
        assert(result.budget.fee(prices).value == 74)
    }

    test("2nd int in a list of ints fee = 105") {
        given PlutusVM = PlutusVM.makePlutusV3VM()
        val prices = CardanoInfo.mainnet.protocolParams.executionUnitPrices
        val i1 = BigInt(0)
        val i2 = BigInt(1)
        val intData = listData(BuiltinList(i1.toData, i2.toData))

        val sir = compile { (d: Data) =>
            unIData(headList(tailList(unListData(d))))
        }
        val uplc = (sir.toUplc() $ intData.asTerm) |> Inliner.apply
        val result = uplc.evaluateDebug

        assert(result.success.term == i2.asTerm)
        assert(result.budget.memory == 1328)
        assert(result.budget.steps == 387590)
        assert(result.budget.fee(prices).value == 105)
    }

    test("2nd int in a bytestring of 64bit packed ints fee = 177") {
        given PlutusVM = PlutusVM.makePlutusV3VM()
        val prices = CardanoInfo.mainnet.protocolParams.executionUnitPrices
        val i1 = BigInt(0)
        val i2 = BigInt(1)
        val packedInts =
            (ByteString.fromBigIntBigEndian(i1, 8) ++ ByteString.fromBigIntBigEndian(i2, 8)).toData

        val sir = compile { (d: Data) =>
            byteStringToInteger(true, sliceByteString(8, 8, unBData(d)))
        }
        val uplc = (sir.toUplc() $ packedInts.asTerm) |> Inliner.apply
        val result = uplc.evaluateDebug

        assert(result.success.term == i2.asTerm)
        assert(result.budget.memory == 1437)
        assert(result.budget.steps == 1298626)
        assert(result.budget.fee(prices).value == 177)
    }

}
