package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.{compile, Options}
import scalus.compiler.sir.TargetLoweringBackend
import scalus.uplc.*
import scalus.uplc.eval.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.{BuiltinList, ByteString, Data}
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data.toData
import scalus.cardano.ledger.{ExUnitPrices, ExUnits, NonNegativeInterval}
import scala.language.implicitConversions

@Compile
object UnrolledCheck {
    inline def checkN(
        inline n: Int,
        sigs: BuiltinList[Data],
        expected: BuiltinList[Data]
    ): Unit =
        inline if n <= 0 then ()
        else {
            sigs.head == expected.head || (throw new RuntimeException("missing sig"))
            checkN(n - 1, sigs.tail, expected.tail)
        }

    def validator(datum: Data, redeemer: Data, ctx: Data): Unit = {
        val sigs = ctx.toList
        val expected = datum.toList
        checkN(3, sigs, expected)
    }
}

@Compile
object RecursiveCheck {
    def checkSignatories(sigs: BuiltinList[Data], expected: BuiltinList[Data]): Unit =
        if expected.isEmpty then ()
        else {
            sigs.head == expected.head || (throw new RuntimeException("missing sig"))
            checkSignatories(sigs.tail, expected.tail)
        }

    def validator(datum: Data, redeemer: Data, ctx: Data): Unit = {
        val sigs = ctx.toList
        val expected = datum.toList
        checkSignatories(sigs, expected)
    }
}

class LoopUnrollingTest extends AnyFunSuite {

    private val exPrices = ExUnitPrices(
      priceMemory = NonNegativeInterval(0.0577, precision = 15),
      priceSteps = NonNegativeInterval(0.0000721, precision = 15)
    )

    test("Loop unrolling: inline vs recursive") {
        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = false,
          optimizeUplc = true
        )
        given PlutusVM = PlutusVM.makePlutusV3VM()

        val sig1 = hex"aa".toData
        val sig2 = hex"bb".toData
        val sig3 = hex"cc".toData
        val expected =
            Data.List(scalus.cardano.onchain.plutus.prelude.List(sig1, sig2, sig3))
        val sigs = expected
        val redeemer = Data.I(BigInt(0))

        val unrolledSir = compile(UnrolledCheck.validator)
        val unrolledUplc = unrolledSir.toUplc(generateErrorTraces = false)
        val unrolledProgram = unrolledUplc.plutusV3

        val recursiveSir = compile(RecursiveCheck.validator)
        val recursiveUplc = recursiveSir.toUplc(generateErrorTraces = false)
        val recursiveProgram = recursiveUplc.plutusV3

        def eval(name: String, program: Program): Unit = {
            val result = program.deBruijnedProgram.evaluateDebug
            result match
                case Result.Success(_, budget, _, _) =>
                    val flatSize = program.flatEncoded.length
                    val fee = ExUnits(budget.memory, budget.steps).fee(exPrices)
                    info(
                      f"$name%-40s flat=$flatSize%4d B  cpu=${budget.steps}%,12d  mem=${budget.memory}%,10d  fee=${fee.value}%,8d"
                    )
                case Result.Failure(err, _, _, _) =>
                    fail(s"$name failed: ${err.getMessage}")
        }

        eval("Unrolled (3 iterations)", unrolledProgram $ expected $ redeemer $ sigs)
        eval("Recursive (3 iterations)", recursiveProgram $ expected $ redeemer $ sigs)
    }
}
