package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.Options
import scalus.compiler.sir.*
import scalus.uplc.*
import scalus.uplc.Constant.asConstant
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.{toData, ToData}
import scalus.uplc.eval.PlutusVM

class SirToUplcV3LoweringConstantsTest extends AnyFunSuite {

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
    )

    extension (sir: SIR)
        infix def lowersTo(r: Term): Unit = {
            val result = SirToUplcV3Lowering(sir, generateErrorTraces = false).lower()
            assert(result.alphaEq(r), s"expected $r, but got $result")
        }

    def lower(
        sir: SIR,
        generateErrorTraces: Boolean = true,
        upcastTo: SIRType = SIRType.FreeUnificator,
        representation: LoweredValueRepresentation = TypeVarRepresentation(true)
    ): Term =
        SirToUplcV3Lowering(
          sir,
          generateErrorTraces = generateErrorTraces,
          upcastTo = upcastTo,
          representation = representation,
          debug = false,
        ).lower()

    extension (term: Term)
        infix def alphaEq(other: Term): Boolean =
            DeBruijn.deBruijnTerm(term) α_== DeBruijn.deBruijnTerm(other)

    private val ae = AnnotationsDecl.empty

    given PlutusVM = PlutusVM.makePlutusV3VM()

    test("lower boolean") {
        SIR.Const(true.asConstant, SIRType.Integer, ae) lowersTo Term.Const(true.asConstant)
    }

    test("lower toData[Boolean]") {
        val compiled = PlutusV3.compile(true.toData)
        assert(compiled.program.term α_== true.toData.asTerm)
    }

    test("lower toData[BigInt]") {
        val compiled = PlutusV3.compile(summon[ToData[BigInt]](BigInt(123)))
        assert(compiled.program.term α_== BigInt(123).toData.asTerm)
    }

    test("lower toData[ByteString]") {
        val compiled = PlutusV3.compile(ByteString.empty.toData)
        assert(compiled.program.term α_== ByteString.empty.toData.asTerm)
    }

    test("lower toData[String]") {
        val compiled = PlutusV3.compile("hello".toData)
        assert(compiled.program.term α_== "hello".toData.asTerm)
    }
}
