package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.{Options, UplcRepr, UplcRepresentation}
import scalus.uplc.{Constant, PlutusV3, Term}
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.{PlutusVM, Result}

enum EnumCaseToDataAction derives FromData, ToData:
    case Timeout
    case Reveal(preimage: ByteString)

/** Only cases without fields, so the constructor index is all the encoding carries. */
enum EnumCaseToDataColor derives FromData, ToData:
    case Red, Green, Blue

@UplcRepr(UplcRepresentation.UplcConstr)
enum EnumCaseToDataNative derives FromData, ToData:
    case Idle
    case Busy(since: BigInt)

/** `.toData` on an enum case written directly, without a `val` or an ascription in between.
  *
  * A constructor typed at the enum lowers to a value typed at its variant. `toData` asks for the
  * enum's data representation, so it has to upcast that value first. A case with fields did not
  * show the problem: its inlined receiver is bound to a `val` of the enum's type, and a let binding
  * upcasts. See https://github.com/scalus3/scalus/issues/377.
  */
class EnumCaseToDataLoweringTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM()
    private given Options = Options.default

    private def eval(t: Term): Term =
        t.evaluateDebug match
            case Result.Success(v, _, _, _) => v
            case other                      => fail(s"evaluation failed: $other")

    private def data(d: Data): Term = Term.Const(Constant.Data(d))

    test("case without fields, written directly") {
        for options <- Seq(Options.default, Options.release, Options.debug) do
            given Options = options
            val t = PlutusV3.compile(EnumCaseToDataAction.Timeout.toData).program.term
            assert(eval(t) == data(EnumCaseToDataAction.Timeout.toData), s"options $options")
    }

    test("case without fields inside a lambda") {
        val t = PlutusV3.compile((x: BigInt) => EnumCaseToDataAction.Timeout.toData).program.term
        assert(eval(t $ BigInt(1).asTerm) == data(EnumCaseToDataAction.Timeout.toData))
    }

    test("case without fields keeps its constructor index") {
        val green = PlutusV3.compile(EnumCaseToDataColor.Green.toData).program.term
        assert(eval(green) == data(EnumCaseToDataColor.Green.toData))
        val blue = PlutusV3.compile(EnumCaseToDataColor.Blue.toData).program.term
        assert(eval(blue) == data(EnumCaseToDataColor.Blue.toData))
    }

    test("case without fields typed as the enum") {
        val ascribed = PlutusV3
            .compile((EnumCaseToDataAction.Timeout: EnumCaseToDataAction).toData)
            .program
            .term
        assert(eval(ascribed) == data(EnumCaseToDataAction.Timeout.toData))
        val bound = PlutusV3
            .compile {
                val a: EnumCaseToDataAction = EnumCaseToDataAction.Timeout
                a.toData
            }
            .program
            .term
        assert(eval(bound) == data(EnumCaseToDataAction.Timeout.toData))
    }

    test("case with fields") {
        val t = PlutusV3
            .compile(EnumCaseToDataAction.Reveal(ByteString.fromHex("CAFE")).toData)
            .program
            .term
        assert(eval(t) == data(EnumCaseToDataAction.Reveal(ByteString.fromHex("CAFE")).toData))
    }

    test("cases of an enum in the UplcConstr representation") {
        val idle = PlutusV3.compile(EnumCaseToDataNative.Idle.toData).program.term
        assert(eval(idle) == data(EnumCaseToDataNative.Idle.toData))
        val busy = PlutusV3.compile(EnumCaseToDataNative.Busy(BigInt(5)).toData).program.term
        assert(eval(busy) == data(EnumCaseToDataNative.Busy(BigInt(5)).toData))
    }
}
