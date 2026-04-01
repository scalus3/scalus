package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.*
import scalus.uplc.Term.*
import scalus.uplc.eval.*
import scalus.utils.ScalusSourcePos

class CekProfilerTest extends AnyFunSuite {

    given PlutusVM = PlutusVM.makePlutusV3VM()

    test("evaluateProfile returns Result with profile data") {
        val pos = ScalusSourcePos("Test.scala", 10, 0, 10, 20)
        val term = Term.Const(Constant.Integer(42), UplcAnnotation(pos))
        val result = term.evaluateProfile
        assert(result.isSuccess)
        assert(result.profile.isDefined)
        val profile = result.profile.get
        assert(profile.totalBudget.memory > 0)
        assert(profile.totalBudget.steps > 0)
    }

    test("evaluateDebug returns Result without profile") {
        val term = Term.Const(Constant.Integer(42))
        val result = term.evaluateDebug
        assert(result.isSuccess)
        assert(result.profile.isEmpty)
    }

    test("profiling tracks source locations") {
        val pos1 = ScalusSourcePos("A.scala", 5, 0, 5, 10)
        val pos2 = ScalusSourcePos("B.scala", 20, 0, 20, 10)
        // (\ x -> x) 42 with annotations
        val term = Apply(
          LamAbs("x", Var(NamedDeBruijn("x", 1), UplcAnnotation(pos2)), UplcAnnotation(pos1)),
          Const(Constant.Integer(42), UplcAnnotation(pos1)),
          UplcAnnotation(pos1)
        )
        val result = term.evaluateProfile
        assert(result.isSuccess)
        val profile = result.profile.get
        assert(profile.bySourceLocation.nonEmpty)
        val files = profile.bySourceLocation.map(_.file).toSet
        assert(files.contains("A.scala"))
    }

    test("profiling tracks builtins") {
        // addInteger 1 2
        val term = Apply(
          Apply(
            Builtin(DefaultFun.AddInteger),
            Const(Constant.Integer(1))
          ),
          Const(Constant.Integer(2))
        )
        val result = term.evaluateProfile
        assert(result.isSuccess)
        val profile = result.profile.get
        val funcNames = profile.byFunction.map(_.name).toSet
        assert(funcNames.contains("AddInteger"))
    }

    test("empty annotations grouped correctly") {
        val term = Apply(
          LamAbs("x", Var(NamedDeBruijn("x", 1))),
          Const(Constant.Integer(42))
        )
        val result = term.evaluateProfile
        assert(result.isSuccess)
        val profile = result.profile.get
        // With empty annotations, bySourceLocation should have no entries
        assert(profile.bySourceLocation.isEmpty)
        assert(profile.totalBudget.memory > 0)
    }

    test("ProfileFormatter.toText produces output") {
        val term = Term.Const(Constant.Integer(42))
        val result = term.evaluateProfile
        val profile = result.profile.get
        val text = ProfileFormatter.toText(profile)
        assert(text.contains("Profile by Source Location"))
        assert(text.contains("Profile by Function"))
        assert(text.contains("Total:"))
    }

    test("ProfileFormatter.toHtml produces valid HTML") {
        val term = Term.Const(Constant.Integer(42))
        val result = term.evaluateProfile
        val profile = result.profile.get
        val html = ProfileFormatter.toHtml(profile)
        assert(html.contains("<!DOCTYPE html>"))
        assert(html.contains("By Source Location"))
        assert(html.contains("By Function"))
        assert(html.contains("</html>"))
    }

    test("evaluateScriptProfile works") {
        val term = Term.Const(Constant.Unit)
        val program = Program.plutusV3(term).deBruijnedProgram
        given vm: PlutusVM = PlutusVM.makePlutusV3VM()
        val result = vm.evaluateScriptProfile(program)
        assert(result.isSuccess)
        assert(result.profile.isDefined)
        assert(result.profile.get.totalBudget.memory > 0)
    }
}
