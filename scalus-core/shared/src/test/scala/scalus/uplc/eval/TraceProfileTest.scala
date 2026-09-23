package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.*
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.Constant.given
import scalus.uplc.DefaultFun.*
import scala.language.implicitConversions

class TraceProfileTest extends AnyFunSuite {
    private given vm: PlutusVM = PlutusVM.makePlutusV3VM()

    // Traces `first`, then `second`, then returns unit: the outer trace returns the delayed
    // inner trace, and the force runs it.
    private val twoTraces: Term = !(!Trace $ "first" $ ~(!Trace $ "second" $ ().asTerm))
    private val traceThenFail: Term = !(!Trace $ "before" $ ~Error())

    test("a debug run reports bare traces and no profile") {
        // spec [TB-1]
        val result = vm.evaluateScriptDebug(twoTraces.plutusV3.deBruijnedProgram)
        assert(result.isSuccess)
        assert(result.logs == Seq("first", "second"))
        assert(result.profile.isEmpty)
        assert(twoTraces.evaluateDebug.logs == Seq("first", "second"))
    }

    test("a profile run pairs each trace with the budget spent when it was emitted") {
        // spec [TB-1] [TB-2] [TB-3] [TB-4]
        val result = vm.evaluateScriptProfile(twoTraces.plutusV3.deBruijnedProgram)
        assert(result.isSuccess)
        assert(result.logs == Seq("first", "second"))
        val profile = result.profile.getOrElse(fail("profile"))
        assert(profile.traces.map(_.message) == result.logs)
        val budgets = profile.traces.map(_.budget)
        assert(budgets.size == 2)
        val first = budgets(0)
        val second = budgets(1)
        assert(first.memory > 0 && first.steps > 0)
        assert(second.memory > first.memory && second.steps > first.steps)
        assert(second.memory <= result.budget.memory && second.steps <= result.budget.steps)
    }

    test("a failing profile run keeps the traces emitted before the failure") {
        // spec [TB-2] [TB-4]
        val result = vm.evaluateScriptProfile(traceThenFail.plutusV3.deBruijnedProgram)
        assert(!result.isSuccess)
        assert(result.logs == Seq("before"))
        assert(result.profile.map(_.traces.map(_.message)) == Some(Seq("before")))
    }
}
