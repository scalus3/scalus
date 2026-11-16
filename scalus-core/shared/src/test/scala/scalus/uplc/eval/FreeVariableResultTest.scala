package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{ExUnits, MajorProtocolVersion}
import scalus.uplc.{DeBruijnedProgram, Term}
import scalus.uplc.builtin.Data

import scala.util.{Failure, Try}

/** A result can hold a variable with no binder: like Plutus, the ledger scope check does not look
  * inside `Constr` or `Case`, and discharging a value leaves a free variable as it is. Plutus
  * 1.70.0.0 accepts such a result from PlutusV1 and PlutusV2, and fails it with
  * `InvalidReturnValue` for PlutusV3. The scripts and the Plutus budgets are from
  * https://github.com/scalus3/scalus/issues/474.
  */
class FreeVariableResultTest extends AnyFunSuite {

    /** `(program 1.1.0 (lam r (lam c (constr 0 (delay (var 3))))))` */
    private val constrDelayVar3 = "480101002280088019"

    /** `(program 1.1.0 (lam x (delay (constr 0 (var 3)))))` */
    private val delayConstrVar3 = "480101002180080181"

    /** `(program 1.1.0 (lam x (constr 0 (delay (var 2)))))` */
    private val constrDelayVar2 = "480101002800880101"

    private def evaluate(vm: PlutusVM, cborHex: String, argCount: Int): (Try[Term], ExUnits) = {
        val program = (1 to argCount).foldLeft(DeBruijnedProgram.fromCborHex(cborHex)) { (p, _) =>
            p $ Data.I(0)
        }
        val spender = CountingBudgetSpender()
        (Try(vm.evaluateScript(program, spender, NoLogger)), spender.getSpentBudget)
    }

    test("PlutusV1 and PlutusV2 accept a result with a free variable under constr") {
        val vanRossem = MajorProtocolVersion.vanRossemPV
        for vm <- Seq(PlutusVM.makePlutusV1VM(vanRossem), PlutusVM.makePlutusV2VM(vanRossem)) do
            val (result, budget) = evaluate(vm, constrDelayVar3, 2)
            assert(result.isSuccess, result)
            assert(result.get.show == "(constr 0 (delay free3))")
            assert(budget == ExUnits(memory = 900, steps = 128_100))
    }

    test("PlutusV3 fails a result with a free variable with InvalidReturnValue") {
        val vm = PlutusVM.makePlutusV3VM(MajorProtocolVersion.plominPV)
        for (script, expected) <- Seq(
              delayConstrVar3 -> ExUnits(memory = 500, steps = 64_100),
              constrDelayVar2 -> ExUnits(memory = 600, steps = 80_100)
            )
        do
            val (result, budget) = evaluate(vm, script, 1)
            result match
                case Failure(_: InvalidReturnValue) =>
                case other                          => fail(s"$script: $other")
            assert(budget == expected, script)
    }

    test("evaluateScriptDebug reports the same outcome as evaluateScript") {
        val program = DeBruijnedProgram.fromCborHex(constrDelayVar3) $ Data.I(0) $ Data.I(0)
        val v1 = PlutusVM.makePlutusV1VM(MajorProtocolVersion.vanRossemPV)
        val result = v1.evaluateScriptDebug(program)
        assert(result.isSuccess, result)
        assert(result.budget == ExUnits(memory = 900, steps = 128_100))
        val v3 = PlutusVM.makePlutusV3VM(MajorProtocolVersion.plominPV)
        v3.evaluateScriptDebug(DeBruijnedProgram.fromCborHex(constrDelayVar2) $ Data.I(0)) match
            case f: Result.Failure => assert(f.exception.isInstanceOf[InvalidReturnValue], f)
            case other             => fail(other.toString)
    }

    test("an open script under constr still converts to a named program") {
        val program = DeBruijnedProgram.fromCborHex(constrDelayVar3).toProgram
        assert(program.term.show == "(lam i0 (lam i1 (constr 0 (delay free1))))")
    }
}
