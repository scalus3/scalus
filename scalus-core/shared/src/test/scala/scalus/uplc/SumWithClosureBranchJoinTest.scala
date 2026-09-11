package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.compiler.{Compile, Options}
import scalus.uplc.builtin.Data
import scalus.uplc.eval.PlutusVM

/** A sum type holding a function has no Data form: it is UplcConstr-only. */
enum ClosureStep:
    case Run(f: BigInt => BigInt)
    case Skip

sealed trait ClosureShape extends Product with Serializable
case class ClosureA(f: BigInt => BigInt) extends ClosureShape
case class ClosureB(n: BigInt) extends ClosureShape

@Compile
object SumWithClosureFixtures {
    def enumIf(d: Data): Unit = {
        val n = d.to[BigInt]
        val s: ClosureStep = if n > 0 then ClosureStep.Run(x => x + 1) else ClosureStep.Skip
        val r = s match
            case ClosureStep.Run(f) => f(n)
            case ClosureStep.Skip   => n
        require(r >= 0, "neg")
    }

    def enumMatch(d: Data): Unit = {
        val n = d.to[BigInt]
        val s: ClosureStep = n match
            case x if x > 0 => ClosureStep.Run(x => x + 1)
            case _          => ClosureStep.Skip
        val r = s match
            case ClosureStep.Run(f) => f(n)
            case ClosureStep.Skip   => n
        require(r >= 0, "neg")
    }

    def traitIf(d: Data): Unit = {
        val n = d.to[BigInt]
        val t: ClosureShape = if n > 0 then ClosureA(x => x + 1) else ClosureB(n)
        val r = t match
            case ClosureA(f) => f(n)
            case ClosureB(m) => m
        require(r >= 0, "neg")
    }
}

/** Choosing between two constructors of a function-holding sum type at runtime. Building through a
  * single constructor already works; the join of two different constructors did not.
  */
class SumWithClosureBranchJoinTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM()
    private val one = Term.Const(Constant.Data(Data.I(1)))
    private val zero = Term.Const(Constant.Data(Data.I(0)))

    private def runsOnBothBranches(compiled: PlutusV3[Data => Unit]): Unit = {
        val program = compiled.program
        assert((program $ one).evaluateDebug.isSuccess, "Run branch")
        assert((program $ zero).evaluateDebug.isSuccess, "Skip branch")
    }

    test("if between two constructors of a function-holding enum") {
        runsOnBothBranches(PlutusV3.compile(SumWithClosureFixtures.enumIf)(using Options.release))
    }

    test("match returning two constructors of a function-holding enum") {
        runsOnBothBranches(
          PlutusV3.compile(SumWithClosureFixtures.enumMatch)(using Options.release)
        )
    }

    test("if between two case classes of a function-holding sealed trait") {
        runsOnBothBranches(PlutusV3.compile(SumWithClosureFixtures.traitIf)(using Options.release))
    }
}
