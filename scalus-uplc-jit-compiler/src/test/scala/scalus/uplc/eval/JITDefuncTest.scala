package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.{Constant, DefaultFun, Term}

class JITDefuncTest extends AnyFunSuite {

  test("Simple constant evaluation") {
    // Test: Const(42)
    val term = Term.Const(Constant.Integer(42))

    val program = JITDefuncCompiler.compile(term)
    val result = JITDefunc.eval(
      program,
      NoBudgetSpender,
      NoLogger,
      MachineParams.defaultPlutusV2PostConwayParams
    )

    assert(result == BigInt(42))
  }

  test("Simple addition: 5 + 10") {
    // Test: (AddInteger 5) 10
    val term = Term.Apply(
      Term.Apply(
        Term.Builtin(DefaultFun.AddInteger),
        Term.Const(Constant.Integer(5))
      ),
      Term.Const(Constant.Integer(10))
    )

    val program = JITDefuncCompiler.compile(term)
    val result = JITDefunc.eval(
      program,
      NoBudgetSpender,
      NoLogger,
      MachineParams.defaultPlutusV2PostConwayParams
    )

    assert(result == BigInt(15))
  }

  test("Multiple operations: (5 + 10) * 2") {
    // Test: ((MultiplyInteger (AddInteger 5 10)) 2)
    val term = Term.Apply(
      Term.Apply(
        Term.Builtin(DefaultFun.MultiplyInteger),
        Term.Apply(
          Term.Apply(
            Term.Builtin(DefaultFun.AddInteger),
            Term.Const(Constant.Integer(5))
          ),
          Term.Const(Constant.Integer(10))
        )
      ),
      Term.Const(Constant.Integer(2))
    )

    val program = JITDefuncCompiler.compile(term)
    val result = JITDefunc.eval(
      program,
      NoBudgetSpender,
      NoLogger,
      MachineParams.defaultPlutusV2PostConwayParams
    )

    assert(result == BigInt(30))
  }
}
