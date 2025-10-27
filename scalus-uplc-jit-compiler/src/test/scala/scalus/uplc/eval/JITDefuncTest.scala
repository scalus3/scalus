package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.{Constant, DefaultFun, Term}
import scala.util.Try

class JITDefuncTest extends AnyFunSuite {

  // Test against all JIT implementations
  JITImplementation.all.foreach { jit =>
    def runTest(testName: String)(testBody: => Unit): Unit = {
      if (jit.isImplemented) {
        test(s"[${jit.name}] $testName")(testBody)
      } else {
        test(s"[${jit.name}] $testName") {
          pending // Mark as pending for unimplemented JIT
        }
      }
    }

    runTest("Simple constant evaluation") {
      // Test: Const(42)
      val term = Term.Const(Constant.Integer(42))

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV2PostConwayParams
      )

      assert(result == BigInt(42))
    }

    runTest("Simple addition: 5 + 10") {
      // Test: (AddInteger 5) 10
      val term = Term.Apply(
        Term.Apply(
          Term.Builtin(DefaultFun.AddInteger),
          Term.Const(Constant.Integer(5))
        ),
        Term.Const(Constant.Integer(10))
      )

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV2PostConwayParams
      )

      assert(result == BigInt(15))
    }

    runTest("Multiple operations: (5 + 10) * 2") {
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

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV2PostConwayParams
      )

      assert(result == BigInt(30))
    }

    runTest("Lambda application: (\\x -> x + 1) 5") {
      // Test: (\x -> AddInteger x 1) 5
      // Expected: 6
      val term = Term.Apply(
        Term.LamAbs(
          "x",
          Term.Apply(
            Term.Apply(
              Term.Builtin(DefaultFun.AddInteger),
              Term.Var(scalus.uplc.NamedDeBruijn("x", 0))
            ),
            Term.Const(Constant.Integer(1))
          )
        ),
        Term.Const(Constant.Integer(5))
      )

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV2PostConwayParams
      )

      assert(result == BigInt(6))
    }

    runTest("Identity function: (\\x -> x) 42") {
      // Test: (\x -> x) 42
      // Expected: 42
      val term = Term.Apply(
        Term.LamAbs(
          "x",
          Term.Var(scalus.uplc.NamedDeBruijn("x", 0))
        ),
        Term.Const(Constant.Integer(42))
      )

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV2PostConwayParams
      )

      assert(result == BigInt(42))
    }
  }
}
