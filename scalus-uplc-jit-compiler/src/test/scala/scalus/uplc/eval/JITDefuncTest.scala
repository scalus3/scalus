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
    
    runTest("Force/Delay: Force (Delay 42) = 42") {
      // Test: Force (Delay 42)
      // Expected: 42
      val term = Term.Force(
        Term.Delay(
          Term.Const(Constant.Integer(42))
        )
      )

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV2PostConwayParams
      )

      assert(result == BigInt(42))
    }
    
    runTest("ByteString constant") {
      val bytes = scalus.builtin.ByteString.fromArray(Array[Byte](1, 2, 3, 4, 5))
      val term = Term.Const(Constant.ByteString(bytes))

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV2PostConwayParams
      )

      assert(result == bytes)
    }
    
    runTest("String constant") {
      val term = Term.Const(Constant.String("Hello, World!"))

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV2PostConwayParams
      )

      assert(result == "Hello, World!")
    }
    
    runTest("Bool constant: true") {
      val term = Term.Const(Constant.Bool(true))

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV2PostConwayParams
      )

      assert(result == true)
    }
    
    runTest("Bool constant: false") {
      val term = Term.Const(Constant.Bool(false))

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV2PostConwayParams
      )

      assert(result == false)
    }
    
    runTest("Unit constant") {
      val term = Term.Const(Constant.Unit)

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV2PostConwayParams
      )

      assert(result == ())
    }
  }
}
