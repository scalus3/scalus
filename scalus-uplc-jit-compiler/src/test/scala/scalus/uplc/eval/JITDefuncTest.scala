package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.Word64
import scalus.uplc.{Constant, DefaultFun, NamedDeBruijn, Term}
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
    
    runTest("DivideInteger: 15 / 3 = 5") {
      val term = Term.Apply(
        Term.Apply(
          Term.Builtin(DefaultFun.DivideInteger),
          Term.Const(Constant.Integer(15))
        ),
        Term.Const(Constant.Integer(3))
      )

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV2PostConwayParams
      )

      assert(result == BigInt(5))
    }
    
    runTest("EqualsInteger: 5 == 5 = true") {
      val term = Term.Apply(
        Term.Apply(
          Term.Builtin(DefaultFun.EqualsInteger),
          Term.Const(Constant.Integer(5))
        ),
        Term.Const(Constant.Integer(5))
      )

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV2PostConwayParams
      )

      assert(result == true)
    }
    
    runTest("EqualsInteger: 5 == 3 = false") {
      val term = Term.Apply(
        Term.Apply(
          Term.Builtin(DefaultFun.EqualsInteger),
          Term.Const(Constant.Integer(5))
        ),
        Term.Const(Constant.Integer(3))
      )

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV2PostConwayParams
      )

      assert(result == false)
    }
    
    runTest("LessThanInteger: 3 < 5 = true") {
      val term = Term.Apply(
        Term.Apply(
          Term.Builtin(DefaultFun.LessThanInteger),
          Term.Const(Constant.Integer(3))
        ),
        Term.Const(Constant.Integer(5))
      )

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV2PostConwayParams
      )

      assert(result == true)
    }
    
    runTest("LessThanInteger: 5 < 3 = false") {
      val term = Term.Apply(
        Term.Apply(
          Term.Builtin(DefaultFun.LessThanInteger),
          Term.Const(Constant.Integer(5))
        ),
        Term.Const(Constant.Integer(3))
      )

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV2PostConwayParams
      )

      assert(result == false)
    }
    
    runTest("IfThenElse: if true then 1 else 2 = 1") {
      // IfThenElse takes a unit argument first for polymorphism
      val term = Term.Apply(
        Term.Apply(
          Term.Apply(
            Term.Apply(
              Term.Builtin(DefaultFun.IfThenElse),
              Term.Const(Constant.Unit)
            ),
            Term.Const(Constant.Bool(true))
          ),
          Term.Const(Constant.Integer(1))
        ),
        Term.Const(Constant.Integer(2))
      )

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV2PostConwayParams
      )

      assert(result == BigInt(1))
    }
    
    runTest("IfThenElse: if false then 1 else 2 = 2") {
      val term = Term.Apply(
        Term.Apply(
          Term.Apply(
            Term.Apply(
              Term.Builtin(DefaultFun.IfThenElse),
              Term.Const(Constant.Unit)
            ),
            Term.Const(Constant.Bool(false))
          ),
          Term.Const(Constant.Integer(1))
        ),
        Term.Const(Constant.Integer(2))
      )

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV2PostConwayParams
      )

      assert(result == BigInt(2))
    }

    runTest("Constr: empty constructor Constr(0, [])") {
      // Test: Constr(0, [])
      val term = Term.Constr(Word64(0), List.empty)

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV3Params
      )

      assert(result == (0L, List.empty))
    }

    runTest("Constr: constructor with args Constr(1, [42, 99])") {
      // Test: Constr(1, [42, 99])
      val term = Term.Constr(
        Word64(1),
        List(
          Term.Const(Constant.Integer(42)),
          Term.Const(Constant.Integer(99))
        )
      )

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV3Params
      )

      assert(result == (1L, List(BigInt(42), BigInt(99))))
    }

    runTest("Constr: nested constructor Constr(0, [Constr(1, [5])])") {
      // Test: Constr(0, [Constr(1, [5])])
      val term = Term.Constr(
        Word64(0),
        List(
          Term.Constr(
            Word64(1),
            List(Term.Const(Constant.Integer(5)))
          )
        )
      )

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV3Params
      )

      assert(result == (0L, List((1L, List(BigInt(5))))))
    }

    runTest("Case: simple case with no args") {
      // Test: case Constr(0, []) of { 0 -> 100; 1 -> 200 }
      val term = Term.Case(
        Term.Constr(Word64(0), List.empty),
        List(
          Term.Const(Constant.Integer(100)), // case 0
          Term.Const(Constant.Integer(200))  // case 1
        )
      )

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV3Params
      )

      assert(result == BigInt(100))
    }

    runTest("Case: select second branch") {
      // Test: case Constr(1, []) of { 0 -> 100; 1 -> 200 }
      val term = Term.Case(
        Term.Constr(Word64(1), List.empty),
        List(
          Term.Const(Constant.Integer(100)), // case 0
          Term.Const(Constant.Integer(200))  // case 1
        )
      )

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV3Params
      )

      assert(result == BigInt(200))
    }

    runTest("Case: with single argument - identity function") {
      // Test: case Constr(0, [42]) of { 0 -> \x -> x; 1 -> \x -> 0 }
      val term = Term.Case(
        Term.Constr(Word64(0), List(Term.Const(Constant.Integer(42)))),
        List(
          Term.LamAbs("x", Term.Var(NamedDeBruijn("x", 0))), // case 0: identity
          Term.LamAbs("x", Term.Const(Constant.Integer(0)))  // case 1: const 0
        )
      )

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV3Params
      )

      assert(result == BigInt(42))
    }

    runTest("Case: with multiple arguments") {
      // Test: case Constr(0, [5, 10]) of { 0 -> \x y -> x + y; 1 -> \x y -> x * y }
      val term = Term.Case(
        Term.Constr(
          Word64(0),
          List(
            Term.Const(Constant.Integer(5)),
            Term.Const(Constant.Integer(10))
          )
        ),
        List(
          // case 0: \x y -> x + y
          Term.LamAbs(
            "x",
            Term.LamAbs(
              "y",
              Term.Apply(
                Term.Apply(
                  Term.Builtin(DefaultFun.AddInteger),
                  Term.Var(NamedDeBruijn("x", 1))
                ),
                Term.Var(NamedDeBruijn("y", 0))
              )
            )
          ),
          // case 1: \x y -> x * y
          Term.LamAbs(
            "x",
            Term.LamAbs(
              "y",
              Term.Apply(
                Term.Apply(
                  Term.Builtin(DefaultFun.MultiplyInteger),
                  Term.Var(NamedDeBruijn("x", 1))
                ),
                Term.Var(NamedDeBruijn("y", 0))
              )
            )
          )
        )
      )

      // Use PrintLogger to see what's happening
      class PrintLogger extends Logger {
        override def log(msg: String): Unit = println(s"[${jit.name}] $msg")
        override def getLogs: Array[String] = Array.empty
      }

      val result = jit.eval(
        term,
        new PrintLogger(),
        NoBudgetSpender,
        MachineParams.defaultPlutusV3Params
      )

      assert(result == BigInt(15)) // 5 + 10
    }

    runTest("Case: nested case") {
      // Test: case (case Constr(0, [1]) of { 0 -> \x -> Constr(1, [x]); 1 -> \x -> Constr(0, [x]) })
      //       of { 0 -> 100; 1 -> 200 }
      val innerCase = Term.Case(
        Term.Constr(Word64(0), List(Term.Const(Constant.Integer(1)))),
        List(
          // case 0: \x -> Constr(1, [x])
          Term.LamAbs(
            "x",
            Term.Constr(Word64(1), List(Term.Var(NamedDeBruijn("x", 0))))
          ),
          // case 1: \x -> Constr(0, [x])
          Term.LamAbs(
            "x",
            Term.Constr(Word64(0), List(Term.Var(NamedDeBruijn("x", 0))))
          )
        )
      )

      val term = Term.Case(
        innerCase,
        List(
          Term.Const(Constant.Integer(100)), // case 0
          Term.Const(Constant.Integer(200))  // case 1
        )
      )

      val result = jit.eval(
        term,
        NoLogger,
        NoBudgetSpender,
        MachineParams.defaultPlutusV3Params
      )

      assert(result == BigInt(200)) // Inner case produces Constr(1, [1]), outer case selects branch 1
    }
  }
}
