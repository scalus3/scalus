package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Builtins.*
import scalus.builtin.{BuiltinList, ByteString}

/** Tests for the dropList builtin function.
  *
  * dropList drops the first n elements from a list. If n <= 0, the original list is returned. If n
  * >= length of list, an empty list is returned.
  *
  * Cost model: dropList uses IntegerCostedLiterally, meaning the cost is based on the absolute
  * value of n, not its memory representation. This allows for proper cost calculation even with
  * very large n values.
  */
class DropListTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    // Test with Scala Builtins (compile-time evaluation)
    test("dropList: drop 0 elements returns the original list") {
        val list = BuiltinList[BigInt](1, 2, 3, 4, 5)
        val result = dropList(BigInt(0), list)
        assert(result.toList == List(BigInt(1), BigInt(2), BigInt(3), BigInt(4), BigInt(5)))
    }

    test("dropList: drop negative elements returns the original list") {
        val list = BuiltinList[BigInt](1, 2, 3, 4, 5)
        val result = dropList(BigInt(-10), list)
        assert(result.toList == List(BigInt(1), BigInt(2), BigInt(3), BigInt(4), BigInt(5)))
    }

    test("dropList: drop 3 elements from a 5 element list") {
        val list = BuiltinList[BigInt](1, 2, 3, 4, 5)
        val result = dropList(BigInt(3), list)
        assert(result.toList == List(BigInt(4), BigInt(5)))
    }

    test("dropList: drop all elements") {
        val list = BuiltinList[BigInt](1, 2, 3)
        val result = dropList(BigInt(3), list)
        assert(result.toList == List.empty)
    }

    test("dropList: drop more than list length returns empty list") {
        val list = BuiltinList[BigInt](1, 2, 3)
        val result = dropList(BigInt(100), list)
        assert(result.toList == List.empty)
    }

    test("dropList: drop from empty list returns empty list") {
        val list = BuiltinList.empty[BigInt]
        val result = dropList(BigInt(5), list)
        assert(result.toList == List.empty)
    }

    test("dropList: drop with very large n returns empty list") {
        val list = BuiltinList[BigInt](1, 2, 3)
        val result = dropList(BigInt(Long.MaxValue), list)
        assert(result.toList == List.empty)
    }

    test("dropList: works with string list") {
        val list = BuiltinList[String]("a", "b", "c", "d")
        val result = dropList(BigInt(2), list)
        assert(result.toList == List("c", "d"))
    }

    test("dropList: works with ByteString list") {
        import scalus.builtin.ByteString.hex
        val list = BuiltinList[ByteString](hex"01", hex"02", hex"03")
        val result = dropList(BigInt(1), list)
        assert(result.toList == List(hex"02", hex"03"))
    }

    // Test with UPLC evaluation using direct Term construction
    test("dropList UPLC: drop 2 from list of 5 integers") {
        import scalus.uplc.{Constant, DefaultFun, DefaultUni, Term}
        import scalus.uplc.Term.*

        val listConst = Constant.List(
          DefaultUni.Integer,
          List(
            Constant.Integer(1),
            Constant.Integer(2),
            Constant.Integer(3),
            Constant.Integer(4),
            Constant.Integer(5)
          )
        )
        val term = Apply(
          Apply(
            Force(Builtin(DefaultFun.DropList)),
            Const(Constant.Integer(2))
          ),
          Const(listConst)
        )
        val result = term.evaluateDebug
        result match
            case Result.Success(resultTerm, _, _, _) =>
                val expectedList = Constant.List(
                  DefaultUni.Integer,
                  List(
                    Constant.Integer(3),
                    Constant.Integer(4),
                    Constant.Integer(5)
                  )
                )
                assert(
                  resultTerm == Const(expectedList),
                  s"Expected $expectedList but got $resultTerm"
                )
            case Result.Failure(e, _, _, _) =>
                fail(s"Evaluation failed: $e")
    }

    test("dropList UPLC: drop 0 returns original list") {
        import scalus.uplc.{Constant, DefaultFun, DefaultUni, Term}
        import scalus.uplc.Term.*

        val listConst = Constant.List(
          DefaultUni.Integer,
          List(
            Constant.Integer(1),
            Constant.Integer(2),
            Constant.Integer(3)
          )
        )
        val term = Apply(
          Apply(
            Force(Builtin(DefaultFun.DropList)),
            Const(Constant.Integer(0))
          ),
          Const(listConst)
        )
        val result = term.evaluateDebug
        result match
            case Result.Success(resultTerm, _, _, _) =>
                assert(resultTerm == Const(listConst))
            case Result.Failure(e, _, _, _) =>
                fail(s"Evaluation failed: $e")
    }

    test("dropList UPLC: negative n returns original list") {
        import scalus.uplc.{Constant, DefaultFun, DefaultUni, Term}
        import scalus.uplc.Term.*

        val listConst = Constant.List(
          DefaultUni.Integer,
          List(
            Constant.Integer(1),
            Constant.Integer(2),
            Constant.Integer(3)
          )
        )
        val term = Apply(
          Apply(
            Force(Builtin(DefaultFun.DropList)),
            Const(Constant.Integer(-5))
          ),
          Const(listConst)
        )
        val result = term.evaluateDebug
        result match
            case Result.Success(resultTerm, _, _, _) =>
                assert(resultTerm == Const(listConst))
            case Result.Failure(e, _, _, _) =>
                fail(s"Evaluation failed: $e")
    }

    // Budget tests - verify cost model behavior
    test("dropList budget: cost increases linearly with n") {
        import scalus.uplc.{Constant, DefaultFun, DefaultUni, Term}
        import scalus.uplc.Term.*

        val listConst = Constant.List(
          DefaultUni.Integer,
          List(
            Constant.Integer(1),
            Constant.Integer(2),
            Constant.Integer(3),
            Constant.Integer(4),
            Constant.Integer(5)
          )
        )

        val term1 = Apply(
          Apply(Force(Builtin(DefaultFun.DropList)), Const(Constant.Integer(1))),
          Const(listConst)
        )
        val term100 = Apply(
          Apply(Force(Builtin(DefaultFun.DropList)), Const(Constant.Integer(100))),
          Const(listConst)
        )

        val budget1 = term1.evaluateDebug.budget
        val budget100 = term100.evaluateDebug.budget

        // The cost for n=100 should be higher than n=1 due to linear_in_x cost model
        assert(
          budget100.steps > budget1.steps,
          s"Budget should increase with larger n: n=1 had ${budget1.steps}, n=100 had ${budget100.steps}"
        )
    }

    // Test SIR to UPLC compilation produces DropList builtin
    test("SIR dropList compiles to UPLC DropList builtin") {
        import scalus.uplc.{DefaultFun, Term}
        import scalus.uplc.Term.*

        // Compile a simple dropList call as a function
        val sir = compile { (n: BigInt, list: BuiltinList[BigInt]) =>
            dropList(n, list)
        }

        // Convert to UPLC and check that it contains DropList builtin
        val uplc = sir.toUplc()

        // Helper to check if term contains DropList
        def containsDropList(term: Term): Boolean = term match
            case Builtin(DefaultFun.DropList) => true
            case Apply(f, arg)                => containsDropList(f) || containsDropList(arg)
            case LamAbs(_, body)              => containsDropList(body)
            case Force(t)                     => containsDropList(t)
            case Delay(t)                     => containsDropList(t)
            case _                            => false

        assert(
          containsDropList(uplc),
          s"UPLC should contain DropList builtin, got: ${uplc.pretty.flatten.render(100)}"
        )
    }
}
