package scalus.testing.regression

import scala.language.implicitConversions
import scalus.*
import scalus.prelude.*
import scalus.uplc.eval.*
import org.scalatest.funsuite.AnyFunSuite

@Compile
object Min202507171 {

    enum Tree:
        case One(arg: BigInt)
        case Two(arg1: Tree, arg2: Tree)

    def doClause(
        formula: Tree,
        vars: (List[BigInt], List[BigInt])
    ): (List[BigInt], List[BigInt]) =
        formula match
            case Tree.Two(arg1, arg2) => doClause(arg1, doClause(arg2, vars))
            case _                    => fail()

}

@Compile
object Min202507172:

    extension [A: Ord](self: List[A])
        def insertUniqueOrdered(elem: A): List[A] =
            self match
                case List.Nil => List.single(elem)
                case List.Cons(head, tail) =>
                    elem <=> head match
                        case Order.Less    => self.prepended(elem)
                        case Order.Greater => tail.insertUniqueOrdered(elem).prepended(head)
                        case Order.Equal   => self

    end extension

end Min202507172

class ClausifyMinTest extends AnyFunSuite:

    given PlutusVM = PlutusVM.makePlutusV3VM()

    given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("fail as foirst argument in branch") {
        import Min202507171.*
        val sir = Compiler
            .compile {

                val x = doClause(Tree.One(BigInt(1)), (List.empty, List.empty))

            }
        val uplc = sir.toUplc(generateErrorTraces = false)

        val result = uplc.evaluateDebug

        assert(result.isFailure)

    }

    test("insertUniqueOrdered should work") {
        import Min202507172.*
        val sir = Compiler
            .compile {
                val lrVars: (List[BigInt], List[BigInt]) = (List.single(BigInt(1)), List.empty)
                List.empty.insertUniqueOrdered(lrVars)
            }
        val uplc = sir.toUplcOptimized(generateErrorTraces = false)

        val result = uplc.evaluateDebug

        assert(result.isSuccess)

    }

end ClausifyMinTest
