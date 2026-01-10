package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.{Data, FromData, ToData}
import scalus.compiler.compile
import scalus.compiler.sir.*
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.uplc.{Constant, Term}

object SIRUplcV3LoweringRandomSpec {

    case class AA(flag: Boolean, a: BigInt) derives FromData, ToData

    enum BB derives FromData, ToData:
        case C
        case D(a: BigInt)
        case E(a: BigInt, b: BigInt)

}

class SIRUplcV3LoweringRandomSpec extends AnyFunSuite {

    given scalus.compiler.Options = scalus.compiler.Options(
      targetLoweringBackend = scalus.compiler.sir.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    import SIRUplcV3LoweringRandomSpec.*

    given PlutusVM = PlutusVM.makePlutusV3VM()

    test("lowering simple case class") {
        val sir = compile {
            val aa = AA(true, BigInt(123))
            aa.a
        }
        // println(sir.showHighlighted)
        val lowering = SirToUplcV3Lowering(sir)
        val term = lowering.lower()
        // println(term.showHighlighted)
        // println(term.evaluateDebug)
    }

    test("lowering match on simple case class") {
        val sir = compile { (data: Data) =>
            // val aa = AA(true, BigInt(123))
            val aa = Data.fromData[AA](data)
            aa match {
                case AA(flag, a) =>
                    if a != BigInt(123) then {
                        scalus.prelude.fail("Expected a to be 123")
                    }
            }
        }
        // println(sir.showHighlighted)
        val lowering = SirToUplcV3Lowering(sir)
        val term = lowering.lower()
        val aa = AA(true, BigInt(123))
        val termWithData = term $ Term.Const(Constant.Data(Data.toData(aa)))
//        println(term.showHighlighted)
        val result = termWithData.evaluateDebug
        assert(result.isSuccess, s"Lowere dcode failed with result: $result")
    }

    test("lowering simple enum") {
        val sir = compile {
            val bb = BB.E(BigInt(1), BigInt(2))
            bb match
                case BB.C       => BigInt(0)
                case BB.D(a)    => a
                case BB.E(a, b) => a + b
        }
        // println(sir.showHighlighted)

        val lowering = SirToUplcV3Lowering(sir)
        val term = lowering.lower()
        // println(term.showHighlighted)
        val result = term.evaluateDebug
        // println(result)
        assert(result.isSuccess)
        result match {
            case Result.Success(term, budget, costs, log) =>
                assert(term == Term.Const(Constant.Integer(BigInt(3))))
            case Result.Failure(err, budget, costs, log) =>
                fail(s"Lowering failed with error: $err, budget: $budget, costs: $costs, log: $log")
        }
    }

    test("lowering constr/deconstr tuple3") {
        import scalus.prelude.*
        val sir = compile {
            val input = (BigInt(1), BigInt(2), BigInt(3))
            val output = input match
                case (a, b, c) =>
                    require(c == BigInt(3), "Expected c to be 3")
                    (a, b, c + 1)

            // require(x1._1 == BigInt(1))
            // require(x1._2 == BigInt(2))
            // require(x1._3 == BigInt(4))
            output._3
            // val x2 = x1 match
            //    case (a, b, c) =>
            //        // require(c == BigInt(4))
            //        c
            // x2
        }
        val lowering = SirToUplcV3Lowering(sir, generateErrorTraces = true)
        val term = lowering.lower()
        val lv = lowering.lastLoweredValue.getOrElse {
            this.fail("No lowered value found")
        }
        // println(s"lv=${lv.show}")
        val result = term.evaluateDebug
        result match {
            case Result.Success(term, budget, costs, log) =>
                assert(term == Term.Const(Constant.Integer(BigInt(4))))
            case Result.Failure(err, budget, costs, log) =>
                this.fail(
                  s"Lowering failed with error: $err,  log: $log"
                )
        }
    }

    test("lowering unmapping to builtin list") {
        val sir = compile {
            import scalus.builtin.Data.toData
            import scalus.prelude.*
            val mapData = SortedMap.unsafeFromList(List(("a", BigInt(1)), ("b", BigInt(2)))).toData
            val list_138 = scalus.builtin.Builtins.unMapData(mapData)
            val h1 = list_138.head
            h1.fst
        }
        // println(sir.show)
        val lowering = SirToUplcV3Lowering(sir, generateErrorTraces = true)
        val term = lowering.lower()
        val lv = lowering.lastLoweredValue.getOrElse {
            this.fail("No lowered value found")
        }
        // println(lv.show)
        // println(s"lv.sirType = ${lv.sirType.show}")
        // println(s"lv.representation=${lv.representation.show}")
        val result = term.evaluateDebug
        result match {
            case Result.Success(term, budget, costs, log) =>
                val dataString = Data.toData("a")
                assert(term == Term.Const(Constant.Data(dataString)))
            case Result.Failure(err, budget, costs, log) =>
                this.fail(
                  s"Lowering failed with error: $err,  log: $log"
                )
        }
    }

    test("lowering pairlist repr") {
        import scalus.*
        import scalus.prelude.*

        val sir = scalus.compiler.compile {
            def go(lst: List[(BigInt, BigInt)]): List[(BigInt, BigInt)] =
                lst match
                    case List.Nil =>
                        List.Cons((BigInt(1), BigInt(1)), List.Nil)
                    case List.Cons(pair, tail) =>
                        List.Cons(pair, go(tail))

            SortedMap.unsafeFromList(go(List.empty))

        }

        // val lw = sir.toLoweredValue()
        val uplc = sir.toUplc()
        val result = uplc.evaluateDebug
        assert(result.isSuccess)

    }

}
