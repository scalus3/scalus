package scalus.testing.regression.hzmin20251002

import scalus.*
import scalus.Compiler.compile
import org.scalatest.funsuite.AnyFunSuite

class HzMinCompilationTest extends AnyFunSuite {

    test("compile scriot should be lowered correctly") {
        // val sir1 = compile {
        //    HzMinValidator.validate
        // }

        import scalus.prelude.List
        import scalus.prelude.SortedMap
        import scalus.ledger.api.v1.Value
        import scalus.builtin.ByteString

        val sir1 = compile { (value: Value) =>
            val headMp = ByteString.fromHex("03")
            value.toSortedMap
                .get(headMp)
                .getOrElse(SortedMap.empty)
                .toList match
                case List.Cons((tokenName, amount), none) => BigInt(1)
                case _                                    => BigInt(2)
        }

        // println(sir1.pretty.render(100))

        val lc = sir1.toLoweredValue()
        // println(lc.pretty.render(100))

        val uplc = sir1.toUplc(generateErrorTraces = true)
        // println(uplc.pretty.render(100))

        assert(sir1 != null)

    }

}
