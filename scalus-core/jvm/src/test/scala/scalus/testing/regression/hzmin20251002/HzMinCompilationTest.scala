package scalus.testing.regression.hzmin20251002

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.compile

class HzMinCompilationTest extends AnyFunSuite {

    test("compile scriot should be lowered correctly") {
        // val sir1 = compile {
        //    HzMinValidator.validate
        // }

        import scalus.uplc.builtin.ByteString
        import scalus.cardano.onchain.plutus.v1.Value
        import scalus.cardano.onchain.plutus.prelude.{List, SortedMap}

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
