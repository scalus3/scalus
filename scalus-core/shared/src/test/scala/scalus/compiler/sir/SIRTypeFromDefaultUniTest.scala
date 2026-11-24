package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.SIRType.BuiltinPair
import scalus.uplc.DefaultUni

class SIRTypeFromDefaultUniTest extends AnyFunSuite {

    test("a pair type applied to its first component keeps that component first") {
        // `pair integer` waits for the second component: [B] =>> pair(integer, B)
        val pairOfInteger =
            SIRType.fromDefaultUni(DefaultUni.Apply(DefaultUni.ProtoPair, DefaultUni.Integer))
        assert(
          SIRType.typeApply(pairOfInteger, List(SIRType.ByteString)) ==
              BuiltinPair(SIRType.Integer, SIRType.ByteString)
        )
    }

    test("a type that takes no argument cannot be applied to a type") {
        val error = intercept[IllegalArgumentException] {
            SIRType.fromDefaultUni(DefaultUni.Apply(DefaultUni.Integer, DefaultUni.Bool))
        }
        assert(error.getMessage.contains("Integer"))
    }

    test("a list type cannot be applied to a second type") {
        val listOfInteger = DefaultUni.Apply(DefaultUni.ProtoList, DefaultUni.Integer)
        val error = intercept[IllegalArgumentException] {
            SIRType.fromDefaultUni(DefaultUni.Apply(listOfInteger, DefaultUni.Bool))
        }
        assert(error.getMessage.contains("Apply(ProtoList,Integer)"))
    }

    test("a list type cannot take a type constructor as its element type") {
        // `list list`: the inner `list` has no element type
        val error = intercept[IllegalArgumentException] {
            SIRType.fromDefaultUni(DefaultUni.Apply(DefaultUni.ProtoList, DefaultUni.ProtoList))
        }
        assert(error.getMessage.contains("ProtoList"))
    }

    test("a pair type cannot take a partly applied pair as a component") {
        val pairOfInteger = DefaultUni.Apply(DefaultUni.ProtoPair, DefaultUni.Integer)
        val error = intercept[IllegalArgumentException] {
            SIRType.fromDefaultUni(DefaultUni.Pair(pairOfInteger, DefaultUni.Bool))
        }
        assert(error.getMessage.contains("Apply(ProtoPair,Integer)"))
    }
}
