package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.SIRType.BuiltinPair
import scalus.uplc.DefaultUni
import scalus.uplc.TypeScheme.{App, Type}

/** A type application in a type scheme follows the rules of [[SIRType.fromDefaultUni]]. */
class SIRTypeFromUplcTypeSchemeTest extends AnyFunSuite {

    test("a pair type applied to its first component keeps that component first") {
        // `pair integer` waits for the second component: [B] =>> pair(integer, B)
        val pairOfInteger =
            SIRType.fromUplcTypeScheme(App(Type(DefaultUni.ProtoPair), Type(DefaultUni.Integer)))
        assert(
          SIRType.typeApply(pairOfInteger, List(SIRType.ByteString)) ==
              BuiltinPair(SIRType.Integer, SIRType.ByteString)
        )
    }

    test("a type that takes no argument cannot be applied to a type") {
        val error = intercept[IllegalArgumentException] {
            SIRType.fromUplcTypeScheme(App(Type(DefaultUni.Integer), Type(DefaultUni.Bool)))
        }
        assert(error.getMessage.contains("Int"))
    }

    test("a list type cannot be applied to a second type") {
        val listOfInteger = App(Type(DefaultUni.ProtoList), Type(DefaultUni.Integer))
        val error = intercept[IllegalArgumentException] {
            SIRType.fromUplcTypeScheme(App(listOfInteger, Type(DefaultUni.Bool)))
        }
        assert(error.getMessage.contains("BuiltinList"))
    }

    test("a list type cannot take a type constructor as its element type") {
        // `list list`: the inner `list` has no element type
        val error = intercept[IllegalArgumentException] {
            SIRType.fromUplcTypeScheme(App(Type(DefaultUni.ProtoList), Type(DefaultUni.ProtoList)))
        }
        assert(error.getMessage.contains("BuiltinList"))
    }
}
