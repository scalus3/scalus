package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.serialization.flat.*

import scala.language.implicitConversions

class SirToExprTest extends AnyFunSuite {

    val ae = AnnotationsDecl.empty

    test("serialize/deserialize List") {
        val sir = SIR.Const(
          scalus.uplc.Constant.List(
            scalus.uplc.DefaultUni.Integer,
            List(
              scalus.uplc.Constant.Integer(1),
              scalus.uplc.Constant.Integer(2),
              scalus.uplc.Constant.Integer(3)
            )
          ),
          SIRType.List(SIRType.Integer),
          ae
        )
        val len = ToExprHSSIRFlat.bitSize(sir)
        val encoded = EncoderState(len / 8 + 1)
        ToExprHSSIRFlat.encode(sir, encoded)
        val decoded = ToExprHSSIRFlat.decode(DecoderState(encoded.buffer))
        assert(sir ~=~ decoded)
    }

    test("serialize/deserialize ListType") {
        val tp = SIRType.List(SIRType.Integer)
        val bitSize = ToExprHSSIRTypeFlat.bitSize(tp)
        val encoded = EncoderState(bitSize / 8 + 1)
        ToExprHSSIRTypeFlat.encode(tp, encoded)
        val decoded = ToExprHSSIRTypeFlat.decode(DecoderState(encoded.buffer)).asInstanceOf[SIRType]
        assert(tp ~=~ decoded)
    }

    test("list type of builtin fun is serialized without unfilled proxies") {
        val fun = SIRBuiltins.tailList
        val tp = fun.tp
        val bitSize = ToExprHSSIRTypeFlat.bitSize(tp)
        val encoded = EncoderState(bitSize / 8 + 1)
        ToExprHSSIRTypeFlat.encode(tp, encoded)
        val decoded = ToExprHSSIRTypeFlat.decode(DecoderState(encoded.buffer)).asInstanceOf[SIRType]
        assert(SIRType.checkAllProxiesFilled(decoded))
        assert(tp ~=~ decoded)
    }

    test("type of builtin fun application is serialized without unfilled proxies") {
        val fun = SIRBuiltins.tailList
        val arg: AnnotatedSIR = SIR.Const(
          scalus.uplc.Constant
              .List(scalus.uplc.DefaultUni.Integer, List(scalus.uplc.Constant.Integer(1))),
          SIRType.BuiltinList(SIRType.Integer),
          ae
        )
        val fun1 = SIR.Apply(
          fun,
          arg,
          SIRType.calculateApplyType(fun.tp, arg.tp, Map.empty),
          AnnotationsDecl.empty
        )
        val tp = fun1.tp
        val bitSize = ToExprHSSIRTypeFlat.bitSize(tp)
        val encoded = EncoderState(bitSize / 8 + 1)
        ToExprHSSIRTypeFlat.encode(tp, encoded)
        val decoded = ToExprHSSIRTypeFlat.decode(DecoderState(encoded.buffer)).asInstanceOf[SIRType]
        assert(SIRType.checkAllProxiesFilled(decoded))
        assert(tp ~=~ decoded)
    }

    test("builtin fun application is serialized without unfilled proxies") {
        val fun0 = SIRBuiltins.tailList
        val arg: AnnotatedSIR = SIR.Const(
          scalus.uplc.Constant
              .List(scalus.uplc.DefaultUni.Integer, List(scalus.uplc.Constant.Integer(1))),
          SIRType.BuiltinList(SIRType.Integer),
          ae
        )
        val fun1 = SIR.Apply(
          fun0,
          arg,
          SIRType.calculateApplyType(fun0.tp, arg.tp, Map.empty),
          AnnotationsDecl.empty
        )
        val bitSize = ToExprHSSIRFlat.bitSize(fun1)
        val encoded = EncoderState(bitSize / 8 + 1)
        ToExprHSSIRFlat.encode(fun1, encoded)
        val decoded = ToExprHSSIRFlat.decode(DecoderState(encoded.buffer))
        assert(fun1 ~=~ decoded)
    }

}
