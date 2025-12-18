package scalus.uplc.jit

import scalus.uplc.DefaultUni

import scala.quoted.*

given ToExpr[DefaultUni] with {
    def apply(u: DefaultUni)(using Quotes): Expr[DefaultUni] = u match {
        case DefaultUni.Integer              => '{ DefaultUni.Integer }
        case DefaultUni.ByteString           => '{ DefaultUni.ByteString }
        case DefaultUni.String               => '{ DefaultUni.String }
        case DefaultUni.Unit                 => '{ DefaultUni.Unit }
        case DefaultUni.Bool                 => '{ DefaultUni.Bool }
        case DefaultUni.BLS12_381_G1_Element => '{ DefaultUni.BLS12_381_G1_Element }
        case DefaultUni.BLS12_381_G2_Element => '{ DefaultUni.BLS12_381_G2_Element }
        case DefaultUni.BLS12_381_MlResult   => '{ DefaultUni.BLS12_381_MlResult }
        case DefaultUni.BuiltinValue         => '{ DefaultUni.BuiltinValue }
        case DefaultUni.Data                 => '{ DefaultUni.Data }
        case DefaultUni.ProtoList            => '{ DefaultUni.ProtoList }
        case DefaultUni.ProtoPair            => '{ DefaultUni.ProtoPair }
        case DefaultUni.ProtoArray           => '{ DefaultUni.ProtoArray }
        case DefaultUni.Apply(f, arg) =>
            val fExpr = apply(f)
            val argExpr = apply(arg)
            '{ DefaultUni.Apply($fExpr, $argExpr) }
    }
}
