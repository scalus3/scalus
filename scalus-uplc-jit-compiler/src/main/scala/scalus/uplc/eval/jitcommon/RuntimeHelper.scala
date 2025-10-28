package scalus.uplc.eval.jitcommon

import scalus.uplc.*
import scalus.builtin.*

object RuntimeHelper {

    def anyUplcConstant(in: Any): Constant = {
        in match
            case i: Int         => Constant.Integer(i)
            case s: String      => Constant.String(s)
            case b: Boolean     => Constant.Bool(b)
            case bs: ByteString => Constant.ByteString(bs)
            case d: Data        => Constant.Data(d)
            case p: BuiltinPair[?, ?] =>
                Constant.Pair(anyUplcConstant(p.fst), anyUplcConstant(p.snd))
            case p: Tuple2[?, ?] => Constant.Pair(anyUplcConstant(p._1), anyUplcConstant(p._2))
            case l: List[?]      =>
                // Lists in JIT are plain List[Any] - element type is lost at runtime
                // For serialization, we assume Data type (most common case)
                Constant.List(DefaultUni.Data, l.map(anyUplcConstant))
            case _ => throw new IllegalArgumentException(s"Unsupported type: ${in.getClass}")
    }

    def uplcToJitAny(in: Constant): Any = {
        in match
            case Constant.Integer(i)     => i
            case Constant.String(s)      => s
            case Constant.Bool(b)        => b
            case Constant.ByteString(bs) => bs
            case Constant.Data(d)        => d
            case Constant.Pair(fst, snd) =>
                BuiltinPair(uplcToJitAny(fst), uplcToJitAny(snd))
            case Constant.List(elemType, v) =>
                // Return plain List[Any] - element type not needed at runtime
                v.map(uplcToJitAny)
            case _ => throw new IllegalArgumentException(s"Unsupported Constant type: ${in.tpe}")
    }

    final def unConstrData(d: Data): BuiltinPair[BigInt, List[Data]] = {
        d match {
            case Data.Constr(index, fields) =>
                BuiltinPair(
                  BigInt(index),
                  fields // Plain List[Data]
                )
            case _ =>
                throw new IllegalArgumentException("Data is not a Constr")
        }
    }

    final def unListData(d: Data): List[Data] = d match
        case Data.List(values) => values // Plain List[Data]
        case _                 => throw new Exception(s"not a list but $d")

}
