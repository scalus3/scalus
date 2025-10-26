package scalus.uplc.eval
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
            case l: List[?] =>
                // List element type is lost at runtime, assume Data for now
                // This is only used for debugging/serialization
                val constElems = l.map(anyUplcConstant)
                Constant.List(DefaultUni.Data, constElems)
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
                v.map(uplcToJitAny)
            case _ => throw new IllegalArgumentException(s"Unsupported Constant type: ${in.tpe}")
    }

    final def unConstrData(d: Data): BuiltinPair[BigInt, List[Data]] = {
        d match {
            case Data.Constr(index, fields) =>
                BuiltinPair(BigInt(index), fields)
            case _ =>
                throw new IllegalArgumentException("Data is not a Constr")
        }
    }

    final def unListData(d: Data): List[Data] = d match
        case Data.List(values) => values
        case _                 => throw new Exception(s"not a list but $d")

}
