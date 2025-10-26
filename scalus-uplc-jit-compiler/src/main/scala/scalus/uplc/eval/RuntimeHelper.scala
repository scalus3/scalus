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
            case l: ListJitRepr  => l.toConstant
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
            case l @ Constant.List(elemType, v) =>
                ListJitRepr.fromConstantList(l)
            case _ => throw new IllegalArgumentException(s"Unsupported Constant type: ${in.tpe}")
    }

    final def unConstrData(d: Data): BuiltinPair[BigInt, ListJitRepr] = {
        d match {
            case Data.Constr(index, fields) =>
                BuiltinPair(
                  BigInt(index),
                  ListJitRepr(DefaultUni.Data, fields)
                )
            case _ =>
                throw new IllegalArgumentException("Data is not a Constr")
        }
    }

    final def unListData(d: Data): ListJitRepr = d match
        case Data.List(values) => ListJitRepr(DefaultUni.Data, values)
        case _                 => throw new Exception(s"not a list but $d")

    final def binaryOp[A <: Matchable, B <: Matchable](
        x: Any,
        y: Any,
        f: (A, B) => Any
    ): Any = {
        x match
            case xcc: CallCCTrampoline =>
                y match
                    case ycc: CallCCTrampoline =>
                        xcc.map(x1 => ycc.map(y1 => f(x1.asInstanceOf[A], y1.asInstanceOf[B])))
                    case yv: B =>
                        xcc.map(x1 => f(x1.asInstanceOf[A], yv))
                    case _ =>
                        throw IllegalStateException(s"unexpected type for y: ${y.getClass}")
            case xv: A =>
                y match
                    case ycc: CallCCTrampoline =>
                        ycc.map(y1 => f(xv, y1.asInstanceOf[B]))
                    case yv: B =>
                        f(xv, yv)
                    case _ =>
                        throw IllegalStateException(s"unexpected type for y: ${y.getClass}")
            case _ =>
                throw IllegalStateException(s"unexpected type for x: ${x.getClass}")
    }

    final def integerOp(x: Any, y: Any, f: (BigInt, BigInt) => Any): Any = {
        binaryOp[BigInt, BigInt](x, y, f)
    }

    final def unaryOp[A <: Matchable](x: Any, f: A => Any): Any = {
        x match
            case xcc: CallCCTrampoline =>
                xcc.map(x1 => f(x1.asInstanceOf[A]))
            case xv: A =>
                f(xv)
            case _ =>
                throw IllegalStateException(s"unexpected type: ${x.getClass}")
    }

    final def unwrapList(xs: List[Any]): Any = {
        def go(remaining: List[Any], acc: List[Any]): Any = {
            remaining match
                case Nil => acc.reverse
                case head :: tail =>
                    head match
                        case cc: CallCCTrampoline =>
                            cc.map(unwrapped => go(unwrapped :: tail, acc))
                        case v =>
                            go(tail, v :: acc)
        }
        go(xs, Nil)
    }

}
