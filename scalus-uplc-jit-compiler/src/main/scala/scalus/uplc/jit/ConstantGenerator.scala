package scalus.uplc.jit

import scalus.uplc.builtin.*
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.cardano.onchain.plutus.prelude.List.toScalaList
import scalus.uplc.Constant

import scala.quoted.*

/** Generates Scala 3 quoted expressions for UPLC constants.
  *
  * Shared utility for all JIT compilers to convert UPLC Constant values into quoted Expr
  * representations for code generation.
  */
object ConstantGenerator {

    /** Converts a UPLC Constant to a quoted expression.
      *
      * @param const
      *   The constant to convert
      * @return
      *   Quoted expression representing the constant value
      */
    def constantToExpr(const: Constant)(using Quotes): Expr[Any] = {
        given ByteStringToExpr: ToExpr[ByteString] with {
            def apply(x: ByteString)(using Quotes): Expr[ByteString] =
                '{ ByteString.fromArray(${ Expr(x.bytes) }) }
        }

        given DataToExpr: ToExpr[Data] with {
            def apply(x: Data)(using Quotes): Expr[Data] = x match
                case Data.Constr(tag, args) =>
                    val tagExpr = Expr(tag)
                    val argsExpr = Expr.ofList(args.toScalaList.map(apply))
                    '{ Data.Constr($tagExpr, PList.from($argsExpr)) }
                case Data.List(value) =>
                    val valueExpr = Expr.ofList(value.toScalaList.map(apply))
                    '{ Data.List(PList.from($valueExpr)) }
                case Data.Map(values) =>
                    val argsListOfExprTuple = values.toScalaList.map { case (k, v) =>
                        Expr.ofTuple(apply(k), apply(v))
                    }
                    val argsExpr = Expr.ofList(argsListOfExprTuple)
                    '{ Data.Map(PList.from($argsExpr)) }
                case Data.I(value) => '{ Data.I(${ Expr(value) }) }
                case Data.B(value) => '{ Data.B(${ Expr(value) }) }
        }

        const match
            case Constant.Integer(value)        => Expr(value)
            case Constant.ByteString(value)     => Expr(value)
            case Constant.String(value)         => Expr(value)
            case Constant.Unit                  => '{ () }
            case Constant.Bool(value)           => Expr(value)
            case Constant.Data(value)           => Expr(value)
            case Constant.List(elemType, value) =>
                // Lists are represented as plain Scala List[Any] at runtime
                // No need to track element type - only used for serialization
                Expr.ofList(value.map(constantToExpr))
            case Constant.Array(elemType, value) =>
                // Arrays are represented as Vector[Any] at runtime
                val elements = value.map(constantToExpr)
                '{ Vector(${ Varargs(elements) }*) }
            case Constant.Pair(a, b) =>
                '{ BuiltinPair(${ constantToExpr(a) }, ${ constantToExpr(b) }) }
            case Constant.BLS12_381_G1_Element(value) =>
                '{ BLS12_381_G1_Element(${ Expr(value.toCompressedByteString) }) }
            case Constant.BLS12_381_G2_Element(value) =>
                '{ BLS12_381_G2_Element(${ Expr(value.toCompressedByteString) }) }
            case Constant.BLS12_381_MlResult(value) =>
                sys.error("BLS12_381_MlResult values cannot be serialized as constants in UPLC")
            case Constant.BuiltinValue(value) =>
                val dataExpr: Expr[Data] = Expr(BuiltinValue.toData(value))
                '{ BuiltinValue.fromData($dataExpr) }
    }
}
