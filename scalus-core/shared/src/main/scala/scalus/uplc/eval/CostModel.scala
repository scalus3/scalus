package scalus.uplc.eval

import scalus.cardano.ledger.ExUnits
import scalus.uplc.*
import upickle.default.*

import scala.compiletime.erasedValue

// Capture the Long ReadWriter before the opaque type is defined
private[eval] val longReadWriter: ReadWriter[Long] = summon[ReadWriter[Long]]

/** Saturating integer type for cost calculations.
  *
  * Matches Plutus SatInt behavior: arithmetic operations saturate to Long.MaxValue on overflow
  * instead of wrapping around. This ensures cost calculations never produce negative or wrapped
  * values.
  */
opaque type CostingInteger = Long

object CostingInteger {
    inline def apply(value: Long): CostingInteger = value

    def fromBigInt(value: BigInt): CostingInteger =
        if value.isValidLong then value.toLong
        else if value > 0 then Long.MaxValue
        else Long.MinValue

    given ReadWriter[CostingInteger] = longReadWriter.bimap(
      (c: CostingInteger) => c: Long,
      (l: Long) => l: CostingInteger
    )

    /** Saturating addition - returns Long.MaxValue on positive overflow, Long.MinValue on negative
      * overflow
      */
    def satPlus(a: Long, b: Long): Long = {
        val r = a + b
        // Overflow if both have same sign but result has different sign
        if (a ^ r) < 0 && (a ^ b) >= 0 then if a >= 0 then Long.MaxValue else Long.MinValue
        else r
    }

    /** Saturating subtraction */
    def satMinus(a: Long, b: Long): Long = {
        val r = a - b
        // Overflow if operands have different signs and result has different sign from a
        if (a ^ b) < 0 && (a ^ r) < 0 then if a >= 0 then Long.MaxValue else Long.MinValue
        else r
    }

    /** Saturating multiplication */
    def satMul(a: Long, b: Long): Long = {
        if a == 0 || b == 0 then 0L
        else
            val r = a * b
            // Check for overflow: result / a should equal b
            if r / a != b then if (a > 0) == (b > 0) then Long.MaxValue else Long.MinValue
            else r
    }

    extension (a: CostingInteger) {
        inline def toLong: Long = a

        inline def +(b: CostingInteger): CostingInteger = satPlus(a, b)
        inline def -(b: CostingInteger): CostingInteger = satMinus(a, b)
        inline def *(b: CostingInteger): CostingInteger = satMul(a, b)
        inline def /(b: CostingInteger): CostingInteger = a / b

        inline def min(b: CostingInteger): CostingInteger = Math.min(a, b)
        inline def max(b: CostingInteger): CostingInteger = Math.max(a, b)

        inline def >(b: CostingInteger): Boolean = a > b
        inline def >=(b: CostingInteger): Boolean = a >= b
        inline def <(b: CostingInteger): Boolean = a < b
        inline def <=(b: CostingInteger): Boolean = a <= b
        inline def ==(b: CostingInteger): Boolean = a == b
    }
}

type Intercept = CostingInteger
type Slope = CostingInteger

case class OneVariableLinearFunction(intercept: Intercept, slope: Slope) derives ReadWriter {
    def apply(arg: CostingInteger): CostingInteger = {
        intercept + arg * slope
    }
}

case class SubtractedSizesLinearFunction(
    intercept: Intercept,
    slope: Slope,
    minimum: CostingInteger
) derives ReadWriter {
    def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger = {
        intercept + slope * (arg1 - arg2).max(minimum)
    }
}

case class TwoVariableLinearFunction(intercept: Intercept, slope1: Slope, slope2: Slope)
    derives ReadWriter {
    def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger = {
        intercept + arg1 * slope1 + arg2 * slope2
    }
}

case class TwoVariableWithInteractionFunction(
    c00: CostingInteger,
    c10: CostingInteger,
    c01: CostingInteger,
    c11: CostingInteger
) derives ReadWriter {
    // cost = c10*x + c01*y + c11*(x*y) + c00
    def apply(x: CostingInteger, y: CostingInteger): CostingInteger =
        c10 * x + c01 * y + c11 * (x * y) + c00
}

case class OneVariableQuadraticFunction(
    c0: CostingInteger,
    c1: CostingInteger,
    c2: CostingInteger
) derives ReadWriter {
    def apply(x: CostingInteger): CostingInteger = {
        c0 + c1 * x + c2 * x * x
    }
}

/** c00 + c10*x + c01*y + c20*x^2 + c11*c*y + c02*y^2
  *
  * @note
  *   Minimum values for two-variable quadratic costing functions. Unlike most of our other costing
  *   functions our use cases for two-variable quadratic costing functions may require one or more
  *   negative coefficients, so there's a danger that we could return a negative cost. This is
  *   unlikely, but we make certain that it never happens by returning a result that is at never
  *   smaller than a minimum value that is stored along with the coefficients of the function.
  *
  * @param minimum
  * @param c00
  * @param c10
  * @param c01
  * @param c20
  * @param c11
  * @param c02
  */
case class TwoVariableQuadraticFunction(
    minimum: CostingInteger,
    c00: CostingInteger,
    c10: CostingInteger,
    c01: CostingInteger,
    c20: CostingInteger,
    c11: CostingInteger,
    c02: CostingInteger
) derives ReadWriter {
    def apply(x: CostingInteger, y: CostingInteger): CostingInteger = {
        val result = c00 + c10 * x + c01 * y + c20 * x * x + c11 * x * y + c02 * y * y
        result.max(minimum)
    }
}

/** Costing function for modular exponentiation (expModInteger builtin).
  *
  * The cost formula is: if aa <= mm then cost0 else cost0 + cost0/2 where cost0 = c00 + c11 * ee *
  * mm + c12 * ee * mm * mm
  *
  * @param coefficient00
  *   constant coefficient
  * @param coefficient11
  *   coefficient for e * m term
  * @param coefficient12
  *   coefficient for e * m^2 term
  */
case class ExpModCostingFunction(
    coefficient00: CostingInteger,
    coefficient11: CostingInteger,
    coefficient12: CostingInteger
) derives ReadWriter {
    def apply(aa: CostingInteger, ee: CostingInteger, mm: CostingInteger): CostingInteger = {
        val cost0 = coefficient00 + coefficient11 * ee * mm + coefficient12 * ee * mm * mm
        if aa <= mm then cost0 else cost0 + cost0 / CostingInteger(2L)
    }
}

case class ConstantOrLinear(constant: CostingInteger, intercept: Intercept, slope: Slope)
    derives ReadWriter {
    def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger = {
        if arg1 == arg2 then intercept + arg1 * slope else constant
    }
}

case class ConstantOrOneArgument(constant: CostingInteger, model: OneArgument) derives ReadWriter

case class ConstantOrTwoArguments(constant: CostingInteger, model: TwoArguments) derives ReadWriter

sealed trait CostModel {
    def calculateCost(args: Seq[CostingInteger]): CostingInteger
}

sealed trait BaseConstantCostModel extends CostModel {
    def cost: CostingInteger
}

trait OneArgument extends CostModel {
    def apply(arg: CostingInteger): CostingInteger
    def calculateCost(args: Seq[CostingInteger]) = apply(args.head)
}

object OneArgument:
    case class ConstantCost(cost: CostingInteger) extends OneArgument with BaseConstantCostModel {
        def apply(arg: CostingInteger): CostingInteger = cost
    }

    case class LinearInX(costFun: OneVariableLinearFunction) extends OneArgument {
        def apply(arg: CostingInteger): CostingInteger =
            costFun(arg)
    }

    case class QuadraticInX(costFun: OneVariableQuadraticFunction) extends OneArgument {
        def apply(arg: CostingInteger): CostingInteger = costFun(arg)
    }

    given ReadWriter[OneArgument] = readwriter[ujson.Value].bimap(
      {
          case ConstantCost(cost) =>
              ujson.Obj("type" -> "constant_cost", "arguments" -> cost.toLong)
          case LinearInX(cost) =>
              ujson.Obj("type" -> "linear_in_x", "arguments" -> writeJs(cost))
          case QuadraticInX(cost) =>
              ujson.Obj("type" -> "quadratic_in_x", "arguments" -> writeJs(cost))
      },
      json => {
          json.obj("type").str match
              case "constant_cost" =>
                  ConstantCost(CostingInteger(json.obj("arguments").num.toLong))
              case "linear_in_x" =>
                  LinearInX(read[OneVariableLinearFunction](json.obj("arguments")))
              case "quadratic_in_x" =>
                  QuadraticInX(read[OneVariableQuadraticFunction](json.obj("arguments")))
              case other => throw new RuntimeException(s"Unexpected type ${other}")
      }
    )

trait TwoArguments extends CostModel {
    def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger
    def calculateCost(args: Seq[CostingInteger]) = apply(args(0), args(1))
}

object TwoArguments {
    case class ConstantCost(cost: CostingInteger) extends TwoArguments with BaseConstantCostModel {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger = cost
    }

    case class LinearInX(costFun: OneVariableLinearFunction) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            costFun(arg1)
    }

    case class LinearInY(costFun: OneVariableLinearFunction) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            costFun(arg2)
    }

    case class LinearInXAndY(cost: TwoVariableLinearFunction) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            cost.intercept + arg1 * cost.slope1 + arg2 * cost.slope2
    }

    case class AddedSizes(cost: OneVariableLinearFunction) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            cost(arg1 + arg2)
    }

    case class SubtractedSizes(cost: SubtractedSizesLinearFunction) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            cost(arg1, arg2)
    }

    case class MultipliedSizes(cost: OneVariableLinearFunction) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            cost(arg1 * arg2)
    }

    case class MinSize(cost: OneVariableLinearFunction) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            cost(arg1.min(arg2))
    }

    case class MaxSize(cost: OneVariableLinearFunction) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            cost(arg1.max(arg2))
    }

    case class LinearOnDiagonal(cost: ConstantOrLinear) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            cost(arg1, arg2)
    }
    case class ConstBelowDiagonal(cost: ConstantOrTwoArguments) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            if arg1 > arg2 then cost.constant else cost.model(arg1, arg2)
    }
    case class ConstAboveDiagonal(cost: ConstantOrTwoArguments) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            if arg1 < arg2 then cost.constant else cost.model(arg1, arg2)
    }

    case class ConstOffDiagonal(cost: ConstantOrOneArgument) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            if arg1 != arg2 then cost.constant else cost.model(arg1)
    }

    case class QuadraticInY(cost: OneVariableQuadraticFunction) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            cost(arg2)
    }

    case class QuadraticInXAndY(cost: TwoVariableQuadraticFunction) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            cost(arg1, arg2)
    }

    case class WithInteractionInXAndY(cost: TwoVariableWithInteractionFunction)
        extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger = cost(arg1, arg2)
    }

    given ReadWriter[TwoArguments] = readwriter[ujson.Value].bimap(
      {
          case ConstantCost(cost) =>
              ujson.Obj("type" -> "constant_cost", "arguments" -> cost.toLong)
          case LinearInX(costFun) =>
              ujson.Obj("type" -> "linear_in_x", "arguments" -> writeJs(costFun))
          case LinearInY(costFun) =>
              ujson.Obj("type" -> "linear_in_y", "arguments" -> writeJs(costFun))
          case LinearInXAndY(cost) =>
              ujson.Obj("type" -> "linear_in_x_and_y", "arguments" -> writeJs(cost))
          case AddedSizes(cost) =>
              ujson.Obj("type" -> "added_sizes", "arguments" -> writeJs(cost))
          case SubtractedSizes(cost) =>
              ujson.Obj("type" -> "subtracted_sizes", "arguments" -> writeJs(cost))
          case MultipliedSizes(cost) =>
              ujson.Obj("type" -> "multiplied_sizes", "arguments" -> writeJs(cost))
          case MinSize(cost) =>
              ujson.Obj("type" -> "min_size", "arguments" -> writeJs(cost))
          case MaxSize(cost) =>
              ujson.Obj("type" -> "max_size", "arguments" -> writeJs(cost))
          case LinearOnDiagonal(cost) =>
              ujson.Obj("type" -> "linear_on_diagonal", "arguments" -> writeJs(cost))
          case ConstAboveDiagonal(cost) =>
              ujson.Obj("type" -> "const_above_diagonal", "arguments" -> writeJs(cost))
          case ConstBelowDiagonal(cost) =>
              ujson.Obj("type" -> "const_below_diagonal", "arguments" -> writeJs(cost))
          case QuadraticInY(cost) =>
              ujson.Obj("type" -> "quadratic_in_y", "arguments" -> writeJs(cost))
          case QuadraticInXAndY(cost) =>
              ujson.Obj("type" -> "quadratic_in_x_and_y", "arguments" -> writeJs(cost))
          case ConstOffDiagonal(cost) =>
              ujson.Obj("type" -> "const_off_diagonal", "arguments" -> writeJs(cost))
          case WithInteractionInXAndY(cost) =>
              ujson.Obj("type" -> "with_interaction_in_x_and_y", "arguments" -> writeJs(cost))
      },
      json => {
          json.obj("type").str match
              case "constant_cost" => ConstantCost(CostingInteger(json.obj("arguments").num.toLong))
              case "linear_in_x" =>
                  LinearInX(read[OneVariableLinearFunction](json.obj("arguments")))
              case "linear_in_y" =>
                  LinearInY(read[OneVariableLinearFunction](json.obj("arguments")))
              case "linear_in_x_and_y" =>
                  LinearInXAndY(read[TwoVariableLinearFunction](json.obj("arguments")))
              case "quadratic_in_y" =>
                  QuadraticInY(read[OneVariableQuadraticFunction](json.obj("arguments")))
              case "quadratic_in_x_and_y" =>
                  QuadraticInXAndY(read[TwoVariableQuadraticFunction](json.obj("arguments")))
              case "added_sizes" =>
                  AddedSizes(read[OneVariableLinearFunction](json.obj("arguments")))
              case "subtracted_sizes" =>
                  SubtractedSizes(read[SubtractedSizesLinearFunction](json.obj("arguments")))
              case "multiplied_sizes" =>
                  MultipliedSizes(read[OneVariableLinearFunction](json.obj("arguments")))
              case "min_size" =>
                  MinSize(read[OneVariableLinearFunction](json.obj("arguments")))
              case "max_size" =>
                  MaxSize(read[OneVariableLinearFunction](json.obj("arguments")))
              case "linear_on_diagonal" =>
                  LinearOnDiagonal(read[ConstantOrLinear](json.obj("arguments")))
              case "const_above_diagonal" =>
                  ConstAboveDiagonal(read[ConstantOrTwoArguments](json.obj("arguments")))
              case "const_below_diagonal" =>
                  ConstBelowDiagonal(read[ConstantOrTwoArguments](json.obj("arguments")))
              case "const_off_diagonal" =>
                  ConstOffDiagonal(read[ConstantOrOneArgument](json.obj("arguments")))
              case "with_interaction_in_x_and_y" =>
                  WithInteractionInXAndY(
                    read[TwoVariableWithInteractionFunction](json.obj("arguments"))
                  )
              case other => throw new RuntimeException(s"Unexpected type ${other}")
      }
    )
}

trait ThreeArguments extends CostModel {
    def apply(arg1: CostingInteger, arg2: CostingInteger, arg3: CostingInteger): CostingInteger
    def calculateCost(args: Seq[CostingInteger]) = apply(args(0), args(1), args(2))
}

object ThreeArguments {
    case class ConstantCost(cost: CostingInteger)
        extends ThreeArguments
        with BaseConstantCostModel {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger
        ): CostingInteger = cost
    }

    case class LinearInX(costFun: OneVariableLinearFunction) extends ThreeArguments {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger
        ): CostingInteger =
            costFun(arg1)
    }

    case class LinearInY(costFun: OneVariableLinearFunction) extends ThreeArguments {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger
        ): CostingInteger =
            costFun(arg2)
    }

    case class LinearInZ(costFun: OneVariableLinearFunction) extends ThreeArguments {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger
        ): CostingInteger =
            costFun(arg3)
    }

    case class QuadraticInZ(costFun: OneVariableQuadraticFunction) extends ThreeArguments {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger
        ): CostingInteger =
            costFun(arg3)
    }

    case class LiteralInYOrLinearInZ(costFun: OneVariableLinearFunction) extends ThreeArguments {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger
        ): CostingInteger =
            if arg2 == CostingInteger(0L) then costFun(arg3) else arg2
    }

    case class LinearInMaxYZ(costFun: OneVariableLinearFunction) extends ThreeArguments {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger
        ): CostingInteger =
            costFun(arg2.max(arg3))
    }

    case class LinearInYAndZ(costFun: TwoVariableLinearFunction) extends ThreeArguments {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger
        ): CostingInteger =
            costFun(arg2, arg3)
    }

    /** ExpMod cost function for modular exponentiation.
      *
      * Cost formula: if a <= m then cost0 else cost0 + cost0/2 where cost0 = c00 + c11 * e * m +
      * c12 \* e * m^2
      */
    case class ExpModCost(costFun: ExpModCostingFunction) extends ThreeArguments {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger
        ): CostingInteger =
            costFun(arg1, arg2, arg3)
    }

    given ReadWriter[ThreeArguments] = readwriter[ujson.Value].bimap(
      {
          case ConstantCost(cost) =>
              ujson.Obj("type" -> "constant_cost", "arguments" -> cost.toLong)
          case LinearInX(costFun) =>
              ujson.Obj("type" -> "linear_in_x", "arguments" -> writeJs(costFun))
          case LinearInY(costFun) =>
              ujson.Obj("type" -> "linear_in_y", "arguments" -> writeJs(costFun))
          case LinearInZ(costFun) =>
              ujson.Obj("type" -> "linear_in_z", "arguments" -> writeJs(costFun))
          case QuadraticInZ(costFun) =>
              ujson.Obj("type" -> "quadratic_in_z", "arguments" -> writeJs(costFun))
          case LiteralInYOrLinearInZ(costFun) =>
              ujson.Obj("type" -> "literal_in_y_or_linear_in_z", "arguments" -> writeJs(costFun))
          case LinearInMaxYZ(costFun) =>
              ujson.Obj("type" -> "linear_in_max_yz", "arguments" -> writeJs(costFun))
          case LinearInYAndZ(costFun) =>
              ujson.Obj("type" -> "linear_in_y_and_z", "arguments" -> writeJs(costFun))
          case ExpModCost(costFun) =>
              ujson.Obj("type" -> "exp_mod_cost", "arguments" -> writeJs(costFun))
      },
      json => {
          json.obj("type").str match
              case "constant_cost" => ConstantCost(CostingInteger(json.obj("arguments").num.toLong))
              case "linear_in_x" =>
                  LinearInX(read[OneVariableLinearFunction](json.obj("arguments")))
              case "linear_in_y" =>
                  LinearInY(read[OneVariableLinearFunction](json.obj("arguments")))
              case "linear_in_z" =>
                  LinearInZ(read[OneVariableLinearFunction](json.obj("arguments")))
              case "quadratic_in_z" =>
                  QuadraticInZ(read[OneVariableQuadraticFunction](json.obj("arguments")))
              case "literal_in_y_or_linear_in_z" =>
                  LiteralInYOrLinearInZ(read[OneVariableLinearFunction](json.obj("arguments")))
              case "linear_in_max_yz" =>
                  LinearInMaxYZ(read[OneVariableLinearFunction](json.obj("arguments")))
              case "linear_in_y_and_z" =>
                  LinearInYAndZ(read[TwoVariableLinearFunction](json.obj("arguments")))
              case "exp_mod_cost" =>
                  ExpModCost(read[ExpModCostingFunction](json.obj("arguments")))
              case other => throw new RuntimeException(s"Unexpected type ${other}")
      }
    )
}

trait FourArguments extends CostModel {
    def apply(
        arg1: CostingInteger,
        arg2: CostingInteger,
        arg3: CostingInteger,
        arg4: CostingInteger
    ): CostingInteger
    def calculateCost(args: Seq[CostingInteger]) =
        apply(args(0), args(1), args(2), args(3))
}

object FourArguments {
    case class ConstantCost(cost: CostingInteger) extends FourArguments with BaseConstantCostModel {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger,
            arg4: CostingInteger
        ): CostingInteger = cost
    }

    case class LinearInU(costFun: OneVariableLinearFunction) extends FourArguments {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger,
            arg4: CostingInteger
        ): CostingInteger =
            costFun(arg4)
    }

    given ReadWriter[FourArguments] = readwriter[ujson.Value].bimap(
      {
          case ConstantCost(cost) =>
              ujson.Obj("type" -> "constant_cost", "arguments" -> cost.toLong)
          case LinearInU(costFun) =>
              ujson.Obj("type" -> "linear_in_u", "arguments" -> writeJs(costFun))
      },
      json => {
          json.obj("type").str match
              case "constant_cost" => ConstantCost(CostingInteger(json.obj("arguments").num.toLong))
              case "linear_in_u" =>
                  LinearInU(read[OneVariableLinearFunction](json.obj("arguments")))
              case other => throw new RuntimeException(s"Unexpected type ${other}")
      }
    )
}

trait FiveArguments extends CostModel {
    def apply(
        arg1: CostingInteger,
        arg2: CostingInteger,
        arg3: CostingInteger,
        arg4: CostingInteger,
        arg5: CostingInteger
    ): CostingInteger
    def calculateCost(args: Seq[CostingInteger]) =
        apply(args(0), args(1), args(2), args(3), args(4))
}

object FiveArguments {
    case class ConstantCost(cost: CostingInteger) extends FiveArguments with BaseConstantCostModel {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger,
            arg4: CostingInteger,
            arg5: CostingInteger
        ): CostingInteger = cost
    }

    given ReadWriter[FiveArguments] = readwriter[ujson.Value].bimap(
      { case ConstantCost(cost) =>
          ujson.Obj("type" -> "constant_cost", "arguments" -> cost.toLong)
      },
      json => {
          json.obj("type").str match
              case "constant_cost" => ConstantCost(CostingInteger(json.obj("arguments").num.toLong))
              case other           => throw new RuntimeException(s"Unexpected type ${other}")
      }
    )
}

trait SixArguments extends CostModel {
    def apply(
        arg1: CostingInteger,
        arg2: CostingInteger,
        arg3: CostingInteger,
        arg4: CostingInteger,
        arg5: CostingInteger,
        arg6: CostingInteger
    ): CostingInteger
    def calculateCost(args: Seq[CostingInteger]) =
        apply(args(0), args(1), args(2), args(3), args(4), args(5))
}

object SixArguments {
    case class ConstantCost(cost: CostingInteger) extends SixArguments with BaseConstantCostModel {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger,
            arg4: CostingInteger,
            arg5: CostingInteger,
            arg6: CostingInteger
        ): CostingInteger = cost
    }

    given ReadWriter[SixArguments] = readwriter[ujson.Value].bimap(
      { case ConstantCost(cost) =>
          ujson.Obj("type" -> "constant_cost", "arguments" -> cost.toLong)
      },
      json => {
          json.obj("type").str match
              case "constant_cost" => ConstantCost(CostingInteger(json.obj("arguments").num.toLong))
              case other           => throw new RuntimeException(s"Unexpected type ${other}")
      }
    )
}

trait CostingFun {
    def calculateCost(args: CekValue*): ExUnits
}

case class DefaultCostingFun[+M <: CostModel](cpu: M, memory: M) extends CostingFun
    derives ReadWriter {
    def calculateCost(args: CekValue*): ExUnits = {
        val argsMem = args.map(MemoryUsage.memoryUsage)
        val cpu = this.cpu.calculateCost(argsMem)
        val mem = this.memory.calculateCost(argsMem)
        ExUnits(mem.toLong, cpu.toLong)
    }

    /** Calculate cost from pre-computed memory usage values. Useful for JIT to avoid CekValue
      * wrapper allocation.
      */
    def calculateCostFromMemory(argsCostingInteger: Seq[CostingInteger]): ExUnits = {
        val cpu = this.cpu.calculateCost(argsCostingInteger)
        val mem = this.memory.calculateCost(argsCostingInteger)
        ExUnits(mem.toLong, cpu.toLong)
    }
}

/** CostingFun wrapper that uses ValueMaxDepth memory for BuiltinValue at a specific argument
  * position. Matches Plutus ValueMaxDepth costing wrapper for insertCoin/lookupCoin.
  */
class ValueMaxDepthCostingFun[+M <: CostModel](delegate: DefaultCostingFun[M], valueArgIndex: Int)
    extends CostingFun {
    def calculateCost(args: CekValue*): ExUnits = {
        val argsMem = args.zipWithIndex.map { case (arg, i) =>
            if i == valueArgIndex then
                arg match
                    case CekValue.VCon(Constant.BuiltinValue(v)) =>
                        BuiltinValueOps.valueMaxDepth(v)
                    case _ => MemoryUsage.memoryUsage(arg)
            else MemoryUsage.memoryUsage(arg)
        }
        val cpu = delegate.cpu.calculateCost(argsMem)
        val mem = delegate.memory.calculateCost(argsMem)
        ExUnits(mem.toLong, cpu.toLong)
    }
}

case class ConstCostingFun(cpu: CostingInteger, memory: CostingInteger) extends CostingFun {

    def calculateCost(args: CekValue*): ExUnits = {
        constantCost
    }

    val constantCost: ExUnits = ExUnits(memory.toLong, cpu.toLong)

    inline def toDefaultFun[M <: CostModel]: DefaultCostingFun[M] = {
        inline erasedValue[M] match {
            case _: OneArgument =>
                DefaultCostingFun[OneArgument](
                  cpu = OneArgument.ConstantCost(cpu),
                  memory = OneArgument.ConstantCost(memory)
                ).asInstanceOf[DefaultCostingFun[M]]
            case _: TwoArguments =>
                DefaultCostingFun[TwoArguments](
                  cpu = TwoArguments.ConstantCost(cpu),
                  memory = TwoArguments.ConstantCost(memory)
                ).asInstanceOf[DefaultCostingFun[M]]
            case _: ThreeArguments =>
                DefaultCostingFun[ThreeArguments](
                  cpu = ThreeArguments.ConstantCost(cpu),
                  memory = ThreeArguments.ConstantCost(memory)
                ).asInstanceOf[DefaultCostingFun[M]]
            case _: FourArguments =>
                DefaultCostingFun[FourArguments](
                  cpu = FourArguments.ConstantCost(cpu),
                  memory = FourArguments.ConstantCost(memory)
                ).asInstanceOf[DefaultCostingFun[M]]
            case _: FiveArguments =>
                DefaultCostingFun[FiveArguments](
                  cpu = FiveArguments.ConstantCost(cpu),
                  memory = FiveArguments.ConstantCost(memory)
                ).asInstanceOf[DefaultCostingFun[M]]
            case _: SixArguments =>
                DefaultCostingFun[SixArguments](
                  cpu = SixArguments.ConstantCost(cpu),
                  memory = SixArguments.ConstantCost(memory)
                ).asInstanceOf[DefaultCostingFun[M]]
        }
    }

}

object ConstCostingFun {

    def fromDefaultFun[M <: CostModel](df: DefaultCostingFun[M]): Option[ConstCostingFun] = {
        (df.cpu, df.memory) match {
            case (constCpu: BaseConstantCostModel, constMem: BaseConstantCostModel) =>
                Some(ConstCostingFun(constCpu.cost, constMem.cost))
            case _ => None
        }
    }

    def readDefaultFun[M <: CostModel](
        name: String,
        s: ujson.Readable,
        trace: Boolean = false
    )(using Reader[DefaultCostingFun[M]]): ConstCostingFun = {
        val df = read[DefaultCostingFun[M]](s, trace)
        fromDefaultFun[M](df) match {
            case Some(cf) => cf
            case None =>
                throw new IllegalStateException(s"Costing function $name is not constant")
        }
    }

}

/** When invoking `integerToByteString` built-in function, its second argument is a built-in Integer
  * but with a different size measure, specifying the width (in bytes) of the output bytestring
  * (zero-padded to the desired size). The memory consumed by the function is given by `w`, not the
  * size of `w`. Its `MemoryUsage` is equal to the number of eight-byte words required to contain
  * `w` bytes, allowing its costing function to work properly.
  *
  * @see
  *   Plutus implementation
  *   https://github.com/IntersectMBO/plutus/blob/bc8c3a765769d2c0cd41c43278f5954cfdfd9b15/plutus-core/plutus-core/src/PlutusCore/Evaluation/Machine/ExMemoryUsage.hs#L171
  */
case class IntegerToByteStringCostingFun(cpu: ThreeArguments, memory: ThreeArguments)
    extends CostingFun derives ReadWriter {
    def calculateCost(args: CekValue*): ExUnits = {
        val Seq(arg0, CekValue.VCon(Constant.Integer(size)), arg2) = args.toSeq: @unchecked
        val argsMem = Seq(
          MemoryUsage.memoryUsage(arg0),
          MemoryUsage.memoryUsageLiteralByteSize(size),
          MemoryUsage.memoryUsage(arg2)
        )
        val cpu = this.cpu.calculateCost(argsMem)
        val mem = this.memory.calculateCost(argsMem)
        ExUnits(mem.toLong, cpu.toLong)
    }
}

case class ReplicateByteCostingFun(cpu: TwoArguments, memory: TwoArguments) extends CostingFun
    derives ReadWriter {
    def calculateCost(args: CekValue*): ExUnits = {
        val Seq(CekValue.VCon(Constant.Integer(size)), arg1) = args.toSeq: @unchecked
        val argsMem = Seq(
          MemoryUsage.memoryUsageLiteralByteSize(size),
          MemoryUsage.memoryUsage(arg1)
        )
        val cpu = this.cpu.calculateCost(argsMem)
        val mem = this.memory.calculateCost(argsMem)
        ExUnits(mem.toLong, cpu.toLong)
    }
}

case class ShiftOrRotateByteStringCostingFun(cpu: TwoArguments, memory: TwoArguments)
    extends CostingFun derives ReadWriter {
    def calculateCost(args: CekValue*): ExUnits = {
        val Seq(arg0, CekValue.VCon(Constant.Integer(size))) = args.toSeq: @unchecked
        val argsMem = Seq(
          MemoryUsage.memoryUsage(arg0),
          MemoryUsage.memoryUsageLiteral(size)
        )
        val cpu = this.cpu.calculateCost(argsMem)
        val mem = this.memory.calculateCost(argsMem)
        ExUnits(mem.toLong, cpu.toLong)
    }
}

case class WriteBitsCostingFun(cpu: ThreeArguments, memory: ThreeArguments) extends CostingFun
    derives ReadWriter {
    def calculateCost(args: CekValue*): ExUnits = {
        val Seq(arg0, CekValue.VCon(Constant.List(_, list)), arg2) = args.toSeq: @unchecked
        val argsMem =
            Seq(
              MemoryUsage.memoryUsage(arg0),
              CostingInteger(list.size.toLong),
              MemoryUsage.memoryUsage(arg2)
            )
        ExUnits(
          this.memory.calculateCost(argsMem).toLong,
          this.cpu.calculateCost(argsMem).toLong
        )
    }
}

/** Custom costing function for dropList builtin.
  *
  * dropList uses IntegerCostedLiterally for the first argument, meaning the cost is based on the
  * absolute value of the integer, not its memory representation.
  */
case class DropListCostingFun(cpu: TwoArguments, memory: TwoArguments) extends CostingFun
    derives ReadWriter {
    def calculateCost(args: CekValue*): ExUnits = {
        val Seq(CekValue.VCon(Constant.Integer(n)), arg1) = args.toSeq: @unchecked
        val argsMem = Seq(
          MemoryUsage.memoryUsageLiteral(n), // Use literal value, not memory representation
          MemoryUsage.memoryUsage(arg1)
        )
        val cpu = this.cpu.calculateCost(argsMem)
        val mem = this.memory.calculateCost(argsMem)
        ExUnits(mem.toLong, cpu.toLong)
    }
}

/** Custom costing function for expModInteger builtin (CIP-109).
  *
  * The CPU cost uses the ExpModCost formula which accounts for the size of base, exponent, and
  * modulus. Memory cost is linear in the size of the modulus.
  */
case class ExpModIntegerCostingFun(cpu: ThreeArguments, memory: ThreeArguments) extends CostingFun
    derives ReadWriter {
    def calculateCost(args: CekValue*): ExUnits = {
        val argsMem = args.map(MemoryUsage.memoryUsage)
        val cpu = this.cpu.calculateCost(argsMem)
        val mem = this.memory.calculateCost(argsMem)
        ExUnits(mem.toLong, cpu.toLong)
    }
}
