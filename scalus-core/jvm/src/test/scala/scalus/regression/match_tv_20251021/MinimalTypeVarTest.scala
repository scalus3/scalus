package scalus.regression.match_tv_20251021

import scalus.*
import scalus.Compiler.compile
import org.scalatest.funsuite.AnyFunSuite

/** Minimal test case to reproduce TypeVarRepresentation in match expression issue
  *
  * The problem: When an extension method with a type parameter T returns T, and we match on the
  * result, the lowering code sees TypeVarRepresentation instead of the concrete type's
  * representation.
  *
  * This reproduces the error from hydrozoa20250804.SameCborTest where:
  * treasuryInput.resolved.inlineDatumOfType[TreasuryDatum] match { ... } fails with "Unsupported
  * representation TypeVarRepresentation(false)"
  */

// Define a simple enum at top level
@Compile
object TestEnum {
    enum MyEnum:
        case CaseA(value: BigInt)
        case CaseB(value: BigInt)
}

// Wrapper type for testing extension methods without Data
@Compile
object GenericWrapper {
    case class Wrapper[A](value: A)

    extension [A](self: Wrapper[A]) def unwrapAs[T]: T = self.value.asInstanceOf[T]
}

class MinimalTypeVarTest extends AnyFunSuite {

    test("extension method with type parameter in match - reproduces TypeVar issue") {
        val sir = compile {
            import GenericWrapper.*
            import TestEnum.*
            import TestEnum.MyEnum.*

            val wrapped: Wrapper[MyEnum] = Wrapper(CaseA(BigInt(42)))

            // This should trigger TypeVarRepresentation error
            // The issue: unwrapAs[MyEnum] returns T (type parameter), not concrete MyEnum
            wrapped.unwrapAs[MyEnum] match {
                case CaseA(v) => v
                case CaseB(v) => v
            }
        }

        val uplc = sir.toUplcOptimized()
        assert(uplc.plutusV3.cborByteString.nonEmpty)
    }
}
