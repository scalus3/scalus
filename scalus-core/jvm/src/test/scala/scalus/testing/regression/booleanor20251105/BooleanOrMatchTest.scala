package scalus.testing.regression.booleanor20251105

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.Data
import scalus.builtin.Data.{FromData, ToData}
import scalus.prelude.*

// Simple enum similar to VoteStatus
enum Status derives FromData, ToData:
    case Pending(value: BigInt)
    case Complete(result: BigInt, timestamp: BigInt)

@Compile
object Status

case class TestDatum(
    id: BigInt,
    status: Status
) derives FromData,
      ToData

@Compile
object TestDatum

/** Test case for boolean OR operator with match expressions
  *
  * This reproduces an issue where using `||` directly on parenthesized match expressions causes the
  * compiler to fail with: "Module scala.Boolean, referenced from var scala.Boolean.|| is not found"
  *
  * Root Cause: When a match expression returns boolean literals (true/false), Scala infers the type
  * as: OrType(ConstantType(Constant(true)), ConstantType(Constant(false))) This is a union type
  * that is a SUBTYPE of Boolean, but not EQUAL to Boolean.
  *
  * Fix: Changed boolean operator type checks from =:= (equality) to <:< (subtype) in
  * scalus-plugin/src/main/scala/scalus/SIRCompiler.scala lines 2285-2297
  */
class BooleanOrMatchTest extends AnyFunSuite:

    import scalus.uplc.eval.PlutusVM
    given PlutusVM = PlutusVM.makePlutusV2VM()

    given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    test("Boolean OR with extracted match results (WORKING)") {
        // This should compile successfully
        val sir = Compiler.compile {
            val datum1 = TestDatum(BigInt(1), Status.Pending(BigInt(42)))
            val datum2 = TestDatum(BigInt(2), Status.Complete(BigInt(100), BigInt(200)))

            // Extract boolean results into variables first
            val isPending1 = datum1.status match {
                case Status.Pending(_)     => true
                case Status.Complete(_, _) => false
            }

            val isPending2 = datum2.status match {
                case Status.Pending(_)     => true
                case Status.Complete(_, _) => false
            }

            // Now use || with the extracted values
            if isPending1 || isPending2 then BigInt(1)
            else BigInt(0)
        }

        val uplc = sir.toUplc()
        val result = uplc.evaluate

        // Should evaluate successfully
        assert(result.isInstanceOf[scalus.uplc.Term.Const])
    }

    test("Boolean OR with parenthesized match expressions - simple (WORKS)") {
        // This simple case works fine
        val sir = Compiler.compile {
            val datum1 = TestDatum(BigInt(1), Status.Pending(BigInt(42)))
            val datum2 = TestDatum(BigInt(2), Status.Complete(BigInt(100), BigInt(200)))

            // Using || directly on parenthesized match expressions
            // @formatter:off
            if ((datum1.status match {
                    case Status.Pending(_)     => true
                    case Status.Complete(_, _) => false
                }) || (datum2.status match {
                    case Status.Pending(_)     => true
                    case Status.Complete(_, _) => false
                })) then BigInt(1)
                    else BigInt(0)
            // @formatter:on
        }

        val uplc = sir.toUplc()
        val result = uplc.evaluate
        assert(result.isInstanceOf[scalus.uplc.Term.Const])
    }

    test("Boolean OR in complex expression (TRY TO REPRODUCE BUG)") {
        // Try to reproduce with more complex nesting like user's actual code
        val sir = Compiler.compile {
            def checkStatus(datum: TestDatum, other: TestDatum): BigInt = {
                val continuing = datum
                val removed = other
                // Pattern closer to user's original code
                // @formatter:off
                if ((continuing.status match {
                        case Status.Pending(_)     => true
                        case Status.Complete(_, _) => false
                    }) || (removed.status match {
                        case Status.Pending(_)     => true
                        case Status.Complete(_, _) => false
                    })) then BigInt(1)
                       else BigInt(0)
                // @formatter:on
            }

            val d1 = TestDatum(BigInt(1), Status.Pending(BigInt(42)))
            val d2 = TestDatum(BigInt(2), Status.Complete(BigInt(100), BigInt(200)))
            checkStatus(d1, d2)
        }

        val uplc = sir.toUplc()
        val result = uplc.evaluate
        assert(result.isInstanceOf[scalus.uplc.Term.Const])
    }

    test("Boolean AND with extracted match results (WORKING)") {
        // Test that && works with the same pattern
        val sir = Compiler.compile {
            val datum1 = TestDatum(BigInt(1), Status.Pending(BigInt(42)))
            val datum2 = TestDatum(BigInt(2), Status.Complete(BigInt(100), BigInt(200)))

            val isPending1 = datum1.status match {
                case Status.Pending(_)     => true
                case Status.Complete(_, _) => false
            }

            val isPending2 = datum2.status match {
                case Status.Pending(_)     => true
                case Status.Complete(_, _) => false
            }

            // Use && with extracted values
            if isPending1 && isPending2 then BigInt(1)
            else BigInt(0)
        }

        val uplc = sir.toUplc()
        val result = uplc.evaluate

        assert(result.isInstanceOf[scalus.uplc.Term.Const])
    }

    test("Nested if-else as alternative to OR (WORKING)") {
        // This is another workaround that should work
        val sir = Compiler.compile {
            val datum1 = TestDatum(BigInt(1), Status.Pending(BigInt(42)))
            val datum2 = TestDatum(BigInt(2), Status.Complete(BigInt(100), BigInt(200)))

            val isPending1 = datum1.status match {
                case Status.Pending(_)     => true
                case Status.Complete(_, _) => false
            }

            val isPending2 = datum2.status match {
                case Status.Pending(_)     => true
                case Status.Complete(_, _) => false
            }

            // Use nested if-else instead of ||
            if isPending1 then BigInt(1)
            else if isPending2 then BigInt(1)
            else BigInt(0)
        }

        val uplc = sir.toUplc()
        val result = uplc.evaluate

        assert(result.isInstanceOf[scalus.uplc.Term.Const])
    }

end BooleanOrMatchTest
