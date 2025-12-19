package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.{BuiltinArray, Data}
import scalus.builtin.Builtins.*
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.uplc.{Constant, Term}

/** Tests for BuiltinArray lowering in case classes.
  *
  * BuiltinArray now has a dedicated BuiltinArraySirTypeGenerator that handles conversion to/from
  * Data representation. When stored in a case class, BuiltinArray is converted to Data.List by
  * iterating through the array elements using indexArray. When extracted, it's converted back to
  * BuiltinArray using listToArray.
  */
class BuiltinArrayLoweringTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    // Case class with BuiltinArray field for testing
    case class ArrayWrapper(arr: BuiltinArray[Data])

    // Test that BuiltinArray can be passed through a case class and extracted correctly
    test("extract BuiltinArray from case class and access first element") {
        val sir = compile {
            // Create an array
            val arr = BuiltinArray(iData(42), iData(100), iData(200))
            // Wrap it in a case class
            val wrapper = new ArrayWrapper(arr)
            // Extract and access first element
            val extractedArr = wrapper.arr
            indexArray(extractedArr, BigInt(0))
        }

        val term = sir.toUplc()
        val result = term.evaluateDebug

        result match {
            case Result.Success(evaled, _, _, _) =>
                assert(evaled == Term.Const(Constant.Data(Data.I(42))))
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success but got failure: $err")
        }
    }

    // Test that BuiltinArray can be passed to a function and returned
    test("pass BuiltinArray to function and return") {
        val sir = compile {
            def extractArray(w: ArrayWrapper): BuiltinArray[Data] = w.arr

            val arr = BuiltinArray(iData(10), iData(20), iData(30))
            val wrapper = new ArrayWrapper(arr)
            val extracted = extractArray(wrapper)
            lengthOfArray(extracted)
        }

        val term = sir.toUplc()
        val result = term.evaluateDebug

        result match {
            case Result.Success(evaled, _, _, _) =>
                assert(evaled == Term.Const(Constant.Integer(3)))
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success but got failure: $err")
        }
    }

    // Test accessing elements after extraction from case class
    test("access multiple elements after extracting array from case class") {
        val sir = compile {
            val arr = BuiltinArray(iData(100), iData(200), iData(300))
            val wrapper = new ArrayWrapper(arr)
            val extractedArr = wrapper.arr
            // Access element at index 1
            val elem = indexArray(extractedArr, BigInt(1))
            unIData(elem)
        }

        val term = sir.toUplc()
        val result = term.evaluateDebug

        result match {
            case Result.Success(evaled, _, _, _) =>
                assert(evaled == Term.Const(Constant.Integer(200)))
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success but got failure: $err")
        }
    }

    // Test that checks the default representation for BuiltinArray type
    // This test documents what representation is currently assigned (incorrectly)
    test("check default representation for BuiltinArray type") {
        import scalus.compiler.sir.SIRType
        import scalus.compiler.sir.lowering.typegens.SirTypeUplcGenerator

        given LoweringContext = LoweringContext()

        // BuiltinArray[Data]
        val builtinArrayDataType = SIRType.BuiltinArray(SIRType.Data.tp)
        val generator = SirTypeUplcGenerator(builtinArrayDataType)
        val defaultRepr = generator.defaultRepresentation(builtinArrayDataType)

        // Document the current (incorrect) state
        info(s"BuiltinArray[Data] generator: ${generator.getClass.getSimpleName}")
        info(s"BuiltinArray[Data] default representation: $defaultRepr")

        // The generator falls through to ProductCaseSirTypeGenerator
        // which gives ProdDataList representation - this is incorrect for arrays.
        // A proper implementation would need a dedicated BuiltinArraySirTypeGenerator
        // that uses a native array representation (similar to how BuiltinList uses SumDataList).
    }

    // Test nested case class with BuiltinArray
    case class OuterWrapper(inner: ArrayWrapper, value: BigInt)

    test("nested case class with BuiltinArray field") {
        val sir = compile {
            val arr = BuiltinArray(iData(1), iData(2), iData(3))
            val inner = new ArrayWrapper(arr)
            val outer = new OuterWrapper(inner, BigInt(999))
            // Extract nested array
            val extractedArr = outer.inner.arr
            lengthOfArray(extractedArr)
        }

        val term = sir.toUplc()
        val result = term.evaluateDebug

        result match {
            case Result.Success(evaled, _, _, _) =>
                assert(evaled == Term.Const(Constant.Integer(3)))
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success but got failure: $err")
        }
    }

    // Test BuiltinArray in function call
    test("BuiltinArray passed through function") {
        val sir = compile {
            def processWrapper(w: ArrayWrapper): BigInt = {
                val arr = w.arr
                lengthOfArray(arr)
            }

            val arr = BuiltinArray(iData(5), iData(6))
            val wrapper = new ArrayWrapper(arr)
            processWrapper(wrapper)
        }

        val term = sir.toUplc()
        val result = term.evaluateDebug

        result match {
            case Result.Success(evaled, _, _, _) =>
                assert(evaled == Term.Const(Constant.Integer(2)))
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success but got failure: $err")
        }
    }
}
