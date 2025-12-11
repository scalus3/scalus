package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.BuiltinList
import scalus.builtin.Builtins.*
import scalus.uplc.eval.{PlutusVM, Result}

/** Tests for lowering issues with BuiltinList literals.
  *
  * These tests document known issues with compiling BuiltinList literals in certain contexts. The
  * lowering phase has trouble with representation conversion for BuiltinList types.
  *
  * Error: LoweringException: Unexpected representation conversion for
  * scalus.builtin.BuiltinList[Int] from DataConstr to SumDataList
  */
class BuiltinListLoweringTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    // TODO: Fix lowering issue with BuiltinList literal in dropList result
    // This test fails with LoweringException: Unexpected representation conversion
    // for scalus.builtin.BuiltinList[Int] from DataConstr to SumDataList
    test("dropList with BuiltinList literal") {
        import scalus.compiler.sir.SIRType
        import scalus.compiler.sir.lowering.LoweringContext
        import scalus.compiler.sir.lowering.typegens.SirTypeUplcGenerator

        val sir = compile {
            val list = BuiltinList[BigInt](10, 20, 30, 40, 50)
            dropList(BigInt(2), list)
        }
        println(s"SIR:\n${sir.pretty.render(80)}")

        // Check default representation for BuiltinList type
        given LoweringContext = LoweringContext()
        val builtinListType = SIRType.BuiltinList(SIRType.Integer)
        val generator = SirTypeUplcGenerator(builtinListType)
        val defaultRepr = generator.defaultRepresentation(builtinListType)
        println(s"BuiltinList[Integer] default representation: $defaultRepr")
        println(s"Generator class: ${generator.getClass.getSimpleName}")

        // Also check for Cons constructor type - get the constrDecl
        val consConstrDecl =
            SIRType.BuiltinList.dataDecl.constructors.find(_.name.contains("Cons")).get
        println(s"BuiltinList.Cons constrDecl: ${consConstrDecl.name}")
        // Create a CaseClass type for Cons[Integer]
        val consType =
            SIRType.CaseClass(consConstrDecl, List(SIRType.Integer), Some(builtinListType))
        val consGenerator = SirTypeUplcGenerator(consType)
        val consDefaultRepr = consGenerator.defaultRepresentation(consType)
        println(s"BuiltinList.Cons[Integer] type: ${consType.show}")
        println(s"BuiltinList.Cons[Integer] default representation: $consDefaultRepr")
        println(s"Cons Generator class: ${consGenerator.getClass.getSimpleName}")

        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                val expected = compile(BuiltinList[BigInt](30, 40, 50)).toUplc()
                assert(term α_== expected, s"Expected $expected but got $term")
            case Result.Failure(e, _, _, _) =>
                fail(s"Evaluation failed: $e")
    }

    // Test to understand BuiltinList literal compilation
    ignore("BuiltinList literal standalone") {
        val sir = compile {
            BuiltinList[BigInt](1, 2, 3)
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                info(s"Result: $term")
            case Result.Failure(e, _, _, _) =>
                fail(s"Evaluation failed: $e")
    }

    // Test headList with BuiltinList literal
    ignore("headList with BuiltinList literal") {
        val sir = compile {
            val list = BuiltinList[BigInt](10, 20, 30)
            headList(list)
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                info(s"Result: $term")
            case Result.Failure(e, _, _, _) =>
                fail(s"Evaluation failed: $e")
    }

    // Test tailList with BuiltinList literal
    ignore("tailList with BuiltinList literal") {
        val sir = compile {
            val list = BuiltinList[BigInt](10, 20, 30)
            tailList(list)
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                info(s"Result: $term")
            case Result.Failure(e, _, _, _) =>
                fail(s"Evaluation failed: $e")
    }

    // Test constRepresentation for BuiltinList[Data] and BuiltinList[BuiltinPair[Data,Data]]
    test("constRepresentation for BuiltinList[Data] returns SumDataList") {
        import scalus.compiler.sir.SIRType

        given LoweringContext = LoweringContext()

        // BuiltinList[Data] should return SumDataList
        val builtinListDataType = SIRType.BuiltinList(SIRType.Data)
        val dataListRepr = LoweredValueRepresentation.constRepresentation(builtinListDataType)
        assert(
          dataListRepr == SumCaseClassRepresentation.SumDataList,
          s"Expected SumDataList but got $dataListRepr"
        )

        // BuiltinList[BuiltinPair[Data, Data]] should return SumDataPairList
        val pairType = SIRType.CaseClass(
          SIRType.BuiltinPair.constrDecl,
          List(SIRType.Data, SIRType.Data),
          None
        )
        val builtinListPairType = SIRType.BuiltinList(pairType)
        val pairListRepr = LoweredValueRepresentation.constRepresentation(builtinListPairType)
        assert(
          pairListRepr == SumCaseClassRepresentation.SumDataPairList,
          s"Expected SumDataPairList but got $pairListRepr"
        )

        // BuiltinList[Integer] should throw exception (non-Data element type not supported)
        val builtinListIntType = SIRType.BuiltinList(SIRType.Integer)
        val ex = intercept[LoweringException] {
            LoweredValueRepresentation.constRepresentation(builtinListIntType)
        }
        assert(
          ex.getMessage.contains("non-Data element type"),
          s"Expected exception about non-Data element type, got: ${ex.getMessage}"
        )
    }
}
