package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.BuiltinList
import scalus.uplc.builtin.Builtins.*
import scalus.compiler.compile
import scalus.uplc.eval.{PlutusVM, Result}

/** Tests for lowering issues with BuiltinList literals.
  *
  * These tests document known issues with compiling BuiltinList literals in certain contexts. The
  * lowering phase has trouble with representation conversion for BuiltinList types.
  *
  * Error: LoweringException: Unexpected representation conversion for
  * scalus.uplc.builtin.BuiltinList[Int] from DataConstr to SumDataList
  */
class BuiltinListLoweringTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    // TODO: Fix lowering issue with BuiltinList literal in dropList result
    // This test fails with LoweringException: Unexpected representation conversion
    // for scalus.uplc.builtin.BuiltinList[Int] from DataConstr to SumDataList
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
    test("BuiltinList literal standalone") {
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
    test("headList with BuiltinList literal") {
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
    test("tailList with BuiltinList literal") {
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
    test("constRepresentation for BuiltinList[Data] returns SumBuiltinList(DataData)") {
        import scalus.compiler.sir.SIRType

        given LoweringContext = LoweringContext()

        // BuiltinList[Data] should return SumBuiltinList(DataData)
        val builtinListDataType = SIRType.BuiltinList(SIRType.Data.tp)
        val dataListRepr = LoweredValueRepresentation.constRepresentation(builtinListDataType)
        assert(
          dataListRepr == SumCaseClassRepresentation.SumBuiltinList(SumCaseClassRepresentation.DataData),
          s"Expected SumBuiltinList(DataData) but got $dataListRepr"
        )

        // BuiltinList[BuiltinPair[Data, Data]] should return SumDataPairList
        val pairType = SIRType.CaseClass(
          SIRType.BuiltinPair.constrDecl,
          List(SIRType.Data.tp, SIRType.Data.tp),
          None
        )
        val builtinListPairType = SIRType.BuiltinList(pairType)
        val pairListRepr = LoweredValueRepresentation.constRepresentation(builtinListPairType)
        assert(
          pairListRepr == SumCaseClassRepresentation.SumDataPairList,
          s"Expected SumDataPairList but got $pairListRepr"
        )

        // BuiltinList[Integer] should return SumBuiltinList(Constant) for native primitive lists
        val builtinListIntType = SIRType.BuiltinList(SIRType.Integer)
        val intListRepr = LoweredValueRepresentation.constRepresentation(builtinListIntType)
        assert(
          intListRepr == SumCaseClassRepresentation.SumBuiltinList(
            PrimitiveRepresentation.Constant
          ),
          s"Expected SumBuiltinList(Constant) but got $intListRepr"
        )
    }

    // --- SumBuiltinList tests ---

    test("SumBuiltinList structural equality") {
        import SumCaseClassRepresentation.*

        // SumBuiltinList(DataData) structural equality
        val dataList1 = SumBuiltinList(DataData)
        val dataList2 = SumBuiltinList(DataData)
        assert(dataList1 == dataList2)
        assert(dataList1.isInstanceOf[SumBuiltinList])

        // SumDataPairList is SumBuiltinList(PairData)
        assert(SumDataPairList == SumBuiltinList(ProductCaseClassRepresentation.PairData))
        assert(SumDataPairList.isInstanceOf[SumBuiltinList])

        // Different element reprs are not equal
        assert(dataList1 != SumDataPairList)
        assert(SumBuiltinList(PrimitiveRepresentation.Constant) != dataList1)
    }

    test("SumBuiltinList isCompatibleWithType checks") {
        import scalus.compiler.sir.SIRType
        import SumCaseClassRepresentation.*

        given LoweringContext = LoweringContext()

        val listIntType = SIRType.List(SIRType.Integer)
        val pairType = SIRType.CaseClass(
          SIRType.BuiltinPair.constrDecl,
          List(SIRType.Data.tp, SIRType.Data.tp),
          None
        )
        val listPairType = SIRType.List(pairType)

        // SumBuiltinList(DataData) is compatible with any list
        val dataList = SumBuiltinList(DataData)
        assert(dataList.isCompatibleWithType(listIntType))
        assert(dataList.isCompatibleWithType(listPairType))

        // SumBuiltinList(PairData) is compatible only with pair-element lists
        assert(SumDataPairList.isCompatibleWithType(listPairType))
        assert(!SumDataPairList.isCompatibleWithType(listIntType))

        // Non-list types are not compatible with either
        assert(!dataList.isCompatibleWithType(SIRType.Integer))
        assert(!SumDataPairList.isCompatibleWithType(SIRType.Integer))
    }

    test("SumBuiltinList isPackedData and isDataCentric") {
        import SumCaseClassRepresentation.*

        // SumBuiltinList(DataData): isPackedData=false, isDataCentric=true
        val dataList = SumBuiltinList(DataData)
        assert(!dataList.isPackedData)
        assert(dataList.isDataCentric)

        // SumBuiltinList(PairData): isPackedData=false, isDataCentric=true
        assert(!SumDataPairList.isPackedData)
        assert(SumDataPairList.isDataCentric)

        // SumBuiltinList(Constant): isPackedData=false, isDataCentric=false
        val nativeList = SumBuiltinList(PrimitiveRepresentation.Constant)
        assert(!nativeList.isPackedData)
        assert(!nativeList.isDataCentric)
    }

    // --- Phase 2: Native element storage tests ---

    test("BuiltinList[BigInt] preserves native Constant.List(Integer)") {
        val sir = compile {
            BuiltinList[BigInt](1, 2, 3)
        }
        val uplc = sir.toUplc()
        val result = uplc.evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                // Should be a native integer list, not Data-wrapped
                term match
                    case scalus.uplc.Term.Const(scalus.uplc.Constant.List(elemType, elements), _) =>
                        assert(
                          elemType == scalus.uplc.DefaultUni.Integer,
                          s"Expected Integer element type but got $elemType"
                        )
                        assert(elements.size == 3, s"Expected 3 elements but got ${elements.size}")
                    case other =>
                        fail(s"Expected Constant.List but got $other")
            case Result.Failure(e, _, _, _) =>
                fail(s"Evaluation failed: $e")
    }

    test("headList on native BuiltinList[BigInt] returns Integer") {
        val sir = compile {
            val list = BuiltinList[BigInt](10, 20, 30)
            headList(list)
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                term match
                    case scalus.uplc.Term.Const(scalus.uplc.Constant.Integer(v), _) =>
                        assert(v == 10, s"Expected 10 but got $v")
                    case other =>
                        fail(s"Expected Constant.Integer but got $other")
            case Result.Failure(e, _, _, _) =>
                fail(s"Evaluation failed: $e")
    }

    test("tailList on native BuiltinList[BigInt]") {
        val sir = compile {
            val list = BuiltinList[BigInt](10, 20, 30)
            tailList(list)
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                term match
                    case scalus.uplc.Term.Const(scalus.uplc.Constant.List(elemType, elements), _) =>
                        assert(
                          elemType == scalus.uplc.DefaultUni.Integer,
                          s"Expected Integer element type but got $elemType"
                        )
                        assert(elements.size == 2, s"Expected 2 elements but got ${elements.size}")
                    case other =>
                        fail(s"Expected Constant.List but got $other")
            case Result.Failure(e, _, _, _) =>
                fail(s"Evaluation failed: $e")
    }

    test("BuiltinList[ByteString] native literal") {
        import scalus.uplc.builtin.ByteString
        val sir = compile {
            BuiltinList[ByteString](ByteString.fromHex("dead"), ByteString.fromHex("beef"))
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                term match
                    case scalus.uplc.Term.Const(scalus.uplc.Constant.List(elemType, elements), _) =>
                        assert(
                          elemType == scalus.uplc.DefaultUni.ByteString,
                          s"Expected ByteString element type but got $elemType"
                        )
                        assert(elements.size == 2)
                    case other =>
                        fail(s"Expected Constant.List but got $other")
            case Result.Failure(e, _, _, _) =>
                fail(s"Evaluation failed: $e")
    }

    test("BuiltinList[String] native literal") {
        val sir = compile {
            BuiltinList[String]("hello", "world")
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                term match
                    case scalus.uplc.Term.Const(scalus.uplc.Constant.List(elemType, elements), _) =>
                        assert(
                          elemType == scalus.uplc.DefaultUni.String,
                          s"Expected String element type but got $elemType"
                        )
                        assert(elements.size == 2)
                    case other =>
                        fail(s"Expected Constant.List but got $other")
            case Result.Failure(e, _, _, _) =>
                fail(s"Evaluation failed: $e")
    }

    test("BuiltinList[Boolean] native literal") {
        val sir = compile {
            BuiltinList[Boolean](true, false, true)
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                term match
                    case scalus.uplc.Term.Const(scalus.uplc.Constant.List(elemType, elements), _) =>
                        assert(
                          elemType == scalus.uplc.DefaultUni.Bool,
                          s"Expected Bool element type but got $elemType"
                        )
                        assert(elements.size == 3)
                    case other =>
                        fail(s"Expected Constant.List but got $other")
            case Result.Failure(e, _, _, _) =>
                fail(s"Evaluation failed: $e")
    }

    test("uplcType for SumBuiltinList representations") {
        import scalus.compiler.sir.SIRType

        given LoweringContext = LoweringContext()

        val listIntType = SIRType.List(SIRType.Integer)

        // SumBuiltinList(Constant) → BuiltinList[Integer]
        val nativeRepr = SumCaseClassRepresentation.SumBuiltinList(PrimitiveRepresentation.Constant)
        val nativeUplc = nativeRepr.uplcType(listIntType)
        assert(
          nativeUplc == SIRType.BuiltinList(SIRType.Integer),
          s"Expected BuiltinList[Integer] but got ${nativeUplc.show}"
        )

        // SumBuiltinList(DataData) → BuiltinList[Data]
        val dataUplc = SumCaseClassRepresentation.SumBuiltinList(SumCaseClassRepresentation.DataData).uplcType(listIntType)
        assert(
          dataUplc == SIRType.BuiltinList(SIRType.Data.tp),
          s"Expected BuiltinList[Data] but got ${dataUplc.show}"
        )
    }

    test("List.get with intrinsic modules") {
        import scalus.cardano.onchain.plutus.prelude.List
        import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}
        import scalus.uplc.*
        import scalus.compiler.Options
        import scalus.compiler.sir.TargetLoweringBackend
        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = true,
          optimizeUplc = false,
          debug = false
        )
        val compiled = PlutusV3.compile {
            Cons(BigInt(1), Cons(BigInt(2), Nil)).get(BigInt(0))
        }
        val lowering = new SirToUplcV3Lowering(
          compiled.sir,
          generateErrorTraces = true,
          intrinsicModules = IntrinsicResolver.defaultIntrinsicModules,
          supportModules = IntrinsicResolver.defaultSupportModules
        )
        val uplc = lowering.lower()
        val result = uplc.evaluateDebug
        result match
            case eval.Result.Success(term, _, _, _) => ()
            case f =>
                fail(s"get evaluation failed:\n$f")
    }

    test("List.isEmpty with intrinsic modules") {
        import scalus.cardano.onchain.plutus.prelude.List
        import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}
        import scalus.uplc.*
        import scalus.compiler.Options
        import scalus.compiler.sir.TargetLoweringBackend
        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = true,
          optimizeUplc = true,
          debug = false
        )
        val compiled = PlutusV3.compile(!Cons(BigInt(1), Cons(BigInt(2), Nil)).isEmpty)
        val lowering = new SirToUplcV3Lowering(
          compiled.sir,
          generateErrorTraces = true,
          intrinsicModules = IntrinsicResolver.defaultIntrinsicModules,
          supportModules = IntrinsicResolver.defaultSupportModules
        )
        val unoptimized = lowering.lower()
        val unoptResult = unoptimized.evaluateDebug
        unoptResult match
            case eval.Result.Success(term, _, _, _) =>
                assert(term == Term.Const(Constant.Bool(true)), s"Expected true but got ${term.show}")
            case f =>
                fail(s"isEmpty evaluation failed:\n$f")
    }
}
