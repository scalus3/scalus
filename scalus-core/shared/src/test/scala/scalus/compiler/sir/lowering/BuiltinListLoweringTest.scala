package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.BuiltinList
import scalus.uplc.builtin.Builtins.*
import scalus.compiler.compile
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.uplc.eval.{PlutusVM, Result}

/** Tests for BuiltinList lowering with both nativeListElements flag values.
  */
class BuiltinListLoweringTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    private val flagValues = List(false, true)

    private def withBothFlags(testName: String)(body: Boolean => Unit): Unit =
        for native <- flagValues do
            test(s"$testName [nativeListElements=$native]") {
                body(native)
            }

    private def optionsWithFlag(native: Boolean): Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      nativeListElements = native
    )

    withBothFlags("dropList with BuiltinList literal") { native =>
        given Options = optionsWithFlag(native)
        val sir = compile {
            val list = BuiltinList[BigInt](10, 20, 30, 40, 50)
            dropList(BigInt(2), list)
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                // Both flag values should produce equivalent results when evaluated
                val expected = compile(BuiltinList[BigInt](30, 40, 50)).toUplc().evaluate
                assert(term α_== expected, s"Expected $expected but got $term")
            case Result.Failure(e, _, _, _) =>
                fail(s"Evaluation failed: $e")
    }

    withBothFlags("BuiltinList literal standalone") { native =>
        given Options = optionsWithFlag(native)
        val sir = compile {
            BuiltinList[BigInt](1, 2, 3)
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                info(s"[native=$native] Result: $term")
            case Result.Failure(e, _, _, _) =>
                fail(s"Evaluation failed: $e")
    }

    withBothFlags("headList with BuiltinList literal") { native =>
        given Options = optionsWithFlag(native)
        val sir = compile {
            val list = BuiltinList[BigInt](10, 20, 30)
            headList(list)
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                info(s"[native=$native] Result: $term")
            case Result.Failure(e, _, _, _) =>
                fail(s"Evaluation failed: $e")
    }

    withBothFlags("tailList with BuiltinList literal") { native =>
        given Options = optionsWithFlag(native)
        val sir = compile {
            val list = BuiltinList[BigInt](10, 20, 30)
            tailList(list)
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                info(s"[native=$native] Result: $term")
            case Result.Failure(e, _, _, _) =>
                fail(s"Evaluation failed: $e")
    }

    withBothFlags("constRepresentation for BuiltinList types") { native =>
        import scalus.compiler.sir.SIRType

        given LoweringContext = LoweringContext(nativeListElements = native)

        // BuiltinList[Data] should always return SumBuiltinList(DataData)
        val builtinListDataType = SIRType.BuiltinList(SIRType.Data.tp)
        val dataListRepr = LoweredValueRepresentation.constRepresentation(builtinListDataType)
        assert(
          dataListRepr == SumCaseClassRepresentation.SumBuiltinList(
            SumCaseClassRepresentation.DataData
          ),
          s"Expected SumBuiltinList(DataData) but got $dataListRepr"
        )

        // BuiltinList[BuiltinPair[Data, Data]] should always return SumPairBuiltinList
        val pairType = SIRType.CaseClass(
          SIRType.BuiltinPair.constrDecl,
          List(SIRType.Data.tp, SIRType.Data.tp),
          None
        )
        val builtinListPairType = SIRType.BuiltinList(pairType)
        val pairListRepr = LoweredValueRepresentation.constRepresentation(builtinListPairType)
        assert(
          pairListRepr.isInstanceOf[SumCaseClassRepresentation.SumPairBuiltinList],
          s"Expected SumPairBuiltinList but got $pairListRepr"
        )

        // BuiltinList[Integer] depends on flag
        val builtinListIntType = SIRType.BuiltinList(SIRType.Integer)
        val intListRepr = LoweredValueRepresentation.constRepresentation(builtinListIntType)
        val expectedIntRepr =
            if native then
                SumCaseClassRepresentation.SumBuiltinList(PrimitiveRepresentation.Constant)
            else SumCaseClassRepresentation.SumBuiltinList(PrimitiveRepresentation.PackedData)
        assert(
          intListRepr == expectedIntRepr,
          s"Expected $expectedIntRepr but got $intListRepr"
        )
    }

    // --- SumBuiltinList tests (flag-independent) ---

    test("SumBuiltinList structural equality") {
        import SumCaseClassRepresentation.*

        val dataList1 = SumBuiltinList(DataData)
        val dataList2 = SumBuiltinList(DataData)
        assert(dataList1 == dataList2)
        assert(dataList1.isInstanceOf[SumBuiltinList])

        val pairList1 = SumPairBuiltinList(
          PrimitiveRepresentation.PackedData,
          PrimitiveRepresentation.PackedData
        )
        val pairList2 = SumPairBuiltinList(
          PrimitiveRepresentation.PackedData,
          PrimitiveRepresentation.PackedData
        )
        assert(pairList1 == pairList2)
        assert(pairList1.isInstanceOf[SumPairBuiltinList])

        assert(dataList1 != pairList1)
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

        val dataList = SumBuiltinList(DataData)
        assert(dataList.isCompatibleWithType(listIntType))
        assert(dataList.isCompatibleWithType(listPairType))

        val pairList = SumPairBuiltinList(
          PrimitiveRepresentation.PackedData,
          PrimitiveRepresentation.PackedData
        )
        assert(pairList.isCompatibleWithType(listPairType))
        assert(!pairList.isCompatibleWithType(listIntType))

        assert(!dataList.isCompatibleWithType(SIRType.Integer))
        assert(!pairList.isCompatibleWithType(SIRType.Integer))
    }

    test("SumBuiltinList isPackedData and isDataCentric") {
        import SumCaseClassRepresentation.*

        val dataList = SumBuiltinList(DataData)
        assert(!dataList.isPackedData)
        assert(dataList.isDataCentric)

        val pairList = SumPairBuiltinList(
          PrimitiveRepresentation.PackedData,
          PrimitiveRepresentation.PackedData
        )
        assert(!pairList.isPackedData)
        assert(pairList.isDataCentric)

        val nativeList = SumBuiltinList(PrimitiveRepresentation.Constant)
        assert(!nativeList.isPackedData)
        assert(!nativeList.isDataCentric)
    }

    // --- Phase 2: Native element storage tests (nativeListElements=true only) ---

    test("BuiltinList[BigInt] preserves native Constant.List(Integer) [nativeListElements=true]") {
        given Options = optionsWithFlag(true)
        val sir = compile {
            BuiltinList[BigInt](1, 2, 3)
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
                        assert(elements.size == 3, s"Expected 3 elements but got ${elements.size}")
                    case other =>
                        fail(s"Expected Constant.List but got $other")
            case Result.Failure(e, _, _, _) =>
                fail(s"Evaluation failed: $e")
    }

    test("headList on native BuiltinList[BigInt] returns Integer [nativeListElements=true]") {
        given Options = optionsWithFlag(true)
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

    test("tailList on native BuiltinList[BigInt] [nativeListElements=true]") {
        given Options = optionsWithFlag(true)
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

    test("BuiltinList[ByteString] native literal [nativeListElements=true]") {
        given Options = optionsWithFlag(true)
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

    test("BuiltinList[String] native literal [nativeListElements=true]") {
        given Options = optionsWithFlag(true)
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

    test("BuiltinList[Boolean] native literal [nativeListElements=true]") {
        given Options = optionsWithFlag(true)
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
        val dataUplc = SumCaseClassRepresentation
            .SumBuiltinList(SumCaseClassRepresentation.DataData)
            .uplcType(listIntType)
        assert(
          dataUplc == SIRType.BuiltinList(SIRType.Data.tp),
          s"Expected BuiltinList[Data] but got ${dataUplc.show}"
        )
    }

    withBothFlags("List.get with intrinsic modules") { native =>
        import scalus.cardano.onchain.plutus.prelude.List
        import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}
        import scalus.uplc.*
        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = true,
          optimizeUplc = false,
          nativeListElements = native
        )
        val compiled = PlutusV3.compile {
            Cons(BigInt(1), Cons(BigInt(2), Nil)).get(BigInt(0))
        }
        val lowering = new SirToUplcV3Lowering(
          compiled.sir,
          generateErrorTraces = true,
          intrinsicModules = IntrinsicResolver.defaultIntrinsicModules,
          supportModules = IntrinsicResolver.defaultSupportModules,
          nativeListElements = native
        )
        val uplc = lowering.lower()
        val result = uplc.evaluateDebug
        result match
            case eval.Result.Success(term, _, _, _) => ()
            case f =>
                fail(s"get evaluation failed:\n$f")
    }

    withBothFlags("List.isEmpty with intrinsic modules") { native =>
        import scalus.cardano.onchain.plutus.prelude.List
        import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}
        import scalus.uplc.*
        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = true,
          optimizeUplc = true,
          nativeListElements = native
        )
        val compiled = PlutusV3.compile(!Cons(BigInt(1), Cons(BigInt(2), Nil)).isEmpty)
        val lowering = new SirToUplcV3Lowering(
          compiled.sir,
          generateErrorTraces = true,
          intrinsicModules = IntrinsicResolver.defaultIntrinsicModules,
          supportModules = IntrinsicResolver.defaultSupportModules,
          nativeListElements = native
        )
        val unoptimized = lowering.lower()
        val unoptResult = unoptimized.evaluateDebug
        unoptResult match
            case eval.Result.Success(term, _, _, _) =>
                assert(
                  term == Term.Const(Constant.Bool(true)),
                  s"Expected true but got ${term.show}"
                )
            case f =>
                fail(s"isEmpty evaluation failed:\n$f")
    }

    withBothFlags("List.single with nativeListElements") { native =>
        import scalus.cardano.onchain.plutus.prelude.List
        import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}
        import scalus.cardano.onchain.plutus.prelude.{===, Eq}
        import scalus.uplc.*
        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = true,
          optimizeUplc = true,
          nativeListElements = native
        )
        // Test 1: simple list construction and equality
        val compiled = PlutusV3.compile {
            List.single(BigInt(1)) === Cons(BigInt(1), Nil)
        }
        val result = compiled.program.term.evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                assert(
                  term == Term.Const(Constant.Bool(true)),
                  s"Expected true but got ${term.show}"
                )
            case Result.Failure(e, _, _, _) =>
                fail(s"List.single equality failed: $e")
    }

    withBothFlags("List.single with fromData argument") { native =>
        import scalus.cardano.onchain.plutus.prelude.List
        import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}
        import scalus.cardano.onchain.plutus.prelude.{===, Eq}
        import scalus.uplc.*
        import scalus.uplc.Term.asTerm
        import scalus.uplc.builtin.Data
        import scalus.uplc.builtin.Data.{toData, FromData}
        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = true,
          optimizeUplc = true,
          nativeListElements = native
        )
        // Test 2: list from Data argument compared with literal
        val compiled = PlutusV3.compile { (d: Data) =>
            val list = d.to[scalus.cardano.onchain.plutus.prelude.List[BigInt]]
            list === List.single(BigInt(1))
        }
        val arg = toData[scalus.cardano.onchain.plutus.prelude.List[BigInt]](
          List.single(BigInt(1))
        )
        val applied = Term.Apply(compiled.program.term, arg.asTerm)
        val result = applied.evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                assert(
                  term == Term.Const(Constant.Bool(true)),
                  s"Expected true but got ${term.show}"
                )
            case Result.Failure(ex, _, _, _) =>
                fail(s"List equality with fromData failed: $ex")
    }

    withBothFlags("List.quicksort") { native =>
        import scalus.cardano.onchain.plutus.prelude.List
        import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}
        import scalus.cardano.onchain.plutus.prelude.{===, Eq}
        import scalus.uplc.*
        import scalus.uplc.Term.asTerm
        import scalus.uplc.builtin.Data
        import scalus.uplc.builtin.Data.{toData, FromData}
        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = true,
          optimizeUplc = true,
          nativeListElements = native
        )
        val compiled = PlutusV3.compile { (d: Data) =>
            d.to[List[BigInt]].quicksort
        }
        val arg = toData[List[BigInt]](Cons(3, Cons(1, Cons(2, Nil))))
        val applied = Term.Apply(compiled.program.term, arg.asTerm)
        val result = applied.evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                val expected = PlutusV3.compile(
                  Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
                )
                val expectedTerm = expected.program.term.evaluate
                assert(term α_== expectedTerm, s"Expected $expectedTerm but got ${term.show}")
            case Result.Failure(ex, _, _, _) =>
                fail(s"quicksort failed: $ex")
    }

    withBothFlags("List.map2") { native =>
        import scalus.cardano.onchain.plutus.prelude.List
        import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}
        import scalus.cardano.onchain.plutus.prelude.{===, Eq}
        import scalus.uplc.*
        import scalus.uplc.Term.asTerm
        import scalus.uplc.builtin.Data
        import scalus.uplc.builtin.Data.{toData, FromData}
        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = true,
          optimizeUplc = true,
          nativeListElements = native
        )
        val compiled = PlutusV3.compile { (d: Data) =>
            val list = d.to[List[BigInt]]
            List.map2(list, Cons(BigInt(3), Cons(BigInt(4), Nil)))(_ + _)
        }
        val arg = toData[List[BigInt]](Cons(1, Cons(2, Nil)))
        val applied = Term.Apply(compiled.program.term, arg.asTerm)
        val result = applied.evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                val expected =
                    PlutusV3.compile(Cons(BigInt(4), Cons(BigInt(6), Nil)))
                val expectedTerm = expected.program.term.evaluate
                assert(term α_== expectedTerm, s"Expected $expectedTerm but got ${term.show}")
            case Result.Failure(ex, _, _, _) =>
                fail(s"map2 failed: $ex")
    }

    withBothFlags("List.zip") { native =>
        import scalus.cardano.onchain.plutus.prelude.List
        import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}
        import scalus.uplc.*
        import scalus.uplc.Term.asTerm
        import scalus.uplc.builtin.Data
        import scalus.uplc.builtin.Data.{toData, FromData}
        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = true,
          optimizeUplc = true,
          nativeListElements = native
        )
        val compiled = PlutusV3.compile { (d: Data) =>
            val list = d.to[List[BigInt]]
            list.zip(List.single(BigInt(2)))
        }
        val arg = toData[List[BigInt]](List.single(BigInt(1)))
        val applied = Term.Apply(compiled.program.term, arg.asTerm)
        val result = applied.evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                info(s"[native=$native] zip result: ${term.show}")
            case Result.Failure(ex, _, _, _) =>
                fail(s"zip failed: $ex")
    }

    withBothFlags("List.take") { native =>
        import scalus.cardano.onchain.plutus.prelude.List
        import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}
        import scalus.cardano.onchain.plutus.prelude.{===, Eq}
        import scalus.uplc.*
        import scalus.uplc.Term.asTerm
        import scalus.uplc.builtin.Data
        import scalus.uplc.builtin.Data.{toData, FromData}
        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = true,
          optimizeUplc = true,
          nativeListElements = native
        )
        val compiled = PlutusV3.compile { (d: Data) =>
            d.to[List[BigInt]].take(BigInt(2))
        }
        val arg = toData[List[BigInt]](Cons(1, Cons(2, Cons(3, Nil))))
        val applied = Term.Apply(compiled.program.term, arg.asTerm)
        val result = applied.evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                val expected = PlutusV3.compile(Cons(BigInt(1), Cons(BigInt(2), Nil)))
                val expectedTerm = expected.program.term.evaluate
                assert(term α_== expectedTerm, s"Expected $expectedTerm but got ${term.show}")
            case Result.Failure(ex, _, _, _) =>
                fail(s"take failed: $ex")
    }

    withBothFlags("List.lastOption") { native =>
        import scalus.cardano.onchain.plutus.prelude.List
        import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}
        import scalus.cardano.onchain.plutus.prelude.{Option as ScalusOption, ===, Eq}
        import scalus.uplc.*
        import scalus.uplc.Term.asTerm
        import scalus.uplc.builtin.Data
        import scalus.uplc.builtin.Data.{toData, FromData}
        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = true,
          optimizeUplc = true,
          nativeListElements = native
        )
        val compiled = PlutusV3.compile { (d: Data) =>
            d.to[List[BigInt]].lastOption
        }
        val arg = toData[List[BigInt]](Cons(1, Cons(2, Cons(3, Nil))))
        val applied = Term.Apply(compiled.program.term, arg.asTerm)
        val result = applied.evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                info(s"[native=$native] lastOption result: ${term.show}")
            case Result.Failure(ex, _, _, _) =>
                fail(s"lastOption failed: $ex")
    }

    // TODO: List[List[BigInt]].flatten fails with nativeListElements=true
    // because convertBuiltinList generates wrong nil type for nested lists.
    // The inner list nil should be list(list data) but gets list(data).
    test("List[List[BigInt]].flatten [nativeListElements=false]") {
        import scalus.cardano.onchain.plutus.prelude.List
        import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}
        import scalus.uplc.*
        import scalus.uplc.Term.asTerm
        import scalus.uplc.builtin.Data
        import scalus.uplc.builtin.Data.{toData, FromData}
        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = true,
          optimizeUplc = true,
          nativeListElements = false
        )
        val compiled = PlutusV3.compile { (d: Data) =>
            d.to[List[List[BigInt]]].flatten
        }
        val arg = toData[List[List[BigInt]]](
          List.single(Cons(3, Cons(1, Cons(2, Nil))))
        )
        val applied = Term.Apply(compiled.program.term, arg.asTerm)
        val result = applied.evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                info(s"flatten result: ${term.show}")
            case Result.Failure(ex, _, _, _) =>
                fail(s"flatten failed: $ex")
    }
}
