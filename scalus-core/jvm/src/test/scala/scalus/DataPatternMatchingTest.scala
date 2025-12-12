package scalus

import org.scalatest.funsuite.AnyFunSuite
import scalus.Compiler.compile
import scalus.builtin.{ByteString, Data}
import scalus.cardano.ledger.Language
import scalus.compiler.sir.{AnnotationsDecl, SIR, SIRType}
import scalus.prelude.List as PList
import scalus.uplc.*
import scalus.uplc.eval.PlutusVM
import scalus.uplc.Term.asTerm

class DataPatternMatchingTest extends AnyFunSuite:

    // Test data constructed outside compile block
    val testConstr42 =
        Data.Constr(BigInt(42), PList.from(List(Data.I(BigInt(1)), Data.I(BigInt(2)))))
    val testConstr0 =
        Data.Constr(BigInt(0), PList.from(List(Data.I(BigInt(100)), Data.I(BigInt(200)))))
    val testConstr1 =
        Data.Constr(
          BigInt(1),
          PList.from(List(Data.I(BigInt(10)), Data.I(BigInt(20)), Data.I(BigInt(30))))
        )
    val testDataI42 = Data.I(BigInt(42))
    val testDataB = Data.B(ByteString.fromHex("DEADBEEF"))
    val testDataList =
        Data.List(PList.from(List(Data.I(BigInt(42)), Data.I(BigInt(2)), Data.I(BigInt(3)))))
    val testDataMap =
        Data.Map(
          PList.from(
            List((Data.I(BigInt(1)), Data.I(BigInt(10))), (Data.I(BigInt(2)), Data.I(BigInt(20))))
          )
        )

    test("Pattern match on Data.Constr - extract constructor tag") {
        given PlutusVM = PlutusVM.makePlutusV4VM()
        given Compiler.Options = Compiler.Options.default.copy(targetLanguage = Language.PlutusV4)

        // Compile the pattern matching function
        val sirFun = compile { (d: Data) =>
            d match
                case Data.Constr(tag, _) => tag
                case _                   => BigInt(-1)
        }

        // Apply it to test data at runtime using $ operator
        val sirWithArg =
            sirFun $ SIR.Const(Constant.Data(testConstr42), SIRType.Data.tp, AnnotationsDecl.empty)
        val result = sirWithArg.toUplc().evaluate
        assert(result == 42.asTerm)
    }

    test("Pattern match on Data.I - extract integer") {
        given PlutusVM = PlutusVM.makePlutusV4VM()
        given Compiler.Options = Compiler.Options.default.copy(targetLanguage = Language.PlutusV4)

        val sirFun = compile { (d: Data) =>
            d match
                case Data.I(i) => i
                case _         => BigInt(-999)
        }

        val sirWithArg =
            sirFun $ SIR.Const(Constant.Data(testDataI42), SIRType.Data.tp, AnnotationsDecl.empty)
        val result = sirWithArg.toUplc().evaluate
        assert(result == 42.asTerm)
    }

    test("Pattern match on Data - type discrimination") {
        given PlutusVM = PlutusVM.makePlutusV4VM()
        given Compiler.Options = Compiler.Options.default.copy(targetLanguage = Language.PlutusV4)

        val sirFun = compile { (d: Data) =>
            d match
                case Data.Constr(_, _) => BigInt(0)
                case Data.Map(_)       => BigInt(1)
                case Data.List(_)      => BigInt(2)
                case Data.I(_)         => BigInt(3)
                case Data.B(_)         => BigInt(4)
        }

        val sirWithArg =
            sirFun $ SIR.Const(Constant.Data(testDataI42), SIRType.Data.tp, AnnotationsDecl.empty)
        val result = sirWithArg.toUplc().evaluate
        assert(result == 3.asTerm)
    }

    test("Pattern match on Data in V4 generates Case instruction") {
        given PlutusVM = PlutusVM.makePlutusV4VM()
        given Compiler.Options = Compiler.Options.default.copy(targetLanguage = Language.PlutusV4)

        val sirFun = compile { (d: Data) =>
            d match
                case Data.I(i) => i
                case _         => BigInt(-1)
        }

        val uplc = sirFun.toUplc()
        val uplcStr = uplc.pretty.render(200)

        assert(
          uplcStr.contains("case"),
          s"Expected Case instruction in UPLC for PlutusV4, got:\n$uplcStr"
        )
    }

    // ============================================================================
    // Tests for constructing Data variants (genConstr)
    // ============================================================================

    test("Construct Data.I inside compile block") {
        given PlutusVM = PlutusVM.makePlutusV4VM()
        given Compiler.Options = Compiler.Options.default.copy(targetLanguage = Language.PlutusV4)

        // Use a function to construct Data.I from an Integer parameter
        val sirFun = compile { (i: BigInt) =>
            Data.I(i)
        }

        val sirWithArg =
            sirFun $ SIR.Const(Constant.Integer(42), SIRType.Integer, AnnotationsDecl.empty)
        val result = sirWithArg.toUplc().evaluate
        assert(result == Term.Const(Constant.Data(Data.I(BigInt(42)))))
    }

    test("Construct Data.B inside compile block") {
        given PlutusVM = PlutusVM.makePlutusV4VM()
        given Compiler.Options = Compiler.Options.default.copy(targetLanguage = Language.PlutusV4)

        // Use a function to construct Data.B from a ByteString parameter
        val sirFun = compile { (bs: ByteString) =>
            Data.B(bs)
        }

        val testBytes = ByteString.fromHex("DEADBEEF")
        val sirWithArg =
            sirFun $ SIR.Const(
              Constant.ByteString(testBytes),
              SIRType.ByteString,
              AnnotationsDecl.empty
            )
        val result = sirWithArg.toUplc().evaluate
        assert(result == Term.Const(Constant.Data(Data.B(testBytes))))
    }

    test("Construct Data.List inside compile block") {
        given PlutusVM = PlutusVM.makePlutusV4VM()
        given Compiler.Options = Compiler.Options.default.copy(targetLanguage = Language.PlutusV4)

        // Use a function to construct Data.List from a list parameter
        val sirFun = compile { (elements: scalus.prelude.List[Data]) =>
            Data.List(elements)
        }

        val inputList = scalus.prelude.List[Data](Data.I(BigInt(1)), Data.I(BigInt(2)))
        val inputConst =
            Constant.List(DefaultUni.Data, inputList.toScalaList.map(Constant.Data.apply))
        val sirWithArg =
            sirFun $ SIR.Const(inputConst, SIRType.List(SIRType.Data.tp), AnnotationsDecl.empty)
        val result = sirWithArg.toUplc().evaluate
        val expected = Data.List(PList.from(List(Data.I(BigInt(1)), Data.I(BigInt(2)))))
        assert(result == Term.Const(Constant.Data(expected)))
    }

    test("Construct Data.Map inside compile block") {
        given PlutusVM = PlutusVM.makePlutusV4VM()
        given Compiler.Options = Compiler.Options.default.copy(targetLanguage = Language.PlutusV4)

        // Use a function to construct Data.Map from a list of pairs parameter
        val sirFun = compile { (entries: scalus.prelude.List[(Data, Data)]) =>
            Data.Map(entries)
        }

        val inputPairs = List(
          (Data.I(BigInt(1)), Data.I(BigInt(10))),
          (Data.I(BigInt(2)), Data.I(BigInt(20)))
        )
        val pairType =
            DefaultUni.Apply(
              DefaultUni.Apply(DefaultUni.ProtoPair, DefaultUni.Data),
              DefaultUni.Data
            )
        val inputConst = Constant.List(
          pairType,
          inputPairs.map { case (k, v) => Constant.Pair(Constant.Data(k), Constant.Data(v)) }
        )
        val sirWithArg = sirFun $ SIR.Const(
          inputConst,
          SIRType.List(SIRType.Tuple2(SIRType.Data.tp, SIRType.Data.tp)),
          AnnotationsDecl.empty
        )
        val result = sirWithArg.toUplc().evaluate
        val expected = Data.Map(PList.from(inputPairs))
        assert(result == Term.Const(Constant.Data(expected)))
    }

    test("Construct Data.Constr inside compile block") {
        given PlutusVM = PlutusVM.makePlutusV4VM()
        given Compiler.Options = Compiler.Options.default.copy(targetLanguage = Language.PlutusV4)

        // Use a function to construct Data.Constr from tag and args parameters
        val sirFun = compile { (tag: BigInt, args: scalus.prelude.List[Data]) =>
            Data.Constr(tag, args)
        }

        val inputArgs = scalus.prelude.List[Data](Data.I(BigInt(100)), Data.I(BigInt(200)))
        val argsConst =
            Constant.List(DefaultUni.Data, inputArgs.toScalaList.map(Constant.Data.apply))

        // Apply tag first, then args
        val sirWithTag =
            sirFun $ SIR.Const(Constant.Integer(0), SIRType.Integer, AnnotationsDecl.empty)
        val sirWithArgs =
            sirWithTag $ SIR.Const(argsConst, SIRType.List(SIRType.Data.tp), AnnotationsDecl.empty)

        val result = sirWithArgs.toUplc().evaluate
        val expected =
            Data.Constr(BigInt(0), PList.from(List(Data.I(BigInt(100)), Data.I(BigInt(200)))))
        assert(result == Term.Const(Constant.Data(expected)))
    }

    test("Construct and immediately match on Data.I") {
        given PlutusVM = PlutusVM.makePlutusV4VM()
        given Compiler.Options = Compiler.Options.default.copy(targetLanguage = Language.PlutusV4)

        val sirFun = compile { (i: BigInt) =>
            val d: Data = Data.I(i)
            d match
                case Data.I(x) => x
                case _         => BigInt(-1)
        }

        val sirWithArg =
            sirFun $ SIR.Const(Constant.Integer(42), SIRType.Integer, AnnotationsDecl.empty)
        val result = sirWithArg.toUplc().evaluate
        assert(result == 42.asTerm)
    }

    test("Construct and immediately match on Data.B") {
        given PlutusVM = PlutusVM.makePlutusV4VM()
        given Compiler.Options = Compiler.Options.default.copy(targetLanguage = Language.PlutusV4)

        val testBytes = ByteString.fromHex("CAFE")
        val sirFun = compile { (bs: ByteString) =>
            val d: Data = Data.B(bs)
            d match
                case Data.B(x) => x
                case _         => ByteString.empty
        }

        val sirWithArg =
            sirFun $ SIR.Const(
              Constant.ByteString(testBytes),
              SIRType.ByteString,
              AnnotationsDecl.empty
            )
        val result = sirWithArg.toUplc().evaluate
        assert(result == Term.Const(Constant.ByteString(testBytes)))
    }

    // ============================================================================
    // Tests for field selection on Data subtypes
    // ============================================================================

    test("Select value field from Data.I") {
        given PlutusVM = PlutusVM.makePlutusV4VM()
        given Compiler.Options = Compiler.Options.default.copy(targetLanguage = Language.PlutusV4)

        val sirFun = compile { (i: BigInt) =>
            val x = Data.I(i)
            x.value
        }

        val sirWithArg =
            sirFun $ SIR.Const(Constant.Integer(42), SIRType.Integer, AnnotationsDecl.empty)
        val result = sirWithArg.toUplc().evaluate
        assert(result == 42.asTerm)
    }

    test("Select value field from Data.B") {
        given PlutusVM = PlutusVM.makePlutusV4VM()
        given Compiler.Options = Compiler.Options.default.copy(targetLanguage = Language.PlutusV4)

        val testBytes = ByteString.fromHex("DEADBEEF")
        val sirFun = compile { (bs: ByteString) =>
            val x = Data.B(bs)
            x.value
        }

        val sirWithArg =
            sirFun $ SIR.Const(
              Constant.ByteString(testBytes),
              SIRType.ByteString,
              AnnotationsDecl.empty
            )
        val result = sirWithArg.toUplc().evaluate
        assert(result == Term.Const(Constant.ByteString(testBytes)))
    }

    test("Select values field from Data.List") {
        given PlutusVM = PlutusVM.makePlutusV4VM()
        given Compiler.Options = Compiler.Options.default.copy(targetLanguage = Language.PlutusV4)

        val inputList = scalus.prelude.List[Data](Data.I(BigInt(1)), Data.I(BigInt(2)))
        val sirFun = compile { (elements: scalus.prelude.List[Data]) =>
            val x = Data.List(elements)
            x.values
        }

        val inputConst =
            Constant.List(DefaultUni.Data, inputList.toScalaList.map(Constant.Data.apply))
        val sirWithArg =
            sirFun $ SIR.Const(inputConst, SIRType.List(SIRType.Data.tp), AnnotationsDecl.empty)
        val result = sirWithArg.toUplc().evaluate
        assert(result == Term.Const(inputConst))
    }

    test("Select values field from Data.Map") {
        given PlutusVM = PlutusVM.makePlutusV4VM()
        given Compiler.Options = Compiler.Options.default.copy(targetLanguage = Language.PlutusV4)

        val inputPairs = List(
          (Data.I(BigInt(1)), Data.I(BigInt(10))),
          (Data.I(BigInt(2)), Data.I(BigInt(20)))
        )
        val sirFun = compile { (entries: scalus.prelude.List[(Data, Data)]) =>
            val x = Data.Map(entries)
            x.values
        }

        val pairType =
            DefaultUni.Apply(
              DefaultUni.Apply(DefaultUni.ProtoPair, DefaultUni.Data),
              DefaultUni.Data
            )
        val inputConst = Constant.List(
          pairType,
          inputPairs.map { case (k, v) => Constant.Pair(Constant.Data(k), Constant.Data(v)) }
        )
        val sirWithArg = sirFun $ SIR.Const(
          inputConst,
          SIRType.List(SIRType.Tuple2(SIRType.Data.tp, SIRType.Data.tp)),
          AnnotationsDecl.empty
        )
        val result = sirWithArg.toUplc().evaluate
        // Map values are stored as Data.Constr(0, [key, value]) pairs
        val expectedConst = Constant.List(
          DefaultUni.Data,
          inputPairs.map { case (k, v) =>
              Constant.Data(Data.Constr(BigInt(0), PList.from(List(k, v))))
          }
        )
        assert(result == Term.Const(expectedConst))
    }

    test("Select constr field from Data.Constr") {
        given PlutusVM = PlutusVM.makePlutusV4VM()
        given Compiler.Options = Compiler.Options.default.copy(targetLanguage = Language.PlutusV4)

        val sirFun = compile { (tag: BigInt, args: scalus.prelude.List[Data]) =>
            val x = Data.Constr(tag, args)
            x.constr
        }

        val inputArgs = scalus.prelude.List[Data](Data.I(BigInt(100)), Data.I(BigInt(200)))
        val argsConst =
            Constant.List(DefaultUni.Data, inputArgs.toScalaList.map(Constant.Data.apply))

        val sirWithTag =
            sirFun $ SIR.Const(Constant.Integer(42), SIRType.Integer, AnnotationsDecl.empty)
        val sirWithArgs =
            sirWithTag $ SIR.Const(argsConst, SIRType.List(SIRType.Data.tp), AnnotationsDecl.empty)

        val result = sirWithArgs.toUplc().evaluate
        assert(result == 42.asTerm)
    }

    test("Select args field from Data.Constr") {
        given PlutusVM = PlutusVM.makePlutusV4VM()
        given Compiler.Options = Compiler.Options.default.copy(targetLanguage = Language.PlutusV4)

        val sirFun = compile { (tag: BigInt, args: scalus.prelude.List[Data]) =>
            val x = Data.Constr(tag, args)
            x.args
        }

        val inputArgs = scalus.prelude.List[Data](Data.I(BigInt(100)), Data.I(BigInt(200)))
        val argsConst =
            Constant.List(DefaultUni.Data, inputArgs.toScalaList.map(Constant.Data.apply))

        val sirWithTag =
            sirFun $ SIR.Const(Constant.Integer(42), SIRType.Integer, AnnotationsDecl.empty)
        val sirWithArgs =
            sirWithTag $ SIR.Const(argsConst, SIRType.List(SIRType.Data.tp), AnnotationsDecl.empty)

        val result = sirWithArgs.toUplc().evaluate
        assert(result == Term.Const(argsConst))
    }
