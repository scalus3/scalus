package scalus.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.builtin.ByteString.*
import scalus.builtin.Data.I
import scalus.sir.SIR.Pattern
import scalus.sir.SIRType.{FreeUnificator, SumCaseClass, TypeVar}
import scalus.sir.*
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.uplc.*
import scalus.uplc.test.ArbitraryInstances

import scala.language.implicitConversions

class SirToUplcSmLoweringTest
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances:

    extension (sir: SIR)
        infix def lowersTo(r: Term): Unit =
            // assert(SimpleSirToUplcLowering(sir, generateErrorTraces = false).lower() == r)
            assert(SirToUplcV3Lowering(sir, generateErrorTraces = false).lower().alphaEq(r))

    def lower(
        sir: SIR,
        generateErrorTraces: Boolean = true,
        upcastTo: SIRType = SIRType.FreeUnificator,
        representation: LoweredValueRepresentation = TypeVarRepresentation(true)
    ): Term =
        SirToUplcV3Lowering(
          sir,
          generateErrorTraces = generateErrorTraces,
          upcastTo = upcastTo,
          representation = representation,
          debug = false,
        ).lower()

    extension (term: Term)
        infix def alphaEq(other: Term): Boolean =
            DeBruijn.deBruijnTerm(term) α_== DeBruijn.deBruijnTerm(other)

    private val ae = AnnotationsDecl.empty

    given PlutusVM = PlutusVM.makePlutusV3VM()

    test("lower constant") {
        forAll { (c: Constant) =>
            SIR.Const(c, SIRType.Integer, ae) lowersTo Term.Const(c)
        }
    }

    test("lower error") {
        SIR.Error("error", ae) lowersTo Term.Error
        assert(
          SIR.Error("error", ae)
              .toUplc(
                generateErrorTraces = true,
                optimizeUplc = false
              ) == !(!Trace $ "error" $ ~Term.Error)
        )
    }

    test("lower Var in let") {
        // now we can't lower var in isolation, so should use Var in some context
        val sir = SIR.Let(
          List(
            Binding(
              "x",
              SIRType.ByteString,
              SIR.Const(asConstant(hex"DEADBEEF"), SIRType.ByteString, ae)
            )
          ),
          SIR.Var("x", SIRType.ByteString, ae),
          SIR.LetFlags.None,
          ae
        )
        val uplc = lower(sir)
        val expected = LamAbs(
          "x",
          Var(NamedDeBruijn("x")),
        ) $ asConstant(hex"DEADBEEF")
        // println(s"Lowered SIR: ${uplc.pretty.render(100)}")
        // println(s"Expected UPLC: ${expected.pretty.render(100)}")
        assert(uplc alphaEq expected)
        // lowersTo vr"x"
    }

    test("lower Lam/Apply with builtin type-vars (non-scal)") {
        import SIRType.{TypeLambda, TypeVar, Unit}
        val idType = TypeLambda(List(TypeVar("A", Some(1), false)), TypeVar("A", Some(1), false))
        val x = SIR.Var("x", TypeVar("X", Some(2), true), ae)

        val sir = SIR.Apply(
          SIR.LamAbs(x, x, List.empty, ae),
          SIR.Const(Constant.Unit, Unit, ae),
          Unit,
          ae
        )
        val uplc = lower(sir)
        val expected = lam("x")(vr"x") $ Constant.Unit
        assert(uplc alphaEq expected)
    }

    test("lower Lam/Apply with scala type-vars") {
        import SIRType.{TypeLambda, TypeVar, Unit}
        val idType = TypeLambda(List(TypeVar("A", Some(1), false)), TypeVar("A", Some(1), false))
        val x = SIR.Var("x", TypeVar("X", Some(2), false), ae)

        val sir = SIR.Apply(
          SIR.LamAbs(x, x, List.empty, ae),
          SIR.Const(Constant.Unit, Unit, ae),
          Unit,
          ae
        )
        val uplc = lower(sir)
        // sscala type-vars are represented as Data.
        println("uplc = " + uplc.pretty.render(100))
        val expected = lam("x1")(vr"x1") $ Constant.Unit
        assert(uplc alphaEq expected)
    }

    test("lower builtins") {
        SIRBuiltins.addInteger lowersTo AddInteger
        SIRBuiltins.headList lowersTo !HeadList
        SIRBuiltins.fstPair lowersTo !(!FstPair)
    }

    test("lower let") {
        import SIRType.{Fun, Integer}
        /* let x = 1 in
       let y = 2 in x + y
       lowers to (\x -> (\y -> x + y) 2) 1
         */
        SIR.Let(
          Binding("x", Integer, SIR.Const(asConstant(1), Integer, ae)) :: Binding(
            "y",
            Integer,
            SIR.Const(asConstant(2), Integer, ae)
          ) :: Nil,
          SIR.Apply(
            SIR.Apply(
              SIRBuiltins.addInteger,
              SIR.Var("x", Integer, ae),
              Fun(Integer, Integer),
              ae
            ),
            SIR.Var("y", Integer, ae),
            Integer,
            ae
          ),
          SIR.LetFlags.None,
          ae
        ) lowersTo (lam("x")(lam("y")(AddInteger $ vr"x" $ vr"y") $ 2) $ 1)
    }

    test("lower Constr") {
        import SIRType.{TypeVar, TypeProxy, ByteString}
        /* Nil
       lowers to (\Nil Cons -> force Nil)
       TxId(name)
       lowers to (\name TxId -> TxId name) name
         */
        val a1TypeVar = TypeVar("A", Some(1), false)
        val a2TypeVar = TypeVar("A", Some(2), false)
        val tailTypeProxy = new TypeProxy(null)
        val listData =
            DataDecl(
              "scalus.prelude.List",
              List(
                ConstrDecl(
                  "scalus.prelude.List$.Nil",
                  List(),
                  List(),
                  List(SIRType.TypeNothing),
                  ae
                ),
                ConstrDecl(
                  "scalus.prelude.List$.Cons",
                  List(TypeBinding("head", a2TypeVar), TypeBinding("tail", tailTypeProxy)),
                  List(a2TypeVar),
                  List(a2TypeVar),
                  ae
                )
              ),
              List(a1TypeVar),
              ae
            )

        tailTypeProxy.ref = SumCaseClass(listData, List(a2TypeVar))

        val txIdData = DataDecl(
          "TxId",
          List(
            ConstrDecl("TxId", List(TypeBinding("hash", ByteString)), List(), List(), ae)
          ),
          List(),
          ae
        )
        def withDecls(sir: SIR) = SIR.Decl(listData, SIR.Decl(txIdData, sir))
        val originSir1 = withDecls(
          SIR.Constr(
            "scalus.prelude.List$.Nil",
            listData,
            List(),
            listData.constrType("scalus.prelude.List$.Nil"),
            ae
          )
        )

        // println(s"oriignSir1.tp = ${originSir1.tp.show}")
        val gen1 = scalus.sir.lowering.typegens.SirTypeUplcGenerator(originSir1.tp)
        given LoweringContext = LoweringContext()
        val representation1 = gen1.defaultRepresentation(originSir1.tp)
        assert(representation1 == scalus.sir.lowering.SumCaseClassRepresentation.SumDataList)

        val genDataList =
            scalus.sir.lowering.typegens.SirTypeUplcGenerator(SIRType.List(SIRType.Data))

        val origin1 = lower(originSir1)
        val expected1 = Term.Builtin(DefaultFun.MkNilData) $ Term.Const(Constant.Unit)
        // println(s"Lowered SIR: ${origin1.pretty.render(100)}")
        // println(s"Expected UPLC: ${expected1.pretty.render(100)}")

        assert(origin1 alphaEq expected1)

        val result1 = origin1.evaluateDebug
        result1 match {
            case Result.Success(term, _, _, _) =>
                val nilData = scalus.prelude.List.empty[scalus.builtin.Data].toData
                val termValue = term match {
                    case Term.Const(Constant.List(DefaultUni.Data, value)) => value
                    case _ => fail(s"Expected a List constant, got: ${term}")
                }
                /// assert(termValue == nilData)
                assert(term == Term.Const(Constant.List(scalus.uplc.DefaultUni.Data, List())))
            case _ =>
                fail(s"Expected success, got: ${result1}")
        }

        val originSir2 = withDecls(
          SIR.Constr(
            "TxId",
            txIdData,
            List(SIR.Const(asConstant(hex"DEADBEEF"), ByteString, ae)),
            txIdData.constrType("TxId"),
            ae
          )
        )

        val uplc2 = lower(originSir2)

        val gen2 = scalus.sir.lowering.typegens.SirTypeUplcGenerator(originSir2.tp)
        val representation2 = gen2.defaultRepresentation
        // println(s"gen2 = $gen2, representation2=${representation2}")

        // lowersTo (lam("hash", "TxId")(vr"TxId" $ vr"hash") $ hex"DEADBEEF")

    }

    test("lower And, Or, Not") {
        // And True False
        // lowers to (\True False -> And True False) True False
        //

        val a = SIR.Var("a", SIRType.Boolean, ae)
        val b = SIR.Var("b", SIRType.Boolean, ae)

        def withLet(a: Boolean = true, b: Boolean = true)(f: => SIR): SIR = {
            SIR.Let(
              List(
                Binding("a", SIRType.Boolean, SIR.Const(Constant.Bool(a), SIRType.Boolean, ae)),
                Binding("b", SIRType.Boolean, SIR.Const(Constant.Bool(b), SIRType.Boolean, ae))
              ),
              f,
              SIR.LetFlags.None,
              ae
            )
        }

        def withLambda(f: => SIR): SIR = {
            SIR.LamAbs(
              SIR.Var("a", SIRType.Boolean, ae),
              SIR.LamAbs(SIR.Var("b", SIRType.Boolean, ae), f, List.empty, ae),
              List.empty,
              ae
            )
        }

        // SIR.And(a, b, ae) lowersTo !(!IfThenElse $ vr"a" $ ~vr"b" $ ~false)
        val andLet1 = withLet(true, true) {
            SIR.And(a, b, ae)
        }
        val andLetUplc1 = lower(andLet1)

        val expected1 = lam("a")(
          lam("b")(!(!IfThenElse $ vr"a" $ ~vr"b" $ ~false)) $ Constant.Bool(
            true
          )
        ) $ Constant.Bool(true)

        assert(
          andLetUplc1 alphaEq expected1
        )
        val resultAnd1 = andLetUplc1.evaluateDebug
        resultAnd1 match {
            case Result.Success(term, _, _, _) =>
                assert(term == Term.Const(Constant.Bool(true)))
            case _ =>
                fail(s"Expected success, got: ${resultAnd1}")
        }

        val andLambda1 = withLambda(SIR.And(a, b, ae))
        val andLambdaUplc1 = lower(andLambda1)
        val expectedLambda1 = lam("a")(
          lam("b")(!(!IfThenElse $ vr"a" $ ~vr"b" $ ~false))
        )

        assert(
          andLambdaUplc1 alphaEq expectedLambda1
        )

        // SIR.Or(a, b, ae) lowersTo !(!IfThenElse $ vr"a" $ ~true $ ~vr"b")
        val orLambda1 = withLambda(SIR.Or(a, b, ae))
        val orLambdaUplc1 = lower(orLambda1)
        val expectedOr1 = lam("a")(
          lam("b")(!(!IfThenElse $ vr"a" $ ~true $ ~vr"b"))
        )
        assert(
          orLambdaUplc1 alphaEq expectedOr1
        )

        // SIR.Not(a, ae) lowersTo !(!IfThenElse $ vr"a" $ ~false $ ~true)
        val nonLambda1 = SIR.LamAbs(a, SIR.Not(a, ae), List.empty, ae)
        val nonLambdaUplc1 = lower(nonLambda1)
        val expectedNon1 = lam("a")(!(!IfThenElse $ vr"a" $ ~false $ ~true))
        assert(
          nonLambdaUplc1 alphaEq expectedNon1
        )

    }

    test("lower Match / List[?]") {
        /* Nil match
        case Nil -> 1
        case Cons(h, tl) -> 2
        // constructors are sorted by their order of declaration
        //   in this tests this is (Nil, Cons) [see below]
        lowers to (\Nil Cons -> force Nil) (delay 1) (\h tl -> 2)
         */
        val tailTypeProxy = new SIRType.TypeProxy(null)
        val a1TypeVar = SIRType.TypeVar("A1", Some(1), false)
        val a2TypeVar = SIRType.TypeVar("A2", Some(2), false)
        val nilConstr = ConstrDecl(
          "scalus.prelude.List$.Nil",
          List(),
          List(),
          List(SIRType.TypeNothing),
          ae
        )
        val consConstr = ConstrDecl(
          "scalus.prelude.List$.Cons",
          List(TypeBinding("head", a2TypeVar), TypeBinding("tail", tailTypeProxy)),
          List(a2TypeVar),
          List(a2TypeVar),
          ae
        )
        val listData =
            DataDecl("scalus.prelude.List", List(nilConstr, consConstr), List(a1TypeVar), ae)
        tailTypeProxy.ref = SumCaseClass(listData, List(a2TypeVar))

        val txIdData = DataDecl(
          "TxId",
          List(
            ConstrDecl(
              "TxId",
              List(TypeBinding("hash", SIRType.ByteString)),
              List(),
              List(),
              ae
            )
          ),
          List(),
          ae
        )

        def withDecls(sir: SIR) = SIR.Decl(listData, SIR.Decl(txIdData, sir))

        val listAnyType = SIRType.SumCaseClass(listData, List(FreeUnificator))

        val sirMatch1 = withDecls(
          SIR.Match(
            SIR.Constr(
              "scalus.prelude.List$.Nil",
              listData,
              List(),
              listData.constrType("scalus.prelude.List$.Nil"),
              ae
            ),
            List(
              SIR.Case(
                Pattern.Constr(nilConstr, Nil, Nil),
                SIR.Const(Constant.Integer(1), SIRType.Integer, ae),
                ae
              ),
              SIR.Case(
                Pattern
                    .Constr(consConstr, List("h", "tl"), List(SIRType.FreeUnificator, listAnyType)),
                SIR.Const(Constant.Integer(2), SIRType.Integer, ae),
                ae
              )
            ),
            SIRType.Integer,
            ae
          )
        )

        val sirMatchUplc1 = lower(sirMatch1)

        // println(s"Lowered SIR: ${sirMatchUplc1.pretty.render(100)}")

        val uplcExpected1 = lam("Nil", "Cons")(!vr"Nil") $ ~asConstant(1) $ lam("h", "tl")(2)
        val uplcExpected = asConstant(1)

        assert(
          sirMatchUplc1 alphaEq uplcExpected
        )

        // To really check match we should put inti into lambda
        val sirMath2 = withDecls(
          SIR.LamAbs(
            SIR.Var("scrutinee", listAnyType, ae),
            SIR.Match(
              SIR.Var("scrutinee", listAnyType, ae),
              List(
                SIR.Case(
                  Pattern.Constr(nilConstr, Nil, Nil),
                  SIR.Const(Constant.Integer(1), SIRType.Integer, ae),
                  ae
                ),
                SIR.Case(
                  Pattern
                      .Constr(
                        consConstr,
                        List("h", "tl"),
                        List(SIRType.FreeUnificator, listAnyType)
                      ),
                  SIR.Const(Constant.Integer(2), SIRType.Integer, ae),
                  ae
                )
              ),
              SIRType.Integer,
              ae
            ),
            List.empty,
            ae
          )
        )

        val sirMatchUplc2 = lower(sirMath2)
        // println(s"match2:  ${sirMatchUplc2.pretty.render(100)}")

        val sirMatchExpectedUplc2 = lam("scrutinee")(
          !(!(!Term.Builtin(DefaultFun.ChooseList)) $ vr"scrutinee" $ ~asConstant(1) $ ~asConstant(
            2
          )),
        )

        assert(
          sirMatchUplc2 alphaEq sirMatchExpectedUplc2
        )

        val matchArgumentSIR = SIR.Constr(
          "scalus.prelude.List$.Nil",
          listData,
          List(),
          listData.constrType("scalus.prelude.List$.Nil"),
          ae
        )

        val argumentUplc = lower(matchArgumentSIR)

        val uplcApplied = sirMatchUplc2 $ argumentUplc

        val resultMatchUplc2 = uplcApplied.evaluateDebug

        resultMatchUplc2 match {
            case Result.Success(term, _, _, _) =>
                assert(term == Term.Const(Constant.Integer(1)))
            case _ =>
                fail(s"Expected success, got: ${resultMatchUplc2}")
        }

    }

    test("lower Match / SumCaseClass for Option[Int]") {

        val someTypeProxy = new SIRType.TypeProxy(null)
        val aTypeVar = SIRType.TypeVar("A", Some(1), false)
        val bTypeVar = SIRType.TypeVar("B", Some(2), false)
        val noneConstr = ConstrDecl(
          "scalus.prelude.Option$.None",
          List(),
          List(),
          List(SIRType.TypeNothing),
          ae
        )
        val someConstr = ConstrDecl(
          "scalus.prelude.Option$.Some",
          List(TypeBinding("value", aTypeVar)),
          List(aTypeVar),
          List(aTypeVar),
          ae
        )
        val optionDataDecl =
            DataDecl("scalus.prelude.Option", List(noneConstr, someConstr), List(bTypeVar), ae)

        def withOptionDecl(sir: SIR) = SIR.Decl(optionDataDecl, sir)

        val sirMatch1 = withOptionDecl(
          SIR.Match(
            SIR.Constr(
              "scalus.prelude.Option$.Some",
              optionDataDecl,
              List(SIR.Const(Constant.Integer(42), SIRType.Integer, ae)),
              optionDataDecl.constrType("scalus.prelude.Option$.Some"),
              ae
            ),
            List(
              SIR.Case(
                Pattern.Constr(noneConstr, Nil, Nil),
                SIR.Const(Constant.Integer(1), SIRType.Integer, ae),
                ae
              ),
              SIR.Case(
                Pattern
                    .Constr(someConstr, List("value"), List(SIRType.FreeUnificator)),
                SIR.Var("value", aTypeVar, ae),
                ae
              )
            ),
            SIRType.Integer,
            ae
          )
        )

        val sirMatchUplc1 = lower(sirMatch1)

        // println(s"Lowered SIR: ${sirMatchUplc1.pretty.render(100)}")

        val uplcExpected1 = !DefaultFun.HeadList $ (
          !DefaultFun.MkCons $
              (DefaultFun.IData $ Term.Const(Constant.Integer(42)))
              $ Term.Builtin(DefaultFun.MkNilData)
        )

        val result1 = sirMatchUplc1.evaluateDebug

        // println(s"result1: ${result1}")

        result1 match {
            case Result.Success(term, _, _, _) =>
                // from option - in PackedData representation.
                assert(term == Term.Const(Constant.Data(I(42))))
            case _ =>
                fail(s"Expected success, got: ${result1}")
        }

        def optionType(a: SIRType) = SIRType.typeApply(optionDataDecl.tp, List(a))

        val scrutineeVar = SIR.Var(
          "scrutinee",
          SIRType.SumCaseClass(optionDataDecl, List(SIRType.Integer)),
          ae
        )

        val sirMatch2 = withOptionDecl(
          SIR.LamAbs(
            scrutineeVar,
            SIR.Match(
              scrutineeVar,
              List(
                SIR.Case(
                  Pattern.Constr(noneConstr, Nil, Nil),
                  SIR.Const(Constant.Integer(1), SIRType.Integer, ae),
                  ae
                ),
                SIR.Case(
                  Pattern
                      .Constr(someConstr, List("value"), List(SIRType.FreeUnificator)),
                  SIR.Var("value", aTypeVar, ae),
                  ae
                )
              ),
              SIRType.Integer,
              ae
            ),
            List.empty,
            ae
          )
        )
        // println(s"sirMatch2: ${sirMatch2.pretty.render(100)}")

        val uplcMatch2 = lower(sirMatch2)

        // println("lowered match2: " + uplcMatch2.pretty.render(100))

        val arg1Sir = SIR.Constr(
          "scalus.prelude.Option$.Some",
          optionDataDecl,
          List(SIR.Const(Constant.Integer(42), SIRType.Integer, ae)),
          SIRType.typeApply(
            optionDataDecl.constrType("scalus.prelude.Option$.Some"),
            SIRType.Integer :: Nil
          ),
          ae
        )

        val genArg1 = scalus.sir.lowering.typegens.SirTypeUplcGenerator(arg1Sir.tp)
        // println("genArg1 = " + genArg1)

        val arg1 = lower(arg1Sir, upcastTo = optionType(SIRType.Integer))

        // println("arg1=" + arg1.pretty.render(100))

        val result2 = (uplcMatch2 $ arg1).evaluateDebug

        result2 match {
            case Result.Success(term, _, _, _) =>
                // from option - in PackedData representation.
                assert(term == Term.Const(Constant.Data(I(42))))
            case _ =>
                fail(s"Expected success, got: ${result2}")
        }

    }

    test("lower constant pattern match on Integer") {
        // match x with
        //   case 1 => 10
        //   case 2 => 20
        //   case _ => 30
        val scrutineeVar = SIR.Var("x", SIRType.Integer, ae)
        val matchExpr = SIR.Match(
          scrutineeVar,
          List(
            SIR.Case(
              SIR.Pattern.Const(SIR.Const(Constant.Integer(1), SIRType.Integer, ae)),
              SIR.Const(Constant.Integer(10), SIRType.Integer, ae),
              ae
            ),
            SIR.Case(
              SIR.Pattern.Const(SIR.Const(Constant.Integer(2), SIRType.Integer, ae)),
              SIR.Const(Constant.Integer(20), SIRType.Integer, ae),
              ae
            ),
            SIR.Case(
              SIR.Pattern.Wildcard,
              SIR.Const(Constant.Integer(30), SIRType.Integer, ae),
              ae
            )
          ),
          SIRType.Integer,
          ae
        )

        val sirWithLet = SIR.Let(
          List(Binding("x", SIRType.Integer, SIR.Const(Constant.Integer(1), SIRType.Integer, ae))),
          matchExpr,
          SIR.LetFlags.None,
          ae
        )

        val uplc = lower(sirWithLet)
        val result = uplc.evaluateDebug

        result match {
            case Result.Success(term, _, _, _) =>
                assert(term == Term.Const(Constant.Integer(10)))
            case _ =>
                fail(s"Expected success with result 10, got: ${result}")
        }

        // Test with x = 2
        val sirWithLet2 = SIR.Let(
          List(Binding("x", SIRType.Integer, SIR.Const(Constant.Integer(2), SIRType.Integer, ae))),
          matchExpr,
          SIR.LetFlags.None,
          ae
        )

        val result2 = lower(sirWithLet2).evaluateDebug
        result2 match {
            case Result.Success(term, _, _, _) =>
                assert(term == Term.Const(Constant.Integer(20)))
            case _ =>
                fail(s"Expected success with result 20, got: ${result2}")
        }

        // Test with x = 5 (wildcard case)
        val sirWithLet3 = SIR.Let(
          List(Binding("x", SIRType.Integer, SIR.Const(Constant.Integer(5), SIRType.Integer, ae))),
          matchExpr,
          SIR.LetFlags.None,
          ae
        )

        val result3 = lower(sirWithLet3).evaluateDebug
        result3 match {
            case Result.Success(term, _, _, _) =>
                assert(term == Term.Const(Constant.Integer(30)))
            case _ =>
                fail(s"Expected success with result 30, got: ${result3}")
        }
    }

    test("lower constant pattern match on Boolean") {
        // match x with
        //   case true => 1
        //   case false => 0
        val scrutineeVar = SIR.Var("x", SIRType.Boolean, ae)
        val matchExpr = SIR.Match(
          scrutineeVar,
          List(
            SIR.Case(
              SIR.Pattern.Const(SIR.Const(Constant.Bool(true), SIRType.Boolean, ae)),
              SIR.Const(Constant.Integer(1), SIRType.Integer, ae),
              ae
            ),
            SIR.Case(
              SIR.Pattern.Const(SIR.Const(Constant.Bool(false), SIRType.Boolean, ae)),
              SIR.Const(Constant.Integer(0), SIRType.Integer, ae),
              ae
            )
          ),
          SIRType.Integer,
          ae
        )

        val sirWithLetTrue = SIR.Let(
          List(Binding("x", SIRType.Boolean, SIR.Const(Constant.Bool(true), SIRType.Boolean, ae))),
          matchExpr,
          SIR.LetFlags.None,
          ae
        )

        val resultTrue = lower(sirWithLetTrue).evaluateDebug
        resultTrue match {
            case Result.Success(term, _, _, _) =>
                assert(term == Term.Const(Constant.Integer(1)))
            case _ =>
                fail(s"Expected success with result 1, got: ${resultTrue}")
        }

        val sirWithLetFalse = SIR.Let(
          List(Binding("x", SIRType.Boolean, SIR.Const(Constant.Bool(false), SIRType.Boolean, ae))),
          matchExpr,
          SIR.LetFlags.None,
          ae
        )

        val resultFalse = lower(sirWithLetFalse).evaluateDebug
        resultFalse match {
            case Result.Success(term, _, _, _) =>
                assert(term == Term.Const(Constant.Integer(0)))
            case _ =>
                fail(s"Expected success with result 0, got: ${resultFalse}")
        }
    }

    test("lower constant pattern match with @unchecked annotation") {
        // match x with
        //   case 1 => 10
        // This is non-exhaustive but marked as @unchecked, so should generate error term instead of throwing
        val scrutineeVar = SIR.Var("x", SIRType.Integer, ae)
        val matchExpr = SIR.Match(
          scrutineeVar,
          List(
            SIR.Case(
              SIR.Pattern.Const(SIR.Const(Constant.Integer(1), SIRType.Integer, ae)),
              SIR.Const(Constant.Integer(10), SIRType.Integer, ae),
              ae
            )
          ),
          SIRType.Integer,
          ae ++ Map("unchecked" -> SIR.Const(Constant.Unit, SIRType.Unit, ae))
        )

        val sirWithLet = SIR.Let(
          List(Binding("x", SIRType.Integer, SIR.Const(Constant.Integer(1), SIRType.Integer, ae))),
          matchExpr,
          SIR.LetFlags.None,
          ae
        )

        val uplc = lower(sirWithLet)
        val result = uplc.evaluateDebug

        result match {
            case Result.Success(term, _, _, _) =>
                assert(term == Term.Const(Constant.Integer(10)))
            case _ =>
                fail(s"Expected success with result 10, got: ${result}")
        }

        // Test with x = 2 (should hit the error term)
        val sirWithLet2 = SIR.Let(
          List(Binding("x", SIRType.Integer, SIR.Const(Constant.Integer(2), SIRType.Integer, ae))),
          matchExpr,
          SIR.LetFlags.None,
          ae
        )

        val result2 = lower(sirWithLet2).evaluateDebug
        result2 match {
            case Result.Failure(err, _, _, _) =>
                // Expected to fail with error term generated by @unchecked
                // The error message will be "Error evaluated"
                assert(err.getMessage.contains("Error"))
            case _ =>
                fail(s"Expected failure for non-matching case, got: ${result2}")
        }
    }
