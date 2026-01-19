package scalus.compiler.sir.lowering
package simple

import scalus.*
import scalus.builtin.ByteString.*
import scalus.compiler.sir.*
import scalus.compiler.sir.SIR.Pattern
import scalus.compiler.sir.SIRType.{FreeUnificator, SumCaseClass, TypeNothing}
import scalus.uplc.DefaultFun.*
import scalus.uplc.Constant.asConstant
import scalus.uplc.eval.PlutusVM
import scalus.uplc.{Constant, Term}
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given

import scala.language.implicitConversions

class ScottEncodingLoweringTest extends SimpleLoweringTestBase:

    // Implement lower method for Scott encoding
    override def lower(sir: SIR): Term =
        ScottEncodingLowering(sir, generateErrorTraces = false).lower()

    // Provide PlutusVM for evaluation
    override given vm: PlutusVM = PlutusVM.makePlutusV3VM()

    // Extension for structural comparison tests
    extension (sir: SIR)
        infix def lowersTo(r: Term): Unit =
            assert(ScottEncodingLowering(sir, generateErrorTraces = false).lower() == r)

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
        SIR.Var("x", SIRType.ByteString, ae) lowersTo vr"x"
    }

    test("lower Lam/Apply") {
        import SIRType.{TypeLambda, TypeVar, Unit}
        val idType = TypeLambda(List(TypeVar("A", Some(1), false)), TypeVar("A", Some(1), false))
        val x = SIR.Var("x", TypeVar("X", Some(2), false), ae)
        SIR.Apply(
          SIR.LamAbs(x, x, List.empty, ae),
          SIR.Const(Constant.Unit, Unit, ae),
          Unit,
          ae
        ) lowersTo (lam("x")(vr"x") $ Constant.Unit)

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
                  List(TypeNothing),
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
        withDecls(
          SIR.Constr(
            "scalus.prelude.List$.Nil",
            listData,
            List(),
            listData.constrType("scalus.prelude.List$.Nil"),
            ae
          )
        ) lowersTo (lam("scalus.prelude.List$.Nil", "scalus.prelude.List$.Cons")(
          !vr"scalus.prelude.List$$.Nil"
        ))
        withDecls(
          SIR.Constr(
            "TxId",
            txIdData,
            List(SIR.Const(asConstant(hex"DEADBEEF"), ByteString, ae)),
            txIdData.constrType("TxId"),
            ae
          )
        ) lowersTo (lam("hash", "TxId")(vr"TxId" $ vr"hash") $ hex"DEADBEEF")

    }

    test("lower And, Or, Not") {
        /* And True False
       lowers to (\True False -> And True False) True False
         */
        val a = SIR.Var("a", SIRType.Boolean, ae)
        val b = SIR.Var("b", SIRType.Boolean, ae)
        SIR.And(a, b, ae) lowersTo !(!IfThenElse $ vr"a" $ ~vr"b" $ ~false)
        SIR.Or(a, b, ae) lowersTo !(!IfThenElse $ vr"a" $ ~true $ ~vr"b")
        SIR.Not(a, ae) lowersTo !(!IfThenElse $ vr"a" $ ~false $ ~true)
    }

    test("lower Match") {
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
        val nilConstr = ConstrDecl("Nil", List(), List(), List(), ae)
        val consConstr = ConstrDecl(
          "Cons",
          List(TypeBinding("head", a2TypeVar), TypeBinding("tail", tailTypeProxy)),
          List(a2TypeVar),
          List(a2TypeVar),
          ae
        )
        val listData = DataDecl("List", List(nilConstr, consConstr), List(a1TypeVar), ae)
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

        withDecls(
          SIR.Match(
            SIR.Constr("Nil", listData, List(), listData.constrType("Nil"), ae),
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
        ) lowersTo (lam("Nil", "Cons")(!vr"Nil") $ ~asConstant(1) $ lam("h", "tl")(2))
    }

    test("lower Boolean constant pattern match") {
        /* x match
            case true -> 1
            case false -> 2
        lowers to ifThenElse x 1 2
         */
        val x = SIR.Var("x", SIRType.Boolean, ae)
        SIR.Match(
          x,
          List(
            SIR.Case(
              Pattern.Const(SIR.Const(Constant.Bool(true), SIRType.Boolean, ae)),
              SIR.Const(asConstant(1), SIRType.Integer, ae),
              ae
            ),
            SIR.Case(
              Pattern.Const(SIR.Const(Constant.Bool(false), SIRType.Boolean, ae)),
              SIR.Const(asConstant(2), SIRType.Integer, ae),
              ae
            )
          ),
          SIRType.Integer,
          ae
        ) lowersTo !(!IfThenElse $ vr"x" $ ~asConstant(1) $ ~asConstant(2))
    }

    test("lower Boolean constant pattern match - exhaustive optimization") {
        /* x match
            case false -> 1
            case true -> 2
        // Since both cases are covered, the last case should just return the body
        lowers to ifThenElse x 2 1
         */
        val x = SIR.Var("x", SIRType.Boolean, ae)
        SIR.Match(
          x,
          List(
            SIR.Case(
              Pattern.Const(SIR.Const(Constant.Bool(false), SIRType.Boolean, ae)),
              SIR.Const(asConstant(1), SIRType.Integer, ae),
              ae
            ),
            SIR.Case(
              Pattern.Const(SIR.Const(Constant.Bool(true), SIRType.Boolean, ae)),
              SIR.Const(asConstant(2), SIRType.Integer, ae),
              ae
            )
          ),
          SIRType.Integer,
          ae
        ) lowersTo !(!IfThenElse $ vr"x" $ ~asConstant(2) $ ~asConstant(1))
    }

    test("lower Boolean constant pattern match with wildcard") {
        /* x match
            case true -> 1
            case _ -> 2
        lowers to ifThenElse x 1 2
         */
        val x = SIR.Var("x", SIRType.Boolean, ae)
        SIR.Match(
          x,
          List(
            SIR.Case(
              Pattern.Const(SIR.Const(Constant.Bool(true), SIRType.Boolean, ae)),
              SIR.Const(asConstant(1), SIRType.Integer, ae),
              ae
            ),
            SIR.Case(
              Pattern.Wildcard,
              SIR.Const(asConstant(2), SIRType.Integer, ae),
              ae
            )
          ),
          SIRType.Integer,
          ae
        ) lowersTo !(!IfThenElse $ vr"x" $ ~asConstant(1) $ ~asConstant(2))
    }

    test("lower Integer constant pattern match") {
        /* x match
            case 1 -> "one"
            case 2 -> "two"
            case _ -> "other"
        lowers to ifThenElse (equalsInteger x 1) "one"
                    (ifThenElse (equalsInteger x 2) "two" "other")
         */
        val x = SIR.Var("x", SIRType.Integer, ae)
        SIR.Match(
          x,
          List(
            SIR.Case(
              Pattern.Const(SIR.Const(asConstant(1), SIRType.Integer, ae)),
              SIR.Const(asConstant("one"), SIRType.String, ae),
              ae
            ),
            SIR.Case(
              Pattern.Const(SIR.Const(asConstant(2), SIRType.Integer, ae)),
              SIR.Const(asConstant("two"), SIRType.String, ae),
              ae
            ),
            SIR.Case(
              Pattern.Wildcard,
              SIR.Const(asConstant("other"), SIRType.String, ae),
              ae
            )
          ),
          SIRType.String,
          ae
        ) lowersTo !(!IfThenElse $ !(EqualsInteger $ vr"x" $ 1) $ ~"one" $ ~(!(!IfThenElse $ !(
          EqualsInteger $ vr"x" $ 2
        ) $ ~"two" $ ~"other")))
    }

    test("lower ByteString constant pattern match") {
        /* x match
            case #deadbeef -> 1
            case _ -> 2
        lowers to ifThenElse (equalsByteString x #deadbeef) 1 2
         */
        val x = SIR.Var("x", SIRType.ByteString, ae)
        SIR.Match(
          x,
          List(
            SIR.Case(
              Pattern.Const(SIR.Const(asConstant(hex"DEADBEEF"), SIRType.ByteString, ae)),
              SIR.Const(asConstant(1), SIRType.Integer, ae),
              ae
            ),
            SIR.Case(
              Pattern.Wildcard,
              SIR.Const(asConstant(2), SIRType.Integer, ae),
              ae
            )
          ),
          SIRType.Integer,
          ae
        ) lowersTo !(!IfThenElse $ !(EqualsByteString $ vr"x" $ hex"DEADBEEF") $ ~asConstant(
          1
        ) $ ~asConstant(2))
    }

    test("lower String constant pattern match") {
        /* x match
            case "hello" -> 1
            case "world" -> 2
            case _ -> 3
        lowers to ifThenElse (equalsString x "hello") 1
                    (ifThenElse (equalsString x "world") 2 3)
         */
        val x = SIR.Var("x", SIRType.String, ae)
        SIR.Match(
          x,
          List(
            SIR.Case(
              Pattern.Const(SIR.Const(asConstant("hello"), SIRType.String, ae)),
              SIR.Const(asConstant(1), SIRType.Integer, ae),
              ae
            ),
            SIR.Case(
              Pattern.Const(SIR.Const(asConstant("world"), SIRType.String, ae)),
              SIR.Const(asConstant(2), SIRType.Integer, ae),
              ae
            ),
            SIR.Case(
              Pattern.Wildcard,
              SIR.Const(asConstant(3), SIRType.Integer, ae),
              ae
            )
          ),
          SIRType.Integer,
          ae
        ) lowersTo !(!IfThenElse $ !(EqualsString $ vr"x" $ "hello") $ ~asConstant(
          1
        ) $ ~(!(!IfThenElse $ !(
          EqualsString $ vr"x" $ "world"
        ) $ ~asConstant(2) $ ~asConstant(3))))
    }

    test("lower Data.I constructor") {
        /* Data.I(42)
           lowers to iData 42
           Note: iData is not polymorphic, so no Force needed
         */
        val dataDecl = SIRType.Data.dataDecl
        val dataDeclSIR = SIR.Decl(dataDecl, _: SIR)
        dataDeclSIR(
          SIR.Constr(
            SIRType.Data.I.name,
            dataDecl,
            List(SIR.Const(asConstant(42), SIRType.Integer, ae)),
            dataDecl.constrType(SIRType.Data.I.name),
            ae
          )
        ) lowersTo (IData $ asConstant(42))
    }

    test("lower Data.B constructor") {
        /* Data.B(#deadbeef)
           lowers to bData #deadbeef
         */
        val dataDecl = SIRType.Data.dataDecl
        val dataDeclSIR = SIR.Decl(dataDecl, _: SIR)
        dataDeclSIR(
          SIR.Constr(
            SIRType.Data.B.name,
            dataDecl,
            List(SIR.Const(asConstant(hex"DEADBEEF"), SIRType.ByteString, ae)),
            dataDecl.constrType(SIRType.Data.B.name),
            ae
          )
        ) lowersTo (BData $ asConstant(hex"DEADBEEF"))
    }

    test("lower Data.List constructor") {
        /* Data.List(emptyList)
           lowers to listData emptyList
         */
        val dataDecl = SIRType.Data.dataDecl
        val dataDeclSIR = SIR.Decl(dataDecl, _: SIR)
        val emptyListType = SIRType.BuiltinList(SIRType.Data.tp)
        val emptyListVar = SIR.Var("list", emptyListType, ae)
        dataDeclSIR(
          SIR.Constr(
            SIRType.Data.List.name,
            dataDecl,
            List(emptyListVar),
            dataDecl.constrType(SIRType.Data.List.name),
            ae
          )
        ) lowersTo (ListData $ vr"list")
    }

    test("lower Data.Map constructor") {
        /* Data.Map(pairs)
           lowers to mapData pairs
         */
        val dataDecl = SIRType.Data.dataDecl
        val dataDeclSIR = SIR.Decl(dataDecl, _: SIR)
        val pairsType = SIRType.BuiltinList(SIRType.BuiltinPair(SIRType.Data.tp, SIRType.Data.tp))
        val pairsVar = SIR.Var("pairs", pairsType, ae)
        dataDeclSIR(
          SIR.Constr(
            SIRType.Data.Map.name,
            dataDecl,
            List(pairsVar),
            dataDecl.constrType(SIRType.Data.Map.name),
            ae
          )
        ) lowersTo (MapData $ vr"pairs")
    }

    test("lower Data.Constr constructor") {
        /* Data.Constr(tag, dataArgs)
           lowers to constrData tag dataArgs
         */
        val dataDecl = SIRType.Data.dataDecl
        val dataDeclSIR = SIR.Decl(dataDecl, _: SIR)
        val tagVar = SIR.Var("tag", SIRType.Integer, ae)
        val argsType = SIRType.BuiltinList(SIRType.Data.tp)
        val argsVar = SIR.Var("dataArgs", argsType, ae)
        dataDeclSIR(
          SIR.Constr(
            SIRType.Data.Constr.name,
            dataDecl,
            List(tagVar, argsVar),
            dataDecl.constrType(SIRType.Data.Constr.name),
            ae
          )
        ) lowersTo (ConstrData $ vr"tag" $ vr"dataArgs")
    }

    // TODO: Add evaluation-based tests for Data match that can be reused across backends
    // The exact UPLC structure differs between Scott encoding and other backends,
    // so structural comparison tests are not suitable here.
