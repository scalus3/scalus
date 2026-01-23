package scalus.compiler.sir.lowering
package simple

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.uplc.builtin.ByteString.*
import scalus.compiler.sir.*
import scalus.compiler.sir.SIR.Pattern
import scalus.uplc.DefaultFun.*
import scalus.uplc.Constant.asConstant
import scalus.uplc.{Constant, DeBruijn, Term}
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.uplc.test.ArbitraryInstances

import scala.language.implicitConversions

/** Base trait for evaluation-based tests that can be reused across different lowering backends.
  *
  * Subclasses must provide:
  *   - `lower`: Method to lower SIR to UPLC Term
  *   - `given PlutusVM`: VM for evaluation
  */
trait SimpleLoweringTestBase
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances:

    /** Lower SIR to UPLC Term. Must be implemented by subclasses. */
    def lower(sir: SIR): Term

    /** PlutusVM for evaluation. Must be provided by subclasses. */
    given vm: PlutusVM

    protected val ae: AnnotationsDecl = AnnotationsDecl.empty

    extension (term: Term)
        infix def alphaEq(other: Term): Boolean =
            DeBruijn.deBruijnTerm(term) α_== DeBruijn.deBruijnTerm(other)

    // ==================== Data Constructor Tests ====================

    test("eval Data.I constructor") {
        val dataDecl = SIRType.Data.dataDecl
        val sir = SIR.Decl(
          dataDecl,
          SIR.Constr(
            SIRType.Data.I.name,
            dataDecl,
            List(SIR.Const(asConstant(42), SIRType.Integer, ae)),
            dataDecl.constrType(SIRType.Data.I.name),
            ae
          )
        )

        val uplc = lower(sir)
        val result = uplc.evaluateDebug

        result match {
            case Result.Success(term, _, _, _) =>
                // Data.I(42) should evaluate to iData(42)
                assert(term == (IData $ 42.asTerm).evaluate)
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success, got failure: ${err.getMessage}")
        }
    }

    test("eval Data.B constructor") {
        val dataDecl = SIRType.Data.dataDecl
        val sir = SIR.Decl(
          dataDecl,
          SIR.Constr(
            SIRType.Data.B.name,
            dataDecl,
            List(SIR.Const(asConstant(hex"DEADBEEF"), SIRType.ByteString, ae)),
            dataDecl.constrType(SIRType.Data.B.name),
            ae
          )
        )

        val uplc = lower(sir)
        val result = uplc.evaluateDebug

        result match {
            case Result.Success(term, _, _, _) =>
                // Data.B(#deadbeef) should evaluate to bData(#deadbeef)
                assert(term == (BData $ hex"DEADBEEF".asTerm).evaluate)
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success, got failure: ${err.getMessage}")
        }
    }

    test("eval Data.Constr constructor") {
        val dataDecl = SIRType.Data.dataDecl
        // Create Data.Constr(0, [])
        val emptyListTerm = Term.Const(Constant.List(scalus.uplc.DefaultUni.Data, List()))
        val sir = SIR.Decl(
          dataDecl,
          SIR.Let(
            List(
              Binding(
                "dataArgs",
                SIRType.BuiltinList(SIRType.Data.tp),
                SIR.Const(
                  Constant.List(scalus.uplc.DefaultUni.Data, List()),
                  SIRType.BuiltinList(SIRType.Data.tp),
                  ae
                )
              )
            ),
            SIR.Constr(
              SIRType.Data.Constr.name,
              dataDecl,
              List(
                SIR.Const(asConstant(0), SIRType.Integer, ae),
                SIR.Var("dataArgs", SIRType.BuiltinList(SIRType.Data.tp), ae)
              ),
              dataDecl.constrType(SIRType.Data.Constr.name),
              ae
            ),
            SIR.LetFlags.None,
            ae
          )
        )

        val uplc = lower(sir)
        val result = uplc.evaluateDebug

        result match {
            case Result.Success(term, _, _, _) =>
                // Data.Constr(0, []) should evaluate to constrData(0, [])
                val expected = (ConstrData $ 0.asTerm $ emptyListTerm).evaluate
                assert(term == expected)
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success, got failure: ${err.getMessage}")
        }
    }

    // ==================== Data Match Tests ====================

    test("eval Data match - I case") {
        val dataDecl = SIRType.Data.dataDecl
        val iConstr = dataDecl.constructors.find(_.name == SIRType.Data.I.name).get
        val bConstr = dataDecl.constructors.find(_.name == SIRType.Data.B.name).get
        val listConstr = dataDecl.constructors.find(_.name == SIRType.Data.List.name).get
        val mapConstr = dataDecl.constructors.find(_.name == SIRType.Data.Map.name).get
        val constrConstr = dataDecl.constructors.find(_.name == SIRType.Data.Constr.name).get

        // Create: let d = Data.I(42) in match d { case I(v) => v; case _ => 0 }
        val sir = SIR.Decl(
          dataDecl,
          SIR.Let(
            List(
              Binding(
                "d",
                SIRType.Data.tp,
                SIR.Constr(
                  SIRType.Data.I.name,
                  dataDecl,
                  List(SIR.Const(asConstant(42), SIRType.Integer, ae)),
                  dataDecl.constrType(SIRType.Data.I.name),
                  ae
                )
              )
            ),
            SIR.Match(
              SIR.Var("d", SIRType.Data.tp, ae),
              List(
                SIR.Case(
                  Pattern.Constr(iConstr, List("v"), List()),
                  SIR.Var("v", SIRType.Integer, ae),
                  ae
                ),
                SIR.Case(
                  Pattern.Wildcard,
                  SIR.Const(asConstant(0), SIRType.Integer, ae),
                  ae
                )
              ),
              SIRType.Integer,
              ae
            ),
            SIR.LetFlags.None,
            ae
          )
        )

        val uplc = lower(sir)
        val result = uplc.evaluateDebug

        result match {
            case Result.Success(term, _, _, _) =>
                assert(term == 42.asTerm)
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success with result 42, got failure: ${err.getMessage}")
        }
    }

    test("eval Data match - B case") {
        val dataDecl = SIRType.Data.dataDecl
        val iConstr = dataDecl.constructors.find(_.name == SIRType.Data.I.name).get
        val bConstr = dataDecl.constructors.find(_.name == SIRType.Data.B.name).get

        // Create: let d = Data.B(#deadbeef) in match d { case B(v) => 1; case _ => 0 }
        val sir = SIR.Decl(
          dataDecl,
          SIR.Let(
            List(
              Binding(
                "d",
                SIRType.Data.tp,
                SIR.Constr(
                  SIRType.Data.B.name,
                  dataDecl,
                  List(SIR.Const(asConstant(hex"DEADBEEF"), SIRType.ByteString, ae)),
                  dataDecl.constrType(SIRType.Data.B.name),
                  ae
                )
              )
            ),
            SIR.Match(
              SIR.Var("d", SIRType.Data.tp, ae),
              List(
                SIR.Case(
                  Pattern.Constr(bConstr, List("v"), List()),
                  SIR.Const(asConstant(1), SIRType.Integer, ae),
                  ae
                ),
                SIR.Case(
                  Pattern.Wildcard,
                  SIR.Const(asConstant(0), SIRType.Integer, ae),
                  ae
                )
              ),
              SIRType.Integer,
              ae
            ),
            SIR.LetFlags.None,
            ae
          )
        )

        val uplc = lower(sir)
        val result = uplc.evaluateDebug

        result match {
            case Result.Success(term, _, _, _) =>
                assert(term == 1.asTerm)
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success with result 1, got failure: ${err.getMessage}")
        }
    }

    test("eval Data match - Constr case with bindings") {
        val dataDecl = SIRType.Data.dataDecl
        val constrConstr = dataDecl.constructors.find(_.name == SIRType.Data.Constr.name).get

        // Create: let d = Data.Constr(5, []) in match d { case Constr(tag, _) => tag; case _ => 0 }
        val emptyList = Constant.List(scalus.uplc.DefaultUni.Data, List())
        val sir = SIR.Decl(
          dataDecl,
          SIR.Let(
            List(
              Binding(
                "dataArgs",
                SIRType.BuiltinList(SIRType.Data.tp),
                SIR.Const(emptyList, SIRType.BuiltinList(SIRType.Data.tp), ae)
              )
            ),
            SIR.Let(
              List(
                Binding(
                  "d",
                  SIRType.Data.tp,
                  SIR.Constr(
                    SIRType.Data.Constr.name,
                    dataDecl,
                    List(
                      SIR.Const(asConstant(5), SIRType.Integer, ae),
                      SIR.Var("dataArgs", SIRType.BuiltinList(SIRType.Data.tp), ae)
                    ),
                    dataDecl.constrType(SIRType.Data.Constr.name),
                    ae
                  )
                )
              ),
              SIR.Match(
                SIR.Var("d", SIRType.Data.tp, ae),
                List(
                  SIR.Case(
                    Pattern.Constr(constrConstr, List("tag", "dataArgs2"), List()),
                    SIR.Var("tag", SIRType.Integer, ae),
                    ae
                  ),
                  SIR.Case(
                    Pattern.Wildcard,
                    SIR.Const(asConstant(0), SIRType.Integer, ae),
                    ae
                  )
                ),
                SIRType.Integer,
                ae
              ),
              SIR.LetFlags.None,
              ae
            ),
            SIR.LetFlags.None,
            ae
          )
        )

        val uplc = lower(sir)
        val result = uplc.evaluateDebug

        result match {
            case Result.Success(term, _, _, _) =>
                assert(term == 5.asTerm)
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success with result 5, got failure: ${err.getMessage}")
        }
    }

    test("eval Data match - wildcard fallback") {
        val dataDecl = SIRType.Data.dataDecl
        val iConstr = dataDecl.constructors.find(_.name == SIRType.Data.I.name).get

        // Create: let d = Data.B(#aa) in match d { case I(v) => v; case _ => 99 }
        // Should hit the wildcard case
        val sir = SIR.Decl(
          dataDecl,
          SIR.Let(
            List(
              Binding(
                "d",
                SIRType.Data.tp,
                SIR.Constr(
                  SIRType.Data.B.name,
                  dataDecl,
                  List(SIR.Const(asConstant(hex"AA"), SIRType.ByteString, ae)),
                  dataDecl.constrType(SIRType.Data.B.name),
                  ae
                )
              )
            ),
            SIR.Match(
              SIR.Var("d", SIRType.Data.tp, ae),
              List(
                SIR.Case(
                  Pattern.Constr(iConstr, List("v"), List()),
                  SIR.Var("v", SIRType.Integer, ae),
                  ae
                ),
                SIR.Case(
                  Pattern.Wildcard,
                  SIR.Const(asConstant(99), SIRType.Integer, ae),
                  ae
                )
              ),
              SIRType.Integer,
              ae
            ),
            SIR.LetFlags.None,
            ae
          )
        )

        val uplc = lower(sir)
        val result = uplc.evaluateDebug

        result match {
            case Result.Success(term, _, _, _) =>
                assert(term == 99.asTerm)
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success with result 99, got failure: ${err.getMessage}")
        }
    }
