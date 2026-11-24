package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.SIRType.{CaseClass, Fun}
import scalus.uplc.Constant

/** The constructors of SIR nodes and SIR types check only the rules that hold for every program.
  * Each of the first three tests builds a value that a check left from a debug session rejected.
  */
class SIRConstructionTest extends AnyFunSuite {

    private val anns = AnnotationsDecl.empty
    private val one = SIR.Const(Constant.Integer(1), SIRType.Integer, anns)

    test("a let can bind the name args") {
        val let = SIR.Let(
          List(Binding("args", SIRType.Integer, one)),
          SIR.Var("args", SIRType.Integer, anns),
          SIR.LetFlags.None,
          anns
        )
        assert(let.tp == SIRType.Integer)
    }

    test("a cast does not depend on the name of its source file") {
        val intToPair = Fun(SIRType.Integer, SIRType.Tuple2(SIRType.Integer, SIRType.Integer))
        val inRegressionTest =
            AnnotationsDecl(SIRPosition("GeneratingUnscopedVarsTest.scala", 0, 0, 0, 0))
        val cast =
            SIR.Cast(SIR.Var("f", SIRType.FreeUnificator, anns), intToPair, inRegressionTest)
        assert(cast.tp == intToPair)
    }

    test("a case class type without a parent does not depend on the name of the class") {
        // The removed check matched this class name. The test does not say that the type of an
        // enum case can have no parent.
        val close = ConstrDecl(
          "scalus.testing.regression.cosmex20251107.minimized.Action$.Close",
          Nil,
          Nil,
          Nil,
          anns
        )
        assert(CaseClass(close, Nil, None).parent.isEmpty)
    }

    test("a tuple type with a parent is rejected") {
        val error = intercept[IllegalArgumentException] {
            CaseClass(
              SIRType.Tuple2.constrDecl,
              List(SIRType.Integer, SIRType.Integer),
              Some(SIRType.Data.tp)
            )
        }
        assert(error.getMessage.contains("scala.Tuple2"))
    }
}
