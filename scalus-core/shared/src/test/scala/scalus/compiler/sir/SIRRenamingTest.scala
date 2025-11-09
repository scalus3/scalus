package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite

class SIRRenamingTest extends AnyFunSuite {

    test("check that renaming of free vars correctly") {

        val x = SIR.Var("x", SIRType.Integer, AnnotationsDecl.empty)
        val y = SIR.Var("y", SIRType.Integer, AnnotationsDecl.empty)
        val fun = SIR.Var(
          "fun",
          SIRType.Fun(SIRType.Integer, SIRType.Fun(SIRType.Integer, SIRType.Integer)),
          AnnotationsDecl.empty
        )

        // when all variables are free
        val apply_f_x =
            SIR.Apply(fun, x, SIRType.Fun(SIRType.Integer, SIRType.Integer), AnnotationsDecl.empty)
        val apply_f_x_y = SIR.Apply(apply_f_x, y, SIRType.Integer, AnnotationsDecl.empty)
        val renamed1 =
            SIR.renameFreeVarsInExpr(apply_f_x_y, Map("x" -> "x1", "y" -> "y1", "fun" -> "fun1"))

        renamed1 match
            case SIR.Apply(
                  f1_x1,
                  y1,
                  SIRType.Integer,
                  _
                ) =>
                assert(y1 ~=~ SIR.Var("y1", SIRType.Integer, AnnotationsDecl.empty))
                f1_x1 match
                    case SIR.Apply(
                          fun1,
                          x1,
                          SIRType.Fun(SIRType.Integer, SIRType.Integer),
                          _
                        ) =>
                        assert(x1 ~=~ SIR.Var("x1", SIRType.Integer, AnnotationsDecl.empty))
                        assert(
                          fun1 ~=~ SIR.Var(
                            "fun1",
                            SIRType.Fun(
                              SIRType.Integer,
                              SIRType.Fun(SIRType.Integer, SIRType.Integer)
                            ),
                            AnnotationsDecl.empty
                          )
                        )
                    case _ => fail(s"Unexpected renamed expression: $f1_x1")
            case _ => fail(s"Unexpected renamed expression: $renamed1")

        // when x is bound in a lambda
        val lambda_x_apply_f_x_y = SIR.LamAbs(x, apply_f_x_y, List.empty, AnnotationsDecl.empty)
        val rename2 = SIR.renameFreeVarsInExpr(
          lambda_x_apply_f_x_y,
          Map("x" -> "x1", "y" -> "y1", "fun" -> "fun1")
        )

        rename2 match {
            case SIR.LamAbs(v, term, _, _) =>
                assert(v ~=~ x) // x is not renamed because it is bound
                term match {
                    case SIR.Apply(f1_x, y1, SIRType.Integer, _) =>
                        assert(y1 ~=~ SIR.Var("y1", SIRType.Integer, AnnotationsDecl.empty))
                        f1_x match {
                            case SIR.Apply(
                                  fun1,
                                  xN,
                                  SIRType.Fun(SIRType.Integer, SIRType.Integer),
                                  _
                                ) =>
                                assert(x ~=~ xN) // x is not renamed because it is bound
                                assert(
                                  fun1 ~=~ SIR.Var(
                                    "fun1",
                                    SIRType.Fun(
                                      SIRType.Integer,
                                      SIRType.Fun(SIRType.Integer, SIRType.Integer)
                                    ),
                                    AnnotationsDecl.empty
                                  )
                                )
                            case _ => fail(s"Unexpected renamed expression: $f1_x")
                        }
                    case _ => fail(s"Unexpected renamed expression: $term")
                }
            case _ => fail("renaming appply should leave apply, we have: $rename2")
        }

    }

}
