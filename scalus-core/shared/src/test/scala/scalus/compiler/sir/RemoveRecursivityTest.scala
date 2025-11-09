package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.Constant
import SIR.*
import SIRType.{Boolean, Fun, Integer, Unit}

class RemoveRecursivityTest extends AnyFunSuite:
    test("remove recursivity") {
        val compiled = Compiler.compile {
            def nonRecursive(x: BigInt): BigInt =
                def recursive(x: BigInt): BigInt =
                    if x == BigInt(0) then 0 else recursive(x - 1)
                recursive(x)
        }
        val optimized = RemoveRecursivity(compiled)
        val ae = AnnotationsDecl.empty
        val xVar = Var("x", SIRType.Integer, ae)
        val nonRecursiveVar = Var("nonRecursive", Fun(Integer, Integer), ae)
        val recursiveVar = Var("recursive", Fun(Integer, Integer), ae)

        assert(
          optimized ~=~ Let(
            List(
              Binding(
                "nonRecursive",
                SIRType.Fun(Integer, Integer),
                LamAbs(
                  xVar,
                  Let(
                    List(
                      Binding(
                        "recursive",
                        SIRType.Fun(Integer, Integer),
                        LamAbs(
                          xVar,
                          IfThenElse(
                            Apply(
                              Apply(
                                SIRBuiltins.equalsInteger,
                                xVar,
                                Fun(Integer, Boolean),
                                ae
                              ),
                              Const(Constant.Integer(0), Integer, ae),
                              Boolean,
                              ae
                            ),
                            Const(Constant.Integer(0), Integer, ae),
                            Apply(
                              recursiveVar,
                              Apply(
                                Apply(
                                  SIRBuiltins.subtractInteger,
                                  xVar,
                                  Fun(Integer, Integer),
                                  ae
                                ),
                                Const(Constant.Integer(1), Integer, ae),
                                Integer,
                                ae
                              ),
                              Integer,
                              ae
                            ),
                            Integer,
                            ae
                          ),
                          List.empty,
                          ae
                        )
                      )
                    ),
                    Apply(recursiveVar, xVar, Integer, ae),
                    SIR.LetFlags.Recursivity,
                    ae
                  ),
                  List.empty,
                  ae
                )
              )
            ),
            Const(Constant.Unit, Unit, ae),
            SIR.LetFlags.None,
            ae
          )
        )
    }
