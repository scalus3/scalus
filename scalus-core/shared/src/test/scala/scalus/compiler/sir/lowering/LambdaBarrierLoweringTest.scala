package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.*
import scalus.compiler.sir.SIRType.Integer
import scalus.uplc.Term
import LoweredValue.Builder.*

/** Test suite for lambda barrier in lowering backend.
  *
  * The lambda barrier prevents non-effortless computations from being placed inside lambda bodies,
  * which would cause them to be re-evaluated on each lambda call. Effortless values (constants,
  * variables, lambdas) can safely be placed inside lambdas.
  */
class LambdaBarrierLoweringTest extends AnyFunSuite:

    private val emptyAnns = AnnotationsDecl.empty
    private val pos = emptyAnns.pos

    test("non-effortless variable placed outside lambda") {
        // Create: let x = 1 + 2 in lambda y => x + y
        // x is non-effortless (Apply), should be outside lambda
        given lctx: LoweringContext = LoweringContext()

        val xRhs = BuilinApply2LoweredVale(
          SIRBuiltins.addInteger,
          Integer,
          PrimitiveRepresentation.Constant,
          pos,
          lvIntConstant(1, pos),
          lvIntConstant(2, pos)
        )

        val x = new VariableLoweredValue(
          id = "x",
          name = "x",
          sir = SIR.Var("x", Integer, emptyAnns),
          representation = PrimitiveRepresentation.Constant,
          optRhs = Some(xRhs)
        )

        val lambda = lvLamAbs(
          "y",
          Integer,
          PrimitiveRepresentation.Constant,
          y => {
              BuilinApply2LoweredVale(
                SIRBuiltins.addInteger,
                Integer,
                PrimitiveRepresentation.Constant,
                pos,
                x,
                y
              )
          },
          pos
        )

        // Generate UPLC term
        val gctx = TermGenerationContext(generatedVars = Set.empty)
        val term = lambda.termWithNeededVars(gctx)

        // Verify structure: should be ((\x -> (\y -> x + y)) (1 + 2))
        // x should be bound outside the lambda
        term match
            case Term.Apply(Term.LamAbs("x", lambdaBody), rhs) =>
                // x is bound outside lambda - correct!
                assert(lambdaBody.isInstanceOf[Term.LamAbs])
                assert(rhs.isInstanceOf[Term.Apply]) // 1 + 2
            case _ =>
                fail(s"Expected (\\x -> \\y -> ...) (1 + 2), got: ${term}")
    }

    test("effortless constant allowed inside lambda") {
        // Create: let c = 42 in lambda y => c + y
        // c is effortless (Const), can be inlined inside lambda
        given lctx: LoweringContext = LoweringContext()

        val c = new VariableLoweredValue(
          id = "c",
          name = "c",
          sir = SIR.Var("c", Integer, emptyAnns),
          representation = PrimitiveRepresentation.Constant,
          optRhs = Some(lvIntConstant(42, pos))
        )

        val lambda = lvLamAbs(
          "y",
          Integer,
          PrimitiveRepresentation.Constant,
          y => {
              BuilinApply2LoweredVale(
                SIRBuiltins.addInteger,
                Integer,
                PrimitiveRepresentation.Constant,
                pos,
                c,
                y
              )
          },
          pos
        )

        // Generate UPLC term
        val gctx = TermGenerationContext(generatedVars = Set.empty)
        val term = lambda.termWithNeededVars(gctx)

        // Verify structure: effortless constant is inlined inside lambda
        // Should be: \y -> (42 + y)
        term match
            case Term.LamAbs(yName, body) =>
                // c is effortless, so it's inlined (42 appears directly in body)
                assert(body.toString.contains("42"))
            case _ =>
                fail(s"Expected \\y -> ..., got: ${term}")
    }

    test("effortless variable allowed inside lambda") {
        // Create: let outer = 10 in (let inner = outer in lambda y => inner + y)
        // inner is effortless (just a Var reference), can be inlined inside lambda
        given lctx: LoweringContext = LoweringContext()

        val outer = new VariableLoweredValue(
          id = "outer",
          name = "outer",
          sir = SIR.Var("outer", Integer, emptyAnns),
          representation = PrimitiveRepresentation.Constant,
          optRhs = Some(lvIntConstant(10, pos))
        )

        val inner = new VariableLoweredValue(
          id = "inner",
          name = "inner",
          sir = SIR.Var("inner", Integer, emptyAnns),
          representation = PrimitiveRepresentation.Constant,
          optRhs = Some(outer)
        )

        val lambda = lvLamAbs(
          "y",
          Integer,
          PrimitiveRepresentation.Constant,
          y => {
              BuilinApply2LoweredVale(
                SIRBuiltins.addInteger,
                Integer,
                PrimitiveRepresentation.Constant,
                pos,
                inner,
                y
              )
          },
          pos
        )

        // Generate UPLC term
        val gctx = TermGenerationContext(generatedVars = Set.empty)
        val term = lambda.termWithNeededVars(gctx)

        // inner->outer chain gets inlined to just the constant 10
        // Should be: \y -> (10 + y)
        term match
            case Term.LamAbs(yName, body) =>
                // Both inner and outer are effortless, so they're inlined to 10
                assert(body.toString.contains("10"))
            case _ =>
                fail(s"Expected \\y -> ..., got: ${term}")
    }

    test("mixed effortless and non-effortless variables") {
        // Create: let x = 1 + 2 in let c = 42 in lambda y => x + c + y
        // x is non-effortless (Apply), should be outside
        // c is effortless (Const), can be inlined inside
        given lctx: LoweringContext = LoweringContext()

        val xRhs = BuilinApply2LoweredVale(
          SIRBuiltins.addInteger,
          Integer,
          PrimitiveRepresentation.Constant,
          pos,
          lvIntConstant(1, pos),
          lvIntConstant(2, pos)
        )

        val x = new VariableLoweredValue(
          id = "x",
          name = "x",
          sir = SIR.Var("x", Integer, emptyAnns),
          representation = PrimitiveRepresentation.Constant,
          optRhs = Some(xRhs)
        )

        val c = new VariableLoweredValue(
          id = "c",
          name = "c",
          sir = SIR.Var("c", Integer, emptyAnns),
          representation = PrimitiveRepresentation.Constant,
          optRhs = Some(lvIntConstant(42, pos))
        )

        val lambda = lvLamAbs(
          "y",
          Integer,
          PrimitiveRepresentation.Constant,
          y => {
              val xPlusC = BuilinApply2LoweredVale(
                SIRBuiltins.addInteger,
                Integer,
                PrimitiveRepresentation.Constant,
                pos,
                x,
                c
              )
              BuilinApply2LoweredVale(
                SIRBuiltins.addInteger,
                Integer,
                PrimitiveRepresentation.Constant,
                pos,
                xPlusC,
                y
              )
          },
          pos
        )

        // Generate UPLC term
        val gctx = TermGenerationContext(generatedVars = Set.empty)
        val term = lambda.termWithNeededVars(gctx)

        // Verify: x should be outside lambda (non-effortless), c is inlined inside (effortless)
        // Should be: (\x -> \y -> x + 42 + y) (1 + 2)
        term match
            case Term.Apply(Term.LamAbs("x", lambdaBody), _) =>
                // x is bound outside - correct!
                lambdaBody match
                    case Term.LamAbs(yName, body) =>
                        // c is effortless, so 42 should appear directly in body
                        assert(body.toString.contains("42"))
                        // x is used as Var in body
                        assert(body.toString.contains("x"))
                    case _ =>
                        fail(s"Expected \\y -> ... inside, got: ${lambdaBody}")
            case _ =>
                fail(s"Expected (\\x -> ...) ..., got: ${term}")
    }

    test("nested lambdas with barrier") {
        // Create: let x = 1 + 2 in lambda a => lambda b => x + a + b
        // x should be outside both lambdas (non-effortless)
        given lctx: LoweringContext = LoweringContext()

        val xRhs = BuilinApply2LoweredVale(
          SIRBuiltins.addInteger,
          Integer,
          PrimitiveRepresentation.Constant,
          pos,
          lvIntConstant(1, pos),
          lvIntConstant(2, pos)
        )

        val x = new VariableLoweredValue(
          id = "x",
          name = "x",
          sir = SIR.Var("x", Integer, emptyAnns),
          representation = PrimitiveRepresentation.Constant,
          optRhs = Some(xRhs)
        )

        val lambda = lvLamAbs(
          "a",
          Integer,
          PrimitiveRepresentation.Constant,
          a => {
              lvLamAbs(
                "b",
                Integer,
                PrimitiveRepresentation.Constant,
                b => {
                    val xPlusA = BuilinApply2LoweredVale(
                      SIRBuiltins.addInteger,
                      Integer,
                      PrimitiveRepresentation.Constant,
                      pos,
                      x,
                      a
                    )
                    BuilinApply2LoweredVale(
                      SIRBuiltins.addInteger,
                      Integer,
                      PrimitiveRepresentation.Constant,
                      pos,
                      xPlusA,
                      b
                    )
                },
                pos
              )
          },
          pos
        )

        // Generate UPLC term
        val gctx = TermGenerationContext(generatedVars = Set.empty)
        val term = lambda.termWithNeededVars(gctx)

        // Verify: x should be bound outside both lambdas
        // Should be: (\x -> \a -> \b -> x + a + b) (1 + 2)
        term match
            case Term.Apply(Term.LamAbs("x", lambdaBody), rhs) =>
                // x is bound outside both lambdas - correct!
                assert(rhs.isInstanceOf[Term.Apply]) // 1 + 2
                lambdaBody match
                    case Term.LamAbs(aName, innerLambda) =>
                        innerLambda match
                            case Term.LamAbs(bName, body) =>
                                // x should be used as Var in innermost body
                                assert(body.toString.contains("x"))
                            case _ =>
                                fail(s"Expected \\b -> ..., got: ${innerLambda}")
                    case _ =>
                        fail(s"Expected \\a -> ..., got: ${lambdaBody}")
            case _ =>
                fail(s"Expected (\\x -> ...) ..., got: ${term}")
    }

    test("transitive non-effortless dependency") {
        // Create: let c = 1 + 2 in let b = c in let a = b in lambda y => a + y
        // c is non-effortless, b and a are effortless references
        // But a transitively depends on non-effortless c, so a should be outside lambda
        given lctx: LoweringContext = LoweringContext()

        val cRhs = BuilinApply2LoweredVale(
          SIRBuiltins.addInteger,
          Integer,
          PrimitiveRepresentation.Constant,
          pos,
          lvIntConstant(1, pos),
          lvIntConstant(2, pos)
        )

        val c = new VariableLoweredValue(
          id = "c",
          name = "c",
          sir = SIR.Var("c", Integer, emptyAnns),
          representation = PrimitiveRepresentation.Constant,
          optRhs = Some(cRhs)
        )

        val b = new VariableLoweredValue(
          id = "b",
          name = "b",
          sir = SIR.Var("b", Integer, emptyAnns),
          representation = PrimitiveRepresentation.Constant,
          optRhs = Some(c)
        )

        val a = new VariableLoweredValue(
          id = "a",
          name = "a",
          sir = SIR.Var("a", Integer, emptyAnns),
          representation = PrimitiveRepresentation.Constant,
          optRhs = Some(b)
        )

        val lambda = lvLamAbs(
          "y",
          Integer,
          PrimitiveRepresentation.Constant,
          y => {
              BuilinApply2LoweredVale(
                SIRBuiltins.addInteger,
                Integer,
                PrimitiveRepresentation.Constant,
                pos,
                a,
                y
              )
          },
          pos
        )

        // Generate UPLC term
        val gctx = TermGenerationContext(generatedVars = Set.empty)
        val term = lambda.termWithNeededVars(gctx)

        // a->b->c chain: only c has non-effortless RHS, so only c is outside
        // a and b are effortless (variable references) and get inlined
        // Should be: (\c -> \y -> c + y) (1 + 2)
        term match
            case Term.Apply(Term.LamAbs("c", lambdaBody), rhs) =>
                // c (the non-effortless computation) is bound outside - correct!
                assert(rhs.isInstanceOf[Term.Apply]) // 1 + 2
                lambdaBody match
                    case Term.LamAbs(yName, body) =>
                        // Body should use c (a and b were inlined to c)
                        assert(body.toString.contains("c"))
                    case _ =>
                        fail(s"Expected \\y -> ... inside, got: ${lambdaBody}")
            case _ =>
                fail(s"Expected (\\c -> ...) (1 + 2), got: ${term}")
    }

    test("lambda is effortless and can be inside another lambda") {
        // Create: let f = (lambda x => x + 1) in lambda y => f(y)
        // f is effortless (lambda), can be inlined inside outer lambda
        given lctx: LoweringContext = LoweringContext()

        val innerLambda = lvLamAbs(
          "x",
          Integer,
          PrimitiveRepresentation.Constant,
          x => {
              BuilinApply2LoweredVale(
                SIRBuiltins.addInteger,
                Integer,
                PrimitiveRepresentation.Constant,
                pos,
                x,
                lvIntConstant(1, pos)
              )
          },
          pos
        )

        val f = new VariableLoweredValue(
          id = "f",
          name = "f",
          sir = SIR.Var("f", SIRType.Fun(Integer, Integer), emptyAnns),
          representation = LambdaRepresentation(
            SIRType.Fun(Integer, Integer),
            InOutRepresentationPair(
              PrimitiveRepresentation.Constant,
              PrimitiveRepresentation.Constant
            )
          ),
          optRhs = Some(innerLambda)
        )

        val outerLambda = lvLamAbs(
          "y",
          Integer,
          PrimitiveRepresentation.Constant,
          y => {
              ApplyLoweredValue(
                f,
                y,
                Integer,
                PrimitiveRepresentation.Constant,
                pos
              )
          },
          pos
        )

        // Generate UPLC term
        val gctx = TermGenerationContext(generatedVars = Set.empty)
        val term = outerLambda.termWithNeededVars(gctx)

        // f (lambda) is effortless, gets inlined inside outer lambda
        // Should be: \y -> (\x -> x + 1) y  (inline application)
        term match
            case Term.LamAbs(yName, body) =>
                // f is effortless lambda, so it's inlined in the body
                // The body should be an application
                assert(body.isInstanceOf[Term.Apply])
            case _ =>
                fail(s"Expected \\y -> ..., got: ${term}")
    }

end LambdaBarrierLoweringTest
