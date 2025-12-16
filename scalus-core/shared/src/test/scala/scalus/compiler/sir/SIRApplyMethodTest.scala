package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.Constant

/** Test suite for the SIR.$ method (function application operator).
  *
  * Tests cover:
  *   - Basic function application with simple Fun types
  *   - Application with Decl wrappers
  *   - Application with TypeLambda-wrapped functions
  *   - Error handling for non-function types
  */
class SIRApplyMethodTest extends AnyFunSuite:

    // Create empty annotations for test simplicity
    private val emptyAnns = AnnotationsDecl.empty

    // Helper to create a simple variable
    private def mkVar(name: String, tp: SIRType): SIR.Var =
        SIR.Var(name, tp, emptyAnns)

    // Helper to create a lambda
    private def mkLam(paramName: String, paramType: SIRType, body: SIR): SIR.LamAbs =
        SIR.LamAbs(
          mkVar(paramName, paramType),
          body,
          List.empty,
          emptyAnns
        )

    // Helper to create a constant
    private def mkConst(value: Int): SIR.Const =
        SIR.Const(Constant.Integer(value), SIRType.Integer, emptyAnns)

    // Helper to create a data declaration
    private def mkDataDecl(name: String): DataDecl =
        val constr = ConstrDecl(
          name = s"$name.Constr",
          params = List.empty,
          typeParams = List.empty,
          parentTypeArgs = List.empty,
          annotations = emptyAnns
        )
        DataDecl(
          name = name,
          constructors = List(constr),
          typeParams = List.empty,
          annotations = emptyAnns
        )

    test("apply simple function to argument") {
        // Create a lambda: λx. x
        val identityLam = mkLam("x", SIRType.Integer, mkVar("x", SIRType.Integer))
        val arg = mkConst(42)

        // Apply: (λx. x) $ 42
        val result = identityLam $ arg

        // Check result is an Apply node
        result match
            case SIR.Apply(f, a, tp, _) =>
                assert(f == identityLam)
                assert(a == arg)
                assert(tp == SIRType.Integer)
            case _ => fail(s"Expected Apply, got $result")
    }

    test("apply function wrapped in single Decl") {
        // Create a lambda
        val lam = mkLam("x", SIRType.Integer, mkVar("x", SIRType.Integer))
        val dataDecl = mkDataDecl("TestData")

        // Wrap lambda in Decl
        val wrappedLam = SIR.Decl(dataDecl, lam)
        val arg = mkConst(42)

        // Apply: Decl(data, λx. x) $ 42
        val result = wrappedLam $ arg

        // Check result is wrapped in the same Decl
        result match
            case SIR.Decl(d, term) =>
                assert(d == dataDecl)
                term match
                    case SIR.Apply(f, a, tp, _) =>
                        assert(f == lam)
                        assert(a == arg)
                        assert(tp == SIRType.Integer)
                    case _ => fail(s"Expected Apply inside Decl, got $term")
            case _ => fail(s"Expected Decl, got $result")
    }

    test("apply function wrapped in multiple Decls") {
        val lam = mkLam("x", SIRType.Integer, mkVar("x", SIRType.Integer))
        val dataDecl1 = mkDataDecl("TestData1")
        val dataDecl2 = mkDataDecl("TestData2")

        // Wrap lambda in two Decls
        val wrappedLam = SIR.Decl(dataDecl1, SIR.Decl(dataDecl2, lam))
        val arg = mkConst(42)

        // Apply
        val result = wrappedLam $ arg

        // Check result preserves both Decls
        result match
            case SIR.Decl(d1, inner) =>
                assert(d1 == dataDecl1)
                inner match
                    case SIR.Decl(d2, term) =>
                        assert(d2 == dataDecl2)
                        term match
                            case SIR.Apply(f, a, _, _) =>
                                assert(f == lam)
                                assert(a == arg)
                            case _ => fail(s"Expected Apply, got $term")
                    case _ => fail(s"Expected second Decl, got $inner")
            case _ => fail(s"Expected first Decl, got $result")
    }

    test("apply polymorphic function with TypeLambda") {
        // Create a polymorphic identity function: ∀A. A -> A
        val typeVar = SIRType.TypeVar("A", None, false)
        val polyType = SIRType.TypeLambda(
          List(typeVar),
          SIRType.Fun(typeVar, typeVar)
        )

        // Create a variable with polymorphic type
        val polyFun = mkVar("polyId", polyType)
        val arg = mkConst(42)

        // Apply: polyId $ 42
        val result = polyFun $ arg

        // Check result
        result match
            case SIR.Apply(f, a, tp, _) =>
                assert(f == polyFun)
                assert(a == arg)
                assert(tp == typeVar) // Result type should be the type variable
            case _ => fail(s"Expected Apply, got $result")
    }

    test("apply preserves annotations from function term") {
        val customAnns = AnnotationsDecl(
          pos = SIRPosition("test.scala", 10, 5, 10, 15, Nil),
          comment = Some("test annotation")
        )

        // Create a lambda with custom annotations
        val lam = SIR.LamAbs(
          mkVar("x", SIRType.Integer),
          mkVar("x", SIRType.Integer),
          List.empty,
          customAnns
        )
        val arg = mkConst(42)

        // Apply
        val result = lam $ arg

        // Check annotations are preserved
        result match
            case SIR.Apply(_, _, _, anns) =>
                assert(anns == customAnns)
            case _ => fail(s"Expected Apply, got $result")
    }

    test("apply extracts correct output type from function type") {
        // Create a function of type Int -> String
        val funType = SIRType.Fun(SIRType.Integer, SIRType.String)
        val fun = mkVar("f", funType)
        val arg = mkConst(42)

        // Apply
        val result = fun $ arg

        // Check the result type is String
        result match
            case SIR.Apply(_, _, tp, _) =>
                assert(tp == SIRType.String)
            case _ => fail(s"Expected Apply, got $result")
    }

    test("apply fails on non-function type") {
        // Create a non-function value
        val notAFunction = mkConst(42) // Just an integer constant
        val arg = mkConst(100)

        // Applying should throw RuntimeException
        val exception = intercept[RuntimeException] {
            notAFunction $ arg
        }

        assert(exception.getMessage.contains("Cannot apply non-function type"))
        assert(exception.getMessage.contains("Int"))
    }

    test("apply fails on TypeLambda with non-function body") {
        // Create a type lambda that doesn't wrap a function
        val typeVar = SIRType.TypeVar("A", None, false)
        val badPolyType = SIRType.TypeLambda(
          List(typeVar),
          SIRType.Integer // Not a function type!
        )

        val badPolyFun = mkVar("bad", badPolyType)
        val arg = mkConst(42)

        // Applying should throw RuntimeException
        val exception = intercept[RuntimeException] {
            badPolyFun $ arg
        }

        assert(exception.getMessage.contains("Cannot apply non-function type"))
    }

    test("apply works with curried functions") {
        // Create a curried function: Int -> (Int -> Int)
        val innerFunType = SIRType.Fun(SIRType.Integer, SIRType.Integer)
        val curriedType = SIRType.Fun(SIRType.Integer, innerFunType)

        val curriedFun = mkVar("add", curriedType)
        val arg1 = mkConst(10)

        // First application: add $ 10
        val partialApp = curriedFun $ arg1

        // Check result type is Int -> Int
        partialApp match
            case SIR.Apply(_, _, tp, _) =>
                assert(tp == innerFunType)
            case _ => fail(s"Expected Apply, got $partialApp")

        // Second application: (add $ 10) $ 20
        val arg2 = mkConst(20)
        val fullApp = partialApp $ arg2

        // Check final result type is Int
        fullApp match
            case SIR.Apply(_, _, tp, _) =>
                assert(tp == SIRType.Integer)
            case _ => fail(s"Expected Apply, got $fullApp")
    }

    test("apply with Decl preserves Decl order") {
        val lam = mkLam("x", SIRType.Integer, mkVar("x", SIRType.Integer))
        val decl1 = mkDataDecl("Outer")
        val decl2 = mkDataDecl("Middle")
        val decl3 = mkDataDecl("Inner")

        // Create nested Decls: Outer(Middle(Inner(lam)))
        val nested = SIR.Decl(decl1, SIR.Decl(decl2, SIR.Decl(decl3, lam)))
        val arg = mkConst(1)

        val result = nested $ arg

        // Verify the Decls are in the same order
        def extractDecls(sir: SIR): List[String] = sir match
            case SIR.Decl(data, term) => data.name :: extractDecls(term)
            case _                    => Nil

        val declNames = extractDecls(result)
        assert(declNames == List("Outer", "Middle", "Inner"))
    }
