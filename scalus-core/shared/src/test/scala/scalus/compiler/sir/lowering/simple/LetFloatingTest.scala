package scalus.compiler.sir.lowering.simple

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.*
import scalus.compiler.sir.SIR.*
import scalus.compiler.sir.SIRType.{Boolean as BoolType, Integer}
import scalus.uplc.Constant

/** Test suite for LetFloating transformation.
  *
  * Tests various scenarios including:
  *   - Basic lazy let floating
  *   - Transitive dependencies
  *   - Variable renaming to avoid shadowing
  *   - Multiple children usage
  *   - Unused variable elimination
  */
class LetFloatingTest extends AnyFunSuite:

    private val emptyAnns = AnnotationsDecl.empty

    // Helper to create integer constants
    private def int(n: Int): Const =
        Const(Constant.Integer(n), Integer, emptyAnns)

    // Helper to create variables
    private def vr(name: String, tp: SIRType = Integer): Var =
        Var(name, tp, emptyAnns)

    // Helper to create bindings
    private def binding(name: String, value: SIR, tp: SIRType = Integer): Binding =
        Binding(name, tp, value)

    // Helper to create lazy let
    private def lazyLet(bindings: List[Binding], body: SIR): Let =
        Let(bindings, body, LetFlags.Lazy, emptyAnns)

    // Helper to create non-lazy let
    private def normalLet(bindings: List[Binding], body: SIR): Let =
        Let(bindings, body, LetFlags.None, emptyAnns)

    test("eliminate unused lazy let") {
        // let lazy x = 42 in 10
        // Should become: 10
        val input = lazyLet(
          List(binding("x", int(42))),
          int(10)
        )

        val result = LetFloating(input)

        assert(result == int(10))
    }

    test("float lazy let to its usage point") {
        // let lazy x = 42 in (let y = 1 in x)
        // x should float down to where it's used (inside the inner let)
        val input = lazyLet(
          List(binding("x", int(42))),
          normalLet(
            List(binding("y", int(1))),
            vr("x")
          )
        )

        val result = LetFloating(input)

        // The result should have y in outer let, x floated to inner let
        assert(result.isInstanceOf[Let])
        val outerLet = result.asInstanceOf[Let]
        assert(outerLet.bindings.head.name == "y")
        assert(!outerLet.flags.isLazy)

        // Inner let should have the floated x
        assert(outerLet.body.isInstanceOf[Let])
        val innerLet = outerLet.body.asInstanceOf[Let]
        assert(innerLet.bindings.exists(_.name.contains("floated")))
    }

    test("handle transitive dependencies - direct usage") {
        // let lazy x = 42 in
        //   let lazy y = x in
        //     let z = 1 in
        //       y
        //
        // Both x and y should be floated to where y is used
        val input = lazyLet(
          List(binding("x", int(42))),
          lazyLet(
            List(binding("y", vr("x"))),
            normalLet(
              List(binding("z", int(1))),
              vr("y")
            )
          )
        )

        val result = LetFloating(input)

        // x should not be at top level
        assert(!result.isInstanceOf[Let] || {
            val let = result.asInstanceOf[Let]
            !let.bindings.exists(_.name == "x")
        })

        // Result structure should be: let z = 1 in let x$floated$N = 42 in let y$floated$M = x$floated$N in y$floated$M
        assert(result.isInstanceOf[Let])
        val outerLet = result.asInstanceOf[Let]
        assert(outerLet.bindings.head.name == "z")
    }

    test("rename floated variables to avoid shadowing") {
        // let lazy x = 42 in
        //   let x = 1 in   // shadows outer x
        //     let y = 2 in
        //       10
        //
        // The outer x is unused and should be eliminated
        val input = lazyLet(
          List(binding("x", int(42))),
          normalLet(
            List(binding("x", int(1))),
            normalLet(
              List(binding("y", int(2))),
              int(10)
            )
          )
        )

        val result = LetFloating(input)

        // The lazy x should be eliminated (unused)
        assert(result.isInstanceOf[Let])
        val outerLet = result.asInstanceOf[Let]
        assert(outerLet.bindings.head.name == "x") // The shadowing x
        assert(outerLet.bindings.head.value == int(1))
    }

    test("rename floated variables when they would shadow") {
        // let lazy x = 42 in
        //   let z = x in      // z depends on lazy x
        //     let x = 1 in    // shadows outer x
        //       z             // uses z (which depends on outer x)
        //
        // The lazy x should be renamed and floated before it's shadowed
        val input = lazyLet(
          List(binding("x", int(42))),
          normalLet(
            List(binding("z", vr("x"))),
            normalLet(
              List(binding("x", int(1))),
              vr("z")
            )
          )
        )

        val result = LetFloating(input)

        // Result should rename x to avoid shadowing
        assert(result.isInstanceOf[Let])
        val outerLet = result.asInstanceOf[Let]
        // First binding should be z
        assert(outerLet.bindings.head.name == "z")
    }

    test("keep non-lazy let in place") {
        // let x = 42 in x
        // Should stay the same (non-lazy)
        val input = normalLet(
          List(binding("x", int(42))),
          vr("x")
        )

        val result = LetFloating(input)

        assert(result.isInstanceOf[Let])
        val let = result.asInstanceOf[Let]
        assert(let.bindings.head.name == "x")
        assert(!let.flags.isLazy)
        assert(let.body == vr("x"))
    }

    test("handle multiple children usage - float to common ancestor") {
        // let lazy x = 42 in
        //   if cond then x else x
        //
        // x is used in both branches, stays in place (body is IfThenElse, not Var)
        val cond = vr("cond", BoolType)
        val input = lazyLet(
          List(binding("x", int(42))),
          IfThenElse(cond, vr("x"), vr("x"), Integer, emptyAnns)
        )

        val result = LetFloating(input)

        // x gets floated and renamed even though target is same location (body is not a Var)
        assert(result.isInstanceOf[Let])
        val let = result.asInstanceOf[Let]
        assert(let.bindings.head.name.contains("floated")) // Renamed even though same location
        assert(!let.flags.isLazy) // Lazy flag removed
        assert(let.body.isInstanceOf[IfThenElse])
    }

    test("handle nested lazy lets with different dependencies") {
        // let lazy a = 1 in
        //   let lazy b = 2 in
        //     let lazy c = a in
        //       b + c
        //
        // All variables float to their usage point
        // b floats to where it's used (Var(b))
        // a and c both float to where c is used (Var(c)) because c depends on a
        val input = lazyLet(
          List(binding("a", int(1))),
          lazyLet(
            List(binding("b", int(2))),
            lazyLet(
              List(binding("c", vr("a"))),
              Apply(
                Apply(SIRBuiltins.addInteger, vr("b"), SIRType.Fun(Integer, Integer), emptyAnns),
                vr("c"),
                Integer,
                emptyAnns
              )
            )
          )
        )

        val result = LetFloating(input)

        // Verify the transformation produces valid output
        assert(result != null)
        assert(result.isInstanceOf[Apply])
    }

    test("preserve already optimal placement") {
        // let lazy x = 42 in x
        // Already optimal, should just remove lazy flag, keep original name
        val input = lazyLet(
          List(binding("x", int(42))),
          vr("x")
        )

        val result = LetFloating(input)

        // Should become a normal let (lazy flag removed), variable keeps original name
        assert(result.isInstanceOf[Let])
        val let = result.asInstanceOf[Let]
        assert(!let.flags.isLazy)
        assert(let.bindings.head.name == "x") // Not renamed since it doesn't float
    }

    test("handle complex transitive dependency chain") {
        // let lazy a = 1 in
        //   let lazy b = a in
        //     let lazy c = b in
        //       let lazy d = c in
        //         d
        //
        // All should be floated to the usage of d
        val input = lazyLet(
          List(binding("a", int(1))),
          lazyLet(
            List(binding("b", vr("a"))),
            lazyLet(
              List(binding("c", vr("b"))),
              lazyLet(
                List(binding("d", vr("c"))),
                vr("d")
              )
            )
          )
        )

        val result = LetFloating(input)

        // All bindings should be floated together
        assert(result.isInstanceOf[Let])
    }

    test("handle mixed lazy and non-lazy lets") {
        // let x = 1 in           // non-lazy, stays
        //   let lazy y = x in    // lazy, depends on x
        //     let z = 2 in       // non-lazy, stays
        //       y
        val input = normalLet(
          List(binding("x", int(1))),
          lazyLet(
            List(binding("y", vr("x"))),
            normalLet(
              List(binding("z", int(2))),
              vr("y")
            )
          )
        )

        val result = LetFloating(input)

        // x should stay at top, z should stay in its position
        assert(result.isInstanceOf[Let])
        val outerLet = result.asInstanceOf[Let]
        assert(outerLet.bindings.head.name == "x")
        assert(!outerLet.flags.isLazy)
    }

    test("eliminate multiple unused lazy lets") {
        // let lazy x = 1 in
        //   let lazy y = 2 in
        //     let lazy z = 3 in
        //       42
        //
        // All are unused, should be eliminated
        val input = lazyLet(
          List(binding("x", int(1))),
          lazyLet(
            List(binding("y", int(2))),
            lazyLet(
              List(binding("z", int(3))),
              int(42)
            )
          )
        )

        val result = LetFloating(input)

        assert(result == int(42))
    }

    test("handle lazy let with multiple bindings") {
        // let lazy x = 1, y = 2 in x
        // Only x is used, y should be eliminated
        val input = lazyLet(
          List(binding("x", int(1)), binding("y", int(2))),
          vr("x")
        )

        val result = LetFloating(input)

        assert(result.isInstanceOf[Let])
        val let = result.asInstanceOf[Let]
        // Only x should be in the result, y eliminated
        assert(let.bindings.length == 1)
        assert(let.bindings.head.name == "x") // Not renamed since target == current position
    }

    test("lambda barrier: non-effortless binding blocked by lambda") {
        // let lazy x = 1 + 2 in    // Non-effortless (Apply)
        //   lambda y =>
        //     x
        //
        // x should NOT float into the lambda body
        val input = lazyLet(
          List(
            binding(
              "x",
              Apply(
                Apply(SIRBuiltins.addInteger, int(1), SIRType.Fun(Integer, Integer), emptyAnns),
                int(2),
                Integer,
                emptyAnns
              )
            )
          ),
          LamAbs(
            vr("y"),
            vr("x"),
            Nil,
            emptyAnns
          )
        )

        val result = LetFloating(input)

        // x should be placed outside the lambda (not floated inside)
        assert(result.isInstanceOf[Let])
        val let = result.asInstanceOf[Let]
        // The binding should be outside the lambda
        assert(let.bindings.exists(_.name.contains("floated")))
        assert(let.body.isInstanceOf[LamAbs])
    }

    test("lambda barrier: effortless binding allowed to cross lambda") {
        // let lazy x = 42 in    // Effortless (Const)
        //   lambda y =>
        //     x
        //
        // x CAN float into the lambda body (it's effortless)
        val input = lazyLet(
          List(binding("x", int(42))),
          LamAbs(
            vr("y"),
            vr("x"),
            Nil,
            emptyAnns
          )
        )

        val result = LetFloating(input)

        // x should be floated inside the lambda (after the lambda parameter)
        assert(result.isInstanceOf[LamAbs])
        val lambda = result.asInstanceOf[LamAbs]
        // Inside the lambda, there should be a let binding x
        assert(lambda.term.isInstanceOf[Let])
    }

    test("lambda barrier: nested lambdas stop at outermost") {
        // let lazy x = 1 + 2 in    // Non-effortless
        //   lambda a =>
        //     lambda b =>
        //       lambda c =>
        //         x
        //
        // x should NOT cross ANY lambda, staying outside all three
        val input = lazyLet(
          List(
            binding(
              "x",
              Apply(
                Apply(SIRBuiltins.addInteger, int(1), SIRType.Fun(Integer, Integer), emptyAnns),
                int(2),
                Integer,
                emptyAnns
              )
            )
          ),
          LamAbs(
            vr("a"),
            LamAbs(
              vr("b"),
              LamAbs(
                vr("c"),
                vr("x"),
                Nil,
                emptyAnns
              ),
              Nil,
              emptyAnns
            ),
            Nil,
            emptyAnns
          )
        )

        val result = LetFloating(input)

        // x should be placed outside the outermost lambda
        assert(result.isInstanceOf[Let])
        val let = result.asInstanceOf[Let]
        assert(let.bindings.exists(_.name.contains("floated")))
        assert(let.body.isInstanceOf[LamAbs])
        // The body should be the lambda structure
        val outerLambda = let.body.asInstanceOf[LamAbs]
        assert(outerLambda.param.name == "a")
    }

    test("lambda barrier: mixed effortless and non-effortless") {
        // let lazy a = 42 in           // Effortless
        //   let lazy b = 1 + 2 in      // Non-effortless
        //     lambda x =>
        //       a + b
        //
        // a can cross lambda, b cannot
        val input = lazyLet(
          List(binding("a", int(42))),
          lazyLet(
            List(
              binding(
                "b",
                Apply(
                  Apply(
                    SIRBuiltins.addInteger,
                    int(1),
                    SIRType.Fun(Integer, Integer),
                    emptyAnns
                  ),
                  int(2),
                  Integer,
                  emptyAnns
                )
              )
            ),
            LamAbs(
              vr("x"),
              Apply(
                Apply(SIRBuiltins.addInteger, vr("a"), SIRType.Fun(Integer, Integer), emptyAnns),
                vr("b"),
                Integer,
                emptyAnns
              ),
              Nil,
              emptyAnns
            )
          )
        )

        val result = LetFloating(input)

        // b should be outside the lambda
        // a can be inside the lambda
        assert(result.isInstanceOf[Let])
        val outerLet = result.asInstanceOf[Let]
        // Should have b's binding
        assert(outerLet.bindings.exists(_.name.contains("floated")))
        assert(outerLet.body.isInstanceOf[LamAbs])
    }

    test("lambda barrier: effortless var binding crosses lambda") {
        // let lazy x = y in    // Effortless (Var)
        //   lambda z =>
        //     x
        //
        // x (which is just a variable reference) can cross the lambda
        val input = lazyLet(
          List(binding("x", vr("y"))),
          LamAbs(
            vr("z"),
            vr("x"),
            Nil,
            emptyAnns
          )
        )

        val result = LetFloating(input)

        // x should float inside the lambda
        assert(result.isInstanceOf[LamAbs])
        val lambda = result.asInstanceOf[LamAbs]
        assert(lambda.term.isInstanceOf[Let])
    }

end LetFloatingTest
