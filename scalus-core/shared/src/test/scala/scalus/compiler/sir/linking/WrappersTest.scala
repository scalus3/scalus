package scalus.compiler.sir.linking

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.{AnnotationsDecl, Binding, ConstrDecl, DataDecl, SIR, SIRType}
import scalus.uplc.Constant

class WrappersTest extends AnyFunSuite {

    private val anns = AnnotationsDecl.empty

    private def int(value: Int): SIR =
        SIR.Const(Constant.Integer(value), SIRType.Integer, anns)

    private def let(name: String, value: SIR, body: SIR): SIR =
        SIR.Let(List(Binding(name, SIRType.Integer, value)), body, SIR.LetFlags.None, anns)

    private val data =
        DataDecl(
          "example.Marker",
          List(ConstrDecl("example.Marker", Nil, Nil, Nil, anns)),
          Nil,
          anns
        )

    private val result = SIR.Var("x", SIRType.Integer, anns)

    /** A value of the expression's own, under two definitions and a data declaration, as linking
      * leaves a program.
      */
    private val expression = let("x", int(3), result)
    private val defined =
        let("example.Module$.f", int(1), let("example.Module$.g", int(2), expression))
    private val program = SIR.Decl(data, defined)

    test("every declaration and let an expression starts with is taken, and put back") {
        val (wrappers, inner) = Wrappers.of(program)
        assert(inner == result)
        assert(wrappers.declarations == List(data))
        assert(
          wrappers.bindings.map(_.name) == List("example.Module$.f", "example.Module$.g", "x")
        )
        assert(wrappers(inner) == program)
        // What was made of the expression goes inside the same wrappers.
        assert(
          wrappers(int(4)) == SIR.Decl(
            data,
            let(
              "example.Module$.f",
              int(1),
              let("example.Module$.g", int(2), let("x", int(3), int(4)))
            )
          )
        )
    }

    test("the definitions of a linked program end at a let of its own") {
        val (definitions, inner) = Wrappers.definitions(defined)
        assert(inner == expression)
        assert(definitions.bindings.map(_.name) == List("example.Module$.f", "example.Module$.g"))
        assert(definitions(inner) == defined)
    }

    test("a data declaration is not among the definitions") {
        val (definitions, inner) = Wrappers.definitions(program)
        assert(definitions.layers.isEmpty)
        assert(inner == program)
    }

    test("an expression with nothing around it is its own") {
        val (wrappers, inner) = Wrappers.of(result)
        assert(wrappers.layers.isEmpty)
        assert(inner == result)
        assert(wrappers(inner) == result)
    }
}
