package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.Constant

class MultiBindingLetPrettyPrinterTest extends AnyFunSuite {
    private val ann = AnnotationsDecl.empty
    private val intTp = SIRType.Integer
    private val intToInt = SIRType.Fun(intTp, intTp)
    private def intConst(v: Int) = SIR.Const(Constant.Integer(v), intTp, ann)
    private def vr(name: String, tp: SIRType = intTp) = SIR.Var(name, tp, ann)

    test("non-rec multi-binding let renders without error") {
        val let = SIR.Let(
          List(Binding("a", intTp, intConst(1)), Binding("b", intTp, intConst(2))),
          vr("a"),
          SIR.LetFlags.None,
          ann
        )
        val rendered = let.show
        assert(rendered.contains("let"), rendered)
        assert(rendered.contains("a: Int"), rendered)
        assert(rendered.contains("b: Int"), rendered)
        assert(rendered.contains("in"), rendered)
    }

    test("rec multi-binding let renders as fun group joined by 'and'") {
        def lam(body: SIR) = SIR.LamAbs(vr("n"), body, List.empty, ann)
        val let = SIR.Let(
          List(
            Binding("f", intToInt, lam(vr("g", intToInt))),
            Binding("g", intToInt, lam(vr("f", intToInt)))
          ),
          vr("f", intToInt),
          SIR.LetFlags.Recursivity,
          ann
        )
        val rendered = let.show
        assert(rendered.contains("fun f"), rendered)
        assert(rendered.contains("and"), rendered)
        assert(rendered.contains("fun g"), rendered)
        assert(rendered.contains("in"), rendered)
    }
}
