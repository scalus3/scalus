package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.utils.Style
import scalus.uplc.DefaultFun.AddInteger

class PrettyDecoratedTest extends AnyFunSuite {
    private val term = Term.Apply(
      Term.Apply(Term.Builtin(AddInteger), Term.Const(Constant.Integer(1))),
      Term.Const(Constant.Integer(2))
    )

    test("identity decorator renders identically to pretty") {
        val sanitized = TermSanitizer.sanitizeNames(term)
        val doc = TermPrinter.prettySanitized(sanitized, Style.Normal, (_, d) => d)
        assert(doc.render(80) == term.show)
    }

    test("decorator wraps every printed node") {
        var count = 0
        val sanitized = TermSanitizer.sanitizeNames(term)
        TermPrinter
            .prettySanitized(sanitized, Style.Normal, (_, d) => { count += 1; d })
            .render(80)
        // builtin + 2 consts + outermost Apply of the flattened chain = 4
        assert(count == 4)
    }
}
