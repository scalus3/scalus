package scalus.compiler.plugin

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.compile
import scalus.toUplc
import scalus.uplc.*
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.PlutusVM

import scala.language.implicitConversions

/** Audit finding E7: the `a -> b` special-case (which hardcodes stdlib `ArrowAssoc` — it has no
  * TASTy) must fire ONLY for `Predef.ArrowAssoc.->`, not for any method named `->` returning a
  * `Tuple2`. Previously it matched on the name alone and then blindly stripped one `Apply` layer
  * off the receiver (assuming it was `ArrowAssoc(a)`), so a user-defined `->` on `Foo(x)` silently
  * compiled to `Tuple2(x, b)`, dropping the receiver. The special-case now keys on the ArrowAssoc
  * `->` symbol, so a user `->` falls through to normal compilation instead of being silently
  * mis-desugared.
  */
class ArrowDesugaringTest extends AnyFunSuite with SnippetCompilation {

    private given PlutusVM = PlutusVM.makePlutusV2VM()

    import scalus.compiler.Options
    import scalus.compiler.sir.TargetLoweringBackend
    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    // Control: the stdlib ArrowAssoc `a -> b` still builds Tuple2(a, b) with correct semantics.
    test("stdlib ArrowAssoc a -> b still builds Tuple2(a, b)") {
        val compiled = compile { (x: BigInt) =>
            val p = x -> (x + BigInt(1))
            p._1 + p._2
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        assert((uplc $ 1.asTerm).evaluate == 3.asTerm) // 1 + 2
    }

    // A user-defined member `->` returning Tuple2 must NOT be silently mis-desugared into
    // Tuple2(receiver-stripped, arg). It now falls through to normal compilation; since Scalus does
    // not compile instance methods on case classes, it surfaces a clear error naming the `->`
    // method instead of silently producing wrong code (the bug compiled this snippet cleanly).
    test("user-defined -> is not silently mis-desugared") {
        val errors = compileSnippet(
          """import scalus.compiler.Compile
            |@Compile
            |object M {
            |    case class Foo(v: BigInt) {
            |        def ->(y: BigInt): (BigInt, BigInt) = (v + BigInt(100), y)
            |    }
            |    def use(x: BigInt): BigInt = {
            |        val p = Foo(x) -> x
            |        p._1
            |    }
            |}
            |""".stripMargin
        )
        assert(
          errors.exists(_.contains("->")),
          s"user-defined `->` must not be silently mis-desugared; expected a diagnostic, got: $errors"
        )
    }
}
