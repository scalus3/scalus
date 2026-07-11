package scalus.compiler.plugin

import org.scalatest.funsuite.AnyFunSuite

/** Audit finding M6 (compiler plugin audit): `SIRTyper` contained three `???` placeholders
  * (`SuperType`, the `makeSIRFunType` fallback, and `makeFunTypeLambda`). Hitting one crashed the
  * compiler with an unpositioned `scala.NotImplementedError` and no diagnostic at all. They must
  * report a positioned unsupported-type error instead.
  *
  * The reachable repro: a function type nested inside an intersection whose top-level components
  * are not classes routes through `makeSIRClassTypeNoTypeArgs` into `makeFunTypeLambda`.
  */
class UnsupportedTypeErrorTest extends AnyFunSuite with SnippetCompilation {

    test("function type nested in an intersection reports an unsupported-type error") {
        val (errors, _) = compileSnippetAllowingCrash(
          """import scalus.compiler.*
            |object W {
            |    val sir = compile {
            |        def f[A, B](x: (A & (BigInt => BigInt)) & B): BigInt = BigInt(0)
            |        f[BigInt => BigInt, BigInt => BigInt]((x: BigInt) => x)
            |    }
            |}
            |""".stripMargin
        )
        assert(
          errors.exists(_.toLowerCase.contains("unsupported type")),
          s"expected a positioned unsupported-type diagnostic, got: $errors"
        )
    }
}
