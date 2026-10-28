package scalus.compiler.plugin

import org.scalatest.funsuite.AnyFunSuite

/** User code compiled with `-Yexplicit-nulls` must go through the Scalus plugin.
  *
  * Under explicit nulls, Java members are typed with `| Null` (3.3) or as flexible types (3.5+).
  * For example, the `RuntimeException(String)` constructor takes `String | Null` on 3.3. The plugin
  * must not depend on the plain `String` type there.
  */
class ExplicitNullsTest extends AnyFunSuite with SnippetCompilation {

    private val explicitNulls = Seq("-Yexplicit-nulls")

    test("a Validator that does not override every endpoint compiles with explicit nulls") {
        // The plugin adds a throwing body for each endpoint that is not overridden (mint, reward,
        // ...). It looks up the RuntimeException(String) constructor to build it.
        val errors = compileSnippet(
          """import scalus.compiler.Compile
            |import scalus.uplc.builtin.Data
            |import scalus.cardano.onchain.plutus.v3.*
            |import scalus.cardano.onchain.plutus.prelude.*
            |
            |@Compile
            |object SpendOnly extends Validator {
            |    inline override def spend(
            |        datum: Option[Data],
            |        redeemer: Data,
            |        tx: TxInfo,
            |        ownRef: TxOutRef
            |    ): Unit = require(datum.isDefined, "datum")
            |}
            |""".stripMargin,
          explicitNulls
        )
        assert(errors.isEmpty, s"expected no errors, got: $errors")
    }

    // Regression guard, not a test of a fix: it also passes without the generateThrow fix. It
    // keeps strings and pattern matching compiling through the plugin under the flag.
    test("on-chain code with strings and pattern matching compiles with explicit nulls") {
        val errors = compileSnippet(
          """import scalus.compiler.*
            |enum E:
            |    case A(s: String)
            |    case B
            |object W {
            |    val sir = compile { (e: E) =>
            |        e match
            |            case E.A(s) => s == "x"
            |            case E.B    => false
            |    }
            |}
            |""".stripMargin,
          explicitNulls
        )
        assert(errors.isEmpty, s"expected no errors, got: $errors")
    }
}
