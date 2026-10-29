package scalus.testing.budget

import scalus.testing.internal.SourceText

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.Math.*
import scalus.cardano.onchain.plutus.prelude.absolute

/** The automatic pin labels: the caller's source text, also through inline methods. */
class SourceTextTest extends AnyFunSuite {
    private inline def once(inline code: Any): String = SourceText.of(code)
    private inline def twice(inline code: Any): String = once(code)

    test("direct call: the source text, whitespace collapsed") {
        assert(SourceText.of(math.min(3, 4)) == "math.min(3, 4)")
    }

    test("through inline methods: the caller's text, not the parameter name") {
        assert(once(gcd(BigInt(0), BigInt(0))) == "gcd(BigInt(0), BigInt(0))")
        assert(twice(gcd(BigInt(0), BigInt(0))) == "gcd(BigInt(0), BigInt(0))")
    }

    test("an inline call the user wrote: its text, not its expanded body") {
        assert(once(BigInt(0).absolute) == "BigInt(0).absolute")
        assert(twice(BigInt(0).absolute) == "BigInt(0).absolute")
    }
}
