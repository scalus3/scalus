package scalus.compiler

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.compile

import scala.language.implicitConversions

/** Audit finding M2 (compiler plugin audit): the Inline/ByReference embedding heuristic for
  * pattern-match actions and guards multiplied the action size by the action's *index* `i` in
  * `parsedActions` instead of its usage *count*. Consequence: the action of the first case (index
  * 0) was always inlined regardless of size and reuse, duplicating large shared actions (e.g. from
  * or-patterns) in the generated SIR.
  */
class PatternActionEmbeddingTest extends AnyFunSuite {

    test("big shared or-pattern action at case index 0 is embedded once, not duplicated") {
        import scalus.cardano.onchain.plutus.prelude.*
        val sir = compile { (x: These[BigInt, BigInt]) =>
            x match
                case These.This(_) | These.That(_) =>
                    BigInt(31337) + BigInt(1) + BigInt(2) + BigInt(3) + BigInt(4) + BigInt(5)
                case These.These(a, b) => a + b
        }
        val rendered = sir.pretty.render(120)
        val occurrences = "31337".r.findAllIn(rendered).size
        assert(
          occurrences == 1,
          s"expected the shared action to appear once (by reference), found it $occurrences times"
        )
    }
}
