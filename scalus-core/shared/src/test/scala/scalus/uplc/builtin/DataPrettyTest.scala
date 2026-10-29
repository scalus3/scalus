package scalus.uplc.builtin

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc.*
import scalus.uplc.builtin.Data.{B, Constr, I, Map}
import scalus.uplc.test.ArbitraryInstances
import scalus.utils.Pretty.{lit, rainbowBracket}
import scalus.utils.{Pretty, Style}

/** `Pretty[Data]` builds its document without recursing over the nesting (#423). Within the depth
  * it prints in full, the document must be the one the recursive instance built.
  */
class DataPrettyTest extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {

    test("the output is that of the recursive instance it replaced") {
        forAll { (data: Data) =>
            for style <- Seq(Style.Normal, Style.XTerm); width <- Seq(20, 80, 1000) do
                val expected = recursivePretty(data, style, 0).render(width)
                assert(Pretty[Data].pretty(data, style).render(width) == expected)
        }
    }

    /** The recursive instance that `Pretty[Data]` replaced, kept as an oracle. */
    private def recursivePretty(a: Data, style: Style, depth: Int): Doc = a match
        case Constr(constr, args) =>
            val argsDoc =
                fill(comma + space, args.toScalaList.map(recursivePretty(_, style, depth + 1)))
            val inner = lit(str(constr), style) + comma &
                rainbowBracket(argsDoc, '[', ']', depth + 1, style)
            rainbowBracket(inner, '<', '>', depth, style).grouped
        case Map(values) =>
            val entries = values.toScalaList.map { case (k, v) =>
                recursivePretty(k, style, depth + 1) + char(':') + space +
                    recursivePretty(v, style, depth + 1)
            }
            rainbowBracket(fill(comma + space, entries), '{', '}', depth, style).grouped
        case Data.List(values) =>
            val items = values.toScalaList.map(recursivePretty(_, style, depth + 1))
            rainbowBracket(fill(comma + space, items), '[', ']', depth, style).grouped
        case I(value) => lit(str(value), style)
        case B(value) => lit(char('"') + text(value.toHex) + char('"'), style)
}
