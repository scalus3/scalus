package scalus.uplc.transform

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.{Program, Term}
import scalus.uplc.internal.TermTraversal.foldLeft
import scalus.uplc.transform.CommonSubexpressionElimination.AlphaTermKey

import scala.collection.mutable

/** On real scripts, [[AlphaIds]] must partition the subterms exactly as [[AlphaTermKey]] does. */
class AlphaIdsUseCaseTest extends AnyFunSuite {

    for script <- List("stablecoin_1-5", "uniswap-3", "auction_1-2") do
        test(s"IDs partition the subterms of $script like AlphaTermKey") {
            val bytes = getClass
                .getResourceAsStream(s"/data/plutus_use_cases/$script.flat")
                .readAllBytes()
            val subterms = Program.fromFlatEncoded(bytes).term.foldLeft(Vector.empty[Term])(_ :+ _)
            val ids = new AlphaIds
            val byId = mutable.LinkedHashMap.empty[Int, Term]
            for t <- subterms do
                val first = byId.getOrElseUpdate(ids(t), t)
                assert(new AlphaTermKey(first) == new AlphaTermKey(t), s"one ID: ${t.showShort}")
            // Different IDs must also mean different keys.
            val keys = byId.values.map(new AlphaTermKey(_)).toSet
            assert(keys.size == byId.size)
        }
}
