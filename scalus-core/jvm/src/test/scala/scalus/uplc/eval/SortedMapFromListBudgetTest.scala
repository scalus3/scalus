package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Tag
import scalus.*
import scalus.cardano.ledger.{CardanoInfo, ExUnits}
import scalus.cardano.onchain.plutus.prelude.List as SList
import scalus.compiler.{compile, Options}
import scalus.serialization.flat.Flat
import scalus.uplc.Term
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{fromData, toData}

/** Execution-budget comparison of `SortedMap.fromList` implementations.
  *
  * `SortedMap.fromList` USED to fold LEFT, inserting each pair into a growing sorted accumulator –
  * an insertion sort. Folding left made ALREADY-ASCENDING input its worst case, because every
  * insert walked the whole accumulator to reach the end, and ascending is how ledger data arrives.
  * It now folds right; `baseline(shipped)` below preserves the old body so the comparison survives.
  *
  * Candidates live in [[SortedMapFromListCandidates]]. All of them keep the documented contract:
  * strictly ascending keys, first occurrence of a duplicate key wins.
  *
  * Tagged `scalus.testing.Benchmark`, so it is EXCLUDED from the default test run: it performs a
  * couple of thousand UPLC evaluations, including quadratic candidates at n=48 and n=64. Run it
  * deliberately with:
  * `scalusJVM/testOnly SortedMapFromListBudgetTest -- -n scalus.testing.Benchmark -oD`
  *
  * Excluding it costs no coverage of SHIPPED behaviour: `SortedMapTest` stays in the default run
  * and asserts that both constructors agree and that each resolves a duplicate key to the first
  * occurrence.
  */
class SortedMapFromListBudgetTest extends AnyFunSuite {

    /** Mirrors `BilinearAccumulatorBenchmarkTest`: scalus-core tests cannot see
      * `scalus.testing.Benchmark` (it lives in scalus-testkit), so the tag is declared locally with
      * the same name that `build.sbt` excludes.
      */
    private object Benchmark extends Tag("scalus.testing.Benchmark")

    private given PlutusVM = PlutusVM.makePlutusV3VM()
    private given Options = Options.release

    private val prices = CardanoInfo.mainnet.protocolParams.executionUnitPrices
    private val flat = summon[Flat[Term]]

    /** Key patterns. Values are `key * 1000 + occurrence` so that a wrong duplicate-resolution
      * choice produces a visibly different answer rather than an accidentally equal one.
      */
    private val patterns: Seq[(String, Int => Seq[BigInt])] = Seq(
      "random" -> (n => new scala.util.Random(42).shuffle((1 to n).map(BigInt(_)).toVector)),
      "ascending" -> (n => (1 to n).map(BigInt(_))),
      "descending" -> (n => (n to 1 by -1).map(BigInt(_))),
      "allEqual" -> (n => Seq.fill(n)(BigInt(1))),
      "fewUnique" -> (n => (1 to n).map(i => BigInt(i % 3))),
      "organPipe" -> (n => (1 to n / 2).map(BigInt(_)) ++ ((n - n / 2) to 1 by -1).map(BigInt(_)))
    )

    private val sizes = Seq(0, 1, 2, 3, 4, 6, 8, 10, 12, 16, 24, 32, 48, 64)

    private def pairs(keys: Seq[BigInt]): Seq[(BigInt, BigInt)] =
        keys.zipWithIndex.map { case (k, i) => (k, k * 1000 + i) }

    private def asData(ps: Seq[(BigInt, BigInt)]): Data = toData(SList.from(ps))

    /** The contract: ascending by key, first occurrence of a duplicate key wins. */
    private def expected(ps: Seq[(BigInt, BigInt)]): Data = {
        val firstWins = ps.foldLeft(Vector.empty[(BigInt, BigInt)]) { (acc, p) =>
            if acc.exists(_._1 == p._1) then acc else acc :+ p
        }
        asData(firstWins.sortBy(_._1))
    }

    private val candidates: Seq[(String, Term)] = Seq(
      "baseline(shipped)" -> compile { (d: Data) =>
          SortedMapFromListCandidates.baseline(fromData[SList[(BigInt, BigInt)]](d)).toData
      }.toUplc(),
      "insertRight" -> compile { (d: Data) =>
          SortedMapFromListCandidates.insertRight(fromData[SList[(BigInt, BigInt)]](d)).toData
      }.toUplc(),
      "sortDedup(natural)" -> compile { (d: Data) =>
          SortedMapFromListCandidates.sortDedup(fromData[SList[(BigInt, BigInt)]](d)).toData
      }.toUplc(),
      // The actual shipped implementation, which reuses `List.sort` rather than open-coding its
      // own merge. That sharing is the point, so it has to be measured as shipped.
      "SHIPPED(fromList)" -> compile { (d: Data) =>
          scalus.cardano.onchain.plutus.prelude.SortedMap
              .fromList(fromData[SList[(BigInt, BigInt)]](d))
              .toList
              .toData
      }.toUplc(),
      // The Theta(n log n) constructor. Also the only place the PRELUDE `sortWith` is measured,
      // as opposed to the local candidates above, so it guards that path against regression.
      "SHIPPED(fromLargeList)" -> compile { (d: Data) =>
          scalus.cardano.onchain.plutus.prelude.SortedMap
              .fromLargeList(fromData[SList[(BigInt, BigInt)]](d))
              .toList
              .toData
      }.toUplc()
    )

    private case class Cell(budget: ExUnits, ok: Boolean)

    private lazy val matrix: Map[(String, String, Int), Cell] = {
        val b = Map.newBuilder[(String, String, Int), Cell]
        for
            n <- sizes
            (patName, gen) <- patterns
        do
            val ps = pairs(gen(n))
            val arg = asData(ps)
            val want = expected(ps).asTerm
            for (name, term) <- candidates do
                val cell = (term $ arg.asTerm).evaluateDebug match
                    case Result.Success(r, bud, _, _) => Cell(bud, r == want)
                    case Result.Failure(_, bud, _, _) => Cell(bud, false)
                b += ((name, patName, n) -> cell)
        b.result()
    }

    private def worst(name: String, n: Int): (Cell, String) =
        patterns.map { case (p, _) => (matrix((name, p, n)), p) }.maxBy(_._1.budget.steps)

    test("every candidate honours the fromList contract on every input", Benchmark) {
        val wrong = matrix.filterNot(_._2.ok).keys.toSeq.sorted
        assert(wrong.isEmpty, s"contract violated for: ${wrong.mkString(", ")}")
    }

    test("script size", Benchmark) {
        for (name, term) <- candidates do info(f"$name%-20s ${flat.bitSize(term) / 8}%6d bytes")
    }

    test("worst-case CPU across key patterns", Benchmark) {
        info(f"${"n"}%4s" + candidates.map(c => f"${c._1}%20s").mkString)
        for n <- sizes do
            info(f"$n%4d" + candidates.map(c => f"${worst(c._1, n)._1.budget.steps}%20d").mkString)
    }

    test("worst-case MEMORY across key patterns", Benchmark) {
        info("Memory is the binding resource and the dominant fee term; CPU is the cheap half.")
        info(f"${"n"}%4s" + candidates.map(c => f"${c._1}%20s").mkString)
        for n <- sizes do
            info(f"$n%4d" + candidates.map(c => f"${worst(c._1, n)._1.budget.memory}%20d").mkString)
    }

    test("worst-case FEE in lovelace across key patterns", Benchmark) {
        info(f"${"n"}%4s" + candidates.map(c => f"${c._1}%20s").mkString)
        for n <- sizes do
            info(
              f"$n%4d" + candidates
                  .map(c => f"${worst(c._1, n)._1.budget.fee(prices).value}%20d")
                  .mkString
            )
    }

    test("memory on ascending keys, the shape ledger data actually has", Benchmark) {
        info(f"${"n"}%4s" + candidates.map(c => f"${c._1}%20s").mkString)
        for n <- sizes do
            info(
              f"$n%4d" + candidates
                  .map(c => f"${matrix((c._1, "ascending", n)).budget.memory}%20d")
                  .mkString
            )
    }

    test("cost on ascending keys – the shape ledger data actually has", Benchmark) {
        info("tx.inputs arrives ordered by TxOutRef and SortedMap contents are key-ordered, so an")
        info("already-ascending key list is the common case for fromList, not a corner case.")
        info(f"${"n"}%4s" + candidates.map(c => f"${c._1}%20s").mkString)
        for n <- sizes do
            info(
              f"$n%4d" + candidates
                  .map(c => f"${matrix((c._1, "ascending", n)).budget.steps}%20d")
                  .mkString
            )
    }

    test("worst-case ranking with fees", Benchmark) {
        for n <- sizes do
            val rows = candidates
                .map { case (name, _) =>
                    val (cell, pat) = worst(name, n)
                    (name, cell.budget.steps, cell.budget.fee(prices).value, pat)
                }
                .sortBy(_._2)
            info(f"--- n=$n%d ---")
            for (name, cpu, fee, pat) <- rows do
                info(f"    $name%-20s cpu=$cpu%13d fee=$fee%8d worstPattern=$pat")
    }
}
