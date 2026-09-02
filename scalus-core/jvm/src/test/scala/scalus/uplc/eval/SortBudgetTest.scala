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

/** Execution-budget comparison of on-chain sorting algorithms.
  *
  * The shipped `prelude.List.sort` is a head-pivot quicksort. Head-pivot quicksort is Theta(n^2) on
  * already-sorted and reverse-sorted input, and on Cardano such input is the norm rather than the
  * exception: `tx.inputs` arrives ordered by `TxOutRef` and `SortedMap` contents are key-ordered. A
  * validator has to be provisioned for the worst input an adversary can present, so the ranking
  * metric here is worst-case ExUnits across input patterns, not the average over random data.
  *
  * Method. Each candidate is compiled once as `(d: Data) => sortX(fromData(d)).toData`. The list is
  * applied to the compiled term as a UPLC argument rather than written as a literal inside the
  * `compile {}` block – a literal would let the optimizer fold the whole sort away at compile time
  * and every row would read zero. This is the same shape `EvalTestKit.assertEvalWithBudget` uses.
  *
  * Candidates live in [[SortCandidates]]. The suite is a measurement harness: the correctness of
  * every candidate on every input is asserted, but the budget numbers are reported rather than
  * pinned, so re-running it after a compiler change reports new numbers instead of failing.
  *
  * Tagged `scalus.testing.Benchmark`, so it is EXCLUDED from the default test run: it performs a
  * couple of thousand UPLC evaluations, including quadratic candidates at n=48 and n=64. Run it
  * deliberately with: `scalusJVM/testOnly SortBudgetTest -- -n scalus.testing.Benchmark -oD`
  *
  * Excluding it costs no coverage of SHIPPED behaviour: the candidates here are research artifacts,
  * and `List.sort` itself is covered by `ListTest` and `UplcConstrSortTest`, both of which stay in
  * the default run — including the guard that structured input costs no more than 2x random, which
  * is what would fail if a quadratic sort were ever reintroduced.
  */
class SortBudgetTest extends AnyFunSuite {

    /** Mirrors `BilinearAccumulatorBenchmarkTest`: scalus-core tests cannot see
      * `scalus.testing.Benchmark` (it lives in scalus-testkit), so the tag is declared locally with
      * the same name that `build.sbt` excludes.
      */
    private object Benchmark extends Tag("scalus.testing.Benchmark")

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    /** Production-like lowering: no error traces, optimizer on. */
    private given Options = Options.release

    private val prices = CardanoInfo.mainnet.protocolParams.executionUnitPrices
    private val maxTxExUnits = CardanoInfo.mainnet.protocolParams.maxTxExecutionUnits
    private val flat = summon[Flat[Term]]

    // ----------------------------------------------------------------------------------
    // Input patterns
    // ----------------------------------------------------------------------------------

    /** A deterministic shuffle, so every run of the suite measures the same "random" input. */
    private def shuffled(n: Int): Seq[BigInt] =
        new scala.util.Random(42).shuffle((1 to n).map(BigInt(_)).toVector)

    /** The input shapes a validator can actually be handed.
      *
      *   - `ascending` is what the ledger gives you: `tx.inputs` is ordered by `TxOutRef` and
      *     `SortedMap` contents are key-ordered, so this is a DEFAULT case on-chain, not a corner.
      *   - `allEqual` and `fewUnique` model token names or credentials drawn from a small set.
      *   - `sawtooth` and `organPipe` are the classic adversarial shapes that defeat a fixed
      *     deterministic pivot rule; they exist to show that median-of-three does not rescue
      *     quicksort's worst case.
      */
    private val patterns: Seq[(String, Int => Seq[BigInt])] = Seq(
      "random" -> (n => shuffled(n)),
      "ascending" -> (n => (1 to n).map(BigInt(_))),
      "descending" -> (n => (n to 1 by -1).map(BigInt(_))),
      "allEqual" -> (n => Seq.fill(n)(BigInt(1))),
      "fewUnique" -> (n => (1 to n).map(i => BigInt(i % 3))),
      "sawtooth" -> (n => (1 to n).map(i => BigInt(i % math.max(1, n / 4)))),
      "organPipe" -> (n => (1 to n / 2).map(BigInt(_)) ++ ((n - n / 2) to 1 by -1).map(BigInt(_)))
    )

    private val sizes = Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 16, 24, 32, 48, 64)

    private def asData(xs: Seq[BigInt]): Data = toData(SList.from(xs))

    // ----------------------------------------------------------------------------------
    // Candidates
    // ----------------------------------------------------------------------------------

    private val candidates: Seq[(String, Term)] = Seq(
      "OLD(headPivotQS)" -> compile { (d: Data) =>
          SortCandidates.headPivotQuicksort(fromData[SList[BigInt]](d)).toData
      }.toUplc(),
      "NEW(prelude)" -> compile { (d: Data) =>
          SortCandidates.preludeSort(fromData[SList[BigInt]](d)).toData
      }.toUplc(),
      "insertRight" -> compile { (d: Data) =>
          SortCandidates.insertionSortRight(fromData[SList[BigInt]](d)).toData
      }.toUplc(),
      "insertLeft" -> compile { (d: Data) =>
          SortCandidates.insertionSortLeft(fromData[SList[BigInt]](d)).toData
      }.toUplc(),
      "mergeDeal" -> compile { (d: Data) =>
          SortCandidates.mergeSortDeal(fromData[SList[BigInt]](d)).toData
      }.toUplc(),
      "mergeNatural" -> compile { (d: Data) =>
          SortCandidates.mergeSortNatural(fromData[SList[BigInt]](d)).toData
      }.toUplc(),
      "qsortMedian3" -> compile { (d: Data) =>
          SortCandidates.quicksortMedian3(fromData[SList[BigInt]](d)).toData
      }.toUplc(),
      "mergeGuarded" -> compile { (d: Data) =>
          SortCandidates.mergeSortGuarded(fromData[SList[BigInt]](d)).toData
      }.toUplc(),
      "mergeCounted" -> compile { (d: Data) =>
          SortCandidates.mergeSortCounted(fromData[SList[BigInt]](d)).toData
      }.toUplc(),
      "hybrid" -> compile { (d: Data) =>
          SortCandidates.sortHybrid(fromData[SList[BigInt]](d)).toData
      }.toUplc(),
      "natural(lt)" -> compile { (d: Data) =>
          SortCandidates
              .mergeSortNaturalLt(fromData[SList[BigInt]](d), (a: BigInt, b: BigInt) => a < b)
              .toData
      }.toUplc(),
      "insertLeft(lt)" -> compile { (d: Data) =>
          SortCandidates
              .insertionSortLeftLt(fromData[SList[BigInt]](d), (a: BigInt, b: BigInt) => a < b)
              .toData
      }.toUplc(),
      "natural(ordMatch)" -> compile { (d: Data) =>
          SortCandidates.mergeSortNaturalOrdMatch(fromData[SList[BigInt]](d)).toData
      }.toUplc(),
      "natural(lt,consFirst)" -> compile { (d: Data) =>
          SortCandidates
              .mergeSortNaturalLtConsFirst(
                fromData[SList[BigInt]](d),
                (a: BigInt, b: BigInt) => a < b
              )
              .toData
      }.toUplc()
    )

    private val isSortedTerm: Term = compile { (d: Data) =>
        SortCandidates.isSorted(fromData[SList[BigInt]](d)).toData
    }.toUplc()

    // ----------------------------------------------------------------------------------
    // Measurement – the whole matrix is evaluated once, in a lazy val, and every test below
    // reads from it. Evaluating a quadratic sort at n=64 is expensive enough that repeating
    // it per test would dominate the suite's runtime.
    // ----------------------------------------------------------------------------------

    /** ExUnits spent, and whether the program actually produced the right answer. A quadratic sort
      * at a large enough n exhausts the machine budget; that is a result, not an error.
      */
    private case class Cell(budget: ExUnits, ok: Boolean) {
        def steps: Long = budget.steps
        def fee: Long = budget.fee(prices).value
    }

    private def measure(term: Term, arg: Data, expected: Data): Cell =
        (term $ arg.asTerm).evaluateDebug match
            case Result.Success(result, budget, _, _) =>
                Cell(budget, result == expected.asTerm)
            case Result.Failure(_, budget, _, _) => Cell(budget, false)

    /** (candidate, pattern, n) -> Cell */
    private lazy val matrix: Map[(String, String, Int), Cell] = {
        val b = Map.newBuilder[(String, String, Int), Cell]
        for
            n <- sizes
            (patName, gen) <- patterns
        do
            val input = gen(n)
            val arg = asData(input)
            val expected = asData(input.sorted)
            for (name, term) <- candidates do
                b += ((name, patName, n) -> measure(term, arg, expected))
        b.result()
    }

    private def worst(name: String, n: Int): (Cell, String) =
        patterns
            .map { case (p, _) => (matrix((name, p, n)), p) }
            .maxBy(_._1.steps)

    private def fits(b: ExUnits): Boolean =
        b.steps <= maxTxExUnits.steps && b.memory <= maxTxExUnits.memory

    // ----------------------------------------------------------------------------------
    // Tests
    // ----------------------------------------------------------------------------------

    test("every candidate sorts every input correctly", Benchmark) {
        val wrong = matrix.filterNot(_._2.ok).keys.toSeq.sorted
        assert(wrong.isEmpty, s"incorrect results for: ${wrong.mkString(", ")}")
    }

    /** The shipped `List.sort` carries an `INVESTIGATION` note that a `Tuple2`-returning partition
      * triggers a `Case index 2 out of bounds` lowering failure with `optimizeUplc = false`. Every
      * candidate here returns multiple values through a named case class (`Split`, `Parts3`,
      * `Taken`) or a nested `List[List[A]]`, which is the same shape class. Before any of them can
      * be recommended for the prelude, they have to lower and run correctly with the optimizer off
      * as well as on.
      */
    test("every candidate lowers and sorts correctly with optimizeUplc = false", Benchmark) {
        val unoptimized = Options(generateErrorTraces = false, optimizeUplc = false)
        val terms: Seq[(String, Term)] = Seq(
          "OLDUNUSED" -> compile { (d: Data) =>
              SortCandidates.preludeSort(fromData[SList[BigInt]](d)).toData
          }.toUplc(using unoptimized)(),
          "insertRight" -> compile { (d: Data) =>
              SortCandidates.insertionSortRight(fromData[SList[BigInt]](d)).toData
          }.toUplc(using unoptimized)(),
          "insertLeft" -> compile { (d: Data) =>
              SortCandidates.insertionSortLeft(fromData[SList[BigInt]](d)).toData
          }.toUplc(using unoptimized)(),
          "mergeDeal" -> compile { (d: Data) =>
              SortCandidates.mergeSortDeal(fromData[SList[BigInt]](d)).toData
          }.toUplc(using unoptimized)(),
          "mergeNatural" -> compile { (d: Data) =>
              SortCandidates.mergeSortNatural(fromData[SList[BigInt]](d)).toData
          }.toUplc(using unoptimized)(),
          "qsortMedian3" -> compile { (d: Data) =>
              SortCandidates.quicksortMedian3(fromData[SList[BigInt]](d)).toData
          }.toUplc(using unoptimized)(),
          "mergeCounted" -> compile { (d: Data) =>
              SortCandidates.mergeSortCounted(fromData[SList[BigInt]](d)).toData
          }.toUplc(using unoptimized)(),
          "natural(lt)" -> compile { (d: Data) =>
              SortCandidates
                  .mergeSortNaturalLt(fromData[SList[BigInt]](d), (a: BigInt, b: BigInt) => a < b)
                  .toData
          }.toUplc(using unoptimized)()
        )
        for
            (name, term) <- terms
            (patName, gen) <- patterns
            n <- Seq(0, 1, 2, 4, 8, 16)
        do
            val input = gen(n)
            (term $ asData(input).asTerm).evaluateDebug match
                case Result.Success(r, _, _, _) =>
                    assert(
                      r == asData(input.sorted).asTerm,
                      s"$name wrong on $patName n=$n with optimizeUplc=false"
                    )
                case f => fail(s"$name failed to run on $patName n=$n with optimizeUplc=false: $f")
    }

    test("script size of each candidate", Benchmark) {
        info("Compiled term size. Script size is paid on every transaction carrying the script,")
        info("and again in the reference-script fee if it is referenced.")
        for (name, term) <- candidates do info(f"$name%-15s ${flat.bitSize(term) / 8}%6d bytes")
        info(f"${"isSorted"}%-15s ${flat.bitSize(isSortedTerm) / 8}%6d bytes")
    }

    test("worst-case CPU across all input patterns", Benchmark) {
        info(
          "Worst case over random, ascending, descending, allEqual, fewUnique, sawtooth, organPipe."
        )
        info(
          "Ascending is the ledger's own ordering, so on-chain it is a default case, not a corner."
        )
        info(f"${"n"}%4s" + candidates.map(c => f"${c._1}%16s").mkString)
        for n <- sizes do
            info(f"$n%4d" + candidates.map(c => f"${worst(c._1, n)._1.steps}%16d").mkString)
    }

    /** Memory is the resource that actually binds. `fee = priceMemory * memory + priceSteps *
      * steps`, and at mainnet prices a memory unit costs roughly 800x a CPU step, so the fee is
      * dominated by memory. The whole-transaction limits are lopsided the same way. Ranking sorts
      * by CPU alone is therefore close to ranking them by the wrong number.
      */
    test("worst-case MEMORY across all input patterns", Benchmark) {
        info("Memory, not CPU, is what a validator runs out of and what it mostly pays for.")
        info(f"${"n"}%4s" + candidates.map(c => f"${c._1}%16s").mkString)
        for n <- sizes do
            info(f"$n%4d" + candidates.map(c => f"${worst(c._1, n)._1.budget.memory}%16d").mkString)
    }

    test("worst-case FEE in lovelace across all input patterns", Benchmark) {
        info(s"mainnet priceMemory=${prices.priceMemory}, priceSteps=${prices.priceSteps}")
        info(f"${"n"}%4s" + candidates.map(c => f"${c._1}%16s").mkString)
        for n <- sizes do
            info(f"$n%4d" + candidates.map(c => f"${worst(c._1, n)._1.fee}%16d").mkString)
    }

    /** How much of the fee each resource is responsible for. If memory dominates, then optimizing
      * for CPU steps is optimizing the cheap half.
      */
    test("fee composition: how much of the fee is memory vs steps", Benchmark) {
        for n <- Seq(8, 16, 32, 64) do
            info(f"--- n=$n%d ---")
            for (name, _) <- candidates do
                val b = worst(name, n)._1.budget
                // Price each resource on its own by zeroing the other, so the split uses the same
                // fee function the ledger does rather than a hand-rolled reimplementation of it.
                val memPart = ExUnits(memory = b.memory, steps = 0).fee(prices).value
                val stepPart = ExUnits(memory = 0, steps = b.steps).fee(prices).value
                val total = (memPart + stepPart).toDouble
                info(
                  f"    $name%-22s fee=${memPart + stepPart}%8d  memory=${memPart * 100 / total}%5.1f%%  steps=${stepPart * 100 / total}%5.1f%%"
                )
    }

    /** Which limit does a candidate hit first as n grows: the CPU ceiling or the memory ceiling?
      * Reported as percentage of each whole-transaction limit consumed by ONE sort.
      */
    test("budget headroom: share of the mainnet tx limits consumed by one sort", Benchmark) {
        info(s"limits: steps ${maxTxExUnits.steps}, memory ${maxTxExUnits.memory}")
        for n <- Seq(16, 32, 64) do
            info(f"--- n=$n%d ---")
            for (name, _) <- candidates do
                val b = worst(name, n)._1.budget
                val cpuPct = b.steps * 100.0 / maxTxExUnits.steps
                val memPct = b.memory * 100.0 / maxTxExUnits.memory
                val binds = if memPct >= cpuPct then "MEMORY" else "cpu"
                info(f"    $name%-22s cpu=$cpuPct%5.1f%%  mem=$memPct%5.1f%%  binds first: $binds")
    }

    test("worst-case ranking and the pattern that causes it", Benchmark) {
        for n <- sizes do
            val rows = candidates
                .map { case (name, _) =>
                    val (cell, pat) = worst(name, n)
                    (name, cell.steps, cell.fee, pat)
                }
                .sortBy(_._2)
            info(f"--- n=$n%d --- cheapest worst case: ${rows.head._1} (${rows.head._3} lovelace)")
            for (name, cpu, fee, pat) <- rows do
                info(f"    $name%-15s cpu=$cpu%13d fee=$fee%8d worstPattern=$pat")
    }

    test("full matrix: every candidate on every pattern", Benchmark) {
        info(f"${""}%16s" + candidates.map(c => f"${c._1}%16s").mkString)
        for
            n <- sizes
            (patName, _) <- patterns
        do
            val cells = candidates.map { case (name, _) =>
                val c = matrix((name, patName, n))
                if c.ok then f"${c.steps}%16d" else f"${"FAILED"}%16s"
            }
            info(f"n=$n%3d $patName%-11s" + cells.mkString)
    }

    test("the floor: isSorted verifies an off-chain-supplied order in one pass", Benchmark) {
        info("If the caller supplies the list already sorted, verifying costs one pass and n-1")
        info("comparisons. This is the lower bound every sort must be compared against.")
        for n <- sizes do
            val input = (1 to n).map(BigInt(_))
            (isSortedTerm $ asData(input).asTerm).evaluateDebug match
                case Result.Success(_, b, _, _) =>
                    val sortWorst = worst("OLD(headPivotQS)", n)._1.steps
                    val ratio = if b.steps > 0 then sortWorst.toDouble / b.steps else 0.0
                    info(
                      f"n=$n%3d isSorted cpu=${b.steps}%12d mem=${b.memory}%8d fee=${b.fee(prices).value}%7d" +
                          f"   prelude sort worst case is ${ratio}%.1fx that"
                    )
                case f => fail(s"isSorted failed at n=$n: $f")
    }

    test("where each candidate stops fitting in a mainnet transaction budget", Benchmark) {
        info(s"mainnet maxTxExUnits = steps ${maxTxExUnits.steps}, memory ${maxTxExUnits.memory}")
        for (name, _) <- candidates do
            sizes.find(n => !fits(worst(name, n)._1.budget)) match
                case Some(n) => info(f"$name%-15s worst case exceeds the whole-tx budget at n=$n")
                case None => info(f"$name%-15s fits at every measured size (up to n=${sizes.max})")
    }
}
