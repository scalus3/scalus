package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.{CardanoInfo, ExUnits}
import scalus.cardano.onchain.plutus.prelude.{List as SList, Ord}
import scalus.compiler.{compile, Compile, Options, UplcRepr, UplcRepresentation}
import scalus.uplc.Term
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.{Data, FromData, ToData}
import scalus.uplc.builtin.Data.{fromData, toData}

/** Direct coverage for `sort` on the `UplcConstr` list representation.
  *
  * `List.sort` once had two independent implementations: the prelude body and a
  * `UplcConstrListOperations.sort` intrinsic dispatched by `IntrinsicResolver`. The intrinsic was
  * deleted after measurement showed every input shape was cheaper without it, so BOTH
  * representations now run the one prelude body. This suite still earns its place: it is the only
  * coverage of `sort` reached through a `UplcConstr`-represented receiver, which lowers differently
  * from the Data-packed path even though the source is shared.
  *
  * Before this suite existed, that path's only coverage was the Knights benchmark, which reaches it
  * via `descAndNo.quicksort` inside a `@UplcRepr(UplcConstr) descendants`. That is real coverage
  * but weak for this purpose: Knights compares budgets with a 5% tolerance and the sort is a small
  * share of its 24.5-billion-step total, so an algorithm swap can pass unnoticed, and a wrong sort
  * would surface only as a wrong tour rather than as a diagnosable failure.
  *
  * The wrapper exists because a bare `List[BigInt]` argument arrives Data-packed. Reading the list
  * out of a `@UplcRepr(UplcConstr)` field is what puts it in the representation under test.
  */
class UplcConstrSortTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    /** The lowering production actually uses: no error traces, traces removed, optimizer on. */
    private given Options = Options.release

    private val prices = CardanoInfo.mainnet.protocolParams.executionUnitPrices

    private val sortUplc: Term = compile { (d: Data) =>
        UplcConstrSortFixture.sortItems(fromData[UplcConstrSortFixture.Box](d)).toData
    }.toUplc()

    /** Same program with the optimizer off: the shipped `sort` carries an `INVESTIGATION` note
      * about a lowering failure that appears only in that mode.
      */
    private val sortUplcUnoptimized: Term = compile { (d: Data) =>
        UplcConstrSortFixture.sortItems(fromData[UplcConstrSortFixture.Box](d)).toData
    }.toUplc(using Options.release.copy(optimizeUplc = false))()

    /** The same sort reached through a plain Data-packed list instead of the `UplcConstr` wrapper,
      * so it runs the PRELUDE `sort` body rather than the intrinsic.
      *
      * This is what a caller pays if the `UplcConstr` sort intrinsic did not exist. The two are not
      * perfectly isolated — the wrapper path also pays to decode the `Box` — so the comparison
      * brackets the intrinsic's value rather than pinning it exactly.
      */
    private val sortDataPacked: Term = compile { (d: Data) =>
        UplcConstrSortFixture.sortPlain(fromData[SList[BigInt]](d)).toData
    }.toUplc()

    private def patterns(n: Int): Seq[(String, Seq[BigInt])] = Seq(
      "random" -> new scala.util.Random(42).shuffle((1 to n).map(BigInt(_)).toVector),
      "ascending" -> (1 to n).map(BigInt(_)),
      "descending" -> (n to 1 by -1).map(BigInt(_)),
      "allEqual" -> Seq.fill(n)(BigInt(1)),
      "fewUnique" -> (1 to n).map(i => BigInt(i % 3)),
      "organPipe" -> ((1 to n / 2).map(BigInt(_)) ++ ((n - n / 2) to 1 by -1).map(BigInt(_)))
    )

    private val sizes = Seq(0, 1, 2, 3, 4, 5, 8, 13, 16, 24, 32)

    private def boxData(xs: Seq[BigInt]): Data =
        toData(UplcConstrSortFixture.Box(SList.from(xs)))

    private def run(term: Term, xs: Seq[BigInt]): (Term, ExUnits) =
        (term $ boxData(xs).asTerm).evaluateDebug match
            case Result.Success(r, b, _, _) => (r, b)
            case f                          => fail(s"evaluation failed on $xs: $f")

    test("sorts correctly on every input shape and size") {
        for
            n <- sizes
            (patName, input) <- patterns(n)
        do
            val (result, _) = run(sortUplc, input)
            assert(
              result == toData(SList.from(input.sorted)).asTerm,
              s"wrong result for $patName n=$n"
            )
    }

    test("sorts correctly with optimizeUplc = false") {
        for
            n <- sizes
            (patName, input) <- patterns(n)
        do
            val (result, _) = run(sortUplcUnoptimized, input)
            assert(
              result == toData(SList.from(input.sorted)).asTerm,
              s"wrong result for $patName n=$n with optimizeUplc=false"
            )
    }

    /** The defect that motivated the change: a head-pivot quicksort is Theta(n^2) on ascending,
      * descending and all-equal input, so those shapes cost dramatically more than random. A
      * Theta(n log n) sort keeps them in line with it. The bound is deliberately loose — this
      * guards against a regression to quadratic behaviour, not against normal cost drift.
      */
    test("structured inputs cost no more than random input, which quicksort could not manage") {
        val n = 32
        val randomCost = run(sortUplc, patterns(n).toMap.apply("random"))._2
        for shape <- Seq("ascending", "descending", "allEqual", "organPipe") do
            val cost = run(sortUplc, patterns(n).toMap.apply(shape))._2
            assert(
              cost.steps <= randomCost.steps * 2,
              s"$shape at n=$n costs ${cost.steps} vs random ${randomCost.steps}: " +
                  "more than 2x suggests a return to quadratic behaviour"
            )
    }

    test("report: cost by shape at n=32") {
        for (patName, input) <- patterns(32) do
            val (_, b) = run(sortUplc, input)
            info(
              f"$patName%-11s cpu=${b.steps}%12d mem=${b.memory}%9d fee=${b.fee(prices).value}%8d"
            )
    }

    /** Is the `UplcConstr` sort intrinsic worth its maintenance cost?
      *
      * It is intricate code with constraints no other sort has (self-contained, no nested annotated
      * lists, hand-annotated representations). If a Data-packed list running the ordinary prelude
      * `sort` is close in cost, the intrinsic is not paying for itself and could be deleted.
      *
      * Caveat on reading these numbers: the `UplcConstr` side also pays to decode the `Box`
      * wrapper, and the two sides run different merge sorts (counted vs natural), so this brackets
      * the difference rather than isolating the intrinsic.
      */
    /** Kept after the intrinsic was deleted, now as a regression guard rather than a decision aid:
      * both columns run the same prelude sort, so a large divergence would mean the `UplcConstr`
      * lowering had drifted from the Data-packed one.
      */
    test("UplcConstr and Data-packed paths stay within sight of each other") {
        info("both columns now run the same prelude sort; the wrapper decode explains the gap")
        info(
          f"${"n"}%4s ${"shape"}%-11s ${"UplcConstr"}%14s ${"DataPacked"}%14s ${"ratio"}%8s"
        )
        for
            n <- Seq(4, 8, 16, 32)
            (patName, input) <- patterns(n)
        do
            val (_, constr) = run(sortUplc, input)
            val dataArg = toData(SList.from(input))
            val dataCost = (sortDataPacked $ dataArg.asTerm).evaluateDebug match
                case Result.Success(_, b, _, _) => b
                case f                          => fail(s"data-packed sort failed: $f")
            info(
              f"$n%4d $patName%-11s ${constr.steps}%14d ${dataCost.steps}%14d " +
                  f"${dataCost.steps.toDouble / constr.steps}%7.2fx"
            )
    }
}

@Compile
object UplcConstrSortFixture {

    /** Holds the list in the `UplcConstr` representation, so that reading `items` and sorting it
      * dispatches to `UplcConstrListOperations.sort` rather than the prelude body.
      */
    @UplcRepr(UplcRepresentation.UplcConstr)
    case class Box(
        @UplcRepr(UplcRepresentation.UplcConstr)
        items: SList[BigInt]
    )

    given ToData[Box] = ToData.derived
    given FromData[Box] = FromData.derived

    def sortItems(b: Box): SList[BigInt] = b.items.sort

    /** Same call on a plain Data-packed list, which dispatches to the prelude `sort` body rather
      * than to `UplcConstrListOperations.sort`.
      */
    def sortPlain(xs: SList[BigInt]): SList[BigInt] = xs.sort
}
