package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite

/** Spec `[kit.exunits.bounded-given]` in `docs/internal/test-refactor/d6-rules-kits.md`. */
class ValidatorRulesTestKitTest extends AnyFunSuite, ValidatorRulesTestKit {

    private val max = CardanoInfo.mainnet.protocolParams.maxTxExecutionUnits

    private def withinMax(units: ExUnits): Boolean =
        units.memory <= max.memory && units.steps <= max.steps

    // Fixed seeds: the draws are the same on every run, and a failure names its seed.
    private def draws[A](gen: Gen[A], n: Int): Seq[(Long, A)] =
        (0L until n.toLong).map(i => i -> gen.pureApply(Gen.Parameters.default, Seed(i)))

    test(
      "the redeemers of an arbitrary transaction in the kit fit in maxTxExecutionUnits together"
    ) {
        def total(rs: Redeemers) = rs.toSeq.map(_.exUnits).foldLeft(ExUnits.zero)(_ + _)
        // The bound is on `Redeemers`; 300 draws of it are cheap, of a full transaction not.
        val drawn = draws(arbitrary[Redeemers], 300)
        assert(drawn.exists(_._2.toSeq.size == 3), "no generated Redeemers had 3 redeemers")
        for (seed, rs) <- drawn do assert(withinMax(total(rs)), s"seed $seed: $rs")
        // A few transactions show that the kit's given reaches `Arbitrary[Transaction]`.
        for
            (seed, tx) <- draws(arbitrary[Transaction], 20)
            rs <- tx.witnessSet.redeemers.map(_.value)
        do assert(withinMax(total(rs)), s"seed $seed: $rs")
    }

    test("Arbitrary[ExUnits] stays wide in the kit") {
        assert(draws(arbitrary[ExUnits], 1000).exists((_, u) => !withinMax(u)))
    }

    test("genExUnits by name keeps the wide part above maxTxExecutionUnits") {
        assert(draws(genExUnits, 1000).exists((_, u) => !withinMax(u)))
    }
}
