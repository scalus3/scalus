package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.ExUnits
import scalus.cardano.onchain.plutus.prelude.PairList
import scalus.cardano.onchain.plutus.prelude.PairList.*
import scalus.testing.kit.EvalTestKit

class PairListTest extends AnyFunSuite with EvalTestKit {

    test("head") {
        assertEvalWithBudget(
          (pl: PairList[BigInt, BigInt]) => pl.head,
          PairList.single(BigInt(1), BigInt(2)),
          (BigInt(1), BigInt(2)),
          ExUnits(memory = 5856, steps = 1632628)
        )
    }

    test("tail") {
        assertEvalWithBudget(
          (pl: PairList[BigInt, BigInt]) => pl.tail,
          PairCons((BigInt(1), BigInt(2)), PairCons((BigInt(3), BigInt(4)), PairNil)),
          PairCons((BigInt(3), BigInt(4)), PairNil),
          ExUnits(memory = 3196, steps = 719380)
        )
    }

    test("isEmpty") {
        assertEvalWithBudget(
          (pl: PairList[BigInt, BigInt]) => pl.isEmpty,
          PairList.empty[BigInt, BigInt],
          true,
          ExUnits(memory = 1964, steps = 445717)
        )
        assertEvalWithBudget(
          (pl: PairList[BigInt, BigInt]) => pl.isEmpty,
          PairList.single(BigInt(1), BigInt(2)),
          false,
          ExUnits(memory = 1964, steps = 445717)
        )
    }
}
