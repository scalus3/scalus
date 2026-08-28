package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.ExUnits
import scalus.cardano.onchain.plutus.prelude.PairList
import scalus.cardano.onchain.plutus.prelude.PairList.*
import scalus.testing.kit.EvalTestKit

class PairListTest extends AnyFunSuite with EvalTestKit {

    test("head") {
        assertEvalWithBudgets(
          (pl: PairList[BigInt, BigInt]) => pl.head,
          PairList.singleton(BigInt(1), BigInt(2)),
          (BigInt(1), BigInt(2)),
          Seq(
            compilerOptions -> ExUnits(memory = 3196, steps = 649447)
          )
        )
    }

    test("tail") {
        assertEvalWithBudgets(
          (pl: PairList[BigInt, BigInt]) => pl.tail,
          PairCons((BigInt(1), BigInt(2)), PairCons((BigInt(3), BigInt(4)), PairNil)),
          PairCons((BigInt(3), BigInt(4)), PairNil),
          Seq(
            compilerOptions -> ExUnits(memory = 1132, steps = 184723)
          )
        )
    }

    test("isEmpty") {
        assertEvalWithBudgets(
          (pl: PairList[BigInt, BigInt]) => pl.isEmpty,
          PairList.empty[BigInt, BigInt],
          true,
          Seq(
            compilerOptions -> ExUnits(memory = 932, steps = 152723)
          )
        )
        assertEvalWithBudgets(
          (pl: PairList[BigInt, BigInt]) => pl.isEmpty,
          PairList.singleton(BigInt(1), BigInt(2)),
          false,
          Seq(
            compilerOptions -> ExUnits(memory = 1132, steps = 184723)
          )
        )
    }
}
