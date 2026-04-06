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
          PairList.single(BigInt(1), BigInt(2)),
          (BigInt(1), BigInt(2)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5324, steps = 1_469478)
          )
        )
    }

    test("tail") {
        assertEvalWithBudgets(
          (pl: PairList[BigInt, BigInt]) => pl.tail,
          PairCons((BigInt(1), BigInt(2)), PairCons((BigInt(3), BigInt(4)), PairNil)),
          PairCons((BigInt(3), BigInt(4)), PairNil),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 2296, steps = 575380)
          )
        )
    }

    test("isEmpty") {
        assertEvalWithBudgets(
          (pl: PairList[BigInt, BigInt]) => pl.isEmpty,
          PairList.empty[BigInt, BigInt],
          true,
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 1664, steps = 397717)
          )
        )
        assertEvalWithBudgets(
          (pl: PairList[BigInt, BigInt]) => pl.isEmpty,
          PairList.single(BigInt(1), BigInt(2)),
          false,
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 1664, steps = 397717)
          )
        )
    }
}
