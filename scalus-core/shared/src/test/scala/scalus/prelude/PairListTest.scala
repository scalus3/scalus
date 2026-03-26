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
            compilerOptions.copy(nativeListElements = false) -> ExUnits(memory = 5856, steps = 1632628),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(memory = 5856, steps = 1_632_628)
          )
        )
    }

    test("tail") {
        assertEvalWithBudgets(
          (pl: PairList[BigInt, BigInt]) => pl.tail,
          PairCons((BigInt(1), BigInt(2)), PairCons((BigInt(3), BigInt(4)), PairNil)),
          PairCons((BigInt(3), BigInt(4)), PairNil),
          Seq(
            compilerOptions.copy(nativeListElements = false) -> ExUnits(memory = 3196, steps = 719380),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(memory = 3196, steps = 719_380)
          )
        )
    }

    test("isEmpty") {
        assertEvalWithBudgets(
          (pl: PairList[BigInt, BigInt]) => pl.isEmpty,
          PairList.empty[BigInt, BigInt],
          true,
          Seq(
            compilerOptions.copy(nativeListElements = false) -> ExUnits(memory = 1964, steps = 445717),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(memory = 1964, steps = 445_717)
          )
        )
        assertEvalWithBudgets(
          (pl: PairList[BigInt, BigInt]) => pl.isEmpty,
          PairList.single(BigInt(1), BigInt(2)),
          false,
          Seq(
            compilerOptions.copy(nativeListElements = false) -> ExUnits(memory = 1964, steps = 445717),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(memory = 1964, steps = 445_717)
          )
        )
    }
}
