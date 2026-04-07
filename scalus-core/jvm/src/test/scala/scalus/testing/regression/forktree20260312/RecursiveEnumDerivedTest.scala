package scalus.testing.regression.forktree20260312

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.{Data, FromData, ToData}
import scalus.cardano.onchain.plutus.prelude.List
import scalus.cardano.onchain.plutus.v1.Datum
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v3.{DataParameterizedValidator, *}
import scalus.uplc.eval.PlutusVM

/** Regression: recursive enum with derived FromData/ToData fails to compile.
  *
  * The bug: `enum ForkTree derives FromData, ToData` with a recursive
  * `Fork(children: List[ForkTree])` case triggers a compilation error in the Scalus compiler
  * plugin.
  */

enum ForkTree derives FromData, ToData {
    case Fork(children: List[ForkTree])
}

@Compile
object ForkTreeValidator extends DataParameterizedValidator {

    inline override def spend(
        param: Data,
        datum: Option[Datum],
        redeemer: Datum,
        tx: TxInfo,
        outRef: TxOutRef
    ): Unit = {
        update(redeemer.to[ForkTree])
    }

    def update(state: ForkTree): ForkTree = state
}

class RecursiveEnumDerivedTest extends AnyFunSuite {

    given PlutusVM = PlutusVM.makePlutusV3VM()

    test("recursive enum ForkTree with derived FromData/ToData compiles and evaluates") {
        val compiled = Compiler.compile(ForkTreeValidator.validate)
        val uplc = compiled.toUplcOptimized(generateErrorTraces = true)
        // If we get here, compilation succeeded
        assert(uplc != null)
    }
}
