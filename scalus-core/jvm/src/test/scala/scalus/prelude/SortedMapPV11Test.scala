package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.cardano.onchain.plutus.prelude.{List, Ord, SortedMap}
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.testing.kit.EvalTestKit
import scalus.uplc.eval.PlutusVM

class SortedMapPV11Test extends AnyFunSuite with EvalTestKit {
    override protected def compilerOptions: Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      targetProtocolVersion = MajorProtocolVersion.vanRossemPV,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    override protected def plutusVM: PlutusVM =
        PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)

    test("delete at PV11") {
        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).delete(BigInt(1)).toList,
          List.empty[(BigInt, BigInt)]
        )
    }
}
