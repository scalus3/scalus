package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.Language
import scalus.cardano.onchain.plutus.prelude.require
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Data
import scalus.uplc.transform.V3Optimizer

class UplcPipelineTest extends AnyFunSuite {

    private val releaseNoTag = Options(
      generateErrorTraces = false,
      removeTraces = true,
      optimizeUplc = true
    )

    test("CompiledPlutus.program and UplcPipeline.run produce the same term") {
        given Options = releaseNoTag
        val compiled = PlutusV3.compile { (d: Data) =>
            val x = d.to[BigInt]
            require(x > BigInt(0))
        }
        val direct = UplcPipeline.run(
          compiled.sir,
          compiled.options,
          Language.PlutusV3,
          new V3Optimizer(compiled.options.cseIterations, compiled.options.cceEnabled)
        )
        assert(compiled.program.term == direct)
    }
}
