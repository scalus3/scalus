package scalus.cardano.blueprint

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import HtlcValidatorInputs.{Action, ContractDatum}
import scalus.uplc.Program

class ScriptDefinitionTest extends AnyFunSuite {

    test("should compile the user program the same as they would compile it themselves") {
        val scriptDefinition = PlutusV3CompiledContract.create[ContractDatum, Action](
          "empty validator"
        )(code = EmptyValidator.validate)
        val app = Application(
          "Test title",
          "Test description",
          "1.0.0",
          Seq(scriptDefinition)
        )

        val program: Program =
            scalus.compiler.compile(EmptyValidator.validate).toUplcOptimized().plutusV3
        assert(program.cborEncoded sameElements app.contracts.head.program.cborEncoded)

    }
}
