package scalus.cardano.blueprint

import scala.annotation.nowarn
import scalus.compiler.Options

@deprecated("Use PlutusV3.compile and Blueprint.plutusV3 instead", since = "0.14.0")
sealed trait Contract {
    @nowarn("cat=deprecation")
    def defaultCompiledContract: CompiledContract
    @nowarn("cat=deprecation")
    def debugCompiledContract: CompiledContract
    @nowarn("cat=deprecation")
    def releaseCompiledContract: CompiledContract
}

@nowarn("cat=deprecation")
object Contract {
    @deprecated("Use PlutusV3.compile and Blueprint.plutusV3 instead", since = "0.14.0")
    case class PlutusV3Contract private (
        override val defaultCompiledContract: PlutusV3CompiledContract,
        override val debugCompiledContract: PlutusV3CompiledContract,
        override val releaseCompiledContract: PlutusV3CompiledContract
    ) extends Contract

    object PlutusV3Contract {
        inline def apply[D, R](preamble: Preamble, inline code: Any): PlutusV3Contract = {
            val defaultCompiledContract =
                PlutusV3CompiledContract.create[D, R](preamble, Options.default)(code)
            val debugCompiledContract =
                PlutusV3CompiledContract.create[D, R](preamble, Options.debug)(code)
            val releaseCompiledContract =
                PlutusV3CompiledContract.create[D, R](preamble, Options.release)(code)

            PlutusV3Contract(
              defaultCompiledContract = defaultCompiledContract,
              debugCompiledContract = debugCompiledContract,
              releaseCompiledContract = releaseCompiledContract
            )
        }
    }
}
