package scalus.examples

import scalus.*
import scalus.builtin.Data
import scalus.cardano.ledger.{Language, PlutusScript, Script, ScriptHash}
import scalus.ledger.api.v3.ScriptContext
import scalus.compiler.sir.SIR
import scalus.uplc.Program

case class Config(
    generateErrorTraces: Boolean = true,
    optimizeUplc: Boolean = false,
    debug: Boolean = false
)
case class PV3Wrapper(sir: SIR, config: Config, title: String = "Plutus V3 Wrapper") {
    def toProgram: Program = {
        sir.toUplc(
          generateErrorTraces = config.generateErrorTraces,
          backend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
          optimizeUplc = config.optimizeUplc,
          debug = config.debug
        ).plutusV3
    }
    def blueprint: String = ???

    def script: PlutusScript = Script.PlutusV3(toProgram.cborByteString)
    def scriptHash: ScriptHash = script.scriptHash
}

trait Project {
    def blueprint: String
}
object BP {
    def projectOf(w: PV3Wrapper*): Project = ???
}

object PlutusV3 {
    inline def fromCode(inline code: Any): PV3Wrapper = ???
}

trait CC[A]
trait PV3CC[A] extends CC[A] {
    val language: Language = Language.PlutusV3
    def program: Program = ???
    def script: PlutusScript = ???
    def scriptHash: ScriptHash = ???
    def toUplc() = ???
    def code: A = ???
}

object Plutus {
    object V3 {
        inline def apply[A](inline code: A)(using Config): PV3CC[A] = ???
        inline def validator(inline code: Data => Unit)(using Config): PV3CC[Data => Unit] = ???
        inline def typedValidator(inline code: ScriptContext => Unit)(using
            Config
        ): PV3CC[ScriptContext => Unit] = ???
    }
}

trait WhereWeMakeADecision {

//    def v1 = ???
//    def v2 = ???
//    def v3 = PV3Wrapper()
}
