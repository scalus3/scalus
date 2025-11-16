package scalus.uplc

import scalus.Compiler
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{Credential, Language, PlutusScript, Script}
import scalus.compiler.sir.lowering.SirToUplcV3Lowering
import scalus.compiler.sir.lowering.simple.{ScottEncodingLowering, SumOfProductsLowering}
import scalus.compiler.sir.{SIR, TargetLoweringBackend}
import scalus.uplc.eval.Log
import scalus.uplc.transform.{CaseConstrApply, EtaReduce, ForcedBuiltinsExtractor, Inliner}

import scala.annotation.threadUnsafe

trait Compiled[A] {
    def code: () => A
    def language: Language
    def sir: SIR
    def options: Compiler.Options
    def program: Program
    def script: PlutusScript
    def address(network: Network): Address = Address(
      network,
      Credential.ScriptHash(script.scriptHash)
    )
}

trait Optimizer {
    def apply(term: Term): Term
    def logs: Seq[String]
}

class V1V2Optimizer extends Optimizer {
    private val logger = Log()
    def apply(term: Term): Term = {
        ForcedBuiltinsExtractor(
          // dead code elimination, var and constant inlining
          Inliner(term = EtaReduce(term = term, logger = logger.log)),
          logger = logger.log
        )
    }
    def logs: Seq[String] = logger.getLogs.toVector
}

class V3Optimizer extends Optimizer {
    private val logger = Log()
    def apply(term: Term): Term = {
        logger.clear()
        CaseConstrApply(
          // CaseConstrApply will optimize further after this
          ForcedBuiltinsExtractor(
            // dead code elimination, var and constant inlining
            Inliner(term = EtaReduce(term = term, logger = logger.log)),
            logger = logger.log
          )
        )
    }
    def logs: Seq[String] = logger.getLogs.toVector
}

case class PlutusV3[A](code: () => A, sir: SIR, options: Compiler.Options, optimizer: Optimizer)
    extends Compiled[A] {
    def language: Language = Language.PlutusV3
    @threadUnsafe
    lazy val program: Program = Program.plutusV3(toUplc)
    @threadUnsafe lazy val script: Script.PlutusV3 = Script.PlutusV3(program.cborByteString)
    protected def toUplc: Term = {
        val backend = options.targetLoweringBackend
        val uplc = backend match
            case TargetLoweringBackend.ScottEncodingLowering |
                TargetLoweringBackend.SimpleSirToUplcLowering =>
                ScottEncodingLowering(sir, options.generateErrorTraces).lower()
            case TargetLoweringBackend.SumOfProductsLowering |
                TargetLoweringBackend.SirToUplc110Lowering =>
                SumOfProductsLowering(sir, options.generateErrorTraces).lower()
            case TargetLoweringBackend.SirToUplcV3Lowering =>
                SirToUplcV3Lowering(
                  sir,
                  generateErrorTraces = options.generateErrorTraces,
                  debug = options.debug
                ).lower()
        val retval =
            if options.optimizeUplc then optimizer(uplc)
            else uplc
        retval
    }
}

object PlutusV3 {
    inline def compile[A](inline code: A)(using opts: Compiler.Options): PlutusV3[A] = {
        val sir = Compiler.compileInlineWithOptions(opts, code)
        PlutusV3(() => code, sir, opts, new V3Optimizer())
    }
}
