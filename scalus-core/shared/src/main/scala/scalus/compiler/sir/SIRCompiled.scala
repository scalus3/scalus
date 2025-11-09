package scalus.compiler.sir

import scala.annotation.StaticAnnotation

trait SIRCompiled {

    /** The SIR module */
    val sirModule: Module

    /** The dependencies of the SIR module */
    lazy val sirDeps: List[SIRCompiled]
}

class SIRModuleWithDeps(
    override val sirModule: Module,
    sirDepsExpr: => List[SIRCompiled]
) extends SIRCompiled {

    /** The dependencies of the SIR module */
    lazy val sirDeps: List[SIRCompiled] = sirDepsExpr

    override def toString: String = {
        s"SIRModuleWithDeps(sirModule=$sirModule, sirDeps=${sirDeps.map(_.sirModule.name)})"
    }

}

object SIRModuleWithDeps {

    def apply(sirModule: Module, sirDepsExpr: => List[SIRCompiled]): SIRModuleWithDeps = {
        new SIRModuleWithDeps(sirModule, sirDepsExpr)
    }

    def list(deps: SIRModuleWithDeps*): List[SIRModuleWithDeps] = {
        deps.toList
    }

}

case class SIRBodyAnnotation(module: Module, deps: List[SIRModuleWithDeps]) extends StaticAnnotation
