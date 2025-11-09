package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite

class SIRPreprocessingTest extends AnyFunSuite {

    test("check that module SIR is written by compiler into the module val") {
        val listSirModule: scalus.compiler.sir.Module = scalus.prelude.List.sirModule
        assert(listSirModule != null)
        assert(listSirModule.defs.head.name.startsWith("scalus.prelude.List$."))
        val optionSirModule: scalus.compiler.sir.Module = scalus.prelude.Option.sirModule
        assert(optionSirModule != null)

        val listDeps = scalus.prelude.List.sirDeps
        // println("listDeps names: " + listDeps.map(_.sirModule.name).mkString(", "))
        assert(listDeps.nonEmpty)
        assert(listDeps.find(_.sirModule.name == "scalus.prelude.Option$") != None)
    }

}
