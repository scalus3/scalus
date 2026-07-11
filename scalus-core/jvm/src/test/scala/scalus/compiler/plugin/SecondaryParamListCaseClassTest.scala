package scalus.compiler.plugin

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.{Module, SIRType}

import java.net.URLClassLoader

/** Audit finding H2 (compiler plugin audit): `SIRTyper.retrieveTypeParamsAndParamsFromConstructor`
  * returned swapped results for a primary constructor with an empty first parameter list, so for
  * `case class Wrap()(val y: BigInt)` the field `y` became a bogus SIR *type* parameter and the
  * constructor ended up with no fields at all.
  *
  * The test compiles a snippet with the packaged plugin, loads the generated `sirModule` from the
  * compiled classes, and asserts on the constructor declaration.
  */
class SecondaryParamListCaseClassTest extends AnyFunSuite with SnippetCompilation {

    private def constrDeclOf(source: String, memberName: String) = {
        val (errors, outDir) = compileSnippetWithOutput(source)
        assert(errors.isEmpty, s"expected clean compile, got: $errors")
        val loader = new URLClassLoader(Array(outDir.toUri.toURL), getClass.getClassLoader)
        val moduleClass = Class.forName("H2Repro$", true, loader)
        val instance = moduleClass.getField("MODULE$").get(null)
        val sirModule = moduleClass.getMethod("sirModule").invoke(instance).asInstanceOf[Module]
        val binding = sirModule.defs
            .find(_.name.contains(memberName))
            .getOrElse(
              fail(s"binding '$memberName' not found in ${sirModule.defs.map(_.name)}")
            )
        // a nullary def is compiled as `Unit -> T`; unwrap to the result type
        def resultType(tp: SIRType): SIRType = tp match
            case SIRType.Fun(_, res) => resultType(res)
            case other               => other
        SIRType.collectProdCaseClass(resultType(binding.tp)) match
            case Some((_, caseClass)) => caseClass.constrDecl
            case None => fail(s"binding type is not a product case class: ${binding.tp.show}")
    }

    test("control: single param list case class has its field as a constructor param") {
        val constrDecl = constrDeclOf(
          """import scalus.compiler.Compile
            |@Compile
            |object H2Repro {
            |    case class Wrap(y: BigInt)
            |    def mk: Wrap = Wrap(BigInt(1))
            |}
            |""".stripMargin,
          "mk"
        )
        assert(constrDecl.typeParams.isEmpty, s"unexpected type params: ${constrDecl.typeParams}")
        assert(constrDecl.params.map(_.name) == List("y"), s"params: ${constrDecl.params}")
    }

    test("empty first param list: field lands in params, not in type params") {
        val constrDecl = constrDeclOf(
          """import scalus.compiler.Compile
            |@Compile
            |object H2Repro {
            |    case class Wrap()(val y: BigInt)
            |    def mk: Wrap = Wrap()(BigInt(1))
            |}
            |""".stripMargin,
          "mk"
        )
        assert(
          constrDecl.typeParams.isEmpty,
          s"field 'y' must not become a type param, got type params: ${constrDecl.typeParams}"
        )
        assert(
          constrDecl.params.map(_.name) == List("y"),
          s"expected params [y], got: ${constrDecl.params}"
        )
    }
}
