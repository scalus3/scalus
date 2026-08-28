package scalus.tsexport

import org.scalatest.funsuite.AnyFunSuite

import scala.quoted.*
import scala.tasty.inspector.*

class SmokeTest extends AnyFunSuite {
    test("inspector loads fixture TASTy and sees @JSExportTopLevel") {
        var foundExportNames = List.empty[String]
        val inspector = new Inspector {
            def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit = {
                import quotes.reflect.*
                for tasty <- tastys do {
                    object walker extends TreeAccumulator[Unit] {
                        def foldTree(u: Unit, tree: Tree)(owner: Symbol): Unit = tree match
                            case c: ClassDef =>
                                val names = c.symbol.annotations.collect {
                                    case a
                                        if a.tpe.typeSymbol.fullName ==
                                            "scala.scalajs.js.annotation.JSExportTopLevel" =>
                                        a match
                                            case Apply(_, List(Literal(StringConstant(n)))) => n
                                            case _ => c.symbol.name
                                }
                                foundExportNames = foundExportNames ++ names
                                foldOverTree(u, tree)(owner)
                            case _ => foldOverTree(u, tree)(owner)
                    }
                    walker.foldTree((), tasty.ast)(tasty.ast.symbol)
                }
            }
        }
        val ok = TastyInspector.inspectAllTastyFiles(
          InspectorFixture.tastyFilesUnder(InspectorFixture.fixtureClasses),
          Nil,
          InspectorFixture.fixtureClasspath
        )(inspector)
        assert(ok, "inspector reported failure")
        assert(foundExportNames.contains("Point"))
    }
}
