package scalus.tsexport

import org.scalatest.funsuite.AnyFunSuite

import scala.quoted.*
import scala.tasty.inspector.*

class TypeMapperTest extends AnyFunSuite {

    /** "ClassName.methodName" -> rendered param types :+ rendered result type. Overloads get a "#2"
      * suffix. Mapping errors render as "ERROR(<message>)".
      */
    private lazy val signatures: Map[String, List[String]] = {
        var out = Map.empty[String, List[String]]
        val inspector = new Inspector {
            def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit = {
                import quotes.reflect.*
                val mapper = new TypeMapper(
                  Map(
                    "tsfixtures.Point" -> "Point",
                    "tsfixtures.Conf" -> "Config",
                    "tsfixtures.Inner" -> "Inner"
                  ),
                  sym => Some(sym.name)
                )
                val classes = List(
                  "tsfixtures.Kitchen",
                  "tsfixtures.Intersections",
                  "tsfixtures.BadLong",
                  "tsfixtures.BadOption",
                  "tsfixtures.BadColl",
                  "tsfixtures.BadOpaque"
                )
                for cls <- classes do {
                    val sym = Symbol.requiredClass(cls)
                    for m <- sym.declaredMethods do
                        m.tree match
                            case dd: DefDef =>
                                val types =
                                    dd.termParamss.flatMap(_.params).map(_.tpt.tpe) :+
                                        dd.returnTpt.tpe
                                val rendered = types.map { t =>
                                    mapper.map(t, s"$cls.${m.name}") match
                                        case Right(ts) => Emitter.render(ts)
                                        case Left(e)   => s"ERROR(${e.message})"
                                }
                                val key = s"${sym.name}.${m.name}"
                                out = out.updated(
                                  if out.contains(key) then s"$key#2" else key,
                                  rendered
                                )
                            case _ => ()
                }
            }
        }
        val ok = TastyInspector.inspectAllTastyFiles(
          InspectorFixture.tastyFilesUnder(InspectorFixture.fixtureClasses),
          Nil,
          InspectorFixture.fixtureClasspath
        )(inspector)
        assert(ok, "inspection failed")
        out
    }

    test("maps primitives, strings, void") {
        assert(signatures("Kitchen.prims") == List("boolean", "number", "number", "string", "void"))
    }

    test("maps js.BigInt, arrays, UndefOr, unions") {
        assert(signatures("Kitchen.big") == List("bigint", "bigint"))
        assert(signatures("Kitchen.arr") == List("string[]", "number[][]"))
        assert(signatures("Kitchen.undef") == List("string | undefined", "number | undefined"))
        assert(signatures("Kitchen.union") == List("bigint | null", "Uint8Array | null"))
    }

    test("maps dictionary, promise, functions, dynamic, typed arrays, object") {
        assert(
          signatures("Kitchen.dict") ==
              List("{ [key: string]: string }", "{ [key: string]: number[] }")
        )
        assert(signatures("Kitchen.promise") == List("Promise<string>"))
        assert(signatures("Kitchen.fun") == List("(arg0: number) => string", "() => void"))
        assert(signatures("Kitchen.dyn") == List("any", "any"))
        assert(signatures("Kitchen.bytes") == List("Uint8Array", "Uint8Array"))
        assert(signatures("Kitchen.obj") == List("object", "object"))
    }

    test("maps getters, defaults, overloads, known and chased references") {
        assert(signatures("Kitchen.getter") == List("number"))
        assert(signatures("Kitchen.dflt") == List("number", "string", "number"))
        assert(signatures("Kitchen.overloaded") == List("number", "number"))
        assert(signatures("Kitchen.overloaded#2") == List("number", "string", "string"))
        assert(signatures("Kitchen.config") == List("Config", "Config"))
        // the mapper itself ignores @TsType; the collector applies it
        assert(signatures("Kitchen.credType") == List("string"))
    }

    test("maps intersection types") {
        assert(signatures("Intersections.both") == List("Config & Inner", "Config & Inner"))
        assert(signatures("Intersections.withObject") == List("object & Inner", "object & Inner"))
    }

    test("errors on non-exportable types with helpful messages") {
        assert(signatures("BadLong.bad").forall(_.startsWith("ERROR(")))
        assert(signatures("BadLong.bad").head.contains("Long"))
        assert(signatures("BadOption.bad").head.contains("js.UndefOr"))
        assert(signatures("BadColl.bad").last.contains("js.Array"))
        assert(signatures("BadOpaque.bad").last.contains("Instant"))
    }
}
