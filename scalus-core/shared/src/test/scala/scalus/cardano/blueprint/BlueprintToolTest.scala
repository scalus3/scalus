package scalus.cardano.blueprint

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.Language

import scala.jdk.CollectionConverters.*

class BlueprintToolTest extends AnyFunSuite {

    private def bp(title: String, version: String, compiledCode: String): Blueprint =
        Blueprint(
          Preamble(
            title = title,
            version = Some(version),
            compiler = Some(CompilerInfo("scalus", Some("0.19.0"))),
            plutusVersion = Some(Language.PlutusV3)
          ),
          Seq(Validator(title = title, compiledCode = Some(compiledCode)))
        )

    test("stampScalaVersion adds the top-level scalus key and preserves everything else") {
        val original = bp("A", "1.0.0", "aa")
        val stamped =
            Blueprint.fromJson(BlueprintTool.stampScalaVersion(original.toJson(), "3.3.7"))
        assert(stamped == original.copy(scalus = Some(ScalusInfo(Some("3.3.7")))))
    }

    test("stampScalaVersion overwrites a previous stamp") {
        val original = bp("A", "1.0.0", "aa").copy(scalus = Some(ScalusInfo(Some("3.3.6"))))
        val stamped =
            Blueprint.fromJson(BlueprintTool.stampScalaVersion(original.toJson(), "3.3.7"))
        assert(stamped.scalus == Some(ScalusInfo(Some("3.3.7"))))
    }

    test("aggregate merges validators in input order under a project-level preamble") {
        val a = bp("A", "1.0.0", "aa")
        val b = bp("B", "2.0.0", "bb")
        val json = BlueprintTool.aggregate(
          List(a.toJson(), b.toJson()).asJava,
          "my-project",
          "0.1.0",
          "3.3.7"
        )
        val agg = Blueprint.fromJson(json)
        assert(agg.preamble.title == "my-project")
        assert(agg.preamble.version.contains("0.1.0"))
        assert(agg.preamble.compiler.contains(CompilerInfo("scalus", Some("0.19.0"))))
        assert(agg.preamble.plutusVersion.contains(Language.PlutusV3))
        assert(agg.validators.map(_.title) == Seq("A", "B"))
        assert(agg.validators.flatMap(_.compiledCode) == Seq("aa", "bb"))
        assert(agg.scalus == Some(ScalusInfo(Some("3.3.7"))))
    }

    test("aggregate leaves plutusVersion unset when inputs mix Plutus versions") {
        val a = bp("A", "1.0.0", "aa")
        val v2 = bp("B", "2.0.0", "bb")
        val b = v2.copy(preamble = v2.preamble.copy(plutusVersion = Some(Language.PlutusV2)))
        val json = BlueprintTool.aggregate(
          List(a.toJson(), b.toJson()).asJava,
          "my-project",
          "0.1.0",
          "3.3.7"
        )
        assert(Blueprint.fromJson(json).preamble.plutusVersion.isEmpty)
    }

    test("aggregate of an empty list yields an empty validators array") {
        val json =
            BlueprintTool.aggregate(List.empty[String].asJava, "my-project", "0.1.0", "3.3.7")
        val agg = Blueprint.fromJson(json)
        assert(agg.validators.isEmpty)
        assert(agg.preamble.title == "my-project")
    }
}
