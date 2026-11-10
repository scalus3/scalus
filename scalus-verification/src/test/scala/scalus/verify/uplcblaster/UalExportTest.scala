package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.Math
import scalus.cardano.onchain.plutus.v3.ScriptContext
import scalus.uplc.builtin.Data.toData
import scalus.verify.{FunctionDef, FunctionTable}
import scalus.verify.Props.*
import scalus.verify.ual.UalExport

import java.nio.charset.StandardCharsets

/** Statements as documents of the Universal Annotation Language ([[UalExport]]): what is written,
  * without a consumer of the language to check it.
  */
class UalExportTest extends AnyFunSuite {

    private val clamp = FunctionDef(Math.clamp)

    private val inRange = forAll[BigInt, BigInt, BigInt]((x, lo, hi) =>
        (lo <= hi) ==>: callRef(clamp.ref, (x, lo, hi))(r => lo <= r && r <= hi)
    )

    private def stated(id: String): UplcBlaster.UalStatement = {
        val lowered = UplcBlaster.lower(inRange, FunctionTable(clamp)).fold(fail(_), identity)
        UplcBlaster.ualStatement(lowered, 120, id).fold(fail(_), identity)
    }

    private val preamble = UalExport.Preamble(
      "Clamp",
      "A statement about Math.clamp.",
      "1.0.0",
      List("Scalus"),
      "2026-11-10",
      Some("Apache-2.0")
    )

    test("a statement is a proposition over the functions of its tests, read by its polarity") {
        val statement = stated("clamp_in_range")
        // The premise is assumed: its test neither returns false nor fails. The claim returns
        // true. A function takes every variable in the scope of its test, used or not.
        assert(
          statement.formal ==
              "(∀ (x0 : Integer), (∀ (x1 : Integer), (∀ (x2 : Integer), " +
              "((¬ clamp_in_range_leaf0_returns x0 x1 x2 false ∧ " +
              "¬ isUnsuccessful (clamp_in_range_leaf0 x0 x1 x2)) → " +
              "(clamp_in_range_leaf1_returns x0 x1 x2 true)))))"
        )
        assert(statement.steps == 120)
        assert(statement.predicates.isEmpty)
        assert(
          statement.functions.map(_.id) == Vector("clamp_in_range_leaf0", "clamp_in_range_leaf1")
        )
        assert(
          statement.functions.map(_.arguments.map(_.schema)) == Vector.fill(2)(
            List.fill(3)("""{"dataType":"#integer"}""")
          )
        )
    }

    test("a variable over a type's Data has the type's predicate as a premise and a fragment") {
        val prop = forAllData[ScriptContext](ctx => ctx.toData == ctx.toData)
        val lowered = UplcBlaster.lower(prop, FunctionTable.empty).fold(fail(_), identity)
        val statement = UplcBlaster.ualStatement(lowered, 100, "same").fold(fail(_), identity)
        assert(statement.formal.startsWith("(∀ (x0 : Data), ("), statement.formal.take(80))
        assert(statement.formal.contains(" x0 = true) → (same_leaf0_returns x0 true)"))
        assert(statement.predicates.contains("def scalusIsI : Data → Bool"))
        assert(
          statement.functions.head.arguments.map(_.annotation) == List("{ BuiltinData : asData }")
        )
    }

    test("a statement checked within a budget is not exported") {
        val lowered = UplcBlaster.lower(inRange, FunctionTable(clamp)).fold(fail(_), identity)
        assert(UplcBlaster.ualStatement(lowered.copy(within = Some(120)), 120, "within").isLeft)
    }

    test(
      "the documents have the properties, the functions with their hashes, and the annotations"
    ) {
        val statement = stated("clamp_in_range")
        val property = UalExport.Property("clamp_in_range", "Clamp stays \"in range\".", statement)
        val blueprint =
            UalExport.Artifact("plutus.json", Some("{}".getBytes(StandardCharsets.UTF_8)))
        val without = UalExport.bundle(preamble, blueprint, None, List(property))
        assert(without.files.map(_._1) == List("assurance.json", "annotations.ual"))
        val assurance = without.files.toMap.apply("assurance.json")
        assert(
          assurance.contains(
            """"$schema": "https://cips.cardano.org/cips/cipXXXX/schemas/assurance-v2.json""""
          )
        )
        assert(assurance.contains(""""version": "1.0""""))
        assert(assurance.contains(""""text": "Clamp stays \"in range\".""""))
        statement.functions.foreach { function =>
            assert(assurance.contains(s""""${function.id}": {"""))
            assert(assurance.contains(s""""digest": "${function.hash}""""))
            assert(assurance.contains(UplcBlaster.programHex(function.program)))
        }
        // No context is written without the environment of a consumer, and no evidence ever.
        assert(!assurance.contains("checkingContext"))
        assert(!assurance.contains("evidence"))

        val annotations = without.files.toMap.apply("annotations.ual")
        assert(
          annotations.contains(
            "/*@ ONCHAIN [kind: function] [version: PlutusV3] [steps: 120, semantics: E]\n" +
                "    clamp_in_range_leaf0 :: { Integer : asNative } -> { Integer : asNative } -> " +
                "{ Integer : asNative } -> { Bool : asNative }\n@*/"
          ),
          annotations
        )
        assert(
          annotations.contains(
            "/*@ PROPERTY [scope: clamp_in_range_leaf0 clamp_in_range_leaf1] clamp_in_range\n" +
                "    \"Clamp stays \\\"in range\\\".\"\n  : (∀ (x0 : Integer)"
          ),
          annotations
        )

        val environment =
            UalExport.Artifact("environment.json", Some("{}".getBytes(StandardCharsets.UTF_8)))
        val bundled = UalExport.bundle(preamble, blueprint, Some(environment), List(property))
        assert(
          bundled.files.map(_._1) ==
              List("assurance.json", "checking-clamp_in_range.json", "annotations.ual")
        )
        val context = bundled.files.toMap.apply("checking-clamp_in_range.json")
        assert(context.contains(""""steps": 120"""))
        assert(context.contains(""""semanticsVariant": "E""""))
        assert(context.contains(""""function": "clamp_in_range_leaf1""""))
        assert(
          bundled.files.toMap
              .apply("assurance.json")
              .contains(""""checkingContext": "clamp_in_range"""")
        )
    }

    test("two properties of one id are refused") {
        val property = UalExport.Property("p", "A property.", stated("p"))
        val blueprint = UalExport.Artifact("plutus.json", None)
        assertThrows[IllegalArgumentException](
          UalExport.bundle(preamble, blueprint, None, List(property, property))
        )
    }
}
