package scalus.cardano.blueprint

import org.scalatest.funsuite.AnyFunSuite
import scalus.Compiler
import scalus.builtin.Data
import scalus.cardano.blueprint.HtlcValidatorInputs.{Action, ContractDatum}
import scalus.cardano.ledger.Language
import scalus.uplc.PlutusV3
import scalus.utils.BuildInfo
class BlueprintTest extends AnyFunSuite {

    private given Compiler.Options = Compiler.Options.release

    test("should be deserializable from JSON") {
        val value = Application
            .ofSingleValidator[ContractDatum, Action](
              "Htlc Validator",
              "",
              "1.0.0",
              (ctx: Data) => ()
            )
            .blueprint
        val stringValue =
            s"""
               |{
               |  "preamble": {
               |    "title": "Htlc Validator",
               |    "description": "",
               |    "compiler": {
               |      "name": "scalus",
               |      "version": "${BuildInfo.version}"
               |    },
               |    "plutusVersion": "v3"
               |  },
               |  "validators": [
               |    {
               |      "title": "Htlc Validator",
               |      "redeemer": {
               |        "schema": {
               |          "title": "Action",
               |          "anyOf": [
               |            {
               |              "dataType": "constructor",
               |              "title": "Timeout",
               |              "index": 0,
               |              "fields": [
               |                
               |              ]
               |            },
               |            {
               |              "dataType": "constructor",
               |              "title": "Reveal",
               |              "index": 1,
               |              "fields": [
               |                {
               |                  "dataType": "bytes",
               |                  "title": "preimage"
               |                }
               |              ]
               |            }
               |          ]
               |        }
               |      },
               |      "datum": {
               |        "schema": {
               |          "dataType": "constructor",
               |          "title": "ContractDatum",
               |          "fields": [
               |            {
               |              "dataType": "bytes",
               |              "title": "committer"
               |            },
               |            {
               |              "dataType": "bytes",
               |              "title": "receiver"
               |            },
               |            {
               |              "dataType": "bytes",
               |              "title": "image"
               |            },
               |            {
               |              "dataType": "integer",
               |              "title": "timeout"
               |            }
               |          ]
               |        }
               |      },
               |      "compiledCode": "450101002499",
               |      "hash": "186e32faa80a26810392fda6d559c7ed4721a65ce1c9d4ef3e1c87b4"
               |    }
               |  ]
               |}
               |""".stripMargin
        assert(Blueprint.fromJson(stringValue) == value)
    }

    // This case is covered by the following tests, keeping this test to check compatibility with aiken.
    // https://github.com/aiken-lang/aiken/blob/main/crates/aiken-project/src/blueprint/snapshots/aiken_project__blueprint__validator__tests__generics.snap#L57
    test("should produce correct schemas for `enum` types") {
        val intervalSchema = PlutusDataSchema.derived[Interval]

        assert(intervalSchema.toJson() == Interval.schema)
    }

    test("should produce correct schemas for `HtlcValidator` input types") {
        val datumSchema = PlutusDataSchema.derived[ContractDatum]
        val redeemerSchema = PlutusDataSchema.derived[Action]

        assert(datumSchema.toJson() == ContractDatum.schema)
    }

    test("should produce correct schemas for tuples") {
        val tuple2Schema = PlutusDataSchema.derived[(Int, String)]
        assert(tuple2Schema.dataType.contains(DataType.PairBuiltin))
        assert(tuple2Schema.title.contains("Tuple2"))
        assert(tuple2Schema.items.isDefined)
        assert(tuple2Schema.items.get.length == 2)
        val items = tuple2Schema.items.get
        assert(items(0).dataType.contains(DataType.Integer))
        assert(items(1).dataType.contains(DataType.StringBuiltin))
    }

    test("should produce correct schemas for nested tuples") {
        val nestedTupleSchema = PlutusDataSchema.derived[(Int, (String, Boolean))]

        assert(nestedTupleSchema.dataType.contains(DataType.PairBuiltin))
        assert(nestedTupleSchema.items.isDefined)
        assert(nestedTupleSchema.items.get.length == 2)

        val secondItem = nestedTupleSchema.items.get(1)
        assert(secondItem.dataType.contains(DataType.PairBuiltin))
        assert(secondItem.items.isDefined)
        assert(secondItem.items.get.length == 2)
    }

    test("should produce correct schemas for case classes with tuple fields") {
        case class TestCaseClass(
            name: String,
            coordinates: (Int, Int)
        )

        val schema = PlutusDataSchema.derived[TestCaseClass]

        assert(schema.dataType.contains(DataType.Constructor))
        assert(schema.title.contains("TestCaseClass"))
        assert(schema.fields.isDefined)
        assert(schema.fields.get.length == 2)

        val coordinatesField = schema.fields.get(1)
        assert(coordinatesField.title.contains("coordinates"))
        assert(coordinatesField.dataType.contains(DataType.PairBuiltin))
        assert(coordinatesField.items.isDefined)
        assert(coordinatesField.items.get.length == 2)

        val tupleItems = coordinatesField.items.get
        assert(tupleItems(0).dataType.contains(DataType.Integer))
        assert(tupleItems(1).dataType.contains(DataType.Integer))
    }

    test(
      "Blueprint.plutusV3[Datum, Redeemer] should create blueprint with datum and redeemer schemas"
    ) {
        val compiled = PlutusV3.compile((ctx: Data) => ())
        val bp = Blueprint.plutusV3[ContractDatum, Action](
          "Test Validator",
          "A test validator",
          version = "1.0.0",
          license = Some("MIT"),
          compiled
        )

        assert(bp.preamble.title == "Test Validator")
        assert(bp.preamble.description.contains("A test validator"))
        assert(bp.preamble.version.contains("1.0.0"))
        assert(bp.preamble.license.contains("MIT"))
        assert(bp.preamble.plutusVersion.contains(Language.PlutusV3))
        assert(bp.validators.length == 1)

        val validator = bp.validators.head
        assert(validator.title == "Test Validator")
        assert(validator.description.contains("A test validator"))
        assert(validator.datum.isDefined)
        assert(validator.redeemer.isDefined)
        assert(validator.parameters.isEmpty)
        assert(validator.compiledCode.isDefined)
        assert(validator.hash.isDefined)

        // Verify datum schema matches ContractDatum
        val datumSchema = validator.datum.get.schema
        assert(datumSchema.title.contains("ContractDatum"))
        assert(datumSchema.dataType.contains(DataType.Constructor))

        // Verify redeemer schema matches Action
        val redeemerSchema = validator.redeemer.get.schema
        assert(redeemerSchema.title.contains("Action"))
    }

    test(
      "Blueprint.plutusV3[Param, Datum, Redeemer] should create blueprint with parameter, datum, and redeemer schemas"
    ) {
        val compiled = PlutusV3.compile((param: BigInt) => (ctx: Data) => ())
        val bp = Blueprint.plutusV3[BigInt, ContractDatum, Action](
          "Parameterized Validator",
          "A parameterized validator",
          version = "2.0.0",
          license = Some("Apache-2.0"),
          compiled
        )

        assert(bp.preamble.title == "Parameterized Validator")
        assert(bp.preamble.description.contains("A parameterized validator"))
        assert(bp.preamble.version.contains("2.0.0"))
        assert(bp.preamble.license.contains("Apache-2.0"))
        assert(bp.preamble.plutusVersion.contains(Language.PlutusV3))
        assert(bp.validators.length == 1)

        val validator = bp.validators.head
        assert(validator.title == "Parameterized Validator")
        assert(validator.datum.isDefined)
        assert(validator.redeemer.isDefined)
        assert(validator.parameters.isDefined)
        assert(validator.parameters.get.length == 1)

        // Verify parameter schema
        val paramSchema = validator.parameters.get.head.schema
        assert(paramSchema.dataType.contains(DataType.Integer))
    }

    test(
      "Blueprint.plutusV3[Redeemer] should create blueprint with only redeemer schema (no datum)"
    ) {
        val compiled = PlutusV3.compile((ctx: Data) => ())
        val bp = Blueprint.plutusV3[Action](
          "Minting Policy",
          "A minting policy without datum",
          version = "1.0.0",
          license = None,
          compiled
        )

        assert(bp.preamble.title == "Minting Policy")
        assert(bp.preamble.description.contains("A minting policy without datum"))
        assert(bp.preamble.version.contains("1.0.0"))
        assert(bp.preamble.license.isEmpty)
        assert(bp.preamble.plutusVersion.contains(Language.PlutusV3))
        assert(bp.validators.length == 1)

        val validator = bp.validators.head
        assert(validator.title == "Minting Policy")
        assert(validator.datum.isEmpty)
        assert(validator.redeemer.isDefined)
        assert(validator.parameters.isEmpty)
        assert(validator.compiledCode.isDefined)
        assert(validator.hash.isDefined)

        // Verify redeemer schema matches Action
        val redeemerSchema = validator.redeemer.get.schema
        assert(redeemerSchema.title.contains("Action"))
    }

    test(
      "Blueprint.plutusV3[Param, Redeemer] should create blueprint with parameter and redeemer but no datum"
    ) {
        val compiled = PlutusV3.compile((param: BigInt) => (ctx: Data) => ())
        val bp = Blueprint.plutusV3[BigInt, Action](
          "Parameterized Minting Policy",
          "A parameterized minting policy without datum",
          version = "3.0.0",
          license = Some("GPL-3.0"),
          compiled
        )

        assert(bp.preamble.title == "Parameterized Minting Policy")
        assert(bp.preamble.description.contains("A parameterized minting policy without datum"))
        assert(bp.preamble.version.contains("3.0.0"))
        assert(bp.preamble.license.contains("GPL-3.0"))
        assert(bp.preamble.plutusVersion.contains(Language.PlutusV3))
        assert(bp.validators.length == 1)

        val validator = bp.validators.head
        assert(validator.title == "Parameterized Minting Policy")
        assert(validator.datum.isEmpty)
        assert(validator.redeemer.isDefined)
        assert(validator.parameters.isDefined)
        assert(validator.parameters.get.length == 1)
        assert(validator.compiledCode.isDefined)
        assert(validator.hash.isDefined)

        // Verify parameter schema
        val paramSchema = validator.parameters.get.head.schema
        assert(paramSchema.dataType.contains(DataType.Integer))

        // Verify redeemer schema matches Action
        val redeemerSchema = validator.redeemer.get.schema
        assert(redeemerSchema.title.contains("Action"))
    }

    test("Blueprint.plutusV3 methods should produce valid JSON") {
        val compiled = PlutusV3.compile((ctx: Data) => ())
        val bp = Blueprint.plutusV3[ContractDatum, Action](
          "JSON Test",
          "Testing JSON serialization",
          version = "1.2.3",
          license = Some("BSD-3-Clause"),
          compiled
        )

        val json = bp.toJson()
        // Verify it can be parsed back
        val parsed = Blueprint.fromJson(json)
        assert(parsed.preamble.title == bp.preamble.title)
        assert(parsed.preamble.version.contains("1.2.3"))
        assert(parsed.preamble.license.contains("BSD-3-Clause"))
        assert(parsed.validators.length == bp.validators.length)
        assert(parsed.validators.head.datum.isDefined)
        assert(parsed.validators.head.redeemer.isDefined)
    }

}
