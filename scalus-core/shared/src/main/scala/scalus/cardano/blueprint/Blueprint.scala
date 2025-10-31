package scalus.cardano.blueprint

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scalus.cardano.ledger.{Language, PlutusScript, Script}
import scalus.utils.BuildInfo
import scalus.utils.Hex.toHex

import java.io.File
import java.nio.file.Files

/** A CIP-57 compliant description of a set of validators.
  *
  * Each validator description contains schemas [[PlutusDataSchema]] of the datum and redeemer
  * formats expected by the contracts.
  *
  * @see
  *   https://cips.cardano.org/cip/CIP-57
  */
case class Blueprint(
    preamble: Preamble,
    validators: Seq[Validator] = Nil,
) {

    /** @return
      *   a JSON string representing this blueprint. The returned string is compliant with a
      *   respective CIP-57 JSON Schema and can be used for deserialization, such that
      *   `Blueprint(myBlueprint.show) == myBlueprint` always holds.
      */
    def show: String = toJson()

    def toJson(indentation: Int = 2): String =
        writeToString(this, WriterConfig.withIndentionStep(indentation))

    def addValidator(v: Validator): Blueprint = copy(validators = validators.appended(v))

    def writeToFile(f: File): Unit = Files.writeString(f.toPath, show)
}

object Blueprint {

    given JsonValueCodec[Blueprint] = JsonCodecMaker.make

    /** Returns a CIP-57 compliant [[Blueprint]] based on the provided [[validator]].
      *
      * The returned `Blueprint` always contains only 1 validator.
      *
      * To specify the `redeemer` and `datum` schemas, use [[Blueprint.newBuilder()]].
      *
      * @param contractTitle
      *   the title of the "blueprintee" contract
      * @param description
      *   the description of the "blueprintee" contact
      * @param validatorScript
      *   the script of the validator
      */
    def apply(
        contractTitle: String,
        description: String,
        validatorScript: PlutusScript
    ): Blueprint = {
        val preamble = Preamble(contractTitle, description, validatorScript.language)
        val blueprintValidator = mkValidator(validatorScript)
        Blueprint(preamble, Seq(blueprintValidator))
    }

    def fromJson(s: String): Blueprint = readFromString(s)

    private def mkValidator(validatorScript: Script) = {
        val cbor = validatorScript match {
            case s: PlutusScript       => s.script.toHex
            case Script.Native(script) => script.toCbor.toHex
        }
        Validator(
          "validator",
          compiledCode = Some(cbor),
          hash = Some(validatorScript.scriptHash.toHex)
        )
    }
}

/** An object that holds blueprint metadata. Does not include information about contracts and
  * instead contains apps title and description, compiler information, plutus version used, etc.
  *
  * For applications that only have 1 validator, the preamble data may repeat that of the validator.
  */
case class Preamble(
    title: String,
    description: Option[String] = None,
    version: Option[String] = None,
    compiler: Option[CompilerInfo] =
        None, // TODO: failed if make default  Some(CompilerInfo.currentScalus)
    plutusVersion: Option[Language] = None,
    license: Option[String] = None
)

object Preamble {
    def apply(title: String, description: String, plutusVersion: Language): Preamble = Preamble(
      title = title,
      description = Some(description),
      compiler = Some(CompilerInfo.currentScalus),
      plutusVersion = Some(plutusVersion)
    )

    given JsonValueCodec[Language] = new JsonValueCodec[Language] {
        override def nullValue: Language = Language.PlutusV3

        override def decodeValue(in: JsonReader, default: Language): Language =
            in.readString("") match {
                case "v1" => Language.PlutusV1
                case "v2" => Language.PlutusV2
                case "v3" => Language.PlutusV3
                case x    =>
                    throw new RuntimeException(
                      s"Error when reading blueprint plutus version. Expected one of [v1, v2, v3], got $x"
                    )
            }

        override def encodeValue(x: Language, out: JsonWriter): Unit =
            out.writeVal(x.show)
    }
    given JsonValueCodec[Preamble] = JsonCodecMaker.make
}

case class CompilerInfo(
    name: String,
    version: Option[String] = None
)
object CompilerInfo {
    given JsonValueCodec[CompilerInfo] = JsonCodecMaker.make
    val currentScalus: CompilerInfo = CompilerInfo("scalus", Some(BuildInfo.version))
}

case class Validator(
    title: String,
    description: Option[String] = None,
    redeemer: Option[TypeDescription] = None,
    datum: Option[TypeDescription] = None,
    parameters: Option[List[TypeDescription]] = None,
    compiledCode: Option[String] = None,
    hash: Option[String] = None
)

object Validator {
    given JsonValueCodec[Validator] = JsonCodecMaker.make
}

case class TypeDescription(
    title: Option[String] = None,
    description: Option[String] = None,
    purpose: Option[Purpose] = None,
    schema: PlutusDataSchema
)

object TypeDescription {
    given JsonValueCodec[TypeDescription] = JsonCodecMaker.make
}

enum DataType(val value: String) {
    case Integer extends DataType("integer")
    case Bytes extends DataType("bytes")
    case List extends DataType("list")
    case Map extends DataType("map")
    case Constructor extends DataType("constructor")
    case UnitBuiltin extends DataType("#unit")
    case BooleanBuiltin extends DataType("#boolean")
    case IntegerBuiltin extends DataType("#integer")
    case BytesBuiltin extends DataType("#bytes")
    case StringBuiltin extends DataType("#string")
    case PairBuiltin extends DataType("#pair")
    case ListBuiltin extends DataType("#list")
}

object DataType {
    given JsonValueCodec[DataType] = new JsonValueCodec[DataType] {
        override def nullValue: DataType = DataType.Integer

        def decodeValue(in: JsonReader, default: DataType): DataType =
            val s = in.readString(null)
            DataType.values.find(_.value == s).getOrElse {
                in.decodeError(s"unknown dataType '$s'")
            }

        def encodeValue(x: DataType, out: JsonWriter): Unit = out.writeVal(x.value)
    }
}

enum Purpose {
    case Spend
    case Mint
    case Withdraw
    case Publish
    case OneOf(purposes: Seq[Purpose]) extends Purpose
}

object Purpose {
    given JsonValueCodec[Purpose] = new JsonValueCodec[Purpose] {
        override def nullValue: Purpose = Purpose.Spend

        def decodeValue(in: JsonReader, default: Purpose): Purpose =
            val s = in.readString(null)
            s match {
                case "spend"    => Spend
                case "mint"     => Mint
                case "withdraw" => Withdraw
                case "publish"  => Publish
                case "oneOf"    => OneOf(Seq.empty) // todo
            }

        def encodeValue(x: Purpose, out: JsonWriter): Unit =
            x match {
                case Spend           => out.writeVal("spend")
                case Mint            => out.writeVal("mint")
                case Withdraw        => out.writeVal("withdraw")
                case Publish         => out.writeVal("publish")
                case OneOf(purposes) =>
                    // todo
                    out.writeArrayStart()
                    out.writeArrayEnd()
            }
    }
}
