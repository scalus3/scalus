package scalus.cardano.blueprint

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scalus.builtin.Data
import scalus.cardano.ledger.{Language, PlutusScript, Script}
import scalus.uplc.PlutusV3
import scalus.utils.BuildInfo
import scalus.utils.Hex.toHex

import java.io.{File, InputStream}
import java.nio.file.Files
import scala.annotation.targetName

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

    /** Returns a CIP-57 compliant [[Blueprint]] based on the provided validator script.
      *
      * The returned `Blueprint` always contains only 1 validator.
      *
      * To specify the `redeemer` and `datum` schemas, use [[Application]] instead.
      *
      * @param title
      *   the title of the "blueprintee" contract
      * @param description
      *   the description of the "blueprintee" contact
      * @param validatorScript
      *   the script of the validator
      */
    @deprecated("Use plutusV* methods", "0.14.1")
    def apply(
        title: String,
        description: String,
        validatorScript: PlutusScript
    ): Blueprint = {
        val preamble = Preamble(title, description, validatorScript.language)
        val blueprintValidator = mkValidator(validatorScript)
        Blueprint(preamble, Seq(blueprintValidator))
    }

    /** Creates a CIP-57 compliant [[Blueprint]] from a compiled validator with datum and redeemer
      * type descriptions.
      *
      * The returned `Blueprint` contains a single validator with automatically derived schemas for
      * the datum and redeemer types.
      *
      * @tparam Datum
      *   the type of the datum, must have a [[HasTypeDescription]] instance
      * @tparam Redeemer
      *   the type of the redeemer, must have a [[HasTypeDescription]] instance
      * @param title
      *   the title of the validator
      * @param description
      *   the description of the validator
      * @param version
      *   the version of the blueprint (e.g., "1.0.0")
      * @param license
      *   optional license identifier (e.g., Some("Apache-2.0"))
      * @param compiled
      *   the compiled validator script
      * @return
      *   a [[Blueprint]] with a single validator
      */
    @targetName("plutusV3Spend")
    def plutusV3[Datum: HasTypeDescription, Redeemer: HasTypeDescription](
        title: String,
        description: String,
        version: String,
        license: Option[String],
        compiled: PlutusV3[Data => Unit]
    ): Blueprint = {
        val preamble = Preamble(
          title,
          description,
          version,
          plutusVersion = compiled.language,
          license = license
        )
        val redeemer = summon[HasTypeDescription[Redeemer]].typeDescription
        val datum = summon[HasTypeDescription[Datum]].typeDescription
        val validator = Validator(
          title = title,
          description = Some(description),
          redeemer = Some(redeemer),
          datum = Some(datum),
          parameters = None,
          compiledCode = Some(compiled.program.cborEncoded.toHex),
          hash = Some(compiled.script.scriptHash.toHex)
        )
        Blueprint(preamble, Seq(validator))
    }

    /** Creates a CIP-57 compliant [[Blueprint]] from a parameterized compiled validator with datum
      * and redeemer type descriptions.
      *
      * The returned `Blueprint` contains a single validator with automatically derived schemas for
      * the parameter, datum, and redeemer types. Use this method when your validator requires
      * parameters to be applied before deployment.
      *
      * @tparam Param
      *   the type of the validator parameter, must have a [[HasTypeDescription]] instance
      * @tparam Datum
      *   the type of the datum, must have a [[HasTypeDescription]] instance
      * @tparam Redeemer
      *   the type of the redeemer, must have a [[HasTypeDescription]] instance
      * @param title
      *   the title of the validator
      * @param description
      *   the description of the validator
      * @param version
      *   the version of the blueprint (e.g., "1.0.0")
      * @param license
      *   optional license identifier (e.g., Some("Apache-2.0"))
      * @param compiled
      *   the compiled parameterized validator script
      * @return
      *   a [[Blueprint]] with a single validator including parameter schema
      */
    @targetName("plutusV3ParameterizedSpend")
    def plutusV3[
        Param: HasTypeDescription,
        Datum: HasTypeDescription,
        Redeemer: HasTypeDescription
    ](
        title: String,
        description: String,
        version: String,
        license: Option[String],
        compiled: PlutusV3[Param => Data => Unit]
    ): Blueprint = {
        val preamble = Preamble(
          title,
          description,
          version,
          plutusVersion = compiled.language,
          license = license
        )
        val param = summon[HasTypeDescription[Param]].typeDescription
        val redeemer = summon[HasTypeDescription[Redeemer]].typeDescription
        val datum = summon[HasTypeDescription[Datum]].typeDescription
        val validator = Validator(
          title = title,
          description = Some(description),
          redeemer = Some(redeemer),
          datum = Some(datum),
          parameters = Some(List(param)),
          compiledCode = Some(compiled.program.cborEncoded.toHex),
          hash = Some(compiled.script.scriptHash.toHex)
        )
        Blueprint(preamble, Seq(validator))
    }

    /** Creates a CIP-57 compliant [[Blueprint]] from a compiled validator with only a redeemer type
      * description.
      *
      * The returned `Blueprint` contains a single validator with an automatically derived schema
      * for the redeemer type. Use this method for validators that don't use datum (e.g., minting
      * policies, staking validators).
      *
      * @tparam Redeemer
      *   the type of the redeemer, must have a [[HasTypeDescription]] instance
      * @param title
      *   the title of the validator
      * @param description
      *   the description of the validator
      * @param version
      *   the version of the blueprint (e.g., "1.0.0")
      * @param license
      *   optional license identifier (e.g., Some("Apache-2.0"))
      * @param compiled
      *   the compiled validator script
      * @return
      *   a [[Blueprint]] with a single validator (no datum schema)
      */
    def plutusV3[Redeemer: HasTypeDescription](
        title: String,
        description: String,
        version: String,
        license: Option[String],
        compiled: PlutusV3[Data => Unit]
    ): Blueprint = {
        val preamble = Preamble(
          title,
          description,
          version,
          plutusVersion = compiled.language,
          license = license
        )
        val redeemer = summon[HasTypeDescription[Redeemer]].typeDescription
        val validator = Validator(
          title = title,
          description = Some(description),
          redeemer = Some(redeemer),
          datum = None,
          parameters = None,
          compiledCode = Some(compiled.program.cborEncoded.toHex),
          hash = Some(compiled.script.scriptHash.toHex)
        )
        Blueprint(preamble, Seq(validator))
    }

    /** Creates a CIP-57 compliant [[Blueprint]] from a parameterized compiled validator with only a
      * redeemer type description (no datum).
      *
      * The returned `Blueprint` contains a single validator with automatically derived schemas for
      * the parameter and redeemer types. Use this method for parameterized validators that don't
      * use datum (e.g., parameterized minting policies).
      *
      * @tparam Param
      *   the type of the validator parameter, must have a [[HasTypeDescription]] instance
      * @tparam Redeemer
      *   the type of the redeemer, must have a [[HasTypeDescription]] instance
      * @param title
      *   the title of the validator
      * @param description
      *   the description of the validator
      * @param compiled
      *   the compiled parameterized validator script
      * @return
      *   a [[Blueprint]] with a single validator including parameter schema but no datum schema
      */
    @targetName("plutusV3Parameterized")
    def plutusV3[Param: HasTypeDescription, Redeemer: HasTypeDescription](
        title: String,
        description: String,
        version: String,
        license: Option[String],
        compiled: PlutusV3[Param => Data => Unit]
    ): Blueprint = {
        val preamble = Preamble(
          title,
          description,
          version,
          plutusVersion = compiled.language,
          license = license
        )
        val param = summon[HasTypeDescription[Param]].typeDescription
        val redeemer = summon[HasTypeDescription[Redeemer]].typeDescription
        val validator = Validator(
          title = title,
          description = Some(description),
          redeemer = Some(redeemer),
          datum = None,
          parameters = Some(List(param)),
          compiledCode = Some(compiled.program.cborEncoded.toHex),
          hash = Some(compiled.script.scriptHash.toHex)
        )
        Blueprint(preamble, Seq(validator))
    }

    /** Parses a CIP-57 compliant [[Blueprint]] from a JSON string.
      *
      * @param json
      *   the JSON string representing a blueprint
      * @return
      *   the parsed [[Blueprint]]
      * @throws com.github.plokhotnyuk.jsoniter_scala.core.JsonReaderException
      *   if the JSON is invalid or doesn't conform to the CIP-57 schema
      */
    def fromJson(json: String): Blueprint = readFromString(json)

    /** Parses a CIP-57 compliant [[Blueprint]] from a [[java.io.InputStream]].
      *
      * @param inputStream
      *   the input stream containing JSON data representing a blueprint
      * @return
      *   the parsed [[Blueprint]]
      * @throws com.github.plokhotnyuk.jsoniter_scala.core.JsonReaderException
      *   if the JSON is invalid or doesn't conform to the CIP-57 schema
      */
    def fromJson(inputStream: InputStream): Blueprint = readFromStream(inputStream)

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

    def apply(
        title: String,
        description: String,
        version: String,
        plutusVersion: Language,
        license: Option[String]
    ): Preamble = Preamble(
      title = title,
      description = Some(description),
      version = Some(version),
      license = license,
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
                case x =>
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

/** Type class for types that can provide a [[TypeDescription]] for CIP-57 blueprints.
  *
  * Instances of this type class are used to generate datum, redeemer, and parameter schemas in
  * [[Blueprint]] validators.
  *
  * @tparam A
  *   the type that can be described
  */
case class HasTypeDescription[A](typeDescription: TypeDescription)

object HasTypeDescription {

    /** Derives a [[HasTypeDescription]] instance for type `A` using [[TypeDescription.derived]]. */
    inline given derived[A]: HasTypeDescription[A] = HasTypeDescription[A](
      typeDescription = TypeDescription.derived[A]
    )
}

/** Describes a type for CIP-57 blueprint schemas.
  *
  * Contains metadata about a type (title, description, purpose) along with its [[PlutusDataSchema]]
  * that describes the Plutus Data encoding.
  *
  * @param title
  *   optional title for the type
  * @param description
  *   optional description of the type
  * @param purpose
  *   optional purpose (spend, mint, withdraw, publish)
  * @param schema
  *   the Plutus Data schema for the type
  */
case class TypeDescription(
    title: Option[String] = None,
    description: Option[String] = None,
    purpose: Option[Purpose] = None,
    schema: PlutusDataSchema
)

object TypeDescription {
    given JsonValueCodec[TypeDescription] = JsonCodecMaker.make

    /** Derives a [[TypeDescription]] for type `A` by deriving its [[PlutusDataSchema]].
      *
      * The title is extracted from the derived schema.
      *
      * @tparam A
      *   the type to derive a description for
      * @return
      *   a [[TypeDescription]] with the derived schema
      */
    inline def derived[A]: TypeDescription = {
        val schema = PlutusDataSchema.derived[A]
        TypeDescription(
          title = schema.title,
          description = None,
          purpose = None,
          schema = schema
        )
    }
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
