package scalus.cardano.blueprint

import scalus.*
import scalus.cardano.address.*
import scalus.cardano.ledger.{Credential, Language, PlutusScript, Script}
import scalus.compiler.sir.SIR
import scalus.uplc.Program

/** A description of a Scalus application, containing one or more contracts.
  */
case class Application(
    preamble: Preamble,
    contracts: Seq[CompiledContract],
) {

    /** A CIP-57 compliant Blueprint, describing the application. */
    def blueprint: Blueprint = {
        Blueprint(preamble, validators = contracts.map(_.describeValidator))
    }
}

object Application {
    def apply(
        title: String,
        description: String,
        _version: String,
        contracts: Seq[CompiledContract]
    ): Application = {
        val preamble = Preamble(title, description, Language.PlutusV3)
        new Application(preamble, contracts)
    }

    inline def ofSingleValidator[D, R](
        title: String,
        description: String,
        version: String,
        inline code: Any
    ): Application = {
        val contract = PlutusV3CompiledContract.create[D, R](title, description)(code)
        Application(title, description, version, Seq(contract))
    }
}

/** A smart contract compiled with Scalus. */
trait CompiledContract {
    def sir: SIR
    def program: Program
    def script: PlutusScript
    def describeValidator: Validator
    def blueprint: Blueprint

    def address(network: Network): Address = Address(
      network,
      Credential.ScriptHash(script.scriptHash)
    )
}

class PlutusV3CompiledContract(
    preamble: Preamble,
    override val sir: SIR,
    override val program: Program,
    datumSchema: Option[PlutusDataSchema],
    redeemerSchema: Option[PlutusDataSchema]
) extends CompiledContract {
    require(
      preamble.plutusVersion.forall(_ == scalus.cardano.ledger.Language.PlutusV3),
      "PlutusV3Contract must have PlutusV3 as its plutus version in the preamble"
    )

    require(
      program.version == (1, 1, 0),
      "PlutusV3Contract must have UPLC version 1.1.0"
    )

    override val script: Script.PlutusV3 = Script.PlutusV3(program.cborByteString)

    override lazy val describeValidator: Validator = {
        Validator(
          title = preamble.title,
          // TODO: test failed if uncommit
          // description = Some(preamble.description),
          datum = datumSchema.map(schema => TypeDescription(schema = schema)),
          redeemer = redeemerSchema.map(schema => TypeDescription(schema = schema)),
          compiledCode = Some(script.toHex),
          hash = Some(script.scriptHash.toHex)
        )
    }

    override lazy val blueprint: Blueprint = Blueprint(preamble, Seq(describeValidator))
}

object PlutusV3CompiledContract {

    inline def create[D, R](
        title: String,
        description: String = ""
    )(inline code: Any): PlutusV3CompiledContract = {
        val sir = Compiler.compileInline(code)
        val program = sir.toUplcOptimized().plutusV3
        val datumSchema = PlutusDataSchema.derived[D]
        val redeemerSchema = PlutusDataSchema.derived[R]
        PlutusV3CompiledContract(
          Preamble(title, Some(description)),
          sir,
          program,
          Some(datumSchema),
          Some(redeemerSchema)
        )
    }

    inline def create[D, R](
        preamble: Preamble,
        inline options: scalus.Compiler.Options
    )(inline code: Any): PlutusV3CompiledContract = {
        val sir = Compiler.compileInlineWithOptions(options, code)
        val program = sir.toUplc(using options)().plutusV3
        val datumSchema = PlutusDataSchema.derived[D]
        val redeemerSchema = PlutusDataSchema.derived[R]
        PlutusV3CompiledContract(preamble, sir, program, Some(datumSchema), Some(redeemerSchema))
    }
}
