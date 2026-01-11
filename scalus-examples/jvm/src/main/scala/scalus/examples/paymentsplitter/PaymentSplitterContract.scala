package scalus.examples.paymentsplitter

import scalus.builtin.ByteString
import scalus.cardano.blueprint.{Blueprint, HasTypeDescription, Preamble, Validator}
import scalus.compiler.Options
import scalus.prelude.List
import scalus.uplc.PlutusV3
import scalus.utils.Hex.toHex

private given Options = Options.release
lazy val PaymentSplitterContract = PlutusV3.compile(PaymentSplitterValidator.validate)

lazy val PaymentSplitterBlueprint: Blueprint = {
    val title = "Payment Splitter"
    val description = "Split payouts equally among a list of specified payees"
    val compiled = PaymentSplitterContract
    val param = summon[HasTypeDescription[List[ByteString]]].typeDescription
    Blueprint(
      preamble = Preamble(
        title,
        description,
        "1.0.0",
        plutusVersion = compiled.language,
        license = Some("Apache-2.0")
      ),
      validators = Seq(
        Validator(
          title = title,
          description = Some(description),
          redeemer = Some(summon[HasTypeDescription[Unit]].typeDescription),
          datum = Some(summon[HasTypeDescription[Unit]].typeDescription),
          parameters = Some(scala.List(param)),
          compiledCode = Some(compiled.program.cborEncoded.toHex),
          hash = Some(compiled.script.scriptHash.toHex)
        )
      )
    )
}
