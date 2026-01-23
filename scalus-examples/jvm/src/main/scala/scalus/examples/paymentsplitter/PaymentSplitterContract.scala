package scalus.examples.paymentsplitter

import scalus.uplc.builtin.ByteString
import scalus.cardano.blueprint.{Blueprint, HasTypeDescription, Preamble, Validator}
import scalus.compiler.Options
import scalus.cardano.onchain.plutus.prelude.List
import scalus.uplc.PlutusV3
import scalus.utils.Hex.toHex

private given Options = Options.release
lazy val NaivePaymentSplitterContract = PlutusV3.compile(NaivePaymentSplitterValidator.validate)
lazy val OptimizedPaymentSplitterContract =
    PlutusV3.compile(OptimizedPaymentSplitterValidator.validate)

lazy val NaivePaymentSplitterBlueprint: Blueprint = {
    val title = "Naive Payment Splitter"
    val description =
        "Split payouts equally among a list of specified payees (naive implementation)"
    val compiled = NaivePaymentSplitterContract
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

lazy val OptimizedPaymentSplitterBlueprint: Blueprint = {
    val title = "Optimized Payment Splitter"
    val description =
        "Split payouts equally among a list of specified payees (optimized with stake validator pattern)"
    val compiled = OptimizedPaymentSplitterContract
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
          redeemer = Some(summon[HasTypeDescription[SplitVerificationRedeemer]].typeDescription),
          datum = Some(summon[HasTypeDescription[Unit]].typeDescription),
          parameters = Some(scala.List(param)),
          compiledCode = Some(compiled.program.cborEncoded.toHex),
          hash = Some(compiled.script.scriptHash.toHex)
        )
      )
    )
}
