package scalus.examples

import scalus.*
import scalus.builtin.{ByteString, Data}
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.cardano.blueprint.{CompilerInfo, Contract, Preamble}
import scalus.cardano.ledger.Language

@Compile
object TmpValidator extends Validator {
    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        log("TmpValidator invoked")
    }
}

lazy val TmpContract = Contract.PlutusV3Contract[ByteString, ByteString](
  Preamble(
    title = "Temporary contract",
    description = Some("Test issue."),
    version = Some("1.0.0"),
    compiler = Some(CompilerInfo.currentScalus),
    plutusVersion = Some(Language.PlutusV3),
    license = None
  ),
  TmpValidator.validate
)
