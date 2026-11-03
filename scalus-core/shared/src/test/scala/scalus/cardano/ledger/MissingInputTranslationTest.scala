package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

/** A missing input UTxO fails the translation with the ledger's ContextError constructor,
  * `TranslationLogicMissingInput`
  * (eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Plutus/TxInfo.hs:217), as every other BadTranslation
  * message does.
  */
class MissingInputTranslationTest extends AnyFunSuite {

    private val input =
        TransactionInput(TransactionHash.fromByteString(ByteString.fromHex("00" * 32)), 0)

    test("a missing input UTxO is TranslationLogicMissingInput in every Plutus version") {
        val translations = Seq(
          () => LedgerToPlutusTranslation.getTxInInfoV1(input, Map.empty),
          () => LedgerToPlutusTranslation.getTxInInfoV2(input, Map.empty),
          () => LedgerToPlutusTranslation.getTxInInfoV3(input, Map.empty)
        )
        for translate <- translations do
            val thrown = intercept[IllegalStateException](translate())
            assert(thrown.getMessage.startsWith("TranslationLogicMissingInput: "), thrown)
    }
}
