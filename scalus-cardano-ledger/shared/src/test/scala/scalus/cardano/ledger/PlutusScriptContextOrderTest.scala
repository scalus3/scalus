package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.rules.ValidatorRulesTestKit
import scalus.uplc.Term
import scalus.uplc.builtin.Data

import scala.collection.immutable.SortedMap

/** The evaluator builds the script context of every needed script before it runs any script, as the
  * ledger collects all of them before it evaluates one
  * (eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Plutus/Evaluate.hs:157-214).
  */
class PlutusScriptContextOrderTest extends AnyFunSuite, ValidatorRulesTestKit {

    test("a context that cannot be built fails the tx before an earlier script runs") {
        // The mint script runs first and fails. The withdrawal script comes after it, and its
        // PlutusV2 context cannot be built: V2 has no donation field. An evaluator that built each
        // context just before its script would throw PlutusScriptEvaluationException instead.
        val failingMint = Script.PlutusV3(Term.Error().plutusV3.cborByteString)
        val withdrawal = Script.PlutusV2(Term.Error().plutusV2.cborByteString)
        val (mintOnly, utxos) = mintTx(failingMint)
        val account = RewardAccount(
          StakeAddress(Network.Mainnet, StakePayload.Script(withdrawal.scriptHash))
        )
        val tx = Transaction(
          TransactionBody(
            inputs = mintOnly.body.value.inputs,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            mint = mintOnly.body.value.mint,
            withdrawals = Some(Withdrawals(SortedMap(account -> Coin.zero))),
            donation = Some(Coin(1))
          ),
          TransactionWitnessSet(
            scripts = Seq(failingMint, withdrawal),
            redeemers = Some(
              Redeemers(
                Redeemer(RedeemerTag.Mint, 0, Data.unit, ExUnits.zero),
                Redeemer(RedeemerTag.Reward, 0, Data.unit, ExUnits.zero)
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val evaluator =
            PlutusScriptEvaluator(CardanoInfo.mainnet, EvaluatorMode.EvaluateAndComputeCost)
        val error = intercept[IllegalArgumentException] {
            evaluator.evalPlutusScripts(tx, utxos)
        }
        assert(error.getMessage.startsWith("TreasuryDonationFieldNotSupported"))
    }
}
