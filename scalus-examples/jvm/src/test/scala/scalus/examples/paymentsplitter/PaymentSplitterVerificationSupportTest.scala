package scalus.examples.paymentsplitter

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.v3.ScriptContext
import scalus.uplc.builtin.ByteString

/** What [[PaymentSplitterVerificationTest]] rests on, checked without Lean: the premises of its
  * statements that a payout is accepted are not vacuous.
  */
class PaymentSplitterVerificationSupportTest extends AnyFunSuite {
    import PaymentSplitterVerificationTest.*

    private val payees = samplePayees.to[prelude.List[ByteString]]

    test("the sample payouts the validators accept meet the premises of the statements") {
        // A statement whose premise no context meets would be proved for nothing. The samples
        // are accepted on the Lean machine; here, on the JVM, they meet the premises of the
        // statements that an honest payout is accepted.
        val payout = samplePayout.to[ScriptContext]
        assert(isSpending(payout) && areDifferent(payees))
        assert(aPayout(payout))
        assert(isAPayoutOfTheSplit(payees, payout, spentCredential(payout)))
        val spend = sampleSpend.to[ScriptContext]
        assert(aSpend(spend))
        assert(isSpending(spend) && namesTheSpentInput(spend))
        assert(isWithdrawnFrom(spend, namedInput(spend)))
        val reward = sampleReward.to[ScriptContext]
        assert(aPayout(reward))
        assert(isRewarding(reward) && claimsAnHonestPayout(payees, reward))
    }

    test("the premises of the rejections do not hold of the sample payouts") {
        // The validators accept the samples, so each premise that says a payout is rejected is
        // false of them: a premise true of an accepted payout would make its statement false.
        val payout = samplePayout.to[ScriptContext]
        val contract = spentCredential(payout)
        assert(payout.txInfo.inputs.exists(i => isAPayee(payees, i.resolved.address.credential)))
        assert(!spendsAnInputOfNeither(payees, payout, contract))
        assert(!someOtherPayeeIsNotPaidTheSplit(payees, payout, contract))
        assert(!payout.txInfo.outputs.exists(o => !isAPayee(payees, o.address.credential)))
        assert(!payees.exists(payee => !isPaid(payout, payee)))
        assert(!spendsAContractInputWithTokens(payees, payout, contract))
        val reward = sampleReward.to[ScriptContext]
        assert(!someOtherPayeeIsNotPaidTheSplit(payees, reward, rewardedCredential(reward)))
    }
}
