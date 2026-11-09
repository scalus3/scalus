package scalus.examples.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.v3.{Interval, ScriptContext}
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.toData

/** What [[HtlcVerificationTest]] rests on, checked without Lean: the premises of its statements
  * that a refund and a reveal are accepted are not vacuous.
  */
class HtlcVerificationSupportTest extends AnyFunSuite {
    import HtlcVerificationTest.*

    test("the samples the validator accepts meet the premises of the statements that it accepts") {
        // A statement whose premise no context meets would be proved for nothing. The samples
        // are accepted on the Lean machine; here, on the JVM, they meet the premises.
        val refund = sampleSpending(
          Action.Timeout.toData,
          Interval.after(BigInt(1000)),
          signedBy(committer)
        ).to[ScriptContext]
        assert(isRefund(refund) && startsFromTheTimeout(refund) && signedByTheCommitter(refund))
        val reveal = sampleSpending(
          Action.Reveal(hex"cafe").toData,
          Interval.entirelyBefore(BigInt(1000)),
          signedBy(receiver)
        ).to[ScriptContext]
        assert(
          isReveal(reveal) && endsByTheTimeout(reveal) && signedByTheReceiver(reveal) &&
              revealsAPreimage(reveal)
        )
        // The reveal ends at the timeout, and the refund starts at it: not before the reveal
        // ends, which the statement of the two together needs to reject one of them.
        assert(datumOf(reveal) == datumOf(refund) && !startsBeforeTheEnd(refund, reveal))
    }

    test("a context with a bound flipped is the context with that bound the other way") {
        // The statements that a bound's closure does not matter compare the validator on a
        // context and on that one.
        val ctx = sampleSpending(
          Action.Timeout.toData,
          Interval.after(BigInt(1000)),
          signedBy(committer)
        ).to[ScriptContext]
        val range = ctx.txInfo.validRange
        val lower = withTheLowerBoundFlipped(ctx).to[ScriptContext]
        assert(lower.txInfo.validRange.from.isInclusive == !range.from.isInclusive)
        assert(lower.txInfo.validRange.to == range.to)
        assert(lower.copy(txInfo = lower.txInfo.copy(validRange = range)).toData == ctx.toData)
        val upper = withTheUpperBoundFlipped(ctx).to[ScriptContext]
        assert(upper.txInfo.validRange.to.isInclusive == !range.to.isInclusive)
        assert(upper.copy(txInfo = upper.txInfo.copy(validRange = range)).toData == ctx.toData)
    }
}
