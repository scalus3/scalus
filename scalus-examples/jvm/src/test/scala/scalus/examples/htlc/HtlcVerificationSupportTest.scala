package scalus.examples.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.v3.{IntervalBound, IntervalBoundType}
import scalus.cardano.onchain.plutus.v3.{Interval, ScriptContext, ScriptInfo, TxId, TxInfo, TxOutRef}
import scalus.examples.vesting.VestingVerificationTest.{noDatum, scriptContext, someDatum, spendingInfo}
import scalus.uplc.builtin.Builtins.unConstrData
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData

/** What [[HtlcVerificationTest]] rests on, checked without Lean: the contexts its statements build
  * are the ledger's encoding.
  */
class HtlcVerificationSupportTest extends AnyFunSuite {
    import HtlcVerificationTest.*

    test("the contexts built from Data are the ledger's encoding") {
        // The statements of `HtlcVerificationTest` say that the validator fails, which a malformed context would make
        // true for the wrong reason.
        val ref = TxOutRef(TxId(hex"00"), 0)
        val datum = Config(committer, receiver, hex"ab", BigInt(7)).toData
        val timeout: Action = Action.Timeout
        def expected(datum: prelude.Option[Data]) = {
            ScriptContext(
              TxInfo.placeholder,
              timeout.toData,
              ScriptInfo.SpendingScript(ref, datum)
            ).toData
        }
        def built(datum: Data) = {
            scriptContext(
              TxInfo.placeholder.toData,
              timeout.toData,
              spendingInfo(ref.toData, datum)
            )
        }
        assert(built(noDatum) == expected(prelude.Option.None))
        assert(built(someDatum(datum)) == expected(prelude.Option.Some(datum)))
    }

    test("a refund and a reveal written field by field are the ledger's encoding") {
        // The statements about them write the transaction out, so that what the validator does
        // not read can be any `Data`. With the fields of a transaction that the ledger's types
        // build, they are the context of that transaction.
        val from = IntervalBound(IntervalBoundType.Finite(BigInt(5)), true)
        val to = IntervalBound(IntervalBoundType.Finite(BigInt(9)), false)
        val txInfo = TxInfo(
          inputs = prelude.List.Nil,
          validRange = Interval(from, to),
          signatories = signedBy(committer),
          id = TxId(hex"5555555555555555555555555555555555555555555555555555555555555555")
        )
        val fields = unConstrData(txInfo.toData).snd.toList
        def unread(otherBound: Data) = Unread(
          inputs = fields(0),
          referenceInputs = fields(1),
          outputs = fields(2),
          fee = fields(3),
          mint = fields(4),
          certificates = fields(5),
          withdrawals = fields(6),
          redeemers = fields(9),
          data = fields(10),
          id = fields(11),
          votes = fields(12),
          proposalProcedures = fields(13),
          currentTreasuryAmount = fields(14),
          treasuryDonation = fields(15),
          otherBound = otherBound,
          ref = htlcRef.toData
        )
        def expected(redeemer: Data) = {
            ScriptContext(
              txInfo,
              redeemer,
              ScriptInfo.SpendingScript(htlcRef, prelude.Option.Some(sample.toData))
            ).toData
        }
        assert(
          refund(sample, BigInt(5), signedBy(committer), unread(to.toData)) ==
              expected(Action.Timeout.toData)
        )
        assert(
          reveal(sample, hex"cafe", BigInt(9), signedBy(committer), unread(from.toData)) ==
              expected(Action.Reveal(hex"cafe").toData)
        )
    }
}
