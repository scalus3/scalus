package scalus.examples.paymentsplitter

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.v1.{Credential, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v3.{ScriptContext, ScriptInfo, TxId, TxInfo}
import scalus.uplc.builtin.Builtins.unConstrData
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.toData

/** What [[PaymentSplitterVerificationTest]] rests on, checked without Lean: the contexts its
  * statements build are the ledger's encoding.
  */
class PaymentSplitterVerificationSupportTest extends AnyFunSuite {
    import PaymentSplitterVerificationTest.*

    test("the contexts written field by field are the ledger's encoding") {
        // The statements write the transaction out, so that what the validators do not read can
        // be any `Data`. With the fields of a transaction that the ledger's types build, they are
        // the context of that transaction.
        val inputs = prelude.List.Cons(
          payerInput(first, BigInt(5)),
          prelude.List.Cons(contractInput(script, Value.lovelace(BigInt(100))), prelude.List.Nil)
        )
        val outputs =
            prelude.List.Cons(
              toKey(first, BigInt(50)),
              prelude.List.Cons(toKey(second, BigInt(50)), prelude.List.Nil)
            )
        val tx = TxInfo(
          inputs = inputs,
          outputs = outputs,
          id = TxId(hex"5555555555555555555555555555555555555555555555555555555555555555")
        )
        val fields = unConstrData(tx.toData).snd.toList
        val unread = Unread(
          referenceInputs = fields(1),
          fee = fields(3),
          mint = fields(4),
          certificates = fields(5),
          withdrawals = fields(6),
          validRange = fields(7),
          signatories = fields(8),
          redeemers = fields(9),
          data = fields(10),
          id = fields(11),
          votes = fields(12),
          proposalProcedures = fields(13),
          currentTreasuryAmount = fields(14),
          treasuryDonation = fields(15),
          redeemer = BigInt(0).toData,
          datum = prelude.Option.Some(BigInt(1).toData).toData
        )
        assert(
          spending(inputs.toData, outputs.toData, unread) == ScriptContext(
            tx,
            BigInt(0).toData,
            ScriptInfo.SpendingScript(contractRef, prelude.Option.Some(BigInt(1).toData))
          ).toData
        )
        val spendRedeemer = SpendRedeemer(1).toData
        val withdrawals =
            prelude.SortedMap.singleton(Credential.ScriptCredential(script), BigInt(0))
        assert(
          spendingAt(
            BigInt(1),
            first,
            BigInt(5),
            BigInt(100),
            outputs.toData,
            withdrawalOf(script, BigInt(0)),
            unread
          ) ==
              ScriptContext(
                TxInfo(
                  inputs = inputs,
                  outputs = outputs,
                  withdrawals = withdrawals,
                  id = TxId(hex"5555555555555555555555555555555555555555555555555555555555555555")
                ),
                spendRedeemer,
                ScriptInfo.SpendingScript(contractRef, prelude.Option.Some(BigInt(1).toData))
              ).toData
        )
        // The value of a contract output with a token, written out for the solver, is the sum the
        // ledger's types make.
        val token = hex"44444444444444444444444444444444444444444444444444444444"
        assert(
          lovelaceAnd(BigInt(7), token, hex"cafe", BigInt(3)) ==
              Value.lovelace(BigInt(7)) + Value(token, hex"cafe", BigInt(3))
        )
        val redeemer = SplitVerificationRedeemer(PubKeyHash(first), 100, 50, 2).toData
        assert(
          rewarding(script, redeemer, inputs.toData, outputs.toData, unread) == ScriptContext(
            tx,
            redeemer,
            ScriptInfo.RewardingScript(Credential.ScriptCredential(script))
          ).toData
        )
    }
}
