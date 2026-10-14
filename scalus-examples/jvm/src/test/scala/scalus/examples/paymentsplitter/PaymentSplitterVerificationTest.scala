package scalus.examples.paymentsplitter

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.v1.{Address, Credential, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v2.TxOut
import scalus.cardano.onchain.plutus.v3.{ScriptContext, ScriptInfo, TxCert, TxId, TxInInfo, TxInfo, TxOutRef}
import scalus.compiler.Compile
import scalus.examples.vesting.VestingVerificationTest.{scriptContext, spendingInfo}
import scalus.uplc.builtin.Builtins.{constrData, lengthOfByteString, mkCons, mkNilData, unConstrData}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{toData, FromData, ToData}
import scalus.verify.*
import scalus.verify.Props.*
import scalus.verify.uplcblaster.{Budget, LeanProofs, LeanProofsSuite}

import java.nio.file.Path

/** What the payment splitters do not read of a script context: the fields of the transaction other
  * than its inputs, its outputs and its withdrawals, and the script's redeemer and datum. Each is
  * any `Data`, so a statement over an `Unread` holds whatever they are. A statement about an
  * endpoint that reads the withdrawals or the redeemer gives them, and the field here is not used.
  */
case class Unread(
    referenceInputs: Data,
    fee: Data,
    mint: Data,
    certificates: Data,
    withdrawals: Data,
    validRange: Data,
    signatories: Data,
    redeemers: Data,
    data: Data,
    id: Data,
    votes: Data,
    proposalProcedures: Data,
    currentTreasuryAmount: Data,
    treasuryDonation: Data,
    redeemer: Data,
    datum: Data
) derives FromData,
      ToData

@Compile
object Unread

/** Properties of [[NaivePaymentSplitterValidator]] and [[OptimizedPaymentSplitterValidator]],
  * proved about their compiled UPLC with the `UplcBlaster` tactic (see
  * `docs/design/verification-details/uplc-blaster.md`).
  *
  * A payout splits the lovelace locked at the contract equally among its payees. The statements are
  * about every two payees, any keys, scripts and amounts, and a transaction whose other parts are
  * any `Data` ([[Unread]]). The groups:
  *   - the naive validator: who may submit a payout, who may be paid, that every payee is paid,
  *     that each is paid exactly the split, that only ADA is split, and that an honest payout is
  *     accepted;
  *   - the optimized validator: that spending a contract output needs the stake validator to run
  *     and the redeemer to name that output, that the stake validator checks the same split, and
  *     that the contract's stake credential cannot be delegated.
  *
  * What is not proved:
  *   - a transaction of any shape. The validators fold over the inputs, the outputs and the payees,
  *     and the tactic cannot follow a loop over a list of unknown length: here there are two
  *     payees, a payout has the input of the payee who submits it and one contract output, and pays
  *     two outputs at most;
  *   - the payee who submits a payout is paid the rest, their own input less the fee. That is the
  *     ledger's balance of the transaction, which the validators do not see;
  *   - the published scripts' bytes. The validators are compiled here with the tactic's options
  *     (`UplcBlaster.options`): the statements are about the same source.
  *
  * Without Lean, that is without `lake` and a built workspace, the tests pass on the results kept
  * beside this file (see [[LeanProofs]]).
  */
class PaymentSplitterVerificationTest extends AnyFunSuite with LeanProofsSuite {
    import proofs.*

    import PaymentSplitterVerificationTest.*

    override protected def keptResultsFile: Option[Path] = Some(
      LeanProofs
          .inSources(
            "scalus-examples",
            "jvm",
            "src",
            "test",
            "scala",
            "scalus",
            "examples",
            "paymentsplitter"
          )
          .resolve("PaymentSplitterVerificationTest.proofs.json")
    )

    /** The naive validator, applied to its payees and then to a script context. */
    private val naive = FunctionDef.named(
      "naiveSplitter",
      (payees: Data, context: Data) => NaivePaymentSplitterValidator.validate(payees)(context)
    )

    /** The optimized validator, applied to its payees and then to a script context. */
    private val optimized = FunctionDef.named(
      "optimizedSplitter",
      (payees: Data, context: Data) => OptimizedPaymentSplitterValidator.validate(payees)(context)
    )

    /** Proves `prop` at a budget the tactic finds, and reports it. */
    private def measured(prop: Prop, functions: FunctionDef[?, ?]*): Unit =
        info(s"budget ${provenAt(prop, functions*)}")

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
        val redeemer = SplitVerificationRedeemer(PubKeyHash(first), 100, 50, 2).toData
        assert(
          rewarding(script, redeemer, inputs.toData, outputs.toData, unread) == ScriptContext(
            tx,
            redeemer,
            ScriptInfo.RewardingScript(Credential.ScriptCredential(script))
          ).toData
        )
    }

    // The naive validator

    test("only a payee submits a payout") {
        // Whatever the transaction pays: the input of a key that is not a payee is rejected.
        measured(
          forAll[Unread](rest =>
              forAll[ByteString, ByteString, ByteString]((a, b, payer) =>
                  forAll[BigInt, BigInt, BigInt]((locked, funds, paid) =>
                      !Prop(payer == a || payer == b) ==> fails(
                        naive,
                        (
                          payees(a, b),
                          payout(locked, payer, funds, both(toKey(a, paid), toKey(b, paid)), rest)
                        )
                      )
                  )
              )
          ),
          naive
        )
    }

    test("the payees other than the submitter are paid exactly the split") {
        // The payee who submits is paid the rest, so the other one is paid half the contract's
        // lovelace, rounded down: an accepted payout pays it no more and no less.
        measured(
          forAll[Unread](rest =>
              forAll[ByteString, ByteString, BigInt]((a, b, locked) =>
                  forAll[BigInt, BigInt, BigInt]((funds, toA, toB) =>
                      !Prop(2 * toB <= locked && locked < 2 * toB + 2) ==> fails(
                        naive,
                        (
                          payees(a, b),
                          payout(locked, a, funds, both(toKey(a, toA), toKey(b, toB)), rest)
                        )
                      )
                  )
              )
          ),
          naive
        )
    }

    test("nobody but the payees is paid") {
        // A key that is not a payee, and a script, whatever the amounts.
        measured(
          forAll[Unread](rest =>
              forAll[ByteString, ByteString, ByteString]((a, b, other) =>
                  forAll[BigInt, BigInt, BigInt]((locked, funds, paid) =>
                      !Prop(other == a || other == b) ==> fails(
                        naive,
                        (
                          payees(a, b),
                          payout(locked, a, funds, both(toKey(a, paid), toKey(other, paid)), rest)
                        )
                      )
                  )
              )
          ),
          naive
        )
        measured(
          forAll[Unread](rest =>
              forAll[ByteString, ByteString, ByteString]((a, b, other) =>
                  forAll[BigInt, BigInt, BigInt]((locked, funds, paid) =>
                      fails(
                        naive,
                        (
                          payees(a, b),
                          payout(
                            locked,
                            a,
                            funds,
                            both(toKey(a, paid), toScript(other, Value.lovelace(paid))),
                            rest
                          )
                        )
                      )
                  )
              )
          ),
          naive
        )
    }

    test("every payee is paid") {
        // A payout that pays the submitter alone leaves the other payee out.
        measured(
          forAll[Unread](rest =>
              forAll[ByteString, ByteString, BigInt]((a, b, locked) =>
                  forAll[BigInt, BigInt]((funds, paid) =>
                      fails(
                        naive,
                        (payees(a, b), payout(locked, a, funds, only(toKey(a, paid)), rest))
                      )
                  )
              )
          ),
          naive
        )
    }

    test("the payees other than the submitter are paid exactly the split, negative control") {
        // The split is rounded down, so an odd amount locked is not paid out in two exact halves.
        refuted(
          forAll[Unread](rest =>
              forAll[ByteString, ByteString, BigInt]((a, b, locked) =>
                  forAll[BigInt, BigInt, BigInt]((funds, toA, toB) =>
                      !Prop(2 * toB == locked) ==> fails(
                        naive,
                        (
                          payees(a, b),
                          payout(locked, a, funds, both(toKey(a, toA), toKey(b, toB)), rest)
                        )
                      )
                  )
              )
          ),
          Budget.Auto,
          naive
        )
    }

    test("only ADA is split") {
        // A contract output that holds a token besides its lovelace is rejected: the payout
        // reconciles lovelace alone, so its submitter would keep the token.
        measured(
          forAll[Unread](rest =>
              forAll[ByteString, ByteString, BigInt]((a, b, locked) =>
                  forAll[ByteString, ByteString, BigInt]((policy, name, amount) =>
                      forAll[BigInt, BigInt, BigInt]((funds, toA, toB) =>
                          (!Prop(policy == ByteString.empty && name == ByteString.empty) && !Prop(
                            amount == BigInt(0)
                          )) ==>
                              fails(
                                naive,
                                (
                                  payees(a, b),
                                  payoutOf(
                                    Value.lovelace(locked) + Value(policy, name, amount),
                                    a,
                                    funds,
                                    both(toKey(a, toA), toKey(b, toB)),
                                    rest
                                  )
                                )
                              )
                      )
                  )
              )
          ),
          naive
        )
    }

    test("an honest payout is accepted") {
        // The funds are not stuck: whatever else the transaction has, a payout by one payee that
        // pays the other the split is accepted, for any two different payees and any amounts.
        measured(
          forAll[Unread](rest =>
              forAll[ByteString, ByteString, BigInt]((a, b, locked) =>
                  forAll[BigInt, BigInt, BigInt]((funds, toA, toB) =>
                      (!Prop(a == b) && Prop(
                        2 * toB <= locked && locked < 2 * toB + 2
                      )) ==> succeeds(
                        naive,
                        (
                          payees(a, b),
                          payout(locked, a, funds, both(toKey(a, toA), toKey(b, toB)), rest)
                        )
                      )
                  )
              )
          ),
          naive
        )
        // negative control: not with any amount to the other payee
        refuted(
          forAll[Unread](rest =>
              forAll[ByteString, ByteString, BigInt]((a, b, locked) =>
                  forAll[BigInt, BigInt, BigInt]((funds, toA, toB) =>
                      !Prop(a == b) ==> succeeds(
                        naive,
                        (
                          payees(a, b),
                          payout(locked, a, funds, both(toKey(a, toA), toKey(b, toB)), rest)
                        )
                      )
                  )
              )
          ),
          Budget.Auto,
          naive
        )
    }

    // The optimized validator

    test("a contract output is spent only with the stake validator's withdrawal") {
        // The spending endpoint checks nothing of the payout itself: the withdrawal makes the
        // ledger run the stake validator, which does. Without one from the contract's script, a
        // spend is rejected, whatever the parameter and the outputs are.
        measured(
          forAll[Unread](rest =>
              forAll[Data, Data, ByteString]((parameter, outputs, payer) =>
                  forAll[BigInt](locked =>
                      fails(
                        optimized,
                        (
                          parameter,
                          spendingAt(
                            BigInt(1),
                            payer,
                            BigInt(0),
                            locked,
                            outputs,
                            noWithdrawals,
                            rest
                          )
                        )
                      )
                  )
              )
          ),
          optimized
        )
        measured(
          forAll[Unread](rest =>
              forAll[Data, Data, ByteString]((parameter, outputs, payer) =>
                  forAll[ByteString, BigInt, BigInt]((other, amount, locked) =>
                      !Prop(other == script) ==> fails(
                        optimized,
                        (
                          parameter,
                          spendingAt(
                            BigInt(1),
                            payer,
                            BigInt(0),
                            locked,
                            outputs,
                            withdrawalOf(other, amount),
                            rest
                          )
                        )
                      )
                  )
              )
          ),
          optimized
        )
        // With it, and the redeemer naming the contract's output, the spend is accepted.
        measured(
          forAll[Unread](rest =>
              forAll[Data, Data, ByteString]((parameter, outputs, payer) =>
                  forAll[BigInt, BigInt]((amount, locked) =>
                      succeeds(
                        optimized,
                        (
                          parameter,
                          spendingAt(
                            BigInt(1),
                            payer,
                            BigInt(0),
                            locked,
                            outputs,
                            withdrawalOf(script, amount),
                            rest
                          )
                        )
                      )
                  )
              )
          ),
          optimized
        )
    }

    test("the redeemer of a spend names the contract's output") {
        measured(
          forAll[Unread](rest =>
              forAll[Data, Data, ByteString]((parameter, outputs, payer) =>
                  forAll[BigInt, BigInt, BigInt]((index, amount, locked) =>
                      !Prop(index == BigInt(1)) ==> fails(
                        optimized,
                        (
                          parameter,
                          spendingAt(
                            index,
                            payer,
                            BigInt(0),
                            locked,
                            outputs,
                            withdrawalOf(script, amount),
                            rest
                          )
                        )
                      )
                  )
              )
          ),
          optimized
        )
    }

    test("the stake validator pays the payees other than the submitter exactly the split") {
        // Whatever the redeemer claims: an accepted payout pays the other payee the split, which
        // is half the contract's lovelace, rounded down.
        measured(
          forAll[Unread](rest =>
              forAll[ByteString, ByteString, BigInt]((a, b, locked) =>
                  forAll[BigInt, BigInt, BigInt]((funds, toA, toB) =>
                      forAll[ByteString, BigInt, BigInt]((change, claimed, split) =>
                          forAll[BigInt](n =>
                              (!Prop(a == b) && !Prop(
                                toB == split && 2 * split <= locked && locked < 2 * split + 2
                              )) ==> fails(
                                optimized,
                                (
                                  payees(a, b),
                                  payoutReward(
                                    SplitVerificationRedeemer(
                                      PubKeyHash(change),
                                      claimed,
                                      split,
                                      n
                                    ).toData,
                                    locked,
                                    a,
                                    funds,
                                    both(toKey(a, toA), toKey(b, toB)),
                                    rest
                                  )
                                )
                              )
                          )
                      )
                  )
              )
          ),
          optimized
        )
    }

    test("the stake validator accepts an honest payout") {
        measured(
          forAll[Unread](rest =>
              forAll[ByteString, ByteString, BigInt]((a, b, locked) =>
                  forAll[BigInt, BigInt, BigInt]((funds, toA, split) =>
                      (!Prop(a == b) && Prop(
                        split <= toA && 2 * split <= locked && locked < 2 * split + 2
                      )) ==> succeeds(
                        optimized,
                        (
                          payees(a, b),
                          payoutReward(
                            SplitVerificationRedeemer(
                              PubKeyHash(a),
                              locked,
                              split,
                              BigInt(2)
                            ).toData,
                            locked,
                            a,
                            funds,
                            both(toKey(a, toA), toKey(b, split)),
                            rest
                          )
                        )
                      )
                  )
              )
          ),
          optimized
        )
    }

    test("the contract's stake credential is registered and deregistered, and not delegated") {
        // `RegStaking` is the certificate of tag 0 and `UnRegStaking` the one of tag 1: any other,
        // a delegation among them, is rejected, whatever the transaction is.
        measured(
          forAll[Data, Data, Data]((parameter, txInfo, redeemer) =>
              forAll[Data, BigInt]((cert, index) =>
                  !Prop(
                    unConstrData(cert).fst == BigInt(0) || unConstrData(cert).fst == BigInt(1)
                  ) ==>
                      fails(
                        optimized,
                        (parameter, scriptContext(txInfo, redeemer, certifying(index, cert)))
                      )
              )
          ),
          optimized
        )
        measured(
          forAll[Data, Data, Data]((parameter, txInfo, redeemer) =>
              forAll[BigInt](index =>
                  succeeds(
                    optimized,
                    (
                      parameter,
                      scriptContext(
                        txInfo,
                        redeemer,
                        certifying(
                          index,
                          TxCert
                              .RegStaking(Credential.ScriptCredential(script), prelude.Option.None)
                              .toData
                        )
                      )
                    )
                  )
              )
          ),
          optimized
        )
    }
}

/** The values the statements of [[PaymentSplitterVerificationTest]] are about. They are `inline`,
  * so that each is part of the expression it is used in, which the Scalus plugin compiles. They
  * live in an object: an `inline def` of the test class that uses another one would leave a
  * reference to the test's `this` in that expression.
  */
object PaymentSplitterVerificationTest {

    /** The keys and the script of the sample transactions. */
    inline def first: ByteString = hex"11111111111111111111111111111111111111111111111111111111"

    inline def second: ByteString = hex"22222222222222222222222222222222222222222222222222222222"

    inline def script: ByteString = hex"33333333333333333333333333333333333333333333333333333333"

    /** The contract's output that a payout spends, and the input of the payee who submits it. */
    inline def contractRef: TxOutRef =
        TxOutRef(TxId(hex"4444444444444444444444444444444444444444444444444444444444444444"), 0)

    inline def payerRef: TxOutRef =
        TxOutRef(TxId(hex"6666666666666666666666666666666666666666666666666666666666666666"), 0)

    /** The parameter of both validators: two payees, by their keys. */
    inline def payees(inline a: ByteString, inline b: ByteString): Data =
        prelude.List.Cons(a, prelude.List.Cons(b, prelude.List.Nil)).toData

    /** An output of `amount` lovelace to the key `key`. */
    inline def toKey(inline key: ByteString, inline amount: BigInt): TxOut =
        TxOut(
          Address(Credential.PubKeyCredential(PubKeyHash(key)), prelude.Option.None),
          Value.lovelace(amount)
        )

    /** An output of `value` to the script `hash`. */
    inline def toScript(inline hash: ByteString, inline value: Value): TxOut =
        TxOut(Address(Credential.ScriptCredential(hash), prelude.Option.None), value)

    /** The input of the payee `payer`, of `funds` lovelace, from which the fee is paid. */
    inline def payerInput(inline payer: ByteString, inline funds: BigInt): TxInInfo =
        TxInInfo(payerRef, toKey(payer, funds))

    /** The contract's output, holding `value` under the script `hash`. */
    inline def contractInput(inline hash: ByteString, inline value: Value): TxInInfo =
        TxInInfo(contractRef, toScript(hash, value))

    /** Two outputs, in this order. */
    inline def both(inline a: TxOut, inline b: TxOut): Data =
        prelude.List.Cons(a, prelude.List.Cons(b, prelude.List.Nil)).toData

    /** One output. */
    inline def only(inline a: TxOut): Data = prelude.List.Cons(a, prelude.List.Nil).toData

    /** The transaction, as the ledger encodes it, with these inputs, outputs and withdrawals, all
      * encoded: the rest is `unread`.
      */
    inline def txInfo(
        inline inputs: Data,
        inline outputs: Data,
        inline withdrawals: Data,
        inline unread: Unread
    ): Data =
        constrData(
          0,
          mkCons(
            inputs,
            mkCons(
              unread.referenceInputs,
              mkCons(
                outputs,
                mkCons(
                  unread.fee,
                  mkCons(
                    unread.mint,
                    mkCons(
                      unread.certificates,
                      mkCons(
                        withdrawals,
                        mkCons(
                          unread.validRange,
                          mkCons(
                            unread.signatories,
                            mkCons(
                              unread.redeemers,
                              mkCons(
                                unread.data,
                                mkCons(
                                  unread.id,
                                  mkCons(
                                    unread.votes,
                                    mkCons(
                                      unread.proposalProcedures,
                                      mkCons(
                                        unread.currentTreasuryAmount,
                                        mkCons(unread.treasuryDonation, mkNilData())
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )

    /** The script context of spending the contract's output, in a transaction with these inputs and
      * outputs. Its withdrawals, redeemer and datum are those of `unread`.
      */
    inline def spending(inline inputs: Data, inline outputs: Data, inline unread: Unread): Data =
        scriptContext(
          txInfo(inputs, outputs, unread.withdrawals, unread),
          unread.redeemer,
          spendingInfo(contractRef.toData, unread.datum)
        )

    /** A payout: the contract's output holds `locked` lovelace under [[script]], and the payee
      * `payer` submits the transaction with an input of `funds`. It pays `outputs`.
      */
    inline def payout(
        inline locked: BigInt,
        inline payer: ByteString,
        inline funds: BigInt,
        inline outputs: Data,
        inline unread: Unread
    ): Data =
        spending(
          prelude.List
              .Cons(
                payerInput(payer, funds),
                prelude.List.Cons(contractInput(script, Value.lovelace(locked)), prelude.List.Nil)
              )
              .toData,
          outputs,
          unread
        )

    /** A payout of the contract's output holding `value`, submitted by `payer`. */
    inline def payoutOf(
        inline value: Value,
        inline payer: ByteString,
        inline funds: BigInt,
        inline outputs: Data,
        inline unread: Unread
    ): Data =
        spending(
          prelude.List
              .Cons(
                payerInput(payer, funds),
                prelude.List.Cons(contractInput(script, value), prelude.List.Nil)
              )
              .toData,
          outputs,
          unread
        )

    /** No withdrawal. */
    inline def noWithdrawals: Data = prelude.SortedMap.empty[Credential, BigInt].toData

    /** The withdrawal of `amount` from the stake credential of the script `hash`. */
    inline def withdrawalOf(inline hash: ByteString, inline amount: BigInt): Data =
        prelude.SortedMap.singleton(Credential.ScriptCredential(hash), amount).toData

    /** The optimized validator's spend of the contract's output, holding `locked` lovelace under
      * [[script]], with the redeemer naming input `index`. The other input is of the key `payer`.
      */
    inline def spendingAt(
        inline index: BigInt,
        inline payer: ByteString,
        inline funds: BigInt,
        inline locked: BigInt,
        inline outputs: Data,
        inline withdrawals: Data,
        inline unread: Unread
    ): Data =
        scriptContext(
          txInfo(
            prelude.List
                .Cons(
                  payerInput(payer, funds),
                  prelude.List.Cons(contractInput(script, Value.lovelace(locked)), prelude.List.Nil)
                )
                .toData,
            outputs,
            withdrawals,
            unread
          ),
          SpendRedeemer(index).toData,
          spendingInfo(contractRef.toData, unread.datum)
        )

    /** The stake validator's run for a payout, with `redeemer`: the transaction is the one of
      * [[payout]].
      */
    inline def payoutReward(
        inline redeemer: Data,
        inline locked: BigInt,
        inline payer: ByteString,
        inline funds: BigInt,
        inline outputs: Data,
        inline unread: Unread
    ): Data =
        rewarding(
          script,
          redeemer,
          prelude.List
              .Cons(
                payerInput(payer, funds),
                prelude.List.Cons(contractInput(script, Value.lovelace(locked)), prelude.List.Nil)
              )
              .toData,
          outputs,
          unread
        )

    /** The script info of certifying `cert`, the certificate at `index` of the transaction. */
    inline def certifying(inline index: BigInt, inline cert: Data): Data =
        constrData(3, mkCons(index.toData, mkCons(cert, mkNilData())))

    /** The script context of the stake validator of the script `hash`, run for its withdrawal in a
      * transaction with these inputs and outputs, with `redeemer`.
      */
    inline def rewarding(
        inline hash: ByteString,
        inline redeemer: Data,
        inline inputs: Data,
        inline outputs: Data,
        inline unread: Unread
    ): Data =
        scriptContext(
          txInfo(inputs, outputs, unread.withdrawals, unread),
          redeemer,
          constrData(2, mkCons(Credential.ScriptCredential(hash).toData, mkNilData()))
        )
}
