package scalus.examples.vesting

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.v1.{Address, Credential, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v3.{Interval, ScriptContext, ScriptInfo, TxId, TxInInfo, TxInfo, TxOut, TxOutRef}
import scalus.uplc.builtin.Builtins.{constrData, mkCons, mkNilData, unConstrData}
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.verify.*
import scalus.verify.Props.*
import scalus.verify.uplcblaster.LeanProofs

/** Properties of [[VestingValidator]], proved about its compiled UPLC with the `UplcBlaster` tactic
  * (see `docs/design/verification-details/uplc-blaster.md`).
  *
  * Three groups of statements, from the most general to the most specific:
  *   - the vesting schedule, `linearVesting`, for every datum and time;
  *   - the validator on every script context of some form, whatever the transaction is;
  *   - the validator on withdrawals of one shape, see [[withdrawal]]: who may withdraw, how much,
  *     and that a vested amount can be withdrawn.
  *
  * Two limits of what is proved:
  *   - the validator is compiled here with the tactic's options (`UplcBlaster.options`), which
  *     differ from those of [[VestingContract]]: Lean's model has no `Value` builtins yet. The
  *     statements are about the same source, not about the published script's bytes;
  *   - a statement about withdrawals says nothing about transactions of another shape. The
  *     validator walks the inputs and the outputs, and the tactic cannot follow a loop over a list
  *     of unknown length.
  *
  * The tests need Lean: without `lake` and a built workspace they are canceled (see
  * [[LeanProofs]]).
  */
class VestingVerificationTest extends AnyFunSuite with LeanProofs {
    import VestingVerificationTest.*

    // The schedule

    private val linearVesting = FunctionDef(VestingValidator.linearVesting)

    /** The steps Lean's CEK may make per test of a statement about the schedule. It has no loop, so
      * its step count does not depend on its input.
      */
    private val scheduleBudget = 400

    test("nothing is vested before the start") {
        proven(
          forAll[BigInt, BigInt]((start, duration) =>
              forAll[BigInt, BigInt]((amount, time) =>
                  (time < start) ==>
                      call(linearVesting, (config(start, duration, amount), time))(vested =>
                          vested == BigInt(0)
                      )
              )
          ),
          scheduleBudget,
          linearVesting
        )
    }

    test("everything is vested from the end of the period on") {
        proven(
          forAll[BigInt, BigInt]((start, duration) =>
              forAll[BigInt, BigInt]((amount, time) =>
                  (start <= time && start + duration <= time) ==>
                      call(linearVesting, (config(start, duration, amount), time))(vested =>
                          vested == amount
                      )
              )
          ),
          scheduleBudget,
          linearVesting
        )
    }

    test("the schedule returns for every datum and time") {
        // In particular it does not divide by a zero duration.
        proven(
          forAll[BigInt, BigInt]((start, duration) =>
              forAll[BigInt, BigInt]((amount, time) =>
                  succeeds(linearVesting, (config(start, duration, amount), time))
              )
          ),
          scheduleBudget,
          linearVesting
        )
    }

    test("no more than the initial amount is ever vested") {
        proven(
          forAll[BigInt, BigInt]((start, duration) =>
              forAll[BigInt, BigInt]((amount, time) =>
                  (amount >= BigInt(0)) ==>
                      call(linearVesting, (config(start, duration, amount), time))(vested =>
                          BigInt(0) <= vested && vested <= amount
                      )
              )
          ),
          scheduleBudget,
          linearVesting
        )
        // negative control: a negative initial amount is below what is vested before the start
        refuted(
          forAll[BigInt, BigInt]((start, duration) =>
              forAll[BigInt, BigInt]((amount, time) =>
                  call(linearVesting, (config(start, duration, amount), time))(vested =>
                      vested <= amount
                  )
              )
          ),
          scheduleBudget,
          linearVesting
        )
    }

    test("the vested amount does not decrease with time") {
        proven(
          forAll[BigInt, BigInt]((start, duration) =>
              forAll[BigInt, BigInt, BigInt]((amount, earlier, later) =>
                  (amount >= BigInt(0) && earlier <= later) ==>
                      call(linearVesting, (config(start, duration, amount), earlier))(before =>
                          call(linearVesting, (config(start, duration, amount), later))(after =>
                              before <= after
                          )
                      )
              )
          ),
          scheduleBudget,
          linearVesting
        )
        // negative control: a negative initial amount is taken away over time
        refuted(
          forAll[BigInt, BigInt]((start, duration) =>
              forAll[BigInt, BigInt, BigInt]((amount, earlier, later) =>
                  (earlier <= later) ==>
                      call(linearVesting, (config(start, duration, amount), earlier))(before =>
                          call(linearVesting, (config(start, duration, amount), later))(after =>
                              before <= after
                          )
                      )
              )
          ),
          scheduleBudget,
          linearVesting
        )
    }

    // The validator, on every script context

    /** The validator's entry point, compiled with the tactic's options. */
    private val validator =
        FunctionDef.named("vesting", (context: Data) => VestingValidator.validate(context))

    /** The steps for a context the validator rejects before it reads the transaction. */
    private val rejectionBudget = 600

    test("the contexts built from Data are the ledger's encoding") {
        // The statements below say that the validator fails, which a malformed context would make
        // true for the wrong reason.
        val ref = TxOutRef(TxId(hex"00"), 0)
        val datum = config(BigInt(1), BigInt(2), BigInt(3)).toData
        val redeemer = Action(BigInt(4)).toData
        def expected(datum: prelude.Option[Data]) =
            ScriptContext(
              TxInfo.placeholder,
              redeemer,
              ScriptInfo.SpendingScript(ref, datum)
            ).toData
        def built(datum: Data) =
            scriptContext(TxInfo.placeholder.toData, redeemer, spendingInfo(ref.toData, datum))
        assert(built(noDatum) == expected(prelude.Option.None))
        assert(built(someDatum(datum)) == expected(prelude.Option.Some(datum)))
    }

    test("an output without a datum cannot be spent") {
        proven(
          forAll[Data, Data, Data]((txInfo, redeemer, ref) =>
              fails(validator, scriptContext(txInfo, redeemer, spendingInfo(ref, noDatum)))
          ),
          rejectionBudget,
          validator
        )
    }

    test("a withdrawal of a non-positive amount is rejected") {
        proven(
          forAll[Data, Data]((txInfo, ref) =>
              forAll[Data, BigInt]((datum, requested) =>
                  (requested <= BigInt(0)) ==> fails(
                    validator,
                    scriptContext(
                      txInfo,
                      Action(requested).toData,
                      spendingInfo(ref, someDatum(datum))
                    )
                  )
              )
          ),
          rejectionBudget,
          validator
        )
    }

    test("the script validates nothing but spending") {
        // Spending is the script info of tag 1. For a script info that is not a constructor the
        // premise's test fails, so its negation holds, and the validator fails as well.
        proven(
          forAll[Data, Data, Data]((txInfo, redeemer, scriptInfo) =>
              !Prop(unConstrData(scriptInfo).fst == BigInt(1)) ==>
                  fails(validator, scriptContext(txInfo, redeemer, scriptInfo))
          ),
          rejectionBudget,
          validator
        )
    }

    // Withdrawals

    /** The steps for a withdrawal: the validator runs to its end on one that it accepts. */
    private val withdrawalBudget = 12000

    test("a sample withdrawal is accepted on the Lean machine, and rejected unsigned") {
        // Closed statements: Lean decides them by running the validator.
        val accepted = succeeds(
          validator,
          withdrawal(
            BigInt(1000),
            BigInt(1000),
            BigInt(20_000_000),
            BigInt(20_000_000),
            BigInt(20_000_000),
            BigInt(2000),
            BigInt(200_000),
            signed,
            payment(BigInt(19_800_000))
          )
        )
        assert(proven(accepted, withdrawalBudget, validator) == ProofKind.LeanNative)
        val unsigned = fails(
          validator,
          withdrawal(
            BigInt(1000),
            BigInt(1000),
            BigInt(20_000_000),
            BigInt(20_000_000),
            BigInt(20_000_000),
            BigInt(2000),
            BigInt(200_000),
            prelude.List.Nil,
            payment(BigInt(19_800_000))
          )
        )
        assert(proven(unsigned, withdrawalBudget, validator) == ProofKind.LeanNative)
    }

    test("nobody withdraws without the beneficiary's signature") {
        // For every datum, amount and time, and whatever the outputs are. The validator fails
        // before it reads the outputs, so they can be any Data.
        proven(
          forAll[BigInt, BigInt, BigInt]((start, duration, amount) =>
              forAll[BigInt, BigInt, BigInt]((locked, requested, time) =>
                  forAll[BigInt, Data]((fee, outputs) =>
                      fails(
                        validator,
                        withdrawal(
                          start,
                          duration,
                          amount,
                          locked,
                          requested,
                          time,
                          fee,
                          prelude.List.Nil,
                          outputs.to[prelude.List[TxOut]]
                        )
                      )
                  )
              )
          ),
          withdrawalBudget,
          validator
        )
        // negative control: with the signature, the solver finds a withdrawal that is accepted
        refuted(
          forAll[BigInt, BigInt, BigInt]((start, duration, amount) =>
              forAll[BigInt, BigInt, BigInt]((locked, requested, time) =>
                  forAll[BigInt, BigInt]((fee, paid) =>
                      fails(
                        validator,
                        withdrawal(
                          start,
                          duration,
                          amount,
                          locked,
                          requested,
                          time,
                          fee,
                          signed,
                          payment(paid)
                        )
                      )
                  )
              )
          ),
          withdrawalBudget,
          validator
        )
    }

    test("what stays locked is at least what has not vested yet") {
        // A withdrawal that would leave less is rejected, even when the beneficiary signs it.
        proven(
          forAll[BigInt, BigInt, BigInt]((start, duration, amount) =>
              forAll[BigInt, BigInt, BigInt]((locked, requested, time) =>
                  forAll[BigInt, BigInt]((fee, paid) =>
                      (locked - requested < amount - VestingValidator.linearVesting(
                        config(start, duration, amount),
                        time
                      )) ==> fails(
                        validator,
                        withdrawal(
                          start,
                          duration,
                          amount,
                          locked,
                          requested,
                          time,
                          fee,
                          signed,
                          payment(paid)
                        )
                      )
                  )
              )
          ),
          withdrawalBudget,
          validator,
          linearVesting
        )
        // negative control: leaving exactly what has not vested is accepted
        refuted(
          forAll[BigInt, BigInt, BigInt]((start, duration, amount) =>
              forAll[BigInt, BigInt, BigInt]((locked, requested, time) =>
                  forAll[BigInt, BigInt]((fee, paid) =>
                      (locked - requested <= amount - VestingValidator.linearVesting(
                        config(start, duration, amount),
                        time
                      )) ==> fails(
                        validator,
                        withdrawal(
                          start,
                          duration,
                          amount,
                          locked,
                          requested,
                          time,
                          fee,
                          signed,
                          payment(paid)
                        )
                      )
                  )
              )
          ),
          withdrawalBudget,
          validator,
          linearVesting
        )
    }

    test("after the end of the period the beneficiary withdraws everything that is locked") {
        // The funds are not stuck: such a withdrawal, paid out less the fee, is always accepted.
        proven(
          forAll[BigInt, BigInt, BigInt]((start, duration, amount) =>
              forAll[BigInt, BigInt, BigInt]((locked, time, fee) =>
                  (duration >= BigInt(0) && start + duration <= time && locked > BigInt(0)
                      && locked <= amount) ==> succeeds(
                    validator,
                    withdrawal(
                      start,
                      duration,
                      amount,
                      locked,
                      locked,
                      time,
                      fee,
                      signed,
                      payment(locked - fee)
                    )
                  )
              )
          ),
          withdrawalBudget,
          validator
        )
        // negative control: before the end of the period not everything has vested
        refuted(
          forAll[BigInt, BigInt, BigInt]((start, duration, amount) =>
              forAll[BigInt, BigInt, BigInt]((locked, time, fee) =>
                  (duration >= BigInt(0) && locked > BigInt(0) && locked <= amount) ==> succeeds(
                    validator,
                    withdrawal(
                      start,
                      duration,
                      amount,
                      locked,
                      locked,
                      time,
                      fee,
                      signed,
                      payment(locked - fee)
                    )
                  )
              )
          ),
          withdrawalBudget,
          validator
        )
    }
}

/** The values the statements of [[VestingVerificationTest]] are about. They are `inline`, so that
  * each is part of the expression it is used in, which the Scalus plugin compiles. They live in an
  * object: an `inline def` of the test class that uses another one would leave a reference to the
  * test's `this` in that expression.
  */
object VestingVerificationTest {

    /** The beneficiary of every datum here. It is a constant: the tactic has no `ByteString`
      * variables yet.
      */
    inline def beneficiary: PubKeyHash =
        PubKeyHash(hex"11111111111111111111111111111111111111111111111111111111")

    inline def config(
        inline start: BigInt,
        inline duration: BigInt,
        inline amount: BigInt
    ): Config = Config(beneficiary, start, duration, amount)

    /** A script context, as the ledger encodes it, from the encodings of its three fields. */
    inline def scriptContext(
        inline txInfo: Data,
        inline redeemer: Data,
        inline scriptInfo: Data
    ): Data =
        constrData(0, mkCons(txInfo, mkCons(redeemer, mkCons(scriptInfo, mkNilData()))))

    /** The script info of spending `ref`, whose datum is the encoded `Option[Data]`. */
    inline def spendingInfo(inline ref: Data, inline datum: Data): Data =
        constrData(1, mkCons(ref, mkCons(datum, mkNilData())))

    inline def noDatum: Data = constrData(1, mkNilData())

    inline def someDatum(inline datum: Data): Data =
        constrData(0, mkCons(datum, mkNilData()))

    inline def vestingRef: TxOutRef =
        TxOutRef(TxId(hex"2222222222222222222222222222222222222222222222222222222222222222"), 0)

    /** The script context of a withdrawal of `requested` lovelace at `time`.
      *
      * The transaction's only input is the vesting output, which holds `locked` lovelace under the
      * datum `config(start, duration, amount)`. Its validity range starts at `time`.
      */
    inline def withdrawal(
        inline start: BigInt,
        inline duration: BigInt,
        inline amount: BigInt,
        inline locked: BigInt,
        inline requested: BigInt,
        inline time: BigInt,
        inline fee: BigInt,
        inline signatories: prelude.List[PubKeyHash],
        inline outputs: prelude.List[TxOut]
    ): Data =
        ScriptContext(
          TxInfo(
            inputs = prelude.List.Cons(
              TxInInfo(
                vestingRef,
                TxOut(
                  Address(
                    Credential.ScriptCredential(
                      hex"33333333333333333333333333333333333333333333333333333333"
                    ),
                    prelude.Option.None
                  ),
                  Value.lovelace(locked)
                )
              ),
              prelude.List.Nil
            ),
            outputs = outputs,
            fee = fee,
            validRange = Interval.after(time),
            signatories = signatories,
            id = TxId(hex"5555555555555555555555555555555555555555555555555555555555555555")
          ),
          Action(requested).toData,
          ScriptInfo.SpendingScript(
            vestingRef,
            prelude.Option.Some(config(start, duration, amount).toData)
          )
        ).toData

    /** The beneficiary's signature alone. */
    inline def signed: prelude.List[PubKeyHash] =
        prelude.List.Cons(beneficiary, prelude.List.Nil)

    /** One output, of `paid` lovelace to the beneficiary. */
    inline def payment(inline paid: BigInt): prelude.List[TxOut] = prelude.List.Cons(
      TxOut(
        Address(Credential.PubKeyCredential(beneficiary), prelude.Option.None),
        Value.lovelace(paid)
      ),
      prelude.List.Nil
    )
}
