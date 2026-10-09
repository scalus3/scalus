package scalus.examples.midgard

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import scalus.ScalaCompilerVersion
import scalus.cardano.blueprint.Blueprint
import scalus.cardano.ledger.{CardanoInfo, ExUnits, RefScriptFee}
import scalus.cardano.onchain.plutus.prelude.{AssocMap, List, Option}
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.uplc.Program
import scalus.uplc.eval.PlutusVM
import scalus.uplc.builtin.Builtins.{blake2b_256, serialiseData}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData

import scala.collection.mutable

/** Aiken `da_bond_pool` compiled code, loaded from the vendored blueprint. */
object AikenDaBondPool {
    lazy val program: Program = {
        val resource = "/scalus/examples/midgard/da_bond_pool.plutus.json"
        val in = getClass.getResourceAsStream(resource)
        if in == null then throw new RuntimeException(s"Resource not found: $resource")
        try Blueprint.fromJson(in).validators.head.compiledCode.map(Program.fromCborHex).get
        finally in.close()
    }

    /** Applies the four Aiken parameters in declaration order. */
    def apply(p: DaBondPoolParams): Program = DaBondPoolContract.applyParams(program, p)
}

/** The 68 handler tests of Midgard `validators/da-bond-pool.test.ak`, run on the Aiken script.
  *
  * Each test builds one `ScriptContext` with the same fixture and the same single mutation as its
  * Aiken twin. The test name drops the `da_bond_pool_` prefix.
  */
class DaBondPoolDifferentialTest extends AnyFunSuite with BeforeAndAfterAll {
    import DaBondPoolConstants.*
    import DaBondPoolFixtures.*

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    private lazy val aiken = AikenDaBondPool(params)
    private lazy val scalus =
        DaBondPoolContract.applyParams(DaBondPoolContract.compiled.program, params)

    /** The same validator with error traces: only to read which guard rejected a context. */
    private lazy val scalusTraced =
        DaBondPoolContract.applyParams(DaBondPoolContract.compiled.withErrorTraces.program, params)

    /** `List.at` message for an index past the end, as Aiken's `list.at` returns `None`. */
    private val ListAtOutOfBounds = "Index out of bounds in List.at"

    /** Aiken and Scalus ExUnits of every case that succeeds, in run order. */
    protected val budgets: mutable.LinkedHashMap[String, (ExUnits, ExUnits)] =
        mutable.LinkedHashMap.empty

    private def check(name: String, ctx: ScriptContext, expectSuccess: Boolean): Unit = {
        val aikenResult = (aiken $ ctx.toData).evaluateDebug
        val scalusResult = (scalus $ ctx.toData).evaluateDebug
        assert(
          aikenResult.isSuccess == expectSuccess,
          s"Aiken $name: expected success=$expectSuccess, got $aikenResult"
        )
        assert(
          scalusResult.isSuccess == expectSuccess,
          s"Scalus $name: expected success=$expectSuccess, got $scalusResult"
        )
        if expectSuccess then
            budgets(name) = (aikenResult.budget, scalusResult.budget)
            assert(scalusResult.budget == scalusPins(name), s"Scalus $name budget")
    }

    /** Exact Scalus ExUnits of every accepted case (`scalus:contract-test` pin convention). */
    private val scalusPins: Map[String, ExUnits] = Map(
      "init_accepts_honest_pool" -> ExUnits(memory = 50711, steps = 22_525575),
      "top_up_accepts_honest_bonded" -> ExUnits(memory = 65975, steps = 31_410473),
      "top_up_accepts_honest_withdrawing" -> ExUnits(memory = 65975, steps = 31_546868),
      "top_up_accepts_exact_minimum" -> ExUnits(memory = 65975, steps = 31_410473),
      "slash_accepts_honest_bonded" -> ScalaCompilerVersion.baseline(
        pre38 = ExUnits(memory = 132537, steps = 58_156509),
        since38 = ExUnits(memory = 130677, steps = 57_491822)
      ),
      "slash_accepts_withdrawing_and_keeps_unlock_at" -> ScalaCompilerVersion.baseline(
        pre38 = ExUnits(memory = 132537, steps = 58_292904),
        since38 = ExUnits(memory = 130677, steps = 57_628217)
      ),
      "slash_takes_exactly_the_backing_when_short" -> ScalaCompilerVersion.baseline(
        pre38 = ExUnits(memory = 132537, steps = 58_156509),
        since38 = ExUnits(memory = 130677, steps = 57_491822)
      ),
      "slash_takes_zero_below_the_floor (at the floor)" -> ScalaCompilerVersion.baseline(
        pre38 = ExUnits(memory = 132537, steps = 58_156509),
        since38 = ExUnits(memory = 130677, steps = 57_491822)
      ),
      "slash_takes_zero_below_the_floor (below the floor)" -> ScalaCompilerVersion.baseline(
        pre38 = ExUnits(memory = 132537, steps = 58_156509),
        since38 = ExUnits(memory = 130677, steps = 57_491822)
      ),
      "begin_withdraw_accepts_honest" -> ExUnits(memory = 115905, steps = 52_278376),
      "cancel_withdraw_accepts_honest" -> ExUnits(memory = 105594, steps = 48_212407),
      "complete_withdraw_accepts_honest" -> ExUnits(memory = 117150, steps = 52_132631),
      "complete_withdraw_accepts_at_unlock_boundary" -> ExUnits(memory = 117150, steps = 52_132631),
      "complete_withdraw_accepts_the_whole_backing" -> ExUnits(memory = 117150, steps = 52_132631)
    )

    /** Prints a Markdown table: per honest case, both budgets and the per-tx script cost (ExUnits
      * fee plus the reference-script fee of the applied script).
      */
    override def afterAll(): Unit = {
        val pp = CardanoInfo.mainnet.protocolParams
        val refScriptFee = new RefScriptFee(pp)
        val aikenSize = aiken.cborByteString.size
        val scalusSize = scalus.cborByteString.size
        def exFee(units: ExUnits): Long = units.fee(pp.executionUnitPrices).value
        def cost(units: ExUnits, size: Int): Long =
            exFee(units) + refScriptFee.calculate(size).value
        val lines = mutable.ArrayBuffer(
          s"Applied script size: Aiken $aikenSize B, Scalus $scalusSize B",
          s"Reference-script fee: Aiken ${refScriptFee.calculate(aikenSize).value}, " +
              s"Scalus ${refScriptFee.calculate(scalusSize).value} lovelace",
          "",
          "| case | Aiken mem | Scalus mem | mem ratio | Aiken ExUnits fee | Scalus ExUnits fee | Aiken cost | Scalus cost | cost ratio |",
          "|---|---:|---:|---:|---:|---:|---:|---:|---:|"
        )
        for (name, (a, s)) <- budgets do
            val (aFee, sFee) = (exFee(a), exFee(s))
            val (aCost, sCost) = (cost(a, aikenSize), cost(s, scalusSize))
            lines += f"| $name | ${a.memory}%,d | ${s.memory}%,d | ${s.memory.toDouble / a.memory}%.2f | " +
                f"$aFee%,d | $sFee%,d | $aCost%,d | $sCost%,d | ${sCost.toDouble / aCost}%.2f |"
        println(lines.mkString("\n"))
        super.afterAll()
    }

    private def accepts(name: String)(ctx: => ScriptContext): Unit =
        test(name)(check(name, ctx, expectSuccess = true))

    /** Both scripts reject `ctx`, and the traced Scalus build fails on the guard named `reason`. A
      * rejection for any other reason fails the test, so a broken fixture cannot pass as a guard.
      */
    private def rejects(name: String, reason: String)(ctx: => ScriptContext): Unit =
        test(name) {
            val context = ctx
            check(name, context, expectSuccess = false)
            val traced = (scalusTraced $ context.toData).evaluateDebug
            assert(
              traced.logs.exists(_.contains(reason)),
              s"Scalus $name: expected failure '$reason', got logs ${traced.logs}"
            )
        }

    // ------------------------------------------------------------------------
    // mint / InitPool
    // ------------------------------------------------------------------------

    accepts("init_accepts_honest_pool")(mintContext(honestInitTx))

    rejects("init_rejects_burning_the_nft", DaBondPoolValidator.MintExactlyOneNft)(
      mintContext(honestInitTx.copy(mint = asset(poolPolicy, poolAssetName, -1)))
    )

    rejects("init_rejects_minting_two_nfts", DaBondPoolValidator.MintExactlyOneNft)(
      mintContext(honestInitTx.copy(mint = asset(poolPolicy, poolAssetName, 2)))
    )

    rejects("init_rejects_a_second_asset_name", DaBondPoolValidator.MintExactlyOneNft)(
      mintContext(
        honestInitTx.copy(mint =
            poolNft + asset(poolPolicy, ByteString.fromString("MIDGARD_DA_BOND_POOL_2"), 1)
        )
      )
    )

    rejects("init_rejects_without_spending_init_ref", DaBondPoolValidator.InitRefNotSpent)(
      mintContext(honestInitTx.copy(inputs = List(walletInput(9))))
    )

    rejects("init_rejects_datum_hash", DaBondPoolValidator.InitDatumNotBonded)(
      mintContext(
        withPoolOutput(
          honestInitTx,
          initOutput(poolFloor).copy(datum =
              OutputDatum.OutputDatumHash(blake2b_256(ByteString.fromHex("00")))
          )
        )
      )
    )

    rejects("init_rejects_reference_script", DaBondPoolValidator.ReferenceScriptForbidden)(
      mintContext(
        withPoolOutput(
          honestInitTx,
          initOutput(poolFloor).copy(referenceScript = Option.Some(foreignPolicy))
        )
      )
    )

    rejects("init_rejects_stake_credential", DaBondPoolValidator.InitAddressMismatch)(
      mintContext(
        withPoolOutput(
          honestInitTx,
          initOutput(poolFloor).copy(address = stakedAt(poolAddress, owner1))
        )
      )
    )

    rejects("init_rejects_other_address", DaBondPoolValidator.InitAddressMismatch)(
      mintContext(
        withPoolOutput(
          honestInitTx,
          initOutput(poolFloor).copy(address = scriptAddress(foreignPolicy))
        )
      )
    )

    rejects("init_rejects_wrong_datum", DaBondPoolValidator.InitDatumNotBonded)(
      mintContext(
        withPoolOutput(
          honestInitTx,
          initOutput(poolFloor).copy(datum = OutputDatum.OutputDatum(withdrawing(0)))
        )
      )
    )

    rejects("init_rejects_another_token", DaBondPoolValidator.PoolValueNotExact)(
      mintContext(
        withPoolOutput(
          honestInitTx,
          initOutput(poolFloor).copy(value =
              poolValue(poolFloor) + asset(foreignPolicy, ByteString.fromString("x"), 1)
          )
        )
      )
    )

    rejects("init_rejects_below_the_floor", DaBondPoolValidator.BelowFloor)(
      mintContext(withPoolOutput(honestInitTx, initOutput(poolFloor - 1)))
    )

    // ------------------------------------------------------------------------
    // spend / TopUp
    // ------------------------------------------------------------------------

    private val increase = BigInt(50_000_000)
    private val toppedUp = poolLovelace + increase

    accepts("top_up_accepts_honest_bonded")(spendContext(topUp, honestTopUpTx(bonded, increase)))

    accepts("top_up_accepts_honest_withdrawing")(
      spendContext(topUp, honestTopUpTx(withdrawing(withdrawingUnlockAt), increase))
    )

    accepts("top_up_accepts_exact_minimum")(spendContext(topUp, honestTopUpTx(bonded, minTopUp)))

    rejects("top_up_rejects_below_minimum", DaBondPoolValidator.TopUpTooSmall)(
      spendContext(topUp, honestTopUpTx(bonded, minTopUp - 1))
    )

    rejects("top_up_rejects_non_ada_token", DaBondPoolValidator.PoolValueNotExact)(
      spendContext(
        topUp,
        withPoolOutput(
          honestTopUpTx(bonded, increase),
          poolOutput(bonded, toppedUp).copy(value =
              poolValue(toppedUp) + asset(foreignPolicy, ByteString.fromString("x"), 1)
          )
        )
      )
    )

    rejects("top_up_rejects_changed_datum", DaBondPoolValidator.DatumChanged)(
      spendContext(
        topUp,
        withPoolOutput(
          honestTopUpTx(bonded, increase),
          poolOutput(withdrawing(withdrawingUnlockAt), toppedUp)
        )
      )
    )

    rejects("top_up_rejects_moved_address", DaBondPoolValidator.PoolAddressChanged)(
      spendContext(
        topUp,
        withPoolOutput(
          honestTopUpTx(bonded, increase),
          poolOutput(bonded, toppedUp).copy(address = scriptAddress(foreignPolicy))
        )
      )
    )

    rejects("top_up_rejects_datum_hash", DaBondPoolValidator.DatumChanged)(
      spendContext(
        topUp,
        withPoolOutput(
          honestTopUpTx(bonded, increase),
          poolOutput(bonded, toppedUp).copy(datum =
              OutputDatum.OutputDatumHash(blake2b_256(serialiseData(bonded)))
          )
        )
      )
    )

    // ------------------------------------------------------------------------
    // spend / common
    // ------------------------------------------------------------------------

    rejects("spend_rejects_a_second_pool_input", DaBondPoolValidator.SecondPoolInput) {
        val honest = honestTopUpTx(bonded, increase)
        val stray = TxInInfo(
          outref(7, 0),
          TxOut(
            poolAddress,
            Value.lovelace(2_000_000),
            OutputDatum.OutputDatum(bonded),
            Option.None
          )
        )
        spendContext(topUp, honest.copy(inputs = List.Cons(stray, honest.inputs)))
    }

    rejects("else_rejects_other_purpose", DaBondPoolValidator.PurposeNotAllowed)(
      ScriptContext(
        honestTopUpTx(bonded, increase),
        topUp.toData,
        ScriptInfo.RewardingScript(Credential.ScriptCredential(poolPolicy))
      )
    )

    // ------------------------------------------------------------------------
    // spend / Slash
    // ------------------------------------------------------------------------

    private val slashOutLovelace = poolLovelace - daBond

    accepts("slash_accepts_honest_bonded")(spendContext(slash, honestBondedSlashTx))

    accepts("slash_accepts_withdrawing_and_keeps_unlock_at")(
      spendContext(
        slash,
        honestSlashTx(withdrawing(withdrawingUnlockAt), poolLovelace, slashOutLovelace)
      )
    )

    accepts("slash_takes_exactly_the_backing_when_short")(
      spendContext(slash, honestSlashTx(bonded, poolFloor + 300_000_000, poolFloor))
    )

    // The Aiken test runs two contexts under one `and`.
    accepts("slash_takes_zero_below_the_floor (at the floor)")(
      spendContext(slash, honestSlashTx(bonded, poolFloor, poolFloor))
    )

    accepts("slash_takes_zero_below_the_floor (below the floor)")(
      spendContext(slash, honestSlashTx(bonded, poolFloor - 1_000_000, poolFloor - 1_000_000))
    )

    rejects("slash_rejects_unauthentic_hub", DaBondPoolValidator.HubNotAuthentic)(
      spendContext(
        slash,
        honestBondedSlashTx.copy(referenceInputs = List(hubRefInput(foreignPolicy)))
      )
    )

    rejects("slash_rejects_without_state_queue_redeemer", ListAtOutOfBounds)(
      spendContext(
        slash,
        honestBondedSlashTx.copy(redeemers =
            AssocMap.fromList(List((ScriptPurpose.Spending(poolRef), slash.toData)))
        )
      )
    )

    rejects(
      "slash_rejects_remove_redeemer_under_a_foreign_policy",
      DaBondPoolValidator.RedeemerPurposeMismatch
    )(
      spendContext(
        slash,
        honestBondedSlashTx.copy(redeemers =
            AssocMap.fromList(
              List(
                (ScriptPurpose.Spending(poolRef), slash.toData),
                (ScriptPurpose.Minting(foreignPolicy), removeUnavailableRedeemer)
              )
            )
        )
      )
    )

    rejects("slash_rejects_other_state_queue_redeemer", DaBondPoolValidator.NotUnavailableRemoval)(
      spendContext(
        slash,
        honestBondedSlashTx.copy(redeemers =
            AssocMap.fromList(
              List(
                (ScriptPurpose.Spending(poolRef), slash.toData),
                (ScriptPurpose.Minting(stateQueuePolicy), removeUnattestedRedeemer)
              )
            )
        )
      )
    )

    rejects(
      "slash_rejects_resume_step_locked_correction_lock",
      DaBondPoolValidator.CorrectionLockNotIdle
    )(
      spendContext(
        slash,
        honestBondedSlashTx.copy(inputs =
            List(correctionLockInput(lockLocked), poolInput(bonded, poolLovelace))
        )
      )
    )

    rejects("slash_rejects_without_correction_lock_input", DaBondPoolValidator.NotCorrectionLock)(
      spendContext(slash, honestBondedSlashTx.copy(inputs = List(poolInput(bonded, poolLovelace))))
    )

    rejects("slash_rejects_idle_datum_without_lock_token", DaBondPoolValidator.NotCorrectionLock) {
        val counterfeit = correctionLockInput(lockIdle)
        val stripped =
            counterfeit.copy(resolved =
                counterfeit.resolved.copy(value = Value.lovelace(2_000_000))
            )
        spendContext(
          slash,
          honestBondedSlashTx.copy(inputs = List(stripped, poolInput(bonded, poolLovelace)))
        )
    }

    rejects("slash_rejects_changed_datum", DaBondPoolValidator.DatumChanged)(
      spendContext(
        slash,
        withPoolOutput(
          honestBondedSlashTx,
          poolOutput(withdrawing(withdrawingUnlockAt), slashOutLovelace)
        )
      )
    )

    rejects("slash_rejects_changed_stake_credential", DaBondPoolValidator.PoolAddressChanged)(
      spendContext(
        slash,
        withPoolOutput(
          honestBondedSlashTx,
          poolOutput(bonded, slashOutLovelace).copy(address = stakedAt(poolAddress, owner1))
        )
      )
    )

    rejects("slash_rejects_reference_script", DaBondPoolValidator.ReferenceScriptForbidden)(
      spendContext(
        slash,
        withPoolOutput(
          honestBondedSlashTx,
          poolOutput(bonded, slashOutLovelace).copy(referenceScript = Option.Some(foreignPolicy))
        )
      )
    )

    rejects("slash_rejects_dropped_nft", DaBondPoolValidator.PoolValueNotExact)(
      spendContext(
        slash,
        withPoolOutput(
          honestBondedSlashTx,
          poolOutput(bonded, slashOutLovelace).copy(value = Value.lovelace(slashOutLovelace))
        )
      )
    )

    rejects("slash_rejects_taking_more", DaBondPoolValidator.SlashAmountWrong)(
      spendContext(slash, honestSlashTx(bonded, poolLovelace, slashOutLovelace - 1))
    )

    rejects("slash_rejects_taking_less", DaBondPoolValidator.SlashAmountWrong)(
      spendContext(slash, honestSlashTx(bonded, poolLovelace, slashOutLovelace + 1))
    )

    // ------------------------------------------------------------------------
    // spend / BeginWithdraw
    // ------------------------------------------------------------------------

    accepts("begin_withdraw_accepts_honest")(spendContext(begin, honestBeginTx))

    rejects("begin_withdraw_rejects_below_threshold", DaBondPoolValidator.QuorumNotMet)(
      spendContext(begin, withSigners(honestBeginTx, List(owner1)))
    )

    rejects("begin_withdraw_rejects_non_owner_signers", DaBondPoolValidator.QuorumNotMet)(
      spendContext(begin, withSigners(honestBeginTx, List(nonOwner1, nonOwner2)))
    )

    rejects("begin_withdraw_rejects_params_without_nft", DaBondPoolValidator.ParamsNoNft)(
      spendContext(begin, paramsWithoutNft(honestBeginTx))
    )

    rejects(
      "begin_withdraw_rejects_params_at_other_credential",
      DaBondPoolValidator.ParamsWrongCredential
    )(
      spendContext(begin, paramsAtOtherCredential(honestBeginTx))
    )

    rejects("begin_withdraw_rejects_from_withdrawing", DaBondPoolValidator.BeginNeedsBonded)(
      spendContext(
        begin,
        quorumTx(
          withdrawing(withdrawingUnlockAt),
          withdrawing(beginUnlockAt),
          poolLovelace,
          beginRange
        )
      )
    )

    rejects("begin_withdraw_rejects_wrong_unlock_at", DaBondPoolValidator.UnlockAtWrong)(
      spendContext(
        begin,
        quorumTx(bonded, withdrawing(beginUnlockAt - 1), poolLovelace, beginRange)
      )
    )

    rejects("begin_withdraw_rejects_changed_value", DaBondPoolValidator.LovelaceChanged)(
      spendContext(
        begin,
        quorumTx(bonded, withdrawing(beginUnlockAt), poolLovelace - 1, beginRange)
      )
    )

    // ------------------------------------------------------------------------
    // spend / CancelWithdraw
    // ------------------------------------------------------------------------

    accepts("cancel_withdraw_accepts_honest")(spendContext(cancelWithdraw, honestCancelTx))

    rejects("cancel_withdraw_rejects_below_threshold", DaBondPoolValidator.QuorumNotMet)(
      spendContext(cancelWithdraw, withSigners(honestCancelTx, List(owner2)))
    )

    rejects("cancel_withdraw_rejects_non_owner_signers", DaBondPoolValidator.QuorumNotMet)(
      spendContext(cancelWithdraw, withSigners(honestCancelTx, List(nonOwner1, nonOwner2)))
    )

    rejects("cancel_withdraw_rejects_params_without_nft", DaBondPoolValidator.ParamsNoNft)(
      spendContext(cancelWithdraw, paramsWithoutNft(honestCancelTx))
    )

    rejects(
      "cancel_withdraw_rejects_params_at_other_credential",
      DaBondPoolValidator.ParamsWrongCredential
    )(
      spendContext(cancelWithdraw, paramsAtOtherCredential(honestCancelTx))
    )

    rejects("cancel_withdraw_rejects_from_bonded", DaBondPoolValidator.CancelNeedsWithdrawing)(
      spendContext(cancelWithdraw, quorumTx(bonded, bonded, poolLovelace, Interval.always))
    )

    rejects("cancel_withdraw_rejects_output_not_bonded", DaBondPoolValidator.OutputNotBonded)(
      spendContext(
        cancelWithdraw,
        quorumTx(withdrawing(withdrawingUnlockAt), withdrawing(0), poolLovelace, Interval.always)
      )
    )

    rejects("cancel_withdraw_rejects_changed_value", DaBondPoolValidator.LovelaceChanged)(
      spendContext(
        cancelWithdraw,
        quorumTx(withdrawing(withdrawingUnlockAt), bonded, poolLovelace + 1, Interval.always)
      )
    )

    // ------------------------------------------------------------------------
    // spend / CompleteWithdraw
    // ------------------------------------------------------------------------

    private val completeOut = poolLovelace - withdrawAmount

    accepts("complete_withdraw_accepts_honest")(
      spendContext(complete(withdrawAmount), honestCompleteTx)
    )

    accepts("complete_withdraw_accepts_at_unlock_boundary")(
      spendContext(
        complete(withdrawAmount),
        completeTx(withdrawing(withdrawingUnlockAt), bonded, completeOut, withdrawingUnlockAt)
      )
    )

    accepts("complete_withdraw_accepts_the_whole_backing")(
      spendContext(
        complete(poolLovelace - poolFloor),
        completeTx(withdrawing(withdrawingUnlockAt), bonded, poolFloor, withdrawingUnlockAt)
      )
    )

    rejects("complete_withdraw_rejects_below_threshold", DaBondPoolValidator.QuorumNotMet)(
      spendContext(complete(withdrawAmount), withSigners(honestCompleteTx, List(owner3)))
    )

    rejects("complete_withdraw_rejects_non_owner_signers", DaBondPoolValidator.QuorumNotMet)(
      spendContext(
        complete(withdrawAmount),
        withSigners(honestCompleteTx, List(nonOwner1, nonOwner2))
      )
    )

    rejects("complete_withdraw_rejects_params_without_nft", DaBondPoolValidator.ParamsNoNft)(
      spendContext(complete(withdrawAmount), paramsWithoutNft(honestCompleteTx))
    )

    rejects(
      "complete_withdraw_rejects_params_at_other_credential",
      DaBondPoolValidator.ParamsWrongCredential
    )(
      spendContext(complete(withdrawAmount), paramsAtOtherCredential(honestCompleteTx))
    )

    rejects("complete_withdraw_rejects_before_unlock", DaBondPoolValidator.StillLocked)(
      spendContext(
        complete(withdrawAmount),
        completeTx(withdrawing(withdrawingUnlockAt), bonded, completeOut, withdrawingUnlockAt - 1)
      )
    )

    rejects("complete_withdraw_rejects_more_than_backing", DaBondPoolValidator.AmountAboveBacking) {
        val amount = poolLovelace - poolFloor + 1
        spendContext(
          complete(amount),
          completeTx(
            withdrawing(withdrawingUnlockAt),
            bonded,
            poolLovelace - amount,
            withdrawingUnlockAt
          )
        )
    }

    rejects("complete_withdraw_rejects_zero", DaBondPoolValidator.AmountNotPositive)(
      spendContext(
        complete(0),
        completeTx(withdrawing(withdrawingUnlockAt), bonded, poolLovelace, withdrawingUnlockAt)
      )
    )

    rejects("complete_withdraw_rejects_from_bonded", DaBondPoolValidator.CompleteNeedsWithdrawing)(
      spendContext(
        complete(withdrawAmount),
        completeTx(bonded, bonded, completeOut, withdrawingUnlockAt)
      )
    )

    rejects("complete_withdraw_rejects_output_not_bonded", DaBondPoolValidator.OutputNotBonded)(
      spendContext(
        complete(withdrawAmount),
        completeTx(
          withdrawing(withdrawingUnlockAt),
          withdrawing(withdrawingUnlockAt),
          completeOut,
          withdrawingUnlockAt
        )
      )
    )

    rejects(
      "complete_withdraw_rejects_output_not_input_minus_amount",
      DaBondPoolValidator.WithdrawAmountWrong
    )(
      spendContext(
        complete(withdrawAmount),
        completeTx(withdrawing(withdrawingUnlockAt), bonded, completeOut - 1, withdrawingUnlockAt)
      )
    )

    // ------------------------------------------------------------------------
    // Extra cases, not in the Aiken suite: one per safety check the
    // `scalus:contract-test` negative-test convention asks for. They run on both scripts too.
    // ------------------------------------------------------------------------

    rejects("extra_begin_withdraw_rejects_unbounded_range", DaBondPoolValidator.RangeNotShort)(
      spendContext(
        begin,
        quorumTx(bonded, withdrawing(beginUnlockAt), poolLovelace, Interval.always)
      )
    )

    rejects("extra_begin_withdraw_rejects_range_over_480_s", DaBondPoolValidator.RangeNotShort)(
      spendContext(
        begin,
        quorumTx(
          bonded,
          withdrawing(beginUnlockAt),
          poolLovelace,
          ledgerRange(beginLower, beginUpper + 1)
        )
      )
    )

    rejects(
      "extra_complete_withdraw_rejects_unbounded_range",
      DaBondPoolValidator.RangeNoLowerBound
    )(
      spendContext(
        complete(withdrawAmount),
        quorumTx(withdrawing(withdrawingUnlockAt), bonded, completeOut, Interval.always)
      )
    )

    rejects("extra_spend_rejects_pool_input_with_two_nfts", DaBondPoolValidator.PoolInputNoNft) {
        val honest = honestTopUpTx(bonded, increase)
        val doubled = TxInInfo(
          poolRef,
          poolOutput(bonded, poolLovelace).copy(value =
              Value.lovelace(poolLovelace) + asset(poolPolicy, poolAssetName, 2)
          )
        )
        spendContext(topUp, honest.copy(inputs = List(doubled, walletInput(9))))
    }

    // `spend_rejects_a_second_pool_input` runs the pool's spend. The stray input's own run must
    // fail too: a second own input fails in every script run.
    rejects("extra_spend_rejects_the_stray_inputs_own_run", DaBondPoolValidator.PoolInputNoNft) {
        val honest = honestTopUpTx(bonded, increase)
        val strayRef = outref(7, 0)
        val stray = TxInInfo(
          strayRef,
          TxOut(
            poolAddress,
            Value.lovelace(2_000_000),
            OutputDatum.OutputDatum(bonded),
            Option.None
          )
        )
        ScriptContext(
          honest.copy(inputs = List.Cons(stray, honest.inputs)),
          topUp.toData,
          ScriptInfo.SpendingScript(strayRef, Option.None)
        )
    }
}
