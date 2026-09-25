package scalus.uplc.eval

import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.JsProtocolParams.underlying
import scalus.cardano.txbuilder.{Change, SomeBuildError, TransactionBuilder, TxBalancingError}
import scalus.interop.{TsName, TsType}
import scalus.utils.scalajs.internal.*

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.scalajs.js.typedarray.Uint8Array

/** Balancing a transaction: the fee, the execution units and one change output, to a fixpoint. */
@JSExportTopLevel("balancer")
object JBalancer {
    bindExports(this)

    /** Balances a transaction you have already built: sets every redeemer's execution units, the
      * fee, and the lovelace of one change output, until the three agree.
      *
      * EXPERIMENTAL. This may change shape or be removed in any release, including a patch one. It
      * is published to be used and reported on, not to be depended upon yet.
      *
      * Coin selection and change placement stay with you. This only settles the numbers, so your
      * own policy for choosing inputs and splitting change is untouched: name the output that
      * should absorb the difference, and its lovelace is the only field edited.
      *
      * Why it is a loop. A script sees the whole transaction, so adding change can change what the
      * script costs, which changes the fee, which changes the change. Each pass evaluates the
      * scripts, writes the redeemers and the script data hash, prices the transaction, moves the
      * difference into the change output, and starts again; it returns when a pass changes nothing.
      * Dummy signatures are added before the fee is priced and removed afterwards, so the fee pays
      * for the witnesses the transaction will really carry.
      *
      * The scripts are limited together by the protocol's `maxTxExecutionMemory` and
      * `maxTxExecutionSteps`, exactly as a node limits them, so a transaction whose scripts do not
      * fit fails here instead of being rejected after you submit it.
      *
      * ```ts
      * import { balancer, CardanoInfo } from "scalus";
      *
      * const info = CardanoInfo.preprod();
      * const utxos = await provider.fetchAddressUTxOs(myAddress); // or Utxo handles, or CBOR pairs
      *
      * // `1` is the index of the change output in the transaction you built.
      * const balanced = balancer.balanceTx(
      *   unbalancedTxCbor, utxos, info.slotConfig, info.protocolParams, 1, [],
      * );
      * ```
      *
      * `extraSigners` is the keys that will sign but that the transaction does not name. Pass `[]`
      * when there are none, which is the usual case.
      *
      * The signers are otherwise worked out for you: every input and collateral input paying to a
      * key address needs that key's signature, and so does anything in the transaction's own
      * required-signers field. Reference inputs are read-only and need none. That matters because
      * each signature is about 101 bytes, so a fee priced for the wrong number of them is wrong by
      * roughly 4,400 lovelace each.
      *
      * What cannot be worked out is a native script. Its input sits at a script address, so the
      * keys its `ScriptAll` or `ScriptAny` requires are invisible from the transaction, and a fee
      * priced without them is short one witness per key and rejected. Name them here:
      *
      * ```ts
      * // A 2-of-2 native script input: neither key can be inferred from the script address.
      * const balanced = balancer.balanceTx(
      *   tx, utxos, info.slotConfig, info.protocolParams, 1,
      *   ["1c7f...28-byte-key-hash", "9ab3...28-byte-key-hash"],
      * );
      * ```
      *
      * These are added to the inferred signers, never used instead of them, because an inferred
      * signer is required whatever you pass. A hash given twice counts once.
      *
      * @param tx
      *   the transaction to balance, as hex or CBOR bytes
      * @param utxos
      *   every UTxO the transaction's inputs, collateral inputs and reference inputs name. Each is
      *   a `Utxo`, or a CIP-30 `[input, output]` pair as hex or bytes
      * @param slotConfig
      *   slot arithmetic for the network, so a script sees the validity interval as POSIX times
      * @param params
      *   the protocol parameters, either a `ProtocolParams` or a plain record of the same fields.
      *   Fees, min-ada, collateral, the cost models and the protocol version all come from here
      * @param changeOutputIndex
      *   which output of the transaction absorbs the difference, counting from 0
      * @param extraSigners
      *   hex key hashes, 28 bytes each, for signatures the transaction does not name. `[]` for none
      * @return
      *   the balanced transaction, as CBOR bytes. Sign these bytes; changing them afterwards
      *   invalidates the fee
      * @throws TypeError
      *   if an argument cannot be read, or `changeOutputIndex` is not an output of the transaction
      * @throws PlutusScriptEvaluationError
      *   if one of the transaction's scripts fails, or together they exceed the protocol's maximum
      * @throws TxBalancingError
      *   with `code: "INSUFFICIENT_FUNDS"` if the change output cannot hold min-ada, or
      *   `code: "NOT_CONVERGED"` if the numbers never settle
      */
    @JSExport
    def balanceTx(
        @TsType("string | Uint8Array") tx: js.Any,
        @TsType("readonly (string | Uint8Array | Utxo)[]") utxos: js.Any,
        slotConfig: JSlotConfigLike,
        params: JProtocolParamsLike,
        changeOutputIndex: Double,
        @TsType("readonly string[]") extraSigners: js.Any
    ): Uint8Array = surfacingErrors {
        val transaction = decodeOf(tx, "tx")(Transaction.fromCbor(_))
        val resolved = JEvaluator.utxoMapOf(utxos)
        val slots = slotConfig.asInstanceOf[js.Dynamic]
        val protocolParams = paramsOf(params, "params")
        val info = CardanoInfo(
          protocolParams,
          // Balancing never reads the network. Measured: the same parameters declared mainnet
          // and declared preview balance to the same fee. It prices bytes and execution units,
          // and `Change.changeOutputDiffHandler` only edits an output already there.
          Network.Testnet,
          SlotConfig(
            zeroTime = longOf(slots.zeroTime, "slotConfig.zeroTime"),
            zeroSlot = longOf(slots.zeroSlot, "slotConfig.zeroSlot"),
            slotLength = intOf(slots.slotLength, "slotConfig.slotLength")
          )
        )

        val body = transaction.body.value
        val changeIndex = intOf(changeOutputIndex, "changeOutputIndex")
        if changeIndex < 0 || changeIndex >= body.outputs.size then
            typeError(
              s"changeOutputIndex must be an output of the transaction, " +
                  s"which has ${body.outputs.size}; got $changeIndex"
            )

        // The evaluator needs reference inputs resolved; only the witnessed ones imply a signature.
        val witnessed = body.inputs.toSet ++ body.collateralInputs.toSet
        val extras =
            if js.isUndefined(extraSigners) || extraSigners == null then Seq.empty
            else
                arrayOf(extraSigners, "extraSigners").map { (value, name) =>
                    if js.typeOf(value) != "string" then
                        typeError(s"$name must be a hexadecimal key hash")
                    try AddrKeyHash.fromHex(value.asInstanceOf[String])
                    catch case e: IllegalArgumentException => typeError(s"$name: ${e.getMessage}")
                }
        val expectedSigners = witnessed.toSeq
            .flatMap(resolved.get)
            .map(_.address)
            .collect { case ShelleyAddress(_, ShelleyPaymentPart.Key(hash), _) => hash }
            .toSet ++ body.requiredSigners.toSet ++ extras

        // This overload sets initialBudget to maxTxExecutionUnits, as TxBuilder does, so the
        // transaction's scripts are limited together exactly as the ledger limits them.
        val evaluator = PlutusScriptEvaluator(info, EvaluatorMode.EvaluateAndComputeCost)

        val context = TransactionBuilder.Context(
          transaction = transaction,
          redeemers = Seq.empty,
          network = info.network,
          expectedSigners = expectedSigners,
          resolvedUtxos = TransactionBuilder.ResolvedUtxos(resolved)
        )

        context.balanceContext(
          protocolParams,
          Change.changeOutputDiffHandler(_, _, protocolParams, changeIndex),
          evaluator
        ) match
            case Right(balanced) => balanced.transaction.toCbor.toUint8Array
            case Left(error)     => throw balancingFailure(error)
    }

    /** The JavaScript error for a balancing failure, with the script failure passed through
      * unchanged: a failing script is the evaluator's error, not the balancer's.
      */
    private def balancingFailure(error: Any): js.JavaScriptException = {
        val balancing = error match
            case SomeBuildError.BalancingError(inner, _) => Some(inner)
            case inner: TxBalancingError                 => Some(inner)
            case _                                       => None
        balancing match
            case Some(TxBalancingError.EvaluationFailed(cause)) =>
                js.JavaScriptException(JPlutusScriptEvaluationError.fromException(cause))
            // The enum names this `minRequired`, but `Change.changeOutputDiffHandler` passes
            // `minAda - coin`: it is the shortfall, not the requirement.
            case Some(TxBalancingError.InsufficientFunds(_, shortfall)) =>
                js.JavaScriptException(
                  JTxBalancingError(
                    s"the change output is $shortfall lovelace short of min-ada",
                    "INSUFFICIENT_FUNDS"
                  )
                )
            case Some(TxBalancingError.BalanceDidNotConverge(iterations)) =>
                js.JavaScriptException(
                  JTxBalancingError(
                    s"the fee, execution units and change did not settle in $iterations iterations",
                    "NOT_CONVERGED"
                  )
                )
            case _ => js.JavaScriptException(js.Error(s"balanceTx failed: $error"))
    }

    /** A `ProtocolParams` from the handle, or from any record carrying the fields balancing reads.
      *
      * The handle satisfies the record structurally, so both work. Fields the record does not carry
      * stay at their zero: balancing reads fees, min-ada, collateral, the cost models and the
      * protocol version, never a governance, block or pool parameter.
      */
    private def paramsOf(value: js.Any, name: String): ProtocolParams =
        if value.isInstanceOf[JsProtocolParams] then value.asInstanceOf[JsProtocolParams].underlying
        else {
            if js.isUndefined(value) || value == null then
                typeError(s"$name must be a ProtocolParams or a plain record")
            val p = value.asInstanceOf[js.Dynamic]
            def long(field: String): Long = longOf(p.selectDynamic(field), s"$name.$field")
            def double(field: String): Double =
                safeInteger(p.selectDynamic(field), s"$name.$field")
            val models = p.selectDynamic("costModels")
            val byLanguage = for
                language <- Seq(Language.PlutusV1, Language.PlutusV2, Language.PlutusV3)
                model = models.selectDynamic(language.toString)
                if !js.isUndefined(model) && model != null
            yield language.ordinal -> arrayOf(model, s"$name.costModels.$language").map(longOf)
            JsProtocolParams.zero.copy(
              collateralPercentage = long("collateralPercentage"),
              costModels = CostModels(byLanguage.toMap),
              dRepDeposit = long("dRepDeposit"),
              executionUnitPrices = ExUnitPrices(
                priceMemory = NonNegativeInterval(
                  priceOf(p, s"$name.priceMemory", "priceMemory"),
                  precision = 15
                ),
                priceSteps = NonNegativeInterval(
                  priceOf(p, s"$name.priceSteps", "priceSteps"),
                  precision = 15
                )
              ),
              govActionDeposit = long("govActionDeposit"),
              maxCollateralInputs = long("maxCollateralInputs"),
              maxTxExecutionUnits = ExUnits(
                memory = long("maxTxExecutionMemory"),
                steps = long("maxTxExecutionSteps")
              ),
              maxTxSize = long("maxTxSize"),
              maxValueSize = long("maxValueSize"),
              minFeeRefScriptCostPerByte = long("minFeeRefScriptCostPerByte"),
              protocolVersion = ProtocolVersion(long("protocolMajorVersion").toInt, 0),
              stakeAddressDeposit = long("stakeAddressDeposit"),
              stakePoolDeposit = long("stakePoolDeposit"),
              txFeeFixed = long("txFeeFixed"),
              txFeePerByte = long("txFeePerByte"),
              utxoCostPerByte = long("utxoCostPerByte")
            )
        }

    /** A price is a ratio, so it is the one field that is not an integer. */
    private def priceOf(p: js.Dynamic, name: String, field: String): Double = {
        val value = p.selectDynamic(field)
        if js.typeOf(value) != "number" then typeError(s"$name must be a number")
        val price = value.asInstanceOf[Double]
        if price.isNaN || price < 0 then typeError(s"$name must be a non-negative number")
        price
    }
}

/** The protocol parameters balancing reads, as a record any SDK can build from numbers it already
  * holds. A `ProtocolParams` handle satisfies it, and so does the `PlainProtocolParams` its
  * `toObject()` returns, so all three work.
  *
  * Every integer takes a `number` or a `bigint`: lovelace and execution units exceed 2^53 in
  * principle, but the values an SDK carries are usually plain numbers. The two prices are ratios,
  * so they are always `number`.
  *
  * Fields absent from this record stay at zero inside Scalus. Balancing reads fees, min-ada,
  * collateral, the cost models and the protocol version, and never a governance, block or pool
  * parameter.
  */
@TsName("ProtocolParamsLike")
trait JProtocolParamsLike extends js.Object {
    @TsType("number | bigint") val txFeePerByte: js.Any
    @TsType("number | bigint") val txFeeFixed: js.Any
    @TsType("number | bigint") val maxTxSize: js.Any
    @TsType("number | bigint") val maxValueSize: js.Any
    @TsType("number | bigint") val stakeAddressDeposit: js.Any
    @TsType("number | bigint") val stakePoolDeposit: js.Any
    @TsType("number | bigint") val dRepDeposit: js.Any
    @TsType("number | bigint") val govActionDeposit: js.Any
    @TsType("number | bigint") val utxoCostPerByte: js.Any
    val priceMemory: Double
    val priceSteps: Double
    @TsType("number | bigint") val maxTxExecutionMemory: js.Any
    @TsType("number | bigint") val maxTxExecutionSteps: js.Any
    @TsType("number | bigint") val collateralPercentage: js.Any
    @TsType("number | bigint") val maxCollateralInputs: js.Any
    @TsType("number | bigint") val minFeeRefScriptCostPerByte: js.Any
    @TsType("number | bigint") val protocolMajorVersion: js.Any
    @TsType("CostModelsLike") val costModels: js.Any
}

/** Thrown by `balancer.balanceTx` when the transaction cannot be balanced. A script that fails is
  * not this: it throws `PlutusScriptEvaluationError`, because that is the evaluator's failure.
  *
  * Branch on `code`, not on the message.
  *
  * @param code
  *   `INSUFFICIENT_FUNDS` when the change output cannot hold min-ada, `NOT_CONVERGED` when the fee,
  *   the execution units and the change do not settle.
  */
@JSExportTopLevel("TxBalancingError")
class JTxBalancingError(
    message: String,
    @TsType("\"INSUFFICIENT_FUNDS\" | \"NOT_CONVERGED\"") val code: String
) extends js.Error(message) {
    override val name: String = "TxBalancingError"
}
