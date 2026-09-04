package scalus.cardano.ledger

import scalus.cardano.address.Network
import scalus.interop.{TsName, TsType}

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}

/** The three Plutus cost models, by language rather than by position - so a caller reads
  * `costModels.PlutusV3`, never `Object.values(costModels)[2]` and a comment explaining why.
  */
@TsName("CostModels")
trait JsCostModels extends js.Object {
    val PlutusV1: js.Array[Double]
    val PlutusV2: js.Array[Double]
    val PlutusV3: js.Array[Double]
}

/** The structural form of [[JsProtocolParams]]. */
@TsName("PlainProtocolParams")
trait JsPlainProtocolParams extends js.Object {
    val txFeePerByte: Double
    val txFeeFixed: Double
    val maxTxSize: Double
    val maxValueSize: Double
    val stakeAddressDeposit: js.BigInt
    val stakePoolDeposit: js.BigInt
    val dRepDeposit: js.BigInt
    val govActionDeposit: js.BigInt
    val utxoCostPerByte: js.BigInt
    val priceMemory: Double
    val priceSteps: Double
    val maxTxExecutionMemory: js.BigInt
    val maxTxExecutionSteps: js.BigInt
    val collateralPercentage: Double
    val maxCollateralInputs: Double
    val minFeeRefScriptCostPerByte: Double
    val protocolMajorVersion: Double
    val costModels: JsCostModels
}

/** The structural form of [[JsCardanoInfo]]. */
@TsName("PlainCardanoInfo")
trait JsPlainCardanoInfo extends js.Object {
    val network: String
    val slotConfig: SlotConfig
    val protocolParams: JsPlainProtocolParams
}

/** The protocol parameters a transaction is built and validated against.
  *
  * These are the fields a transaction builder reads - the ones MeshJS's and the Evolution SDK's own
  * parameter types both carry. Fee rates, sizes, percentages and counts are `number`; every
  * deposit, `utxoCostPerByte` and the execution-unit maxima are `bigint`, because lovelace exceeds
  * `Number.MAX_SAFE_INTEGER`.
  *
  * Get them from `CardanoInfo.mainnet()`/`preprod()`/`preview()`/`custom()`, from an emulator's
  * `getProtocolParameters()`, or from `ProtocolParams.fromBlockfrostJson`. The public constructor
  * exists only because Scala.js requires one; it yields all-zero placeholder parameters and is not
  * a supported way to get real ones.
  */
// Implementation notes, deliberately NOT scaladoc - this file's scaladoc ships to npm as
// scalus.d.ts, where plan section numbers and Scala-side class names mean nothing to the reader.
//
// FIELD SUBSET. The shared/ `ProtocolParams` case class has 31 fields, most of which no JS adapter
// reads. What is exposed here is the subset the spec names, i.e. what mesh's `Protocol` and the
// Evolution SDK's `ProtocolParameters` both carry.
//
// BLOCKFROST JSON ONLY, DELIBERATELY. The Scala-side `ProtocolParams` also reads `cardano-cli query
// protocol-parameters` output, but that reader is not exported here: it is devops-tool output, not
// something a browser or Node client has lying around, and nothing in this codebase names a JS
// consumer for it the way MeshJS and the Evolution SDK are named consumers of the Blockfrost shape.
// Exporting it would also pull `CostModels`'s, `ExUnits`'s, `ProtocolVersion`'s, `UnitInterval`'s,
// `ExUnitPrices`'s and the voting thresholds' `macroRW` derivation into the JS bundle for a path
// nothing uses - see docs/internal/JS_BUNDLE_SIZE.md. Use `ProtocolParams.fromCardanoCliJson`
// directly from Scala if you need it there.
//
// WHY THE PUBLIC CONSTRUCTOR TAKES NOTHING. Unlike `JsValue`'s `lovelace` or `JsUtxo`'s raw fields,
// there is no primitive JS value a caller could hand this constructor that means anything. Scala.js
// still requires a public, non-overloaded constructor (see `JsValue`), so this one takes no
// arguments and the wrapped `var` starts at an all-zero placeholder that `wrap` overwrites. The
// placeholder is intentionally *zero*, not a real network's parameters: zero values make a
// hand-constructed instance obviously wrong rather than plausibly right.
@JSExportTopLevel("ProtocolParams")
class JsProtocolParams() extends js.Object {

    private var params: ProtocolParams = JsProtocolParams.zero

    def txFeePerByte: Double = params.txFeePerByte.toDouble
    def txFeeFixed: Double = params.txFeeFixed.toDouble
    def maxTxSize: Double = params.maxTxSize.toDouble
    def maxValueSize: Double = params.maxValueSize.toDouble

    def stakeAddressDeposit: js.BigInt = js.BigInt(params.stakeAddressDeposit.toString)
    def stakePoolDeposit: js.BigInt = js.BigInt(params.stakePoolDeposit.toString)
    def dRepDeposit: js.BigInt = js.BigInt(params.dRepDeposit.toString)
    def govActionDeposit: js.BigInt = js.BigInt(params.govActionDeposit.toString)
    def utxoCostPerByte: js.BigInt = js.BigInt(params.utxoCostPerByte.toString)

    def priceMemory: Double = params.executionUnitPrices.priceMemory.toDouble
    def priceSteps: Double = params.executionUnitPrices.priceSteps.toDouble

    def maxTxExecutionMemory: js.BigInt = js.BigInt(params.maxTxExecutionUnits.memory.toString)
    def maxTxExecutionSteps: js.BigInt = js.BigInt(params.maxTxExecutionUnits.steps.toString)

    def collateralPercentage: Double = params.collateralPercentage.toDouble
    def maxCollateralInputs: Double = params.maxCollateralInputs.toDouble
    def minFeeRefScriptCostPerByte: Double = params.minFeeRefScriptCostPerByte.toDouble
    def protocolMajorVersion: Double = params.protocolVersion.major.toDouble

    /** The three cost models, keyed by language rather than by position.
      *
      * A `def`, like every other accessor - it appears in the `.d.ts` as a readonly property, same
      * as `txFeePerByte` and the rest, because a parameterless Scala.js member always does.
      */
    def costModels: JsCostModels = {
        def modelOf(languageId: Int): js.Array[Double] =
            params.costModels.models
                .getOrElse(languageId, IndexedSeq.empty)
                .map(_.toDouble)
                .toJSArray

        js.Dynamic
            .literal(PlutusV1 = modelOf(0), PlutusV2 = modelOf(1), PlutusV3 = modelOf(2))
            .asInstanceOf[JsCostModels]
    }

    /** Render as Blockfrost's `/epochs/{n}/parameters` JSON - what MeshJS's and the Evolution SDK's
      * own Blockfrost adapters already know how to parse.
      */
    def toBlockfrostJson(): String = ProtocolParams.toBlockfrostJson(params)

    /** A plain object with the same fields.
      *
      * A handle's fields are accessors on the prototype, so `JSON.stringify`, object spread and
      * (the dangerous one) a test framework's `toEqual` all see an empty object on the handle
      * itself. Assert on this instead.
      */
    def toObject(): JsPlainProtocolParams = js.Dynamic
        .literal(
          txFeePerByte = txFeePerByte,
          txFeeFixed = txFeeFixed,
          maxTxSize = maxTxSize,
          maxValueSize = maxValueSize,
          stakeAddressDeposit = stakeAddressDeposit,
          stakePoolDeposit = stakePoolDeposit,
          dRepDeposit = dRepDeposit,
          govActionDeposit = govActionDeposit,
          utxoCostPerByte = utxoCostPerByte,
          priceMemory = priceMemory,
          priceSteps = priceSteps,
          maxTxExecutionMemory = maxTxExecutionMemory,
          maxTxExecutionSteps = maxTxExecutionSteps,
          collateralPercentage = collateralPercentage,
          maxCollateralInputs = maxCollateralInputs,
          minFeeRefScriptCostPerByte = minFeeRefScriptCostPerByte,
          protocolMajorVersion = protocolMajorVersion,
          costModels = costModels
        )
        .asInstanceOf[JsPlainProtocolParams]

    override def toString(): String = params.toString
}

object JsProtocolParams {

    /** The zero-valued placeholder the public constructor starts from. Built from the nested case
      * classes directly, not through `fromBlockfrostJson`, so merely referencing this class never
      * pulls upickle's derivation machinery into the bundle - see the `lazy val` comments on
      * `ProtocolParams.blockfrostParamsReadWriter`.
      *
      * `ProtocolVersion` requires `major >= 1`, so the placeholder is `(1, 0)`, not `(0, 0)`.
      */
    private[scalus] val zero: ProtocolParams = ProtocolParams(
      collateralPercentage = 0,
      committeeMaxTermLength = 0,
      committeeMinSize = 0,
      costModels = CostModels(Map.empty),
      dRepActivity = 0,
      dRepDeposit = 0,
      dRepVotingThresholds = DRepVotingThresholds(
        motionNoConfidence = UnitInterval.zero,
        committeeNormal = UnitInterval.zero,
        committeeNoConfidence = UnitInterval.zero,
        updateToConstitution = UnitInterval.zero,
        hardForkInitiation = UnitInterval.zero,
        ppNetworkGroup = UnitInterval.zero,
        ppEconomicGroup = UnitInterval.zero,
        ppTechnicalGroup = UnitInterval.zero,
        ppGovGroup = UnitInterval.zero,
        treasuryWithdrawal = UnitInterval.zero
      ),
      executionUnitPrices = ExUnitPrices(NonNegativeInterval.zero, NonNegativeInterval.zero),
      govActionDeposit = 0,
      govActionLifetime = 0,
      maxBlockBodySize = 0,
      maxBlockExecutionUnits = ExUnits.zero,
      maxBlockHeaderSize = 0,
      maxCollateralInputs = 0,
      maxTxExecutionUnits = ExUnits.zero,
      maxTxSize = 0,
      maxValueSize = 0,
      minFeeRefScriptCostPerByte = 0,
      minPoolCost = 0,
      monetaryExpansion = 0.0,
      poolPledgeInfluence = 0.0,
      poolRetireMaxEpoch = 0,
      poolVotingThresholds = PoolVotingThresholds(
        motionNoConfidence = UnitInterval.zero,
        committeeNormal = UnitInterval.zero,
        committeeNoConfidence = UnitInterval.zero,
        hardForkInitiation = UnitInterval.zero,
        ppSecurityGroup = UnitInterval.zero
      ),
      protocolVersion = ProtocolVersion(1, 0),
      stakeAddressDeposit = 0,
      stakePoolDeposit = 0,
      stakePoolTargetNum = 0,
      treasuryCut = 0.0,
      txFeeFixed = 0,
      txFeePerByte = 0,
      utxoCostPerByte = 0
    )

    /** Internal bridge: wrap a ledger value without copying. Not exported. */
    private[scalus] def wrap(params: ProtocolParams): JsProtocolParams = {
        val handle = new JsProtocolParams()
        handle.params = params
        handle
    }

    /** Internal bridge: the wrapped ledger value. Not exported - see `JsValue.underlying` for why
      * this is an extension method in the companion rather than a member of the class.
      */
    extension (self: JsProtocolParams) private[scalus] def underlying: ProtocolParams = self.params

    /** Reads protocol parameters from Blockfrost's `/epochs/{n}/parameters` JSON.
      *
      * The only JSON entry point exported to JavaScript. `cardano-cli query protocol-parameters`
      * output is readable from Scala but not from here: it is devops-tool output rather than
      * something a browser or Node client has to hand, and exporting the reader would pull the JSON
      * derivation for six more ledger types into the bundle for a path nothing uses.
      */
    @JSExportStatic
    def fromBlockfrostJson(json: String): JsProtocolParams = wrap(
      ProtocolParams.fromBlockfrostJson(json)
    )
}

/** Network, slot arithmetic and protocol parameters as one coherent triple.
  *
  * This is what an emulator is built from, because a network name alone cannot express what an
  * emulated network needs: slot configuration and protocol parameters vary independently of the
  * network id, and Yaci DevKit's custom `zeroTime`, `slotLength` and short `epochLength` are the
  * standing example.
  *
  * Use `mainnet()`, `preprod()`, `preview()` or `custom()`. The public constructor exists only
  * because Scala.js requires one; it yields an all-zero placeholder.
  */
// Implementation note, deliberately NOT scaladoc - see `JsProtocolParams`. `network` renders only
// `Network.Mainnet` and `Network.Testnet`. `Network.Other` has no meaning here (no cost models, no
// slot config, nothing behaves differently for it), so `custom` cannot construct it and the
// accessor throws rather than invent a third string for a value it can receive but never produce.
@JSExportTopLevel("CardanoInfo")
class JsCardanoInfo() extends js.Object {

    private var info: CardanoInfo = JsCardanoInfo.zero

    /** `"mainnet"` or `"testnet"`.
      *
      * Throws for a network id outside those two, which `custom` will not construct. The type is
      * exactly what `custom` accepts, so a value that cannot be named fails loudly rather than
      * degrading to a string no caller asked for.
      */
    @TsType("\"mainnet\" | \"testnet\"")
    def network: String = info.network match
        case Network.Mainnet => "mainnet"
        case Network.Testnet => "testnet"
        case Network.Other(v) =>
            throw new IllegalStateException(
              s"network id $v has no \"mainnet\"/\"testnet\" representation"
            )

    def slotConfig: SlotConfig = info.slotConfig

    def protocolParams: JsProtocolParams = JsProtocolParams.wrap(info.protocolParams)

    /** A copy carrying `params` in place of the current protocol parameters. Network and slot
      * config are unchanged.
      */
    def withProtocolParams(params: JsProtocolParams): JsCardanoInfo =
        JsCardanoInfo.wrap(info.copy(protocolParams = params.underlying))

    /** A plain object with the same fields.
      *
      * A handle's fields are accessors on the prototype, so `JSON.stringify`, object spread and
      * (the dangerous one) a test framework's `toEqual` all see an empty object on the handle
      * itself. Assert on this instead.
      */
    def toObject(): JsPlainCardanoInfo = js.Dynamic
        .literal(
          network = network,
          slotConfig = slotConfig,
          protocolParams = protocolParams.toObject()
        )
        .asInstanceOf[JsPlainCardanoInfo]

    override def toString(): String = info.toString
}

object JsCardanoInfo {

    /** The placeholder the public constructor starts from - a testnet with all-zero parameters, of
      * no use on its own. See [[JsProtocolParams.zero]] for why zero rather than a real network's
      * values.
      */
    private[scalus] val zero: CardanoInfo =
        CardanoInfo(JsProtocolParams.zero, Network.Testnet, new SlotConfig(0, 0, 1000))

    /** Internal bridge: wrap a ledger value without copying. Not exported. */
    private[scalus] def wrap(info: CardanoInfo): JsCardanoInfo = {
        val handle = new JsCardanoInfo()
        handle.info = info
        handle
    }

    /** Internal bridge: the wrapped ledger value. Not exported - see `JsValue.underlying`. */
    extension (self: JsCardanoInfo) private[scalus] def underlying: CardanoInfo = self.info

    /** Current Cardano mainnet parameters and slot configuration. */
    @JSExportStatic
    def mainnet(): JsCardanoInfo = wrap(CardanoInfo.mainnet)

    /** Preprod testnet parameters and slot configuration. */
    @JSExportStatic
    def preprod(): JsCardanoInfo = wrap(CardanoInfo.preprod)

    /** Preview testnet parameters and slot configuration. */
    @JSExportStatic
    def preview(): JsCardanoInfo = wrap(CardanoInfo.preview)

    /** Yaci DevKit, private testnets, or any other network with its own slot config and parameters.
      * `network` must be `"mainnet"` or `"testnet"`; anything else throws, since those are the only
      * two values `network` can ever read back (see the accessor's doc).
      */
    @JSExportStatic
    def custom(network: String, slotConfig: SlotConfig, params: JsProtocolParams): JsCardanoInfo =
        wrap(CardanoInfo(params.underlying, parseNetwork(network), slotConfig))

    private def parseNetwork(network: String): Network = network match
        case "mainnet" => Network.Mainnet
        case "testnet" => Network.Testnet
        case other =>
            throw new IllegalArgumentException(
              s"""network must be "mainnet" or "testnet", got: "$other""""
            )
}
