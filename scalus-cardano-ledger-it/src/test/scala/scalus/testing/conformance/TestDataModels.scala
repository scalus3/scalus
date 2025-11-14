package scalus.testing.conformance

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import scalus.builtin.ByteString
import scalus.utils.Hex.*
import scala.annotation.targetName

/** Models for parsing Amaru test data JSON files
  *
  * This includes both epoch-level data (pools, rewards, governance)
  * and transaction/block-level data for ledger rules conformance testing.
  */
object TestDataModels:

    /** Helper type for nested lovelace amounts: {"ada": {"lovelace": amount}} */
    case class Lovelace(lovelace: BigInt)
    case class Ada(ada: Lovelace)

    /** Configuration for a network's test data */
    case class Config(
        points: List[Point],
        snapshots: List[Int],
        additionalStakeAddresses: Option[List[String]]
    )

    case class Point(
        epoch: Int,
        slot: Long,
        id: String
    )

    /** Stake pool information for an epoch */
    case class PoolData(
                           id: String,
                           vrfVerificationKeyHash: Option[String],
                           pledge: Ada,
                           cost: Ada,
                           margin: String, // Format: "numerator/denominator"
                           rewardAccount: String,
                           owners: List[String],
                           relays: List[Relay],
                           metadata: Option[PoolMetadata],
                           stake: Option[Ada]
    )

    case class Relay(
        `type`: String,
        hostname: Option[String],
        port: Option[Int],
        ipv4: Option[String],
        ipv6: Option[String]
    )

    case class PoolMetadata(
        url: String,
        hash: String
    )

    /** Rewards provenance data for an epoch */
    case class RewardsProvenance(
                                    totalStake: Ada,
                                    activeStake: Ada,
                                    fees: Ada,
                                    incentives: Ada,
                                    treasuryTax: Ada,
                                    totalRewards: Ada,
                                    efficiency: String, // Format: "numerator/denominator"
                                    stakePools: Map[String, StakePoolRewards]
    )

    case class StakePoolRewards(
                                   relativeStake: String, // Format: "numerator/denominator"
                                   blocksMade: Int,
                                   totalRewards: Ada,
                                   leaderReward: Ada,
                                   delegators: List[Delegator]
    )

    case class Delegator(
        from: String, // "verificationKey" or "script"
        credential: String,
        stake: Ada
    )

    /** DRep (Delegated Representative) data for governance */
    case class DRepData(
                           `type`: String, // "registered", "abstain", "no_confidence"
                           from: Option[String], // "verificationKey" or "script" (only for registered)
                           id: Option[String], // ID (only for registered DReps)
                           mandate: Option[Mandate],
                           deposit: Option[Ada], // Only for registered DReps
                           stake: Ada,
                           metadata: Option[Anchor],
                           delegators: List[DRepDelegator]
    )

    case class Mandate(
        epoch: Int
    )

    case class DRepDelegator(
        from: String,
        credential: String
    )

    case class Anchor(
        url: String,
        hash: String
    )

    /** Treasury and reserve pot balances */
    case class PotsData(
                           treasury: Ada,
                           reserves: Ada
    )

    // jsoniter-scala codecs
    given JsonValueCodec[Lovelace] = JsonCodecMaker.make
    given JsonValueCodec[Ada] = JsonCodecMaker.make

    given JsonValueCodec[Config] = JsonCodecMaker.make
    given JsonValueCodec[Point] = JsonCodecMaker.make

    given JsonValueCodec[PoolData] = JsonCodecMaker.make
    given JsonValueCodec[Relay] = JsonCodecMaker.make
    given JsonValueCodec[PoolMetadata] = JsonCodecMaker.make

    given JsonValueCodec[Delegator] = JsonCodecMaker.make
    given JsonValueCodec[StakePoolRewards] = JsonCodecMaker.make
    given JsonValueCodec[RewardsProvenance] = JsonCodecMaker.make

    given JsonValueCodec[Mandate] = JsonCodecMaker.make
    given JsonValueCodec[DRepDelegator] = JsonCodecMaker.make
    given JsonValueCodec[DRepData] = JsonCodecMaker.make
    given JsonValueCodec[Anchor] = JsonCodecMaker.make

    given JsonValueCodec[PotsData] = JsonCodecMaker.make

    // Collection codecs
    given JsonValueCodec[Map[String, PoolData]] = JsonCodecMaker.make
    @targetName("drepDataListCodec")
    given JsonValueCodec[List[DRepData]] = JsonCodecMaker.make

    /** Helper to parse rational from string "num/denom" */
    def parseRational(s: String): (BigInt, BigInt) =
        val parts = s.split("/")
        if parts.length == 2 then
            (BigInt(parts(0)), BigInt(parts(1)))
        else
            (BigInt(parts(0)), BigInt(1))

    /** Helper to extract lovelace amount */
    def extractLovelace(wrapper: Ada): BigInt = wrapper.ada.lovelace

    // ========================================================================
    // Transaction and Block Level Test Data Models
    // ========================================================================

    /** Transaction input reference */
    case class TransactionInput(
        transaction_id: String,
        index: Int
    )

    /** Script reference - can be PlutusV1, PlutusV2, PlutusV3, or NativeScript */
    enum ScriptType:
        case PlutusV1(hex: String)
        case PlutusV2(hex: String)
        case PlutusV3(hex: String)
        case NativeScript(hex: String)

    /** Datum reference - can be inline Data or hash */
    enum DatumType:
        case Data(hex: String)
        case Hash(hex: String)

    /** Transaction output */
    case class TransactionOutput(
        address: String,
        value: Long,
        datum: Option[DatumType],
        script: Option[ScriptType]
    )

    /** UTXO entry - a pair of input and output */
    type UtxoEntry = (TransactionInput, TransactionOutput)

    /** Required script information */
    case class RequiredScript(
        hash: String,
        index: Int,
        purpose: String,
        datum: Option[DatumType]
    )

    /** Test context containing UTXO set and required witnesses */
    case class TestContext(
        utxo: List[UtxoEntry],
        required_signers: Option[List[String]] = None,
        required_scripts: Option[List[RequiredScript]] = None,
        known_scripts: Option[Map[String, TransactionInput]] = None,
        known_datums: Option[Map[String, TransactionInput]] = None,
        required_supplemental_datums: Option[List[String]] = None,
        required_bootstrap_roots: Option[List[String]] = None
    )

    /** Expected trace event for transaction/block validation */
    case class TraceEvent(
        name: String,
        hash: Option[String] = None,
        position: Option[Int] = None
    )

    /** Test fixture containing all test data for a transaction */
    case class TransactionTestFixture(
        context: TestContext,
        txCbor: Array[Byte],
        witnessCbor: Option[Array[Byte]],
        expectedTraces: List[TraceEvent]
    )

    // jsoniter-scala codecs for transaction/block test data
    // Custom codec for ScriptType to match Amaru format: {"PlutusV2": "hex"}
    given JsonValueCodec[ScriptType] = new JsonValueCodec[ScriptType]:
        def decodeValue(in: JsonReader, default: ScriptType): ScriptType =
            if in.isNextToken('{') then
                val key = in.readKeyAsString()
                val hex = in.readString(null)
                if in.isCurrentToken(',') then in.skip()
                if !in.isCurrentToken('}') then in.nextToken()
                key match
                    case "PlutusV1" => ScriptType.PlutusV1(hex)
                    case "PlutusV2" => ScriptType.PlutusV2(hex)
                    case "PlutusV3" => ScriptType.PlutusV3(hex)
                    case "NativeScript" => ScriptType.NativeScript(hex)
                    case _ => in.decodeError(s"Unknown script type: $key")
            else in.decodeError("Expected object")

        def encodeValue(x: ScriptType, out: JsonWriter): Unit =
            out.writeObjectStart()
            x match
                case ScriptType.PlutusV1(hex) =>
                    out.writeKey("PlutusV1")
                    out.writeVal(hex)
                case ScriptType.PlutusV2(hex) =>
                    out.writeKey("PlutusV2")
                    out.writeVal(hex)
                case ScriptType.PlutusV3(hex) =>
                    out.writeKey("PlutusV3")
                    out.writeVal(hex)
                case ScriptType.NativeScript(hex) =>
                    out.writeKey("NativeScript")
                    out.writeVal(hex)
            out.writeObjectEnd()

        def nullValue: ScriptType = null.asInstanceOf[ScriptType]

    // Custom codec for DatumType to match Amaru format: {"Data": "hex"}
    given JsonValueCodec[DatumType] = new JsonValueCodec[DatumType]:
        def decodeValue(in: JsonReader, default: DatumType): DatumType =
            if in.isNextToken('{') then
                val key = in.readKeyAsString()
                val hex = in.readString(null)
                if in.isCurrentToken(',') then in.skip()
                if !in.isCurrentToken('}') then in.nextToken()
                key match
                    case "Data" => DatumType.Data(hex)
                    case "Hash" => DatumType.Hash(hex)
                    case _ => in.decodeError(s"Unknown datum type: $key")
            else in.decodeError("Expected object")

        def encodeValue(x: DatumType, out: JsonWriter): Unit =
            out.writeObjectStart()
            x match
                case DatumType.Data(hex) =>
                    out.writeKey("Data")
                    out.writeVal(hex)
                case DatumType.Hash(hex) =>
                    out.writeKey("Hash")
                    out.writeVal(hex)
            out.writeObjectEnd()

        def nullValue: DatumType = null.asInstanceOf[DatumType]

    given JsonValueCodec[TransactionInput] = JsonCodecMaker.make
    given JsonValueCodec[TransactionOutput] = JsonCodecMaker.make
    given JsonValueCodec[UtxoEntry] = JsonCodecMaker.make
    given JsonValueCodec[RequiredScript] = JsonCodecMaker.make
    given JsonValueCodec[TestContext] = JsonCodecMaker.make
    given JsonValueCodec[TraceEvent] = JsonCodecMaker.make
    @targetName("traceEventListCodec")
    given JsonValueCodec[List[TraceEvent]] = JsonCodecMaker.make

    /** Helper to convert hex string to ByteString */
    def hexToByteString(hexStr: String): ByteString =
        ByteString.fromHex(hexStr)

    /** Helper to parse address bytes from hex string */
    def parseAddressHex(hexStr: String): Array[Byte] =
        hexStr.hexToBytes
