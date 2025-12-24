package scalus.cardano.ledger

//import io.bullet.borer.NullOptions.given
import io.bullet.borer.{Decoder, Encoder, Reader, Writer}
import scalus.builtin.{BuiltinList, BuiltinPair, Data, FromData, ToData}
import scalus.builtin.Builtins.*

/** Represents a protocol parameter update in the Cardano blockchain.
  *
  * Protocol parameter updates are proposals to change various parameters that govern the behavior
  * of the Cardano blockchain.
  */
case class ProtocolParamUpdate(
    // Basic parameters
    minFeeA: Option[Coin] = None,
    minFeeB: Option[Coin] = None,
    maxBlockBodySize: Option[Int] = None,
    maxTxSize: Option[Int] = None,
    maxBlockHeaderSize: Option[Int] = None,
    keyDeposit: Option[Coin] = None,
    poolDeposit: Option[Coin] = None,
    maxEpoch: Option[Int] = None,
    nOpt: Option[Int] = None,
    poolPledgeInfluence: Option[NonNegativeInterval] = None,
    expansionRate: Option[UnitInterval] = None,
    treasuryGrowthRate: Option[UnitInterval] = None,

    // Advanced parameters
    minPoolCost: Option[Coin] = None,
    adaPerUtxoByte: Option[Coin] = None,
    costModels: Option[CostModels] = None,
    executionCosts: Option[ExUnitPrices] = None,
    maxTxExUnits: Option[ExUnits] = None,
    maxBlockExUnits: Option[ExUnits] = None,
    maxValueSize: Option[Int] = None,
    collateralPercentage: Option[Int] = None,
    maxCollateralInputs: Option[Int] = None,

    // Governance parameters
    poolVotingThresholds: Option[PoolVotingThresholds] = None,
    drepVotingThresholds: Option[DRepVotingThresholds] = None,
    minCommitteeSize: Option[Int] = None,
    committeeTermLimit: Option[Int] = None,
    governanceActionValidityPeriod: Option[Int] = None,
    governanceActionDeposit: Option[Coin] = None,
    drepDeposit: Option[Coin] = None,
    drepInactivityPeriod: Option[Int] = None,

    // Reference scripts
    minFeeRefScriptCoinsPerByte: Option[NonNegativeInterval] = None
)

object ProtocolParamUpdate {

    /** CBOR Encoder for ProtocolParamUpdate. Encodes as a map with integer keys and corresponding
      * values for each set parameter.
      */
    given Encoder[ProtocolParamUpdate] = (w: Writer, value: ProtocolParamUpdate) => {
        // Calculate map size (count non-None fields)
        var mapSize = 0
        if value.minFeeA.isDefined then mapSize += 1
        if value.minFeeB.isDefined then mapSize += 1
        if value.maxBlockBodySize.isDefined then mapSize += 1
        if value.maxTxSize.isDefined then mapSize += 1
        if value.maxBlockHeaderSize.isDefined then mapSize += 1
        if value.keyDeposit.isDefined then mapSize += 1
        if value.poolDeposit.isDefined then mapSize += 1
        if value.maxEpoch.isDefined then mapSize += 1
        if value.nOpt.isDefined then mapSize += 1
        if value.poolPledgeInfluence.isDefined then mapSize += 1
        if value.expansionRate.isDefined then mapSize += 1
        if value.treasuryGrowthRate.isDefined then mapSize += 1
        if value.minPoolCost.isDefined then mapSize += 1
        if value.adaPerUtxoByte.isDefined then mapSize += 1
        if value.costModels.isDefined then mapSize += 1
        if value.executionCosts.isDefined then mapSize += 1
        if value.maxTxExUnits.isDefined then mapSize += 1
        if value.maxBlockExUnits.isDefined then mapSize += 1
        if value.maxValueSize.isDefined then mapSize += 1
        if value.collateralPercentage.isDefined then mapSize += 1
        if value.maxCollateralInputs.isDefined then mapSize += 1
        if value.poolVotingThresholds.isDefined then mapSize += 1
        if value.drepVotingThresholds.isDefined then mapSize += 1
        if value.minCommitteeSize.isDefined then mapSize += 1
        if value.committeeTermLimit.isDefined then mapSize += 1
        if value.governanceActionValidityPeriod.isDefined then mapSize += 1
        if value.governanceActionDeposit.isDefined then mapSize += 1
        if value.drepDeposit.isDefined then mapSize += 1
        if value.drepInactivityPeriod.isDefined then mapSize += 1
        if value.minFeeRefScriptCoinsPerByte.isDefined then mapSize += 1

        w.writeMapOpen(mapSize)

        // Write basic parameters
        value.minFeeA.foreach { v => w.writeInt(0).write(v) }
        value.minFeeB.foreach { v => w.writeInt(1).write(v) }
        value.maxBlockBodySize.foreach { v => w.writeInt(2).writeInt(v) }
        value.maxTxSize.foreach { v => w.writeInt(3).writeInt(v) }
        value.maxBlockHeaderSize.foreach { v => w.writeInt(4).writeInt(v) }
        value.keyDeposit.foreach { v => w.writeInt(5).write(v) }
        value.poolDeposit.foreach { v => w.writeInt(6).write(v) }
        value.maxEpoch.foreach { v => w.writeInt(7).writeInt(v) }
        value.nOpt.foreach { v => w.writeInt(8).writeInt(v) }
        value.poolPledgeInfluence.foreach { v => w.writeInt(9).write(v) }
        value.expansionRate.foreach { v => w.writeInt(10).write(v) }
        value.treasuryGrowthRate.foreach { v => w.writeInt(11).write(v) }

        // Write advanced parameters
        value.minPoolCost.foreach { v => w.writeInt(16).write(v) }
        value.adaPerUtxoByte.foreach { v => w.writeInt(17).write(v) }
        value.costModels.foreach { v => w.writeInt(18).write(v) }
        value.executionCosts.foreach { v => w.writeInt(19).write(v) }
        value.maxTxExUnits.foreach { v => w.writeInt(20).write(v) }
        value.maxBlockExUnits.foreach { v => w.writeInt(21).write(v) }
        value.maxValueSize.foreach { v => w.writeInt(22).writeInt(v) }
        value.collateralPercentage.foreach { v => w.writeInt(23).writeInt(v) }
        value.maxCollateralInputs.foreach { v => w.writeInt(24).writeInt(v) }

        // Write governance parameters
        value.poolVotingThresholds.foreach { v => w.writeInt(25).write(v) }
        value.drepVotingThresholds.foreach { v => w.writeInt(26).write(v) }
        value.minCommitteeSize.foreach { v => w.writeInt(27).writeInt(v) }
        value.committeeTermLimit.foreach { v => w.writeInt(28).writeInt(v) }
        value.governanceActionValidityPeriod.foreach { v => w.writeInt(29).writeInt(v) }
        value.governanceActionDeposit.foreach { v => w.writeInt(30).write(v) }
        value.drepDeposit.foreach { v => w.writeInt(31).write(v) }
        value.drepInactivityPeriod.foreach { v => w.writeInt(32).writeInt(v) }

        // Write reference scripts parameters
        value.minFeeRefScriptCoinsPerByte.foreach { v => w.writeInt(33).write(v) }

        w.writeMapClose()
    }

    /** CBOR Decoder for ProtocolParamUpdate. Decodes from a map with integer keys and corresponding
      * values.
      */
    given Decoder[ProtocolParamUpdate] = (r: Reader) => {
        val mapSize = r.readMapHeader()

        // Initialize all fields
        var minFeeA: Option[Coin] = None
        var minFeeB: Option[Coin] = None
        var maxBlockBodySize: Option[Int] = None
        var maxTxSize: Option[Int] = None
        var maxBlockHeaderSize: Option[Int] = None
        var keyDeposit: Option[Coin] = None
        var poolDeposit: Option[Coin] = None
        var maxEpoch: Option[Int] = None
        var nOpt: Option[Int] = None
        var poolPledgeInfluence: Option[NonNegativeInterval] = None
        var expansionRate: Option[UnitInterval] = None
        var treasuryGrowthRate: Option[UnitInterval] = None
        var minPoolCost: Option[Coin] = None
        var adaPerUtxoByte: Option[Coin] = None
        var costModels: Option[CostModels] = None
        var executionCosts: Option[ExUnitPrices] = None
        var maxTxExUnits: Option[ExUnits] = None
        var maxBlockExUnits: Option[ExUnits] = None
        var maxValueSize: Option[Int] = None
        var collateralPercentage: Option[Int] = None
        var maxCollateralInputs: Option[Int] = None
        var poolVotingThresholds: Option[PoolVotingThresholds] = None
        var drepVotingThresholds: Option[DRepVotingThresholds] = None
        var minCommitteeSize: Option[Int] = None
        var committeeTermLimit: Option[Int] = None
        var governanceActionValidityPeriod: Option[Int] = None
        var governanceActionDeposit: Option[Coin] = None
        var drepDeposit: Option[Coin] = None
        var drepInactivityPeriod: Option[Int] = None
        var minFeeRefScriptCoinsPerByte: Option[NonNegativeInterval] = None

        // Read all fields based on their keys
        for _ <- 0L until mapSize do {
            val key = r.readInt()

            key match {
                // Basic parameters
                case 0  => minFeeA = Some(r.read[Coin]())
                case 1  => minFeeB = Some(r.read[Coin]())
                case 2  => maxBlockBodySize = Some(r.readInt())
                case 3  => maxTxSize = Some(r.readInt())
                case 4  => maxBlockHeaderSize = Some(r.readInt())
                case 5  => keyDeposit = Some(r.read[Coin]())
                case 6  => poolDeposit = Some(r.read[Coin]())
                case 7  => maxEpoch = Some(r.readInt())
                case 8  => nOpt = Some(r.readInt())
                case 9  => poolPledgeInfluence = Some(r.read[NonNegativeInterval]())
                case 10 => expansionRate = Some(r.read[UnitInterval]())
                case 11 => treasuryGrowthRate = Some(r.read[UnitInterval]())

                // Advanced parameters
                case 16 => minPoolCost = Some(r.read[Coin]())
                case 17 => adaPerUtxoByte = Some(r.read[Coin]())
                case 18 => costModels = Some(r.read[CostModels]())
                case 19 => executionCosts = Some(r.read[ExUnitPrices]())
                case 20 => maxTxExUnits = Some(r.read[ExUnits]())
                case 21 => maxBlockExUnits = Some(r.read[ExUnits]())
                case 22 => maxValueSize = Some(r.readInt())
                case 23 => collateralPercentage = Some(r.readInt())
                case 24 => maxCollateralInputs = Some(r.readInt())

                // Governance parameters
                case 25 => poolVotingThresholds = Some(r.read[PoolVotingThresholds]())
                case 26 => drepVotingThresholds = Some(r.read[DRepVotingThresholds]())
                case 27 => minCommitteeSize = Some(r.readInt())
                case 28 => committeeTermLimit = Some(r.readInt())
                case 29 => governanceActionValidityPeriod = Some(r.readInt())
                case 30 => governanceActionDeposit = Some(r.read[Coin]())
                case 31 => drepDeposit = Some(r.read[Coin]())
                case 32 => drepInactivityPeriod = Some(r.readInt())

                // Reference scripts parameters
                case 33 => minFeeRefScriptCoinsPerByte = Some(r.read[NonNegativeInterval]())

                case _ => r.skipElement() // Skip unknown fields
            }
        }

        // Construct ProtocolParamUpdate with all fields
        ProtocolParamUpdate(
          minFeeA = minFeeA,
          minFeeB = minFeeB,
          maxBlockBodySize = maxBlockBodySize,
          maxTxSize = maxTxSize,
          maxBlockHeaderSize = maxBlockHeaderSize,
          keyDeposit = keyDeposit,
          poolDeposit = poolDeposit,
          maxEpoch = maxEpoch,
          nOpt = nOpt,
          poolPledgeInfluence = poolPledgeInfluence,
          expansionRate = expansionRate,
          treasuryGrowthRate = treasuryGrowthRate,
          minPoolCost = minPoolCost,
          adaPerUtxoByte = adaPerUtxoByte,
          costModels = costModels,
          executionCosts = executionCosts,
          maxTxExUnits = maxTxExUnits,
          maxBlockExUnits = maxBlockExUnits,
          maxValueSize = maxValueSize,
          collateralPercentage = collateralPercentage,
          maxCollateralInputs = maxCollateralInputs,
          poolVotingThresholds = poolVotingThresholds,
          drepVotingThresholds = drepVotingThresholds,
          minCommitteeSize = minCommitteeSize,
          committeeTermLimit = committeeTermLimit,
          governanceActionValidityPeriod = governanceActionValidityPeriod,
          governanceActionDeposit = governanceActionDeposit,
          drepDeposit = drepDeposit,
          drepInactivityPeriod = drepInactivityPeriod,
          minFeeRefScriptCoinsPerByte = minFeeRefScriptCoinsPerByte
        )
    }

    /** Converts ProtocolParamUpdate to Plutus Data representation.
      *
      * According to the Cardano ledger specification, protocol parameter updates are represented as
      * a Data.Map with integer keys (0-33) mapping to their respective parameter values. Only
      * non-None fields are included in the map.
      *
      * The mapping follows the CDDL specification: key -> Int key 0 -> minFeeA key 1 -> minFeeB key
      * 2 -> maxBlockBodySize ... key 33 -> minFeeRefScriptCoinsPerByte
      */
    given ToData[ProtocolParamUpdate] = (value: ProtocolParamUpdate) => {
        val pairs = scala.collection.mutable.ArrayBuffer.empty[BuiltinPair[Data, Data]]

        // Helper to add a parameter to the map
        def addParam[A: ToData](key: Int, opt: Option[A]): Unit = {
            opt.foreach { v =>
                pairs += BuiltinPair(iData(key), summon[ToData[A]].apply(v))
            }
        }

        // Basic parameters (keys 0-11)
        addParam(0, value.minFeeA)
        addParam(1, value.minFeeB)
        addParam(2, value.maxBlockBodySize)
        addParam(3, value.maxTxSize)
        addParam(4, value.maxBlockHeaderSize)
        addParam(5, value.keyDeposit)
        addParam(6, value.poolDeposit)
        addParam(7, value.maxEpoch)
        addParam(8, value.nOpt)
        addParam(9, value.poolPledgeInfluence)
        addParam(10, value.expansionRate)
        addParam(11, value.treasuryGrowthRate)

        // Advanced parameters (keys 16-24)
        addParam(16, value.minPoolCost)
        addParam(17, value.adaPerUtxoByte)
        addParam(18, value.costModels)
        addParam(19, value.executionCosts)
        addParam(20, value.maxTxExUnits)
        addParam(21, value.maxBlockExUnits)
        addParam(22, value.maxValueSize)
        addParam(23, value.collateralPercentage)
        addParam(24, value.maxCollateralInputs)

        // Governance parameters (keys 25-32)
        addParam(25, value.poolVotingThresholds)
        addParam(26, value.drepVotingThresholds)
        addParam(27, value.minCommitteeSize)
        addParam(28, value.committeeTermLimit)
        addParam(29, value.governanceActionValidityPeriod)
        addParam(30, value.governanceActionDeposit)
        addParam(31, value.drepDeposit)
        addParam(32, value.drepInactivityPeriod)

        // Reference scripts parameters (key 33)
        addParam(33, value.minFeeRefScriptCoinsPerByte)

        mapData(BuiltinList.from(pairs.toList))
    }

    /** Converts Plutus Data representation back to ProtocolParamUpdate.
      *
      * According to the Cardano ledger specification, protocol parameter updates are represented as
      * a Data.Map with integer keys (0-33) mapping to their respective parameter values. This
      * decodes the map back to a ProtocolParamUpdate instance.
      */
    given FromData[ProtocolParamUpdate] = (data: Data) => {
        val mapPairs = unMapData(data)

        // Initialize all fields as None
        var minFeeA: Option[Coin] = None
        var minFeeB: Option[Coin] = None
        var maxBlockBodySize: Option[Int] = None
        var maxTxSize: Option[Int] = None
        var maxBlockHeaderSize: Option[Int] = None
        var keyDeposit: Option[Coin] = None
        var poolDeposit: Option[Coin] = None
        var maxEpoch: Option[Int] = None
        var nOpt: Option[Int] = None
        var poolPledgeInfluence: Option[NonNegativeInterval] = None
        var expansionRate: Option[UnitInterval] = None
        var treasuryGrowthRate: Option[UnitInterval] = None
        var minPoolCost: Option[Coin] = None
        var adaPerUtxoByte: Option[Coin] = None
        var costModels: Option[CostModels] = None
        var executionCosts: Option[ExUnitPrices] = None
        var maxTxExUnits: Option[ExUnits] = None
        var maxBlockExUnits: Option[ExUnits] = None
        var maxValueSize: Option[Int] = None
        var collateralPercentage: Option[Int] = None
        var maxCollateralInputs: Option[Int] = None
        var poolVotingThresholds: Option[PoolVotingThresholds] = None
        var drepVotingThresholds: Option[DRepVotingThresholds] = None
        var minCommitteeSize: Option[Int] = None
        var committeeTermLimit: Option[Int] = None
        var governanceActionValidityPeriod: Option[Int] = None
        var governanceActionDeposit: Option[Coin] = None
        var drepDeposit: Option[Coin] = None
        var drepInactivityPeriod: Option[Int] = None
        var minFeeRefScriptCoinsPerByte: Option[NonNegativeInterval] = None

        // Process each key-value pair in the map
        mapPairs.toList.foreach { pair =>
            val key = unIData(pair.fst).toInt
            val value = pair.snd

            key match {
                // Basic parameters (keys 0-11)
                case 0 => minFeeA = Some(summon[FromData[Coin]].apply(value))
                case 1 => minFeeB = Some(summon[FromData[Coin]].apply(value))
                case 2 => maxBlockBodySize = Some(unIData(value).toInt)
                case 3 => maxTxSize = Some(unIData(value).toInt)
                case 4 => maxBlockHeaderSize = Some(unIData(value).toInt)
                case 5 => keyDeposit = Some(summon[FromData[Coin]].apply(value))
                case 6 => poolDeposit = Some(summon[FromData[Coin]].apply(value))
                case 7 => maxEpoch = Some(unIData(value).toInt)
                case 8 => nOpt = Some(unIData(value).toInt)
                case 9 =>
                    poolPledgeInfluence = Some(summon[FromData[NonNegativeInterval]].apply(value))
                case 10 => expansionRate = Some(summon[FromData[UnitInterval]].apply(value))
                case 11 => treasuryGrowthRate = Some(summon[FromData[UnitInterval]].apply(value))

                // Advanced parameters (keys 16-24)
                case 16 => minPoolCost = Some(summon[FromData[Coin]].apply(value))
                case 17 => adaPerUtxoByte = Some(summon[FromData[Coin]].apply(value))
                case 18 => costModels = Some(summon[FromData[CostModels]].apply(value))
                case 19 => executionCosts = Some(summon[FromData[ExUnitPrices]].apply(value))
                case 20 => maxTxExUnits = Some(summon[FromData[ExUnits]].apply(value))
                case 21 => maxBlockExUnits = Some(summon[FromData[ExUnits]].apply(value))
                case 22 => maxValueSize = Some(unIData(value).toInt)
                case 23 => collateralPercentage = Some(unIData(value).toInt)
                case 24 => maxCollateralInputs = Some(unIData(value).toInt)

                // Governance parameters (keys 25-32)
                case 25 =>
                    poolVotingThresholds = Some(summon[FromData[PoolVotingThresholds]].apply(value))
                case 26 =>
                    drepVotingThresholds = Some(summon[FromData[DRepVotingThresholds]].apply(value))
                case 27 => minCommitteeSize = Some(unIData(value).toInt)
                case 28 => committeeTermLimit = Some(unIData(value).toInt)
                case 29 => governanceActionValidityPeriod = Some(unIData(value).toInt)
                case 30 => governanceActionDeposit = Some(summon[FromData[Coin]].apply(value))
                case 31 => drepDeposit = Some(summon[FromData[Coin]].apply(value))
                case 32 => drepInactivityPeriod = Some(unIData(value).toInt)

                // Reference scripts parameters (key 33)
                case 33 =>
                    minFeeRefScriptCoinsPerByte =
                        Some(summon[FromData[NonNegativeInterval]].apply(value))

                case _ => () // Ignore unknown keys
            }
        }

        // Construct ProtocolParamUpdate with all decoded fields
        ProtocolParamUpdate(
          minFeeA = minFeeA,
          minFeeB = minFeeB,
          maxBlockBodySize = maxBlockBodySize,
          maxTxSize = maxTxSize,
          maxBlockHeaderSize = maxBlockHeaderSize,
          keyDeposit = keyDeposit,
          poolDeposit = poolDeposit,
          maxEpoch = maxEpoch,
          nOpt = nOpt,
          poolPledgeInfluence = poolPledgeInfluence,
          expansionRate = expansionRate,
          treasuryGrowthRate = treasuryGrowthRate,
          minPoolCost = minPoolCost,
          adaPerUtxoByte = adaPerUtxoByte,
          costModels = costModels,
          executionCosts = executionCosts,
          maxTxExUnits = maxTxExUnits,
          maxBlockExUnits = maxBlockExUnits,
          maxValueSize = maxValueSize,
          collateralPercentage = collateralPercentage,
          maxCollateralInputs = maxCollateralInputs,
          poolVotingThresholds = poolVotingThresholds,
          drepVotingThresholds = drepVotingThresholds,
          minCommitteeSize = minCommitteeSize,
          committeeTermLimit = committeeTermLimit,
          governanceActionValidityPeriod = governanceActionValidityPeriod,
          governanceActionDeposit = governanceActionDeposit,
          drepDeposit = drepDeposit,
          drepInactivityPeriod = drepInactivityPeriod,
          minFeeRefScriptCoinsPerByte = minFeeRefScriptCoinsPerByte
        )
    }
}
