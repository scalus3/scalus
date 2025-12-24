package scalus.cardano.ledger

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}
import upickle.ReadWriter
import scalus.builtin.{BuiltinList, Data, FromData, ToData}
import scalus.builtin.Builtins.{listData, unListData}

/** Represents voting thresholds for stake pools in the Cardano blockchain.
  *
  * Defines the thresholds required for different governance actions to pass when voted on by stake
  * pools.
  *
  * @param motionNoConfidence
  *   Threshold for no confidence motions
  * @param committeeNormal
  *   Threshold for normal committee updates
  * @param committeeNoConfidence
  *   Threshold for committee updates after no confidence
  * @param hardForkInitiation
  *   Threshold for hard fork initiations
  * @param ppSecurityGroup
  *   Threshold for security protocol parameter changes
  */
case class PoolVotingThresholds(
    motionNoConfidence: UnitInterval,
    committeeNormal: UnitInterval,
    committeeNoConfidence: UnitInterval,
    hardForkInitiation: UnitInterval,
    ppSecurityGroup: UnitInterval
) derives ReadWriter

object PoolVotingThresholds {

    /** CBOR Encoder for PoolVotingThresholds. Encodes as an array of 5 UnitIntervals.
      */
    given Encoder[PoolVotingThresholds] = (w: Writer, value: PoolVotingThresholds) =>
        w.writeArrayOpen(5)
            .write(value.motionNoConfidence)
            .write(value.committeeNormal)
            .write(value.committeeNoConfidence)
            .write(value.hardForkInitiation)
            .write(value.ppSecurityGroup)
            .writeArrayClose()

    /** CBOR Decoder for PoolVotingThresholds. Decodes from an array of 5 UnitIntervals.
      */
    given Decoder[PoolVotingThresholds] = (r: Reader) => {
        r.readArrayHeader()
        val motionNoConfidence = r.read[UnitInterval]()
        val committeeNormal = r.read[UnitInterval]()
        val committeeNoConfidence = r.read[UnitInterval]()
        val hardFork = r.read[UnitInterval]()
        val ppSecurityGroup = r.read[UnitInterval]()

        PoolVotingThresholds(
          motionNoConfidence,
          committeeNormal,
          committeeNoConfidence,
          hardFork,
          ppSecurityGroup
        )
    }

    /** ToData instance for PoolVotingThresholds. Encodes as an array of 5 UnitIntervals.
      */
    given ToData[PoolVotingThresholds] = (thresholds: PoolVotingThresholds) => {
        val unitIntervalToData = summon[ToData[UnitInterval]]
        listData(
          BuiltinList.from(
            List(
              unitIntervalToData(thresholds.motionNoConfidence),
              unitIntervalToData(thresholds.committeeNormal),
              unitIntervalToData(thresholds.committeeNoConfidence),
              unitIntervalToData(thresholds.hardForkInitiation),
              unitIntervalToData(thresholds.ppSecurityGroup)
            )
          )
        )
    }

    /** FromData instance for PoolVotingThresholds. Decodes from an array of 5 UnitIntervals.
      */
    given FromData[PoolVotingThresholds] = (data: Data) => {
        val list = unListData(data)
        val fromDataInterval = summon[FromData[UnitInterval]]
        val elements = list.toList
        PoolVotingThresholds(
          motionNoConfidence = fromDataInterval(elements(0)),
          committeeNormal = fromDataInterval(elements(1)),
          committeeNoConfidence = fromDataInterval(elements(2)),
          hardForkInitiation = fromDataInterval(elements(3)),
          ppSecurityGroup = fromDataInterval(elements(4))
        )
    }
}

/** Represents voting thresholds for DReps in the Cardano blockchain.
  *
  * Defines the thresholds required for different governance actions to pass when voted on by
  * delegated representatives.
  *
  * @param motionNoConfidence
  *   Threshold for no confidence motions
  * @param committeeNormal
  *   Threshold for normal committee updates
  * @param committeeNoConfidence
  *   Threshold for committee updates after no confidence
  * @param updateToConstitution
  *   Threshold for constitution updates
  * @param hardForkInitiation
  *   Threshold for hard fork initiations
  * @param ppNetworkGroup
  *   Threshold for network protocol parameter changes
  * @param ppEconomicGroup
  *   Threshold for economic protocol parameter changes
  * @param ppTechnicalGroup
  *   Threshold for technical protocol parameter changes
  * @param ppGovGroup
  *   Threshold for governance protocol parameter changes
  * @param treasuryWithdrawal
  *   Threshold for treasury withdrawals
  */
case class DRepVotingThresholds(
    motionNoConfidence: UnitInterval,
    committeeNormal: UnitInterval,
    committeeNoConfidence: UnitInterval,
    updateToConstitution: UnitInterval,
    hardForkInitiation: UnitInterval,
    ppNetworkGroup: UnitInterval,
    ppEconomicGroup: UnitInterval,
    ppTechnicalGroup: UnitInterval,
    ppGovGroup: UnitInterval,
    treasuryWithdrawal: UnitInterval
) derives ReadWriter

object DRepVotingThresholds {

    /** CBOR Encoder for DRepVotingThresholds. Encodes as an array of 10 UnitIntervals.
      */
    given Encoder[DRepVotingThresholds] = (w: Writer, value: DRepVotingThresholds) =>
        w.writeArrayOpen(10)
            .write(value.motionNoConfidence)
            .write(value.committeeNormal)
            .write(value.committeeNoConfidence)
            .write(value.updateToConstitution)
            .write(value.hardForkInitiation)
            .write(value.ppNetworkGroup)
            .write(value.ppEconomicGroup)
            .write(value.ppTechnicalGroup)
            .write(value.ppGovGroup)
            .write(value.treasuryWithdrawal)
            .writeArrayClose()

    /** CBOR Decoder for DRepVotingThresholds. Decodes from an array of 10 UnitIntervals.
      */
    given Decoder[DRepVotingThresholds] = (r: Reader) => {
        r.readArrayHeader()
        val motionNoConfidence = r.read[UnitInterval]()
        val committeeNormal = r.read[UnitInterval]()
        val committeeNoConfidence = r.read[UnitInterval]()
        val updateToConstitution = r.read[UnitInterval]()
        val hardForkInitiation = r.read[UnitInterval]()
        val ppNetworkGroup = r.read[UnitInterval]()
        val ppEconomicGroup = r.read[UnitInterval]()
        val ppTechnicalGroup = r.read[UnitInterval]()
        val ppGovGroup = r.read[UnitInterval]()
        val treasuryWithdrawal = r.read[UnitInterval]()

        DRepVotingThresholds(
          motionNoConfidence,
          committeeNormal,
          committeeNoConfidence,
          updateToConstitution,
          hardForkInitiation,
          ppNetworkGroup,
          ppEconomicGroup,
          ppTechnicalGroup,
          ppGovGroup,
          treasuryWithdrawal
        )
    }

    /** ToData instance for DRepVotingThresholds. Encodes as an array of 10 UnitIntervals.
      */
    given ToData[DRepVotingThresholds] = (thresholds: DRepVotingThresholds) => {
        val unitIntervalToData = summon[ToData[UnitInterval]]
        listData(
          BuiltinList.from(
            List(
              unitIntervalToData(thresholds.motionNoConfidence),
              unitIntervalToData(thresholds.committeeNormal),
              unitIntervalToData(thresholds.committeeNoConfidence),
              unitIntervalToData(thresholds.updateToConstitution),
              unitIntervalToData(thresholds.hardForkInitiation),
              unitIntervalToData(thresholds.ppNetworkGroup),
              unitIntervalToData(thresholds.ppEconomicGroup),
              unitIntervalToData(thresholds.ppTechnicalGroup),
              unitIntervalToData(thresholds.ppGovGroup),
              unitIntervalToData(thresholds.treasuryWithdrawal)
            )
          )
        )
    }

    /** FromData instance for DRepVotingThresholds. Decodes from an array of 10 UnitIntervals.
      */
    given FromData[DRepVotingThresholds] = (data: Data) => {
        val list = unListData(data)
        val fromDataInterval = summon[FromData[UnitInterval]]
        val elements = list.toList
        DRepVotingThresholds(
          motionNoConfidence = fromDataInterval(elements(0)),
          committeeNormal = fromDataInterval(elements(1)),
          committeeNoConfidence = fromDataInterval(elements(2)),
          updateToConstitution = fromDataInterval(elements(3)),
          hardForkInitiation = fromDataInterval(elements(4)),
          ppNetworkGroup = fromDataInterval(elements(5)),
          ppEconomicGroup = fromDataInterval(elements(6)),
          ppTechnicalGroup = fromDataInterval(elements(7)),
          ppGovGroup = fromDataInterval(elements(8)),
          treasuryWithdrawal = fromDataInterval(elements(9))
        )
    }
}
