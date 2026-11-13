package scalus.cardano.ledger

import scala.annotation.nowarn

/** The deprecated upickle codecs of [[ProtocolParams]], to compare the jsoniter readers with.
  *
  * Reading the same JSON through both is what catches a field mapped to the wrong key: a round trip
  * through one codec cannot, since its writer and reader would agree on the mistake.
  */
object LegacyProtocolParamsCodecs {

    // The deprecated codecs are the reference here on purpose.
    @nowarn("msg=deprecated")
    def legacyBlockfrost(json: String): ProtocolParams =
        upickle.default.read[ProtocolParams](json)(using ProtocolParams.blockfrostParamsReadWriter)

    @nowarn("msg=deprecated")
    def legacyCardanoCli(json: String): ProtocolParams =
        upickle.default.read[ProtocolParams](json)(using ProtocolParams.cardanoCliParamsReadWriter)

    @nowarn("msg=deprecated")
    def legacyCardanoCliJson(params: ProtocolParams): String =
        upickle.default.write(params)(using ProtocolParams.cardanoCliParamsReadWriter)

    /** `p` with every voting threshold in lowest terms. The deprecated codecs read thresholds at a
      * precision of 10^6 (`0.67` as `670000/1000000`), and `UnitInterval` equality is by terms.
      */
    def reducedThresholds(p: ProtocolParams): ProtocolParams = {
        val d = p.dRepVotingThresholds
        val q = p.poolVotingThresholds
        p.copy(
          dRepVotingThresholds = DRepVotingThresholds(
            d.motionNoConfidence.reduce,
            d.committeeNormal.reduce,
            d.committeeNoConfidence.reduce,
            d.updateToConstitution.reduce,
            d.hardForkInitiation.reduce,
            d.ppNetworkGroup.reduce,
            d.ppEconomicGroup.reduce,
            d.ppTechnicalGroup.reduce,
            d.ppGovGroup.reduce,
            d.treasuryWithdrawal.reduce
          ),
          poolVotingThresholds = PoolVotingThresholds(
            q.motionNoConfidence.reduce,
            q.committeeNormal.reduce,
            q.committeeNoConfidence.reduce,
            q.hardForkInitiation.reduce,
            q.ppSecurityGroup.reduce
          )
        )
    }
}
