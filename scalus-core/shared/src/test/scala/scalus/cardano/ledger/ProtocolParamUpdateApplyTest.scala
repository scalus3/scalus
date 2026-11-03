package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite

/** `ProtocolParamUpdate.applyTo`, ledger-rules spec [EN-2a], D89: one case per row of the field
  * table, the cost model merge and the floor of key 33.
  */
class ProtocolParamUpdateApplyTest extends AnyFunSuite {

    private val base = CardanoInfo.mainnet.protocolParams
    private val none = ProtocolParamUpdate()

    private val poolThresholds =
        base.poolVotingThresholds.copy(hardForkInitiation = UnitInterval(3, 7))
    private val drepThresholds =
        base.dRepVotingThresholds.copy(updateToConstitution = UnitInterval(4, 9))
    private val prices = ExUnitPrices(NonNegativeInterval(1, 3), NonNegativeInterval(1, 7))

    /** Each row of the [EN-2a] table: the key, the update with that field only, the
      * `ProtocolParams` field it sets and the value it must hold after.
      */
    private val rows: Seq[(Int, ProtocolParamUpdate, String, Any)] = Seq(
      (0, none.copy(minFeeA = Some(Coin(45))), "txFeePerByte", 45L),
      (1, none.copy(minFeeB = Some(Coin(155_382))), "txFeeFixed", 155_382L),
      (2, none.copy(maxBlockBodySize = Some(90_113)), "maxBlockBodySize", 90_113L),
      (3, none.copy(maxTxSize = Some(16_385)), "maxTxSize", 16_385L),
      (4, none.copy(maxBlockHeaderSize = Some(1_101)), "maxBlockHeaderSize", 1_101L),
      (5, none.copy(keyDeposit = Some(Coin(2_000_001))), "stakeAddressDeposit", 2_000_001L),
      (6, none.copy(poolDeposit = Some(Coin(500_000_001))), "stakePoolDeposit", 500_000_001L),
      (7, none.copy(maxEpoch = Some(19)), "poolRetireMaxEpoch", 19L),
      (8, none.copy(nOpt = Some(501)), "stakePoolTargetNum", 501L),
      (
        9,
        none.copy(poolPledgeInfluence = Some(NonNegativeInterval(4, 10))),
        "poolPledgeInfluence",
        NonNegativeInterval(4, 10)
      ),
      // D89: rho, tau and a0 are reduced, as the ledger holds them (boundRational uses %), so
      // 2/10 is 1/5 (UnitInterval compares by its terms)
      (
        10,
        none.copy(expansionRate = Some(UnitInterval(2, 10))),
        "monetaryExpansion",
        UnitInterval(1, 5)
      ),
      (
        11,
        none.copy(treasuryGrowthRate = Some(UnitInterval(3, 10))),
        "treasuryCut",
        UnitInterval(3, 10)
      ),
      (16, none.copy(minPoolCost = Some(Coin(170_000_001))), "minPoolCost", 170_000_001L),
      (17, none.copy(adaPerUtxoByte = Some(Coin(4_311))), "utxoCostPerByte", 4_311L),
      (
        18,
        none.copy(costModels = Some(CostModels(Map(2 -> IndexedSeq(1L, 2L, 3L))))),
        "costModels",
        CostModels(base.costModels.models.updated(2, IndexedSeq(1L, 2L, 3L)))
      ),
      (19, none.copy(executionCosts = Some(prices)), "executionUnitPrices", prices),
      (20, none.copy(maxTxExUnits = Some(ExUnits(11, 22))), "maxTxExecutionUnits", ExUnits(11, 22)),
      (
        21,
        none.copy(maxBlockExUnits = Some(ExUnits(33, 44))),
        "maxBlockExecutionUnits",
        ExUnits(33, 44)
      ),
      (22, none.copy(maxValueSize = Some(5_001)), "maxValueSize", 5_001L),
      (23, none.copy(collateralPercentage = Some(151)), "collateralPercentage", 151L),
      (24, none.copy(maxCollateralInputs = Some(4)), "maxCollateralInputs", 4L),
      (
        25,
        none.copy(poolVotingThresholds = Some(poolThresholds)),
        "poolVotingThresholds",
        poolThresholds
      ),
      (
        26,
        none.copy(drepVotingThresholds = Some(drepThresholds)),
        "dRepVotingThresholds",
        drepThresholds
      ),
      (27, none.copy(minCommitteeSize = Some(8)), "committeeMinSize", 8L),
      (28, none.copy(committeeTermLimit = Some(147)), "committeeMaxTermLength", 147L),
      (29, none.copy(governanceActionValidityPeriod = Some(7)), "govActionLifetime", 7L),
      (
        30,
        none.copy(governanceActionDeposit = Some(Coin(100_000_000_001L))),
        "govActionDeposit",
        100_000_000_001L
      ),
      (31, none.copy(drepDeposit = Some(Coin(500_000_001))), "dRepDeposit", 500_000_001L),
      (32, none.copy(drepInactivityPeriod = Some(21)), "dRepActivity", 21L),
      (
        33,
        none.copy(minFeeRefScriptCoinsPerByte = Some(NonNegativeInterval(16, 1))),
        "minFeeRefScriptCostPerByte",
        16L
      )
    )

    test("an update with no field keeps every pparam") {
        assert(none.applyTo(base) == base)
    }

    test("the field table has one row per key of the update") {
        // spec [EN-2a]: keys 0 to 11 and 16 to 33; protocolVersion has no key (C/PParams.hs:1182)
        assert(rows.map(_._1) == ((0 to 11) ++ (16 to 33)))
    }

    rows.foreach { (key, update, field, expected) =>
        test(s"key $key sets $field and nothing else") {
            // spec [EN-2a]: conwayApplyPPUpdates, C/PParams.hs:1182-1229
            val applied = update.applyTo(base)
            val diffs = ProtocolParams.diff(base, applied)
            assert(diffs.map(_.field) == Seq(field))
            val index = applied.productElementNames.indexOf(field)
            assert(applied.productElement(index) == expected)
        }
    }

    test("a cost model update keeps the languages it does not name") {
        // spec [EN-2a] key 18: updateCostModels, cardano-ledger-core Plutus/CostModels.hs:401-411
        val params =
            base.copy(costModels = CostModels(Map(0 -> IndexedSeq(1L), 1 -> IndexedSeq(2L))))
        val update =
            none.copy(costModels = Some(CostModels(Map(1 -> IndexedSeq(9L), 2 -> IndexedSeq(7L)))))
        assert(
          update.applyTo(params).costModels ==
              CostModels(Map(0 -> IndexedSeq(1L), 1 -> IndexedSeq(9L), 2 -> IndexedSeq(7L)))
        )
    }

    test("key 33 takes the floor of the interval") {
        // spec [EN-2a] key 33, D89: 15/2 gives 7
        val update = none.copy(minFeeRefScriptCoinsPerByte = Some(NonNegativeInterval(15, 2)))
        assert(update.applyTo(base).minFeeRefScriptCostPerByte == 7L)
    }
}
