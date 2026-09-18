package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{CardanoInfo, CostModels, ExUnits, Language, MajorProtocolVersion}
import scalus.uplc.{Constant, DefaultUni, PlutusV1Params, PlutusV2Params}

class MachineParamsCostModelTest extends AnyFunSuite {
    private val protocolParams = CardanoInfo.mainnet.protocolParams
    private val languages = Seq(Language.PlutusV1, Language.PlutusV2, Language.PlutusV3)
    private val dropArgs = Seq(
      CekValue.VCon(Constant.Integer(1)),
      CekValue.VCon(Constant.List(DefaultUni.Integer, List(Constant.Integer(1))))
    )

    private def load(language: Language, costs: IndexedSeq[Long]): MachineParams =
        MachineParams.fromCostModels(
          CostModels(Map(language.ordinal -> costs)),
          language,
          MajorProtocolVersion.vanRossemPV
        )

    for language <- languages do
        test(s"$language honors supplied dropList costs, including the historical sentinel") {
            val costs = protocolParams.costModels.models(language.ordinal)
            // Positional indices from Plutus 1.63.0.0 V1/V2/V3 ParamName.hs.
            val index = if language == Language.PlutusV3 then 302 else 284
            val initial = load(language, costs).builtinCostModel.dropList.calculateCost(dropArgs*)
            for value <- Seq(costs(index) + 100L, 300_000_000L) do
                val changed = load(language, costs.updated(index, value)).builtinCostModel.dropList
                    .calculateCost(dropArgs*)
                assert(changed.steps == initial.steps + value - costs(index))
                assert(changed.memory == initial.memory)
        }

    test("supplied constr and case costs are exact, even when CPU or memory equals the sentinel") {
        for
            (language, index) <- Seq(
              (Language.PlutusV1, 175),
              (Language.PlutusV2, 185),
              (Language.PlutusV3, 193)
            )
            (cpu, memory) <- Seq((300_000_000L, 1234L), (5678L, 300_000_000L))
        do
            val costs = protocolParams.costModels
                .models(language.ordinal)
                .updated(index, cpu)
                .updated(index + 1, memory)
                .updated(index + 2, cpu)
                .updated(index + 3, memory)
            val machine = load(language, costs).machineCosts
            assert(machine.constrCost == ExUnits(memory, cpu))
            assert(machine.caseCost == ExUnits(memory, cpu))
    }

    test("supplied CEK costs use the existing protocol-parameter loader") {
        for language <- languages do
            val costs = protocolParams.costModels.models(language.ordinal)
            val original = MachineParams.fromProtocolParams(protocolParams, language)
            val changed = load(language, costs.updated(17, costs(17) + 100L))
            assert(
              changed.machineCosts.applyCost.steps == original.machineCosts.applyCost.steps + 100L
            )
    }

    test("V1 and V2 consume the appended fields in ledger order") {
        val costs = (0 until 332).map(_.toLong)
        val v1 = PlutusV1Params.fromSeq(costs)
        val v2 = PlutusV2Params.fromSeq(costs)
        assert(v1.`serialiseData-cpu-arguments-intercept` == 166L)
        assert(v2.`serialiseData-cpu-arguments-intercept` == 133L)
        for params <- Seq(v1, v2) do
            assert(params.`dropList-cpu-arguments-intercept` == 284L)
            assert(params.`scaleValue-cpu-arguments-intercept` == 328L)
            assert(params.`scaleValue-cpu-arguments-slope` == 329L)
            assert(params.`scaleValue-memory-arguments-intercept` == 330L)
            assert(params.`scaleValue-memory-arguments-slope` == 331L)
        assert(PlutusV1Params.toSeq(v1) == costs)
        assert(PlutusV2Params.toSeq(v2) == costs)
    }

    test("historical omitted suffixes retain the existing reference fallback") {
        for (language, length) <- Seq(
              (Language.PlutusV1, 166),
              (Language.PlutusV2, 175),
              (Language.PlutusV2, 185),
              (Language.PlutusV3, 251),
              (Language.PlutusV3, 297)
            )
        do
            val costs = protocolParams.costModels.models(language.ordinal).take(length)
            val loaded = load(language, costs)
            assert(loaded.builtinCostModel.dropList == VanRossemNewBuiltinCosts.dropList)
            if language != Language.PlutusV3 then
                assert(
                  loaded.machineCosts.constrCost == CekMachineCosts.defaultMachineCosts.constrCost
                )
                assert(loaded.machineCosts.caseCost == CekMachineCosts.defaultMachineCosts.caseCost)
        assert(
          PlutusV1Params
              .fromSeq(Vector.fill(166)(1L))
              .`dropList-cpu-arguments-intercept` == 300_000_000L
        )
        assert(
          PlutusV2Params
              .fromSeq(Vector.fill(175)(1L))
              .`dropList-cpu-arguments-intercept` == 300_000_000L
        )
    }
}
