package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{CardanoInfo, CostModels, ExUnits, Language, MajorProtocolVersion}
import scalus.uplc.{Constant, DefaultUni, PlutusV1Params, PlutusV2Params, PlutusV3Params}

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
        test(s"$language honors every supplied dropList cost") {
            val costs = protocolParams.costModels.models(language.ordinal)
            // Positional indices from Plutus 1.63.0.0 V1/V2/V3 ParamName.hs.
            val index = if language == Language.PlutusV3 then 302 else 284
            val initial = load(language, costs).builtinCostModel.dropList.calculateCost(dropArgs*)
            for value <- Seq(costs(index) + 100L, 300_000_000L, 1L) do
                val changed = load(language, costs.updated(index, value)).builtinCostModel.dropList
                    .calculateCost(dropArgs*)
                assert(changed.steps == initial.steps + value - costs(index))
                assert(changed.memory == initial.memory)
        }

    test("supplied constr and case costs are used exactly as given") {
        for
            (language, index) <- Seq(
              (Language.PlutusV1, 175),
              (Language.PlutusV2, 185),
              (Language.PlutusV3, 193)
            )
            (cpu, memory) <- Seq((300_000_000L, 1234L), (5678L, 42L))
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

    test("a parameter the supplied model does not reach is priced beyond any budget") {
        // plutus's rule, from Note [Cost model parameters from the ledger's point of view]: too
        // few entries are padded with maxBound, which prices the affected builtin out rather than
        // giving it a plausible cost. Costing saturates, so this cannot wrap into a cheap budget.
        for (language, length) <- Seq(
              (Language.PlutusV1, 166),
              (Language.PlutusV2, 175),
              (Language.PlutusV2, 185),
              (Language.PlutusV3, 251),
              (Language.PlutusV3, 297)
            )
        do
            val costs = protocolParams.costModels.models(language.ordinal).take(length)
            val dropList = load(language, costs).builtinCostModel.dropList
            assert(dropList.calculateCost(dropArgs*).steps == Long.MaxValue)

        for params <- Seq(
              PlutusV1Params.fromSeq(Vector.fill(166)(1L)),
              PlutusV2Params.fromSeq(Vector.fill(175)(1L))
            )
        do assert(params.`dropList-cpu-arguments-intercept` == Long.MaxValue)
    }

    test("a model longer than this Scalus knows is read up to what it knows") {
        // The other half of the same rule: extra entries are ignored, not rejected. A node running
        // older software must keep working against a newer chain.
        for language <- languages do
            val costs = protocolParams.costModels.models(language.ordinal)
            val future = costs ++ Vector.fill(20)(7L)
            assert(load(language, future) == load(language, costs))
    }

    test("a custom network may supply any length, and none of them is an error") {
        // No table of accepted lengths exists to consult, so a fork or testnet that enacted an
        // unusual model still evaluates rather than being refused.
        val v3 = protocolParams.costModels.models(Language.PlutusV3.ordinal)
        for length <- Seq(0, 1, 100, 251, 297, 349, 350, 351, 400) do
            val costs = v3.take(length).padTo(length, 3L)
            // The assertion is that this returns at all.
            assert(load(Language.PlutusV3, costs).machineCosts != null)
    }

    test("the parameter count comes from the class, not from a table") {
        assert(PlutusV1Params.numberOfParams == 332)
        assert(PlutusV2Params.numberOfParams == 332)
        assert(PlutusV3Params.numberOfParams == 350)
        for language <- languages do
            val enacted = protocolParams.costModels.models(language.ordinal).length
            val known = language match
                case Language.PlutusV1 => PlutusV1Params.numberOfParams
                case Language.PlutusV2 => PlutusV2Params.numberOfParams
                case _                 => PlutusV3Params.numberOfParams
            assert(enacted == known, s"$language: mainnet enacted $enacted, we know $known")
    }

    test("each semantics variant's machine costs are one step cost repeated") {
        // Nothing pinned these before. Every step is 100 memory and the variant's step cost,
        // startup is always 100/100, and A and B differ only in that one number.
        for (costs, stepCost) <- Seq(
              (CekMachineCosts.defaultMachineCostsA, 23000L),
              (CekMachineCosts.defaultMachineCostsB, 16000L)
            )
        do
            assert(costs.startupCost == ExUnits(100, 100))
            val steps = Seq(
              costs.varCost,
              costs.constCost,
              costs.lamCost,
              costs.delayCost,
              costs.forceCost,
              costs.applyCost,
              costs.builtinCost,
              costs.constrCost,
              costs.caseCost
            )
            assert(steps.forall(_ == ExUnits(100, stepCost)))

        assert(CekMachineCosts.defaultMachineCosts == CekMachineCosts.defaultMachineCostsB)
    }

    test("a language without a cost model is rejected by name") {
        // spec [SH-5]
        val mainnet = CardanoInfo.mainnet.protocolParams.costModels.models
        val onlyV3 =
            CostModels(Map(Language.PlutusV3.ordinal -> mainnet(Language.PlutusV3.ordinal)))
        val error = intercept[IllegalArgumentException] {
            MachineParams.fromCostModels(
              onlyV3,
              Language.PlutusV2,
              MajorProtocolVersion.vanRossemPV
            )
        }
        assert(error.getMessage == "no cost model for PlutusV2")
    }
}
