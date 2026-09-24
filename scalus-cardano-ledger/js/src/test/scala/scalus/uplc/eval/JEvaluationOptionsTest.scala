package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{CardanoInfo, CostModels, ExUnits, JsCardanoInfo, JsProtocolParams, Language, MajorProtocolVersion, ProtocolVersion}
import scalus.uplc.{Constant, DefaultUni}

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

class JEvaluationOptionsTest extends AnyFunSuite {

    private val mainnet = CardanoInfo.mainnet.protocolParams

    private def model(language: Language): js.Array[Double] =
        mainnet.costModels.models(language.languageId).map(_.toDouble).toJSArray

    private def options(
        language: js.Any = "PlutusV3",
        protocol: js.Any = 11,
        costs: js.Any = model(Language.PlutusV3)
    ): JEvaluationOptions =
        js.Dynamic
            .literal(plutusVersion = language, protocolMajorVersion = protocol, costModel = costs)
            .asInstanceOf[JEvaluationOptions]

    private def typeError(body: => Any): js.TypeError =
        intercept[js.JavaScriptException](body).exception match
            case e: js.TypeError => e
            case other           => fail(s"expected TypeError, got $other")

    test("mainnet and fromProtocolParams select the requested model and make fresh records") {
        for language <- Seq(Language.PlutusV1, Language.PlutusV2, Language.PlutusV3) do
            val version = language.toString
            val params = JsCardanoInfo.mainnet().protocolParams
            for factory <- Seq(
                  () => JEvaluationOptions.mainnet(version),
                  () => JEvaluationOptions.fromProtocolParams(version, params)
                )
            do
                val a = factory()
                val b = factory()
                assert(a.plutusVersion == version)
                assert(a.protocolMajorVersion == 11d)
                assert(a.costModel.toSeq == model(language).toSeq)
                assert(!(a.costModel eq b.costModel))
    }

    test("a plain object literal is an options record") {
        val vm = JEvaluationOptions.machine(options())
        assert(vm.language == Language.PlutusV3)
        assert(vm.protocolVersion == MajorProtocolVersion.vanRossemPV)
    }

    test("a short cost model configures, and prices out what it does not cover") {
        val vm = JEvaluationOptions.machine(options(costs = js.Array(1d, 2d, 3d)))
        val dropList = vm.machineParams.builtinCostModel.dropList.calculateCost(
          CekValue.VCon(Constant.Integer(1)),
          CekValue.VCon(Constant.List(DefaultUni.Integer, List(Constant.Integer(1))))
        )
        assert(dropList.steps == Long.MaxValue)
    }

    test("any cost model length configures, whatever the protocol") {
        for
            language <- Seq(Language.PlutusV1, Language.PlutusV2, Language.PlutusV3)
            protocol <- language.introducedInVersion.version to 12
            length <- Seq(0, 1, 175, 297, 350, 400)
        do
            val costs = model(language).take(length).toJSArray
            while costs.length < length do costs.push(3d)
            val vm = JEvaluationOptions.machine(options(language.toString, protocol, costs))
            assert(vm.protocolVersion == MajorProtocolVersion(protocol))
    }

    test("PlutusV4 configures at PV12, and a negative cost is a cost") {
        assert(
          JEvaluationOptions.machine(options("PlutusV4", 12)).language == Language.PlutusV4
        )
        val negative = model(Language.PlutusV3)
        negative(0) = -1
        assert(JEvaluationOptions.machine(options(costs = negative)).language == Language.PlutusV3)
    }

    test("a cost may be a bigint, and means the same as the number") {
        val asNumbers = model(Language.PlutusV3)
        val asBigints = asNumbers.map(c => js.BigInt(c.toLong.toString): js.Any)
        assert(
          JEvaluationOptions.machine(options(costs = asBigints)).machineParams ==
              JEvaluationOptions.machine(options(costs = asNumbers)).machineParams
        )
    }

    test("an unrecognised field is ignored") {
        val stray = options().asInstanceOf[js.Dynamic]
        stray.updateDynamic("profile")("not a boolean")
        val vm = JEvaluationOptions.machine(stray.asInstanceOf[JEvaluationOptions])
        assert(vm.machineParams == JEvaluationOptions.machine(options()).machineParams)
    }

    test("a record that would silently mis-cost is a TypeError") {
        val hole = new js.Array[Double](3)
        val cases = Seq[(String, JEvaluationOptions)](
          "language as a number" -> options(language = 3),
          "unknown language" -> options(language = "PlutusV9"),
          "protocol as a string" -> options(protocol = "11"),
          "fractional protocol" -> options(protocol = 9.5),
          "NaN protocol" -> options(protocol = Double.NaN),
          "protocol past Int" -> options(protocol = Int.MaxValue.toDouble + 1),
          "protocol with no semantics" -> options(protocol = 0),
          "cost as a string" -> options(costs = js.Array[js.Any]("1")),
          "fractional cost" -> options(costs = js.Array(1.5)),
          "unsafe cost" -> options(costs = js.Array(9_007_199_254_740_992d)),
          "hole in the costs" -> options(costs = hole)
        )
        for (label, invalid) <- cases do
            withClue(label)(typeError(JEvaluationOptions.machine(invalid)))
        assert(
          typeError(JEvaluationOptions.machine(options(costs = hole))).message ==
              "costModel[0] must be a safe integer"
        )
    }

    test("maxBudget is optional, and takes numbers or bigints") {
        assert(JEvaluationOptions.maxBudget(options()).isEmpty)
        def withBudget(memory: js.Any, steps: js.Any) = {
            val o = options().asInstanceOf[js.Dynamic]
            o.updateDynamic("maxBudget")(js.Dynamic.literal(memory = memory, steps = steps))
            o.asInstanceOf[JEvaluationOptions]
        }
        assert(JEvaluationOptions.maxBudget(withBudget(10, 20)).contains(ExUnits(10, 20)))
        assert(
          JEvaluationOptions
              .maxBudget(withBudget(js.BigInt("10"), js.BigInt("9223372036854775807")))
              .contains(ExUnits(10, Long.MaxValue))
        )
        typeError(JEvaluationOptions.maxBudget(withBudget(1.5, 1)))
        typeError(JEvaluationOptions.maxBudget(withBudget(1, js.BigInt("9223372036854775808"))))
    }

    test("fromProtocolParams copies what the parameters say") {
        typeError {
            JEvaluationOptions.fromProtocolParams(
              "PlutusV3",
              JsProtocolParams.wrap(mainnet.copy(costModels = CostModels(Map.empty)))
            )
        }
        val short = CostModels(Map(Language.PlutusV3.ordinal -> Vector(1L)))
        val fromShort = JEvaluationOptions.fromProtocolParams(
          "PlutusV3",
          JsProtocolParams.wrap(mainnet.copy(costModels = short))
        )
        assert(fromShort.costModel.toSeq == Seq(1d))
        val pv12 = JEvaluationOptions.fromProtocolParams(
          "PlutusV3",
          JsProtocolParams.wrap(mainnet.copy(protocolVersion = ProtocolVersion(12, 0)))
        )
        assert(pv12.protocolMajorVersion == 12d)
    }
}
