package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{Language, MajorProtocolVersion, ProtocolParams}
import scalus.uplc.*
import scalus.uplc.builtin.platform

class BuiltinCostModelTest extends AnyFunSuite:
    test("BuiltinSemanticsVariant follows Van Rossem protocol mapping") {
        assert(
          BuiltinSemanticsVariant.fromProtocolAndPlutusVersion(
            MajorProtocolVersion.plominPV,
            Language.PlutusV3
          ) == BuiltinSemanticsVariant.C
        )
        assert(
          BuiltinSemanticsVariant.fromProtocolAndPlutusVersion(
            MajorProtocolVersion.vanRossemPV,
            Language.PlutusV3
          ) == BuiltinSemanticsVariant.E
        )
        assert(
          BuiltinSemanticsVariant.fromProtocolAndPlutusVersion(
            MajorProtocolVersion.vanRossemPV,
            Language.PlutusV2
          ) == BuiltinSemanticsVariant.D
        )
    }

    test("PlutusV3Params accepts PV11 350-parameter cost model tail") {
        val params = PlutusV3Params.fromSeq(1L to 350L)
        assert(params.numberOfParams == 350)
        assert(params.`ripemd_160-memory-arguments` == 297L)
        assert(params.`expModInteger-cpu-arguments-coefficient00` == 298L)
        assert(params.`dropList-cpu-arguments-intercept` == 303L)
        assert(params.`bls12_381_G2_multiScalarMul-memory-arguments` == 319L)
        assert(params.`scaleValue-memory-arguments-slope` == 350L)

        val legacyParams = PlutusV3Params.fromSeq(Seq.fill(297)(1L))
        assert(legacyParams.`expModInteger-cpu-arguments-coefficient00` == 300_000_000L)
    }

    test("PlutusV3 PV11 divideInteger uses above-and-below-diagonal costing") {
        val params = new PlutusV3Params
        params.`divideInteger-cpu-arguments-constant` = 85848L
        params.`divideInteger-cpu-arguments-c00` = 123203L
        params.`divideInteger-cpu-arguments-c01` = 7305L
        params.`divideInteger-cpu-arguments-c02` = -900L
        params.`divideInteger-cpu-arguments-c10` = 1716L
        params.`divideInteger-cpu-arguments-c11` = 960L
        params.`divideInteger-cpu-arguments-c20` = 57L
        params.`divideInteger-cpu-arguments-minimum` = 85848L
        params.`divideInteger-memory-arguments-intercept` = 0L
        params.`divideInteger-memory-arguments-minimum` = 1L
        params.`divideInteger-memory-arguments-slope` = 1L

        val oneWord = CekValue.VCon(Constant.Integer(0))
        val twoWords = CekValue.VCon(Constant.Integer(BigInt(1) << 64))

        val pv10 = BuiltinCostModel
            .fromPlutusParams(params, Language.PlutusV3, BuiltinSemanticsVariant.C)
            .divideInteger
            .calculateCost(oneWord, twoWords)
        val pv11 = BuiltinCostModel
            .fromPlutusParams(params, Language.PlutusV3, BuiltinSemanticsVariant.E)
            .divideInteger
            .calculateCost(oneWord, twoWords)

        assert(pv10.steps == 85848L)
        assert(pv11.steps == 135188L)
        assert(pv11.steps - pv10.steps == 49340L)
        assert(pv11.memory == pv10.memory)
    }

    test("Van Rossem string builtins cost text by UTF-8 byte length") {
        val params = new PlutusV3Params
        params.`appendString-cpu-arguments-intercept` = 0L
        params.`appendString-cpu-arguments-slope` = 1L
        params.`appendString-memory-arguments-intercept` = 0L
        params.`appendString-memory-arguments-slope` = 1L
        params.`equalsString-cpu-arguments-constant` = 999L
        params.`equalsString-cpu-arguments-intercept` = 0L
        params.`equalsString-cpu-arguments-slope` = 1L
        params.`equalsString-memory-arguments` = 1L
        params.`encodeUtf8-cpu-arguments-intercept` = 0L
        params.`encodeUtf8-cpu-arguments-slope` = 1L
        params.`encodeUtf8-memory-arguments-intercept` = 0L
        params.`encodeUtf8-memory-arguments-slope` = 1L

        val text = CekValue.VCon(Constant.String("abcdefgh"))
        val empty = CekValue.VCon(Constant.String(""))

        val pv10Builtins = new CardanoBuiltins(
          BuiltinCostModel.fromPlutusParams(params, Language.PlutusV3, BuiltinSemanticsVariant.C),
          platform,
          BuiltinSemanticsVariant.C
        )
        val pv11Builtins = new CardanoBuiltins(
          BuiltinCostModel.fromPlutusParams(params, Language.PlutusV3, BuiltinSemanticsVariant.E),
          platform,
          BuiltinSemanticsVariant.E
        )

        assert(pv10Builtins.AppendString.costFunction.calculateCost(text, empty).steps == 8L)
        assert(pv11Builtins.AppendString.costFunction.calculateCost(text, empty).steps == 2L)
        assert(pv11Builtins.EqualsString.costFunction.calculateCost(text, text).steps == 2L)
        assert(pv11Builtins.EncodeUtf8.costFunction.calculateCost(text).steps == 2L)
    }

    test("BuiltinCostModel from Cardano Protocol Parameters") {
        val pparams = ProtocolParams.fromCardanoCliJson(
          this.getClass.getResourceAsStream("/protocol-params.json")
        )
        val v1 = pparams.costModels.models(Language.PlutusV1.languageId)
        val v2 = pparams.costModels.models(Language.PlutusV2.languageId)
        val v3 = pparams.costModels.models(Language.PlutusV3.languageId)
        val paramsV1 = PlutusV1Params.fromSeq(v1)
        val paramsV2 = PlutusV2Params.fromSeq(v2)
        val paramsV3 = PlutusV3Params.fromSeq(v3)
        BuiltinCostModel.fromPlutusParams(paramsV1, Language.PlutusV1, BuiltinSemanticsVariant.B)
        BuiltinCostModel.fromPlutusParams(paramsV2, Language.PlutusV2, BuiltinSemanticsVariant.B)
        BuiltinCostModel.fromPlutusParams(paramsV3, Language.PlutusV3, BuiltinSemanticsVariant.C)
        assert(v1.size == 166)
        assert(v2.size == 175)
        assert(v3.size == 297)
    }

    test("BuiltinCostModel from Blockfrost pre-Plomin HF Protocol Parameters epoch 507") {
        val pparams = ProtocolParams.fromBlockfrostJson(
          this.getClass.getResourceAsStream("/blockfrost-params-epoch-507.json")
        )
        val v1 = pparams.costModels.models(Language.PlutusV1.languageId)
        val v2 = pparams.costModels.models(Language.PlutusV2.languageId)
        val v3 = pparams.costModels.models(Language.PlutusV3.languageId)
        val paramsV1 = PlutusV1Params.fromSeq(v1)
        val paramsV2 = PlutusV2Params.fromSeq(v2)
        val paramsV3 = PlutusV3Params.fromSeq(v3)
        BuiltinCostModel.fromPlutusParams(paramsV1, Language.PlutusV1, BuiltinSemanticsVariant.B)
        BuiltinCostModel.fromPlutusParams(paramsV2, Language.PlutusV2, BuiltinSemanticsVariant.B)
        BuiltinCostModel.fromPlutusParams(paramsV3, Language.PlutusV3, BuiltinSemanticsVariant.C)
        assert(v1.size == 166)
        assert(v2.size == 175)
        assert(v3.size == 251)
        assert(paramsV1.`addInteger-cpu-arguments-intercept` == 100788)
        // not available pre-Plomin HF
        assert(paramsV2.`integerToByteString-cpu-arguments-c0` == 300_000_000L)
        assert(paramsV2.`byteStringToInteger-cpu-arguments-c0` == 300_000_000L)
        assert(paramsV3.`andByteString-cpu-arguments-slope1` == 300_000_000L)
    }

    test("BuiltinCostModel from Blockfrost Plomin HF Protocol Parameters epoch 544") {
        val pparams = ProtocolParams.fromBlockfrostJson(
          this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
        )
        val v1 = pparams.costModels.models(Language.PlutusV1.languageId)
        val v2 = pparams.costModels.models(Language.PlutusV2.languageId)
        val v3 = pparams.costModels.models(Language.PlutusV3.languageId)
        val paramsV1 = PlutusV1Params.fromSeq(v1)
        val paramsV2 = PlutusV2Params.fromSeq(v2)
        val paramsV3 = PlutusV3Params.fromSeq(v3)
        BuiltinCostModel.fromPlutusParams(paramsV1, Language.PlutusV1, BuiltinSemanticsVariant.B)
        BuiltinCostModel.fromPlutusParams(paramsV2, Language.PlutusV2, BuiltinSemanticsVariant.B)
        BuiltinCostModel.fromPlutusParams(paramsV3, Language.PlutusV3, BuiltinSemanticsVariant.C)
        assert(v1.size == 166)
        assert(v2.size == 175)
        assert(v3.size == 297)
        assert(paramsV1.`addInteger-cpu-arguments-intercept` == 100788)
        // for some reason, these values are absent in Blockfrost params
        assert(paramsV2.`integerToByteString-cpu-arguments-c0` == 300_000_000L)
        assert(paramsV2.`byteStringToInteger-cpu-arguments-c0` == 300_000_000L)
        assert(paramsV3.`andByteString-cpu-arguments-slope1` == 726)
    }
