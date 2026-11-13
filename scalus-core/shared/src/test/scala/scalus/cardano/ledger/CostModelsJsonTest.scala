package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.internal.JsonTree

class CostModelsJsonTest extends AnyFunSuite {

    private val mainnet = CardanoInfo.mainnet.protocolParams.costModels

    test("cardano-cli cost models round-trip") {
        assert(CostModels.fromCardanoCliJson(CostModels.toCardanoCliJson(mainnet)) == mainnet)
    }

    test("the writer keys languages by name, in language order, each an array") {
        val tree = JsonTree.parse(CostModels.toCardanoCliJson(mainnet))
        assert(tree.obj.keys.toList == List("PlutusV1", "PlutusV2", "PlutusV3"))
        assert(tree.obj.values.forall(_.arrOpt.isDefined))
    }

    test("the cost models are the costModels field of the cardano-cli protocol parameters") {
        val params =
            JsonTree.parse(ProtocolParams.toCardanoCliJson(CardanoInfo.mainnet.protocolParams))
        val field = JsonTree.render(params("costModels"))
        assert(field == CostModels.toCardanoCliJson(mainnet))
        assert(CostModels.fromCardanoCliJson(field) == mainnet)
    }

    test("a parameter past 2^53 reads and writes exactly") {
        val big = (1L << 53) + 1
        val json = s"""{"PlutusV3": [1, $big, -900]}"""
        val costModels = CostModels.fromCardanoCliJson(json)
        assert(costModels == CostModels(Map(2 -> IndexedSeq(1L, big, -900L))))
        assert(CostModels.toCardanoCliJson(costModels) == s"""{"PlutusV3":[1,$big,-900]}""")
    }

    test("an unknown language fails and names it") {
        val thrown = intercept[IllegalArgumentException](
          CostModels.fromCardanoCliJson("""{"PlutusV9": [1]}""")
        )
        assert(
          thrown.getMessage.contains("costModels.PlutusV9: unknown Plutus language \"PlutusV9\""),
          thrown.getMessage
        )
    }

    test("a language whose parameters are not an array fails and names it") {
        val thrown = intercept[IllegalArgumentException](
          CostModels.fromCardanoCliJson("""{"PlutusV1": {"op0": 1}}""")
        )
        assert(
          thrown.getMessage.contains("costModels.PlutusV1: expected an array, got an object"),
          thrown.getMessage
        )
    }

    test("malformed JSON fails and says which shape was expected") {
        val thrown = intercept[IllegalArgumentException](CostModels.fromCardanoCliJson("[1, 2"))
        assert(thrown.getMessage.contains("as cardano-cli writes them"), thrown.getMessage)
    }

    test("the reader agrees with the deprecated upickle codec") {
        import LegacyProtocolParamsCodecs.legacyCostModels
        val json = CostModels.toCardanoCliJson(mainnet)
        assert(CostModels.fromCardanoCliJson(json) == legacyCostModels(json))
    }
}
