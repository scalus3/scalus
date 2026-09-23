package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import upickle.default.{read, write}

class PlutusParamsJsonTest extends AnyFunSuite {
    private val costs = (1L to 332L).toVector
    private val v1 = PlutusV1Params.fromSeq(costs)
    private val v2 = PlutusV2Params.fromSeq(costs)

    private def prefixJson(json: String, size: Int): String =
        ujson.write(ujson.Obj.from(ujson.read(json).obj.iterator.take(size)))

    test("V1 reads historical JSON without appended parameters") {
        val decoded = read[PlutusV1Params](prefixJson(v1.toJson, 166))
        assert(PlutusV1Params.toSeq(decoded).take(166) == costs.take(166))
        assert(PlutusV1Params.toSeq(decoded).drop(166).forall(_ == Long.MaxValue))
    }

    test("V2 reads historical JSON without appended parameters") {
        for size <- Seq(175, 185) do
            val decoded = read[PlutusV2Params](prefixJson(v2.toJson, size))
            assert(PlutusV2Params.toSeq(decoded).take(size) == costs.take(size))
            assert(PlutusV2Params.toSeq(decoded).drop(size).forall(_ == Long.MaxValue))
    }

    test("V1 and V2 JSON round trips retain every supplied parameter") {
        assert(PlutusV1Params.toSeq(read[PlutusV1Params](write(v1))) == costs)
        assert(PlutusV2Params.toSeq(read[PlutusV2Params](write(v2))) == costs)
    }
}
