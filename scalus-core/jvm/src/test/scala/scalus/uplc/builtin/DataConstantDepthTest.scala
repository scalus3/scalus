package scalus.uplc.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.FlatDecodeOracle.{allAt9To11, assertAgrees, Row}
import scalus.uplc.FlatProgramBuilder.*
import scalus.uplc.Constant
import scalus.uplc.test.SmallStack

/** A `data` constant has no limit on its nesting in the Plutus reference: `decodeData`
  * (`PlutusCore/Data.hs`) reads it with no depth bound, and the only size limit is 64 bytes for a
  * byte string or a chunk of one.
  */
class DataConstantDepthTest extends AnyFunSuite {

    // The reference's verdicts on these scripts, see `FlatDecodeOracle`.
    private val rows = Seq(
      Row(
        "data-depth-1001",
        "be7d09504f71e5f3e27b8776f6cb495acdd669036f1ed592045e45f5004ca539",
        allAt9To11,
        ""
      ),
      Row(
        "data-depth-1001-result",
        "b09433123908dfcd573f4fd9e623f2b32d68c7c4267d31644ba45a6450595d0f",
        allAt9To11,
        ""
      ),
      Row(
        "data-depth-1001-serialised",
        "53ac8250d80be67bcc5e2bfcd128a0ac0e21e9fbae0d14293e55cdb247d5db5e",
        "2/9 3/9 2/10 3/10 1/11 2/11 3/11",
        "1/9 1/10"
      ),
      Row(
        "data-depth-1003",
        "c7dc5566502eef527a31724128ed1dbb68f02df875d1d19a5ed9657dbe86ab86",
        allAt9To11,
        ""
      ),
      Row(
        "data-depth-1003-result",
        "5c77445a5253127b61acc1a30238d790141c4850b24f493f77e55b0855f35baa",
        allAt9To11,
        ""
      ),
      Row(
        "data-depth-1003-serialised",
        "02b83338583e1f3d2aa9383e1e13c761ca71aab40be3a8578adf108f38e60e3b",
        "2/9 3/9 2/10 3/10 1/11 2/11 3/11",
        "1/9 1/10"
      ),
      Row(
        "data-depth-16000",
        "20abb2da518aa6d5292b0fb42f712c2a5b89305b8a07627dc2635a06cb995b60",
        allAt9To11,
        ""
      ),
      Row(
        "data-depth-16000-result",
        "e212d7a3ee7dc98b6a3a4b0565f492146409feb374e2b317d836a41241703e71",
        allAt9To11,
        ""
      ),
      Row(
        "data-depth-16000-serialised",
        "fcf43739f3e4ddb40ee4feb471b2b3e271456acbc888397190ab3e75eb73d6ab",
        "2/9 3/9 2/10 3/10 1/11 2/11 3/11",
        "1/9 1/10"
      ),
      Row(
        "data-depth-1003-equal",
        "46ac5e4c42878315639da777a8b2236b113b7a03fc7ba2574b6834cc41989e7f",
        allAt9To11,
        ""
      ),
    )

    private val depths = Seq(1001, 1003, 16000)

    private val serialiseDataTag = 51
    private val equalsDataTag = 47

    private val scripts: Map[String, Array[Byte]] = {
        val nested = depths.flatMap { levels =>
            val deep = data(nestedList(levels))
            Seq(
              s"data-depth-$levels" -> script(discard(deep)),
              s"data-depth-$levels-result" -> script(deep),
              s"data-depth-$levels-serialised" ->
                  script(discard(Ap(Builtin(serialiseDataTag), deep)))
            )
        }
        val equal = "data-depth-1003-equal" -> script(
          discard(Ap(Ap(Builtin(equalsDataTag), data(nestedList(1003))), data(nestedList(1003))))
        )
        (nested :+ equal).toMap
    }

    test("scripts with deeply nested data constants are admitted as the reference admits them") {
        SmallStack.run(assertAgrees(rows, scripts))
    }

    /** The number of lists around the integer 0, counted with a loop: `==` and `hashCode` of `Data`
      * recurse once per level.
      */
    private def levelsAround(zero: Data): Int = {
        var levels = 0
        var current = zero
        var continue = true
        while continue do
            current match
                case Data.List(items) =>
                    items.toScalaList match
                        case inner :: Nil =>
                            levels += 1
                            current = inner
                        case _ => continue = false
                case _ => continue = false
        assert(current == Data.I(0))
        levels
    }

    // `DataCborDepthTest` decodes and encodes deep data constants on the default stack; here a whole
    // program is decoded on a stack too small for one frame per level
    for levels <- depths do
        test(s"a data constant of $levels levels decodes to as many nested lists") {
            SmallStack.run(decodeDiscarded(data(nestedList(levels)))) match
                case Constant.Data(d) => assert(levelsAround(d) == levels)
                case other            => fail(s"not a data constant: $other")
        }
}
