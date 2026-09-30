package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.{ByteString, Data}

/** The values below are Blaster's counterexamples as Z3 printed them, joined into one line. */
class SmtValuesTest extends AnyFunSuite {

    private val data = "PlutusCore.Data.PlutusCore.DataInternal.Data"
    private val bytes = "PlutusCore.ByteString.PlutusCore.ByteStringInternal.ByteString.mk"
    private val dataList = s"(as List.nil (@List @$data))"

    test("integers and Booleans") {
        assert(SmtValues.integer("42") == Right(BigInt(42)))
        assert(SmtValues.integer("(- 1)") == Right(BigInt(-1)))
        assert(SmtValues.boolean("true") == Right(true))
        assert(SmtValues.integer("x").isLeft)
    }

    test("every Data constructor, nested") {
        assert(SmtValues.data(s"($data.I (- 1))") == Right(Data.I(-1)))
        assert(
          SmtValues.data(s"($data.Constr 2 (List.cons ($data.I 3) $dataList))") ==
              Right(Data.Constr(2, PList(Data.I(3))))
        )
        assert(
          SmtValues.data(
            s"($data.List (List.cons ($data.I 4) (List.cons ($data.I 3) $dataList)))"
          ) == Right(Data.List(PList(Data.I(4), Data.I(3))))
        )
        assert(
          SmtValues.data(
            s"($data.Map (List.cons (Prod.mk ($data.I 3) ($data.I 4)) " +
                s"(as List.nil (@List (@Prod @$data @$data)))))"
          ) == Right(Data.Map(PList(Data.I(3) -> Data.I(4))))
        )
        assert(
          SmtValues.data(s"""($data.B ($bytes "ABC"))""") ==
              Right(Data.B(ByteString.fromString("ABC")))
        )
    }

    test("string literals follow SMT-LIB: a doubled quote, and code points in \\u{...}") {
        assert(
          SmtValues.data(s"""($data.B ($bytes "a""b\\u{ff}\\u0001\\x"))""") ==
              Right(
                Data.B(ByteString.fromArray(Array('a', '"', 'b', 0xff, 1, '\\', 'x').map(_.toByte)))
              )
        )
        // A character above 255 is no byte.
        assert(SmtValues.data(s"""($data.B ($bytes "\\u{100}"))""").isLeft)
    }

    test("malformed terms are reported, not thrown") {
        assert(SmtValues.data(s"($data.I 1").isLeft)
        assert(SmtValues.data(s"""($data.B ($bytes "abc))""").isLeft)
        assert(SmtValues.data(s"($data.Unknown 1)").isLeft)
        assert(SmtValues.data("(Prod.mk 1 2)").isLeft)
    }
}
