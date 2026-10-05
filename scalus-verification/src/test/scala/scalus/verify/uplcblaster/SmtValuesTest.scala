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
        // A character above 255 is no byte: the term is a value of Lean's model, which stores a
        // byte string as a string, and none of a byte string. The term itself is well formed.
        SmtValues.data(s"""($data.B ($bytes "\\u{100}"))""") match
            case Left(SmtValues.Unreadable.OutsideType(reason)) =>
                assert(reason.contains("U+100"), reason)
            case other => fail(s"expected a value outside the type, got $other")
        SmtValues.bytes(s"""($bytes "\\u{100}")""") match
            case Left(SmtValues.Unreadable.OutsideType(_)) =>
            case other => fail(s"expected a value outside the type, got $other")
        assert(SmtValues.bytes("42").left.exists(_.isInstanceOf[SmtValues.Unreadable.Malformed]))
    }

    test("a value the model leaves unconstrained reads as a default, alone or nested") {
        assert(SmtValues.integer("$0") == Right(BigInt(0)))
        assert(SmtValues.boolean("$1") == Right(false))
        assert(SmtValues.data("$2") == Right(Data.I(0)))
        assert(
          SmtValues.data(s"($data.List (List.cons ($data.I 3) $$3))") ==
              Right(Data.List(PList(Data.I(3))))
        )
        assert(SmtValues.data(s"($data.Constr $$4 $$5)") == Right(Data.Constr(0, PList.Nil)))
        assert(SmtValues.data(s"($data.B $$6)") == Right(Data.B(ByteString.empty)))
    }

    test("a large value is abbreviated with let, whose names stand for their terms") {
        // As Z3 prints a long list: the second let uses the name the first one bound.
        val abbreviated =
            s"(let ((a!1 (List.cons ($data.I 3) (List.cons ($data.I 4) $dataList)))) " +
                s"(let ((a!2 (List.cons ($data.I 1) a!1))) " +
                s"($data.List (List.cons ($data.Constr 6 $dataList) a!2))))"
        assert(
          SmtValues.data(abbreviated) == Right(
            Data.List(PList(Data.Constr(6, PList.Nil), Data.I(1), Data.I(3), Data.I(4)))
          )
        )
        assert(SmtValues.integer("(let ((a!1 (- 7))) a!1)") == Right(BigInt(-7)))
        // A binding is read in the scope around its let, not in the let's own.
        assert(SmtValues.integer("(let ((a 1)) (let ((a 2) (b a)) b))") == Right(BigInt(1)))
        assert(SmtValues.integer("(let (a) 1)").isLeft)
    }

    test("a term is whole when its parentheses are closed, outside string literals") {
        assert(SmtValues.complete("42"))
        assert(SmtValues.complete("(- 1)"))
        assert(!SmtValues.complete(s"(let ((a!1 (List.cons ($data.I 3) $dataList)))"))
        assert(!SmtValues.complete(s"""($data.B ($bytes ")")"""))
        assert(SmtValues.complete(s"""($data.B ($bytes ")"))"""))
    }

    test("malformed terms are reported, not thrown") {
        assert(SmtValues.data(s"($data.I 1").isLeft)
        assert(SmtValues.data(s"""($data.B ($bytes "abc))""").isLeft)
        assert(SmtValues.data(s"($data.Unknown 1)").isLeft)
        assert(SmtValues.data("(Prod.mk 1 2)").isLeft)
    }
}
