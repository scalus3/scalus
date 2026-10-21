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

    test("a string is a string literal, and a surrogate is no character of one") {
        assert(SmtValues.string("\"ab\"\"c\"") == Right("ab\"c"))
        assert(
          SmtValues.string("\"\\u{e9}\\u20ac\\u{1f600}\"") == Right("\u00e9\u20ac\uD83D\uDE00")
        )
        assert(SmtValues.string("$0") == Right(""))
        // The solver's strings have surrogates, and two of them are two characters, not one pair.
        for surrogates <- List("\"\\u{d800}\"", "\"\\u{d83d}\\u{de00}\"") do
            SmtValues.string(surrogates) match
                case Left(SmtValues.Unreadable.OutsideType(reason)) =>
                    assert(reason.contains("surrogate U+d8"), reason)
                case other => fail(s"expected a value outside the type, got $other")
        assert(SmtValues.string("42").left.exists(_.isInstanceOf[SmtValues.Unreadable.Malformed]))
    }

    test("a reason writes a term as the solver does, so that it reads back as it was") {
        // A quote is doubled, and a backslash is an escape: the text `\u{41}` is no `A`.
        SmtValues.bytes(s"""($bytes "a""b\\u{5c}u{41}\\u{100}")""") match
            case Left(SmtValues.Unreadable.OutsideType(reason)) =>
                assert(reason.contains("\"a\"\"b\\u{5c}u{41}\\u{100}\""), reason)
            case other => fail(s"expected a value outside the type, got $other")
        // A term that is no value of the type is written as a term, not as the parser holds it.
        SmtValues.string("(f \"AB\" 1)") match
            case Left(SmtValues.Unreadable.Malformed(reason)) =>
                assert(reason == "expected a string, got (f \"AB\" 1)", reason)
            case other => fail(s"expected a malformed term, got $other")
    }

    test("the characters of a term are those of its byte strings and those of its strings") {
        assert(
          SmtValues.characters(
            s"""($data.List (List.cons ($data.B ($bytes "a\\u{100}")) $dataList))"""
          ) == Right(SmtValues.Characters(Set('a', 0x100), Set.empty))
        )
        assert(
          SmtValues.characters("\"x\\u{d800}\"") ==
              Right(SmtValues.Characters(Set.empty, Set('x', 0xd800)))
        )
        assert(SmtValues.characters("$0") == Right(SmtValues.Characters.none))
    }

    test("a renaming reads the characters that are none of the type as others") {
        val renaming = SmtValues.Renaming(Map(0x100 -> 7), Map(0xd800 -> 0xe000))
        // in a byte string, also inside Data, and in a string
        assert(
          SmtValues.bytes(s"""($bytes "a\\u{100}")""", renaming) ==
              Right(ByteString.fromArray(Array[Byte]('a', 7)))
        )
        assert(
          SmtValues.data(s"""($data.B ($bytes "\\u{100}"))""", renaming) ==
              Right(Data.B(ByteString.fromArray(Array[Byte](7))))
        )
        assert(
          SmtValues.string("\"x\\u{d800}\"", renaming) ==
              Right("x" + Character.toString(0xe000))
        )
        // A character it does not rename is still outside the type.
        assert(
          SmtValues
              .bytes(s"""($bytes "\\u{101}")""", renaming)
              .left
              .exists(_.isInstanceOf[SmtValues.Unreadable.OutsideType])
        )
    }

    test("a constructor of Data with a tag below 0 is a value of Lean's model only") {
        // Lean's `Data` takes any integer for the tag, and Scalus's none below 0.
        SmtValues.data(s"($data.Constr (- 1) $dataList)") match
            case Left(SmtValues.Unreadable.OutsideType(reason)) =>
                assert(reason.contains("-1"), reason)
            case other => fail(s"expected a value outside the type, got $other")
        // Inside another value as well.
        SmtValues.data(s"($data.List (List.cons ($data.Constr (- 7) $dataList) $dataList))") match
            case Left(SmtValues.Unreadable.OutsideType(_)) =>
            case other => fail(s"expected a value outside the type, got $other")
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
