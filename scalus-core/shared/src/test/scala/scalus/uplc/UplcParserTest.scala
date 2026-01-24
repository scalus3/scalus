package scalus.uplc

import cats.implicits.toShow
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.language.implicitConversions
import scalus.*
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data
import scalus.cardano.ledger.Word64
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.Constant.given
import scalus.uplc.DefaultUni.ProtoList
import scalus.uplc.DefaultUni.ProtoPair
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.test.ArbitraryInstances

class UplcParserTest extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:
    def parser = UplcParser()
    test("Parse program version") {
        def p(input: String) = parser.programVersion.parse(input)
        def check(input: String, expected: (Int, Int, Int)) =
            p(input) match
                case Left(value)        => fail(value.show)
                case Right((_, result)) => assert(result == expected)
        check("111.2.33333   ", (111, 2, 33333))
        check("1.0.03   ", (1, 0, 3))
        assert(!p("1 . 2 . 3").isRight)
        assert(!p("1.2").isRight)
        assert(!p("1.2.a").isRight)
    }

    test("Parse var") {
        val r = parser.parseProgram("(program 1.0.0 x )")
        assert(
          r == Right(
            Program(version = (1, 0, 0), term = Var(NamedDeBruijn("x")))
          )
        )
    }

    test("Parse lam") {
        val r = parser.parseProgram("(program 1.0.0 (lam x x) )")
        assert(
          r == Right(
            Program(version = (1, 0, 0), term = LamAbs("x", Var(NamedDeBruijn("x"))))
          )
        )
    }

    test("Parse lam/app") {
        val r = parser.parseProgram("(program 1.0.0 [(lam x x) y z])")
        assert(
          r == Right(
            Program(
              version = (1, 0, 0),
              term = Apply(
                Apply(LamAbs("x", Var(NamedDeBruijn("x"))), Var(NamedDeBruijn("y"))),
                Var(NamedDeBruijn("z"))
              )
            )
          )
        )
    }

    test("Parse constr/case") {
        val r = parser.parseProgram("(program 1.1.0 (case (constr 0) (con bool True)))")
        assert(
          r == Right(
            Program((1, 1, 0), Case(Constr(Word64.Zero, List()), List(Const(Constant.Bool(true)))))
          )
        )
    }

    test("Parse constr/case with arguments") {
        val r = parser.parseProgram(
          "(program 1.1.0 (case (constr 0 (con integer 0)) (lam x (con integer 1)) (lam x (con integer 2))))"
        )
        assert(
          r == Right(
            Program(
              (1, 1, 0),
              Case(
                Constr(Word64.Zero, List(Const(Constant.Integer(0)))),
                List(
                  LamAbs("x", Const(Constant.Integer(1))),
                  LamAbs("x", Const(Constant.Integer(2)))
                )
              )
            )
          )
        )
    }

    test("Fail to parse constr before 1.1.0") {
        val r = parser.parseProgram("(program 1.0.0 (constr 0))")
        assert(r.swap.getOrElse("").contains("'constr' is not allowed before version 1.1.0"))
    }

    test("Fail to parse case before 1.1.0") {
        val r = parser.parseProgram("(program 1.0.0 (case (con bool True)))")
        assert(r.swap.getOrElse("").contains("'case' is not allowed before version 1.1.0"))
    }

    test("Parse constant types") {
        def p(input: String) = UplcParser.defaultUni.parse(input).map(_._2)
        assert(p("bool") == Right(DefaultUni.Bool))
        assert(p("bytestring") == Right(DefaultUni.ByteString))
        assert(p("data") == Right(DefaultUni.Data))
        assert(p("integer") == Right(DefaultUni.Integer))
        assert(
          p("(list integer )") == Right(DefaultUni.Apply(ProtoList, DefaultUni.Integer))
        )
        assert(
          p("(list (list unit) )") == Right(
            DefaultUni.Apply(
              ProtoList,
              DefaultUni.Apply(ProtoList, DefaultUni.Unit)
            )
          )
        )
        assert(
          p("(pair integer bool)") == Right(
            DefaultUni.Apply(
              DefaultUni.Apply(ProtoPair, DefaultUni.Integer),
              DefaultUni.Bool
            )
          )
        )
        assert(
          p("(pair (list (list unit)) (pair integer bool) )") == Right(
            DefaultUni.Apply(
              DefaultUni.Apply(
                ProtoPair,
                DefaultUni.Apply(
                  ProtoList,
                  DefaultUni.Apply(ProtoList, DefaultUni.Unit)
                )
              ),
              DefaultUni.Apply(DefaultUni.Apply(ProtoPair, DefaultUni.Integer), DefaultUni.Bool)
            )
          )
        )
        assert(p("string") == Right(DefaultUni.String))
        assert(p("unit") == Right(DefaultUni.Unit))
    }

    test("Parse data") {
        def p(input: String) = UplcParser.dataTerm.parse(input).map(_._2).left.map(e => e.show)
        assert(p("B #  ") == Right(Data.B(scalus.uplc.builtin.ByteString.empty)))
        assert(p("I 123 ") == Right(Data.I(123)))
        assert(
          p("Constr 0 [Constr 1 []] ") == Right(Data.Constr(0, PList(Data.Constr(1, PList.Nil))))
        )
    }

    test("Parse constants") {
        import cats.implicits.toShow
        def p(input: String) = UplcParser.conTerm.parse(input).map(_._2).left.map(e => e.show)
        assert(
          p("(con (list integer) [1,2, 000000000000000000000000000000000000012345])") == Right(
            Seq(1, 2, 12345): Term
          )
        )

        assert(
          p("(con (pair integer bool) (12, False))") == Right((12, false): Term)
        )

        assert(
          p(
            "(con (pair integer (pair data data)) (9223372036854775807, (B #4104, I -1)))"
          ) == Right(
            Const(
              Constant.Pair(
                Constant.Integer(BigInt("9223372036854775807")),
                Constant.Pair(Constant.Data(Data.B(hex"4104")), Constant.Data(Data.I(-1)))
              )
            )
          )
        )

        val r = parser.parseProgram("""(program 1.0.0 [
        |  (con bytestring #001234ff)
        |  (con bool True)
        |  (con bool False)
        |  (con unit () )
        |  (con string "x \8712 \8477 \8658 x\178 \8805 0; z \8712 \8450\\\8477 \8658 z\178 \8713 {x \8712 \8477: x \8805 0}.")
        |  ])""".stripMargin)
        assert(
          r == Right(
            Program(
              version = (1, 0, 0),
              term = scalus.uplc.builtin.ByteString.fromHex(
                "001234ff"
              ) $ true $ false $ () $ "x ∈ ℝ ⇒ x² ≥ 0; z ∈ ℂ\\ℝ ⇒ z² ∉ {x ∈ ℝ: x ≥ 0}."
            )
          )
        )
    }

    test("Parse delay/force/error") {
        val r = parser.parseProgram("(program 1.0.0 (force (delay (error))))")
        assert(
          r == Right(
            Program(version = (1, 0, 0), term = Force(Delay(Error)))
          )
        )
    }

    test("Parse builtins") {
        import cats.implicits.toShow
        def p(input: String) = UplcParser.builtinTerm.parse(input).map(_._2).left.map(e => e.show)

        assert(p("(builtin addInteger)") == Right(Builtin(DefaultFun.AddInteger)))
        assert(p("(builtin appendByteString)") == Right(Builtin(DefaultFun.AppendByteString)))
        assert(
          p("(builtin nonexistent)").swap
              .getOrElse("")
              .contains("unknown builtin function: nonexistent")
        )
    }

    test("Parse program") {
        val r = parser.parseProgram("(program 1.0.0 [(lam x x) (con integer 0)])")
        assert(
          r == Right(
            Program(
              version = (1, 0, 0),
              term = Apply(LamAbs("x", Var(NamedDeBruijn("x"))), Const(Constant.Integer(BigInt(0))))
            )
          )
        )
    }

    test("Pretty-printer <-> parser isomorphism for DefaultUni") {
        forAll { (t: DefaultUni) =>
            val pretty = t.pretty.render(80)
            val parsed = UplcParser.defaultUni.parse(pretty).map(_._2).left.map(e => e.show)
            assert(parsed == Right(t))
        }
    }

    test("Pretty-printer <-> parser isomorphism for Constant") {
        forAll { (t: Constant) =>
            val pretty = t.pretty.render(80)
            val parsed = UplcParser.constant.parse(pretty).map(_._2).left.map(e => e.show)
            assert(parsed == Right(t))
        }
    }

    test("Pretty-printer <-> parser isomorphism for Term") {
        forAll { (t: Term) =>
            val pretty = t.show
            val parsed = parser.term.parse(pretty).map(_._2).left.map(e => e.show)
            assert(parsed == Right(t))
        }
    }

    test("Pretty-printer <-> parser isomorphism for Program") {
        forAll { (t: Program) =>
            val pretty = t.show
            val parsed = parser.parseProgram(pretty)
            assert(parsed == Right(t))
        }
    }

    // Tests verifying that invalid variable names are sanitized and can be round-tripped
    test("Variable names with dots are sanitized and can be parsed") {
        val term = λ("foo.bar")(vr"foo.bar")
        val pretty = term.show
        val parsed = parser.term.parse(pretty).map(_._2)
        // The sanitizer should convert "foo.bar" to "foo_bar"
        assert(parsed == Right(λ("foo_bar")(vr"foo_bar")))
    }

    test("Variable names with dollar signs are sanitized and can be parsed") {
        val term = λ("anonfun$1")(vr("anonfun$1"))
        val pretty = term.show
        val parsed = parser.term.parse(pretty).map(_._2)
        // The sanitizer should convert "anonfun$1" to "anonfun_1"
        assert(parsed == Right(λ("anonfun_1")(vr"anonfun_1")))
    }

    test("Variable names with colons are sanitized and can be parsed") {
        val term = λ("foo:bar")(vr"foo:bar")
        val pretty = term.show
        val parsed = parser.term.parse(pretty).map(_._2)
        // The sanitizer should convert "foo:bar" to "foo_bar"
        assert(parsed == Right(λ("foo_bar")(vr"foo_bar")))
    }

    test("Variable names starting with digits are sanitized and can be parsed") {
        val term = λ("1foo")(vr"1foo")
        val pretty = term.show
        val parsed = parser.term.parse(pretty).map(_._2)
        // The sanitizer should convert "1foo" to "_1foo"
        assert(parsed == Right(λ("_1foo")(vr"_1foo")))
    }

    test("Complex nested term with invalid variable names is sanitized and can be parsed") {
        val term = λ("com.example.Class")(vr"com.example.Class") $ λ("foo$bar")(vr("foo$bar"))
        val pretty = term.show
        val parsed = parser.term.parse(pretty).map(_._2)
        // The sanitizer should convert the names appropriately
        assert(
          parsed == Right(λ("com_example_Class")(vr"com_example_Class") $ λ("foo_bar")(vr"foo_bar"))
        )
    }
