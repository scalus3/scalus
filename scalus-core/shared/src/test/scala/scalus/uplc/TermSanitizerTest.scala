package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.Word64
import scalus.uplc.Term.*
import scalus.uplc.TermSanitizer.*

class TermSanitizerTest extends AnyFunSuite:

    test("Sanitize name with dots") {
        val original = "foo.bar"
        val sanitized = sanitizeName(original)
        assert(sanitized == "foo_bar")
    }

    test("Sanitize name with dollar signs") {
        val original = "anonfun$1"
        val sanitized = sanitizeName(original)
        assert(sanitized == "anonfun_1")
    }

    test("Sanitize name with colons") {
        val original = "foo:bar"
        val sanitized = sanitizeName(original)
        assert(sanitized == "foo_bar")
    }

    test("Sanitize name starting with digit") {
        val original = "1foo"
        val sanitized = sanitizeName(original)
        assert(sanitized == "_1foo")
    }

    test("Sanitize complex name with multiple special characters") {
        val original = "com.example.MyClass$Inner"
        val sanitized = sanitizeName(original)
        assert(sanitized == "com_example_MyClass_Inner")
    }

    test("Preserve valid names") {
        val validNames = List("foo", "bar123", "foo_bar", "foo'")
        for name <- validNames do
            val sanitized = sanitizeName(name)
            assert(sanitized == name, s"Valid name '$name' should not be changed")
    }

    // Reference plutus-core parser (plutus-core-1.45.0.0,
    // PlutusCore.Parser.ParserCommon.name / PlutusCore.Name.Unique) grammar for an unquoted,
    // sanitized name: ASCII letter or '_' first, then any number of ASCII letter/digit/'_'/'\''.
    // '-' never appears in sanitizer output (see TermSanitizer's doc comment), so it is
    // deliberately excluded here even though the upstream grammar allows it as a terminal
    // '-\d+' suffix.
    private val referenceParserNameRegex = "^[A-Za-z_][A-Za-z0-9_']*$".r

    test("Hyphen followed by digits is replaced, not preserved as a Unique suffix") {
        // Regression test: TermSanitizer used to treat '-\d+' as an embeddable character class,
        // so a name that already carried a '-<id>' suffix and *also* collided with another name
        // (getting a `'<counter>` suffix from upstream of the sanitizer) round-tripped untouched,
        // e.g. "a-91533'653". The reference plutus-core parser treats '-<digits>' as a *terminal*
        // token component: nothing may follow it, so "a-91533'653" parses as "a-91533" plus a
        // stray, unparseable "'653". Every '-' must therefore become '_'.
        val cases = List(
          // '-' becomes '_', but the apostrophe that follows is a valid identifier char on its own
          // and is left untouched - it's only invalid when it trails a literal '-\d+' suffix.
          "a-91533'653" -> "a_91533'653",
          "x-1-2" -> "x_1_2",
          "foo-12" -> "foo_12",
          "9bar" -> "_9bar",
          "f$g" -> "f_g",
          "" -> "_empty"
        )
        for (original, expected) <- cases do
            val sanitized = sanitizeName(original)
            assert(
              sanitized == expected,
              s"sanitizeName('$original') was '$sanitized', expected '$expected'"
            )
            if sanitized != "_empty" then
                assert(
                  referenceParserNameRegex.matches(sanitized),
                  s"sanitizeName('$original') = '$sanitized' does not match the reference parser's name grammar"
                )
    }

    test("Names differing only by a hyphen-digit suffix stay distinct after uniqueification") {
        // "a-1" and "a_1" both sanitize to "a_1"; findUniqueName must keep them distinct.
        val term = λ("a-1", "a_1")(vr"a-1" $ vr"a_1")
        val sanitized = sanitizeNames(term)
        val names = sanitized match
            case LamAbs(n1, LamAbs(n2, _, _), _) => List(n1, n2)
            case other                           => fail(s"unexpected shape: $other")
        assert(names.distinct.size == names.size, s"names collided: $names")
        for n <- names do
            assert(
              referenceParserNameRegex.matches(n),
              s"sanitized name '$n' does not match the reference parser's name grammar"
            )
    }

    test("Sanitized names round-trip through UplcParser") {
        val term = λ("a-91533'653", "n-58086'641", "9bar", "f$g")(
          vr"a-91533'653" $ vr"n-58086'641" $ vr"9bar" $ vr("f$g")
        )
        val sanitized = sanitizeNames(term)
        val printed = sanitized.pretty.render(120)
        val parser = UplcParser()
        parser.parseTerm(printed) match
            case Right(parsed) => assert(parsed == sanitized)
            case Left(err)     => fail(s"failed to parse sanitized term back: $err\n$printed")
    }

    test("Handle empty name") {
        val original = ""
        val sanitized = sanitizeName(original)
        assert(sanitized == "_empty")
    }

    test("Sanitize term with simple lambda") {
        val term = λ("foo.bar")(vr"foo.bar")
        val sanitized = sanitizeNames(term)
        assert(sanitized == λ("foo_bar")(vr"foo_bar"))
    }

    test("Sanitize term with nested lambdas") {
        val term = λ("com.example.Class", "foo$bar")(vr"com.example.Class" $ vr("foo$bar"))
        val sanitized = sanitizeNames(term)
        assert(sanitized == λ("com_example_Class", "foo_bar")(vr"com_example_Class" $ vr"foo_bar"))
    }

    test("Handle name conflicts by appending numbers") {
        val term = λ("foo.bar", "foo_bar")(
          vr"foo.bar" $ vr"foo_bar"
        ) // "foo_bar" conflicts with sanitized "foo.bar"
        val sanitized = sanitizeNames(term)
        // "foo.bar" becomes "foo_bar", "foo_bar" becomes "foo_bar1"
        assert(sanitized == λ("foo_bar", "foo_bar1")(vr"foo_bar" $ vr"foo_bar1"))
    }

    test("Sanitize term with Apply") {
        val term = λ("foo.bar")(vr"foo.bar") $ λ("baz$qux")(vr("baz$qux"))
        val sanitized = sanitizeNames(term)
        assert(sanitized == (λ("foo_bar")(vr"foo_bar") $ λ("baz_qux")(vr"baz_qux")))
    }

    test("Sanitize term with Force and Delay") {
        val term = Force(Delay(λ("foo.bar")(vr"foo.bar")))
        val sanitized = sanitizeNames(term)
        assert(sanitized == Force(Delay(λ("foo_bar")(vr"foo_bar"))))
    }

    test("Sanitize term with Constr") {
        val term = Constr(Word64.Zero, List(λ("foo.bar")(vr"foo.bar"), λ("baz$qux")(vr("baz$qux"))))
        val sanitized = sanitizeNames(term)
        assert(
          sanitized == Constr(
            Word64.Zero,
            List(λ("foo_bar")(vr"foo_bar"), λ("baz_qux")(vr"baz_qux"))
          )
        )
    }

    test("Sanitize term with Case") {
        val term = Case(vr"foo.bar", List(λ("case1$")(vr("case1$")), λ("case2$")(vr("case2$"))))
        val sanitized = sanitizeNames(term)
        assert(
          sanitized == Case(vr"foo_bar", List(λ("case1_")(vr"case1_"), λ("case2_")(vr"case2_")))
        )
    }

    test("Preserve constants and builtins") {
        val term = Builtin(DefaultFun.AddInteger) $ Const(Constant.Integer(42))
        val sanitized = sanitizeNames(term)
        assert(sanitized == term)
    }

    test("Preserve Error") {
        val term = Error()
        val sanitized = sanitizeNames(term)
        assert(sanitized == Error())
    }

    test("Handle multiple occurrences of same invalid name") {
        val term = λ("x.y", "z")(vr"x.y" $ vr"z" $ vr"x.y")
        val sanitized = sanitizeNames(term)
        // All occurrences of "x.y" should be renamed to the same sanitized name
        assert(sanitized == λ("x_y", "z")(vr"x_y" $ vr"z" $ vr"x_y"))
    }

    test("Handle DeBruijn index preservation") {
        val term = λ("foo.bar")(Var(NamedDeBruijn("foo.bar", 1)))
        val sanitized = sanitizeNames(term)
        assert(sanitized == λ("foo_bar")(Var(NamedDeBruijn("foo_bar", 1))))
    }
