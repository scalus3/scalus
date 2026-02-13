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
        val validNames = List("foo", "bar123", "foo_bar", "foo'", "name-123")
        for name <- validNames do
            val sanitized = sanitizeName(name)
            assert(sanitized == name, s"Valid name '$name' should not be changed")
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
