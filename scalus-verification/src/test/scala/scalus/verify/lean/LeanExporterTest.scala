package scalus.verify.lean

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString
import scalus.verify.Prop
import scalus.verify.Props.*

class LeanExporterTest extends AnyFunSuite {

    test("exports Boolean and BigInt propositions") {
        assert(LeanExporter(Prop(true)) == "(true = true)")

        val quantified = forAll[BigInt](x => x + 1 > 0)
        val lean = LeanExporter(quantified)
        assert(lean.startsWith("(∀ ("), lean)
        assert(lean.contains(" : Integer), "), lean)
        assert(lean.contains("+ 1"), lean)
        assert(lean.contains("decide"), lean)
        assert(lean.endsWith("= true))"), lean)
    }

    test("exports logical proposition nodes") {
        val prop = Prop(true) ==> (!Prop(false) || Prop(true))
        assert(
          LeanExporter(prop) ==
              "((true = true) → ((¬ (false = true)) ∨ (true = true)))"
        )
    }

    test("rejects unsupported data types") {
        val prop = forAll[ByteString](_ => true)
        val error = intercept[UnsupportedOperationException](LeanExporter(prop))
        assert(error.getMessage.contains("data type ByteString"), error.getMessage)
    }
}
