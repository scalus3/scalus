package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite

class AbbreviateErrorTracesSpec extends AnyFunSuite {

    private val emptyAnns = AnnotationsDecl.empty

    test("transform simple error message to abbreviation") {
        val transformer = new AbbreviateErrorTraces
        val input = SIR.Error("Failed validation", emptyAnns)
        val transformed = transformer.transformSIR(input)

        assert(transformed == SIR.Error("FV", emptyAnns))
        assert(transformer.getAbbreviationMap == Map("FV" -> "Failed validation"))
    }

    test("handle multiple error messages with same abbreviation base") {
        val transformer = new AbbreviateErrorTraces
        val input = SIR.Let(
          List(
            Binding(
              "err1",
              SIRType.TypeNothing,
              SIR.Error("First validation", emptyAnns)
            ),
            Binding(
              "err2",
              SIRType.TypeNothing,
              SIR.Error("Failed validation", emptyAnns)
            ),
            Binding(
              "err3",
              SIRType.TypeNothing,
              SIR.Error("Further validation", emptyAnns)
            )
          ),
          SIR.Error("Final validation", emptyAnns),
          SIR.LetFlags.None,
          emptyAnns
        )

        val transformed = transformer.transformSIR(input)
        val abbrevMap = transformer.getAbbreviationMap

        transformed match
            case SIR.Let(bindings, body, _, _) =>
                assert(bindings(0).value == SIR.Error("FV", emptyAnns))
                assert(bindings(1).value == SIR.Error("FV1", emptyAnns))
                assert(bindings(2).value == SIR.Error("FV2", emptyAnns))
                assert(body == SIR.Error("FV3", emptyAnns))
            case _ => fail("Expected Let expression")

        assert(
          abbrevMap == Map(
            "FV" -> "First validation",
            "FV1" -> "Failed validation",
            "FV2" -> "Further validation",
            "FV3" -> "Final validation"
          )
        )
    }

    test("reuse same abbreviation for duplicate messages") {
        val transformer = new AbbreviateErrorTraces
        val input = SIR.Let(
          List(
            Binding(
              "err1",
              SIRType.TypeNothing,
              SIR.Error("Failed validation", emptyAnns)
            ),
            Binding(
              "err2",
              SIRType.TypeNothing,
              SIR.Error("Failed validation", emptyAnns)
            )
          ),
          SIR.Var("x", SIRType.Integer, emptyAnns),
          SIR.LetFlags.None,
          emptyAnns
        )

        val transformed = transformer.transformSIR(input)

        transformed match
            case SIR.Let(bindings, _, _, _) =>
                val err1 = bindings(0).value.asInstanceOf[SIR.Error].msg
                val err2 = bindings(1).value.asInstanceOf[SIR.Error].msg
                assert(err1 == err2)
                assert(transformer.getAbbreviationMap.size == 1)
            case _ => fail("Expected Let expression")

    }

    test("handle complex nested error transformations") {
        val transformer = new AbbreviateErrorTraces
        val input = SIR.IfThenElse(
          cond = SIR.Error("Failed condition", emptyAnns),
          t = SIR.Let(
            List(
              Binding("x", SIRType.TypeNothing, SIR.Error("First branch", emptyAnns))
            ),
            SIR.Error("First branch", emptyAnns), // Duplicate message
            SIR.LetFlags.None,
            emptyAnns
          ),
          f = SIR.Error("Failed branch", emptyAnns),
          tp = SIRType.TypeNothing,
          emptyAnns
        )

        val transformed = transformer.transformSIR(input)
        val abbrevMap = transformer.getAbbreviationMap

        transformed match
            case SIR.IfThenElse(cond, t, f, _, _) =>
                assert(cond == SIR.Error("FC", emptyAnns))

                t match
                    case SIR.Let(bindings, body, _, _) =>
                        assert(bindings(0).value == SIR.Error("FB", emptyAnns))
                        assert(
                          body == SIR.Error("FB", emptyAnns)
                        ) // Should reuse same abbreviation
                    case _ => fail("Expected Let expression")

                assert(f == SIR.Error("FB1", emptyAnns))
            case _ => fail("Expected IfThenElse expression")

        assert(abbrevMap.size == 3)
        assert(
          abbrevMap == Map(
            "FC" -> "Failed condition",
            "FB" -> "First branch",
            "FB1" -> "Failed branch"
          )
        )
    }
}
