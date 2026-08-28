package scalus.tsexport

import org.scalatest.funsuite.AnyFunSuite

class DocConverterTest extends AnyFunSuite {
    test("strips frame and stars, keeps body") {
        val doc = DocConverter.convert("/** Hello world. */").get
        assert(doc.lines == List("Hello world."))
    }

    test("multi-line with params, return, links, deprecated") {
        val raw =
            """/** Evaluates a script.
              |  *
              |  * See [[evaluateScript]] and [[scalus.uplc.eval.JScalus]].
              |  *
              |  * @param doubleCborHex
              |  *   the script hex
              |  * @param data
              |  *   the argument
              |  * @return
              |  *   the result
              |  * @deprecated Use evaluate instead
              |  */""".stripMargin
        val doc = DocConverter.convert(raw).get
        assert(doc.lines.head == "Evaluates a script.")
        // convert leaves links alone; resolveLinks needs the full set of declaration names
        assert(doc.lines.contains("See [[evaluateScript]] and [[scalus.uplc.eval.JScalus]]."))
        // scaladoc's continuation-line style folds into one TSDoc tag line
        assert(doc.lines.contains("@param doubleCborHex the script hex"))
        assert(doc.lines.contains("@param data the argument"))
        assert(doc.lines.contains("@returns the result"))
        assert(doc.lines.contains("@deprecated Use evaluate instead"))
    }

    test("collapses blank runs, drops leading/trailing blanks") {
        val raw =
            """/**
              |  * First.
              |  *
              |  *
              |  * Second.
              |  *
              |  */""".stripMargin
        val doc = DocConverter.convert(raw).get
        assert(doc.lines == List("First.", "", "Second."))
    }

    test("splits the @constructor section out of a class doc") {
        val doc = DocConverter
            .convert("""/** A rectangle.
                       |  *
                       |  * @constructor
                       |  *   Creates a rectangle.
                       |  * @param width
                       |  *   the width
                       |  */""".stripMargin)
            .get
        val (clsDoc, ctorDoc) = DocConverter.splitConstructorTag(doc)
        assert(ctorDoc.map(_.lines) == Some(List("Creates a rectangle.")))
        assert(clsDoc.map(_.lines) == Some(List("A rectangle.", "", "@param width the width")))
        // a doc without the tag is returned unchanged, with no constructor doc
        val plain = DocConverter.convert("/** Just a class. */").get
        assert(DocConverter.splitConstructorTag(plain) == (Some(plain), None))
    }

    test("links resolve to exported names and degrade to code otherwise") {
        val doc = TsDoc(
          List(
            "See [[evaluateScript]] and [[scalus.uplc.eval.JScalus]].",
            "A target with brackets: [[Map[K, V]]].",
            "Two on one line: [[A]] and [[B]]."
          )
        )
        val known = Map("evaluateScript" -> "evaluate", "A" -> "A")
        val out = DocConverter.resolveLinks(doc, known.get)
        assert(out.lines.head == "See {@link evaluate} and `scalus.uplc.eval.JScalus`.")
        // the reluctant match must not stop on the inner ]]
        assert(out.lines(1) == "A target with brackets: `Map[K, V]`.")
        assert(out.lines(2) == "Two on one line: {@link A} and `B`.")
    }

    test("markdown lists and fenced code survive tag folding") {
        val raw =
            """/** Runs a script.
              |  *
              |  * @param mode
              |  *   one of
              |  *   - fast
              |  *   - slow
              |  * @example
              |  * ```ts
              |  * const r = run("fast");
              |  * // see [[nothing]] and @notATag
              |  * ```
              |  * @returns the result
              |  */""".stripMargin
        val doc = DocConverter.convert(raw).get
        assert(doc.lines.contains("@param mode one of"))
        assert(doc.lines.exists(_.trim == "- fast"))
        assert(doc.lines.exists(_.trim == "- slow"))
        assert(doc.lines.contains("@example"))
        assert(doc.lines.contains("```ts"))
        assert(doc.lines.contains("const r = run(\"fast\");"))
        assert(doc.lines.contains("@returns the result"))
        // fenced content is verbatim: no link rewriting inside it
        val resolved = DocConverter.resolveLinks(doc, _ => Some("X"))
        assert(resolved.lines.contains("// see [[nothing]] and @notATag"))
    }

    test("maps @tparam to @typeParam and keeps @throws") {
        val raw =
            """/** Picks one.
              |  *
              |  * @tparam A
              |  *   the element type
              |  * @throws IllegalArgumentException
              |  *   if the list is empty
              |  */""".stripMargin
        val doc = DocConverter.convert(raw).get
        assert(doc.lines.contains("@typeParam A the element type"))
        assert(doc.lines.contains("@throws IllegalArgumentException if the list is empty"))
    }

    test("empty comment is None") {
        assert(DocConverter.convert("/** */").isEmpty)
        assert(DocConverter.convert("").isEmpty)
    }
}
