package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.compile
import scalus.uplc.*
import scalus.uplc.DefaultFun.AddInteger
import scalus.utils.ScalusSourcePos

class UplcSourceMapRendererTest extends AnyFunSuite {
    private val posA = ScalusSourcePos("/src/Foo.scala", 10, 2, 10, 7)
    private val posB = ScalusSourcePos("/src/Foo.scala", 12, 4, 12, 9)
    private val annA = UplcAnnotation(posA, "validate")
    private val annB = UplcAnnotation(posB, "")

    private val term = Term.Apply(
      Term.Apply(Term.Builtin(AddInteger, annA), Term.Const(Constant.Integer(1), annB)),
      Term.Const(Constant.Integer(2)),
      annA
    )

    test("uplc text equals plain show (markers fully stripped)") {
        val map = UplcSourceMapRenderer.render(term)
        assert(map.uplc == term.show)
    }

    test("spans point at the printed node text") {
        val map = UplcSourceMapRenderer.render(term)
        val builtinSpan =
            map.spans.find(sp => map.uplc.substring(sp.s, sp.e) == "(builtin addInteger)").get
        assert(map.files(builtinSpan.file) == "/src/Foo.scala")
        assert(
          builtinSpan.sl == 10 && builtinSpan.sc == 2 && builtinSpan.el == 10 && builtinSpan.ec == 7
        )
        assert(builtinSpan.fn.map(map.functions) == Some("validate"))
    }

    test("nodes without positions produce no spans") {
        val map = UplcSourceMapRenderer.render(term)
        // the '2' const has an empty annotation
        assert(!map.spans.exists(sp => map.uplc.substring(sp.s, sp.e) == "(con integer 2)"))
    }

    test("a node with a position but no function name gets a span without fn") {
        val map = UplcSourceMapRenderer.render(term)
        val constSpan =
            map.spans.find(sp => map.uplc.substring(sp.s, sp.e) == "(con integer 1)").get
        assert(constSpan.fn.isEmpty)
        assert(constSpan.sl == 12 && constSpan.sc == 4)
    }

    test("spans nest and offsets are within bounds") {
        val map = UplcSourceMapRenderer.render(term)
        assert(map.spans.nonEmpty)
        map.spans.foreach { sp =>
            assert(sp.s >= 0 && sp.e <= map.uplc.length && sp.s < sp.e)
        }
        // the root application encloses every other span
        val root = map.spans.maxBy(sp => sp.e - sp.s)
        map.spans.foreach(sp => assert(sp.s >= root.s && sp.e <= root.e))
    }

    test("post-order indices are stable under Apply wrapping") {
        // How a compiled script gets its parameters applied: the script is a lambda and the
        // wrapper Apply is added on top, so every node of the original term is still printed.
        val wrapped = Term.Apply(Term.LamAbs("p", term), Term.Const(Constant.Integer(3)))
        val base = UplcSourceMapRenderer.render(term)
        val wrap = UplcSourceMapRenderer.render(wrapped)
        val baseByPos = base.spans.map(sp => (sp.sl, sp.sc, sp.n)).toSet
        assert(baseByPos.size == base.spans.size)
        // every base span keeps its node index in the wrapped program
        baseByPos.foreach { case (sl, sc, n) =>
            assert(
              wrap.spans.exists(sp => sp.sl == sl && sp.sc == sc && sp.n == n),
              s"($sl, $sc, $n) missing from ${wrap.spans}"
            )
        }
    }

    test("an Apply-rooted term absorbed into an enclosing chain keeps its children's indices") {
        // The printer flattens application chains, so wrapping an Apply in another Apply drops
        // the inner node's own span. Every other node keeps its post-order index.
        val wrapped = Term.Apply(term, Term.Const(Constant.Integer(3)))
        val base = UplcSourceMapRenderer.render(term)
        val wrap = UplcSourceMapRenderer.render(wrapped)
        def spanOf(map: UplcSourceMap, text: String): Option[UplcSpan] =
            map.spans.find(sp => map.uplc.substring(sp.s, sp.e) == text)
        assert(
          spanOf(base, "(builtin addInteger)").map(_.n) == spanOf(wrap, "(builtin addInteger)")
              .map(_.n)
        )
        assert(spanOf(base, "(con integer 1)").map(_.n) == spanOf(wrap, "(con integer 1)").map(_.n))
        assert(wrap.uplc == wrapped.show)
    }

    test("hasSourceInfo") {
        assert(UplcSourceMapRenderer.hasSourceInfo(term))
        assert(!UplcSourceMapRenderer.hasSourceInfo(Term.Const(Constant.Integer(1))))
        assert(
          UplcSourceMapRenderer.hasSourceInfo(Term.LamAbs("x", Term.Builtin(AddInteger, annA)))
        )
    }

    test("a term without any source info renders to text and no spans") {
        val plain = Term.Apply(Term.Builtin(AddInteger), Term.Const(Constant.Integer(1)))
        val map = UplcSourceMapRenderer.render(plain)
        assert(map.uplc == plain.show)
        assert(map.spans.isEmpty)
        assert(map.files.isEmpty && map.functions.isEmpty)
    }

    test("markers survive multi-line layout") {
        // Force the printer to break the application across lines.
        val long = (1 to 40).foldLeft[Term](Term.Builtin(AddInteger, annA)) { (acc, i) =>
            Term.Apply(acc, Term.Const(Constant.Integer(i), annB), annA)
        }
        val map = UplcSourceMapRenderer.render(long)
        assert(map.uplc == long.show)
        assert(map.uplc.contains("\n"))
        map.spans.foreach { sp =>
            assert(sp.s >= 0 && sp.e <= map.uplc.length && sp.s < sp.e)
        }
        assert(map.spans.exists(sp => map.uplc.substring(sp.s, sp.e) == "(con integer 40)"))
    }

    test("a string constant that spoofs a marker degrades to no spans, never to corrupt text") {
        // UPLC string constants are printed verbatim, so a constant can contain the renderer's
        // marker characters. Here the constant spoofs the *start* marker of node id 2 and is
        // printed before that node, so the scanner's "already recorded" guard cannot help: it
        // would record a bogus offset, drop the constant's characters and emit spans that
        // partially overlap. The renderer must notice and give up the spans rather than hand the
        // view corrupted UPLC.
        val markerStart = 0x01.toChar
        val markerEnd = 0x02.toChar
        val spoof = Term.Const(Constant.String(s"${markerStart}2$markerEnd"), annB)
        val term = Term.Apply(
          Term.Apply(Term.Builtin(AddInteger, annA), spoof, annB),
          Term.Const(Constant.Integer(7), annB),
          annA
        )
        val map = UplcSourceMapRenderer.render(term)
        assert(map.uplc == term.show)
        assert(map.spans.isEmpty)
    }

    test("spans are emitted sorted: s ascending, then e descending") {
        val sir = compile {
            def double(x: BigInt): BigInt = x + x
            double(21)
        }
        val map = UplcSourceMapRenderer.render(sir.toUplc())
        assert(map.spans.size > 1)
        map.spans.sliding(2).foreach { pair =>
            val (a, b) = (pair.head, pair.last)
            assert(a.s < b.s || (a.s == b.s && a.e >= b.e), s"unsorted: $a before $b")
        }
    }

    test("json round-trip") {
        val map = UplcSourceMapRenderer.render(term)
        val json = new String(UplcSourceMapRenderer.toJson(map), "UTF-8")
        assert(json.contains("\"schemaVersion\": 1") || json.contains("\"schemaVersion\":1"))
        assert(json.contains("\"uplc\""))
        assert(json.contains("\"spans\""))
        assert(json.contains("\"fn\""))
    }

    test("json always carries the string tables, even when empty") {
        val plain = Term.Const(Constant.Integer(1))
        val json = new String(UplcSourceMapRenderer.toJson(UplcSourceMapRenderer.render(plain)))
        assert(json.contains("\"files\""))
        assert(json.contains("\"functions\""))
        assert(json.contains("\"spans\""))
    }

    test("invariant holds for a compiled program") {
        val sir = compile {
            def double(x: BigInt): BigInt = x + x
            double(21)
        }
        val t = sir.toUplc()
        val map = UplcSourceMapRenderer.render(t)
        assert(map.uplc == t.show)
        assert(map.spans.nonEmpty)
        assert(map.functions.exists(_.endsWith("double")), s"functions: ${map.functions}")
        map.spans.foreach { sp =>
            assert(sp.s >= 0 && sp.e <= map.uplc.length && sp.s < sp.e)
            assert(sp.file >= 0 && sp.file < map.files.size)
            sp.fn.foreach(i => assert(i >= 0 && i < map.functions.size))
        }
        // Spans are properly nested: the view resolves a cursor to the innermost span containing
        // it, which is only well defined when no two spans partially overlap.
        var open = List.empty[UplcSpan]
        map.spans.sortBy(sp => (sp.s, -sp.e)).foreach { sp =>
            open = open.dropWhile(_.e <= sp.s)
            open.headOption.foreach(o => assert(sp.e <= o.e, s"partial overlap: $o and $sp"))
            open = sp :: open
        }
    }
}
