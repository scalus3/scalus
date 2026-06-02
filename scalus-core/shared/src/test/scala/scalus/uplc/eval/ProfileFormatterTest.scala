package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.ExUnits

class ProfileFormatterTest extends AnyFunSuite {

    private val data = ProfilingData(
      bySourceLocation = Seq(
        SourceLocationProfile("Foo.scala", 3, memory = 100, cpu = 200, count = 5),
        SourceLocationProfile("Foo.scala", 4, memory = 50, cpu = 60, count = 2)
      ),
      byFunction = Seq(FunctionProfile("AddInteger", memory = 30, cpu = 40, count = 3)),
      byLocationFunction = Seq(
        LocationFunctionProfile("Foo.scala", 3, "AddInteger", memory = 30, cpu = 40, count = 3)
      ),
      transitions = Seq(
        SourceTransition("Foo.scala", 3, "Foo.scala", 4, count = 4),
        SourceTransition("Foo.scala", 4, "Foo.scala", 3, count = 1)
      ),
      totalBudget = ExUnits(memory = 150, steps = 260)
    )

    private val sources = Map(
      "Foo.scala" -> IndexedSeq(
        "object Foo {",
        "  def add(a: BigInt, b: BigInt) =",
        "    a + b",
        "  val x = add(1, 2)"
      )
    )

    test("toText still renders the legacy sections") {
        val text = ProfileFormatter.toText(data)
        assert(text.contains("Profile by Source Location"))
        assert(text.contains("Profile by Builtin"))
        assert(text.contains("Total:"))
    }

    test("summary is compact and lists top entries") {
        val s = ProfileFormatter.summary(data)
        assert(s.contains("Total: mem=150 cpu=260"))
        assert(s.contains("AddInteger"))
        assert(s.contains("Foo:3") || s.contains("Foo.scala:3") || s.contains("Foo:"))
    }

    test("toCsv has a header and rows for every section") {
        val csv = ProfileFormatter.toCsv(data)
        assert(csv.startsWith("section,key,detail,count,mem,cpu\n"))
        assert(csv.contains("location,Foo.scala:3,,5,100,200"))
        assert(csv.contains("builtin,AddInteger"))
        assert(csv.contains("edge,Foo.scala:3,Foo.scala:4,4"))
        assert(csv.contains("total,,,0,150,260"))
    }

    test("toJson includes all sections") {
        val json = ProfileFormatter.toJson(data)
        assert(json.contains("\"totalBudget\""))
        assert(json.contains("\"bySourceLocation\""))
        assert(json.contains("\"byFunction\""))
        assert(json.contains("\"transitions\""))
        assert(json.contains("\"fromLine\":3"))
    }

    test("toHtml is self-contained and includes all views") {
        val html = ProfileFormatter.toHtml(data)
        assert(html.startsWith("<!DOCTYPE html>"))
        assert(html.contains("By Source Location"))
        assert(html.contains("By Builtin"))
        assert(html.contains("Transition Matrix"))
        assert(html.contains("Hot Edges"))
        // bundled, no external dependency
        assert(html.contains("<script>"))
        assert(html.contains("scalusFilter"))
        assert(html.contains("</html>"))
        // sections are navigable tabs, not one long scroll
        assert(html.contains("nav class=\"tabs\""))
        assert(html.contains("class=\"tab-btn"))
        assert(html.contains("class=\"tab-panel\""))
        assert(html.contains("scalusTab"))
    }

    test("toHtml with sources renders the annotated-source view") {
        val html = ProfileFormatter.toHtml(data, sources)
        assert(html.contains("Annotated Source"))
        assert(html.contains("def add(a: BigInt, b: BigInt)"))
        // a profiled line carries its cost in the gutter
        assert(html.contains("200 cpu / 100 mem"))
    }

    test("html escaping is applied to rendered content") {
        val tricky = data.copy(
          byFunction = Seq(FunctionProfile("a<b>&c", memory = 1, cpu = 1, count = 1))
        )
        val html = ProfileFormatter.toHtml(tricky)
        assert(html.contains("a&lt;b&gt;&amp;c"))
    }
}
