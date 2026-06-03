package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{ExUnitPrices, ExUnits, NonNegativeInterval}

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

    // A simple acyclic call chain A→B→C with known self-costs, for inclusive-cost / Hot Path checks.
    private val chain = ProfilingData(
      bySourceLocation = Seq(
        SourceLocationProfile("Chain.scala", 1, memory = 1, cpu = 10, count = 1),
        SourceLocationProfile("Chain.scala", 2, memory = 2, cpu = 20, count = 1),
        SourceLocationProfile("Chain.scala", 3, memory = 4, cpu = 100, count = 1)
      ),
      byFunction = Nil,
      byLocationFunction = Nil,
      transitions = Seq(
        SourceTransition("Chain.scala", 1, "Chain.scala", 2, count = 1),
        SourceTransition("Chain.scala", 2, "Chain.scala", 3, count = 1)
      ),
      totalBudget = ExUnits(memory = 7, steps = 130)
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

    test("a derived fee column/field appears across formats when prices are attached") {
        // mainnet-like prices: 0.0577 / mem unit, 0.0000721 / cpu step.
        // fee = ceil(0.0577*mem + 0.0000721*cpu): Foo:3 (100,200) -> 6, total (150,260) -> 9.
        val priced = data.withPrices(
          ExUnitPrices(NonNegativeInterval(577, 10000), NonNegativeInterval(721, 10000000))
        )

        // JSON: per-entry and total fee in lovelace (the shape LLMs consume); absent without prices
        val json = ProfileFormatter.toJson(priced)
        assert(json.contains("\"cpu\":200,\"fee\":6"))
        assert(json.contains("\"fee\":9")) // total
        assert(!ProfileFormatter.toJson(data).contains("\"fee\""))

        // CSV: trailing fee column only when priced
        val csv = ProfileFormatter.toCsv(priced)
        assert(csv.startsWith("section,key,detail,count,mem,cpu,fee\n"))
        assert(csv.contains("total,,,0,150,260,9"))
        assert(ProfileFormatter.toCsv(data).startsWith("section,key,detail,count,mem,cpu\n"))

        // HTML: a Fee column and a total fee; neither present without prices
        val html = ProfileFormatter.toHtml(priced)
        assert(html.contains("<th>Fee (lov)</th>"))
        assert(html.contains("fee=9 lovelace"))
        assert(!ProfileFormatter.toHtml(data).contains("Fee (lov)"))
    }

    test("toHtml is self-contained and includes all views") {
        val html = ProfileFormatter.toHtml(data)
        assert(html.startsWith("<!DOCTYPE html>"))
        assert(html.contains("By Source Location"))
        assert(html.contains("By Builtin"))
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

    test("toHtml shows the profiled contract/test-case title in the head and on top") {
        // default: generic head title, no subtitle
        val plain = ProfileFormatter.toHtml(data)
        assert(plain.contains("<title>Scalus Profile</title>"))
        assert(!plain.contains("class=\"subtitle\""))

        // with a label: it appears in the <head> title and as a subtitle on top, HTML-escaped
        val titled = ProfileFormatter.toHtml(data, sources, title = "Foo<Validator> & spend")
        assert(titled.contains("<title>Scalus Profile — Foo&lt;Validator&gt; &amp; spend</title>"))
        assert(titled.contains("<p class=\"subtitle\">Foo&lt;Validator&gt; &amp; spend</p>"))
    }

    test("By Source Location rows expand into a recursive incoming-call tree") {
        val html = ProfileFormatter.toHtml(data)
        // Locations are tree roots; the incoming graph is embedded as JS data for lazy expansion.
        assert(html.contains("class=\"treenode\""))
        assert(html.contains("var SCALUS_IN="))
        assert(html.contains("var SCALUS_LABEL="))
        assert(html.contains("scalusToggle"))
        // both endpoints are labelled, and the Foo:3 -> Foo:4 edge (count 4, est. cpu 60) is in
        // the adjacency as [sourceId, count, edgeCost]
        assert(html.contains("\"Foo:4\""))
        assert(html.contains("\"Foo:3\""))
        assert(html.contains(",4,60]"))
        // the loop-cutoff path is threaded through data-path
        assert(html.contains("data-path="))
    }

    test("transition edges carry an estimated cost that conserves node self-cost") {
        val html = ProfileFormatter.toHtml(data)
        // edgeCost(A→B) = selfCost(B) · count(A→B)/inDeg(B). Here every share is 1, so each edge
        // carries its target's full self-cost — Hot Edges shows From|To|Count|Mem|CPU:
        //   Foo:4→Foo:3 carries Foo:3 (mem 100, cpu 200), count 1
        //   Foo:3→Foo:4 carries Foo:4 (mem 50, cpu 60), count 4
        assert(html.contains("<td>Foo:4</td><td>Foo:3</td><td>1</td><td>100</td><td>200</td>"))
        assert(html.contains("<td>Foo:3</td><td>Foo:4</td><td>4</td><td>50</td><td>60</td>"))
        // By Source Location gains a Downstream column (cost a line pushes into its callees)
        assert(html.contains("<th>Downstream (cpu)</th>"))
    }

    test("inclusive cost accumulates over the call DAG (Inclusive column)") {
        val html = ProfileFormatter.toHtml(chain)
        // inclusive(C)=100 (self); inclusive(B)=20+100=120; inclusive(A)=10+120=130 (=total budget).
        // The Inclusive column carries these even though the self CPU column is 100, 20, 10.
        assert(html.contains("<th>Inclusive (cpu)</th>"))
        assert(html.contains("<td>130</td>")) // A: inclusive = whole chain
        assert(html.contains("<td>120</td>")) // B: self 20 + C's 100
    }

    test("inclusive cost does not diverge on cycles (SCC collapse)") {
        // `data` has the Foo:3 ⇄ Foo:4 2-cycle: it collapses to one SCC whose inclusive cpu is the
        // total budget (260) — every member shares it, no blow-up.
        val html = ProfileFormatter.toHtml(data)
        assert(html.contains("<th>Inclusive (cpu)</th>"))
        assert(html.contains("<td>260</td>"))
    }

    test("toHtml with sources renders the annotated-source view") {
        val html = ProfileFormatter.toHtml(data, sources)
        assert(html.contains("Annotated Source"))
        assert(html.contains("def add(a: BigInt, b: BigInt)"))
        // a profiled line carries its cost in the gutter
        assert(html.contains("200 cpu / 100 mem"))
    }

    test("By Source Location links to the Annotated Source line (cross-tab nav)") {
        val html = ProfileFormatter.toHtml(data, sources)
        assert(html.contains("class=\"goto\""))
        assert(html.contains("function scalusGoto"))
        // every goto button must target an anchor id that actually exists on a source line
        val anchors = "scalusGoto\\('([^']+)'\\)".r.findAllMatchIn(html).map(_.group(1)).toList
        assert(anchors.nonEmpty, "expected at least one goto button")
        anchors.foreach(a =>
            assert(html.contains(s"""id="$a""""), s"no source line anchored as $a")
        )
    }

    test("no goto button when the source is not annotated") {
        // without sources there is no Annotated Source tab to navigate to
        val html = ProfileFormatter.toHtml(data)
        assert(!html.contains("class=\"goto\""))
    }

    test("html escaping is applied to rendered content") {
        val tricky = data.copy(
          byFunction = Seq(FunctionProfile("a<b>&c", memory = 1, cpu = 1, count = 1))
        )
        val html = ProfileFormatter.toHtml(tricky)
        assert(html.contains("a&lt;b&gt;&amp;c"))
    }
}
