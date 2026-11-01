package scalus.testing.budget

import org.scalatest.events.{Event, TestFailed}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{Args, DoNotDiscover, Reporter}
import scalus.cardano.ledger.ExUnits
import scalus.testing.budget.BudgetTable.Entry
import scalus.utils.BuildInfo

import scala.collection.mutable

class BudgetPinsTest extends AnyFunSuite {
    import BudgetPinsTest.*

    // ----- BudgetText -----

    test("group: thousands separated by underscores") {
        assert(BudgetText.group(0) == "0")
        assert(BudgetText.group(79) == "79")
        assert(BudgetText.group(1101) == "1_101")
        assert(BudgetText.group(203937) == "203_937")
        assert(BudgetText.group(2298799) == "2_298_799")
        assert(BudgetText.group(-1000) == "-1_000")
    }

    test("render: a value beyond Int gets the L suffix, so scalafmt can parse the table") {
        // scalac adapts a large literal to the Long parameter, scalafmt's parser does not.
        assert(
          BudgetText.render(Pin.Eval(31_290_337L, 8_992_712_599L, 1_359, 2_474_213L)) ==
              "Pin.Eval(memory = 31_290_337, steps = 8_992_712_599L, size = 1_359, fee = 2_474_213)"
        )
    }

    test("percent: signed, one decimal, relative to the expected value") {
        assert(BudgetText.percent(1101, 1085) == "-1.5 %")
        assert(BudgetText.percent(1000, 1001) == "+0.1 %")
        assert(BudgetText.percent(1000, 1000) == "+0.0 %")
        assert(BudgetText.percent(0, 0) == "+0.0 %")
        assert(BudgetText.percent(0, 5) == "new")
    }

    test("Pin.eval: fee = execution fee (rounded up) + reference-script fee of the size") {
        // execution: 9_934 * 0.0577 + 2_298_799 * 0.0000721 = 738.9 -> 739
        assert(Pin.executionFee(9934, 2298799) == 739)
        // reference script: 15 lovelace per byte below the first 25_600-byte tier
        assert(Pin.referenceScriptFee(100) == 1500)
        assert(Pin.eval(ExUnits(9934, 2298799), 100) == Pin.Eval(9934, 2298799, 100, 2239))
    }

    test("mismatch message: key, generation, one line per quantity") {
        val message = BudgetText.mismatch(
          "MathTest",
          "abs / zero",
          Generation.Scala33,
          Pin.Eval(1101, 203937, 100, 1579),
          Pin.Eval(1085, 201002, 95, 1503)
        )
        assert(
          message ==
              """MathTest: budget pin mismatch, key "abs / zero", generation Scala33
                |  memory 1_101 -> 1_085 (-1.5 %)
                |  steps 203_937 -> 201_002 (-1.4 %)
                |  size 100 -> 95 (-5.0 %)
                |  fee 1_579 -> 1_503 (-4.8 %)
                |  fee = execution 79 -> 78 + reference script 1_500 -> 1_425
                |Run `sbtn updateBudgets` to accept the new values.""".stripMargin
        )
    }

    test("merge: equal value keeps the row, new keys follow in first-use order") {
        val entries = Seq("a" -> Entry.Plain(eval1))
        val merged = BudgetText.merge(
          entries,
          Generation.Scala33,
          Seq("b" -> eval2, "a" -> eval1),
          dropUnused = true
        )
        assert(merged == Seq("a" -> Entry.Plain(eval1), "b" -> Entry.Plain(eval2)))
    }

    test("merge: a changed plain row splits per generation, the old value goes to the others") {
        val merged = BudgetText.merge(
          Seq("a" -> Entry.Plain(eval1)),
          Generation.Scala38,
          Seq("a" -> eval2),
          dropUnused = true
        )
        assert(
          merged == Seq(
            "a" -> Entry.PerGeneration(
              Map(Generation.Scala33 -> eval1, Generation.Scala38 -> eval2)
            )
          )
        )
    }

    test("merge: generation rows that become equal join into one plain row") {
        val merged = BudgetText.merge(
          Seq(
            "a" -> Entry.PerGeneration(
              Map(Generation.Scala33 -> eval1, Generation.Scala38 -> eval2)
            )
          ),
          Generation.Scala38,
          Seq("a" -> eval1),
          dropUnused = true
        )
        assert(merged == Seq("a" -> Entry.Plain(eval1)))
    }

    test("merge: unused keys are dropped only after a complete run") {
        val entries = Seq("a" -> Entry.Plain(eval1), "gone" -> Entry.Plain(eval2))
        val actuals = Seq("a" -> eval1)
        assert(
          BudgetText.merge(entries, Generation.Scala33, actuals, dropUnused = false) == entries
        )
        assert(
          BudgetText.merge(entries, Generation.Scala33, actuals, dropUnused = true) ==
              Seq("a" -> Entry.Plain(eval1))
        )
    }

    test("source: a generated table with plain and per-generation rows") {
        val source = BudgetText.source(
          "scalus.example",
          "FooTestBudgets",
          "FooTest",
          Seq(
            "a \"quoted\"" -> Entry.Plain(eval1),
            "b" -> Entry.PerGeneration(
              Map(Generation.Scala38 -> eval2, Generation.Scala33 -> eval1)
            )
          )
        )
        assert(
          source ==
              """package scalus.example
                |
                |import scalus.testing.budget.{BudgetTable, Generation, Pin}
                |
                |/** Budget pins of [[FooTest]]. Generated by `sbtn updateBudgets`. Do not edit by hand. */
                |object FooTestBudgets extends BudgetTable {
                |    override val generation: Generation = Generation.current
                |    pin("a \"quoted\"", Pin.Eval(memory = 1_101, steps = 203_937, size = 100, fee = 1_579))
                |    pin("b", Generation.Scala33, Pin.Eval(memory = 1_101, steps = 203_937, size = 100, fee = 1_579))
                |    pin("b", Generation.Scala38, Pin.Eval(memory = 1_085, steps = 201_002, size = 100, fee = 1_578))
                |}
                |""".stripMargin
        )
    }

    // ----- Generation -----

    test("Generation.of: 3.3.x is Scala33, 3.8 and later is Scala38") {
        assert(Generation.of("3.3.8") == Generation.Scala33)
        assert(Generation.of("3.3.9") == Generation.Scala33)
        assert(Generation.of("3.8.4") == Generation.Scala38)
        assert(Generation.of("3.9.0") == Generation.Scala38)
        assert(Generation.of("3.10.0-RC1") == Generation.Scala38)
        assert(Generation.of("4.0.0") == Generation.Scala38)
    }

    test("Generation.current: the macro reads a real compiler version") {
        val version = Generation.currentCompilerVersion
        // BuildInfo holds the Scala version of this build. A mismatch means the macro fell back to
        // its default instead of reading the compiler.
        assert(version == BuildInfo.scalaVersion)
        assert(Generation.current == Generation.of(version))
    }

    // ----- BudgetPinsSuite: checks of a real run -----

    test("check mode: an equal pin passes") {
        val failures = failuresOf(Fixture(table(Pin.eval(units1, size1))))
        assert(failures.isEmpty, failures)
    }

    test("check mode: a changed budget fails with the mismatch message") {
        val failures = failuresOf(Fixture(table(Pin.eval(units2, size1))))
        assert(failures.size == 1)
        assert(failures.head.contains("budget pin mismatch, key \"pins once\""), failures.head)
    }

    test("check mode: a key with no pin fails") {
        val failures = failuresOf(Fixture(emptyTable))
        assert(failures.size == 1)
        assert(failures.head.contains("no budget pin for key \"pins once\""), failures.head)
    }

    test("check mode: a second unlabelled pin in one test fails") {
        val failures = failuresOf(Fixture(table(Pin.eval(units1, size1)), pinTwice = true))
        assert(failures.size == 1)
        assert(failures.head.contains("pinned twice"), failures.head)
    }

    test("check mode: a table key that no test used fails the suite") {
        val withExtra = new BudgetTable {
            val generation: Generation = Generation.Scala33
            pin("pins once", Pin.eval(units1, size1))
            pin("deleted test", Pin.eval(units2, size1))
        }
        val error = intercept[IllegalStateException](failuresOf(Fixture(withExtra)))
        assert(
          error.getMessage.contains("1 unused budget pins: \"deleted test\""),
          error.getMessage
        )
    }

    test("pinLabel: the scope's label replaces the automatic one") {
        val labelled = new BudgetTable {
            val generation: Generation = Generation.Scala33
            pin("pins once / first", Pin.eval(units1, size1))
            pin("pins once / second", Pin.eval(units1, size1))
        }
        val failures =
            failuresOf(Fixture(labelled, pinTwice = true, labels = Seq("first", "second")))
        assert(failures.isEmpty, failures)
    }

    test("unpinned suite: pins are no-ops") {
        val failures = failuresOf(Fixture(BudgetTable.unpinned, pinTwice = true))
        assert(failures.isEmpty, failures)
    }
}

object BudgetPinsTest {
    // fee = execution (79, 78) + reference script (100 bytes x 15 = 1_500)
    val eval1 = Pin.Eval(1101, 203937, 100, 1579)
    val eval2 = Pin.Eval(1085, 201002, 100, 1578)
    val units1 = ExUnits(1101, 203937)
    val units2 = ExUnits(1085, 201002)
    val size1 = 100

    private def table(value: Pin): BudgetTable = new BudgetTable {
        val generation: Generation = Generation.Scala33
        pin("pins once", value)
    }

    private val emptyTable: BudgetTable = new BudgetTable {
        val generation: Generation = Generation.Scala33
    }

    /** A suite under test: one test that pins `units1`, once or twice. */
    @DoNotDiscover
    final class Fixture(table: BudgetTable, pinTwice: Boolean = false, labels: Seq[String] = Nil)
        extends AnyFunSuite,
          BudgetPins,
          BudgetPinsSuite {
        override protected def budgetTable: BudgetTable = table
        test("pins once") {
            if labels.isEmpty then
                pinEval(units1, size1)
                if pinTwice then pinEval(units1, size1)
            else labels.foreach(label => pinLabel(label)(pinEval(units1, size1)))
        }
    }

    /** Runs `suite` and returns the messages of its failed tests. */
    def failuresOf(suite: AnyFunSuite): Seq[String] = {
        val failures = mutable.Buffer.empty[String]
        val reporter = new Reporter {
            def apply(event: Event): Unit = event match
                case failed: TestFailed => failures += failed.message
                case _                  => ()
        }
        val _ = suite.run(None, Args(reporter))
        failures.toSeq
    }
}
