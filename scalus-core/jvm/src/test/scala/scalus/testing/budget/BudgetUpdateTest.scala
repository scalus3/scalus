package scalus.testing.budget

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.events.TestFailed
import org.scalatest.{Args, ConfigMap}

import java.nio.file.Files
import scala.collection.mutable

/** The update run on the JVM: it rewrites the table file by key (`[BP-8]`, `[BP-8a]`). */
class BudgetUpdateTest extends AnyFunSuite {
    import BudgetPinsTest.*

    test("update mode: no failure, the table file gets the actual values by key") {
        val file = Files.createTempFile("FixtureBudgets", ".scala")
        // The table's source path is the temp file, so the writer rewrites that file only.
        val stale = new BudgetTable(using BudgetTable.SourcePath(file.toString)) {
            val generation: Generation = Generation.Scala33
            pin("pins once", Pin.eval(units2, size1))
            pin("deleted test", Pin.eval(units2, size1))
        }
        val failures = mutable.Buffer.empty[String]
        val suite = Fixture(stale)
        val args = Args(
          reporter = event =>
              event match
                  case failed: TestFailed => failures += failed.message
                  case _                  => (),
          configMap = ConfigMap(BudgetPinsSuite.UpdateKey -> "true")
        )
        val _ = suite.run(None, args)
        assert(failures.isEmpty, failures)
        val written = Files.readString(file)
        // The changed row splits: this run's generation gets the new value, the other generation
        // keeps the old one until its own update run (sbtn updateBudgets runs both).
        val g33 =
            """pin("pins once", Generation.Scala33, Pin.Eval(memory = 1_101, steps = 203_937, size = 100, fee = 1_579))"""
        val g38 =
            """pin("pins once", Generation.Scala38, Pin.Eval(memory = 1_085, steps = 201_002, size = 100, fee = 1_578))"""
        assert(written.contains(g33) && written.contains(g38), written)
        assert(!written.contains("deleted test"), written)
        Files.delete(file)
    }
}
