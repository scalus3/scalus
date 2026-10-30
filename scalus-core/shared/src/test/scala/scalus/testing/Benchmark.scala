package scalus.testing

import org.scalatest.Tag

/** Tag for benchmark tests that are slow and should not run by default.
  *
  * Tests tagged with this tag are excluded from default test runs. To run benchmarks, use:
  * {{{
  * sbtn "project/testOnly -- -n scalus.testing.Benchmark"
  * }}}
  */
object Benchmark extends Tag("scalus.testing.Benchmark")
