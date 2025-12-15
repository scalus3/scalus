package scalus.testing

import org.scalatest.Tag

/** Tag for integration tests that require external services (blockchain nodes, APIs).
  *
  * Tests tagged with this tag are excluded from default test runs. To run integration tests, use:
  * {{{
  * sbt "project/testOnly -- -n scalus.testing.IntegrationTest"
  * }}}
  */
object IntegrationTest extends Tag("scalus.testing.IntegrationTest")
