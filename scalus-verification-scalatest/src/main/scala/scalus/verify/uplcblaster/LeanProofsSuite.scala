package scalus.verify.uplcblaster

import org.scalatest.{BeforeAndAfterAll, OneInstancePerTest, Outcome, Tag, TestSuite, TestSuiteMixin}

import java.nio.file.Path

/** Tag of a test about a statement that Lean does not finish: the working set of what the tactic
  * cannot do yet. Such a test waits for its time limit, and the solver can take gigabytes
  * meanwhile, so the build leaves the tag out of `test` and `testQuick`. `testOnly` runs them:
  * {{{
  * sbt "scalusVerification/testOnly *UplcBlasterLimitsTest"
  * sbt "scalusExamplesJVM/testOnly *VestingVerificationTest -- -n scalus.verify.uplcblaster.Unfinished"
  * }}}
  * The second runs only the tagged tests of a suite; `-l` with the tag leaves them out. Unlike a
  * test marked `ignore`, which no option runs.
  */
object Unfinished extends Tag("scalus.verify.uplcblaster.Unfinished")

/** [[LeanProofs]] for a ScalaTest suite. It has nothing of its own but what ScalaTest is asked for:
  * a statement that cannot be checked here cancels its test, a kept result says the tests that
  * state it, what a test that passed no longer states is not kept, nor what only tests the suite no
  * longer has state, and the Lean server is ended after the suite's last test.
  *
  * The suite's tests run one after another, in one instance of the suite: a suite that is also a
  * `OneInstancePerTest`, as `ParallelTestExecution` is, cannot be constructed. Each of its tests
  * would run in an instance of its own, with proofs and a Lean server of its own, and ScalaTest
  * runs `afterAll` only in the first instance: the servers would not be ended, and what the tests
  * no longer state would stay kept. The checks run one after another on one server anyway.
  * {{{
  * class HtlcProofsTest extends AnyFunSuite with LeanProofsSuite {
  *     import proofs.*
  *
  *     private val plusZero =
  *         verifier.statement("plus_zero", forAll[BigInt](x => x + BigInt(0) == x))
  *
  *     test("x + 0 is x") { check(plusZero, 40) }
  * }
  * }}}
  *
  * Statements declared in the suite's body, as here, make `proofs` while the suite is constructed:
  * override its hooks (`leanWorkspace`, `keptResultsFile`, `keptChecksDirectory`) as `def`, since a
  * `val` of the suite is not set yet then, and an error in making `proofs`, as a workspace that is
  * not there, fails the whole suite rather than one test.
  */
trait LeanProofsSuite extends TestSuiteMixin with BeforeAndAfterAll { this: TestSuite =>

    require(
      !this.isInstanceOf[OneInstancePerTest],
      s"${getClass.getName} runs each test in an instance of its own (OneInstancePerTest or " +
          "ParallelTestExecution), but a LeanProofsSuite runs its tests in one instance, " +
          "one after another"
    )

    /** The Lean workspace the suite's checks run in: the project's
      * ([[LeanProofs.projectWorkspace]]), which the build has, so that a suite does not name it. A
      * suite whose proofs are in another workspace overrides this.
      */
    protected def leanWorkspace: Path = projects

    /** The project's workspace, as the class path of the suite says it. */
    private lazy val projects: Path = LeanProofs.projectWorkspace(getClass.getClassLoader)

    /** [[LeanProofs.ready]]: a workspace of the project's own in `directory`, made ready. */
    protected def readyWorkspace(directory: Path): Path = LeanProofs.ready(directory)

    /** The file in which the suite keeps the results of its statements, or none for a suite that
      * does not keep them. It is beside the suite's source, `<Suite>.proofs.json`, and is
      * committed.
      */
    protected def keptResultsFile: Option[Path] = None

    /** The directory into which the suite's checks are also written, each in a directory of its
      * own, to read the Lean they are given; none to follow the environment variable
      * `SCALUS_LEAN_KEEP_CHECKS`. A statement whose kept result is taken runs no check, and writes
      * none.
      */
    protected def keptChecksDirectory: Option[Path] = None

    /** Whether the suite holds the specifications of the functions of its verifier to being checked
      * ([[LeanProofs.checkSpecifications]]): after its last test, where every test but those tagged
      * [[Unfinished]] ran and passed, a clause of a function's code that no proof checks, and that
      * is not declared open, fails the suite. A suite about the verifier itself, whose functions
      * state clauses to be read and not to be proved, says no.
      */
    protected def checksSpecifications: Boolean = true

    /** The test that runs: what it checks is said to be its. The tests run one after another, so
      * one variable says it, also to a check the test makes on another thread, which a thread-local
      * one would not.
      */
    @volatile private var running = ""

    /** The tests that passed. */
    private val passed = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()

    /** The proofs, once a test has asked for them. A suite whose tests ask for none has no
      * workspace made ready, and nothing to close.
      */
    private var made: Option[LeanProofs] = None

    /** The suite's proofs. Its members are the suite's with `import proofs.*`, for which it is
      * final.
      */
    protected final lazy val proofs: LeanProofs = {
        val proofs =
            LeanProofs(
              leanWorkspace,
              keptResultsFile,
              reason => cancel(reason),
              () => running,
              keptChecksDirectory
            )
        made = Some(proofs)
        proofs
    }

    abstract override protected def withFixture(test: NoArgTest): Outcome = {
        running = test.name
        // What is checked after the test, as in a hook or in afterAll, is of no test.
        val outcome =
            try super.withFixture(test)
            finally running = ""
        // A test that passed states all it states: what it no longer does is not kept.
        if outcome.isSucceeded then
            passed.add(test.name)
            made.foreach(_.finished(test.name))
        outcome
    }

    // What is kept for a test the suite no longer has is not kept. The tests are said here, and not
    // where the proofs are made: a suite that states its statements as it is constructed makes
    // them before its tests are registered.
    override protected def afterAll(): Unit = {
        try
            super.afterAll()
            made.filter(_ => checksSpecifications && everyTestPassed).foreach { proofs =>
                val open = proofs.checkSpecifications()
                if open.nonEmpty then
                    println(
                      (s"${getClass.getSimpleName}: clauses that are open:" ::
                          open.map(specification => s"  ${specification.describe}")).mkString("\n")
                    )
            }
        finally
            made.foreach { proofs =>
                proofs.tests(testNames)
                proofs.close()
            }
    }

    /** Whether every test of the suite but those tagged [[Unfinished]] ran and passed: a run of
      * some of them, or one with a test that failed or was canceled, does not hold the
      * specifications to what it checked.
      */
    private def everyTestPassed: Boolean =
        testNames.forall(name =>
            tags.get(name).exists(_.contains(Unfinished.name)) || passed.contains(name)
        )
}
