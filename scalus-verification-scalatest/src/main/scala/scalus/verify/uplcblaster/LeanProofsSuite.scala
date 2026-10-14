package scalus.verify.uplcblaster

import org.scalatest.{BeforeAndAfterAll, Outcome, Tag, TestSuite, TestSuiteMixin}

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
  * a statement that cannot be checked here cancels its test, a result is kept under the name of the
  * test that states it, and the Lean server is ended after the suite's last test.
  * {{{
  * class HtlcProofsTest extends AnyFunSuite with LeanProofsSuite {
  *     import proofs.*
  *
  *     test("x + 0 is x") { proven(forAll[BigInt](x => x + BigInt(0) == x), 40) }
  * }
  * }}}
  */
trait LeanProofsSuite extends TestSuiteMixin with BeforeAndAfterAll { this: TestSuite =>

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

    /** The test that runs: a result is kept under its name. */
    private var running = ""

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
        super.withFixture(test)
    }

    override protected def afterAll(): Unit =
        try super.afterAll()
        finally made.foreach(_.close())
}
