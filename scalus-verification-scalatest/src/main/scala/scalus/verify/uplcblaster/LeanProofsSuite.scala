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
  *     private lazy val workspace = readyWorkspace(Path.of("src/test/lean/Htlc"))
  *     override protected def leanWorkspace: Path = workspace
  *
  *     test("x + 0 is x") { proven(forAll[BigInt](x => x + BigInt(0) == x), 40) }
  * }
  * }}}
  */
trait LeanProofsSuite extends TestSuiteMixin with BeforeAndAfterAll { this: TestSuite =>

    /** The Lean workspace the suite's checks run in. A suite of a project of its own overrides this
      * with a workspace of the project, made ready by [[readyWorkspace]]. Unless it is overridden,
      * it is the workspace of Scalus's Lean library, which only Scalus's own sources have.
      */
    protected def leanWorkspace: Path = LeanProofs.libraryWorkspace

    /** [[LeanProofs.ready]]: a workspace of the project's own in `directory`, made ready. */
    protected def readyWorkspace(directory: Path): Path = LeanProofs.ready(directory)

    /** The file in which the suite keeps the results of its statements, or none for a suite that
      * does not keep them. It is beside the suite's source, `<Suite>.proofs.json`, and is
      * committed.
      */
    protected def keptResultsFile: Option[Path] = None

    /** The suite's proofs. Its members are the suite's with `import proofs.*`, for which it is
      * final.
      */
    protected final lazy val proofs: LeanProofs =
        LeanProofs(leanWorkspace, keptResultsFile, reason => cancel(reason))

    abstract override protected def withFixture(test: NoArgTest): Outcome =
        proofs.during(test.name)(super.withFixture(test))

    override protected def afterAll(): Unit =
        try super.afterAll()
        finally proofs.close()
}
