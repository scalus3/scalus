package scalus.verify.uplcblaster

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import org.scalatest.{Assertions, BeforeAndAfterAll, Suite, Tag}
import scalus.uplc.Constant
import scalus.verify.*
import scalus.verify.lean.{LeanServer, LeanServerProvider, LeanServers}

import java.io.File
import java.nio.file.{Files, Path}
import scala.concurrent.duration.FiniteDuration

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

/** Runs [[UplcBlaster]] on statements through Lean, for test suites. A suite has one Lean server at
  * a time, started for its first check and closed after its last test.
  */
trait LeanProofs extends Assertions with BeforeAndAfterAll { this: Suite =>

    /** The Lean workspace the suite's checks run in: that of Scalus's Lean library, unless the
      * suite overrides it. A workspace of its own requires that library, as a check imports it.
      */
    protected def leanWorkspace: Path = LeanProofs.libraryWorkspace

    /** The Lean servers of the suite's workspace: none is started before a check asks for one. */
    private lazy val servers = LeanServers.in(leanWorkspace)

    /** Gives a tactic the suite's Lean server: the one that runs, or another where that one has
      * ended. A tactic asks when it runs a check. The test is then canceled, or fails, where Lean
      * cannot run here ([[requireLean]]). One that runs no check needs no Lean.
      */
    protected def lean: LeanServerProvider = () => {
        requireLean()
        servers.server()
    }

    /** The suite's Lean server itself, for a test that speaks to it. */
    protected def leanServer: LeanServer = lean.server() match
        case Right(server) => server
        case Left(reason)  => fail(reason)

    /** Starts a Lean server in `workspace`, for whoever closes it. The test is canceled, or fails,
      * where Lean cannot run here ([[requireLean]]), and fails where the server does not start.
      */
    protected def startLean(workspace: Path): LeanServer = {
        requireLean()
        LeanServer.start(workspace) match
            case Right(started) => started
            case Left(reason)   => fail(reason)
    }

    override protected def afterAll(): Unit =
        try super.afterAll()
        finally servers.close()

    /** Whether Lean can run here: `lake` on the `PATH`, and Scalus's Lean library built where the
      * suite's workspace has it. The ci-jvm shell has neither, so the tests that run Lean are
      * canceled there. Build the library with `lake build` (see the module README) to run them.
      */
    protected lazy val leanAvailable: Boolean =
        sys.env
            .getOrElse("PATH", "")
            .split(File.pathSeparator)
            .exists(directory => Files.isExecutable(Path.of(directory, "lake"))) &&
            LeanProofs.isBuilt(leanWorkspace)

    /** Cancels the test when Lean cannot run here, unless the `SCALUS_REQUIRE_LEAN` environment
      * variable is set, as in the Lean-Proofs workflow: there a missing Lean fails the test, so the
      * proofs cannot pass by not running.
      */
    protected def requireLean(): Unit = {
        val missing = s"requires lake and a built Lean workspace in $leanWorkspace"
        if sys.env.contains("SCALUS_REQUIRE_LEAN") then assert(leanAvailable, missing)
        else assume(leanAvailable, missing)
    }

    /** Declares `prop` in a fresh verifier and runs [[UplcBlaster]] on it through Lean. */
    protected def run(
        prop: Prop,
        budget: Int,
        functions: Seq[FunctionDef[?, ?]]
    ): (Verifier, Statement, VerificationResult) = {
        requireLean()
        val verifier = Verifier.empty
        functions.foreach(verifier.addFunction)
        val statement = verifier.statement(prop)
        (verifier, statement, verifier.verify(statement, UplcBlaster(budget, lean)))
    }

    /** Proves `prop`, and returns how the proof was checked. */
    protected def proven(prop: Prop, budget: Int, functions: FunctionDef[?, ?]*): ProofKind =
        run(prop, budget, functions) match
            case (verifier, statement, VerificationResult.Proven(proof)) =>
                val artifact = proof.artifact.asInstanceOf[UplcBlaster.Artifact]
                artifact.kind match
                    case ProofKind.Blaster =>
                        if !artifact.output.contains("✅ Valid") then
                            fail(s"expected a valid proof, got ${artifact.output}")
                    case ProofKind.LeanNative => assert(!artifact.output.contains("error"))
                    case other                => fail(s"unexpected proof kind $other")
                assert(verifier.theorems.exists(_.statement eq statement))
                artifact.kind
            case (_, _, other) => fail(s"expected a proof, got $other")

    /** The replayed counterexample of a refuted statement. */
    protected def refuted(
        prop: Prop,
        budget: Int,
        functions: FunctionDef[?, ?]*
    ): Map[String, Constant] =
        run(prop, budget, functions) match
            case (verifier, _, VerificationResult.Refuted(proof)) =>
                assert(verifier.theorems.isEmpty)
                proof.artifact.asInstanceOf[UplcBlaster.Artifact].counterexample.toMap
            case (_, _, other) => fail(s"expected a refutation, got $other")

    /** Why [[UplcBlaster]] is inconclusive about `prop`, with its check given up after `timeout`.
      */
    protected def inconclusive(
        prop: Prop,
        budget: Int,
        timeout: FiniteDuration,
        functions: FunctionDef[?, ?]*
    ): String = inconclusive(prop, UplcBlaster(budget, lean, timeout), functions*)

    /** Why `tactic` is inconclusive about `prop`. */
    protected def inconclusive(
        prop: Prop,
        tactic: UplcBlaster,
        functions: FunctionDef[?, ?]*
    ): String = {
        val verifier = Verifier.empty
        functions.foreach(verifier.addFunction)
        val statement = verifier.statement(prop)
        verifier.verify(statement, tactic) match
            case VerificationResult.Inconclusive(reason) =>
                assert(verifier.theorems.isEmpty)
                reason
            case other => fail(s"expected an inconclusive result, got $other")
    }

    protected def integer(value: Constant): BigInt = value match
        case Constant.Integer(integer) => integer
        case other                     => fail(s"expected an integer, got $other")
}

object LeanProofs {

    /** The workspace of Scalus's Lean library: the directory the environment variable
      * `SCALUS_LEAN_WORKSPACE` names, or the one in Scalus's sources.
      */
    lazy val libraryWorkspace: Path =
        sys.env
            .get("SCALUS_LEAN_WORKSPACE")
            .filter(_.nonEmpty)
            .map(Path.of(_))
            .getOrElse(librarySources)

    /** The workspace of Scalus's Lean library in Scalus's sources. */
    def librarySources: Path = inSources("scalus-verification", "src", "main", "lean")

    /** A directory of Scalus's sources, named from the build's root, and looked up from the working
      * directory upwards. A forked test of another module runs in that module's directory, not in
      * the build's root.
      */
    def inSources(first: String, more: String*): Path = {
        val directory = Path.of(first, more*)
        Iterator
            .iterate(Path.of("").toAbsolutePath)(_.getParent)
            .takeWhile(_ != null)
            .map(_.resolve(directory))
            .find(Files.isDirectory(_))
            .getOrElse(directory)
    }

    /** Whether a check can run in `workspace`: Scalus's Lean library, which every check imports, is
      * compiled. It lies in the workspace itself, or in one that the workspace requires by its
      * path.
      */
    def isBuilt(workspace: Path): Boolean =
        (workspace :: required(workspace)).exists { directory =>
            Files.isRegularFile(directory.resolve(".lake/build/lib/lean/ScalusProofs/Run.olean"))
        }

    /** A package of a workspace's manifest: one that is cloned has a revision, and one that is
      * required by its path has a directory.
      */
    private final case class Package(name: String, rev: Option[String], dir: Option[String])
    private final case class Manifest(packages: List[Package])
    private given JsonValueCodec[Manifest] = JsonCodecMaker.make

    /** The packages of the manifest of `workspace`: none where it has no manifest. */
    private def packages(workspace: Path): List[Package] = {
        val manifest = workspace.resolve("lake-manifest.json")
        if Files.isRegularFile(manifest) then
            readFromArray[Manifest](Files.readAllBytes(manifest)).packages
        else Nil
    }

    /** The workspaces that `workspace` requires by their paths. */
    private def required(workspace: Path): List[Path] =
        packages(workspace).flatMap(_.dir).map(workspace.resolve(_).normalize)

    /** The revisions at which the manifest of `workspace` pins the packages it clones, by their
      * names.
      */
    def pinned(workspace: Path): Map[String, String] =
        packages(workspace).collect { case Package(name, Some(revision), _) =>
            name -> revision
        }.toMap

    /** The Lean that `workspace` pins. */
    def toolchain(workspace: Path): String =
        Files.readString(workspace.resolve("lean-toolchain")).trim
}
