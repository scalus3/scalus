package scalus.verify.uplcblaster

import org.scalatest.{Assertions, BeforeAndAfterAll, Outcome, Tag, TestSuite, TestSuiteMixin}
import scalus.uplc.Constant
import scalus.verify.*
import scalus.verify.lean.{LeanServer, LeanServerProvider, LeanServers, LeanWorkspace, LeanWorkspaceTool}

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
  *
  * A suite of proofs about code can keep its results ([[keepsResults]]): a statement that has not
  * changed is then not asked of Lean again, and where no Lean runs, its test passes on what is
  * kept.
  */
trait LeanProofs extends TestSuiteMixin with Assertions with BeforeAndAfterAll { this: TestSuite =>

    /** The Lean workspace the suite's checks run in: that of Scalus's Lean library, unless the
      * suite overrides it. A workspace of its own requires that library, as a check imports it.
      */
    protected def leanWorkspace: Path = LeanProofs.libraryWorkspace

    /** The Lean workspace of a project's own in `directory`, made ready
      * ([[LeanWorkspaceTool.init]]): created where it is missing, with Scalus's Lean library where
      * its lakefile requires it from. That needs neither Lean nor the network, so a suite has its
      * workspace in a checkout that has run nothing else, and the results kept for the suite stand
      * there: they rest on the library's sources, which are not committed.
      *
      * The test fails where the workspace does not agree with the library. Its checks would run
      * with another Lean, or other packages, than the library is for.
      *
      * A suite keeps what this returns, and gives it as [[leanWorkspace]]:
      * {{{
      * private lazy val workspace = readyWorkspace(Path.of("src/test/lean/Htlc"))
      * override protected def leanWorkspace: Path = workspace
      * }}}
      */
    protected def readyWorkspace(directory: Path): Path = {
        val outcome = LeanWorkspaceTool.init(directory)
        if outcome.notes.nonEmpty then
            fail(
              (s"the Lean workspace ${outcome.workspace} does not agree with Scalus's Lean " +
                  "library" :: outcome.notes).mkString("\n  ")
            )
        outcome.workspace
    }

    /** The Lean servers of the suite's workspace: none is started before a check asks for one. */
    private lazy val servers = LeanServers.in(leanWorkspace)

    /** Gives a tactic the suite's Lean server: the one that runs, or another where that one has
      * ended. A tactic asks when it runs a check. The test is then canceled, or fails, where Lean
      * cannot run here ([[requireLean]]). One that runs no check needs no Lean.
      */
    protected def lean: LeanServerProvider = new LeanServerProvider {
        override def server(): Either[String, LeanServer] = {
            requireLean()
            servers.server()
        }
        override def workspace: Option[Path] = Some(leanWorkspace)
    }

    /** The file in which the suite keeps the results of the statements its helpers prove and refute
      * (docs/design/verification-overview.md §5.6), or none for a suite that does not keep them. It
      * is beside the suite's source, `<Suite>.proofs.json`, and is committed.
      *
      * It is for a suite of proofs about code. A suite about the tactic itself has Lean asked every
      * time: what it tests is not in a statement's fingerprint.
      */
    protected def keptResultsFile: Option[Path] = None

    /** How a suite that keeps its results uses them: as the environment variable
      * `SCALUS_KEPT_RESULTS` says, `use`, `recalculate`, `frozen`, or `off` for not at all. Without
      * it, a run that requires Lean recalculates, as the Lean-Proofs workflow does; one that has
      * Lean uses what is kept; and one without Lean, as ci-jvm, takes what is kept and asks
      * nothing.
      */
    protected lazy val keptMode: Option[KeptResults.Mode] =
        if keptResultsFile.isEmpty then None
        else
            sys.env.get("SCALUS_KEPT_RESULTS") match
                case Some("off")         => None
                case Some("use")         => Some(KeptResults.Mode.Use)
                case Some("recalculate") => Some(KeptResults.Mode.Recalculate)
                case Some("frozen")      => Some(KeptResults.Mode.Frozen)
                case Some(other) =>
                    throw new IllegalArgumentException(
                      s"SCALUS_KEPT_RESULTS is use, recalculate, frozen or off, not $other"
                    )
                case None =>
                    if sys.env.contains("SCALUS_REQUIRE_LEAN") then
                        Some(KeptResults.Mode.Recalculate)
                    else if leanAvailable then Some(KeptResults.Mode.Use)
                    else Some(KeptResults.Mode.Frozen)

    /** The results kept for the suite, used in the way of [[keptMode]]. */
    private lazy val results: Option[KeptResults] =
        for
            file <- keptResultsFile
            mode <- keptMode
        yield KeptResults.in(file, mode)

    /** The test that runs, and how many statements it has stated: a kept result is under both. */
    private var running = ""
    private var stated = 0

    abstract override protected def withFixture(test: NoArgTest): Outcome = {
        running = test.name
        stated = 0
        super.withFixture(test)
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
        val missing = s"requires lake and a built Lean workspace in $leanWorkspace: " +
            "run `lake build` there, the sbt task leanBuild"
        if sys.env.contains("SCALUS_REQUIRE_LEAN") then assert(leanAvailable, missing)
        else assume(leanAvailable, missing)
    }

    /** Declares `prop` in a fresh verifier and runs [[UplcBlaster]] on it, through Lean or, where
      * the suite keeps its results, from what is kept. A statement that is stale, with nothing kept
      * for it as it is now and no Lean to ask, cancels the test.
      */
    protected def run(
        prop: Prop,
        budget: Budget,
        functions: Seq[FunctionDef[?, ?]]
    ): (Verifier, Statement, VerificationResult) = {
        stated += 1
        val verifier = results.fold(Verifier.empty)(kept => Verifier.keeping(kept.under(running)))
        functions.foreach(verifier.addFunction)
        val statement = verifier.statement(s"#$stated", prop)
        verifier.verify(statement, UplcBlaster(budget, lean)) match
            case VerificationResult.Inconclusive(reason) if reason.startsWith(KeptResults.stale) =>
                cancel(reason)
            case result => (verifier, statement, result)
    }

    /** [[run]], at a budget of `budget` steps of Lean's machine. */
    protected def run(
        prop: Prop,
        budget: Int,
        functions: Seq[FunctionDef[?, ?]]
    ): (Verifier, Statement, VerificationResult) =
        run(prop, Budget.LeanSteps(budget), functions)

    /** Proves `prop` at a budget of `budget` steps, and returns how the proof was checked. */
    protected def proven(prop: Prop, budget: Int, functions: FunctionDef[?, ?]*): ProofKind =
        proof(prop, Budget.LeanSteps(budget), functions*).kind

    /** Proves `prop` at a budget the tactic finds, and returns that budget. */
    protected def provenAt(prop: Prop, functions: FunctionDef[?, ?]*): Int =
        proof(prop, Budget.Auto, functions*).budget

    /** Proves `prop`, and returns what the tactic keeps of the proof. */
    protected def proof(
        prop: Prop,
        budget: Budget,
        functions: FunctionDef[?, ?]*
    ): UplcBlaster.Artifact =
        run(prop, budget, functions) match
            case (verifier, statement, VerificationResult.Proven(proof)) =>
                val artifact = proof.artifact.asInstanceOf[UplcBlaster.Artifact]
                artifact.kind match
                    // A proof that was kept has nothing of Lean's from this run.
                    case ProofKind.Blaster =>
                        if !artifact.kept && !artifact.output.contains("✅ Valid") then
                            fail(s"expected a valid proof, got ${artifact.output}")
                    case ProofKind.LeanNative => assert(!artifact.output.contains("error"))
                    case other                => fail(s"unexpected proof kind $other")
                assert(verifier.theorems.exists(_.statement eq statement))
                artifact
            case (_, _, other) => fail(s"expected a proof, got $other")

    /** The replayed counterexample of a statement that is refuted at a budget of `budget` steps.
      */
    protected def refuted(
        prop: Prop,
        budget: Int,
        functions: FunctionDef[?, ?]*
    ): Map[String, Constant] =
        refuted(prop, Budget.LeanSteps(budget), functions*)

    /** The replayed counterexample of a refuted statement. */
    protected def refuted(
        prop: Prop,
        budget: Budget,
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
    ): String =
        inconclusive(prop, UplcBlaster(Budget.LeanSteps(budget), lean, timeout), functions*)

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
        (workspace :: LeanWorkspace.required(workspace).values.toList).exists { directory =>
            Files.isRegularFile(directory.resolve(".lake/build/lib/lean/ScalusProofs/Run.olean"))
        }

    /** The revisions at which the manifest of `workspace` pins the packages it clones, by their
      * names.
      */
    def pinned(workspace: Path): Map[String, String] = LeanWorkspace.pinned(workspace)

    /** The Lean that `workspace` pins. */
    def toolchain(workspace: Path): String = LeanWorkspace.toolchain(workspace)
}
