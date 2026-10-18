package scalus.verify.uplcblaster

import scalus.uplc.Constant
import scalus.verify.*
import scalus.verify.lean.{LeanServer, LeanServerProvider, LeanServers, LeanWorkspace, LeanWorkspaceTool}

import java.io.File
import java.lang.ref.Cleaner
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Runs the statements of a [[Verifier]] with [[UplcBlaster]] through Lean, for whoever states
  * them: a test of any framework, or a program. It has one Lean server at a time, started for its
  * first check and ended by [[close]].
  *
  * Statements are declared by name in [[verifier]], as statements or as negative controls
  * ([[Verifier.refute]]), and run with [[check]], which holds each to what it was declared to
  * expect (docs/design/verification-overview.md §5.1, §5.2):
  *
  * {{{
  * val proofs = LeanProofs(LeanProofs.projectWorkspace, Some(keptFile))
  * val zero = proofs.verifier.statement("plus_zero", forAll[BigInt](x => x + BigInt(0) == x))
  * proofs.check(zero, 40)
  * }}}
  *
  * The anonymous `proven`, `refuted` and `inconclusive` state a statement without a name, in a
  * verifier of their own.
  *
  * It asks for no test framework, and says what happened in two ways.
  *   - An expectation that does not hold, as a statement that is refuted where a proof was
  *     expected, throws an `AssertionError`. Every test framework takes that for a failed test.
  *   - A statement that cannot be checked here is another thing: Lean does not run here, or the
  *     result kept for the statement is stale and there is no Lean to ask. What happens then is
  *     `notChecked`, the caller's. A test framework has its own way to say that a test did not run,
  *     as ScalaTest's `cancel`. Without one, a [[LeanProofs.NotChecked]] is thrown.
  *
  * Proofs about code can keep their results, in the file `kept`
  * (docs/design/verification-overview.md §5.6): a statement that has not changed is then not asked
  * of Lean again, and where no Lean runs, it stands on what is kept. A result is kept under the
  * name of its statement; a statement stated without one, as by `proven`, is named by its content
  * (docs/design/verification-overview.md §4.4), so that it keeps its result wherever it is stated.
  * An entry says the tests that state it, as `running` or [[during]] says what runs. Where a test
  * ran to its end ([[finished]]), [[close]] takes it out of the entries of statements it no longer
  * states, and an entry goes that no test states any longer ([[KeptResults.prune]]).
  *
  * {{{
  * val proofs = LeanProofs(LeanProofs.projectWorkspace)
  * import proofs.*
  * proven(forAll[BigInt](x => x + BigInt(0) == x), 40)
  * }}}
  *
  * @param workspace
  *   the Lean workspace the checks run in: the project's, [[LeanProofs.projectWorkspace]]. A
  *   workspace of a project's own requires Scalus's Lean library, as a check imports it.
  * @param kept
  *   the file of the kept results, or none where they are not kept. It is for proofs about code.
  *   Where the tactic itself is tested, Lean is asked every time: what is tested is not in a
  *   statement's fingerprint.
  * @param notChecked
  *   what happens where a statement cannot be checked here, with the reason. It does not return.
  * @param running
  *   the name of what runs now, asked when a statement is checked: a test framework says the test.
  *   Where tests of one instance run at the same time, it has to say which of them is asking.
  */
final class LeanProofs private (
    val workspace: Path,
    kept: Option[Path],
    notChecked: String => Nothing,
    running: () => String,
    checksKeptIn: Option[Path]
) extends AutoCloseable {

    /** The Lean servers of the workspace: none is started before a check asks for one. */
    private val servers = LeanServers.in(workspace)

    /** Ends the server where [[close]] was not called and this is no longer used. A server that
      * still runs when the JVM ends is ended by the server itself.
      */
    private val cleanable = LeanProofs.cleaner.register(this, LeanProofs.Closing(servers))

    /** Ends the Lean server that runs. None is started after. Closing again does nothing. */
    override def close(): Unit = {
        try results.foreach(_.prune(completed.asScala.toSet, existing))
        finally cleanable.clean()
    }

    /** The tests that ran to their end ([[finished]]): [[close]] prunes the kept results they no
      * longer state.
      */
    private val completed = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()

    /** Says that the test `label` ran to its end, so that what it no longer states is not kept. A
      * test framework says it of every test that passed; [[during]] says it where its body returns.
      */
    def finished(label: String): Unit = { completed.add(label) }

    /** All the tests there are, said by [[tests]]. */
    @volatile private var existing: Option[Set[String]] = None

    /** Says which tests there are, all of them, whichever of them run: what is kept only for tests
      * that are not among them, as a test that was renamed or removed, is not kept. A test
      * framework knows them, once all of them are registered: it says them before [[close]].
      */
    def tests(names: Set[String]): Unit = { existing = Some(names) }

    /** Gives a tactic the Lean server: the one that runs, or another where that one has ended. A
      * tactic asks when it runs a check, and the statement is then not checked, or the check fails,
      * where Lean cannot run here ([[requireLean]]). A tactic that runs no check needs no Lean.
      */
    val lean: LeanServerProvider = new LeanServerProvider {
        override def server(): Either[String, LeanServer] = {
            requireLean()
            servers.server()
        }
        override def workspace: Option[Path] = Some(LeanProofs.this.workspace)
    }

    /** Whether Lean can run here: `lake` on the `PATH`, and Scalus's Lean library built where the
      * workspace has it. Build the workspace with `lake build` to run the checks.
      */
    lazy val leanAvailable: Boolean =
        sys.env
            .getOrElse("PATH", "")
            .split(File.pathSeparator)
            .exists(directory =>
                // `lake.exe` is its name on Windows.
                List("lake", "lake.exe")
                    .exists(lake => Files.isExecutable(Path.of(directory, lake)))
            ) &&
            LeanProofs.isBuilt(workspace)

    /** Says that nothing is checked where Lean cannot run here ([[notChecked]]). With the
      * `SCALUS_REQUIRE_LEAN` environment variable set, as in the Lean-Proofs workflow, a missing
      * Lean is a failure, so the proofs cannot pass by not running.
      */
    def requireLean(): Unit =
        if !leanAvailable then
            val missing = s"requires lake and a built Lean workspace in $workspace: " +
                "run `lake build` there, the sbt task leanBuild"
            if sys.env.contains("SCALUS_REQUIRE_LEAN") then failed(missing)
            else notChecked(missing)

    /** How the kept results are used: as the environment variable `SCALUS_KEPT_RESULTS` says,
      * `use`, `recalculate`, `frozen`, or `off` for not at all. Without it, a run that requires
      * Lean recalculates, as the Lean-Proofs workflow does; one that has Lean uses what is kept;
      * and one without Lean, as ci-jvm, takes what is kept and asks nothing.
      */
    lazy val keptMode: Option[KeptResults.Mode] =
        if kept.isEmpty then None
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

    /** The kept results, used in the way of [[keptMode]], by the test that runs. */
    private lazy val results: Option[KeptResults] =
        for
            file <- kept
            mode <- keptMode
        yield KeptResults.in(file, mode, () => currentLabel)

    /** What `during` says runs, in place of what `running` says. */
    private var named: Option[String] = None

    /** Runs `body` as `context`, for a caller that has no `running` to give: the kept results and
      * the kept checks of what it checks are said to be of `context`. Where `body` returns,
      * `context` has [[finished]]. After it, what ran before runs again. Where `running` says what
      * runs, as a test framework does, that is what runs, and `during` names nothing.
      *
      * It names what runs for every thread that states meanwhile. Tests that run at the same time
      * are told apart by `running`.
      */
    def during[A](context: String)(body: => A): A = {
        val before = synchronized {
            val before = named
            named = Some(context)
            before
        }
        try
            val result = body
            if running().isEmpty then finished(context)
            result
        finally synchronized { named = before }
    }

    /** What runs now: the test that states what is checked, as `running` says, or else what
      * [[during]] says, or the empty name for none.
      */
    private def currentLabel: String = {
        val test = running()
        if test.nonEmpty then test else synchronized { named.getOrElse("") }
    }

    /** The Lean server itself, for whoever speaks to it. It is these proofs': it is ended with
      * them, also where they are no longer used and were not closed. Whoever speaks to it keeps the
      * proofs.
      */
    def leanServer: LeanServer = lean.server() match
        case Right(server) => server
        case Left(reason)  => failed(reason)

    /** Starts a Lean server in `directory`, for whoever closes it. Nothing is checked, or the check
      * fails, where Lean cannot run for these proofs ([[requireLean]]): that is asked of their own
      * workspace, and not of `directory`, which may have no library of its own. It fails where the
      * server does not start.
      */
    def startLean(directory: Path): LeanServer = {
        requireLean()
        LeanServer.start(directory) match
            case Right(started) => started
            case Left(reason)   => failed(reason)
    }

    /** The verifier of these proofs, in which statements are declared by name, and run with
      * [[check]]. Their results are kept in the file of these proofs, where they keep them, under
      * the statements' names, and used in the way of [[keptMode]]: a statement keeps its result
      * while statements around it change, are added or are removed.
      */
    lazy val verifier: Verifier = results.fold(Verifier.empty)(Verifier.keeping)

    /** Runs `request` in [[verifier]], and holds its result to the one the request expects. A
      * statement that cannot be checked here, as Lean does not run here or its kept result is
      * stale, is not checked ([[notChecked]]). A result other than the expected one throws an
      * `AssertionError` that says what came instead: the counterexample of a refutation, replayed,
      * or the reason.
      *
      * Checks run one at a time: the verifier is not made for concurrent use, and its Lean server
      * runs one check after another anyway. An [[UplcBlaster]] of the request writes its checks
      * where these proofs keep them ([[keepingChecks]]).
      *
      * @return
      *   the result: a proof where the request expects one, and a refutation, whose counterexample
      *   was replayed on the Scalus CEK, where it expects that
      */
    def check(request: VerificationRequest): VerificationResult = {
        val name = request.statement.name
        val tactic = request.tactic match
            case blaster: UplcBlaster => keepingChecks(blaster, request.statement)
            case other                => other
        val result = verifier.synchronized { verifier.verify(request.statement, tactic) }
        (request.expected, result) match
            case (_, VerificationResult.Inconclusive(reason))
                if reason.startsWith(KeptResults.stale) =>
                notChecked(s"$name: $reason")
            case (ExpectedOutcome.Valid, VerificationResult.Proven(proof)) =>
                proof.artifact match
                    case artifact: UplcBlaster.Artifact => checkedProof(artifact)
                    case _                              =>
            case (ExpectedOutcome.Falsified, VerificationResult.Refuted(_)) =>
            case (ExpectedOutcome.Valid, other) =>
                failed(s"$name: expected a proof, got ${LeanProofs.describe(other)}")
            case (ExpectedOutcome.Falsified, other) =>
                failed(s"$name: expected a refutation, got ${LeanProofs.describe(other)}")
        result
    }

    /** [[check]] with [[UplcBlaster]] at a budget it finds ([[Budget.Auto]]), expecting what
      * `statement` was declared to be. The budget is a result, not a part of the statement: where
      * results are kept, it is kept with the proof, and a later search starts at it, so a statement
      * that has not changed is decided in one check, or taken as it was kept.
      */
    def check(statement: Statement): VerificationResult = {
        check(statement, Budget.Auto)
    }

    /** [[check]] with [[UplcBlaster]] at a budget of `budget` steps of Lean's machine, expecting
      * what `statement` was declared to be.
      */
    def check(statement: Statement, budget: Int): VerificationResult = {
        check(statement, Budget.LeanSteps(budget))
    }

    /** [[check]] with [[UplcBlaster]] at `budget`, which the tactic may find ([[Budget.Auto]]),
      * expecting what `statement` was declared to be.
      */
    def check(statement: Statement, budget: Budget): VerificationResult = {
        check(VerificationRequest(statement, UplcBlaster(budget, lean)))
    }

    /** `tactic`, writing the checks of `statement` into a directory of its own
      * ([[LeanProofs.checksDirectoryName]], with the test that runs), under the directory of these
      * proofs, or else the one of `SCALUS_LEAN_KEEP_CHECKS`. The directory holds the checks of the
      * statement's last run ([[UplcBlaster.withLastChecksKeptIn]]). A tactic that was told where to
      * write its checks writes them there, as it was told.
      */
    private def keepingChecks(tactic: UplcBlaster, statement: Statement): UplcBlaster = {
        if tactic.checksKeptIn.isDefined then tactic
        else
            checksKeptIn.orElse(tactic.keptChecks) match
                case None => tactic
                case Some(base) =>
                    val directory = LeanProofs.checksDirectoryName(statement, currentLabel)
                    tactic.withLastChecksKeptIn(base.resolve(directory))
    }

    /** Fails where a proof by [[UplcBlaster]] is not one by its own account: Blaster did not say
      * valid, or Lean reported an error in deciding a closed statement. A proof that was kept has
      * nothing of Lean's from this run.
      */
    private def checkedProof(artifact: UplcBlaster.Artifact): Unit = {
        artifact.kind match
            case ProofKind.Blaster =>
                if !artifact.kept && !artifact.output.contains("✅ Valid") then
                    failed(s"expected a valid proof, got ${artifact.output}")
            case ProofKind.LeanNative =>
                if artifact.output.contains("error") then
                    failed(s"expected a proof without an error, got ${artifact.output}")
            case other => failed(s"unexpected proof kind $other")
    }

    /** Declares `prop` in a fresh verifier and runs [[UplcBlaster]] on it, through Lean or, where
      * the results are kept, from what is kept. A statement that is stale, with nothing kept for it
      * as it is now and no Lean to ask, is not checked ([[notChecked]]).
      */
    def check(
        prop: Prop,
        budget: Budget,
        functions: Seq[FunctionDef[?, ?]]
    ): (Verifier, Statement, VerificationResult) = {
        val verifier = results.fold(Verifier.empty)(Verifier.keeping)
        functions.foreach(verifier.addFunction)
        val statement = verifier.statement(prop)
        verifier.verify(statement, keepingChecks(UplcBlaster(budget, lean), statement)) match
            case VerificationResult.Inconclusive(reason) if reason.startsWith(KeptResults.stale) =>
                notChecked(reason)
            case result => (verifier, statement, result)
    }

    /** [[check]], at a budget of `budget` steps of Lean's machine. */
    def check(
        prop: Prop,
        budget: Int,
        functions: Seq[FunctionDef[?, ?]]
    ): (Verifier, Statement, VerificationResult) =
        check(prop, Budget.LeanSteps(budget), functions)

    /** Proves `prop` at a budget of `budget` steps, and returns how the proof was checked. */
    def proven(prop: Prop, budget: Int, functions: FunctionDef[?, ?]*): ProofKind =
        proof(prop, Budget.LeanSteps(budget), functions*).kind

    /** Proves `prop` at a budget the tactic finds, and returns that budget. */
    def provenAt(prop: Prop, functions: FunctionDef[?, ?]*): Int =
        proof(prop, Budget.Auto, functions*).budget

    /** Proves `prop`, and returns what the tactic keeps of the proof. */
    def proof(
        prop: Prop,
        budget: Budget,
        functions: FunctionDef[?, ?]*
    ): UplcBlaster.Artifact =
        check(prop, budget, functions) match
            case (verifier, statement, VerificationResult.Proven(proof)) =>
                val artifact = proof.artifact.asInstanceOf[UplcBlaster.Artifact]
                checkedProof(artifact)
                if !verifier.theorems.exists(_.statement eq statement) then
                    failed("the statement was proved, and is not among the verifier's theorems")
                artifact
            case (_, _, other) => failed(s"expected a proof, got ${LeanProofs.describe(other)}")

    /** The replayed counterexample of a statement that is refuted at a budget of `budget` steps.
      */
    def refuted(
        prop: Prop,
        budget: Int,
        functions: FunctionDef[?, ?]*
    ): Map[String, Constant] =
        refuted(prop, Budget.LeanSteps(budget), functions*)

    /** The replayed counterexample of a refuted statement. */
    def refuted(
        prop: Prop,
        budget: Budget,
        functions: FunctionDef[?, ?]*
    ): Map[String, Constant] =
        check(prop, budget, functions) match
            case (verifier, _, VerificationResult.Refuted(proof)) =>
                if verifier.theorems.nonEmpty then
                    failed("the statement was refuted, and the verifier has a theorem")
                proof.artifact.asInstanceOf[UplcBlaster.Artifact].counterexample.toMap
            case (_, _, other) =>
                failed(s"expected a refutation, got ${LeanProofs.describe(other)}")

    /** Why [[UplcBlaster]] is inconclusive about `prop`, with its check given up after `timeout`.
      */
    def inconclusive(
        prop: Prop,
        budget: Int,
        timeout: FiniteDuration,
        functions: FunctionDef[?, ?]*
    ): String =
        inconclusive(prop, UplcBlaster(Budget.LeanSteps(budget), lean, timeout), functions*)

    /** Why `tactic` is inconclusive about `prop`. */
    def inconclusive(
        prop: Prop,
        tactic: UplcBlaster,
        functions: FunctionDef[?, ?]*
    ): String = {
        val verifier = Verifier.empty
        functions.foreach(verifier.addFunction)
        val statement = verifier.statement(prop)
        verifier.verify(statement, keepingChecks(tactic, statement)) match
            case VerificationResult.Inconclusive(reason) =>
                if verifier.theorems.nonEmpty then
                    failed("the statement is inconclusive, and the verifier has a theorem")
                reason
            case other => failed(s"expected an inconclusive result, got $other")
    }

    /** The integer `value` is, as a variable of a counterexample. */
    def integer(value: Constant): BigInt = value match
        case Constant.Integer(integer) => integer
        case other                     => failed(s"expected an integer, got $other")

    private def failed(message: String): Nothing = throw new AssertionError(message)
}

object LeanProofs {

    /** A statement was not checked here, with the reason: thrown where the caller of [[LeanProofs]]
      * says nothing else.
      */
    final class NotChecked(reason: String) extends RuntimeException(reason)

    /** Proofs in `workspace` that keep no results, and throw [[NotChecked]]. */
    def apply(workspace: Path): LeanProofs = apply(workspace, None)

    /** Proofs in `workspace` that keep their results in `kept`, and throw [[NotChecked]]. */
    def apply(workspace: Path, kept: Option[Path]): LeanProofs =
        apply(workspace, kept, reason => throw new NotChecked(reason))

    /** Proofs in `workspace`, with what happens where a statement cannot be checked here. */
    def apply(workspace: Path, kept: Option[Path], notChecked: String => Nothing): LeanProofs =
        apply(workspace, kept, notChecked, () => "")

    /** Proofs in `workspace`, with what happens where a statement cannot be checked here, and the
      * name of what runs when a statement is stated.
      */
    def apply(
        workspace: Path,
        kept: Option[Path],
        notChecked: String => Nothing,
        running: () => String
    ): LeanProofs =
        apply(workspace, kept, notChecked, running, None)

    /** Proofs as above, whose tactics also write every check they run into `checksKeptIn`, each in
      * a directory of its own ([[UplcBlaster.withChecksKeptIn]]). With `None` they follow the
      * environment variable `SCALUS_LEAN_KEEP_CHECKS`. A statement whose kept result is taken runs
      * no check, and writes none.
      */
    def apply(
        workspace: Path,
        kept: Option[Path],
        notChecked: String => Nothing,
        running: () => String,
        checksKeptIn: Option[Path]
    ): LeanProofs =
        new LeanProofs(workspace, kept, notChecked, running, checksKeptIn)

    private val cleaner = Cleaner.create()

    /** Whether `c` is kept in the name of a directory: a lower-case letter or a digit of ASCII, or
      * one of `._-`.
      */
    private def plain(c: Char): Boolean =
        (c < 128 && c.isLetterOrDigit && !c.isUpper) || c == '.' || c == '_' || c == '-'

    /** The name of the directory for the checks of the statement `name`: the name itself where it
      * is a plain one ([[plain]]) of 100 characters at most. Otherwise each other character is
      * replaced, the first 100 are taken, as a file system takes names of 255 bytes at most, and a
      * hash of the name is added, so that two names that differ only there, or only in case, as a
      * file system may not tell them apart, do not share a directory.
      */
    private[uplcblaster] def directoryName(name: String): String = {
        if name.forall(plain) && !name.forall(_ == '.') && name.length <= 100 then name
        else
            val safe = name.take(100).map(c => if plain(c) then c else '_')
            val digest = java.security.MessageDigest.getInstance("SHA-256")
            val hash = digest
                .digest(name.getBytes(StandardCharsets.UTF_8))
                .take(4)
                .map(b => f"${b & 0xff}%02x")
                .mkString
            s"$safe-$hash"
    }

    /** The name of the directory of the checks of `statement`, which the test `test` checks: its
      * name, where it was declared with one ([[directoryName]]), and otherwise the test and its
      * content hash, as `only_a_payee_submits.3fa2c0d4e5f6a7b8`, or the hash alone without a test.
      * The hash tells statements of one test apart. Of the test, the first 100 characters are
      * taken, made plain ([[plain]]): a file system takes names of 255 bytes at most.
      */
    private[uplcblaster] def checksDirectoryName(statement: Statement, test: String): String = {
        val hash = statement.name.drop(1)
        if !statement.byContent then directoryName(statement.name)
        else if test.isEmpty then hash
        else
            val plainTest =
                test.toLowerCase(java.util.Locale.ROOT)
                    .take(100)
                    .map(c => if plain(c) then c else '_')
            s"$plainTest.$hash"
    }

    /** A result in a few words, for a failure: a refutation with its replayed counterexample, or
      * the reason there is no result.
      */
    private[uplcblaster] def describe(result: VerificationResult): String = {
        result match
            case VerificationResult.Proven(_) => "a proof"
            case VerificationResult.Refuted(proof) =>
                proof.artifact match
                    case artifact: UplcBlaster.Artifact if artifact.counterexample.nonEmpty =>
                        artifact.counterexample
                            .map((variable, value) => s"$variable = $value")
                            .mkString("a refutation, replayed with ", ", ", "")
                    case _ => "a refutation"
            case VerificationResult.Inconclusive(reason) => s"no result: $reason"
            case VerificationResult.Failed(reason)       => s"a failure: $reason"
            case VerificationResult.Unsupported(report)  => s"an unsupported statement: $report"
    }

    /** What ends the servers of proofs that are no longer used. It does not hold the proofs. */
    private final class Closing(servers: LeanServers) extends Runnable {
        override def run(): Unit = servers.close()
    }

    /** The resource by which a build tells the tests of a project where the project's Lean
      * workspace is: the directory, as text. The Scalus sbt plugin writes it, from its setting
      * `leanWorkspace`.
      */
    val workspaceResource: String = "scalus/verify/lean/workspace"

    /** The Lean workspace of the project whose tests run. The build has it, and no test names it.
      *   - Where the build says it ([[workspaceResource]], on the class path `loader` reads), it is
      *     that directory, made ready ([[ready]]).
      *   - Otherwise it is the workspace of Scalus's Lean library ([[libraryWorkspace]]), where
      *     there is one: in Scalus's own sources.
      *
      * It fails where there is neither, so that proofs do not pass by having no workspace.
      */
    def projectWorkspace(loader: ClassLoader): Path =
        Option(loader.getResourceAsStream(workspaceResource)) match
            case Some(stream) =>
                val said = Using.resource(stream)(_.readAllBytes())
                ready(Path.of(new String(said, StandardCharsets.UTF_8).trim))
            case None =>
                if Files.isDirectory(libraryWorkspace) then libraryWorkspace
                else
                    throw new IllegalStateException(
                      "no Lean workspace is known for this project. With the Scalus sbt plugin, " +
                          "`sbt leanInit` creates the project's, and the build then says it to " +
                          "the tests. Without the plugin, name one: LeanProofs.ready(directory)"
                    )

    /** [[projectWorkspace]], with the class path of the thread that asks. */
    def projectWorkspace: Path = {
        val loader = Thread.currentThread.getContextClassLoader
        projectWorkspace(if loader != null then loader else getClass.getClassLoader)
    }

    /** The Lean workspace of a project's own in `directory`, made ready
      * ([[LeanWorkspaceTool.init]]): created where it is missing, with Scalus's Lean library where
      * its lakefile requires it from. That needs neither Lean nor the network. So whoever states
      * proofs has the workspace in a checkout that has run nothing else, and the results kept for
      * them stand there: they rest on the library's sources, which are not committed.
      *
      * It fails where the workspace does not agree with the library. Its checks would run with
      * another Lean, or other packages, than the library is for.
      *
      * A `directory` that is not absolute is taken from the working directory, which for the tests
      * of a module is the build's root unless they are forked. [[inSources]] finds a workspace that
      * is there from either.
      */
    def ready(directory: Path): Path = {
        val outcome = LeanWorkspaceTool.init(directory)
        if outcome.notes.nonEmpty then
            throw new AssertionError(
              (s"the Lean workspace ${outcome.workspace} does not agree with Scalus's Lean " +
                  "library" :: outcome.notes).mkString("\n  ")
            )
        outcome.workspace
    }

    /** The workspace of Scalus's Lean library: the directory the environment variable
      * `SCALUS_LEAN_WORKSPACE` names, or the one in Scalus's sources. A project of its own has
      * neither, and a workspace of its own ([[ready]]).
      */
    lazy val libraryWorkspace: Path =
        sys.env
            .get("SCALUS_LEAN_WORKSPACE")
            .filter(_.nonEmpty)
            .map(Path.of(_))
            .getOrElse(librarySources)

    /** The workspace of Scalus's Lean library in Scalus's sources. */
    def librarySources: Path = inSources("scalus-verification", "src", "main", "lean")

    /** A directory of a build's sources, named from the build's root, and looked up from the
      * working directory upwards. A forked test of a module runs in that module's directory, not in
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
