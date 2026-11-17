package scalus.verify.uplcblaster

import scalus.verify.*
import scalus.verify.lean.{LeanServer, LeanServerProvider, LeanWorkspace}
import scalus.verify.lean.LeanServer.{Message, Severity}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** The tactic `lean-manual`: a statement proved by a theorem written by hand in the workspace
  * (docs/design/verification-overview.md §6.4).
  *
  * The statement is written into the workspace as the module `<Package>.Generated.<name>`, as
  * `blaster-uplc` lowers it, as written, at `budget` steps, and without the axiom `proved`: no
  * proof of Scalus found it ([[UplcBlaster.statementModule]]). [[LeanProofs]] writes it where its
  * proofs export, and holds the workspace's module to the code where they do not. `theorem`, a
  * theorem of the module `module`, which is under `<Package>/Proofs`, is to prove the module's
  * `statement`. Lake builds both modules, as Lean's server does not build an import that is out of
  * date, and the tactic has Lean elaborate
  * {{{
  * import <module>
  * import <the statement's module>
  * example : <the statement's module>.statement := <theorem>
  * #print axioms <theorem>
  * }}}
  * and reads what the proof rests on from the axioms:
  *   - Lean's own (`propext`, `Classical.choice`, `Quot.sound`): Lean's kernel, of the programs'
  *     runs as Blaster's optimizer gives them, [[ProofKind.LeanKernelOfOptimizedRuns]];
  *   - `Lean.ofReduceBool`: it evaluates with `native_decide`, [[ProofKind.LeanNative]];
  *   - `<Package>.Generated.<other>.proved`: it cites what Scalus proved of another statement,
  *     which is then one of the proof's lemmas, and is to be among the verifier's theorems. A
  *     second check has Lean take each such axiom as that of its statement's module, so a proof
  *     cannot declare one of its own under the name.
  *
  * A proof that uses `sorry`, cites the result of its own statement, or rests on another axiom is
  * none, and so is one that rests on Blaster's `blasterProven`: that axiom proves any proposition
  * where it is used without the tactic, and its uses do not tell one from the other. What Blaster
  * proves, a proof by hand cites by a statement Scalus checked. The first check of a proof may take
  * minutes: Lake builds the statement's module, whose programs Lean runs symbolically.
  *
  * A result is kept as `blaster-uplc`'s are: its fingerprint has the workspace's environment, the
  * hash of the Lean files under `<Package>/Proofs`, which the environment leaves out, the theorem,
  * the statement's text at the budget, and its programs. It keeps the hash of the module of each
  * statement it cites, and stands no longer where one has changed.
  */
final class LeanManual private (
    val module: String,
    val theorem: String,
    val budget: Int,
    val servers: LeanServerProvider,
    val timeout: Option[FiniteDuration],
    /** How the statement's variables are mapped, or none where it was not said: as for
      * [[UplcBlaster.mapping]].
      */
    val mapping: Option[VariableMapping]
) extends Tactic {
    require(budget > 0, "the budget of a statement proved by hand must be positive")
    require(timeout.forall(_.length > 0), "the lean-manual timeout must be positive")

    override type Prepared = LeanManual.Prepared

    override val name: String = "lean-manual"

    /** The tactic that lowers the statement, and writes its module. */
    private def lowering: UplcBlaster = {
        val blaster = UplcBlaster(Budget.LeanSteps(budget), servers)
        mapping.fold(blaster)(blaster.withMapping)
    }

    /** This tactic with the variables mapped as `preferred` says, where no mapping was said. */
    def withMappingUnlessSet(preferred: VariableMapping): LeanManual =
        if mapping.isDefined then this
        else new LeanManual(module, theorem, budget, servers, timeout, Some(preferred))

    override def prepare(goal: Goal): Either[CompatibilityReport, Prepared] =
        lowering.prepare(goal).map(lowered => LeanManual.Prepared(goal, lowered))

    /** The module of the statement in `workspace`, as this tactic writes it: without the axiom
      * `proved`, of the statement as written at the budget.
      */
    def statementModule(prepared: Prepared, workspace: Path): LeanManual.Written = {
        val id = LeanProofs.moduleName(prepared.goal.statement.name)
        val (moduleName, file) = LeanWorkspace.generated(workspace, id)
        val directory = workspace.relativize(file.getParent.resolve(id)).toString
        LeanManual.Written(
          moduleName,
          file,
          directory,
          UplcBlaster.statementModule(
            prepared.asWritten,
            budget,
            None,
            lowering.maxHeartbeats,
            moduleName,
            directory
          )
        )
    }

    override def run(prepared: Prepared, earlier: Option[Kept]): ExecutionResult =
        servers.workspace match
            case None =>
                VerificationResult.Failed(
                  "lean-manual needs a workspace: the statement's module and the proof are in it"
                )
            case Some(workspace) =>
                val proofs =
                    s"${LeanWorkspace.packageName(workspace)}.${LeanWorkspace.ProofsDirectory}."
                if !module.startsWith(proofs) then
                    VerificationResult.Failed(
                      s"the module of a proof by hand is to be under $proofs, and $module is " +
                          "not: the result is kept by the files there, and one elsewhere would " +
                          "change no result that is kept"
                    )
                else
                    val deadline = timeout.map(_.fromNow)
                    val statementModule = LeanManual.moduleOf(workspace, prepared)
                    servers.server() match
                        case Left(reason) => VerificationResult.Failed(reason)
                        case Right(server) =>
                            LeanManual
                                .built(workspace, List(module, statementModule), deadline)
                                .getOrElse(
                                  checked(prepared, workspace, server, statementModule, deadline)
                                )

    /** Lean's check that `theorem` proves the statement of `prepared`, the module
      * `statementModule`, in `server`, with what is left of the time until `deadline`.
      */
    private def checked(
        prepared: Prepared,
        workspace: Path,
        server: LeanServer,
        statementModule: String,
        deadline: Option[Deadline]
    ): ExecutionResult = {
        val text = LeanManual.checkText(module, statementModule, theorem)
        LeanManual.finished(server.check(text, LeanManual.left(deadline)), theorem) match
            case Left(result) => result
            case Right(messages) =>
                val packageName = LeanWorkspace.packageName(workspace)
                LeanManual.read(messages, packageName, statementModule, theorem) match
                    case Left(result) => result
                    case Right((kind, axioms, cited)) =>
                        LeanManual.theorems(prepared.goal, cited) match
                            case Left(reason) => VerificationResult.Inconclusive(reason)
                            case Right(lemmas) =>
                                citations(workspace, server, cited, deadline).getOrElse(
                                  VerificationResult.Proven(
                                    Proof(
                                      LeanManual.Artifact(
                                        theorem,
                                        module,
                                        axioms,
                                        kind,
                                        budget,
                                        prepared.asWritten.programHashes,
                                        LeanManual.citedModules(workspace, cited),
                                        kept = false
                                      ),
                                      lemmas
                                    )
                                  )
                                )
    }

    /** Nothing where Lean takes each axiom `proved` the proof cites, of the statements whose
      * modules are `cited`, as that of its statement's module; else why the proof is none. A
      * proof's own axiom of such a name is another declaration of it, which Lean refuses.
      */
    private def citations(
        workspace: Path,
        server: LeanServer,
        cited: List[String],
        deadline: Option[Deadline]
    ): Option[ExecutionResult] =
        if cited.isEmpty then None
        else
            val modules = cited.map(id => LeanWorkspace.generated(workspace, id)._1)
            LeanManual.built(workspace, modules, deadline).orElse {
                val text = LeanManual.citationText(module, modules)
                LeanManual.finished(server.check(text, LeanManual.left(deadline)), theorem) match
                    case Left(result) => Some(result)
                    case Right(messages) =>
                        val errors = messages.filter(_.severity == Severity.Error)
                        Option.when(errors.nonEmpty)(
                          VerificationResult.Failed(
                            s"$theorem cites a result that is not its statement's: " +
                                LeanManual.described(errors)
                          )
                        )
            }

    override def fingerprint(prepared: Prepared): Option[Fingerprint] =
        servers.workspace
            .filter(workspace => Files.isRegularFile(workspace.resolve("lean-toolchain")))
            .map { workspace =>
                val (statement, programs) = UplcBlaster.identity(prepared.lowered)
                Fingerprint(
                  LeanWorkspace.environment(workspace) +
                      (LeanManual.proofsPart -> LeanManual.proofsHash(workspace)),
                  Fingerprint.hash(s"$name $module $theorem $budget $statement"),
                  programs
                )
            }

    /** The statement as `blaster-uplc` tells it: a proof by hand of a statement proves another of
      * the same check and programs ([[Verifier.specifications]]).
      */
    override def identity(prepared: Prepared): Option[(String, List[String])] =
        Some(UplcBlaster.identity(prepared.lowered))

    override def keep(prepared: Prepared, result: ExecutionResult): Option[Kept] =
        result match
            case VerificationResult.Proven(proof) =>
                proof.artifact match
                    case artifact: LeanManual.Artifact =>
                        Some(
                          Kept(
                            Kept.Result.Proved,
                            Map(
                              LeanManual.kindNote -> artifact.kind.toString,
                              LeanManual.theoremNote -> artifact.theorem,
                              LeanManual.axiomsNote -> artifact.axioms.mkString(", "),
                              LeanManual.citedNote -> artifact.cited
                                  .map((id, hash) => s"$id=$hash")
                                  .mkString(", ")
                            ),
                            Map.empty
                          )
                        )
                    case _ => None
            case _ => None

    /** The proof that was kept, where it still stands: of this theorem, and citing statements whose
      * modules are still those it was checked with, and which are among the verifier's theorems.
      */
    override def restore(prepared: Prepared, kept: Kept): Either[String, ExecutionResult] =
        for
            _ <- Either.cond(kept.result == Kept.Result.Proved, (), "it is no proof")
            workspace <- servers.workspace.toRight("there is no workspace to read its proof in")
            kind <- kept.notes
                .get(LeanManual.kindNote)
                .flatMap(word => ProofKind.values.find(_.toString == word))
                .toRight(s"its kind note, ${kept.notes.get(LeanManual.kindNote)}, is no kind")
            _ <- Either.cond(
              kept.notes.get(LeanManual.theoremNote).contains(theorem),
              (),
              s"it is of ${kept.notes.get(LeanManual.theoremNote).mkString}, not of $theorem"
            )
            axioms = LeanManual.listed(kept.notes.get(LeanManual.axiomsNote))
            cited <- LeanManual.citedOf(kept.notes.get(LeanManual.citedNote))
            ids = cited.map(_._1)
            _ <- Either.cond(
              LeanManual.citedModules(workspace, ids) == cited,
              (),
              "the module of a statement it cites has changed"
            )
            lemmas <- LeanManual.theorems(prepared.goal, ids)
        yield VerificationResult.Proven(
          Proof(
            LeanManual.Artifact(
              theorem,
              module,
              axioms,
              kind,
              budget,
              prepared.asWritten.programHashes,
              cited,
              kept = true
            ),
            lemmas
          )
        )
}

object LeanManual {

    /** The time a check of a proof is given, by default: it may build the statement's module. */
    val defaultTimeout: FiniteDuration = 30.minutes

    /** `theorem`, of the module `module` of the servers' workspace, proves the statement it is
      * asked of, written at `budget` steps.
      */
    def apply(
        module: String,
        theorem: String,
        budget: Int,
        servers: LeanServerProvider
    ): LeanManual =
        new LeanManual(module, theorem, budget, servers, Some(defaultTimeout), None)

    /** [[apply]], with the check given up after `timeout`. */
    def apply(
        module: String,
        theorem: String,
        budget: Int,
        servers: LeanServerProvider,
        timeout: FiniteDuration
    ): LeanManual =
        new LeanManual(module, theorem, budget, servers, Some(timeout), None)

    /** A statement as the tactic lowered it, with the goal whose lemmas a proof may cite. */
    final case class Prepared(goal: Goal, lowered: UplcBlaster.Lowered) {

        /** The lowering the statement's module states: the statement as written. */
        def asWritten: UplcBlaster.Lowered = lowered.unrewritten.getOrElse(lowered)
    }

    /** A statement's module: its name, its file, the directory of its programs from the
      * workspace's, and what is written.
      */
    final case class Written(
        module: String,
        file: Path,
        directory: String,
        written: UplcBlaster.StatementModule
    )

    /** A proof by hand that Lean took: the theorem, its module, the axioms it rests on, the kind
      * they make it, the budget of the statement, the hashes of its programs, and the hash of the
      * module of each statement it cites, by the module's name in `Generated`.
      */
    final case class Artifact(
        theorem: String,
        module: String,
        axioms: List[String],
        kind: ProofKind,
        budget: Int,
        programHashes: List[String],
        cited: List[(String, String)],
        kept: Boolean
    ) extends ProofArtifact

    /** Lean's own axioms, which a proof Lean's kernel checks may rest on. */
    val leanAxioms: Set[String] = Set("propext", "Classical.choice", "Quot.sound")

    /** The axioms of evaluation by `native_decide`. */
    val nativeAxioms: Set[String] = Set("Lean.ofReduceBool", "Lean.trustCompiler")

    /** The axiom with which Blaster closes a goal its solver finds valid, and which proves any
      * proposition where it is used without the tactic.
      */
    val blasterAxiom: String = "Blaster.Tactic.blasterProven"

    private[uplcblaster] val kindNote = "kind"
    private[uplcblaster] val theoremNote = "theorem"
    private[uplcblaster] val axiomsNote = "axioms"
    private[uplcblaster] val citedNote = "cited"

    /** The part of a fingerprint that has the proofs written by hand. */
    private[uplcblaster] val proofsPart = "proofs"

    /** The name of the module of the statement of `prepared` in `workspace`. */
    private[uplcblaster] def moduleOf(workspace: Path, prepared: Prepared): String =
        LeanWorkspace.generated(workspace, LeanProofs.moduleName(prepared.goal.statement.name))._1

    /** What is left of the time until `deadline`, for a check: at least a second. */
    private def left(deadline: Option[Deadline]): Option[FiniteDuration] =
        deadline.map(_.timeLeft.max(1.second))

    /** Builds `modules` in `workspace` with Lake, and the modules they import: Lean's server does
      * not build an import that is out of date, as one `leanExport` wrote again. Nothing where the
      * build succeeds; else why the proof is none, with Lake's errors, or that the time until
      * `deadline` passed.
      */
    private[uplcblaster] def built(
        workspace: Path,
        modules: List[String],
        deadline: Option[Deadline]
    ): Option[ExecutionResult] = {
        val output = Files.createTempFile("scalus-lean-manual-build-", ".log")
        try
            val process = new ProcessBuilder(("lake" :: "build" :: modules).asJava)
                .directory(workspace.toFile)
                .redirectErrorStream(true)
                .redirectOutput(output.toFile)
                .start()
            val ended = deadline match
                case Some(at) =>
                    process.waitFor(
                      at.timeLeft.toMillis.max(1L),
                      java.util.concurrent.TimeUnit.MILLISECONDS
                    )
                case None => process.waitFor(); true
            if !ended then
                scalus.verify.lean.Processes.stop(process, Nil)
                Some(
                  VerificationResult.Inconclusive(
                    s"Lake did not build ${modules.mkString(", ")} in time"
                  )
                )
            else if process.exitValue() != 0 then
                val log = Files
                    .readString(output, StandardCharsets.UTF_8)
                    .linesIterator
                    .filter(line => line.contains("error") || line.startsWith("✖"))
                    .take(20)
                    .mkString("; ")
                Some(
                  VerificationResult.Failed(s"Lake does not build ${modules.mkString(", ")}: $log")
                )
            else None
        finally Files.deleteIfExists(output)
    }

    /** The text Lean elaborates to check `theorem`, of `module`, as a proof of the statement of
      * `statementModule`.
      */
    def checkText(module: String, statementModule: String, theorem: String): String =
        s"""import $module
           |import $statementModule
           |
           |example : $statementModule.statement := $theorem
           |
           |#print axioms $theorem
           |""".stripMargin

    /** The text Lean elaborates to take the axiom `proved` of each of `statementModules` as that of
      * its module, where `module`, a proof's, is imported too.
      */
    def citationText(module: String, statementModules: List[String]): String =
        (s"import $module" :: statementModules.map(cited => s"import $cited")).mkString("\n") +
            "\n\n" +
            statementModules
                .map(cited => s"example : $cited.statement := $cited.proved")
                .mkString("\n") + "\n"

    /** Lean's messages, where it finished a check; else the result of a check it did not finish. */
    private def finished(
        result: LeanServer.Result,
        theorem: String
    ): Either[ExecutionResult, List[Message]] =
        result match
            case LeanServer.Result.Finished(messages) => Right(messages)
            case LeanServer.Result.TimedOut(_) =>
                Left(VerificationResult.Inconclusive(s"Lean did not check $theorem in time"))
            case LeanServer.Result.Failed(reason) => Left(VerificationResult.Failed(reason))

    /** Lean's errors, by their lines. */
    private def described(errors: List[Message]): String =
        errors.map(message => s"${message.line}: ${message.text}").mkString("; ")

    /** The axioms `#print axioms` says `theorem` depends on, from Lean's messages: none where it
      * says nothing of it.
      */
    def axiomsOf(messages: List[Message], theorem: String): Option[List[String]] =
        messages
            .map(_.text)
            .find(text => text.contains(s"'$theorem'") && text.contains("depend"))
            .map { text =>
                if text.contains("does not depend on any axioms") then Nil
                else
                    val start = text.indexOf('[')
                    val end = text.lastIndexOf(']')
                    if start < 0 || end < start then Nil
                    else
                        text.substring(start + 1, end)
                            .split(',')
                            .map(_.trim)
                            .filter(_.nonEmpty)
                            .toList
            }

    /** What Lean's check of `theorem`, as a proof of the statement of `statementModule`, says: the
      * kind of the proof, its axioms, and the modules of the statements it cites; else the result
      * of a check that is no proof. An error is none, and the axioms say what the proof rests on.
      */
    private def read(
        messages: List[Message],
        packageName: String,
        statementModule: String,
        theorem: String
    ): Either[ExecutionResult, (ProofKind, List[String], List[String])] = {
        val errors = messages.filter(_.severity == Severity.Error)
        if errors.nonEmpty then
            Left(
              VerificationResult.Failed(
                s"Lean does not take $theorem as a proof of $statementModule.statement: " +
                    described(errors)
              )
            )
        else
            axiomsOf(messages, theorem) match
                case None =>
                    Left(VerificationResult.Failed(s"Lean did not say the axioms of $theorem"))
                case Some(axioms) =>
                    judgedAxioms(axioms, packageName, statementModule, theorem) match
                        case Left(reason)         => Left(VerificationResult.Inconclusive(reason))
                        case Right((kind, cited)) => Right((kind, axioms, cited))
    }

    /** The kind of a proof of the statement of `statementModule` that rests on `axioms`, and the
      * names of the modules of the statements whose results it cites (`<id>` of
      * `<packageName>.Generated.<id>.proved`), or why it is no proof.
      */
    def judgedAxioms(
        axioms: List[String],
        packageName: String,
        statementModule: String,
        theorem: String
    ): Either[String, (ProofKind, List[String])] = {
        val proved = s"$packageName.${LeanWorkspace.GeneratedDirectory}."
        def cited(axiom: String): Option[String] =
            Option.when(axiom.startsWith(proved) && axiom.endsWith(".proved"))(
              axiom.stripPrefix(proved).stripSuffix(".proved")
            )
        val unknown = axioms.filterNot(axiom =>
            leanAxioms(axiom) || nativeAxioms(axiom) || axiom == blasterAxiom ||
                cited(axiom).isDefined
        )
        if axioms.contains("sorryAx") then Left(s"$theorem uses sorry: it is no proof")
        else if axioms.contains(blasterAxiom) then
            Left(
              s"$theorem rests on Blaster's $blasterAxiom, which proves any proposition where it " +
                  "is used without the tactic: cite what Blaster proved by the axiom `proved` of " +
                  "a statement Scalus checked"
            )
        else if unknown.nonEmpty then
            Left(
              s"$theorem rests on axioms that are not Lean's, or a result Scalus kept: " +
                  unknown.mkString(", ")
            )
        else if axioms.contains(s"$statementModule.proved") then
            Left(s"$theorem cites the result of its own statement, $statementModule.proved")
        else
            val kind =
                if axioms.exists(nativeAxioms) then ProofKind.LeanNative
                else ProofKind.LeanKernelOfOptimizedRuns
            Right(kind -> axioms.flatMap(cited))
    }

    /** The verifier's theorems of the statements whose modules are `ids`, or why one is not among
      * them: a proof that cites a result rests on it, and it is to be a theorem now.
      */
    private def theorems(goal: Goal, ids: List[String]): Either[String, List[Theorem]] =
        ids.foldRight[Either[String, List[Theorem]]](Right(Nil)) { (id, rest) =>
            for
                found <- goal.lemmas
                    .find(lemma => LeanProofs.moduleName(lemma.statement.name) == id)
                    .toRight(
                      s"the proof cites the result of the statement of the module $id, which the " +
                          "verifier has not proved"
                    )
                others <- rest
            yield found :: others
        }

    /** The hash of the module of each statement, of the modules `ids` in `workspace`'s `Generated`:
      * what a proof that cites them was checked with. Nothing of a module that is not there.
      */
    private def citedModules(workspace: Path, ids: List[String]): List[(String, String)] =
        ids.flatMap { id =>
            val file = LeanWorkspace.generated(workspace, id)._2
            Option.when(Files.isRegularFile(file))(
              id -> Fingerprint.hash(Files.readString(file, StandardCharsets.UTF_8))
            )
        }

    /** The modules a kept proof cites, with their hashes, as its note has them, `a=1, b=2`. */
    private def citedOf(note: Option[String]): Either[String, List[(String, String)]] =
        listed(note).foldRight[Either[String, List[(String, String)]]](Right(Nil)) { (item, rest) =>
            item.split('=') match
                case Array(id, hash) => rest.map((id, hash) :: _)
                case _ => Left(s"its note of the statements it cites, $item, is no pair")
        }

    /** The items of a list kept as a note, `a, b`. */
    private def listed(note: Option[String]): List[String] =
        note.filter(_.nonEmpty).fold(Nil)(_.split(",").map(_.trim).filter(_.nonEmpty).toList)

    /** A hash of the Lean files of the proofs written by hand of `workspace`, `<Package>/Proofs`,
      * by their paths and texts, as the environment's sources are hashed: the environment leaves
      * them out.
      */
    def proofsHash(workspace: Path): String = {
        val directory =
            workspace
                .resolve(LeanWorkspace.packageName(workspace))
                .resolve(LeanWorkspace.ProofsDirectory)
        val files =
            if Files.isDirectory(directory) then
                val walked = Files.walk(directory)
                try walked.iterator().asScala.toList
                finally walked.close()
            else Nil
        LeanWorkspace.hashOf(directory, files)
    }
}
