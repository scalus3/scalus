package scalus.verify.uplcblaster

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonReaderException, JsonValueCodec}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scalus.*
import scalus.cardano.ledger.ExUnits
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.compiler.Options
import scalus.compiler.sir.{AnnotatedSIR, AnnotationsDecl, Binding, ConstrDecl, DataDecl, SIR, SIRBuiltins, SIRType}
import scalus.compiler.sir.linking.Wrappers
import scalus.compiler.sir.lowering.{LoweringContext, ProductCaseClassRepresentation, SumCaseClassRepresentation}
import scalus.compiler.sir.lowering.typegens.SirTypeUplcGenerator
import scalus.uplc.{Constant, DeBruijn, DefaultFun, DefaultUni, NamedDeBruijn, Program, Term}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.eval.{MachineError, NoLogger, OutOfExBudgetError, PlutusVM, RestrictingBudgetSpender}
import scalus.utils.{Hex, Utils}
import scalus.verify.*
import scalus.verify.SirTypes.*
import scalus.verify.lean.{Directories, LeanServer, LeanServerProvider, LeanWorkspace}

import java.nio.file.{Files, Path}
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.{Duration, DurationInt, FiniteDuration}

/** How far a check lets the program of each test run.
  *
  * The steps are those of Lean's CEK machine, which counts a term it evaluates and a value it
  * returns alike: about 1.85 times the steps Plutus counts. A proof at any budget holds without the
  * budget. A budget that is too small gives a counterexample that is spurious, and one that is
  * larger than the runs the statement is about only costs: the programs are followed further,
  * through a loop for instance.
  */
enum Budget {

    /** The tactic finds the budget. It starts small, and where Lean's counterexample is spurious it
      * has Lean's machine count the steps the statement needs on that counterexample, and goes on
      * from there. The result names the budget it came to ([[UplcBlaster.Artifact]]). The tactic's
      * time limit is that of the whole search.
      */
    case Auto

    /** `steps` steps of Lean's machine. */
    case LeanSteps(steps: Int)

    /** A bounded check at `steps` steps of Lean's machine: of the statement on the inputs on which
      * each test's program ends within the budget. A run the budget cuts is read in the statement's
      * favour, and a proof is a [[scalus.verify.VerificationResult.ProvenWithin]], no theorem. A
      * counterexample is one where the runs the statement rests on ended, and is replayed as any
      * other. It is for a statement whose quantifiers all stand for every value: a run that is cut
      * is no witness of an `exists`.
      */
    case Within(steps: Int)
}

/** Proves [[scalus.verify.Prop]] statements about their compiled UPLC with Lean Blaster.
  *
  * The supported fragment has universal and existential quantifiers over `BigInt`, `Boolean`,
  * `ByteString`, `String`, `Data`, lists and maps, and case classes and enums of them, with tests,
  * calls, `denotes`, `equal` and the connectives. An `existsLet` is eliminated by applying its body
  * to the supplied witness. Every test is compiled to its own closed UPLC predicate over the
  * quantified values in its scope (see [[UplcBlaster.lower]]). The Lean CEK model runs each
  * predicate for at most `budget` steps, keeping a failing program apart from an exhausted budget,
  * and Blaster decides the resulting proposition. Each test is read according to its polarity
  * (design doc §6.2), so a proof at any budget holds without the budget, and a statement that a
  * program fails can be proved. A closed statement, without quantified variables, has nothing to
  * search for: Lean decides it by running its predicates, with `native_decide`
  * ([[ProofKind.LeanNative]]). A counterexample is replayed on the Scalus CEK before it is reported
  * as a refutation. Other statement shapes are rejected during preparation.
  *
  * The checks run in a Lean server, one after another: the server loads the workspace once, and a
  * check is an edit of its document (docs/design/verification-details/lean-server.md). The tactic
  * asks its provider for the server before every check, and so needs no Lean to be made, or to
  * prepare a statement.
  */
final class UplcBlaster private (
    val budget: Budget,
    val servers: LeanServerProvider,
    val timeout: Option[FiniteDuration],
    val attemptTimeout: Option[FiniteDuration],
    val maxHeartbeats: Int,
    val checksKeptIn: Option[Path],
    lastChecksOnly: Boolean
) extends Tactic {
    override type Prepared = UplcBlaster.Lowered

    budget match
        case Budget.LeanSteps(steps) =>
            require(steps > 0, "the UPLC Blaster budget must be positive")
        case Budget.Within(steps) =>
            require(steps > 0, "the UPLC Blaster budget must be positive")
        case Budget.Auto =>
    require(timeout.forall(_.length > 0), "the UPLC Blaster timeout must be positive")
    require(
      attemptTimeout.forall(_.length > 0),
      "the UPLC Blaster timeout of an attempt must be positive"
    )
    require(maxHeartbeats >= 0, "Lean's limit of work must not be negative")

    /** The same tactic with another limit on the work Lean does for one command of a check, in
      * place of [[UplcBlaster.defaultMaxHeartbeats]]. `0` is no limit: a check then runs until it
      * ends, or until the tactic's time limit.
      */
    def withMaxHeartbeats(limit: Int): UplcBlaster =
        new UplcBlaster(
          budget,
          servers,
          timeout,
          attemptTimeout,
          limit,
          checksKeptIn,
          lastChecksOnly
        )

    /** The same tactic with no time limit, of the whole run or of one attempt. A check then runs
      * until it ends or Lean gives it up at its limit of work, and one that does neither until its
      * thread is interrupted or the server closed.
      */
    def withoutTimeout: UplcBlaster =
        new UplcBlaster(budget, servers, None, None, maxHeartbeats, checksKeptIn, lastChecksOnly)

    /** The same tactic with a time limit for one attempt, beside that of the whole run. A tactic
      * that finds its budget ([[Budget.Auto]]) makes one check after the other, and its time limit
      * is for them all: without this, one check that follows a program through a loop takes all the
      * time that is left. A search can also go on from one quick check to the next without an end,
      * which only the limit of the whole run ends. A tactic whose budget is given makes one check,
      * which gets the shorter of the two.
      */
    def withAttemptTimeout(limit: FiniteDuration): UplcBlaster =
        new UplcBlaster(
          budget,
          servers,
          timeout,
          Some(limit),
          maxHeartbeats,
          checksKeptIn,
          lastChecksOnly
        )

    /** The same tactic, writing every check it runs into `directory` as well, each in a directory
      * of its own: the check's `Check.lean` and the files of its leaves, to read or to run by hand
      * with `lake lean Check.lean` in the workspace. It is set here in place of the environment
      * variable `SCALUS_LEAN_KEEP_CHECKS`, which a tactic otherwise follows.
      */
    def withChecksKeptIn(directory: Path): UplcBlaster =
        new UplcBlaster(
          budget,
          servers,
          timeout,
          attemptTimeout,
          maxHeartbeats,
          Some(directory),
          false
        )

    /** [[withChecksKeptIn]], keeping only the checks of the tactic's last run in `directory`: a run
      * removes what an earlier one wrote there before it writes its own. A run is what one
      * statement asks of the tactic, its search for a budget included; a statement whose kept
      * result is taken runs nothing, and leaves the directory as it is.
      */
    def withLastChecksKeptIn(directory: Path): UplcBlaster =
        new UplcBlaster(
          budget,
          servers,
          timeout,
          attemptTimeout,
          maxHeartbeats,
          Some(directory),
          true
        )

    /** Where the tactic writes the checks it runs: the directory it was told
      * ([[withChecksKeptIn]]), or the one the environment variable `SCALUS_LEAN_KEEP_CHECKS` names.
      */
    def keptChecks: Option[Path] = checksKeptIn.orElse(UplcBlaster.keptChecksFromEnvironment)

    override val name: String = "uplc-blaster"

    override def prepare(goal: Goal): Either[CompatibilityReport, Prepared] =
        UplcBlaster
            .lower(goal.statement.prop, goal.functions)
            .flatMap(lowered =>
                budget match
                    case Budget.Within(_) if lowered.hasExistential =>
                        Left(
                          "a check within a budget reads a run the budget cuts in the " +
                              "statement's favour, and such a run is no witness: it is of a " +
                              "statement whose quantifiers all stand for every value, a forAll in " +
                              "a positive position or an exists in a negative one"
                        )
                    case Budget.Within(steps) => Right(lowered.copy(within = Some(steps)))
                    case _                    => Right(lowered)
            )
            .left
            .map(reason =>
                CompatibilityReport(
                  List(CompatibilityIssue.UnsupportedFeature(List(goal.statement.name), reason))
                )
            )

    /** A search starts at the budget that was kept, also with another revision of Blaster or of the
      * library: where the statement is still decided there, that is one check.
      */
    override def run(prepared: Prepared, earlier: Option[Kept]): ExecutionResult =
        checkOrSearch(
          prepared,
          earlier.flatMap(_.notes.get(UplcBlaster.budgetNote)).flatMap(_.toIntOption)
        )

    /** Runs the check, or the search for its budget, which starts at `first` where that is given.
      */
    private def checkOrSearch(prepared: Prepared, first: Option[Int]): ExecutionResult = {
        val kept = keptChecks
        if lastChecksOnly then kept.foreach(Directories.remove)
        budget match
            case Budget.LeanSteps(steps) =>
                checkAt(prepared, steps, kept)
            case Budget.Within(steps) =>
                checkAt(prepared, steps, kept)
            // A search makes several checks, and asks for the server before each.
            case Budget.Auto =>
                UplcBlaster.search(
                  prepared,
                  maxHeartbeats,
                  servers,
                  timeout,
                  attemptTimeout,
                  kept,
                  first
                )
    }

    /** One check of `prepared` at `steps`. */
    private def checkAt(prepared: Prepared, steps: Int, kept: Option[Path]): ExecutionResult =
        servers.server() match
            case Left(reason) => VerificationResult.Failed(reason)
            case Right(server) =>
                val shorter = (timeout ++ attemptTimeout).minOption
                UplcBlaster.check(prepared, steps, maxHeartbeats, server, shorter, kept)

    /** What a result rests on: the statement as Lean is given it, the programs, and Lean's side,
      * which is read from the workspace of the tactic's servers. A tactic whose provider does not
      * say which workspace that is, or names a directory that is none, keeps no results.
      */
    override def fingerprint(prepared: Prepared): Option[Fingerprint] =
        servers.workspace
            .filter(workspace => Files.isRegularFile(workspace.resolve("lean-toolchain")))
            .map(UplcBlaster.fingerprint(prepared, _))

    override def keep(prepared: Prepared, result: ExecutionResult): Option[Kept] =
        UplcBlaster.kept(prepared, result)

    override def restore(prepared: Prepared, kept: Kept): Either[String, ExecutionResult] =
        UplcBlaster.restored(prepared, kept)
}

object UplcBlaster {

    /** What Lean reported about a statement, and which compiled programs it was about.
      *
      * @param programHashes
      *   the SHA-256 of each leaf's program CBOR, in the order of [[Lowered.leaves]]
      * @param budget
      *   the steps of Lean's machine the check ran with: those the tactic was given, or those it
      *   found ([[Budget.Auto]])
      * @param counterexample
      *   for a refutation, the value of each quantified variable, by name, as replayed on the
      *   Scalus CEK; empty otherwise
      * @param read
      *   how the counterexample was read from `output`, where the solver's values had characters
      *   that are none of their type (`UplcBlaster.renaming`); empty otherwise
      * @param kind
      *   [[ProofKind.Blaster]] for a statement with quantified variables, and
      *   [[ProofKind.LeanNative]] for a closed statement, which Lean decides by evaluation
      * @param kept
      *   whether the result is one that was kept, and this run did not ask Lean
      *   ([[scalus.verify.KeptResults]]). A proof is then the word of the run that kept it, and
      *   `output` is empty; a refutation was replayed on the Scalus CEK as any other.
      */
    final case class Artifact(
        programHashes: List[String],
        budget: Int,
        output: String,
        counterexample: List[(String, Constant)],
        read: String,
        kind: ProofKind,
        kept: Boolean
    ) extends ProofArtifact

    /** A lowered statement over its leaves, retaining its quantifiers and connectives.
      *
      * The leaves of a statement are the nodes of its [[Prop]] tree that are not connectives:
      * tests, calls, `denotes` and `equal`. [[lower]] compiles each leaf into a closed UPLC program
      * in [[Lowered.leaves]], and a `LeafFormula` keeps the connectives around them, with each leaf
      * replaced by its index there. `<=>` is already split into two implications, and a partial
      * call `whenReturns(f, a)(k)` is already `denotes(f(a)) ==> call(f, a)(k)`, two leaves. Both
      * the Lean proposition (`renderFormula`) and the replay of a counterexample (`holds`) are read
      * from it.
      */
    enum LeafFormula {

        /** The predicate returns `true`. */
        case Test(leaf: Int)

        /** The predicate returns without an error. */
        case Denotes(leaf: Int)
        case And(left: LeafFormula, right: LeafFormula)
        case Or(left: LeafFormula, right: LeafFormula)
        case Not(inner: LeafFormula)
        case Implies(premise: LeafFormula, conclusion: LeafFormula)
        case Forall(binders: List[Int], body: LeafFormula)
        case Exists(binders: List[Int], body: LeafFormula)
    }

    /** A statement lowered for Lean: its primitive binders, its body, and one closed UPLC program
      * per leaf of the body. Every program takes the values of its in-scope binders in order.
      */
    final case class Lowered(
        binders: List[PropExpr.Ident[?]],
        body: LeafFormula,
        leaves: Vector[Program],
        private[uplcblaster] val leafBinders: Vector[List[Int]],
        /** For each binder, the Lean predicate its values satisfy, where Lean's variable has more
          * values than its type: a function to `Bool`, applied to the variable in the quantifier.
          */
        private[uplcblaster] val predicates: Vector[Option[String]] = Vector.empty,
        /** The budget it is checked within ([[Budget.Within]]), where it is: a run the budget cuts
          * is read in the statement's favour, and a proof is one within the budget.
          */
        private[uplcblaster] val within: Option[Int] = None
    ) {

        /** The SHA-256 of each leaf's program CBOR, in the order of [[leaves]]. */
        def programHashes: List[String] = leaves.toList.map { program =>
            Hex.bytesToHex(Utils.sha2_256(program.cborEncoded)).toLowerCase
        }

        /** Whether a quantifier of the statement asks for a witness: an `exists` in a positive
          * position, or a `forAll` in a negative one, under `!` or in the premise of `==>`. Either
          * side of `<=>` is in both.
          *
          * Without one, every variable ranges over all its values, and the statement is false where
          * its body is false of one assignment. With one, an assignment shows nothing: the witness
          * may be another value.
          */
        def hasExistential: Boolean = quantifiers(body).exists((_, universal) => !universal)
    }

    /** The Scalus CEK budget for replaying a counterexample: a hundred times the mainnet
      * per-transaction limit. It only guards against a predicate that does not terminate.
      */
    private val replayBudget = ExUnits(memory = 1_400_000_000L, steps = 1_000_000_000_000L)

    private lazy val replayVm: PlutusVM = PlutusVM.makePlutusV3VM()

    /** Compile configuration supported by the current PlutusCoreBlaster model. */
    val options: Options = Options.releaseUntagged.copy(valueBuiltins = false)

    /** A tactic that runs its checks in the Lean server `servers` gives it: one for a workspace
      * that has this module's Lean library. Whoever made the provider ends its servers. A check is
      * given up after [[defaultTimeout]].
      */
    def apply(budget: Budget, servers: LeanServerProvider): UplcBlaster = {
        new UplcBlaster(
          budget,
          servers,
          Some(defaultTimeout),
          None,
          defaultMaxHeartbeats,
          None,
          false
        )
    }

    def apply(budget: Budget, servers: LeanServerProvider, timeout: FiniteDuration): UplcBlaster = {
        new UplcBlaster(budget, servers, Some(timeout), None, defaultMaxHeartbeats, None, false)
    }

    /** The time a check gets unless told otherwise: ten minutes by the clock, from the moment the
      * tactic runs it. A check that takes longer is given up, and the tactic is inconclusive.
      *
      * Some statements Lean does not finish, such as one whose program loops over a list of unknown
      * length. Lean's own limit is one of work ([[defaultMaxHeartbeats]]), and it does not end
      * every such check: it is looked at only at some points of Lean's code, and the solver's time
      * is not counted. This limit ends them all. [[UplcBlaster.withoutTimeout]] lifts it.
      */
    val defaultTimeout: FiniteDuration = 10.minutes

    /** Lean's own default for `maxHeartbeats`, its limit on the work of one command. A heartbeat is
      * a unit of that work, a thousand small allocations, counted the same on every machine.
      */
    val leanMaxHeartbeats: Int = 200000

    /** The limit a tactic sets unless told otherwise ([[UplcBlaster.withMaxHeartbeats]]): twice
      * Lean's own.
      *
      * Lean gives a command of a check up when it has done this much work, and the check is then
      * inconclusive. A command is the symbolic run of one leaf, or Blaster's translation of the
      * statement. The time the solver takes is none of Lean's work, and is not counted.
      */
    val defaultMaxHeartbeats: Int = 2 * leanMaxHeartbeats

    /** Where the checks that run are also written, for looking into one, where code says nothing
      * ([[UplcBlaster.withChecksKeptIn]]): the directory the environment variable
      * `SCALUS_LEAN_KEEP_CHECKS` names, where it is set.
      */
    private[uplcblaster] def keptChecksFromEnvironment: Option[Path] =
        sys.env.get("SCALUS_LEAN_KEEP_CHECKS").filter(_.nonEmpty).map(Path.of(_))

    /** Lowers a statement in the supported fragment, or explains why it is outside it.
      *
      * The fragment has universal and existential quantifiers over `BigInt`, `Boolean`,
      * `ByteString`, `String`, `Data`, lists and maps, and case classes and enums of them. A
      * variable of a case class becomes one variable per field in [[Lowered.binders]], and one of
      * an enum a constructor's number and the fields of every constructor; each test builds it from
      * them. A list or a map is one variable, in the form it is passed, and only where it stands
      * for every value ([[Variable.isLarger]]). `existsLet` applies its body to the supplied
      * witness and introduces no Lean quantifier. Leaves are Boolean tests, calls whose
      * continuation is a test or another total call, `denotes`, and `equal` over `BigInt`,
      * `Boolean`, `ByteString`, `String` and `Data`. A call's arguments and result, and the operand
      * of `denotes`, can have any type. Its connectives are `&&`, `||`, `!`, `==>` and `<=>`. `<=>`
      * becomes two implications, because its operands occur in both polarities. A partial call,
      * `whenReturns(f, a)(r => p)`, becomes `denotes(f(a)) ==> call(f, a)(r => p)`: its two leaves
      * run the function's program each.
      *
      * A call of a function in `functions` is linked to that function's compiled program, so the
      * function's bytes appear unchanged in the predicate. A function of several parameters is
      * called with a tuple written out as `(a, b, ...)`, and its program is applied to each value
      * in turn. Other `@Compile` definitions a test uses are compiled together with the test.
      */
    def lower(prop: Prop, functions: FunctionTable): Either[String, Lowered] =
        checkSignatures(prop, functions).flatMap { _ =>
            val binders = ArrayBuffer.empty[PropExpr.Ident[?]]
            // The predicate of each binder, in the order of `binders`.
            val predicates = ArrayBuffer.empty[Option[String]]
            // The binders whose predicate is not exact, which may stand only for every value.
            val inexact = scala.collection.mutable.Set.empty[Int]
            val leaves = ArrayBuffer.empty[Program]
            val leafBinders = ArrayBuffer.empty[List[Int]]
            // Leaves that are one program over the same variables share it, as the calls of two
            // clauses of a contract do. The variable indices matter: equal program bytes under two
            // different quantifiers are two differently scoped leaves.
            val indices = scala.collection.mutable.Map.empty[(String, List[Int]), Int]
            // The guard of each binder with limits ([[LimitGuards]]), by its position.
            val guards = scala.collection.mutable.Map.empty[Int, Term]

            // A test in the scope of a binder with limits checks its guard first, and fails where
            // the binder's value does not meet them: there the guard, the premise of what the
            // binder's quantifier says, is false, and what the test does there says nothing. Its
            // `dropList`s of a constant are read as the steps of a list's tail, as the guard reads
            // a list, so that what the guard established holds for the test's reads ([[DropLists]]).
            // Another test keeps them: the steps take more of Lean's machine than `dropList` does.
            def leaf(term: Term, scope: List[Int]): Int = {
                val limited =
                    scope.zipWithIndex.flatMap((binder, at) => guards.get(binder).map(_ -> at))
                if limited.isEmpty then unguardedLeaf(term, scope)
                else
                    val names = scope.indices.map(at => s"scalus_limited_$at")
                    val call = names.foldLeft(term)((f, name) =>
                        Term.Apply(f, Term.Var(NamedDeBruijn(name)))
                    )
                    val meets = limited
                        .map((guard, at) => Term.Apply(guard, Term.Var(NamedDeBruijn(names(at)))))
                        .reduce(LimitGuards.and)
                    val guarded = LimitGuards.ifThenElse(meets, call, Term.Error())
                    unguardedLeaf(
                      DropLists.unrolled(names.foldRight(guarded)(Term.LamAbs(_, _))),
                      scope
                    )
            }

            def unguardedLeaf(term: Term, scope: List[Int]): Int = {
                val program = Program.plutusV3(term)
                indices.getOrElseUpdate(
                  Hex.bytesToHex(program.cborEncoded) -> scope, {
                      leaves += program
                      leafBinders += scope
                      leaves.size - 1
                  }
                )
            }

            /** The primitive Lean binders for one statement binder, and a binding that rebuilds a
              * case class from its fields in every leaf in its scope.
              *
              * A list's or a map's items are any `Data` to Lean: the quantifier restricts them to
              * the type's with a predicate derived from the type. Where the predicate is not exact,
              * the variable keeps more values than its type, and is checked to stand for every
              * value once the statement is lowered.
              */
            def quantified(ident: PropExpr.Ident[?]): Either[String, (List[Int], List[Binding])] =
                DataEncoding.encoded(ident.tp) match
                    case Some((valueType, decoder)) => quantifiedData(ident, valueType, decoder)
                    case None                       => quantifiedValues(ident)

            /** A [[Props.forAllData]] variable: one Lean `Data`, restricted to the encodings of its
              * type's values, and the binding that decodes it in every leaf in its scope. The
              * decoder's data declarations go around the leaf with the leaf's own ([[withBuilt]]).
              */
            def quantifiedData(
                ident: PropExpr.Ident[?],
                valueType: SIRType,
                decoder: SIR
            ): Either[String, (List[Int], List[Binding])] =
                val decoded = Props.openVariables(decoder, List(ident.name))
                val limits = DataEncoding.limits(ident.tp)
                val unfit = Option.when(limits.all.nonEmpty)(limits.unfit(valueType)).flatten
                lazy val guard = LimitGuards.guard(DataLayout.of(valueType), limits)
                DataEncoding.of(valueType) match
                    case _ if unfit.isDefined =>
                        Left(
                          s"the limits of a binder of ${valueType.show} do not fit it: ${unfit.get}"
                        )
                    case _ if limits.all.nonEmpty && guard.isLeft =>
                        Left(
                          s"the limits of a binder of ${valueType.show} are not checked: " +
                              guard.left.toOption.mkString
                        )
                    case DataEncoding.Unknown(reason) =>
                        Left(s"a forAllData binder of ${valueType.show} is not supported: $reason")
                    // `d.to[A]` compiles to the compiler's own reading, whatever FromData is
                    // given. A decoder that is not would read other Data than the premise admits.
                    case _ if !decodesByRepresentation(decoded, ident.name) =>
                        Left(
                          s"a forAllData binder of ${valueType.show} is read otherwise than by " +
                              "the compiler's Data representation of the type, which its premise " +
                              "is derived from: a Data the premise admits could fail to decode, " +
                              "and count as a value of the type"
                        )
                    case _ =>
                        val restriction = variableRestriction(Variable.EncodedData, ident.tp)
                        val position = binders.size
                        binders += new PropExpr.Ident[Any](ident.name, 0L, ident.tp)
                        predicates += restriction.premise
                        if !restriction.exact then inexact += position
                        // A guard that checks less than the limits say leaves the variable more
                        // values than they allow, as a predicate that is not exact does.
                        if limits.all.nonEmpty then
                            guard.foreach { checked =>
                                guards(position) = checked.program
                                if !checked.exact then inexact += position
                            }
                        Right(List(position) -> List(Binding(ident.name, valueType, decoded)))

            def quantifiedValues(
                ident: PropExpr.Ident[?]
            ): Either[String, (List[Int], List[Binding])] =
                expand(ident.name, ident.tp).map { (variables, built) =>
                    val restrictions = variables.map(v =>
                        variableOf(v.tp)
                            .filter(_.isLarger)
                            .map(kind => variableRestriction(kind, v.tp))
                    )
                    val from = binders.size
                    binders ++= variables
                    predicates ++= restrictions.map(_.flatMap(_.premise))
                    inexact ++= variables.indices
                        .filter(i => restrictions(i).exists(!_.exact))
                        .map(from + _)
                    val positions = (from until binders.size).toList
                    positions -> built.map(Binding(ident.name, ident.tp, _)).toList
                }

            // A test's program over the values in its quantifier scope, with each case-class
            // variable it uses built from those values.
            def compile(sir: SIR, scope: List[Int], built: List[Binding]): Term =
                compileSirFunction(
                  scope.map(index => binders(index)),
                  withBuilt(built, sir),
                  functions
                )

            def loop(
                current: Prop,
                scope: List[Int],
                built: List[Binding]
            ): Either[String, LeafFormula] = current match {
                case Prop.Call(fn, arg, result, false, body) =>
                    for
                        returns <- returnsSir(fn, arg, DataEncoding.valueType(result.tp), functions)
                        formula <- loop(
                          Prop.Implies(
                            Prop.Denotes(PropExpr.SIRExpr(returns)),
                            Prop.Call(fn, arg, result, true, body)
                          ),
                          scope,
                          built
                        )
                    yield formula
                case _: Prop.Bool | _: Prop.Call[?, ?] =>
                    predicateSir(current, functions)
                        .map(test => LeafFormula.Test(leaf(compile(test, scope, built), scope)))
                case Prop.Equal(left, right) =>
                    equality(expressionSir(left), expressionSir(right))
                        .map(test => LeafFormula.Test(leaf(compile(test, scope, built), scope)))
                case Prop.Denotes(expr) =>
                    Right(
                      LeafFormula.Denotes(
                        leaf(compile(expressionSir(expr), scope, built), scope)
                      )
                    )
                case Prop.And(left, right) =>
                    for
                        l <- loop(left, scope, built)
                        r <- loop(right, scope, built)
                    yield LeafFormula.And(l, r)
                case Prop.Or(left, right) =>
                    for
                        l <- loop(left, scope, built)
                        r <- loop(right, scope, built)
                    yield LeafFormula.Or(l, r)
                case Prop.Not(inner) =>
                    loop(inner, scope, built).map(LeafFormula.Not(_))
                case Prop.Implies(premise, conclusion) =>
                    for
                        p <- loop(premise, scope, built)
                        c <- loop(conclusion, scope, built)
                    yield LeafFormula.Implies(p, c)
                case Prop.Iff(left, right) =>
                    for
                        l <- loop(left, scope, built)
                        r <- loop(right, scope, built)
                    yield LeafFormula.And(LeafFormula.Implies(l, r), LeafFormula.Implies(r, l))
                // A binder with limits stands for the values that meet them: its guard, a test of
                // its own, is the premise of the rest.
                case Prop.Forall(ident, body) =>
                    quantified(ident).flatMap { (introduced, bindings) =>
                        loop(body, scope ++ introduced, built ++ bindings).map { inner =>
                            val premises = introduced.flatMap(binder =>
                                guards
                                    .get(binder)
                                    .map(guard =>
                                        LeafFormula.Test(unguardedLeaf(guard, List(binder)))
                                    )
                            )
                            LeafFormula.Forall(
                              introduced,
                              premises.foldRight(inner)(LeafFormula.Implies(_, _))
                            )
                        }
                    }
                // The limits of a binder are the premise of a universal statement, and a
                // statement that asks for a witness has none.
                case Prop.Exists(ident, None, _) if DataEncoding.limits(ident.tp).all.nonEmpty =>
                    Left(
                      s"an exists over ${ident.name}, a binder with limits, is not supported: the " +
                          "limits of a binder are the premise of a forAll"
                    )
                case Prop.Exists(ident, None, body) =>
                    quantified(ident).flatMap { (introduced, bindings) =>
                        loop(body, scope ++ introduced, built ++ bindings)
                            .map(LeafFormula.Exists(introduced, _))
                    }
                case Prop.Exists(ident, Some(witness), body) =>
                    loop(instantiate(body, ident, witness), scope, built)
            }

            // A variable with more values than its type is sound only where it stands for every
            // value: a statement that holds of all of them holds of the type's, but a value that
            // shows a statement of some value can be none of the type's.
            def standsForEvery(formula: LeafFormula): Either[String, LeafFormula] = {
                val claimed = quantifiers(formula)
                    .collect { case (introduced, false) =>
                        introduced.filter(inexact.contains)
                    }
                    .flatten
                    .distinct
                if claimed.isEmpty then Right(formula)
                else
                    Left(
                      s"${claimed.map(binders(_).name).mkString(", ")}: a list, a map or a " +
                          "forAllData variable whose type cannot be told from Data exactly, as a " +
                          "type that contains itself, or a forAllDataLimited variable whose guard " +
                          "checks less than its limits say, as of the elements of a list that has " +
                          "no most, has more values as a variable than its type or its limits " +
                          "allow. A statement that holds of all of them holds of the type's, but a " +
                          "value that shows a statement of some value can be none of the type's. " +
                          "So such a variable is one only where it stands for every value: in a " +
                          "forAll in a positive position, or an exists in a negative one, and not " +
                          "under <=>"
                    )
            }

            loop(prop, Nil, Nil).flatMap(standsForEvery).flatMap { formula =>
                val utf8 =
                    leaves.flatMap(_.term.collectBuiltins).filter(utf8Builtins).distinct.sorted
                if binders.nonEmpty && utf8.nonEmpty then
                    Left(
                      s"the programs apply ${utf8.map(builtinName).mkString(" and ")} to values " +
                          "of the statement's variables. These builtins see which characters a " +
                          "string has, and the solver's strings have other characters than " +
                          "Plutus's (input-output-hk/Lean-blaster#389), so a proof would not hold. " +
                          "A closed statement, which Lean evaluates, may apply them"
                    )
                else
                    Right(
                      Lowered(
                        binders.toList,
                        formula,
                        leaves.toVector,
                        leafBinders.toVector,
                        predicates.toVector
                      )
                    )
            }
        }

    /** The builtins that see the characters of a string: Blaster cannot translate them today, and
      * where it can, the solver's characters would decide a statement about them.
      */
    private val utf8Builtins: Set[DefaultFun] = Set(DefaultFun.EncodeUtf8, DefaultFun.DecodeUtf8)

    /** A builtin's name as UPLC writes it, as `encodeUtf8`. */
    private def builtinName(builtin: DefaultFun): String = {
        val name = builtin.toString
        s"${name.head.toLower}${name.tail}"
    }

    /** A type a call can pass to a function that has no SIR, and take back from it: one whose
      * values have one form whatever their static type. It says nothing of quantified variables:
      * [[leanVariable]] does, and differs for one constructor of `Data`.
      */
    private def quantifiable(tp: SIRType): Boolean =
        tp == SIRType.Integer || tp == SIRType.Boolean || tp == SIRType.ByteString ||
            tp == SIRType.String || isData(tp)

    /** `Data`, or one of its constructors, such as the type of `Data.I(x)`. */
    private def isData(tp: SIRType): Boolean = tp match
        case SIRType.SumCaseClass(decl, _)         => decl.name == SIRType.Data.name
        case SIRType.CaseClass(_, _, Some(parent)) => isData(parent)
        case _                                     => false

    /** A type a quantified variable can have as it is: Lean has a variable of it, and builds its
      * values. `Data` is one, and a single constructor of `Data` is not. Lean's variable for
      * `Data.I` would be any `Data`, so a statement would be refuted by a value that is no `I`, and
      * an `exists` proved by one.
      */
    private def leanVariable(tp: SIRType): Boolean = variableOf(tp).isDefined

    /** The kind of Lean variable that stands for a variable of type `tp` as it is, if one does.
      *
      * Besides the constants, a list or a map is one: the compiler passes it as a builtin list or
      * as one `Data.Map`, not through its constructors, so Lean has a variable of that form, and
      * the value reaches the programs as it would at run time. Which form is the lowering's default
      * representation of the type: a builtin list of `Data` for a `List` of a type that is `Data`,
      * a builtin list of pairs of `Data` for a list of pairs, and a `Data.Map` for `SortedMap`,
      * `AssocMap` and `Value`. Its elements are any `Data`, as the representation's are, and its
      * length is any length.
      */
    private def variableOf(tp: SIRType): Option[Variable] = unwrap(tp) match
        case _ if DataEncoding.encoded(tp).isDefined => Some(Variable.EncodedData)
        case SIRType.Integer                         => Some(Variable.Integer)
        case SIRType.Boolean                         => Some(Variable.Boolean)
        case SIRType.ByteString                      => Some(Variable.ByteString)
        case SIRType.String                          => Some(Variable.String)
        case whole if isWholeData(whole)             => Some(Variable.WholeData)
        // One constructor of a type, as `Data.I` or `List.Cons`: Lean's variable for it would be
        // one of the whole type, which has other values.
        case SIRType.CaseClass(_, _, Some(_)) => None
        case other if collectionTypes.contains(declarationName(other)) =>
            collections.getOrElseUpdate(typeKey(other), collection(other))
        case _ => None

    /** The types that are passed as a list or as a map: the only ones whose representation decides
      * the variable, and so the only ones whose representation is looked up.
      */
    private val collectionTypes = Set(
      "scalus.cardano.onchain.plutus.prelude.List",
      SIRType.PairList.DataDeclName,
      "scalus.cardano.onchain.plutus.prelude.SortedMap",
      "scalus.cardano.onchain.plutus.prelude.AssocMap",
      "scalus.cardano.onchain.plutus.v1.Value"
    )

    /** [[collection]] of each type it was asked about, by its [[SirTypes.typeKey]]. */
    private val collections = scala.collection.concurrent.TrieMap.empty[String, Option[Variable]]

    /** The context in which the tactic's programs are lowered, which chooses the representations of
      * their parameters.
      */
    private[verify] def representations: LoweringContext =
        UplcSignature.loweringContext(options, options.targetLanguage)

    /** [[variableOf]] for a type that is passed as a list or as a map. */
    private def collection(tp: SIRType): Option[Variable] = {
        given LoweringContext = representations
        SirTypeUplcGenerator.defaultRepresentation(tp) match
            case SumCaseClassRepresentation.SumBuiltinList(element) if element.isPackedData =>
                Some(Variable.DataList)
            case SumCaseClassRepresentation.SumPairBuiltinList(key, value)
                if key.isPackedData && value.isPackedData =>
                Some(Variable.PairList)
            case ProductCaseClassRepresentation.PackedDataMap => Some(Variable.DataMap)
            case ProductCaseClassRepresentation.OneElementWrapper(
                  ProductCaseClassRepresentation.PackedDataMap
                ) =>
                Some(Variable.DataMap)
            case _ => None
    }

    /** The binders of each quantifier of `formula`, and whether the quantifier stands for every
      * value: a `forAll` in a positive position, or an `exists` in a negative one. A side of `<=>`
      * occurs in both positions, so each of its quantifiers is listed twice.
      */
    private def quantifiers(formula: LeafFormula): List[(List[Int], Boolean)] = {
        def loop(formula: LeafFormula, polarity: Polarity): List[(List[Int], Boolean)] =
            formula match
                case LeafFormula.Forall(binders, body) =>
                    (binders -> (polarity == Polarity.Positive)) :: loop(body, polarity)
                case LeafFormula.Exists(binders, body) =>
                    (binders -> (polarity == Polarity.Negative)) :: loop(body, polarity)
                case LeafFormula.And(left, right) => loop(left, polarity) ++ loop(right, polarity)
                case LeafFormula.Or(left, right)  => loop(left, polarity) ++ loop(right, polarity)
                case LeafFormula.Not(inner)       => loop(inner, polarity.turned)
                case LeafFormula.Implies(premise, conclusion) =>
                    loop(premise, polarity.turned) ++ loop(conclusion, polarity)
                case _: LeafFormula.Test | _: LeafFormula.Denotes => Nil
        loop(formula, Polarity.Positive)
    }

    /** Where a part of a statement is: in a positive position, which the statement claims, or in a
      * negative one, under a negation or in a premise.
      */
    private enum Polarity {
        case Positive, Negative

        /** The position under a negation or in a premise of a part in this one. */
        def turned: Polarity = this match
            case Positive => Negative
            case Negative => Positive
    }

    /** A kind of Lean variable: its Lean type, and the term that passes its value to a program, in
      * the form the compiled code takes.
      */
    private enum Variable(val leanType: String) {

        /** Whether the variable has values its type does not have: the items of a list or a map are
          * any `Data`. A statement about all its values holds of the type's, and one that claims
          * some value can be shown by a value the type does not have.
          */
        def isLarger: Boolean = this match
            case DataList | PairList | DataMap | EncodedData => true
            case _                                           => false
        case Integer extends Variable("Integer")
        case Boolean extends Variable("Bool")
        case ByteString extends Variable("ByteString")
        case String extends Variable("String")
        case WholeData extends Variable("Data")

        /** A builtin list of `Data`. */
        case DataList extends Variable("List Data")

        /** A builtin list of pairs of `Data`. */
        case PairList extends Variable("List (Data × Data)")

        /** The entries of one `Data.Map`. */
        case DataMap extends Variable("List (Data × Data)")

        /** The `Data` encoding of a value of its type ([[Props.forAllData]]). */
        case EncodedData extends Variable("Data")

        /** The term that passes the value of the Lean variable `name`. The text is part of every
          * kept result's fingerprint ([[UplcBlaster.fingerprint]]): writing the same term another
          * way makes every kept result stale.
          */
        def argument(name: String): String = this match
            case Integer                 => s"Term.Const (Const.Integer $name)"
            case Boolean                 => s"Term.Const (Const.Bool $name)"
            case ByteString              => s"Term.Const (Const.ByteString $name)"
            case String                  => s"Term.Const (Const.String $name)"
            case WholeData | EncodedData => s"Term.Const (Const.Data $name)"
            case DataList                => s"Term.Const (Const.ConstDataList $name)"
            case PairList                => s"Term.Const (Const.ConstPairDataList $name)"
            case DataMap                 => s"Term.Const (Const.Data (Data.Map $name))"
    }

    /** The variables Lean quantifies over for a statement's variable `name` of type `tp`, and the
      * variable's value built from them, when it is not one of them itself.
      *
      * A variable of a case class ranges over the values of its constructor (prop-semantics.md §1),
      * so it stands for one variable per field, named `name.field`, and its value is the
      * constructor applied to them. A field that is a case class or an enum is expanded in turn: a
      * `Config` with a `PubKeyHash` ends in the hash's `ByteString`.
      *
      * A variable of an enum, or of another type of several constructors, ranges over the values of
      * all of them. It stands for an integer `name.case`, which chooses the constructor, and for
      * the fields of every constructor, named `name.Constructor.field`. Its value is the
      * constructor the integer chooses, applied to that constructor's fields: the first for 0, the
      * second for 1, and so on, and the last for every other integer, so that each integer is some
      * value. A type that contains itself, such as `List`, has values of every size, which no set
      * of variables stands for, and is not supported. `enclosing` names the types being expanded.
      */
    private def expand(
        name: String,
        tp: SIRType,
        enclosing: Set[String] = Set.empty,
        count: Expansion = new Expansion
    ): Either[String, (List[PropExpr.Ident[?]], Option[AnnotatedSIR])] = {
        count.parts += 1
        if count.parts > maxParts then
            Left(
              s"a ${tp.show} binder is not supported: it has more than $maxParts parts, as a " +
                  "type that contains itself under other arguments each time"
            )
        else expandPart(name, tp, enclosing, count)
    }

    /** [[expand]] of one part, counted. */
    private def expandPart(
        name: String,
        tp: SIRType,
        enclosing: Set[String],
        count: Expansion
    ): Either[String, (List[PropExpr.Ident[?]], Option[AnnotatedSIR])] = unwrap(tp) match
        case plain if leanVariable(plain) =>
            Right(List(new PropExpr.Ident[Any](name, 0L, plain)) -> None)
        case product @ SIRType.CaseClass(constructor, typeArguments, None) =>
            // The declaration of a class that is its own only constructor, as the compiler makes
            // it.
            val declaration = DataDecl(
              constructor.name,
              List(constructor),
              constructor.typeParams,
              constructor.annotations
            )
            nested(product, enclosing).flatMap(inner =>
                constructed(name, declaration, constructor, typeArguments, product, inner, count)
            )
        case sum @ SIRType.SumCaseClass(declaration, _) if declaration.constructors.isEmpty =>
            Left(s"a ${sum.show} binder is not supported: the type has no constructors")
        case sum @ SIRType.SumCaseClass(declaration, typeArguments) =>
            nested(sum, enclosing).flatMap(inner =>
                expandSum(name, sum, declaration, typeArguments, inner, count)
            )
        case one @ SIRType.CaseClass(_, _, Some(parent)) =>
            Left(
              s"a ${one.show} binder is not supported: it is one constructor of ${parent.show}, " +
                  s"and Lean's variable for it would have the other values of ${parent.show}. " +
                  s"Quantify over ${parent.show}"
            )
        case other =>
            Left(
              s"a ${other.show} binder is not supported, only BigInt, Boolean, ByteString, " +
                  "String, Data, lists and maps, and case classes and enums of them"
            )

    /** Whether `decoded`, the decoder of a [[Props.forAllData]] variable `name` applied to it, is
      * the compiler's own: `UniversalDataConversion.fromData`, which reads the `Data` as the type's
      * `Data` representation, the one its premise is derived from. Definitions the compiler put
      * around it do not change what it reads.
      */
    private def decodesByRepresentation(decoded: SIR, name: String): Boolean = {
        val fromData = "scalus.uplc.builtin.internal.UniversalDataConversion$.fromData"
        Wrappers.of(decoded)._2 match
            case SIR.Apply(SIR.ExternalVar(_, function, _, _), SIR.Var(argument, _, _), _, _) =>
                function == fromData && argument == name
            case SIR.Apply(SIR.Var(function, _, _), SIR.Var(argument, _, _), _, _) =>
                function == fromData && argument == name
            case _ => false
    }

    /** The parts of one statement variable expanded so far ([[expand]]): its fields, and theirs,
      * whether they are Lean variables or are expanded in turn.
      */
    private final class Expansion(var parts: Int = 0)

    /** How many parts one statement variable may have: many more than a type of the ledger has, and
      * an end to a type that contains itself under other arguments each time, which [[nested]] does
      * not tell, and whose fields double at each level.
      */
    private val maxParts = 10_000

    /** The types being expanded, with `tp`, which is expanded inside them, or why it cannot be. A
      * type is told by its arguments as well: an `Option[Address]` holds an `Option` of another
      * type, and does not contain itself.
      */
    private def nested(tp: SIRType, enclosing: Set[String]): Either[String, Set[String]] = {
        val shown = tp.show
        if enclosing.contains(shown) then
            Left(
              s"a $shown binder is not supported: the type contains itself, so its values have " +
                  "no bound in size, and no set of variables stands for them"
            )
        else if enclosing.size >= maxNesting then
            Left(s"a $shown binder is not supported: its types nest more than $maxNesting deep")
        else Right(enclosing + shown)
    }

    /** The variables of a constructor's fields, named `name.field`, and the constructor applied to
      * them. `tp` is the type of the value, and `typeArguments` are the constructor's own.
      */
    private def constructed(
        name: String,
        declaration: DataDecl,
        constructor: ConstrDecl,
        typeArguments: List[SIRType],
        tp: SIRType,
        enclosing: Set[String],
        count: Expansion
    ): Either[String, (List[PropExpr.Ident[?]], Option[AnnotatedSIR])] = {
        val fields = constructor.params.zip(fieldTypes(constructor, typeArguments)).map {
            (field, fieldType) =>
                val fieldName = s"$name.${field.name}"
                expand(fieldName, fieldType, enclosing, count).map { (binders, built) =>
                    val value: AnnotatedSIR =
                        built.getOrElse(SIR.Var(fieldName, binders.head.tp, AnnotationsDecl.empty))
                    binders -> value
                }
        }
        fields.collectFirst { case Left(reason) => reason } match
            case Some(reason) => Left(reason)
            case None =>
                val (binders, values) = fields.collect { case Right(field) => field }.unzip
                Right(
                  binders.flatten -> Some(
                    SIR.Constr(constructor.name, declaration, values, tp, AnnotationsDecl.empty)
                  )
                )
    }

    /** [[expand]] of a variable of a type of several constructors. */
    private def expandSum(
        name: String,
        sum: SIRType,
        declaration: DataDecl,
        typeArguments: List[SIRType],
        enclosing: Set[String],
        count: Expansion
    ): Either[String, (List[PropExpr.Ident[?]], Option[AnnotatedSIR])] = {
        val annotations = AnnotationsDecl.empty
        val variants = declaration.constructors.map { constructor =>
            ownArguments(constructor, typeArguments) match
                case None =>
                    Left(
                      s"a ${sum.show} binder is not supported: the type arguments of " +
                          s"${constructor.name} are not those of the type"
                    )
                case Some(own) =>
                    val tp = SIRType.typeApply(declaration.constrType(constructor.name), own)
                    val label = constructor.name.split('.').last.stripPrefix("$")
                    constructed(
                      s"$name.$label",
                      declaration,
                      constructor,
                      own,
                      tp,
                      enclosing,
                      count
                    )
        }
        variants.collectFirst { case Left(reason) => reason } match
            case Some(reason) => Left(reason)
            case None =>
                val (binders, values) = variants.collect { case Right(variant) => variant }.unzip
                val built = values.flatten
                if built.size == 1 then Right(binders.flatten -> built.headOption)
                else
                    val tag = new PropExpr.Ident[Any](s"$name.case", 0L, SIRType.Integer)
                    val tagVariable = SIR.Var(tag.name, SIRType.Integer, annotations)
                    def chosen(index: Int): AnnotatedSIR = {
                        val partial = SIR.Apply(
                          SIRBuiltins.equalsInteger,
                          tagVariable,
                          SIRType.Fun(SIRType.Integer, SIRType.Boolean),
                          annotations
                        )
                        val number = SIR.Const(
                          Constant.Integer(BigInt(index)),
                          SIRType.Integer,
                          annotations
                        )
                        SIR.Apply(partial, number, SIRType.Boolean, annotations)
                    }
                    val value = built.init.zipWithIndex.foldRight(built.last) {
                        case ((constructor, index), otherwise) =>
                            SIR.IfThenElse(chosen(index), constructor, otherwise, sum, annotations)
                    }
                    Right((tag :: binders.flatten) -> Some(value))
    }

    /** `sir` with the variables in `built` that it uses bound to their values, inside its data
      * declarations.
      */
    private def withBuilt(built: List[Binding], sir: SIR): SIR = {
        val free = freeVariables(sir)
        val used = built.filter(binding => free.contains(binding.name))
        if used.isEmpty then sir
        else
            val (data, expression) = declarations(sir)
            // A value's own data declarations, as a decoder's, go around the leaf, with those of
            // the constructors it applies.
            val values = used.map(binding => binding -> declarations(binding.value))
            val constructed = values.flatMap { case (_, (own, value)) =>
                own ++ constructorDeclarations(value)
            }
            val bindings = values.map { case (binding, (_, value)) =>
                Binding(binding.name, binding.tp, value)
            }
            withDeclarations(
              constructed ++ data,
              SIR.Let(bindings, expression, SIR.LetFlags.None, AnnotationsDecl.empty)
            )
    }

    /** Applies the body of an `existsLet` to its explicit witness.
      *
      * Each expression in the proposition becomes `let ident = witness in expression`, a strict
      * binding. This preserves strict evaluation and failure: it is not a textual replacement that
      * could discard a failing witness when the body does not use it. The proposition structure
      * stays outside UPLC, so the binding is made independently in each leaf that the structure
      * evaluates.
      *
      * The binding is one more `let` around the expression, with the witness's module definitions
      * around it. So the expression keeps the shape the rest of the lowering reads: a call's
      * arguments are still a tuple written out, under its `let`s ([[tupleComponents]]), and the
      * definitions of registered functions are still where [[unlinkModuleDefinitions]] removes
      * them, in the witness as in the expression. The witness's own `let`s stay in the value that
      * is bound: only its module definitions, which have qualified names, are around the binding.
      */
    private def instantiate[A](
        prop: Prop,
        ident: PropExpr.Ident[A],
        witness: PropExpr[A]
    ): Prop = {
        def expression[T](expr: PropExpr[T]): PropExpr[T] = expr match
            case current: PropExpr.Ident[T]
                if current.id == ident.id && current.name == ident.name =>
                witness.asInstanceOf[PropExpr[T]]
            case PropExpr.SIRExpr(sir) =>
                val (witnessDeclarations, linkedWitness) = declarations(expressionSir(witness))
                val (definitions, witnessValue) = Wrappers.definitions(linkedWitness)
                val (bodyDeclarations, body) = declarations(sir)
                val bound = SIR.Let(
                  List(
                    Binding(
                      ident.name,
                      DataEncoding.valueType(ident.tp),
                      witnessValue
                    )
                  ),
                  body,
                  SIR.LetFlags.None,
                  AnnotationsDecl.empty
                )
                PropExpr.SIRExpr[T](
                  withDeclarations(witnessDeclarations ++ bodyDeclarations, definitions(bound))
                )
            case current => current

        def loop(current: Prop): Prop = current match
            case Prop.Bool(expr)         => Prop.Bool(expression(expr))
            case Prop.Denotes(expr)      => Prop.Denotes(expression(expr))
            case Prop.Equal(left, right) => Prop.Equal(expression(left), expression(right))
            case Prop.Call(fn, arg, result, total, body) =>
                Prop.Call(fn, expression(arg), result, total, loop(body))
            case Prop.Forall(bound, body) => Prop.Forall(bound, loop(body))
            case Prop.Exists(bound, supplied, body) =>
                Prop.Exists(bound, supplied.map(expression), loop(body))
            case Prop.And(left, right)     => Prop.And(loop(left), loop(right))
            case Prop.Or(left, right)      => Prop.Or(loop(left), loop(right))
            case Prop.Implies(left, right) => Prop.Implies(loop(left), loop(right))
            case Prop.Iff(left, right)     => Prop.Iff(loop(left), loop(right))
            case Prop.Not(inner)           => Prop.Not(loop(inner))

        loop(prop)
    }

    /** The data declarations of the constructors a built value applies. */
    private def constructorDeclarations(sir: SIR): List[DataDecl] = sir match
        case SIR.Constr(_, data, arguments, _, _) =>
            data :: arguments.flatMap(constructorDeclarations)
        case SIR.IfThenElse(_, ifTrue, ifFalse, _, _) =>
            constructorDeclarations(ifTrue) ++ constructorDeclarations(ifFalse)
        case _ => Nil

    /** A test, or a total call continuing with one, as one Boolean SIR expression over the
      * statement's variables. A call is `(result => continuation)(fn(arguments...))`, where `fn` is
      * an `ExternalVar` that [[compileSirFunction]] links to the function's own program.
      */
    private def predicateSir(prop: Prop, functions: FunctionTable): Either[String, SIR] =
        prop match
            case Prop.Bool(expr) =>
                val body = expressionSir(expr)
                require(body.tp == SIRType.Boolean, s"a test has type ${body.tp.show}")
                Right(body)
            case Prop.Call(fn, arg, result, true, body) =>
                for
                    arguments <- callArguments(expressionSir(arg), functions(fn).arity)
                    continuation <- predicateSir(body, functions)
                    call <- callSir(fn, arguments, result, continuation, functions)
                yield call
            case Prop.Call(_, _, _, false, _) =>
                Left("a whenReturns inside a call's continuation is not supported")
            case _ =>
                Left("a call's continuation must be a test or another total call")

    /** `(result => continuation)(fn(arguments...))`. The data declarations around the pieces move
      * outside the whole expression.
      */
    private def callSir(
        fn: FunctionRef[?, ?],
        arguments: List[SIR],
        result: PropExpr.Ident[?],
        continuation: SIR,
        functions: FunctionTable
    ): Either[String, SIR] = {
        val annotations = AnnotationsDecl.empty
        val (argumentDeclarations, values) = arguments.map(declarations).unzip
        val (continuationDeclarations, test) = declarations(continuation)
        val resultType = DataEncoding.valueType(result.tp)
        application(fn, values, resultType, functions).map { called =>
            val continued = SIR.Apply(
              SIR.LamAbs(SIR.Var(result.name, resultType, annotations), test, Nil, annotations),
              called,
              SIRType.Boolean,
              annotations
            )
            withDeclarations(argumentDeclarations.flatten ++ continuationDeclarations, continued)
        }
    }

    /** The call of a partial `whenReturns` on its own, `fn(arguments...)`, for the `denotes` that
      * guards its continuation. The data declarations around the arguments move outside it.
      */
    private def returnsSir(
        fn: FunctionRef[?, ?],
        arg: PropExpr[?],
        resultType: SIRType,
        functions: FunctionTable
    ): Either[String, SIR] = {
        for
            arguments <- callArguments(expressionSir(arg), functions(fn).arity)
            (argumentDeclarations, values) = arguments.map(declarations).unzip
            called <- application(fn, values, resultType, functions)
        yield withDeclarations(argumentDeclarations.flatten, called)
    }

    /** `expression` inside the data declarations collected from its pieces, each declared once. */
    private def withDeclarations(data: List[DataDecl], expression: SIR): SIR =
        data.distinctBy(_.name).foldRight(expression)((decl, body) => SIR.Decl(decl, body))

    /** `fn` applied to `values`, one at a time: a function's program is curried. `fn` is an
      * `ExternalVar` that [[compileSirFunction]] links to the function's own program.
      *
      * The variable has the function's declared type, from its SIR, so the lowering passes each
      * value as the function's program takes it: an enum's constructor, such as `Circle(r)`, is
      * passed as the enum. A function without SIR has no declared type, so its type comes from the
      * values and the result. Only `BigInt`, `Boolean`, `ByteString`, `String` and `Data` have one
      * form whatever their static type ([[quantifiable]]), so only those can be passed to it.
      */
    private def application(
        fn: FunctionRef[?, ?],
        values: List[AnnotatedSIR],
        resultType: SIRType,
        functions: FunctionTable
    ): Either[String, AnnotatedSIR] = {
        val annotations = AnnotationsDecl.empty
        val types = functions(fn).get(Representation.Sir) match
            case Some(sir) => curriedTypes(sir.tp, values.size, fn)
            case None =>
                val valueTypes = values.map(_.tp) :+ resultType
                valueTypes.find(tp => !quantifiable(tp)) match
                    case Some(tp) =>
                        Left(
                          s"${fn.displayName} has no SIR, so a call cannot pass a ${tp.show}: " +
                              "only BigInt, Boolean, ByteString, String and Data"
                        )
                    case None =>
                        val declared = valueTypes
                            .map(tp => if isData(tp) then SIRType.Data.tp else tp)
                            .reduceRight(SIRType.Fun(_, _))
                        Right(curriedTypesOf(declared, values.size))
        types.map { types =>
            val function: AnnotatedSIR = SIR.ExternalVar("", fn.name, types.head, annotations)
            values.zip(types.tail).foldLeft(function) { case (applied, (value, tp)) =>
                SIR.Apply(applied, value, tp, annotations)
            }
        }
    }

    /** A function's declared type and the types left after applying it to each of `arity` values,
      * or why it cannot be called that way.
      */
    private def curriedTypes(
        tp: SIRType,
        arity: Int,
        fn: FunctionRef[?, ?]
    ): Either[String, List[SIRType]] =
        unwrap(tp) match
            case SIRType.TypeLambda(_, _) =>
                Left(s"${fn.displayName} is polymorphic, which calls do not support yet")
            case _ if functionDepth(tp) < arity =>
                Left(s"${fn.displayName} has the type ${tp.show}, not one of $arity parameters")
            case _ => Right(curriedTypesOf(tp, arity))

    /** How many parameters a curried function type takes. */
    private def functionDepth(tp: SIRType): Int = unwrap(tp) match
        case SIRType.Fun(_, rest) => 1 + functionDepth(rest)
        case _                    => 0

    /** The types left after applying a curried function type to each of `arity` values. */
    private def curriedTypesOf(tp: SIRType, arity: Int): List[SIRType] =
        if arity == 0 then List(tp)
        else
            unwrap(tp) match
                case SIRType.Fun(_, rest) => tp :: curriedTypesOf(rest, arity - 1)
                case other =>
                    throw new IllegalStateException(s"expected a function type, got ${other.show}")

    /** The data declarations the compiler put around an expression, and the expression. */
    private def declarations(sir: SIR): (List[DataDecl], AnnotatedSIR) = sir match
        case SIR.Decl(data, term) =>
            val (inner, expression) = declarations(term)
            (data :: inner) -> expression
        case expression: AnnotatedSIR => Nil -> expression

    /** The arguments a call passes to a function of `arity` parameters, one per parameter. A
      * function of several parameters takes them in the statement as a tuple, written out as
      * `(a, b, ...)`.
      */
    private def callArguments(argument: SIR, arity: Int): Either[String, List[SIR]] =
        if arity == 1 then Right(List(argument)) else tupleComponents(argument, arity)

    /** The components of a tuple of `arity` values written out as `(a, b, ...)`. The definitions
      * the compiler put around the tuple stay around each component.
      */
    private def tupleComponents(sir: SIR, arity: Int): Either[String, List[SIR]] = {
        val (wrappers, tuple) = Wrappers.of(sir)
        tuple match
            case SIR.Constr(name, _, arguments, _, _)
                if name == s"scala.Tuple$arity" && arguments.size == arity =>
                Right(arguments.map(wrappers(_)))
            case other =>
                Left(
                  s"a function of $arity parameters takes a tuple of $arity values written out " +
                      s"as (a, b, ...), not an expression of type ${other.tp.show}"
                )
    }

    /** `left` and `right` evaluate to equal values. */
    private def equality(left: SIR, right: SIR): Either[String, SIR] = {
        val annotations = AnnotationsDecl.empty
        // `left` and `right` applied to the builtin `equals` over values of type `tp`.
        def applied(equals: SIR.Builtin, tp: SIRType): SIR =
            withExpressions(left, right) { (l, r) =>
                val partial =
                    SIR.Apply(equals, l, SIRType.Fun(tp, SIRType.Boolean), annotations)
                SIR.Apply(partial, r, SIRType.Boolean, annotations)
            }
        if isData(left.tp) && isData(right.tp) then
            Right(applied(SIRBuiltins.equalsData, SIRType.Data.tp))
        else if left.tp != right.tp then
            Left(s"cannot compare ${left.tp.show} with ${right.tp.show}")
        else
            left.tp match
                case SIRType.Integer => Right(applied(SIRBuiltins.equalsInteger, SIRType.Integer))
                case SIRType.ByteString =>
                    Right(applied(SIRBuiltins.equalsByteString, SIRType.ByteString))
                case SIRType.String => Right(applied(SIRBuiltins.equalsString, SIRType.String))
                case SIRType.Boolean =>
                    Right(withExpressions(left, right) { (l, r) =>
                        SIR.IfThenElse(l, r, SIR.Not(r, annotations), SIRType.Boolean, annotations)
                    })
                case other => Left(s"equality of ${other.show} values is not supported")
    }

    /** Combines two expressions, keeping the data declarations around them outside. */
    private def withExpressions(left: SIR, right: SIR)(
        combine: (AnnotatedSIR, AnnotatedSIR) => AnnotatedSIR
    ): SIR = (left, right) match
        case (SIR.Decl(data, term), _) => SIR.Decl(data, withExpressions(term, right)(combine))
        case (_, SIR.Decl(data, term)) => SIR.Decl(data, withExpressions(left, term)(combine))
        case (l: AnnotatedSIR, r: AnnotatedSIR) => combine(l, r)

    private def compileSirFunction(
        binders: List[PropExpr.Ident[?]],
        body: SIR,
        functions: FunctionTable
    ): Term = {
        val (unlinkedBody, free) = unlinkModuleDefinitions(body, functions)
        val external = externalVariables(unlinkedBody).distinctBy(_._1)
        val linked = external.filter((name, _) => functions.contains(name))
        // Other external references, such as the compiler's own support functions behind
        // `d.to[A]`, are resolved by the lowering, which reports one it does not know.
        val unbound = free -- binders.map(_.name) -- external.map(_._1)
        require(
          unbound.isEmpty,
          s"UPLC proposition body has free variables: ${unbound.toList.sorted.mkString(", ")}"
        )
        // A variable over the `Data` encodings of its values takes the `Data`.
        val parameters = linked ++ binders.map(ident =>
            ident.name -> DataEncoding.encoded(ident.tp).fold(ident.tp)(_ => SIRType.Data())
        )
        val function = parameters.foldRight(unlinkedBody) { case ((name, tp), result) =>
            SIR.LamAbs(
              SIR.Var(name, tp, AnnotationsDecl.empty),
              result,
              Nil,
              AnnotationsDecl.empty
            )
        }
        linked.foldLeft(function.toUplc(using options)()) { case (term, (name, _)) =>
            val definition = functions(FunctionRef[Any, Any](name))
            Term.Apply(term, definition(Representation.Uplc).term)
        }
    }

    /** Checks that the tests call each function the statement uses the way its program takes its
      * arguments, or explains the difference.
      *
      * A test calls a function as a variable of its declared type, and the lowering passes values
      * across that call in the representations [[UplcSignature.of]] gives for that type under the
      * tactic's options. The function's program records the ones its own options gave, which can
      * differ, as another lowering backend's do. Linking the two would compose programs that
      * disagree. Both sides are computed from the declared type, so they differ only by the
      * options.
      */
    private def checkSignatures(prop: Prop, functions: FunctionTable): Either[String, Unit] = {
        val names = usedFunctions(prop).filter(functions.contains).distinct
        names.iterator
            .flatMap { name =>
                val definition = functions(FunctionRef[Any, Any](name))
                for
                    sir <- definition.get(Representation.Sir)
                    signature <- definition.get(Representation.UplcSignature)
                    expected = UplcSignature.of(
                      sir.tp,
                      definition.arity,
                      options,
                      options.targetLanguage
                    )
                    if !expected.agrees(signature)
                yield s"the program of ${definition.ref.displayName} takes ${signature.show}, but " +
                    s"the tests call it as ${expected.show}; compile it with UplcBlaster.options"
            }
            .nextOption()
            .toLeft(())
    }

    /** The names of the functions a statement calls or refers to in its expressions. */
    private def usedFunctions(prop: Prop): List[String] = {
        def expression(expr: PropExpr[?]): List[String] = expr match
            case PropExpr.SIRExpr(sir) => externalVariables(sir).map(_._1)
            case _: PropExpr.Ident[?]  => Nil
        prop match
            case Prop.Bool(expr)                => expression(expr)
            case Prop.Denotes(expr)             => expression(expr)
            case Prop.Equal(left, right)        => expression(left) ++ expression(right)
            case Prop.Call(fn, arg, _, _, body) => fn.name :: expression(arg) ++ usedFunctions(body)
            case Prop.Forall(_, body)           => usedFunctions(body)
            case Prop.Exists(_, witness, body) =>
                witness.toList.flatMap(expression) ++ usedFunctions(body)
            case Prop.And(left, right)     => usedFunctions(left) ++ usedFunctions(right)
            case Prop.Or(left, right)      => usedFunctions(left) ++ usedFunctions(right)
            case Prop.Implies(left, right) => usedFunctions(left) ++ usedFunctions(right)
            case Prop.Iff(left, right)     => usedFunctions(left) ++ usedFunctions(right)
            case Prop.Not(inner)           => usedFunctions(inner)
    }

    private def expressionSir(expr: PropExpr[?]): SIR = expr match
        case PropExpr.SIRExpr(sir) => sir
        case PropExpr.Ident(name, _, tp) =>
            SIR.Var(name, DataEncoding.valueType(tp), AnnotationsDecl.empty)

    /** Removes the module definitions of functions in `functions` from around an expression.
      * `Compiler.compile` put them there; their remaining `ExternalVar` occurrences become explicit
      * parameters, filled with the functions' own UPLC programs. Definitions of other functions
      * stay, if the expression uses them, and are compiled together with it.
      *
      * The expression's own `let`s sit in the same place as the module definitions, so a binding is
      * dropped only when nothing uses it and its value cannot fail. A statement such as
      * `require(c)` is an unused binding of a value that can.
      *
      * It returns the free variables of the result as well. They are put together on the way out,
      * so the expression under a long chain of definitions is walked once, and not once for every
      * definition around it.
      */
    private def unlinkModuleDefinitions(
        sir: SIR,
        functions: FunctionTable
    ): (SIR, Set[String]) = sir match {
        case SIR.Let(bindings, body, flags, anns) =>
            val (unlinkedBody, bodyFree) = unlinkModuleDefinitions(body, functions)
            // Each binding with the free variables of its own value, found when they are asked
            // for: a definition that turns out dead is not walked.
            final class Candidate(val binding: Binding) {
                lazy val free: Set[String] = freeVariables(binding.value)
            }
            val candidates = bindings
                .filterNot(binding => functions.contains(binding.name))
                .map(new Candidate(_))
            @annotation.tailrec
            def live(names: Set[String]): Set[String] = {
                val next = names ++ candidates.iterator
                    .filter(candidate => names.contains(candidate.binding.name))
                    .flatMap(_.free)
                if next == names then names else live(next)
            }
            // A strict binding is evaluated where it stands. One whose value can fail, such as a
            // `require(...)` statement, stays whether or not anything uses it: only a value can be
            // dropped unused, as a module's function definitions are.
            val evaluated =
                if SIR.LetFlags.isLazy(flags) then Nil
                else candidates.filterNot(candidate => isValue(candidate.binding.value))
            val roots = bodyFree ++
                evaluated.flatMap(candidate => candidate.free + candidate.binding.name)
            val liveNames = live(roots)
            val kept = candidates.filter(candidate => liveNames.contains(candidate.binding.name))
            if kept.isEmpty then (unlinkedBody, bodyFree)
            else
                val free = freeOfLet(
                  kept.map(_.binding.name).toSet,
                  kept.iterator.flatMap(_.free).toSet,
                  bodyFree,
                  SIR.LetFlags.isRec(flags)
                )
                (SIR.Let(kept.map(_.binding), unlinkedBody, flags, anns), free)
        case SIR.Decl(data, term) =>
            val (unlinkedTerm, free) = unlinkModuleDefinitions(term, functions)
            (SIR.Decl(data, unlinkedTerm), free)
        case other => (other, freeVariables(other))
    }

    /** The free variables of a `let` that binds `bound`: those of its values, without its own names
      * where it is recursive, and those of its body that it does not bind.
      */
    private def freeOfLet(
        bound: Set[String],
        inValues: Set[String],
        inBody: Set[String],
        recursive: Boolean
    ): Set[String] = (if recursive then inValues -- bound else inValues) ++ (inBody -- bound)

    /** A term that evaluates to itself: it cannot fail, so an unused binding of it can be dropped.
      */
    private def isValue(sir: SIR): Boolean = sir match
        case _: SIR.LamAbs | _: SIR.Const | _: SIR.Var | _: SIR.ExternalVar | _: SIR.Builtin => true
        case SIR.Decl(_, term) => isValue(term)
        case _                 => false

    private def externalVariables(sir: SIR): List[(String, SIRType)] = sir match
        case SIR.ExternalVar(_, name, tp, _) => List(name -> tp)
        case SIR.Let(bindings, body, _, _) =>
            bindings.flatMap(binding => externalVariables(binding.value)) ++ externalVariables(body)
        case SIR.LamAbs(_, body, _, _) => externalVariables(body)
        case SIR.Apply(function, argument, _, _) =>
            externalVariables(function) ++ externalVariables(argument)
        case SIR.Select(scrutinee, _, _, _)             => externalVariables(scrutinee)
        case _: SIR.Var | _: SIR.Const | _: SIR.Builtin => Nil
        case SIR.And(left, right, _) => externalVariables(left) ++ externalVariables(right)
        case SIR.Or(left, right, _)  => externalVariables(left) ++ externalVariables(right)
        case SIR.Not(inner, _)       => externalVariables(inner)
        case SIR.IfThenElse(condition, ifTrue, ifFalse, _, _) =>
            externalVariables(condition) ++ externalVariables(ifTrue) ++ externalVariables(ifFalse)
        case SIR.Error(message, _, _)          => externalVariables(message)
        case SIR.Constr(_, _, arguments, _, _) => arguments.flatMap(externalVariables)
        case SIR.Match(scrutinee, cases, _, _) =>
            externalVariables(scrutinee) ++ cases.flatMap(caze => externalVariables(caze.body))
        case SIR.Cast(inner, _, _) => externalVariables(inner)
        case SIR.Decl(_, term)     => externalVariables(term)

    private def freeVariables(sir: SIR): Set[String] = sir match
        case SIR.Var(name, _, _)            => Set(name)
        case SIR.ExternalVar(_, name, _, _) => Set(name)
        case SIR.Let(bindings, body, flags, _) =>
            freeOfLet(
              bindings.map(_.name).toSet,
              bindings.iterator.flatMap(binding => freeVariables(binding.value)).toSet,
              freeVariables(body),
              SIR.LetFlags.isRec(flags)
            )
        case SIR.LamAbs(parameter, body, _, _) => freeVariables(body) - parameter.name
        case SIR.Apply(function, argument, _, _) =>
            freeVariables(function) ++ freeVariables(argument)
        case SIR.Select(scrutinee, _, _, _) => freeVariables(scrutinee)
        case _: SIR.Const                   => Set.empty
        case SIR.And(left, right, _)        => freeVariables(left) ++ freeVariables(right)
        case SIR.Or(left, right, _)         => freeVariables(left) ++ freeVariables(right)
        case SIR.Not(inner, _)              => freeVariables(inner)
        case SIR.IfThenElse(condition, ifTrue, ifFalse, _, _) =>
            freeVariables(condition) ++ freeVariables(ifTrue) ++ freeVariables(ifFalse)
        case _: SIR.Builtin                    => Set.empty
        case SIR.Error(message, _, _)          => freeVariables(message)
        case SIR.Constr(_, _, arguments, _, _) => arguments.iterator.flatMap(freeVariables).toSet
        case SIR.Match(scrutinee, cases, _, _) =>
            freeVariables(scrutinee) ++ cases.iterator.flatMap { caze =>
                val bound = caze.pattern match
                    case SIR.Pattern.Constr(_, bindings, _) => bindings.toSet
                    case _                                  => Set.empty[String]
                freeVariables(caze.body) -- bound
            }.toSet
        case SIR.Cast(inner, _, _) => freeVariables(inner)
        case SIR.Decl(_, term)     => freeVariables(term)

    /** Writes the Lean check of a lowered statement into `directory`: each leaf's program as
      * `Leaf<n>.flat`, which the check imports, and as text, `Leaf<n>.uplc`, to be read; and
      * `Check.lean`, whose path it returns. The tactic runs the same text in its server. A check
      * written with this can be read, or run by hand in a process of its own and with other
      * options: `lake lean Check.lean` in the workspace, which loads Blaster as the server does.
      * `lake env lean` loads no plugin, and runs Blaster interpreted.
      */
    def writeCheck(goal: Lowered, budget: Int, directory: Path): Path =
        writeCheck(goal, budget, defaultMaxHeartbeats, directory)

    /** [[writeCheck]], with the limit on Lean's own work the check sets. */
    def writeCheck(goal: Lowered, budget: Int, maxHeartbeats: Int, directory: Path): Path = {
        val source = directory.resolve("Check.lean")
        Files.writeString(source, checkSource(goal, budget, maxHeartbeats, directory))
        readable(goal.leaves, directory)
        source
    }

    /** Writes each program into `directory` as text, as `Leaf<n>.uplc`, beside its `Leaf<n>.flat`.
      */
    private def readable(programs: Vector[Program], directory: Path): Unit = {
        programs.zipWithIndex.foreach { (program, index) =>
            Files.writeString(directory.resolve(s"Leaf$index.uplc"), program.show)
        }
    }

    /** The text of the check of a lowered statement. Its leaves are written into `directory`, where
      * the text refers to them.
      */
    private def checkSource(
        goal: Lowered,
        budget: Int,
        maxHeartbeats: Int,
        directory: Path
    ): String = renderCheck(goal, written(goal.leaves, directory), budget, maxHeartbeats)

    /** Writes each program into `directory`, as `Leaf<n>.flat`. */
    private def written(programs: Vector[Program], directory: Path): Vector[Path] =
        programs.zipWithIndex.map { (program, index) =>
            val flat = directory.resolve(s"Leaf$index.flat")
            Files.writeString(flat, Hex.bytesToHex(program.cborEncoded).toLowerCase)
            flat
        }

    /** What one check of a statement came to, at one budget. */
    private enum Probe {

        /** The statement's result, which another budget would not change, or would not help. */
        case Decided(result: ExecutionResult)

        /** Lean falsified the statement under the budget with `values`, on which it holds without
          * the budget on the Scalus CEK. Why, Lean's machine says on those values ([[counted]]):
          * the program of a test needs more steps, or the values are not those Lean falsified the
          * statement with.
          */
        case Spurious(values: List[Constant], reason: String)

        /** Lean falsified the statement under the budget, and gave no values to replay: the
          * statement asks for a witness, or the values are outside a variable's type. Nothing shows
          * whether the statement or the budget is at fault.
          */
        case Unreplayed(reason: String)

        /** The result where the budget was given: a falsification that the budget may be at fault
          * for decides nothing.
          */
        def toResult: ExecutionResult = this match
            case Decided(result)     => result
            case Spurious(_, reason) => VerificationResult.Inconclusive(reason)
            case Unreplayed(reason)  => VerificationResult.Inconclusive(reason)
    }

    /** Runs the check of a lowered statement in `server`, and reads what Lean reported.
      *
      * The leaves are in a directory of the server's for as long as the check runs. With `keep`,
      * the check is also written there, in a directory of its own, as [[writeCheck]] writes it.
      */
    private[uplcblaster] def check(
        goal: Lowered,
        budget: Int,
        maxHeartbeats: Int,
        server: LeanServer,
        timeout: Option[FiniteDuration],
        keep: Option[Path]
    ): ExecutionResult = {
        keep.foreach(keepCheck(goal, budget, maxHeartbeats, _))
        val checked = server.check(checkSource(goal, budget, maxHeartbeats, _), timeout)
        probed(goal, budget, checked, timeout.mkString) match
            // Within a budget a run the budget cuts counts for the statement, and a run that ends
            // within it ends so with more steps: the budget is not at fault.
            case Probe.Spurious(_, reason) if goal.within.isDefined =>
                VerificationResult.Inconclusive(
                  s"$reason. Within a budget a run the budget cuts counts for the statement, so " +
                      "more steps change nothing: the values read from Lean's counterexample do " +
                      "not show what Lean found" + unknownTo(goal)
                )
            // The budget was given, so no other is tried. Lean's machine says whether the budget
            // is at fault: a counterexample is spurious for another reason too.
            case Probe.Spurious(values, reason) =>
                keepCount(goal, values, maxHeartbeats, keep)
                val count = server.check(
                  directory =>
                      measureSource(goal, written(goal.leaves, directory), values, maxHeartbeats),
                  timeout
                )
                val why = runs(goal, count).map(counted(goal, _, budget)) match
                    case Right(Right(more)) =>
                        s"On Lean's machine it holds there from $more steps on, so a test needs " +
                            s"more than $budget steps"
                    case Right(Left(other)) => other.capitalize
                    case Left(VerificationResult.Inconclusive(uncounted)) =>
                        s"A test may need more than $budget steps: $uncounted"
                    case Left(VerificationResult.Failed(uncounted)) =>
                        s"A test may need more than $budget steps: $uncounted"
                    case Left(_) => s"A test may need more than $budget steps"
                VerificationResult.Inconclusive(s"$reason. $why")
            case probe => probe.toResult
    }

    /** A directory of its own under `kept`, for one check that is kept. */
    private def keptDirectory(kept: Path): Path =
        Files.createTempDirectory(Files.createDirectories(kept), "check-")

    /** Writes the check of a lowered statement at `budget` into a directory of its own under `kept`
      * ([[writeCheck]]), and says where on the standard output: a check is kept to be read, and the
      * line says which file is the one of a test that runs.
      */
    private def keepCheck(goal: Lowered, budget: Int, maxHeartbeats: Int, kept: Path): Unit = {
        val source = writeCheck(goal, budget, maxHeartbeats, keptDirectory(kept))
        println(s"Lean check at $budget steps: $source")
    }

    /** What a check at `budget` came to, from what the server returned. The reason of a check that
      * was given up names its time limit as `limit`.
      */
    private def probed(
        goal: Lowered,
        budget: Int,
        checked: LeanServer.Result,
        limit: String
    ): Probe = checked match
        case LeanServer.Result.Finished(messages) =>
            probed(
              goal,
              budget,
              messages.exists(_.severity == LeanServer.Severity.Error),
              messages.map(_.text).mkString("\n")
            )
        // Only a check with a time limit is given up for it.
        case LeanServer.Result.TimedOut(progress) =>
            Probe.Decided(
              VerificationResult.Inconclusive(
                s"Lean did not finish within $limit: " +
                    unfinished(goal.leaves.size, budget, progress)
              )
            )
        case LeanServer.Result.Failed(reason) => Probe.Decided(VerificationResult.Failed(reason))

    /** The budget a search starts at: a check at a small budget is cheap. */
    private val searchStart = 100

    /** The most steps of Lean's machine that a search counts on a counterexample, and so the
      * largest budget it comes to ([[Budget.Auto]]).
      */
    val maxSearchedBudget: Int = 100000

    /** The largest budget a search comes to by doubling, where a falsification gives it no values
      * to count on. A statement of that kind that is false is falsified at every budget.
      */
    val maxDoubledBudget: Int = 6400

    /** Finds a budget for a lowered statement, and returns its result at that budget
      * ([[Budget.Auto]], docs/design/verification-details/blaster-budget-search.md).
      *
      * A check at a budget that is too small gives a counterexample that is spurious: the statement
      * holds on it, and under the budget the program of a test had not ended. Lean's machine then
      * counts what each program does on that counterexample, and the next budget is the least at
      * which the statement, as Lean reads it under a budget, holds there. That is not the most
      * steps a program makes on it. A premise that is false need only be seen to be false, however
      * long the program of the conclusion runs: the budget of that run is the oversized one, at
      * which Lean follows the programs through every loop.
      *
      * A falsification without values to count on has the budget doubled, up to
      * [[maxDoubledBudget]]. `timeout` is the time of the whole search, and `attempt` that of one
      * check of it. The server is asked for before every check, as for any other. `first` is a
      * budget an earlier search came to, which is tried before the search begins.
      */
    private[uplcblaster] def search(
        goal: Lowered,
        maxHeartbeats: Int,
        servers: LeanServerProvider,
        timeout: Option[FiniteDuration],
        attempt: Option[FiniteDuration],
        keep: Option[Path],
        first: Option[Int]
    ): ExecutionResult = {
        val deadline = timeout.map(_.fromNow)
        // The time the next check gets, with the name of that limit for the reason of a check
        // that is given up. Nothing where the time of the search has passed.
        def time(): Option[(Option[FiniteDuration], String)] = {
            val left = deadline.map(_.timeLeft)
            if left.exists(_ <= Duration.Zero) then None
            else
                attempt match
                    case Some(one) if left.forall(_ > one) =>
                        Some(Some(one) -> s"$one, the time of one attempt")
                    case _ => Some(left -> timeout.mkString)
        }
        // The programs are written once, where every check of the search imports them from: the
        // lines that import them are then the same in each, and the server elaborates them once.
        val directory = Files.createTempDirectory("scalus-search-")
        val flats = written(goal.leaves, directory)

        // A result that is no proof says which budgets were tried, and what the last gave.
        def inconclusive(
            reason: String,
            tried: List[Int],
            last: Option[String]
        ): ExecutionResult = {
            val sought =
                if tried.isEmpty then "The tactic sought the budget, and had not tried one"
                else
                    "The tactic sought the budget, and tried " +
                        s"${tried.reverse.mkString(", ")} steps"
            VerificationResult.Inconclusive(
              s"$reason. $sought" + last.fold("")(gave => s". The last counterexample: $gave")
            )
        }
        def outOfTime(tried: List[Int], last: Option[String]): ExecutionResult =
            inconclusive(s"Lean did not finish within ${timeout.mkString}", tried, last)

        // One check of the statement at `steps`.
        def probe(steps: Int, wait: Option[FiniteDuration], limit: String): Probe =
            servers.server() match
                case Left(reason) => Probe.Decided(VerificationResult.Failed(reason))
                case Right(server) =>
                    val text = renderCheck(goal, flats, steps, maxHeartbeats)
                    keep.foreach(keepCheck(goal, steps, maxHeartbeats, _))
                    probed(goal, steps, server.check(text, wait), limit)

        // The budget after `steps`, whose check gave the spurious counterexample `values`.
        def counted(
            values: List[Constant],
            steps: Int,
            tried: List[Int],
            reason: String
        ): Either[ExecutionResult, Int] =
            time() match
                case None => Left(outOfTime(tried, Some(reason)))
                case Some((wait, _)) =>
                    raised(goal, flats, values, steps, maxHeartbeats, servers, wait, keep).left
                        .map {
                            case VerificationResult.Inconclusive(why) =>
                                inconclusive(why, tried, Some(reason))
                            case other => other
                        }

        @annotation.tailrec
        def at(steps: Int, tried: List[Int], last: Option[String]): ExecutionResult =
            time() match
                case None => outOfTime(tried, last)
                case Some((wait, limit)) =>
                    probe(steps, wait, limit) match
                        case Probe.Decided(VerificationResult.Inconclusive(reason)) =>
                            inconclusive(reason, steps :: tried, last)
                        case Probe.Decided(result) => result
                        case Probe.Unreplayed(reason) =>
                            if steps * 2 <= maxDoubledBudget then
                                at(steps * 2, steps :: tried, last)
                            else inconclusive(reason, steps :: tried, last)
                        case Probe.Spurious(values, reason) =>
                            counted(values, steps, steps :: tried, reason) match
                                case Right(more)  => at(more, steps :: tried, Some(reason))
                                case Left(result) => result

        // A budget that was kept is tried first. Where it does not decide the statement any
        // more, it says nothing about where to look, and the search is from the start. It gets
        // half of the time at most: the budget may be one that was given, not found, and too
        // large for the statement, and the search is to have its time after it.
        def kept: Option[ExecutionResult] =
            for
                steps <- first
                (wait, limit) <- time()
                result <- probe(steps, wait.map(_ / 2), limit) match
                    case Probe.Decided(proved: VerificationResult.Proven)   => Some(proved)
                    case Probe.Decided(refuted: VerificationResult.Refuted) => Some(refuted)
                    case _                                                  => None
            yield result

        // A statement with limits starts where every test ends on the largest values that meet
        // them. Below, no run ends, the guard's neither: a counterexample would be of runs the
        // budget cuts, which the solver seldom finds, and the search would stop undetermined.
        //
        // The count gets a quarter of the time of an attempt, or of the search, at most, and is
        // written nowhere: it is of no counterexample. Where it does not end, or a run does not
        // end within what it counts, the search starts as any other: the count is a start, not
        // a result.
        def start: Int =
            (largestValues(goal), time()) match
                case (Some(values), Some((wait, _))) =>
                    measured(
                      goal,
                      flats,
                      values,
                      maxHeartbeats,
                      servers,
                      wait.map(_ / 4),
                      None
                    ) match
                        case Right(runs) if runs.forall(_.end != End.Running) =>
                            math.max(searchStart, (runs.map(_.steps).max * 5 + 3) / 4)
                        case _ => searchStart
                case _ => searchStart

        try kept.getOrElse(at(start, Nil, None))
        finally Directories.remove(directory)
    }

    /** The values the search counts the steps of a statement with limits on: for each variable with
      * limits, the largest value that meets them ([[Limits.largest]]), and for each other, the
      * value of a variable Lean leaves out of a counterexample. None for a statement without
      * limits.
      */
    private def largestValues(goal: Lowered): Option[List[Constant]] = {
        val limited = goal.binders.map(binder => DataEncoding.limits(binder.tp))
        if limited.forall(_.all.isEmpty) then None
        else
            val values = goal.binders.zip(limited).map { (binder, limits) =>
                if limits.all.isEmpty then unvalued(binder.tp)
                else
                    limits
                        .largest(DataLayout.of(DataEncoding.valueType(binder.tp)))
                        .map(Constant.Data(_))
            }
            Option.when(values.forall(_.isDefined))(values.flatten)
    }

    /** The value of a variable of type `tp` that Lean leaves out of a counterexample: `0`, `false`,
      * the empty string or list, `I 0`, or an example of the type of a variable over its `Data`.
      */
    private def unvalued(tp: SIRType): Option[Constant] = variableOf(tp).map {
        case Variable.Integer    => Constant.Integer(BigInt(0))
        case Variable.Boolean    => Constant.Bool(false)
        case Variable.ByteString => Constant.ByteString(ByteString.empty)
        case Variable.String     => Constant.String("")
        case Variable.WholeData  => Constant.Data(Data.I(0))
        case Variable.EncodedData =>
            Constant.Data(variableEncoding(tp).example.getOrElse(Data.I(0)))
        case Variable.DataList => dataList(Nil)
        case Variable.PairList => pairList(Nil)
        case Variable.DataMap  => Constant.Data(Data.Map(PList.Nil))
    }

    /** The budget to go on with after `steps` gave the spurious counterexample `values`, or the
      * result where there is none. With `keep`, the count is also written there.
      */
    private def raised(
        goal: Lowered,
        flats: Vector[Path],
        values: List[Constant],
        steps: Int,
        maxHeartbeats: Int,
        servers: LeanServerProvider,
        wait: Option[FiniteDuration],
        keep: Option[Path]
    ): Either[ExecutionResult, Int] =
        measured(goal, flats, values, maxHeartbeats, servers, wait, keep).flatMap { runs =>
            counted(goal, runs, steps).left.map(VerificationResult.Inconclusive(_))
        }

    /** What the runs of the tests' programs on Lean's counterexample to a check at `steps` come to,
      * where the statement holds on it on the Scalus CEK: the least budget at which it holds on
      * Lean's machine too, or why there is none.
      */
    private def counted(goal: Lowered, runs: Vector[Run], steps: Int): Either[String, Int] =
        // Lean falsified the statement at `steps`. Where the runs do not, the values read from
        // its model are not those it falsified the statement with.
        if holdsWithin(goal.body, runs, steps, positive = true) then
            Left(
              s"on the values read from Lean's counterexample the statement holds within " +
                  s"$steps steps of Lean's machine, so they do not show what Lean found" +
                  unknownTo(goal)
            )
        else
            needed(goal.body, runs, steps) match
                case Some(more) => Right(more)
                case None if runs.exists(_.end == End.Running) =>
                    Left(
                      s"on Lean's counterexample the statement takes more than " +
                          s"$maxSearchedBudget steps of Lean's machine, the most the tactic counts"
                    )
                case None =>
                    Left(
                      s"on Lean's counterexample the statement holds at no budget on Lean's " +
                          "machine, though it holds on the Scalus CEK"
                    )

    /** The builtins whose functions Lean's library declares `opaque`: hashes, signatures and the
      * arithmetic of curves. A proof knows nothing of such a function but that equal arguments give
      * equal results, so a counterexample can take a result for one that it does not have.
      */
    private val opaqueBuiltins: Set[DefaultFun] = {
        import DefaultFun.*
        Set(
          Sha2_256,
          Sha3_256,
          Blake2b_224,
          Blake2b_256,
          Keccak_256,
          Ripemd_160,
          VerifyEd25519Signature,
          VerifyEcdsaSecp256k1Signature,
          VerifySchnorrSecp256k1Signature,
          ExpModInteger
        ) ++ DefaultFun.values.filter(_.toString.startsWith("Bls12_381"))
    }

    /** What may make values that do not show what Lean found: the builtins the programs of `goal`
      * apply of which Lean knows nothing ([[opaqueBuiltins]]). Empty where they apply none.
      */
    private def unknownTo(goal: Lowered): String = {
        val applied = goal.leaves
            .flatMap(_.term.collectBuiltins)
            .filter(opaqueBuiltins)
            .map(_.toString)
            .map(name => s"${name.head.toLower}${name.tail}")
            .distinct
            .sorted
        if applied.isEmpty then ""
        else
            s". The programs apply ${applied.mkString(", ")}, of which Lean knows only that equal " +
                "arguments give equal results: its counterexample can take a result for what " +
                "the builtin does not give, and no budget changes that. Write such a result in " +
                "a statement as the builtin applied to its arguments"
    }

    /** How the program of a test ended on Lean's machine. */
    private[uplcblaster] enum End {

        /** It returned: `true` or `false`, or a value that is neither. */
        case Returned(value: Option[Boolean])
        case Failed

        /** It had done neither within the steps that were counted. */
        case Running
    }

    /** What the program of a test did on concrete arguments on Lean's machine: the steps it made,
      * which is the least budget at which Lean sees how it ended.
      */
    private[uplcblaster] final case class Run(steps: Int, end: End)

    private val measurement = """"?(\d+) (true|false|returned|failed|running)"?""".r

    /** What the program of each test does on `values`, on Lean's machine (`measureRun`). */
    private def measured(
        goal: Lowered,
        flats: Vector[Path],
        values: List[Constant],
        maxHeartbeats: Int,
        servers: LeanServerProvider,
        wait: Option[FiniteDuration],
        keep: Option[Path]
    ): Either[ExecutionResult, Vector[Run]] = {
        keepCount(goal, values, maxHeartbeats, keep)
        servers.server() match
            case Left(reason) => Left(VerificationResult.Failed(reason))
            case Right(server) =>
                runs(goal, server.check(measureSource(goal, flats, values, maxHeartbeats), wait))
    }

    /** Writes the count of a counterexample that is kept into `keep`, with the programs beside it,
      * to be run by hand.
      */
    private def keepCount(
        goal: Lowered,
        values: List[Constant],
        maxHeartbeats: Int,
        keep: Option[Path]
    ): Unit =
        keep.foreach { kept =>
            val directory = keptDirectory(kept)
            val text = measureSource(goal, written(goal.leaves, directory), values, maxHeartbeats)
            val source = directory.resolve("Count.lean")
            Files.writeString(source, text)
            readable(goal.leaves, directory)
            println(s"Lean count of a spurious counterexample: $source")
        }

    /** The run of each test's program, from what Lean reported of a count ([[measureSource]]). */
    private def runs(
        goal: Lowered,
        reported: LeanServer.Result
    ): Either[ExecutionResult, Vector[Run]] = {
        def failed(reason: String): Either[ExecutionResult, Vector[Run]] =
            Left(VerificationResult.Failed(reason))
        reported match {
            case LeanServer.Result.Finished(messages) =>
                val read = messages
                    .map(_.text.trim)
                    .collect { case measurement(steps, end) => Run(steps.toInt, ended(end)) }
                    .toVector
                val errors = messages.exists(_.severity == LeanServer.Severity.Error)
                if !errors && read.size == goal.leaves.size then Right(read)
                else
                    val output = concise(messages.map(_.text).mkString("\n"))
                    failed(s"Lean did not count the steps of its counterexample: $output")
            case LeanServer.Result.TimedOut(_) =>
                Left(
                  VerificationResult.Inconclusive(
                    "Lean did not count the steps of its counterexample in the time it had"
                  )
                )
            case LeanServer.Result.Failed(reason) => failed(reason)
        }
    }

    /** How a run ended, from the word `measureRun` says it with. */
    private def ended(word: String): End = word match
        case "true"     => End.Returned(Some(true))
        case "false"    => End.Returned(Some(false))
        case "returned" => End.Returned(None)
        case "failed"   => End.Failed
        case _          => End.Running

    /** The text of a check that has Lean's machine say what the program of each test does on
      * `values`, within the most steps a search counts. The programs are those in `flats`.
      */
    private[uplcblaster] def measureSource(
        goal: Lowered,
        flats: Vector[Path],
        values: List[Constant],
        maxHeartbeats: Int
    ): String = {
        val counts = goal.leafBinders.zipWithIndex.map { (scope, leaf) =>
            val arguments = scope.map(index => s"Term.Const (${leanValue(values(index))})")
            s"#eval measureRun leaf$leaf.script [${arguments.mkString(", ")}] $maxSearchedBudget"
        }
        document(flats, counts.mkString("\n"), maxHeartbeats)
    }

    /** A value of a variable as a Lean constant, to give a counterexample back to Lean as the terms
      * it is made of. An encoded program would not carry every value of Lean's model: a `Data`
      * constructor with a tag of more than 64 bits is written by Scalus in a form that Lean's
      * library does not read.
      */
    private[uplcblaster] def leanValue(value: Constant): String = value match
        case Constant.Integer(number)    => s"Const.Integer ${leanInteger(number)}"
        case Constant.Bool(truth)        => s"Const.Bool $truth"
        case Constant.ByteString(string) => s"Const.ByteString ${leanBytes(string)}"
        case Constant.String(string)     => s"Const.String \"${leanString(string)}\""
        case Constant.Data(tree)         => s"Const.Data ${leanData(tree)}"
        case Constant.List(DefaultUni.Data, items) =>
            val values = items.map {
                case Constant.Data(tree) => leanData(tree)
                case other =>
                    throw new IllegalArgumentException(s"a list of Data has the item $other")
            }
            s"Const.ConstDataList [${values.mkString(", ")}]"
        case Constant.List(element, items) if element == DataPair =>
            val pairs = items.map {
                case Constant.Pair(Constant.Data(key), Constant.Data(value)) =>
                    s"(${leanData(key)}, ${leanData(value)})"
                case other =>
                    throw new IllegalArgumentException(s"a list of pairs has the item $other")
            }
            s"Const.ConstPairDataList [${pairs.mkString(", ")}]"
        case other =>
            throw new IllegalArgumentException(
              s"no variable of a statement has the value $other"
            )

    /** A `Data` as a Lean term of PlutusCore's `Data`. */
    private[uplcblaster] def leanData(value: Data): String = value match
        case Data.Constr(tag, fields) =>
            s"(Data.Constr ${leanInteger(tag)} [${fields.toScalaList.map(leanData).mkString(", ")}])"
        case Data.Map(entries) =>
            val pairs =
                entries.toScalaList.map((key, item) => s"(${leanData(key)}, ${leanData(item)})")
            s"(Data.Map [${pairs.mkString(", ")}])"
        case Data.List(items) =>
            s"(Data.List [${items.toScalaList.map(leanData).mkString(", ")}])"
        case Data.I(number) => s"(Data.I ${leanInteger(number)})"
        case Data.B(string) => s"(Data.B ${leanBytes(string)})"

    private def leanInteger(value: BigInt): String =
        if value < 0 then s"($value)" else value.toString

    /** One character for a byte, as Lean's library holds a byte string. Lean reads `\xHH` as the
      * character of that code; what it prints, `\u{HH}`, it does not read.
      */
    private def leanBytes(value: ByteString): String =
        value.bytes.map(byte => f"\\x${byte & 0xff}%02x").mkString("(ByteString.mk \"", "", "\")")

    /** The truth of a formula as Lean reads it under a budget (`renderFormula`), on the runs of its
      * leaves on one assignment. A program that has not ended within the budget counts against what
      * the formula claims of it, and for what the formula assumes.
      */
    private[uplcblaster] def holdsWithin(
        formula: LeafFormula,
        runs: Vector[Run],
        budget: Int,
        positive: Boolean
    ): Boolean = {
        def end(leaf: Int): End = if runs(leaf).steps <= budget then runs(leaf).end else End.Running
        def within(inner: LeafFormula, positive: Boolean): Boolean =
            holdsWithin(inner, runs, budget, positive)
        formula match
            case LeafFormula.Test(leaf) =>
                if positive then end(leaf) == End.Returned(Some(true))
                else end(leaf) != End.Returned(Some(false)) && end(leaf) != End.Failed
            case LeafFormula.Denotes(leaf) =>
                if positive then end(leaf).isInstanceOf[End.Returned] else end(leaf) != End.Failed
            case LeafFormula.And(left, right) => within(left, positive) && within(right, positive)
            case LeafFormula.Or(left, right)  => within(left, positive) || within(right, positive)
            case LeafFormula.Not(inner)       => !within(inner, !positive)
            case LeafFormula.Implies(premise, conclusion) =>
                !within(premise, !positive) || within(conclusion, positive)
            // One assignment, as in `holds`: a quantifier here ranges over all values.
            case LeafFormula.Forall(_, body) => within(body, positive)
            case LeafFormula.Exists(_, body) => within(body, positive)
    }

    /** The least budget above `above` at which the formula holds on `runs`, read as Lean reads it
      * under a budget. Its truth changes only where a program ends.
      */
    private[uplcblaster] def needed(
        formula: LeafFormula,
        runs: Vector[Run],
        above: Int
    ): Option[Int] = {
        val ends = runs.collect {
            case Run(steps, end) if end != End.Running && steps > above => steps
        }
        ends.distinct.sorted.find(holdsWithin(formula, runs, _, positive = true))
    }

    private val symbolicRun = """#prep_uplc_run prepared(\d+) .*""".r

    /** Where a check of `tests` programs was when its time passed, for the reason of an
      * inconclusive result.
      *
      * A check runs each test's program symbolically for the budget. Then Blaster simplifies the
      * statement, starts the solver, translates the statement for it and asks it. So what a check
      * was at tells what to do about it: a symbolic run that goes on follows a program through a
      * loop, and wants a smaller budget or a fixed shape of what is looped over.
      */
    private[uplcblaster] def unfinished(
        tests: Int,
        budget: Int,
        progress: LeanServer.Progress
    ): String = {
        import LeanServer.{Reached, Started}
        def seconds(time: FiniteDuration): String = s"${time.toSeconds} s"
        // What a check starts besides Lean's own processes, its worker and `lake`, is the solver.
        def solver: Option[Started] = progress.started.find { started =>
            !started.name.startsWith("lean") && !started.name.startsWith("lake")
        }
        progress.reached match
            case Reached.NotStarted => "the check had not started: its server was at another check"
            case Reached.Unreported => "Lean had not said how far the check had come"
            case Reached.End =>
                "Lean had run every command of the check, and had not finished checking what " +
                    "they define"
            case Reached.Command(line) =>
                line.trim match
                    case symbolicRun(test) =>
                        s"after ${seconds(progress.spent)} it was still running the program of " +
                            s"test ${test.toInt + 1} of $tests symbolically, for $budget steps, " +
                            "and had not come to the solver. A budget closer to the steps of " +
                            "the runs the statement is about may do"
                    case command if command.startsWith("#blaster") =>
                        solver match
                            case None =>
                                "Blaster had been simplifying the statement for " +
                                    s"${seconds(progress.spent)}, and had not started the solver"
                            // The solver waits while Blaster translates the statement for it.
                            case Some(Started(_, running, Some(working))) =>
                                s"Blaster had started the solver ${seconds(running)} before, " +
                                    s"and the solver had worked for ${seconds(working)} of them"
                            case Some(Started(_, running, None)) =>
                                s"Blaster had started the solver ${seconds(running)} before"
                    case command if command.startsWith("example") =>
                        s"Lean had been evaluating the statement for ${seconds(progress.spent)}"
                    case _ => "it was still loading the libraries and the programs"
    }

    /** The result that what Lean reported stands for: `output` is its messages, in the order of
      * their commands, and `errors` says whether one of them is an error.
      */
    private[uplcblaster] def verdict(
        goal: Lowered,
        budget: Int,
        errors: Boolean,
        output: String
    ): ExecutionResult = probed(goal, budget, errors, output).toResult

    /** [[verdict]], with what the check came to. */
    private def probed(
        goal: Lowered,
        budget: Int,
        errors: Boolean,
        output: String
    ): Probe = {
        val closed = goal.binders.isEmpty
        val artifact = Artifact(
          goal.programHashes,
          budget,
          output.trim,
          Nil,
          "",
          if closed then ProofKind.LeanNative else ProofKind.Blaster,
          kept = false
        )
        // Lean gives a command up at its own limit of work, `maxHeartbeats`, and says so. The
        // statement is then not decided, as when the time limit passes: nothing went wrong.
        def failure: ExecutionResult =
            if errors && output.contains("(deterministic) timeout") then
                VerificationResult.Inconclusive(
                  s"Lean gave up at its own limit: ${concise(output)}"
                )
            else if errors then
                VerificationResult.Failed(s"Lean reported an error: ${concise(output)}")
            else VerificationResult.Failed(s"Lean reported no verdict: ${concise(output)}")
        def proved: ExecutionResult =
            if goal.within.isDefined then
                VerificationResult.ProvenWithin(Proof(artifact), withinBound(budget))
            else VerificationResult.Proven(Proof(artifact))
        if closed then
            if !errors then Probe.Decided(proved)
            else if output.contains("`native_decide` evaluated that the proposition") then
                replay(goal, artifact)
            else Probe.Decided(failure)
        // Lean's server goes on after a command fails, so Blaster can call a goal valid in a
        // document with an error, such as a definition the goal uses that did not elaborate. Only
        // a document without errors proves the goal.
        else if !errors && output.contains("✅ Valid") then Probe.Decided(proved)
        else if output.contains("❌ Falsified") then
            if goal.hasExistential then
                Probe.Unreplayed(
                  "UPLC Blaster falsified a statement that asks for a witness, with an exists, " +
                      "or with a forAll under a negation or in a premise, but did not provide " +
                      "a finite certificate that the Scalus CEK can replay to establish that no " +
                      "witness exists"
                )
            else replay(goal, artifact)
        else if output.contains("⚠️ Undetermined") then
            Probe.Decided(VerificationResult.Inconclusive("UPLC Blaster was undetermined"))
        else Probe.Decided(failure)
    }

    /** The inputs a proof within `budget` steps holds on ([[Budget.Within]]). */
    private[uplcblaster] def withinBound(budget: Int): String =
        s"every input on which each test's program ends within $budget steps of Lean's machine"

    /** What a predicate did on concrete arguments, on the Scalus CEK. */
    private enum Outcome {
        case Returned(value: Term)
        case Failed
        case Exhausted
    }

    /** Replays Lean's counterexample on the Scalus CEK, without Lean's step budget (§5.2).
      *
      * A falsification under the budget may be spurious: a strong reading of a test is false when
      * the predicate needs more steps. Only a counterexample under which the statement is false
      * without the budget is a refutation.
      */
    private def replay(goal: Lowered, artifact: Artifact): Probe =
        counterexample(goal, artifact.output) match
            case Left(SmtValues.Unreadable.Malformed(error)) =>
                Probe.Decided(
                  VerificationResult.Failed(s"cannot read Lean's counterexample: $error")
                )
            // Lean's model has more values than the variable's type, so this one refutes nothing,
            // and is nothing to count on either.
            case Left(SmtValues.Unreadable.OutsideType(error)) =>
                Probe.Unreplayed(
                  s"Lean's counterexample is no value of its variable's type: $error"
                )
            case Right((values, renaming)) => replayValues(goal, artifact, values, renaming)

    /** Replays `values` on the Scalus CEK. They were read from Lean's counterexample with
      * `renaming` for the characters that are none of their type.
      */
    private def replayValues(
        goal: Lowered,
        artifact: Artifact,
        values: List[Constant],
        renaming: SmtValues.Renaming
    ): Probe = {
        val read =
            if renaming.isEmpty then ""
            else
                val renamed = renaming.bytes.toList.sorted.map((from, to) =>
                    f"U+$from%04X as the byte 0x$to%02x"
                ) ++ renaming.text.toList.sorted.map((from, to) => f"U+$from%04X as U+$to%04X")
                s"reading the solver's ${renamed.mkString(", ")}, which no value of the type has"
        val shown = goal.binders
            .zip(values)
            .map((binder, value) => s"${binder.name} = ${display(value)}")
            .mkString(", ")
        val falsification =
            if shown.isEmpty then "Lean's falsification"
            else if read.isEmpty then s"Lean's counterexample ($shown)"
            else s"Lean's counterexample ($shown, $read)"
        val outcomes = goal.leaves.zip(goal.leafBinders).map { (program, scope) =>
            evaluate(program, scope.map(values))
        }
        holds(goal.body, outcomes) match
            case Some(false) =>
                Probe.Decided(
                  VerificationResult.Refuted(
                    Proof(
                      // A kept refutation says how its values were read when they were found.
                      artifact.copy(
                        counterexample = goal.binders.map(_.name).zip(values),
                        read = if renaming.isEmpty then artifact.read else read
                      )
                    )
                  )
                )
            // Renamed values are not those Lean ran, so what Lean's machine does on them says
            // nothing of the budget: the search goes on as for a falsification without values.
            case Some(true) if !renaming.isEmpty =>
                Probe.Unreplayed(
                  s"$falsification is spurious: the statement holds there on the Scalus CEK, " +
                      "and values that are not Lean's say nothing of the budget"
                )
            case Some(true) =>
                Probe.Spurious(
                  values,
                  s"$falsification is spurious: the statement holds there on the Scalus CEK"
                )
            case None =>
                Probe.Decided(
                  VerificationResult.Inconclusive(
                    s"replaying $falsification exhausted the replay budget"
                  )
                )
    }

    /** What the quantifier asks of the values of a list or a map variable: the Lean premise on the
      * variable, none where it says nothing, and whether it holds of exactly the type's values.
      */
    private final case class Restriction(premise: Option[String], exact: Boolean)

    /** The encoding of a variable of type `tp` that has more values than its type
      * ([[Variable.isLarger]]): a builtin list of items, a builtin list of pairs or a `Data.Map`,
      * which is the type's `Data` encoding, or the `Data` of a value of a [[Props.forAllData]]
      * variable.
      */
    private def variableEncoding(tp: SIRType): DataEncoding =
        DataEncoding.of(DataEncoding.valueType(tp))

    /** The restriction of a variable of `kind` and type `tp` to its type's values: of the items of
      * a list or a map, or of the `Data` of a [[Props.forAllData]] variable.
      */
    private def variableRestriction(kind: Variable, tp: SIRType): Restriction = {
        val fresh = Iterator.from(0)
        variableEncoding(tp) match
            case encoding if kind == Variable.EncodedData =>
                Restriction(
                  Option.unless(encoding.isAnything)(encoding.leanPredicate(fresh)),
                  encoding.exact
                )
            case DataEncoding.Items(items) if items.isAnything => Restriction(None, exact = true)
            case DataEncoding.Entries(keys, values, DataEncoding.KeyOrder.Any)
                if keys.isAnything && values.isAnything =>
                Restriction(None, exact = true)
            case encoding @ DataEncoding.Items(items) =>
                Restriction(Some(s"(scalusAll ${items.leanPredicate(fresh)})"), encoding.exact)
            case encoding @ DataEncoding.Entries(keys, values, order) =>
                Restriction(
                  Some(DataEncoding.leanEntries(keys, values, order, fresh)),
                  encoding.exact
                )
            case other => Restriction(None, other.exact)
    }

    /** Why `value`, Lean's value for a variable of type `tp`, is no value of the type, or `None`
      * where it is one. Only a list, a map or a [[Props.forAllData]] variable has values its type
      * does not have ([[Variable.isLarger]]): its items, or itself, are any `Data`, unless the
      * quantifier's predicate is exact. A value that cannot be checked here counts as none of the
      * type's, so that a refutation never rests on it.
      */
    private[uplcblaster] def outsideType(value: Constant, tp: SIRType): Option[String] =
        outsideEncoding(value, tp).orElse(outsideLimits(value, tp))

    /** Why `value` is outside the limits of a variable of type `tp` with limits
      * ([[Props.forAllDataLimited]]), or none where it is within them, or the variable has none. A
      * guard may check less than the limits say, so a value it lets through is checked here.
      */
    private def outsideLimits(value: Constant, tp: SIRType): Option[String] = {
        val limits = DataEncoding.limits(tp)
        value match
            case Constant.Data(data) if limits.all.nonEmpty =>
                limits
                    .outside(data, DataEncoding.valueType(tp))
                    .map(reason => s"it is outside the variable's limits: $reason")
            case _ => None
    }

    private def outsideEncoding(value: Constant, tp: SIRType): Option[String] =
        variableOf(tp).filter(_.isLarger).flatMap { kind =>
            (variableEncoding(tp), value) match
                case (DataEncoding.Items(items), Constant.List(_, values)) =>
                    values.iterator.zipWithIndex
                        .flatMap {
                            case (Constant.Data(item), index) =>
                                items.outside(item).map(reason => s"item $index: $reason")
                            case (other, index) => Some(s"item $index is $other, no Data")
                        }
                        .nextOption()
                case (DataEncoding.Entries(keys, values, order), Constant.List(_, items)) =>
                    val pairs = items.collect {
                        case Constant.Pair(Constant.Data(key), Constant.Data(value)) => key -> value
                    }
                    if pairs.size != items.size then Some(s"$value is no list of pairs of Data")
                    else DataEncoding.entriesOutside(keys, values, order, pairs)
                case (encoding, Constant.Data(data)) => encoding.outside(data)
                case (encoding, other)               => Some(s"$other is no value of a ${tp.show}")
        }

    /** The value of a variable of a builtin list of `Data`. */
    private def dataList(items: List[Data]): Constant =
        Constant.List(DefaultUni.Data, items.map(Constant.Data(_)))

    /** The type of a pair of `Data`, the items of a builtin list of pairs. */
    private val DataPair: DefaultUni = DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data)

    /** The value of a variable of a builtin list of pairs of `Data`. */
    private def pairList(pairs: List[(Data, Data)]): Constant = {
        Constant.List(
          DataPair,
          pairs.map((key, value) => Constant.Pair(Constant.Data(key), Constant.Data(value)))
        )
    }

    private val counterexampleLine = """-\s+x(\d+):\s+(.+?)\s*$""".r.unanchored

    /** The values of Blaster's counterexample to `goal`, in binder order, with the renaming of the
      * characters that are none of their type ([[renaming]]), or why they cannot be read. A binder
      * the model leaves unconstrained, which Blaster omits or Z3 names instead of valuing, takes
      * `0`, `false` or `I 0`; the replay checks the completed assignment.
      *
      * A value can span several lines: a `Data` value is printed as a term over the lines after
      * `- xN:`, up to the one that closes its parentheses. They are not all indented: Z3 starts a
      * nested `let` of a large value on a new line.
      */
    private def counterexample(
        goal: Lowered,
        output: String
    ): Either[SmtValues.Unreadable, (List[Constant], SmtValues.Renaming)] = {
        val binders = goal.binders
        // The values found so far; the last one goes on while its term is not whole.
        val found = output.linesIterator.foldLeft(List.empty[(Int, String)]) { (found, line) =>
            line match
                case counterexampleLine(index, value) => (index.toInt -> value) :: found
                case _ =>
                    found match
                        case (index, value) :: earlier if !SmtValues.complete(value) =>
                            (index -> s"$value ${line.trim}") :: earlier
                        case _ => found
        }
        val reported = found.toMap
        // A value that is no term is reported as such when it is read. The programs' constants
        // are walked only where a character is to be renamed, which is seldom.
        val inValues = reported.values
            .flatMap(SmtValues.characters(_).toOption)
            .foldLeft(SmtValues.Characters.none)(_ ++ _)
        val renamed =
            if inValues.bytes.exists(_ > 255) || inValues.text.exists(SmtValues.isSurrogate) then
                renaming(inValues ++ constantCharacters(goal))
            else SmtValues.Renaming.none
        val read = binders.zipWithIndex
            .foldRight[Either[SmtValues.Unreadable, List[Constant]]](Right(Nil)) {
                case ((binder, index), rest) =>
                    val text = reported.get(index)
                    val value: Either[SmtValues.Unreadable, Constant] = variableOf(binder.tp) match
                        case Some(Variable.Integer) =>
                            text.fold(Right(BigInt(0)))(SmtValues.integer).map(Constant.Integer(_))
                        case Some(Variable.Boolean) =>
                            text.fold(Right(false))(SmtValues.boolean).map(Constant.Bool(_))
                        case Some(Variable.ByteString) =>
                            text.fold(Right(ByteString.empty))(SmtValues.bytes(_, renamed))
                                .map(Constant.ByteString(_))
                        case Some(Variable.String) =>
                            text.fold(Right(""))(SmtValues.string(_, renamed))
                                .map(Constant.String(_))
                        case Some(Variable.WholeData) =>
                            text.fold(Right(Data.I(0)))(SmtValues.data(_, renamed))
                                .map(Constant.Data(_))
                        // A value Lean leaves out is any value, and one of the type is taken.
                        case Some(Variable.EncodedData) =>
                            val example = variableEncoding(binder.tp).example
                            text.fold(Right(example.getOrElse(Data.I(0))))(
                              SmtValues.data(_, renamed)
                            ).map(Constant.Data(_))
                        case Some(Variable.DataList) =>
                            text.fold(Right(Nil))(SmtValues.dataList(_, renamed)).map(dataList)
                        case Some(Variable.PairList) =>
                            text.fold(Right(Nil))(SmtValues.dataPairs(_, renamed)).map(pairList)
                        case Some(Variable.DataMap) =>
                            text.fold(Right(Nil))(SmtValues.dataPairs(_, renamed))
                                .map(pairs => Constant.Data(Data.Map(PList.from(pairs))))
                        case None =>
                            Left(
                              SmtValues.Unreadable.Malformed(
                                s"a ${binder.tp.show} binder has no counterexample value"
                              )
                            )
                    // A list's or a map's items are any `Data` to Lean, and need not be the type's.
                    val typed = value.flatMap(v =>
                        outsideType(v, binder.tp)
                            .map(reason => SmtValues.Unreadable.OutsideType(reason))
                            .toLeft(v)
                    )
                    // The variable the value is of, in the reason.
                    val named = typed.left.map {
                        case SmtValues.Unreadable.Malformed(reason) =>
                            SmtValues.Unreadable.Malformed(s"${binder.name}: $reason")
                        case SmtValues.Unreadable.OutsideType(reason) =>
                            SmtValues.Unreadable.OutsideType(s"${binder.name}: $reason")
                    }
                    for v <- named; r <- rest yield v :: r
            }
        read.map(_ -> renamed)
    }

    /** Reads each character of a counterexample that is none of its value's type as one that is
      * used nowhere: neither in the counterexample nor in a constant of the programs (`used`).
      * Characters that are equal stay equal, and characters that differ, among the values and from
      * the constants, stay different: that is what appending and comparing for equality see.
      *
      * A character above 255 in a byte string is read as one of the highest unused bytes, in the
      * order of the characters: as the solver's character is above every byte, the byte is above
      * those in use, where they are all below it, so ordering byte strings sees the same too. A
      * byte string has 256 characters: where fewer are unused than there are to rename, none is
      * renamed, and the value stays outside its type. A surrogate in a string is read as an unused
      * character from U+E000 on; no builtin orders strings.
      *
      * The replay decides what the values refute.
      */
    private def renaming(used: SmtValues.Characters): SmtValues.Renaming = {
        val bytes = used.bytes.filter(_ > 255).toList.sorted
        val unusedBytes = (0 until 256).filterNot(used.bytes)
        val surrogates = used.text.filter(SmtValues.isSurrogate).toList.sorted
        val unusedCharacters = Iterator.range(0xe000, 0x110000).filterNot(used.text)
        SmtValues.Renaming(
          if unusedBytes.sizeIs < bytes.size then Map.empty
          else bytes.zip(unusedBytes.takeRight(bytes.size)).toMap,
          surrogates.zip(unusedCharacters.take(surrogates.size).toList).toMap
        )
    }

    /** The characters of the byte strings and strings that `goal`'s programs hold as constants. */
    private def constantCharacters(goal: Lowered): SmtValues.Characters = {
        import SmtValues.Characters
        def all(parts: Iterable[Characters]): Characters = parts.foldLeft(Characters.none)(_ ++ _)
        def bytes(value: ByteString): Characters =
            Characters(value.bytes.map(_ & 0xff).toSet, Set.empty)
        def data(value: Data): Characters = value match
            case Data.B(string)         => bytes(string)
            case Data.I(_)              => Characters.none
            case Data.Constr(_, fields) => all(fields.toScalaList.map(data))
            case Data.List(items)       => all(items.toScalaList.map(data))
            case Data.Map(entries) =>
                all(entries.toScalaList.flatMap((key, item) => List(data(key), data(item))))
        def constant(value: Constant): Characters = value match
            case Constant.ByteString(string) => bytes(string)
            case Constant.String(string) =>
                Characters(Set.empty, string.codePoints().toArray.toSet)
            case Constant.Data(tree)          => data(tree)
            case Constant.List(_, items)      => all(items.map(constant))
            case Constant.Array(_, items)     => all(items.map(constant))
            case Constant.Pair(first, second) => constant(first) ++ constant(second)
            case _                            => Characters.none
        def term(value: Term): Characters = value match
            case Term.Const(held, _)      => constant(held)
            case Term.LamAbs(_, body, _)  => term(body)
            case Term.Force(body, _)      => term(body)
            case Term.Delay(body, _)      => term(body)
            case Term.Apply(f, arg, _)    => term(f) ++ term(arg)
            case Term.Constr(_, args, _)  => all(args.map(term))
            case Term.Case(arg, cases, _) => all((arg :: cases).map(term))
            case _                        => Characters.none
        all(goal.leaves.map(program => term(program.term)))
    }

    private def display(value: Constant): String = value match
        case Constant.Integer(integer) => integer.toString
        case Constant.Bool(boolean)    => boolean.toString
        case Constant.String(string)   => quoted(string)
        case Constant.Data(data)       => data.toString
        case other                     => other.toString

    /** A string as a Scala literal, with a quote, a backslash and a control character escaped. */
    private def quoted(value: String): String = {
        val escaped = value.flatMap {
            case '\\'                         => "\\\\"
            case '"'                          => "\\\""
            case control if control.isControl => f"\\u${control.toInt}%04x"
            case other                        => other.toString
        }
        s"\"$escaped\""
    }

    private def evaluate(program: Program, values: List[Constant]): Outcome = {
        val applied = values.foldLeft(program)((applied, value) => applied $ Term.Const(value))
        // A machine error is the predicate's result here: the test failed.
        try
            Outcome.Returned(
              replayVm.evaluateDeBruijnedTerm(
                DeBruijn.deBruijnTerm(applied.term),
                RestrictingBudgetSpender(replayBudget),
                NoLogger
              )
            )
        catch
            case _: OutOfExBudgetError => Outcome.Exhausted
            case _: MachineError       => Outcome.Failed
    }

    /** The truth of a formula under the semantics of §3.3, or `None` when a predicate it depends on
      * did not finish within the replay budget.
      */
    private def holds(formula: LeafFormula, outcomes: Vector[Outcome]): Option[Boolean] =
        formula match {
            case LeafFormula.Test(leaf) =>
                outcomes(leaf) match
                    case Outcome.Returned(Term.Const(Constant.Bool(value), _)) => Some(value)
                    case Outcome.Returned(_)                                   => Some(false)
                    case Outcome.Failed                                        => Some(false)
                    case Outcome.Exhausted                                     => None
            case LeafFormula.Denotes(leaf) =>
                outcomes(leaf) match
                    case Outcome.Returned(_) => Some(true)
                    case Outcome.Failed      => Some(false)
                    case Outcome.Exhausted   => None
            case LeafFormula.And(left, right) =>
                (holds(left, outcomes), holds(right, outcomes)) match
                    case (Some(false), _) | (_, Some(false)) => Some(false)
                    case (Some(true), Some(true))            => Some(true)
                    case _                                   => None
            case LeafFormula.Or(left, right) =>
                (holds(left, outcomes), holds(right, outcomes)) match
                    case (Some(true), _) | (_, Some(true)) => Some(true)
                    case (Some(false), Some(false))        => Some(false)
                    case _                                 => None
            case LeafFormula.Not(inner) => holds(inner, outcomes).map(!_)
            case LeafFormula.Implies(premise, conclusion) =>
                holds(LeafFormula.Or(LeafFormula.Not(premise), conclusion), outcomes)
            // Replay supplies one concrete value for every binder, and evaluates the quantified
            // body at that assignment. A statement that asks for a witness never reaches replay
            // (`Lowered.hasExistential`), so every quantifier here ranges over all values: a
            // `forAll` in a positive position, an `exists` in a negative one.
            case LeafFormula.Forall(_, body) => holds(body, outcomes)
            case LeafFormula.Exists(_, body) => holds(body, outcomes)
        }

    /** Lean's output, without the lines that report each imported program, and shortened. */
    private def concise(output: String): String = {
        val normalized = output.linesIterator
            .map(_.trim)
            .filter(line => line.nonEmpty && !line.startsWith("Successfully decoded"))
            .mkString(" ")
        if normalized.length <= 500 then normalized else normalized.take(497) + "..."
    }

    /** `value` as the text of a Lean string literal, between its quotes: a value of a variable, or
      * the path of a program. Lean reads `\xHH` and `\uHHHH` for a character by its code, and has
      * no escape for one above `0xFFFF`: such a character is written as it is. A surrogate is no
      * character: Lean would read its escape as another one.
      */
    private def leanString(value: String): String = {
        value
            .codePoints()
            .toArray
            .map {
                case '\\'                                               => "\\\\"
                case '"'                                                => "\\\""
                case printable if printable >= 0x20 && printable < 0x7f => printable.toChar.toString
                case surrogate if SmtValues.isSurrogate(surrogate) =>
                    throw new IllegalArgumentException(
                      s"the string has the surrogate U+${surrogate.toHexString}, no character"
                    )
                case byte if byte <= 0xff     => f"\\x$byte%02x"
                case basic if basic <= 0xffff => f"\\u$basic%04x"
                case other                    => Character.toString(other)
            }
            .mkString
    }

    private def leanType(tp: SIRType): String =
        variable(tp).leanType

    /** The kind of Lean variable of a binder that [[expand]] left, which has one. */
    private def variable(tp: SIRType): Variable =
        variableOf(tp).getOrElse(throw new IllegalStateException(s"unexpected ${tp.show} binder"))

    /** Renders a formula with each leaf read by its polarity (design doc §6.2).
      *
      * Each predicate runs with `ScalusProofs.Run.runFor`, where `State.Error` means that the
      * program failed and a run that exhausts the budget ends in a state that has neither halted
      * nor failed. In a positive position a test must halt within the budget with `true`. In a
      * negative position it must neither halt with `false` nor fail within the budget, so a run
      * that exhausts the budget counts as possibly `true`. `denotes` is read likewise: it halts
      * within the budget, and in a negative position it does not fail within it. Strengthening
      * positive and weakening negative occurrences gives a proposition that implies the statement.
      *
      * `state` is the Lean expression for the final state of a leaf's run. Every reading is a
      * `Bool` comparison, so Blaster can translate it and `native_decide` can evaluate it.
      */
    private def renderFormula(
        formula: LeafFormula,
        state: Int => String,
        binder: Int => String,
        guardOf: Int => Option[String],
        positive: Boolean,
        within: Boolean
    ): String = {
        def render(inner: LeafFormula, positive: Boolean) =
            renderFormula(inner, state, binder, guardOf, positive, within)
        // A variable whose Lean type has more values than its own is restricted to its type's by a
        // premise: `∀ x, p x = true → body`, and so an `exists`, `¬∀¬`, to `∃ x, p x ∧ body`.
        def quantified(symbol: String, binders: List[Int], body: LeafFormula): String =
            if binders.isEmpty then render(body, positive)
            else
                val inner = render(body, positive)
                val guards = binders.flatMap(guardOf)
                val guarded =
                    if guards.isEmpty then inner else s"(${guards.mkString(" ∧ ")} → $inner)"
                s"($symbol ${binders.map(binder).mkString(" ")}, $guarded)"
        formula match
            // Within the budget ([[Budget.Within]]) a run the budget cuts, which neither halted
            // nor failed, is read in the statement's favour: as true where the statement claims
            // the test, and as false where it assumes it.
            case LeafFormula.Test(leaf) =>
                val run = state(leaf)
                if within then
                    if positive then
                        s"(fromFrameToBool $run = some true ∨ (halted $run = false ∧ failed $run = false))"
                    else s"(halted $run = true ∧ fromFrameToBool $run ≠ some false)"
                else if positive then s"(fromFrameToBool $run = some true)"
                else s"(fromFrameToBool $run ≠ some false ∧ failed $run = false)"
            case LeafFormula.Denotes(leaf) =>
                val run = state(leaf)
                if positive != within then s"(halted $run = true)"
                else s"(failed $run = false)"
            case LeafFormula.And(left, right) =>
                s"(${render(left, positive)} ∧ ${render(right, positive)})"
            case LeafFormula.Or(left, right) =>
                s"(${render(left, positive)} ∨ ${render(right, positive)})"
            case LeafFormula.Not(inner) => s"(¬ ${render(inner, !positive)})"
            case LeafFormula.Implies(premise, conclusion) =>
                s"(${render(premise, !positive)} → ${render(conclusion, positive)})"
            case LeafFormula.Forall(binders, body) => quantified("∀", binders, body)
            // Blaster accepts Exists directly, but the equivalent ¬∀¬ form keeps generated
            // quantifiers uniform and lets the ordinary universal path handle their binders.
            case LeafFormula.Exists(binders, body) =>
                render(
                  LeafFormula.Not(LeafFormula.Forall(binders, LeafFormula.Not(body))),
                  positive
                )
    }

    /** The Lean check of a lowered statement. A statement with quantified variables is proved by
      * Blaster over `#prep_uplc_run`. A closed statement has nothing to search for: its predicates
      * run on the computable `runProgramFor`, and `native_decide` evaluates the proposition.
      */
    private def renderCheck(
        goal: Lowered,
        flats: Vector[Path],
        budget: Int,
        maxHeartbeats: Int
    ): String = document(flats, checkBody(goal, budget), maxHeartbeats)

    /** The commands of a check after its programs: the symbolic runs and the statement, or the
      * closed statement.
      */
    private def checkBody(goal: Lowered, budget: Int): String = {
        val check =
            if goal.binders.isEmpty then
                val state = (leaf: Int) => s"(runProgramFor leaf$leaf.script [] $budget)"
                s"example : ${renderFormula(goal.body, state, _ => "", _ => None, positive = true, goal.within.isDefined)} := by native_decide"
            else
                val names = goal.binders.indices.map(index => s"x$index").toList
                val declarations = goal.binders.zip(names).map { (binder, name) =>
                    s"($name : ${leanType(binder.tp)})"
                }
                val arguments = goal.leafBinders.zipWithIndex.map { (scope, leaf) =>
                    val parameters = scope.map(declarations)
                    val terms = scope.map { index =>
                        val binder = goal.binders(index)
                        variable(binder.tp).argument(names(index))
                    }
                    ((s"def arguments$leaf" +: parameters) :+
                        s": List Term := [${terms.mkString(", ")}]").mkString(" ")
                }
                val prepared = goal.leaves.indices.map(index =>
                    s"#prep_uplc_run prepared$index leaf$index arguments$index $budget"
                )
                val state = (leaf: Int) => {
                    val arguments = goal.leafBinders(leaf).map(index => " " + names(index)).mkString
                    s"(prepared$leaf$arguments)"
                }
                val declaration = (index: Int) => declarations(index)
                val premise = (index: Int) =>
                    goal.predicates.lift(index).flatten.map(p => s"($p ${names(index)} = true)")
                val formula =
                    renderFormula(
                      goal.body,
                      state,
                      declaration,
                      premise,
                      positive = true,
                      goal.within.isDefined
                    )
                s"""${arguments.mkString("\n")}
                   |
                   |${prepared.mkString("\n")}
                   |
                   |#blaster (gen-cex: 1) [$formula]""".stripMargin
        // The definitions a check uses where a variable is restricted to its type's values. A
        // check without such a variable has none of them in its text.
        if goal.predicates.exists(_.isDefined) then s"${DataEncoding.leanDefinitions}\n\n$check"
        else check
    }

    /** What the result of a check of `goal` rests on (docs/design/verification-overview.md §5.6):
      * the text of the check, without its budget, its limit of Lean's work and the places of its
      * programs, each program, and Lean's side as `workspace` has it.
      *
      * The budget is not part of it: a proof at any budget holds without the budget, and a
      * refutation is replayed without one. A check within a budget ([[Budget.Within]]) is the
      * exception: its proof is of the inputs its budget reaches, and is none at another budget, so
      * its text has its budget.
      */
    private[uplcblaster] def fingerprint(goal: Lowered, workspace: Path): Fingerprint =
        Fingerprint(
          LeanWorkspace.environment(workspace),
          Fingerprint.hash(document(Vector.empty, checkBody(goal, goal.within.getOrElse(0)), 0)),
          goal.leaves.toList.map(program => Fingerprint.hash(program.cborEncoded))
        )

    /** The notes the tactic keeps with a result: the budget the check ran with, which a later
      * search starts at, and how the statement was decided.
      */
    private[uplcblaster] val budgetNote = "budget"
    private val kindNote = "kind"
    private val readNote = "read"

    /** What is kept of a result: a proof, or a refutation with the values of its counterexample.
      */
    private[uplcblaster] def kept(goal: Lowered, result: ExecutionResult): Option[Kept] = {
        def notes(artifact: Artifact) =
            Map(budgetNote -> artifact.budget.toString, kindNote -> artifact.kind.toString) ++
                Option.when(artifact.read.nonEmpty)(readNote -> artifact.read)
        result match
            case VerificationResult.Proven(proof) =>
                proof.artifact match
                    case artifact: Artifact =>
                        Some(Kept(Kept.Result.Proved, notes(artifact), Map.empty))
                    case _ => None
            case VerificationResult.ProvenWithin(proof, _) =>
                proof.artifact match
                    case artifact: Artifact =>
                        Some(Kept(Kept.Result.ProvedWithin, notes(artifact), Map.empty))
                    case _ => None
            case VerificationResult.Refuted(proof) =>
                proof.artifact match
                    case artifact: Artifact =>
                        val values = artifact.counterexample.map((_, value) => json(value))
                        val named = keptNames(goal.binders).zip(values).toMap
                        Some(Kept(Kept.Result.Refuted, notes(artifact), named))
                    case _ => None
            case _ => None
    }

    /** The result that `kept` stands for, for the statement `goal`, or why it no longer stands. A
      * proof is taken as it was kept. A refutation is replayed on the Scalus CEK, as one that Lean
      * has just given, and is a result only where the statement is false on its values.
      */
    private[uplcblaster] def restored(goal: Lowered, kept: Kept): Either[String, ExecutionResult] =
        for
            budget <- kept.notes
                .get(budgetNote)
                .flatMap(_.toIntOption)
                .toRight(s"its budget note, ${kept.notes.get(budgetNote)}, is no number of steps")
            kind <- kept.notes
                .get(kindNote)
                .flatMap(word => ProofKind.values.find(_.toString == word))
                .toRight(s"its kind note, ${kept.notes.get(kindNote)}, is no kind of proof")
            artifact = Artifact(
              goal.programHashes,
              budget,
              "",
              Nil,
              kept.notes.getOrElse(readNote, ""),
              kind,
              kept = true
            )
            result <- restored(goal, kept, artifact)
        yield result

    private def restored(
        goal: Lowered,
        kept: Kept,
        artifact: Artifact
    ): Either[String, ExecutionResult] =
        kept.result match
            // A check within a budget proves less, and its text, which has its budget, is
            // another ([[fingerprint]]): a result kept for one is not looked up for the other.
            // A proof of one is none of the other, wherever a result comes from.
            case Kept.Result.Proved if goal.within.isDefined =>
                Left("it was proved without a budget, and is checked within one")
            case Kept.Result.Proved => Right(VerificationResult.Proven(Proof(artifact)))
            case Kept.Result.ProvedWithin if goal.within.isDefined =>
                Right(
                  VerificationResult.ProvenWithin(Proof(artifact), withinBound(artifact.budget))
                )
            case Kept.Result.ProvedWithin =>
                Left("it was proved within a budget, and is checked without one")
            // As where Lean falsifies such a statement ([[probed]]), one assignment refutes nothing.
            case Kept.Result.Refuted if goal.hasExistential =>
                Left("one counterexample refutes no statement that asks for a witness")
            case Kept.Result.Refuted =>
                val read = keptNames(goal.binders)
                    .zip(goal.binders)
                    .foldRight[Either[String, List[Constant]]](Right(Nil)) {
                        case ((name, binder), rest) =>
                            for
                                json <- kept.counterexample.get(name).toRight(s"it keeps no $name")
                                value <- constant(binder.tp, json).left
                                    .map(why => s"its value of $name, $json, is $why")
                                others <- rest
                            yield value :: others
                    }
                read.flatMap { values =>
                    replayValues(goal, artifact, values, SmtValues.Renaming.none) match
                        case Probe.Decided(refuted: VerificationResult.Refuted) => Right(refuted)
                        case Probe.Decided(VerificationResult.Inconclusive(reason)) => Left(reason)
                        case Probe.Spurious(_, _) =>
                            Left("the statement holds on its counterexample")
                        case other => Left(s"its counterexample gives $other")
                }

    /** The names a counterexample's values are kept under: those of the variables as their
      * statement has them, without the number the compiler made each unique with, which is another
      * after every compilation ([[PropIdentity.written]]). Two variables of one name are told apart
      * by their places.
      */
    private[uplcblaster] def keptNames(binders: List[PropExpr.Ident[?]]): List[String] = {
        val written = binders.map(binder => PropIdentity.written(binder.name))
        written.zipWithIndex.map { (name, index) =>
            if written.count(_ == name) > 1 then s"$name#${index + 1}" else name
        }
    }

    private given JsonValueCodec[String] = JsonCodecMaker.make

    /** A value of a variable as JSON, for a file a reader opens: a number, `true` or `false`, the
      * bytes in hexadecimal, a string as a JSON string, and `Data` as Plutus writes it in JSON.
      */
    private def json(value: Constant): String = value match
        case Constant.Integer(number)    => number.toString
        case Constant.Bool(truth)        => truth.toString
        case Constant.ByteString(string) => s"\"${string.toHex}\""
        case Constant.String(string)     => writeToString(string)
        case Constant.Data(tree)         => Data.toJson(tree)
        // A list is written as the `Data` list or map of its items, as Plutus writes those.
        case Constant.List(DefaultUni.Data, items) =>
            Data.toJson(Data.List(PList.from(items.map {
                case Constant.Data(tree) => tree
                case other =>
                    throw new IllegalArgumentException(s"a list of Data has the item $other")
            })))
        case Constant.List(element, items) if element == DataPair =>
            Data.toJson(Data.Map(PList.from(items.map {
                case Constant.Pair(Constant.Data(key), Constant.Data(value)) => key -> value
                case other =>
                    throw new IllegalArgumentException(s"a list of pairs has the item $other")
            })))
        case other =>
            throw new IllegalArgumentException(s"no variable of a statement has the value $other")

    /** The value of a variable of type `tp` that [[json]] wrote, or what is wrong with `json` where
      * it is no value [[json]] writes for the type, as in a file that was edited: what is kept then
      * no longer stands ([[scalus.verify.Tactic.restore]]). `json` is a JSON value, as the file was
      * read as JSON.
      */
    private def constant(tp: SIRType, json: String): Either[String, Constant] = {
        val value = json.trim
        // The first line of a codec's message: it goes on with a dump of what it read.
        def told(error: Exception): String = error.getMessage.linesIterator.nextOption().mkString
        tp match
            case SIRType.Integer =>
                Either.cond(
                  integerJson.matches(value),
                  Constant.Integer(BigInt(value)),
                  "no integer"
                )
            case SIRType.Boolean =>
                value match
                    case "true"  => Right(Constant.Bool(true))
                    case "false" => Right(Constant.Bool(false))
                    case _       => Left("no Boolean")
            case SIRType.ByteString =>
                Either.cond(
                  hexJson.matches(value),
                  Constant.ByteString(
                    ByteString.fromHex(value.stripPrefix("\"").stripSuffix("\""))
                  ),
                  "no byte string in hexadecimal"
                )
            // No reader of a JSON string or of `Data` says without throwing whether a value is
            // one: the codec's exception for one that is not is the answer here.
            case SIRType.String =>
                try Right(Constant.String(readFromString[String](value)))
                catch
                    case error: JsonReaderException => Left(s"no string: ${told(error)}")
            // `Data` is a JSON object; its codec reads `null` as a null value.
            case _ if !value.startsWith("{") => Left("no Data value, which is a JSON object")
            case _ =>
                val read =
                    try Right(Data.fromJson(value))
                    catch
                        case error: JsonReaderException => Left(s"no Data value: ${told(error)}")
                        case error: IllegalArgumentException =>
                            Left(s"no Data value: ${told(error)}")
                val restored = read.flatMap(tree =>
                    (variableOf(tp), tree) match
                        case (Some(Variable.DataList), Data.List(items)) =>
                            Right(dataList(items.toScalaList))
                        case (Some(Variable.DataList), _) => Left("no Data list")
                        case (Some(Variable.PairList), Data.Map(entries)) =>
                            Right(pairList(entries.toScalaList))
                        case (Some(Variable.PairList), _)          => Left("no Data map")
                        case (Some(Variable.DataMap), Data.Map(_)) => Right(Constant.Data(tree))
                        case (Some(Variable.DataMap), _)           => Left("no Data map")
                        case _                                     => Right(Constant.Data(tree))
                )
                // A list's or a map's value is checked against its type, as a counterexample is.
                restored.flatMap(value =>
                    outsideType(value, tp)
                        .map(reason => s"no value of its type: $reason")
                        .toLeft(value)
                )
    }

    private val integerJson = """-?\d+""".r
    private val hexJson = """"([0-9a-fA-F]{2})*"""".r

    /** The text of a check: the library, the limit of Lean's work, the programs in `flats` as
      * `leaf<n>`, and the commands of `body`. The lines up to the programs are the same in every
      * check, so the server elaborates them once.
      */
    private def document(flats: Vector[Path], body: String, maxHeartbeats: Int): String = {
        val imports = flats.zipWithIndex.map { (flat, index) =>
            val path = leanString(flat.toAbsolutePath.toString)
            s"""#import_uplc leaf$index PlutusV3 single_cbor_hex "$path""""
        }
        s"""import ScalusProofs.Run
           |
           |set_option maxHeartbeats $maxHeartbeats
           |
           |namespace ScalusProofs.Runtime
           |
           |open PlutusCore.Integer (Integer)
           |open PlutusCore.ByteString (ByteString)
           |open PlutusCore.UPLC
           |open PlutusCore.Data (Data)
           |open PlutusCore.UPLC.Term
           |open PlutusCore.UPLC.Utils
           |open ScalusProofs.Run
           |
           |${imports.mkString("\n")}
           |
           |$body
           |
           |end ScalusProofs.Runtime
           |""".stripMargin
    }
}
