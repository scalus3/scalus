package scalus.verify

import scalus.compiler.sir.SIR
import scalus.compiler.sir.transform.EraseSpecifications

/** A named proposition declared in a [[Verifier]], where it comes from, and what checking it is
  * expected to give: a proof, or for a negative control ([[Verifier.refute]]) a refutation.
  *
  * A statement declared without a name is named by its content, `#` and a hash of it
  * (docs/design/verification-overview.md §4.4). A name that begins with `#` is no other's.
  */
final class Statement private[verify] (
    val name: String,
    val prop: Prop,
    val origin: Origin,
    val expected: ExpectedOutcome
) {

    /** Whether the statement is named by its content. */
    def byContent: Boolean = Statement.contentName.matches(name)

    override def toString: String = s"Statement($name)"
}

object Statement {

    /** The name of a statement named by its content: `#` and 16 hexadecimal digits. */
    private[verify] val contentName = "#[0-9a-f]{16}".r
}

/** Where a statement comes from. */
enum Origin {

    /** Declared with [[Verifier.statement]]. */
    case Explicit

    /** A function's contract, declared with [[Verifier.contract]]. */
    case Contract(contract: FunctionContract[?, ?])

    /** What a call of `callee` in the code of `caller`, at `line` of its source file, owes to the
      * callee's contract named `contract`: its arguments satisfy the precondition. Declared with
      * [[Verifier.obligations]].
      */
    case Obligation(
        caller: FunctionRef[?, ?],
        callee: FunctionRef[?, ?],
        contract: String,
        line: Int
    )

    /** What an `ensures` or `ensuring` clause in the code of `function`, at `line` of its source
      * file, states: where the function returns through the clause, its condition holds. Declared
      * with [[Verifier.guarantees]].
      */
    case Guarantee(function: FunctionRef[?, ?], line: Int)

    /** What the clauses in the code of `function`, at `lines` of its source file, state together:
      * where the function returns, the condition of each holds. Declared with
      * [[Verifier.guarantees]], next to the statement of each clause.
      */
    case Guarantees(function: FunctionRef[?, ?], lines: List[Int])
}

/** The obligations of a function's calls ([[Verifier.obligations]]): one statement per call and
  * contract, and the calls no statement could be made for, each with the reason. The function owes
  * those too; they are listed, not dropped.
  */
final case class CallObligations(statements: List[Statement], unsupported: List[String])

/** The guarantees stated in a function's code ([[Verifier.guarantees]]): one statement per
  * `ensures` or `ensuring` clause, and the clauses no statement could be made for, each with the
  * reason.
  *
  * `together` is the statements as one: where the function returns, the condition of every clause
  * holds. Each statement says that the function returns, so proving them one by one runs its body
  * once per clause, and proving them together once. It is the statement itself where there is one,
  * and `None` where there is none.
  */
final case class StatedGuarantees(
    statements: List[Statement],
    unsupported: List[String],
    together: Option[Statement]
)

/** Which proof mechanism checked an artifact. */
enum ProofKind {

    /** Closed by Blaster through Z3, which Lean admits with the axiom `blasterProven`. */
    case Blaster

    /** Checked by the Lean kernel, with no axiom beyond Lean's own. */
    case LeanKernel

    /** Decided by evaluation with `native_decide`, which trusts the Lean compiler through the axiom
      * `Lean.ofReduceBool`.
      */
    case LeanNative
}

/** Backend-specific proof material retained with a theorem.
  *
  * A concrete tactic supplies an implementation containing its actual proof term, certificate, or
  * check record. The kind describes how that material was checked; it is not the material itself.
  */
trait ProofArtifact {
    def kind: ProofKind
}

/** Evidence supplied by a proof tactic, including its artifact and the lemmas it used. */
final class Proof private[verify] (
    val artifact: ProofArtifact,
    val usedLemmas: List[Theorem]
) {
    def kinds: Set[ProofKind] = Set(artifact.kind) ++ usedLemmas.flatMap(_.proof.kinds)
}

object Proof {
    private[verify] def apply(artifact: ProofArtifact): Proof = new Proof(artifact, Nil)

    private[verify] def apply(
        artifact: ProofArtifact,
        usedLemmas: List[Theorem]
    ): Proof = new Proof(artifact, usedLemmas)
}

/** A statement paired with a proof. It can be supplied as a lemma for later goals. */
final class Theorem private[verify] (val statement: Statement, val proof: Proof)

/** The context passed to a tactic. Lemmas are the theorems currently available in the verifier. */
final case class Goal(
    statement: Statement,
    functions: FunctionTable,
    lemmas: List[Theorem]
)

trait Tactic {
    type Prepared

    def name: String
    def prepare(goal: Goal): Either[CompatibilityReport, Prepared]

    /** Runs the tactic on `prepared`, starting from `earlier`: what was kept of an earlier result,
      * where results are kept and there is one, as a budget that was found. It was kept for the
      * same fingerprint, or for the same statement and programs with another side of the backend
      * ([[KeptResults.keptOnAnotherSide]]), so it is a start, and stands for no result. A tactic
      * that has no use for it ignores it.
      */
    def run(prepared: Prepared, earlier: Option[Kept] = None): ExecutionResult

    /** The hashes of everything a result for `prepared` rests on, part by part. Nothing where the
      * results of the tactic are not kept ([[KeptResults]]). It is read without the backend.
      */
    def fingerprint(prepared: Prepared): Option[Fingerprint] = None

    /** What is kept of `result`, where it is one that is kept: a proof or a refutation. */
    def keep(prepared: Prepared, result: ExecutionResult): Option[Kept] = None

    /** The result that `kept` stands for, made without the backend, or why it no longer stands. A
      * refutation is replayed here, so it is no more than the tactic can check again, and it is
      * also asked of a refutation kept with another side of the backend
      * ([[KeptResults.keptOnAnotherSide]]). A proof is the word of the run that kept it.
      */
    def restore(prepared: Prepared, kept: Kept): Either[String, ExecutionResult] =
        Left(s"$name takes no result that was kept")
}

/** One reason a tactic cannot prepare a goal in its input language. */
enum CompatibilityIssue {

    /** The unsupported feature at `path`, with a backend-specific explanation. */
    case UnsupportedFeature(path: List[String], reason: String)
}

/** The incompatibilities reported while preparing a goal. */
final case class CompatibilityReport(issues: List[CompatibilityIssue]) {
    require(issues.nonEmpty, "a compatibility report must contain an issue")
}

/** The result of a tactic or verifier run. The verifier registers a theorem for a proven statement.
  */
enum VerificationResult {
    case Proven(proof: Proof)
    case Refuted(proof: Proof)
    case Unsupported(report: CompatibilityReport)
    case Inconclusive(reason: String)
    case Failed(reason: String)
}

/** A result produced after successful preparation. Its type excludes
  * [[VerificationResult.Unsupported]].
  */
type ExecutionResult = VerificationResult.Proven | VerificationResult.Refuted |
    VerificationResult.Inconclusive | VerificationResult.Failed

/** Opaque evidence that a tactic successfully prepared a statement in a particular verifier. */
final class PreparedRun private[verify] (
    private[verify] val verifier: Verifier,
    val statement: Statement,
    val tacticName: String,
    private[verify] val execute: () => ExecutionResult
)

/** A runtime context for functions, named statements, and proved lemmas.
  *
  * Proof and refutation artifacts carry backend-specific data, including the checked target
  * identity when needed. Tactics decide whether artifacts and imported lemmas are compatible. The
  * verifier checks registration and explicit function calls.
  */
final class Verifier private (results: Option[KeptResults]) {
    private var functionTable: FunctionTable = FunctionTable.empty
    private var declarations: Map[String, Statement] = Map.empty
    private var proven: Map[String, Theorem] = Map.empty

    def functions: FunctionTable = functionTable
    def statements: Iterable[Statement] = declarations.values
    def theorems: Iterable[Theorem] = proven.values

    def addFunction(definition: FunctionDef[?, ?]): Unit =
        functionTable = functionTable + definition

    /** Adds all entries together; a conflicting name leaves the old table intact. */
    def addFunctions(table: FunctionTable): Unit = {
        val updated = table.definitions.foldLeft(functionTable)(_ + _)
        functionTable = updated
    }

    def statement(name: String, prop: Prop): Statement = declare(name, prop, Origin.Explicit)

    /** Declares a negative control: a statement expected to be refuted, with a counterexample
      * replayed in the semantics of the code (docs/design/verification-overview.md §5.2). A control
      * shows that the statements beside it can fail, and so are not proved for a reason that has
      * nothing to do with the code. A refuted statement is no theorem.
      */
    def refute(name: String, prop: Prop): Statement =
        declare(name, prop, Origin.Explicit, ExpectedOutcome.Falsified)

    /** Declares the contract of a function, built by [[Props.contract]] or [[Props.totalContract]].
      * It is proved like any statement, and [[contracts]] finds it by its function.
      */
    def contract(name: String, contract: FunctionContract[?, ?]): Statement =
        declare(name, contract.prop, Origin.Contract(contract))

    /** The contracts declared for `function`, by name. */
    def contracts(function: FunctionRef[?, ?]): List[Statement] =
        declarations.values
            .filter(_.origin match
                case Origin.Contract(contract) => contract.function == function
                case _                         => false)
            .toList
            .sortBy(_.name)

    /** Declares what the calls in `caller`'s code owe to the contracts of the functions they call:
      * for every call of a function with a declared contract, the statement that the call's
      * arguments satisfy that contract's precondition, whenever the call is reached
      * (prop-semantics.md §6). `caller` must be in the function table, with its SIR.
      */
    def obligations(caller: FunctionRef[?, ?]): CallObligations = declareObligations(caller, None)

    /** [[obligations]] of the function `callerContract` is about, where that contract's own
      * precondition holds: the calling function may assume what its callers establish. They are
      * named after the contract, not the function, so both can be declared in one verifier.
      */
    def obligations(callerContract: Statement): CallObligations = callerContract.origin match
        case Origin.Contract(contract) =>
            declareObligations(contract.function, Some(callerContract.name -> contract))
        case _ =>
            throw new IllegalArgumentException(s"${callerContract.name} is not a contract")

    /** Declares what the `ensures` and `ensuring` clauses in `function`'s code state
      * ([[scalus.cardano.onchain.plutus.prelude.Spec]]): for each clause, the statement that its
      * condition holds wherever the function returns through it. `function` must be in the function
      * table, with its SIR.
      *
      * A clause need not be at the head of the function's body, where [[FunctionContract.inSource]]
      * reads it as part of the function's contract. It is found on any path: in a branch, or in the
      * code of an `inline` method the function calls. That is where the clauses of a validator's
      * handlers are, once `validate` is compiled: a clause on `spend` says what holds of every
      * spend the script accepts.
      *
      * The statement is, over the function's parameters, `denotes(body) ==> Prop(check)`: `check`
      * is the body cut along the path to the clause (as for [[obligations]]), with the clause's
      * condition in its place, about the value the clause is applied to.
      *
      * It is about the function alone, on every argument. The function's `Spec.expects` is no
      * check, so a clause that relies on it does not hold this way: declare it with
      * `guarantees(contract)`.
      *
      * Several clauses are also declared as one statement, [[StatedGuarantees.together]], named
      * after the function alone, `function/ensures`: a tactic then runs the function's body once
      * for all of them.
      */
    def guarantees(function: FunctionRef[?, ?]): StatedGuarantees =
        declareGuarantees(function, None)

    /** [[guarantees]] of the function `contract` is about, where that contract's own precondition
      * holds: a clause may rely on what the function's callers establish. They are named after the
      * contract, not the function, so both can be declared in one verifier.
      */
    def guarantees(contract: Statement): StatedGuarantees = contract.origin match
        case Origin.Contract(stated) =>
            declareGuarantees(stated.function, Some(contract.name -> stated))
        case _ =>
            throw new IllegalArgumentException(s"${contract.name} is not a contract")

    /** The precondition of `contract` over the parameters of the function it is about. */
    private def assumption(contract: FunctionContract[?, ?], parameters: List[SIR.Var]): Prop =
        Props.renameVariables(
          contract.expects,
          contract.variables.map(_.name).zip(parameters.map(_.name)).toMap
        )

    private def declareGuarantees(
        function: FunctionRef[?, ?],
        premise: Option[(String, FunctionContract[?, ?])]
    ): StatedGuarantees = {
        val definition = functionTable(function)
        val body = Obligations.body(definition, definition(Representation.Sir))
        val owner = premise.fold(function.displayName)(_._1)
        val assumed = premise.map((_, contract) => assumption(contract, body.parameters))
        // Where the function returns, `conclusion` holds, over its parameters.
        def stated(conclusion: Prop): Prop = {
            val returned = Prop.Implies(
              Prop.Denotes(PropExpr.SIRExpr[Any](body.wrapped(body.term))),
              conclusion
            )
            val guarded = assumed.fold(returned)(Prop.Implies(_, returned))
            body.variables.foldRight[Prop](guarded)(Prop.Forall(_, _))
        }
        val statements = List.newBuilder[Statement]
        val unsupported = List.newBuilder[String]
        // The condition of each clause that has a statement, where the clause is, and its line.
        val conditions = List.newBuilder[(Prop, Int)]
        val clauses = Obligations.sites(
          body.term,
          Map(Obligations.Ensuring -> 2, Obligations.Ensures -> 1)
        )
        clauses.zipWithIndex.foreach { case ((site, inLambda), index) =>
            val line = Obligations.line(site)
            val where = s"the clause of ${function.displayName} at line $line"
            // The condition in the clause's place: about the value `ensuring` is applied to, or
            // about nothing but the variables in scope, for `ensures`.
            val atClause = site.arguments match
                case List(value, SIR.LamAbs(result, condition, Nil, _)) =>
                    Some(Obligations.bind(result, value, condition))
                case List(SIR.LamAbs(unit, condition, Nil, _)) =>
                    Some(Obligations.bind(unit, Obligations.unit, condition))
                case _ => None
            (atClause, inLambda) match
                case (_, true) =>
                    unsupported += s"$where is inside a function value, which is not supported"
                case (Some(condition), false) =>
                    Obligations.slice(body.term, site.node, condition) match
                        case Some(check) =>
                            val holds = Prop.Bool(PropExpr.SIRExpr[Boolean](body.wrapped(check)))
                            conditions += holds -> line
                            statements += declare(
                              s"$owner/ensures#${index + 1}",
                              stated(holds),
                              Origin.Guarantee(function, line)
                            )
                        case None => unsupported += s"$where was not found on a path"
                case (None, false) =>
                    unsupported += s"$where: its condition must be a function literal, as in " +
                        "body.ensuring(r => ...)"
        }
        // A precondition is part of the contract where the body starts with it. One that is
        // written after another statement is erased from the script like the rest, and is in no
        // contract: no caller owes it, and nothing checks it where the script runs.
        Obligations
            .sites(body.term, Map(Obligations.Expects -> 1))
            .drop(Specifications.expected(body.term, function.displayName))
            .foreach { (site, _) =>
                unsupported += s"the precondition of ${function.displayName} at line " +
                    s"${Obligations.line(site)} is not at the start of the body: it is no part " +
                    "of the contract, and nothing checks it in the script"
            }
        val each = statements.result()
        val together = each match
            case Nil        => None
            case List(only) => Some(only)
            case _ =>
                val (all, lines) = conditions.result().unzip
                Some(
                  declare(
                    s"$owner/ensures",
                    stated(all.reduce(Prop.And(_, _))),
                    Origin.Guarantees(function, lines)
                  )
                )
        StatedGuarantees(each, unsupported.result(), together)
    }

    private def declareObligations(
        caller: FunctionRef[?, ?],
        premise: Option[(String, FunctionContract[?, ?])]
    ): CallObligations = {
        val definition = functionTable(caller)
        val specified = Obligations.body(definition, definition(Representation.Sir))
        // The calls of the function's code: a call in a specification clause is never made.
        val body = specified.copy(term = EraseSpecifications(specified.term))
        val contracts = declarations.values.toList
            .sortBy(_.name)
            .flatMap(statement =>
                statement.origin match
                    case Origin.Contract(contract) => Some(statement -> contract)
                    case _                         => None
            )
            .groupBy(_._2.function.name)
        val arities = contracts.map((name, declared) => name -> declared.head._2.variables.size)
        val parameters = body.variables
        val owner = premise.fold(caller.displayName)(_._1)
        val assumed = premise.map((_, contract) => assumption(contract, body.parameters))
        val statements = List.newBuilder[Statement]
        val unsupported = List.newBuilder[String]
        for
            ((site, inLambda), index) <- Obligations.sites(body.term, arities).zipWithIndex
            (declared, contract) <- contracts(site.callee)
        do
            val callee = contract.function
            val variables = contract.variables
            val line = Obligations.line(site)
            val where = s"${caller.displayName} calls ${callee.displayName} at line $line"
            def cut(atSite: SIR): Option[SIR] =
                Obligations
                    .slice(
                      body.term,
                      site.node,
                      Obligations.bind(variables, site.arguments, atSite)
                    )
                    .map(body.wrapped)
            (contract.expects, inLambda) match
                case (_, true) =>
                    unsupported += s"$where inside a function value, which is not supported"
                case (Prop.Bool(PropExpr.SIRExpr(expects)), false) =>
                    val obligation = for
                        reach <- cut(Obligations.truth)
                        check <- cut(expects)
                    yield Prop.Implies(
                      Prop.Denotes(PropExpr.SIRExpr[Any](reach)),
                      Prop.Bool(PropExpr.SIRExpr[Boolean](check))
                    )
                    obligation match
                        case Some(owed) =>
                            val guarded = assumed.fold(owed)(Prop.Implies(_, owed))
                            statements += declare(
                              s"$owner/${declared.name}#${index + 1}",
                              parameters.foldRight[Prop](guarded)(Prop.Forall(_, _)),
                              Origin.Obligation(caller, callee, declared.name, line)
                            )
                        case None => unsupported += s"$where: the call was not found on a path"
                case _ =>
                    unsupported += s"$where: the precondition of ${declared.name} is a " +
                        "statement, not a Boolean test, which is not supported"
        CallObligations(statements.result(), unsupported.result())
    }

    private def declare(
        name: String,
        prop: Prop,
        origin: Origin,
        expected: ExpectedOutcome = ExpectedOutcome.Valid
    ): Statement = {
        require(name.trim.nonEmpty, "a statement name must be non-empty")
        require(
          !name.startsWith("#"),
          s"a statement name that begins with # is a content's (statement(prop)), not $name"
        )
        declared(new Statement(name, prop, origin, expected))
    }

    private def declared(statement: Statement): Statement = {
        val name = statement.name
        require(!declarations.contains(name), s"a statement named $name is already declared")
        require(!proven.contains(name), s"a theorem named $name is already available")
        declarations = declarations.updated(name, statement)
        statement
    }

    /** Declares a statement named by its content ([[Statement]]): the same content is the same
      * statement, wherever it is stated, and declaring it again gives the statement declared, or
      * the one of a theorem of that content that was imported ([[addTheorem]]).
      */
    def statement(prop: Prop): Statement = byContent(prop, ExpectedOutcome.Valid)

    /** A negative control named by its content ([[refute]], [[statement]]). */
    def refute(prop: Prop): Statement = byContent(prop, ExpectedOutcome.Falsified)

    private def byContent(prop: Prop, expected: ExpectedOutcome): Statement = {
        val name = s"#${PropIdentity.of(prop)}"
        declarations.get(name).orElse(proven.get(name).map(_.statement)) match
            case Some(stated) =>
                require(
                  stated.expected == expected,
                  s"the statement $name is declared both as a statement and as a negative control"
                )
                declarations = declarations.updated(name, stated)
                stated
            case None => declared(new Statement(name, prop, Origin.Explicit, expected))
    }

    /** Imports a theorem proved in another context for use as a lemma. A statement named by its
      * content is the same wherever it is stated: its theorem is taken where a statement of that
      * content is declared here, or proved, already.
      */
    def addTheorem(theorem: Theorem): Unit = {
        val name = theorem.statement.name
        def same(statement: Statement): Boolean =
            (statement eq theorem.statement) ||
                (statement.byContent && statement.expected == theorem.statement.expected)
        require(
          declarations.get(name).forall(same),
          s"a different statement named $name is already declared"
        )
        proven.get(name) match
            case Some(existing) if !same(existing.statement) =>
                throw new IllegalArgumentException(
                  s"a different theorem named $name is already available"
                )
            case _ => proven = proven.updated(name, theorem)
    }

    /** Checks that `tactic` supports the complete goal and, if so, returns opaque evidence that can
      * be passed to [[prove]]. Function representations and available lemmas are captured now.
      */
    def prepare(statement: Statement, tactic: Tactic): Either[CompatibilityReport, PreparedRun] = {
        require(
          declarations.get(statement.name).exists(_ eq statement),
          s"statement ${statement.name} is not registered in this verifier"
        )
        // A statement is no lemma of its own, as where one that is proved is stated again.
        val lemmas = proven.values.filter(_.statement.name != statement.name)
        val goal = Goal(statement, functionTable, lemmas.toList.sortBy(_.statement.name))
        tactic.prepare(goal).map { prepared =>
            new PreparedRun(this, statement, tactic.name, () => run(tactic)(statement, prepared))
        }
    }

    /** Runs `tactic` on a prepared statement, with the results that are kept
      * (docs/design/verification-overview.md §5.6): a result that was kept for the statement as it
      * is now need not be asked of the backend again.
      */
    private def run(tactic: Tactic)(
        statement: Statement,
        prepared: tactic.Prepared
    ): ExecutionResult = {
        val name = statement.name
        results.flatMap(kept => tactic.fingerprint(prepared).map(kept -> _)) match
            case None => tactic.run(prepared)
            case Some((kept, fingerprint)) =>
                val earlier = kept.lookup(name, tactic.name, fingerprint)
                // Where nothing is kept for the statement as it is now, what was kept for it with
                // the backend's old side, as before an update of its library, is no result, but a
                // start. A refutation there is one the tactic checks again without the backend.
                lazy val former =
                    if earlier.isDefined then None
                    else kept.keptOnAnotherSide(name, tactic.name, fingerprint)
                // What the tactic takes of what is kept, or why it does not take it.
                lazy val restored: Option[Either[String, ExecutionResult]] =
                    earlier.map(tactic.restore(prepared, _))
                lazy val formerRefutation: Option[Either[String, ExecutionResult]] =
                    former.filter(_.result == Kept.Result.Refuted).map(tactic.restore(prepared, _))
                def taken: Option[ExecutionResult] =
                    restored.flatMap(_.toOption).orElse(formerRefutation.flatMap(_.toOption))
                def asked: ExecutionResult = tactic.run(prepared, earlier.orElse(former))
                def keeping(result: ExecutionResult): ExecutionResult = {
                    tactic
                        .keep(prepared, result)
                        .foreach(kept.keep(name, tactic.name, fingerprint, _))
                    result
                }
                kept.mode match
                    case KeptResults.Mode.Use =>
                        restored.flatMap(_.toOption).getOrElse {
                            formerRefutation
                                .flatMap(_.toOption)
                                .map(keeping)
                                .getOrElse(keeping(asked))
                        }
                    case KeptResults.Mode.Frozen =>
                        taken.getOrElse {
                            val why = kept.stale(name, tactic.name, fingerprint)
                            val untaken = restored.orElse(formerRefutation).flatMap(_.left.toOption)
                            VerificationResult.Inconclusive(
                              s"${KeptResults.stale}: $why" + untaken.fold("")(reason =>
                                  s": $reason"
                              )
                            )
                        }
                    case KeptResults.Mode.Recalculate =>
                        val result = asked
                        val now = tactic.keep(prepared, result)
                        earlier match
                            case Some(before) if !now.exists(_.result == before.result) =>
                                VerificationResult.Failed(
                                  s"the result kept for $name is ${before.result}, and this run " +
                                      s"gives ${Verifier.outcome(result)}"
                                )
                            // The result that is kept, with the same notes: the entry stays as
                            // it is, whichever counterexample this run came to. So a file that
                            // changes in such a run had an entry that was missing or stale.
                            case Some(before) if now.exists(_.notes == before.notes) => result
                            case _ => keeping(result)
    }

    /** Runs an already compatible tactic and registers a successful theorem for later goals. */
    def prove(prepared: PreparedRun): ExecutionResult = {
        require(prepared.verifier eq this, "the prepared run belongs to another verifier")
        prepared.execute() match
            case VerificationResult.Proven(proof) =>
                proof.usedLemmas.foreach { lemma =>
                    require(
                      proven.get(lemma.statement.name).exists(_ eq lemma),
                      s"tactic ${prepared.tacticName} used an unavailable lemma ${lemma.statement.name}"
                    )
                }
                require(
                  proof.artifact != null,
                  s"tactic ${prepared.tacticName} supplied no proof artifact"
                )
                val theorem = new Theorem(prepared.statement, proof)
                proven = proven.updated(prepared.statement.name, theorem)
                VerificationResult.Proven(proof)
            case VerificationResult.Refuted(proof) => VerificationResult.Refuted(proof)
            case inconclusive @ VerificationResult.Inconclusive(_) => inconclusive
            case failed @ VerificationResult.Failed(_)             => failed
    }

    /** Prepares and runs `tactic`. Use the two-phase API to make incompatibility impossible at the
      * execution call site.
      */
    def prove(statement: Statement, tactic: Tactic): VerificationResult =
        prepare(statement, tactic) match
            case Left(report)    => VerificationResult.Unsupported(report)
            case Right(prepared) => prove(prepared)

    /** The earlier name for [[prove]]. */
    def verify(statement: Statement, tactic: Tactic): VerificationResult = prove(statement, tactic)

}

object Verifier {
    def empty: Verifier = new Verifier(None)

    /** A verifier that keeps the results of its tactics in `results`, and uses those that are kept
      * in the way `results` says (docs/design/verification-overview.md §5.6).
      */
    def keeping(results: KeptResults): Verifier = new Verifier(Some(results))

    /** A result in a few words, for a reason. */
    private def outcome(result: ExecutionResult): String = result match
        case VerificationResult.Proven(_)            => "a proof"
        case VerificationResult.Refuted(_)           => "a refutation"
        case VerificationResult.Inconclusive(reason) => s"no result: $reason"
        case VerificationResult.Failed(reason)       => s"a failure: $reason"
}
