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

/** How a verifier's statement of a clause says the clause of where the function returns
  * ([[Verifier.guarantees]]).
  */
enum Connective {

    /** `denotes(body) ==>: clause` ([[Prop.Implies]]): wherever the function returns, the clause
      * returns `true`. A clause that fails where the function returns is false there.
      */
    case Implies

    /** `denotes(body) ==>?: clause` ([[Prop.Rejects]]): wherever the clause returns `false`, the
      * function does not return. Where the clause fails it says nothing, as where it reads a part
      * of the function's input that the function rejects, and a tactic checks it in the rejecting
      * form alone, whose runs end where the clause fails.
      */
    case Rejects
}

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

    /** The statement holds within `bound`, on the inputs it says: a proof of a weaker statement,
      * which is no theorem, and is no lemma of another. A tactic gives it where it was asked for a
      * bounded check, as UPLC Blaster within a budget (`Budget.Within`).
      */
    case ProvenWithin(proof: Proof, bound: String)
    case Refuted(proof: Proof)
    case Unsupported(report: CompatibilityReport)
    case Inconclusive(reason: String)
    case Failed(reason: String)
}

/** A result produced after successful preparation. Its type excludes
  * [[VerificationResult.Unsupported]].
  */
type ExecutionResult = VerificationResult.Proven | VerificationResult.ProvenWithin |
    VerificationResult.Refuted | VerificationResult.Inconclusive | VerificationResult.Failed

/** Opaque evidence that a tactic successfully prepared a statement in a particular verifier. */
final class PreparedRun private[verify] (
    private[verify] val verifier: Verifier,
    val statement: Statement,
    val tacticName: String,
    private[verify] val execute: () => ExecutionResult,
    /** What the tactic says the statement is ([[StatementIdentity]]), where it says. */
    private[verify] val identity: () => Option[StatementIdentity]
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

    /** The clauses of functions' code that each statement states, by the statement's name: those
      * [[guarantees]] states, and those of a contract read from the code.
      */
    private var stating: Map[String, Set[Clause]] = Map.empty

    /** The tactic that proved each theorem, by the statement's name. */
    private var provedWith: Map[String, String] = Map.empty

    /** The theorems by what their tactics say they are, so that a statement the tactic finds the
      * same as one of them is proved by it.
      */
    private var provedAs: Map[StatementIdentity, Theorem] = Map.empty

    /** The tactics that proved something, by their names: what says whether a statement is the same
      * as a theorem.
      */
    private var tactics: Map[String, Tactic] = Map.empty

    /** The statements declared not proved, with the reasons ([[open]]). */
    private var opened: Map[String, String] = Map.empty

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
    /** `contract` as a statement of its own, which is not the function's contract: no caller owes
      * its precondition. A contract read from the function's code ([[FunctionContract.inSource]]),
      * with `returnsWhen` or `failsWhen` added, checks the `ensures` and `ensuring` clauses it was
      * read from where the statement is proved ([[specifications]]).
      */
    def statement(name: String, contract: FunctionContract[?, ?]): Statement = {
        val declared = declare(name, contract.prop, Origin.Explicit)
        stating =
            stating.updated(name, contract.clauses.filter(_.kind != Clause.Kind.Expects).toSet)
        declared
    }

    def contract(name: String, contract: FunctionContract[?, ?]): Statement = {
        val declared = declare(name, contract.prop, Origin.Contract(contract))
        stating =
            stating.updated(name, contract.clauses.filter(_.kind != Clause.Kind.Expects).toSet)
        declared
    }

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
    def obligations(caller: FunctionRef[?, ?]): CallObligations =
        declareObligations(caller, None, None)

    /** [[obligations]] of `caller`, a function of one parameter, over the values of it that
      * `parameter` says ([[Props.limitedData]]).
      */
    def obligations(caller: FunctionRef[?, ?], parameter: LimitedData): CallObligations =
        declareObligations(caller, None, Some(parameter))

    /** [[obligations]] of the function `callerContract` is about, where that contract's own
      * precondition holds: the calling function may assume what its callers establish. They are
      * named after the contract, not the function, so both can be declared in one verifier.
      */
    def obligations(callerContract: Statement): CallObligations =
        obligationsOf(callerContract, None)

    /** [[obligations]] of the function `callerContract` is about, a function of one parameter, over
      * the values of it that `parameter` says ([[Props.limitedData]]).
      */
    def obligations(callerContract: Statement, parameter: LimitedData): CallObligations =
        obligationsOf(callerContract, Some(parameter))

    private def obligationsOf(
        callerContract: Statement,
        parameter: Option[LimitedData]
    ): CallObligations = callerContract.origin match
        case Origin.Contract(contract) =>
            declareObligations(
              contract.function,
              Some(callerContract.name -> contract),
              parameter
            )
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
      * The statement is, over the function's parameters, `denotes(body) ==>: Prop(check)`: `check`
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
        declareGuarantees(function, None, None)

    /** [[guarantees]] of `function`, a function of one parameter, over the values of it that
      * `parameter` says ([[Props.limitedData]]): as for a validator's entry point, over the script
      * contexts whose collections are within limits.
      */
    def guarantees(function: FunctionRef[?, ?], parameter: LimitedData): StatedGuarantees =
        declareGuarantees(function, None, Some(parameter))

    /** [[guarantees]] of `function` over the values of its parameter that `parameter` says, each
      * stated with `connective`: with [[Connective.Rejects]], a clause that fails, as one that
      * reads a datum the function does not read, says nothing where it fails.
      */
    def guarantees(
        function: FunctionRef[?, ?],
        parameter: LimitedData,
        connective: Connective
    ): StatedGuarantees =
        declareGuarantees(function, None, Some(parameter), connective)

    /** [[guarantees]] of the function `contract` is about, where that contract's own precondition
      * holds: a clause may rely on what the function's callers establish. They are named after the
      * contract, not the function, so both can be declared in one verifier.
      */
    def guarantees(contract: Statement): StatedGuarantees = guaranteesOf(contract, None)

    /** [[guarantees]] of the function `contract` is about, a function of one parameter, over the
      * values of it that `parameter` says ([[Props.limitedData]]).
      */
    def guarantees(contract: Statement, parameter: LimitedData): StatedGuarantees =
        guaranteesOf(contract, Some(parameter))

    private def guaranteesOf(
        contract: Statement,
        parameter: Option[LimitedData]
    ): StatedGuarantees = contract.origin match
        case Origin.Contract(stated) =>
            declareGuarantees(stated.function, Some(contract.name -> stated), parameter)
        case _ =>
            throw new IllegalArgumentException(s"${contract.name} is not a contract")

    /** The variables of a statement about the function of `body`: its parameters, each a variable
      * of its type, but for a function of one parameter that `parameter` says, which is a
      * [[Props.forAllDataLimited]] variable ([[Props.limitedData]]) where it has limits, and one of
      * its type, as a [[Props.forAllData]] variable is, where it has none
      * ([[Props.unlimitedData]]).
      */
    private def variablesOf(
        function: FunctionRef[?, ?],
        body: Obligations.Body,
        parameter: Option[LimitedData]
    ): List[PropExpr.Ident[Any]] = parameter match
        case None => body.variables
        case Some(limited) =>
            body.variables match
                // By what they show: the parameter's type is the function's SIR's, and `limited`'s
                // is compiled apart, so their annotations differ.
                case List(variable) if variable.tp.show == limited.tp.show =>
                    limited.decoder match
                        case Some(decoder) =>
                            List(DataEncoding.encodedVariable(variable, decoder, limited.limits))
                        case None => List(variable)
                case List(variable) =>
                    throw new IllegalArgumentException(
                      s"${function.displayName} takes a ${variable.tp.show}, and the values that " +
                          s"meet the limits are of ${limited.tp.show}"
                    )
                case variables =>
                    throw new IllegalArgumentException(
                      s"${function.displayName} takes ${variables.size} parameters: limits are of " +
                          "the one parameter of a function"
                    )

    /** The precondition of `contract` over the parameters of the function it is about. */
    private def assumption(contract: FunctionContract[?, ?], parameters: List[SIR.Var]): Prop =
        Props.renameVariables(
          contract.expects,
          contract.variables.map(_.name).zip(parameters.map(_.name)).toMap
        )

    private def declareGuarantees(
        function: FunctionRef[?, ?],
        premise: Option[(String, FunctionContract[?, ?])],
        parameter: Option[LimitedData],
        connective: Connective = Connective.Implies
    ): StatedGuarantees = {
        val definition = functionTable(function)
        val body = Obligations.body(definition, definition(Representation.Sir))
        val owner = premise.fold(function.displayName)(_._1)
        val assumed = premise.map((_, contract) => assumption(contract, body.parameters))
        // The variables, whether or not there is a clause: a parameter that does not fit is
        // refused at once.
        val variables = variablesOf(function, body, parameter)
        // Where the function returns, `conclusion` holds, over its parameters.
        def statedAs(conclusion: Prop): Prop = {
            val returns = Prop.Denotes(PropExpr.SIRExpr[Any](body.wrapped(body.term)))
            val returned = connective match
                case Connective.Implies => Prop.Implies(returns, conclusion)
                case Connective.Rejects => Prop.Rejects(returns, conclusion)
            val guarded = assumed.fold(returned)(Prop.Implies(_, returned))
            variables.foldRight[Prop](guarded)(Prop.Forall(_, _))
        }
        val statements = List.newBuilder[Statement]
        val unsupported = List.newBuilder[String]
        // The condition of each clause that has a statement, where the clause is, and its line.
        val conditions = List.newBuilder[(Prop, Int)]
        // The clauses that have a statement.
        val stated = List.newBuilder[Clause]
        val clauses = Obligations.sites(
          body.term,
          Map(Obligations.Ensuring -> 2, Obligations.Ensures -> 1)
        )
        clauses.zipWithIndex.foreach { case ((site, inLambda), index) =>
            val clause =
                Clause.at(site.node, Clause.kindOf(site.callee).getOrElse(Clause.Kind.Ensures))
            // Where the code states the clause: `Spec.ensures` is inline, and its call is at its
            // own definition ([[Clause.at]]).
            val line = clause.line
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
                            stated += clause
                            val name = s"$owner/ensures#${index + 1}"
                            statements += declare(
                              name,
                              statedAs(holds),
                              Origin.Guarantee(function, line)
                            )
                            stating = stating.updated(name, Set(clause))
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
                val name = s"$owner/ensures"
                val declared = declare(
                  name,
                  statedAs(all.reduce(Prop.And(_, _))),
                  Origin.Guarantees(function, lines)
                )
                stating = stating.updated(name, stated.result().toSet)
                Some(declared)
        StatedGuarantees(each, unsupported.result(), together)
    }

    private def declareObligations(
        caller: FunctionRef[?, ?],
        premise: Option[(String, FunctionContract[?, ?])],
        parameter: Option[LimitedData]
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
        val parameters = variablesOf(caller, body, parameter)
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

    /** The goal a tactic is given for `statement`: the statement, the functions, and the theorems
      * proved so far, which are its lemmas.
      */
    private[verify] def goalOf(statement: Statement): Goal = {
        // A statement is no lemma of its own, as where one that is proved is stated again.
        val lemmas = proven.values.filter(_.statement.name != statement.name)
        Goal(statement, functionTable, lemmas.toList.sortBy(_.statement.name))
    }

    /** Checks that `tactic` supports the complete goal and, if so, returns opaque evidence that can
      * be passed to [[prove]]. Function representations and available lemmas are captured now.
      */
    def prepare(statement: Statement, tactic: Tactic): Either[CompatibilityReport, PreparedRun] = {
        require(
          declarations.get(statement.name).exists(_ eq statement),
          s"statement ${statement.name} is not registered in this verifier"
        )
        val goal = goalOf(statement)
        tactic.prepare(goal).map { prepared =>
            new PreparedRun(
              this,
              statement,
              tactic.name,
              () => {
                  tactics = tactics.updated(tactic.name, tactic)
                  run(tactic)(statement, prepared)
              },
              () => identityOf(tactic)(prepared)
            )
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
        // A proof uses only the theorems the verifier has, and says how it was found.
        def checked(proof: Proof): Unit = {
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
        }
        prepared.execute() match
            case VerificationResult.Proven(proof) =>
                checked(proof)
                val theorem = new Theorem(prepared.statement, proof)
                proven = proven.updated(prepared.statement.name, theorem)
                provedWith = provedWith.updated(prepared.statement.name, prepared.tacticName)
                prepared
                    .identity()
                    .foreach(identity => provedAs = provedAs.updated(identity, theorem))
                VerificationResult.Proven(proof)
            // A proof within a bound is of a weaker statement: no theorem is registered.
            case within @ VerificationResult.ProvenWithin(proof, _) =>
                checked(proof)
                within
            case VerificationResult.Refuted(proof) => VerificationResult.Refuted(proof)
            case inconclusive @ VerificationResult.Inconclusive(_) => inconclusive
            case failed @ VerificationResult.Failed(_)             => failed
    }

    /** What `tactic` says a statement it prepared is. */
    private def identityOf(tactic: Tactic)(prepared: tactic.Prepared): Option[StatementIdentity] =
        tactic
            .fingerprint(prepared)
            .map(fingerprint =>
                StatementIdentity(tactic.name, fingerprint.statement, fingerprint.programs)
            )

    /** What each tactic that proved something says of a statement, once for each. */
    private val identities =
        scala.collection.mutable.Map.empty[(String, String), Option[StatementIdentity]]

    private def identityOf(statement: Statement, tactic: Tactic): Option[StatementIdentity] =
        identities.getOrElseUpdate(
          statement.name -> tactic.name,
          tactic
              .prepare(goalOf(statement))
              .toOption
              .flatMap(prepared => identityOf(tactic)(prepared))
        )

    /** Declares that `statement` is not proved, for `reason`: a clause it states, or one whose
      * statement a tactic finds the same as it, is open ([[specifications]]), and not unchecked.
      */
    def open(statement: Statement, reason: String): Unit = {
        require(
          declarations.get(statement.name).exists(_ eq statement),
          s"statement ${statement.name} is not registered in this verifier"
        )
        opened = opened.updated(statement.name, reason)
    }

    /** Every clause of the specifications of the functions here, where their code states them, and
      * how each stands: proved, open, owed by no caller here, or unchecked.
      *
      * An `ensures` or `ensuring` clause is proved by a theorem that states it: one [[guarantees]]
      * declared, or a contract read from the function's code ([[contract]], or [[statement]] of a
      * contract). A theorem also proves a clause where the tactic that proved it finds it the same
      * statement as one that states the clause: the same proposition, about the same programs. So a
      * statement written in other words checks the clause, and the clause is in the code of the
      * statement either way. A clause none proves is open where such a statement is declared open.
      *
      * An `expects` clause is a precondition its function's callers owe: it is proved where the
      * obligation of every call of the function by a function here is ([[obligations]]), and owed
      * by no caller where no function here calls it.
      */
    def specifications: List[Specification] = {
        val clausesOf = List.newBuilder[(Clause, FunctionDef[?, ?])]
        for
            definition <- functionTable.definitions
            sir <- definition.get(Representation.Sir)
            (site, _) <- Obligations.sites(Obligations.body(definition, sir).term, clauseArities)
            kind <- Clause.kindOf(site.callee)
        do clausesOf += Clause.at(site.node, kind) -> definition
        clausesOf
            .result()
            .groupBy(_._1)
            .toList
            .map((clause, stating) => (clause, stating.map(_._2).distinctBy(_.name)))
            .sortBy((clause, _) => (clause.file, clause.line, clause.kind.ordinal))
            .map((clause, functions) =>
                Specification(
                  clause,
                  functions.map(_.ref.displayName).sorted,
                  if clause.kind == Clause.Kind.Expects then expectsStatus(clause, functions)
                  else ensuresStatus(clause)
                )
            )
    }

    private val clauseArities =
        Map(Obligations.Ensures -> 1, Obligations.Ensuring -> 2, Obligations.Expects -> 1)

    /** The theorem `name` as a status: the statements, the tactic and the proofs. */
    private def checkedBy(theorems: List[Theorem]): ClauseStatus.Checked =
        ClauseStatus.Checked(
          theorems.map(_.statement.name),
          theorems
              .flatMap(theorem => provedWith.get(theorem.statement.name))
              .distinct
              .mkString(", "),
          theorems.flatMap(_.proof.kinds).toSet
        )

    /** A theorem that proves `statement`: the statement itself, or one a tactic finds the same. */
    private def provedFor(statement: Statement): Option[Theorem] =
        proven.get(statement.name).orElse {
            tactics.valuesIterator
                .flatMap(tactic => identityOf(statement, tactic).flatMap(provedAs.get))
                .nextOption()
        }

    /** An open statement, with its reason, that is `statement` or the same as it. */
    private def openFor(statement: Statement): Option[(String, String)] =
        opened.get(statement.name).map(statement.name -> _).orElse {
            opened.iterator.collectFirst {
                case (name, reason) if tactics.valuesIterator.exists { tactic =>
                        val identity = identityOf(statement, tactic)
                        identity.nonEmpty && identity == identityOf(declarations(name), tactic)
                    } =>
                    name -> reason
            }
        }

    private def ensuresStatus(clause: Clause): ClauseStatus = {
        val candidates = stating
            .collect {
                case (name, clauses) if clauses(clause) => declarations(name)
            }
            .toList
            .sortBy(_.name)
        candidates.iterator.flatMap(provedFor).nextOption() match
            case Some(theorem) => checkedBy(List(theorem))
            case None =>
                candidates.iterator.flatMap(openFor).nextOption() match
                    case Some((name, reason)) => ClauseStatus.Open(name, reason)
                    case None =>
                        ClauseStatus.Unchecked(
                          candidates.map(_.name),
                          if candidates.isEmpty then
                              "no statement here states it: declare its statements with " +
                                  "verifier.guarantees, or its contract with FunctionContract.inSource"
                          else
                              "prove one of its statements, or one that is the same, or declare it open"
                        )
    }

    private def expectsStatus(clause: Clause, owners: List[FunctionDef[?, ?]]): ClauseStatus = {
        val names = owners.map(_.name).toSet
        // The contracts read from the code that have the clause, by their names.
        val contracts = declarations.values
            .filter(_.origin match
                case Origin.Contract(contract) => contract.clauses.contains(clause)
                case _                         => false)
            .map(_.name)
            .toSet
        val owed = declarations.values.toList
            .filter(_.origin match
                case Origin.Obligation(_, callee, contract, _) =>
                    names(callee.name) && contracts(contract)
                case _ => false)
            .sortBy(_.name)
        // The functions here, but those that state the clause, that call one that does.
        val callers = functionTable.definitions.toList.filter { definition =>
            !names(definition.name) && definition.get(Representation.Sir).exists { sir =>
                val body = Obligations.body(definition, sir)
                Obligations
                    .sites(
                      EraseSpecifications(body.term),
                      owners.map(owner => owner.name -> owner.arity).toMap
                    )
                    .nonEmpty
            }
        }
        if callers.isEmpty then ClauseStatus.NotOwed
        else
            val unstated = callers.filterNot(caller =>
                owed.exists(_.origin match
                    case Origin.Obligation(of, _, _, _) => of.name == caller.name
                    case _                              => false)
            )
            val unproved = owed.filterNot(statement => provedFor(statement).nonEmpty)
            if unstated.isEmpty && unproved.isEmpty then checkedBy(owed.flatMap(provedFor))
            else
                val toState =
                    if unstated.isEmpty then Nil
                    else
                        List(
                          s"state the obligations of ${unstated.map(_.ref.displayName).mkString(", ")}" +
                              " with verifier.obligations, under the contract read from the code"
                        )
                val toProve =
                    if unproved.isEmpty then Nil
                    else List(s"prove ${unproved.map(_.name).mkString(", ")}")
                ClauseStatus.Unchecked(owed.map(_.name), (toState ++ toProve).mkString("; "))
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
        case VerificationResult.Proven(_)              => "a proof"
        case VerificationResult.ProvenWithin(_, bound) => s"a proof within $bound"
        case VerificationResult.Refuted(_)             => "a refutation"
        case VerificationResult.Inconclusive(reason)   => s"no result: $reason"
        case VerificationResult.Failed(reason)         => s"a failure: $reason"
}
