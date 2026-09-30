package scalus.verify.uplcblaster

import scalus.*
import scalus.cardano.ledger.ExUnits
import scalus.compiler.Options
import scalus.compiler.sir.{AnnotatedSIR, AnnotationsDecl, SIR, SIRBuiltins, SIRType}
import scalus.uplc.{Constant, DeBruijn, NamedDeBruijn, Program, Term}
import scalus.uplc.eval.{MachineError, NoLogger, OutOfExBudgetError, PlutusVM, RestrictingBudgetSpender}
import scalus.utils.{Hex, Utils}
import scalus.verify.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

/** Proves [[scalus.verify.Prop]] statements about their compiled UPLC with Lean Blaster.
  *
  * The supported fragment is a prefix of universal quantifiers over `BigInt` and `Boolean` values,
  * followed by a quantifier-free body: tests, total calls, `denotes`, `equal` and the connectives.
  * Every test in the body is compiled to its own closed UPLC predicate over the quantified values
  * (see [[UplcBlaster.lower]]). The Lean CEK model runs each predicate for at most `budget` steps,
  * keeping a failing program apart from an exhausted budget, and Blaster decides the resulting
  * proposition. Each test is read according to its polarity (design doc §6.2), so a proof at any
  * budget holds without the budget, and a statement that a program fails can be proved. A
  * counterexample is replayed on the Scalus CEK before it is reported as a refutation. Other
  * statement shapes are inconclusive.
  */
final class UplcBlaster private (val budget: Int, val leanDirectory: Path) extends Tactic {
    require(budget > 0, "the UPLC Blaster budget must be positive")

    override val name: String = "uplc-blaster"

    /** Exports the current UPLC catalogue in the form consumed by the Lean workspace. */
    def exportUplc(directory: Path): Seq[Path] = UplcBlaster.exportUplc(directory)

    override def discharge(goal: Goal): VerificationResult =
        UplcBlaster.lower(goal.statement.prop, goal.functions) match
            case Left(reason) =>
                VerificationResult.Inconclusive(
                  s"$name cannot check ${goal.statement.name}: $reason"
                )
            case Right(lowered) => UplcBlaster.check(lowered, budget, leanDirectory)
}

object UplcBlaster {

    /** What Lean reported about a statement, and which compiled predicates it was about.
      *
      * @param programHashes
      *   the SHA-256 of each predicate's CBOR, in the order of [[Lowered.leaves]]
      * @param counterexample
      *   for a refutation, the value of each quantified variable, by name, as replayed on the
      *   Scalus CEK; empty otherwise
      */
    final case class Artifact(
        programHashes: List[String],
        budget: Int,
        output: String,
        counterexample: List[(String, Constant)]
    ) extends ProofArtifact {
        override val kind: ProofKind = ProofKind.Blaster
    }

    /** The quantifier-free body of a lowered statement. A leaf is an index into [[Lowered.leaves]].
      */
    enum Formula {

        /** The predicate returns `true`. */
        case Test(leaf: Int)

        /** The predicate returns without an error. */
        case Denotes(leaf: Int)
        case And(left: Formula, right: Formula)
        case Or(left: Formula, right: Formula)
        case Not(inner: Formula)
        case Implies(premise: Formula, conclusion: Formula)
    }

    /** A statement lowered for Lean: its universal binders, its body, and one closed UPLC program
      * per leaf of the body. Every program takes the binders' values in order.
      */
    final case class Lowered(
        binders: List[PropExpr.Ident[?]],
        body: Formula,
        leaves: Vector[Program]
    )

    /** Budget used only by the generated differential checks. Large enough for every target;
      * `runSteps` short-circuits once the machine halts, so an oversized value costs nothing.
      */
    private val guardBudget = 20000

    /** The Scalus CEK budget for replaying a counterexample: a hundred times the mainnet
      * per-transaction limit. It only guards against a predicate that does not terminate.
      */
    private val replayBudget = ExUnits(memory = 1_400_000_000L, steps = 1_000_000_000_000L)

    private lazy val replayVm: PlutusVM = PlutusVM.makePlutusV3VM()

    /** Compile configuration supported by the current PlutusCoreBlaster model. */
    val options: Options = Options.releaseUntagged.copy(valueBuiltins = false)

    def apply(budget: Int): UplcBlaster =
        new UplcBlaster(budget, Path.of("scalus-verification", "src", "main", "lean"))

    def apply(budget: Int, leanDirectory: Path): UplcBlaster =
        new UplcBlaster(budget, leanDirectory)

    /** Lowers a statement in the supported fragment, or explains why it is outside it.
      *
      * The fragment is a prefix of universal quantifiers over `BigInt` and `Boolean` followed by a
      * body without quantifiers. The body's leaves are Boolean tests, total calls whose
      * continuation is a test or another total call, `denotes` and `equal` over `BigInt` or
      * `Boolean`. Its connectives are `&&`, `||`, `!`, `==>` and `<=>`. `<=>` becomes two
      * implications, because its operands occur in both polarities.
      *
      * A call of a function in `functions` is linked to that function's compiled program, so the
      * function's bytes appear unchanged in the predicate. Other `@Compile` definitions a test uses
      * are compiled together with the test.
      */
    def lower(prop: Prop, functions: FunctionTable): Either[String, Lowered] =
        universalPrefix(prop).flatMap { case (binders, body) =>
            val leaves = ArrayBuffer.empty[Program]
            def leaf(term: Term): Int = {
                leaves += Program.plutusV3(term)
                leaves.size - 1
            }
            def loop(current: Prop): Either[String, Formula] = current match
                case _: Prop.Bool | _: Prop.Call[?, ?] =>
                    predicate(current, binders, functions).map(term => Formula.Test(leaf(term)))
                case Prop.Equal(left, right) =>
                    equality(expressionSir(left), expressionSir(right)).map(test =>
                        Formula.Test(leaf(compileSirFunction(binders, test, functions)))
                    )
                case Prop.Denotes(expr) =>
                    val value = expressionSir(expr)
                    if supportedType(value.tp) then
                        Right(Formula.Denotes(leaf(compileSirFunction(binders, value, functions))))
                    else Left(s"denotes over ${value.tp.show} is not supported")
                case Prop.And(left, right) =>
                    for l <- loop(left); r <- loop(right) yield Formula.And(l, r)
                case Prop.Or(left, right) =>
                    for l <- loop(left); r <- loop(right) yield Formula.Or(l, r)
                case Prop.Not(inner) => loop(inner).map(Formula.Not(_))
                case Prop.Implies(premise, conclusion) =>
                    for p <- loop(premise); c <- loop(conclusion) yield Formula.Implies(p, c)
                case Prop.Iff(left, right) =>
                    for l <- loop(left); r <- loop(right)
                    yield Formula.And(Formula.Implies(l, r), Formula.Implies(r, l))
                case _: Prop.Forall[?] | _: Prop.Exists[?] =>
                    Left("a quantifier after the universal prefix is not supported")

            loop(body).map(formula => Lowered(binders, formula, leaves.toVector))
        }

    private def supportedType(tp: SIRType): Boolean = tp == SIRType.Integer || tp == SIRType.Boolean

    private def universalPrefix(prop: Prop): Either[String, (List[PropExpr.Ident[?]], Prop)] = {
        @annotation.tailrec
        def loop(
            current: Prop,
            binders: List[PropExpr.Ident[?]]
        ): Either[String, (List[PropExpr.Ident[?]], Prop)] = current match
            case Prop.Forall(ident, body) =>
                if supportedType(ident.tp) then loop(body, ident :: binders)
                else Left(s"a ${ident.tp.show} binder is not supported, only BigInt and Boolean")
            case body => Right(binders.reverse -> body)

        loop(prop, Nil)
    }

    /** A test, or a total call continuing with one, as a function of `binders` returning a Boolean.
      */
    private def predicate(
        prop: Prop,
        binders: List[PropExpr.Ident[?]],
        functions: FunctionTable
    ): Either[String, Term] = prop match
        case Prop.Bool(expr) =>
            val body = expressionSir(expr)
            require(body.tp == SIRType.Boolean, s"a test has type ${body.tp.show}")
            Right(compileSirFunction(binders, body, functions))
        // A function of several parameters takes them as one tuple in a `Prop.Call`, while its
        // UPLC program is curried, so only single BigInt or Boolean arguments are passed today.
        case Prop.Call(_, arg, _, true, _) if !supportedType(expressionSir(arg).tp) =>
            Left(
              s"a call argument of type ${expressionSir(arg).tp.show} is not supported, only BigInt and Boolean"
            )
        case Prop.Call(_, _, result, true, _) if !supportedType(result.tp) =>
            Left(
              s"a call result of type ${result.tp.show} is not supported, only BigInt and Boolean"
            )
        case Prop.Call(fn, arg, result, true, body) =>
            predicate(body, binders :+ result, functions).map { continuation =>
                val argument = applyParameters(
                  compileSirFunction(binders, expressionSir(arg), functions),
                  binders
                )
                val called = Term.Apply(functions(fn)(Representation.Uplc).term, argument)
                val continued = Term.Apply(applyParameters(continuation, binders), called)
                binders.foldRight(continued) { (binder, current) =>
                    Term.LamAbs(binder.name, current)
                }
            }
        case Prop.Call(_, _, _, false, _) => Left("partial whenReturns calls are not supported")
        case _ =>
            Left("a call's continuation must be a test or another total call")

    /** `left` and `right` evaluate to equal values. */
    private def equality(left: SIR, right: SIR): Either[String, SIR] = {
        val annotations = AnnotationsDecl.empty
        if left.tp != right.tp then Left(s"cannot compare ${left.tp.show} with ${right.tp.show}")
        else
            left.tp match
                case SIRType.Integer =>
                    Right(withExpressions(left, right) { (l, r) =>
                        val partial = SIR.Apply(
                          SIRBuiltins.equalsInteger,
                          l,
                          SIRType.Fun(SIRType.Integer, SIRType.Boolean),
                          annotations
                        )
                        SIR.Apply(partial, r, SIRType.Boolean, annotations)
                    })
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
        val unlinkedBody = unlinkModuleDefinitions(body, functions)
        val linked = externalVariables(unlinkedBody)
            .filter((name, _) => functions.contains(name))
            .distinctBy(_._1)
        val unbound = freeVariables(unlinkedBody) -- binders.map(_.name) -- linked.map(_._1)
        require(
          unbound.isEmpty,
          s"UPLC proposition body has free variables: ${unbound.toList.sorted.mkString(", ")}"
        )
        val parameters = linked ++ binders.map(ident => ident.name -> ident.tp)
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

    private def expressionSir(expr: PropExpr[?]): SIR = expr match
        case PropExpr.SIRExpr(sir) => sir
        case PropExpr.Ident(name, _, tp) =>
            SIR.Var(name, tp, AnnotationsDecl.empty)

    private def applyParameters(
        function: Term,
        parameters: List[PropExpr.Ident[?]]
    ): Term = parameters.foldLeft(function) { (current, parameter) =>
        Term.Apply(current, Term.Var(NamedDeBruijn(parameter.name)))
    }

    /** Removes the module definitions of functions in `functions` from around an expression.
      * `Compiler.compile` put them there; their remaining `ExternalVar` occurrences become explicit
      * parameters, filled with the functions' own UPLC programs. Definitions of other functions
      * stay, if the expression uses them, and are compiled together with it.
      */
    private def unlinkModuleDefinitions(sir: SIR, functions: FunctionTable): SIR = sir match
        case SIR.Let(bindings, body, flags, anns) =>
            val unlinkedBody = unlinkModuleDefinitions(body, functions)
            val candidates = bindings.filterNot(binding => functions.contains(binding.name))
            @annotation.tailrec
            def live(names: Set[String]): Set[String] = {
                val next = names ++ candidates.iterator
                    .filter(binding => names.contains(binding.name))
                    .flatMap(binding => freeVariables(binding.value))
                if next == names then names else live(next)
            }
            val liveNames = live(freeVariables(unlinkedBody))
            val kept = candidates.filter(binding => liveNames.contains(binding.name))
            if kept.isEmpty then unlinkedBody else SIR.Let(kept, unlinkedBody, flags, anns)
        case other => other

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
            val bound = bindings.map(_.name).toSet
            val valueFree = bindings.iterator.flatMap(binding => freeVariables(binding.value)).toSet
            val effectiveValueFree =
                if SIR.LetFlags.isRec(flags) then valueFree -- bound else valueFree
            effectiveValueFree ++ (freeVariables(body) -- bound)
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

    private def check(goal: Lowered, budget: Int, leanDirectory: Path): VerificationResult = {
        require(
          Files.isDirectory(leanDirectory),
          s"Lean workspace does not exist: ${leanDirectory.toAbsolutePath}"
        )
        val temporary = Files.createTempDirectory("scalus-uplc-blaster-")
        try
            val flats = goal.leaves.zipWithIndex.map { (program, index) =>
                val flat = temporary.resolve(s"Leaf$index.flat")
                Files.writeString(flat, Hex.bytesToHex(program.cborEncoded).toLowerCase)
                flat
            }
            val source = temporary.resolve("Check.lean")
            Files.writeString(source, renderCheck(goal, flats, budget))
            val process = new ProcessBuilder("lake", "env", "lean", source.toString)
                .directory(leanDirectory.toAbsolutePath.toFile)
                .redirectErrorStream(true)
                .start()
            val output = String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
            val exit = process.waitFor()
            val artifact = Artifact(
              goal.leaves.toList.map(program =>
                  Hex.bytesToHex(Utils.sha2_256(program.cborEncoded)).toLowerCase
              ),
              budget,
              output.trim,
              Nil
            )
            if output.contains("✅ Valid") then VerificationResult.Proven(Proof(artifact))
            else if output.contains("❌ Falsified") then replay(goal, artifact)
            else if output.contains("⚠️ Undetermined") then
                VerificationResult.Inconclusive("UPLC Blaster was undetermined")
            else
                VerificationResult.Inconclusive(
                  s"Lean exited with code $exit: ${concise(output)}"
                )
        finally
            Files.walk(temporary).iterator().asScala.toList.reverse.foreach(Files.deleteIfExists)
    }

    /** What a predicate did on concrete arguments, on the Scalus CEK. */
    private enum Outcome {
        case Returned(value: Term)
        case Failed
        case Exhausted
    }

    /** Replays Blaster's counterexample on the Scalus CEK, without Lean's step budget (§5.2).
      *
      * A falsification under the budget may be spurious: a strong reading of a test is false when
      * the predicate needs more steps. Only a counterexample under which the statement is false
      * without the budget is a refutation.
      */
    private def replay(goal: Lowered, artifact: Artifact): VerificationResult = {
        val values = counterexample(goal.binders, artifact.output)
        val shown = goal.binders
            .zip(values)
            .map((binder, value) => s"${binder.name} = ${display(value)}")
            .mkString(", ")
        val outcomes = goal.leaves.map(evaluate(_, values))
        holds(goal.body, outcomes) match
            case Some(false) =>
                VerificationResult.Refuted(
                  Proof(artifact.copy(counterexample = goal.binders.map(_.name).zip(values)))
                )
            case Some(true) =>
                VerificationResult.Inconclusive(
                  s"Blaster's counterexample ($shown) is spurious: the statement holds there on the " +
                      s"Scalus CEK, so a test needs more than ${artifact.budget} steps"
                )
            case None =>
                VerificationResult.Inconclusive(
                  s"replaying Blaster's counterexample ($shown) exhausted the replay budget"
                )
    }

    private val counterexampleLine = """-\s+x(\d+):\s+(.+?)\s*$""".r.unanchored

    /** The values of Blaster's counterexample, in binder order. A binder the model leaves
      * unconstrained takes `0` or `false`; the replay checks the completed assignment.
      */
    private def counterexample(
        binders: List[PropExpr.Ident[?]],
        output: String
    ): List[Constant] = {
        val reported = output.linesIterator.collect { case counterexampleLine(index, value) =>
            index.toInt -> value
        }.toMap
        binders.zipWithIndex.map { (binder, index) =>
            val text = reported.get(index)
            binder.tp match
                case SIRType.Integer =>
                    Constant.Integer(
                      text.fold(BigInt(0))(value =>
                          BigInt(value.filterNot(c => c == '(' || c == ')' || c.isWhitespace))
                      )
                    )
                case SIRType.Boolean =>
                    text match
                        case None          => Constant.Bool(false)
                        case Some("true")  => Constant.Bool(true)
                        case Some("false") => Constant.Bool(false)
                        case Some(other) =>
                            throw new IllegalStateException(
                              s"unexpected Boolean in Blaster's counterexample: $other"
                            )
                case other =>
                    throw new IllegalStateException(s"unexpected ${other.show} binder")
        }
    }

    private def display(value: Constant): String = value match
        case Constant.Integer(integer) => integer.toString
        case Constant.Bool(boolean)    => boolean.toString
        case other                     => other.toString

    private def evaluate(program: Program, values: List[Constant]): Outcome = {
        val applied =
            values.foldLeft(program.term)((term, value) => Term.Apply(term, Term.Const(value)))
        // A machine error is the predicate's result here: the test failed.
        try
            Outcome.Returned(
              replayVm.evaluateDeBruijnedTerm(
                DeBruijn.deBruijnTerm(applied),
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
    private def holds(formula: Formula, outcomes: Vector[Outcome]): Option[Boolean] =
        formula match
            case Formula.Test(leaf) =>
                outcomes(leaf) match
                    case Outcome.Returned(Term.Const(Constant.Bool(value), _)) => Some(value)
                    case Outcome.Returned(_)                                   => Some(false)
                    case Outcome.Failed                                        => Some(false)
                    case Outcome.Exhausted                                     => None
            case Formula.Denotes(leaf) =>
                outcomes(leaf) match
                    case Outcome.Returned(_) => Some(true)
                    case Outcome.Failed      => Some(false)
                    case Outcome.Exhausted   => None
            case Formula.And(left, right) =>
                (holds(left, outcomes), holds(right, outcomes)) match
                    case (Some(false), _) | (_, Some(false)) => Some(false)
                    case (Some(true), Some(true))            => Some(true)
                    case _                                   => None
            case Formula.Or(left, right) =>
                (holds(left, outcomes), holds(right, outcomes)) match
                    case (Some(true), _) | (_, Some(true)) => Some(true)
                    case (Some(false), Some(false))        => Some(false)
                    case _                                 => None
            case Formula.Not(inner) => holds(inner, outcomes).map(!_)
            case Formula.Implies(premise, conclusion) =>
                holds(Formula.Or(Formula.Not(premise), conclusion), outcomes)

    private def concise(output: String): String = {
        val normalized = output.linesIterator.map(_.trim).filter(_.nonEmpty).mkString(" ")
        if normalized.length <= 500 then normalized else normalized.take(497) + "..."
    }

    private def leanString(value: String): String =
        value.flatMap {
            case '\\' => "\\\\"
            case '"'  => "\\\""
            case c    => c.toString
        }

    private def leanType(tp: SIRType): String = tp match
        case SIRType.Integer => "Integer"
        case SIRType.Boolean => "Bool"
        case other           => throw new IllegalStateException(s"unexpected ${other.show} binder")

    /** Renders a formula with each leaf read by its polarity (design doc §6.2).
      *
      * Each predicate runs with `ScalusProofs.Run.runFor`, where `State.Error` means that the
      * program failed and a run that exhausts the budget ends in a state that has neither halted
      * nor failed. In a positive position a test must halt within the budget with `true`. In a
      * negative position it must neither halt with `false` nor fail within the budget, so a run
      * that exhausts the budget counts as possibly `true`. `denotes` is read likewise: it halts
      * within the budget, and in a negative position it does not fail within it. Strengthening
      * positive and weakening negative occurrences gives a proposition that implies the statement.
      */
    private def renderFormula(formula: Formula, arguments: String, positive: Boolean): String =
        formula match
            case Formula.Test(leaf) =>
                val state = s"(prepared$leaf$arguments)"
                if positive then s"(fromFrameToBool $state = some true)"
                else s"(fromFrameToBool $state ≠ some false ∧ failed $state = false)"
            case Formula.Denotes(leaf) =>
                val state = s"(prepared$leaf$arguments)"
                if positive then s"(isSuccessful $state)" else s"(failed $state = false)"
            case Formula.And(left, right) =>
                s"(${renderFormula(left, arguments, positive)} ∧ ${renderFormula(right, arguments, positive)})"
            case Formula.Or(left, right) =>
                s"(${renderFormula(left, arguments, positive)} ∨ ${renderFormula(right, arguments, positive)})"
            case Formula.Not(inner) => s"(¬ ${renderFormula(inner, arguments, !positive)})"
            case Formula.Implies(premise, conclusion) =>
                s"(${renderFormula(premise, arguments, !positive)} → ${renderFormula(conclusion, arguments, positive)})"

    private def renderCheck(goal: Lowered, flats: Vector[Path], budget: Int): String = {
        val names = goal.binders.indices.map(index => s"x$index").toList
        val declarations = goal.binders.zip(names).map { (binder, name) =>
            s"($name : ${leanType(binder.tp)})"
        }
        val imports = flats.zipWithIndex.map { (flat, index) =>
            val path = leanString(flat.toAbsolutePath.toString)
            s"""#import_uplc leaf$index PlutusV3 single_cbor_hex "$path""""
        }
        val terms = goal.binders.zip(names).map { (binder, name) =>
            s"Term.Const $$ Const.${leanType(binder.tp)} $name"
        }
        // A statement without binders passes its empty argument list as a plain `List Term`.
        val arguments = (("def arguments" +: declarations) :+
            s": List Term := [${terms.mkString(", ")}]").mkString(" ")
        val prepared = flats.indices.map(index =>
            s"#prep_uplc_run prepared$index leaf$index arguments $budget"
        )
        val formula = renderFormula(goal.body, names.map(" " + _).mkString, positive = true)
        val proposition =
            if declarations.isEmpty then formula else s"∀ ${declarations.mkString(" ")}, $formula"

        s"""import ScalusProofs.Run
           |
           |namespace ScalusProofs.Runtime
           |
           |open PlutusCore.Integer (Integer)
           |open PlutusCore.UPLC
           |open PlutusCore.UPLC.Term
           |open PlutusCore.UPLC.Utils
           |open ScalusProofs.Run
           |
           |${imports.mkString("\n")}
           |
           |$arguments
           |
           |${prepared.mkString("\n")}
           |
           |#blaster (gen-cex: 1) [$proposition]
           |
           |end ScalusProofs.Runtime
           |""".stripMargin
    }

    private def header: String =
        s"""-- Generated by `sbt exportLeanUplc`. Do not edit by hand.
           |--
           |-- Each `example` below asserts that the Lean CEK machine produces the value Scalus's
           |-- own JVM CEK produced for the same arguments, so the two implementations are checked
           |-- against each other on every build. A failing check most often means the compiled
           |-- program changed, not that the Lean model is wrong.
           |import ScalusProofs.Prelude
           |
           |namespace ScalusProofs.Generated
           |open PlutusCore.Integer (Integer)
           |open ScalusProofs.Prelude
           |
           |set_option warn.sorry false
           |""".stripMargin

    private def renderArgs(args: Seq[BigInt]): String =
        args.map(a => if a < 0 then s"($a)" else a.toString).mkString("[", ", ", "]")

    private def renderExpected(value: BigInt): String =
        if value < 0 then s"($value)" else value.toString

    private def renderTarget(target: ProofTarget): String = {
        val uplcImport =
            s"""#import_uplc ${target.leanName} PlutusV3 single_cbor_hex "ScalusProofs/Generated/${target.name}.flat""""
        val checks = target.samples.map { case (args, expected) =>
            s"""example : runInts ${target.leanName} ${renderArgs(args)} $guardBudget """ +
                s"""= some ${renderExpected(expected)} := by native_decide"""
        }
        (uplcImport +: checks).mkString("\n") + "\n"
    }

    /** Regenerates the committed UPLC inputs for the existing Lean proof suite. */
    def exportUplc(directory: Path): Seq[Path] = {
        Files.createDirectories(directory)
        val flats = ProofTargets.all.map { target =>
            val path = directory.resolve(s"${target.name}.flat")
            Files.writeString(path, Hex.bytesToHex(target.program.cborEncoded).toLowerCase)
            path
        }
        val body = ProofTargets.all.map(renderTarget).mkString("\n")
        val lean = directory.resolve("Targets.lean")
        Files.writeString(lean, s"$header\n$body\nend ScalusProofs.Generated\n")
        flats :+ lean
    }

    def main(args: Array[String]): Unit = {
        require(args.nonEmpty, "usage: UplcBlaster <outputDir>")
        val written = exportUplc(Path.of(args(0)))
        written.foreach(path => println(s"wrote $path"))
        println(s"${written.size} files, ${ProofTargets.all.size} targets")
    }
}
