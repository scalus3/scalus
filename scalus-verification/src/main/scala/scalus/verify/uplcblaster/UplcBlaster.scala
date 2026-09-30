package scalus.verify.uplcblaster

import scalus.*
import scalus.cardano.ledger.ExUnits
import scalus.compiler.Options
import scalus.compiler.sir.{AnnotatedSIR, AnnotationsDecl, DataDecl, SIR, SIRBuiltins, SIRType}
import scalus.uplc.{Constant, DeBruijn, Program, Term}
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
  * budget holds without the budget, and a statement that a program fails can be proved. A closed
  * statement, without quantified variables, has nothing to search for: Lean decides it by running
  * its predicates, with `native_decide` ([[ProofKind.LeanNative]]). A counterexample is replayed on
  * the Scalus CEK before it is reported as a refutation. Other statement shapes are inconclusive.
  */
final class UplcBlaster private (val budget: Int, val leanDirectory: Path) extends Tactic {
    require(budget > 0, "the UPLC Blaster budget must be positive")

    override val name: String = "uplc-blaster"

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
      * @param kind
      *   [[ProofKind.Blaster]] for a statement with quantified variables, and
      *   [[ProofKind.LeanNative]] for a closed statement, which Lean decides by evaluation
      */
    final case class Artifact(
        programHashes: List[String],
        budget: Int,
        output: String,
        counterexample: List[(String, Constant)],
        kind: ProofKind
    ) extends ProofArtifact

    /** The quantifier-free body of a lowered statement, over its leaves.
      *
      * The leaves of a statement are the nodes of its [[Prop]] tree that are not connectives:
      * tests, calls, `denotes` and `equal`. [[lower]] compiles each leaf into a closed UPLC program
      * in [[Lowered.leaves]], and a `LeafFormula` keeps the connectives around them, with each leaf
      * replaced by its index there. `<=>` is already split into two implications. Both the Lean
      * proposition (`renderFormula`) and the replay of a counterexample (`holds`) are read from it.
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
    }

    /** A statement lowered for Lean: its universal binders, its body, and one closed UPLC program
      * per leaf of the body. Every program takes the binders' values in order.
      */
    final case class Lowered(
        binders: List[PropExpr.Ident[?]],
        body: LeafFormula,
        leaves: Vector[Program]
    )

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
      * function's bytes appear unchanged in the predicate. A function of several parameters is
      * called with a tuple written out as `(a, b, ...)`, and its program is applied to each value
      * in turn. Other `@Compile` definitions a test uses are compiled together with the test.
      */
    def lower(prop: Prop, functions: FunctionTable): Either[String, Lowered] =
        universalPrefix(prop).flatMap { case (binders, body) =>
            val leaves = ArrayBuffer.empty[Program]
            def leaf(term: Term): Int = {
                leaves += Program.plutusV3(term)
                leaves.size - 1
            }
            def loop(current: Prop): Either[String, LeafFormula] = current match
                case _: Prop.Bool | _: Prop.Call[?, ?] =>
                    predicate(current, binders, functions).map(term => LeafFormula.Test(leaf(term)))
                case Prop.Equal(left, right) =>
                    equality(expressionSir(left), expressionSir(right)).map(test =>
                        LeafFormula.Test(leaf(compileSirFunction(binders, test, functions)))
                    )
                case Prop.Denotes(expr) =>
                    val value = expressionSir(expr)
                    if supportedType(value.tp) then
                        Right(
                          LeafFormula.Denotes(leaf(compileSirFunction(binders, value, functions)))
                        )
                    else Left(s"denotes over ${value.tp.show} is not supported")
                case Prop.And(left, right) =>
                    for l <- loop(left); r <- loop(right) yield LeafFormula.And(l, r)
                case Prop.Or(left, right) =>
                    for l <- loop(left); r <- loop(right) yield LeafFormula.Or(l, r)
                case Prop.Not(inner) => loop(inner).map(LeafFormula.Not(_))
                case Prop.Implies(premise, conclusion) =>
                    for p <- loop(premise); c <- loop(conclusion) yield LeafFormula.Implies(p, c)
                case Prop.Iff(left, right) =>
                    for l <- loop(left); r <- loop(right)
                    yield LeafFormula.And(LeafFormula.Implies(l, r), LeafFormula.Implies(r, l))
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
    ): Either[String, Term] =
        predicateSir(prop, functions).map(body => compileSirFunction(binders, body, functions))

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
            case Prop.Call(_, _, result, true, _) if !supportedType(result.tp) =>
                Left(
                  s"a call result of type ${result.tp.show} is not supported, only BigInt and Boolean"
                )
            case Prop.Call(fn, arg, result, true, body) =>
                for
                    arguments <- callArguments(expressionSir(arg), functions(fn).arity)
                    continuation <- predicateSir(body, functions)
                yield callSir(fn, arguments, result, continuation)
            case Prop.Call(_, _, _, false, _) =>
                Left("partial whenReturns calls are not supported")
            case _ =>
                Left("a call's continuation must be a test or another total call")

    /** `(result => continuation)(fn(arguments...))`. The data declarations around the pieces move
      * outside the whole expression.
      */
    private def callSir(
        fn: FunctionRef[?, ?],
        arguments: List[SIR],
        result: PropExpr.Ident[?],
        continuation: SIR
    ): SIR = {
        val annotations = AnnotationsDecl.empty
        val (argumentDeclarations, values) = arguments.map(declarations).unzip
        val (continuationDeclarations, test) = declarations(continuation)
        // The function is curried: it takes one argument per parameter.
        val types = values.scanRight(result.tp)((value, rest) => SIRType.Fun(value.tp, rest))
        val function: AnnotatedSIR = SIR.ExternalVar("", fn.name, types.head, annotations)
        val called = values.zip(types.tail).foldLeft(function) { case (applied, (value, tp)) =>
            SIR.Apply(applied, value, tp, annotations)
        }
        val continued = SIR.Apply(
          SIR.LamAbs(SIR.Var(result.name, result.tp, annotations), test, Nil, annotations),
          called,
          SIRType.Boolean,
          annotations
        )
        (argumentDeclarations.flatten ++ continuationDeclarations)
            .distinctBy(_.name)
            .foldRight[SIR](continued)((data, body) => SIR.Decl(data, body))
    }

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
    private def callArguments(argument: SIR, arity: Int): Either[String, List[SIR]] = {
        val arguments =
            if arity == 1 then Right(List(argument)) else tupleComponents(argument, arity)
        arguments.flatMap { values =>
            values.find(value => !supportedType(value.tp)) match
                case Some(value) =>
                    Left(
                      s"a call argument of type ${value.tp.show} is not supported, only BigInt and Boolean"
                    )
                case None => Right(values)
        }
    }

    /** The components of a tuple of `arity` values written out as `(a, b, ...)`. The definitions
      * the compiler put around the tuple stay around each component.
      */
    private def tupleComponents(sir: SIR, arity: Int): Either[String, List[SIR]] = sir match
        case SIR.Decl(data, term) =>
            tupleComponents(term, arity).map(_.map(component => SIR.Decl(data, component)))
        case SIR.Let(bindings, body, flags, anns) =>
            tupleComponents(body, arity).map(
              _.map(component => SIR.Let(bindings, component, flags, anns))
            )
        case SIR.Constr(name, _, arguments, _, _)
            if name == s"scala.Tuple$arity" && arguments.size == arity =>
            Right(arguments)
        case other =>
            Left(
              s"a function of $arity parameters takes a tuple of $arity values written out as " +
                  s"(a, b, ...), not an expression of type ${other.tp.show}"
            )

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
        case SIR.Decl(data, term) => SIR.Decl(data, unlinkModuleDefinitions(term, functions))
        case other                => other

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
            val closed = goal.binders.isEmpty
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
              Nil,
              if closed then ProofKind.LeanNative else ProofKind.Blaster
            )
            def failure = VerificationResult.Inconclusive(
              s"Lean exited with code $exit: ${concise(output)}"
            )
            if closed then
                if exit == 0 then VerificationResult.Proven(Proof(artifact))
                else if output.contains("`native_decide` evaluated that the proposition") then
                    replay(goal, artifact)
                else failure
            else if output.contains("✅ Valid") then VerificationResult.Proven(Proof(artifact))
            else if output.contains("❌ Falsified") then replay(goal, artifact)
            else if output.contains("⚠️ Undetermined") then
                VerificationResult.Inconclusive("UPLC Blaster was undetermined")
            else failure
        finally
            Files.walk(temporary).iterator().asScala.toList.reverse.foreach(Files.deleteIfExists)
    }

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
    private def replay(goal: Lowered, artifact: Artifact): VerificationResult = {
        val values = counterexample(goal.binders, artifact.output)
        val shown = goal.binders
            .zip(values)
            .map((binder, value) => s"${binder.name} = ${display(value)}")
            .mkString(", ")
        val falsification =
            if shown.isEmpty then "Lean's falsification" else s"Lean's counterexample ($shown)"
        val outcomes = goal.leaves.map(evaluate(_, values))
        holds(goal.body, outcomes) match
            case Some(false) =>
                VerificationResult.Refuted(
                  Proof(artifact.copy(counterexample = goal.binders.map(_.name).zip(values)))
                )
            case Some(true) =>
                VerificationResult.Inconclusive(
                  s"$falsification is spurious: the statement holds there on the Scalus CEK, so a " +
                      s"test needs more than ${artifact.budget} steps"
                )
            case None =>
                VerificationResult.Inconclusive(
                  s"replaying $falsification exhausted the replay budget"
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
    private def holds(formula: LeafFormula, outcomes: Vector[Outcome]): Option[Boolean] =
        formula match
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

    /** Lean's output, without the lines that report each imported program, and shortened. */
    private def concise(output: String): String = {
        val normalized = output.linesIterator
            .map(_.trim)
            .filter(line => line.nonEmpty && !line.startsWith("Successfully decoded"))
            .mkString(" ")
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
      *
      * `state` is the Lean expression for the final state of a leaf's run. Every reading is a
      * `Bool` comparison, so Blaster can translate it and `native_decide` can evaluate it.
      */
    private def renderFormula(
        formula: LeafFormula,
        state: Int => String,
        positive: Boolean
    ): String =
        def render(inner: LeafFormula, positive: Boolean) = renderFormula(inner, state, positive)
        formula match
            case LeafFormula.Test(leaf) =>
                if positive then s"(fromFrameToBool ${state(leaf)} = some true)"
                else
                    s"(fromFrameToBool ${state(leaf)} ≠ some false ∧ failed ${state(leaf)} = false)"
            case LeafFormula.Denotes(leaf) =>
                if positive then s"(halted ${state(leaf)} = true)"
                else s"(failed ${state(leaf)} = false)"
            case LeafFormula.And(left, right) =>
                s"(${render(left, positive)} ∧ ${render(right, positive)})"
            case LeafFormula.Or(left, right) =>
                s"(${render(left, positive)} ∨ ${render(right, positive)})"
            case LeafFormula.Not(inner) => s"(¬ ${render(inner, !positive)})"
            case LeafFormula.Implies(premise, conclusion) =>
                s"(${render(premise, !positive)} → ${render(conclusion, positive)})"

    /** The Lean check of a lowered statement. A statement with quantified variables is proved by
      * Blaster over `#prep_uplc_run`. A closed statement has nothing to search for: its predicates
      * run on the computable `runProgramFor`, and `native_decide` evaluates the proposition.
      */
    private def renderCheck(goal: Lowered, flats: Vector[Path], budget: Int): String = {
        val imports = flats.zipWithIndex.map { (flat, index) =>
            val path = leanString(flat.toAbsolutePath.toString)
            s"""#import_uplc leaf$index PlutusV3 single_cbor_hex "$path""""
        }
        val check =
            if goal.binders.isEmpty then
                val state = (leaf: Int) => s"(runProgramFor leaf$leaf.script [] $budget)"
                s"example : ${renderFormula(goal.body, state, positive = true)} := by native_decide"
            else
                val names = goal.binders.indices.map(index => s"x$index").toList
                val declarations = goal.binders.zip(names).map { (binder, name) =>
                    s"($name : ${leanType(binder.tp)})"
                }
                val terms = goal.binders.zip(names).map { (binder, name) =>
                    s"Term.Const $$ Const.${leanType(binder.tp)} $name"
                }
                val arguments = (("def arguments" +: declarations) :+
                    s": List Term := [${terms.mkString(", ")}]").mkString(" ")
                val prepared = flats.indices.map(index =>
                    s"#prep_uplc_run prepared$index leaf$index arguments $budget"
                )
                val state = (leaf: Int) => s"(prepared$leaf${names.map(" " + _).mkString})"
                val formula = renderFormula(goal.body, state, positive = true)
                s"""$arguments
                   |
                   |${prepared.mkString("\n")}
                   |
                   |#blaster (gen-cex: 1) [∀ ${declarations.mkString(" ")}, $formula]""".stripMargin

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
           |$check
           |
           |end ScalusProofs.Runtime
           |""".stripMargin
    }
}
