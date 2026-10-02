package scalus.verify.uplcblaster

import scalus.*
import scalus.cardano.ledger.ExUnits
import scalus.compiler.Options
import scalus.compiler.sir.{AnnotatedSIR, AnnotationsDecl, DataDecl, SIR, SIRBuiltins, SIRType}
import scalus.uplc.{Constant, DeBruijn, Program, Term}
import scalus.uplc.builtin.Data
import scalus.uplc.eval.{MachineError, NoLogger, OutOfExBudgetError, PlutusVM, RestrictingBudgetSpender}
import scalus.utils.{Hex, Utils}
import scalus.verify.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

/** Proves [[scalus.verify.Prop]] statements about their compiled UPLC with Lean Blaster.
  *
  * The supported fragment is a prefix of universal quantifiers over `BigInt`, `Boolean` and `Data`,
  * followed by a quantifier-free body: tests, calls, `denotes`, `equal` and the connectives. Every
  * test in the body is compiled to its own closed UPLC predicate over the quantified values (see
  * [[UplcBlaster.lower]]). The Lean CEK model runs each predicate for at most `budget` steps,
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

    /** What Lean reported about a statement, and which compiled programs it was about.
      *
      * @param programHashes
      *   the SHA-256 of each leaf's program CBOR, in the order of [[Lowered.leaves]]
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
      * The fragment is a prefix of universal quantifiers over `BigInt`, `Boolean` and `Data`
      * followed by a body without quantifiers. The body's leaves are Boolean tests, calls whose
      * continuation is a test or another total call, `denotes`, and `equal` over those types. A
      * call's arguments and result, and the operand of `denotes`, can have any type. Its
      * connectives are `&&`, `||`, `!`, `==>` and `<=>`. `<=>` becomes two implications, because
      * its operands occur in both polarities. A partial call, `whenReturns(f, a)(r => p)`, becomes
      * `denotes(f(a)) ==> call(f, a)(r => p)`: its two leaves run the function's program each.
      *
      * A call of a function in `functions` is linked to that function's compiled program, so the
      * function's bytes appear unchanged in the predicate. A function of several parameters is
      * called with a tuple written out as `(a, b, ...)`, and its program is applied to each value
      * in turn. Other `@Compile` definitions a test uses are compiled together with the test.
      */
    def lower(prop: Prop, functions: FunctionTable): Either[String, Lowered] =
        checkSignatures(prop, functions).flatMap(_ => universalPrefix(prop)).flatMap {
            case (binders, body) =>
                val leaves = ArrayBuffer.empty[Program]
                // Leaves that are one program share it, as the calls of two clauses of a contract
                // do: the encoding has no variable names, so equal bytes are equal programs.
                val indices = scala.collection.mutable.Map.empty[String, Int]
                def leaf(term: Term): Int = {
                    val program = Program.plutusV3(term)
                    indices.getOrElseUpdate(
                      Hex.bytesToHex(program.cborEncoded), {
                          leaves += program
                          leaves.size - 1
                      }
                    )
                }
                def loop(current: Prop): Either[String, LeafFormula] = current match
                    case Prop.Call(fn, arg, result, false, body) =>
                        for
                            returns <- returnsSir(fn, arg, result.tp, functions)
                            formula <- loop(
                              Prop.Implies(
                                Prop.Denotes(PropExpr.SIRExpr(returns)),
                                Prop.Call(fn, arg, result, true, body)
                              )
                            )
                        yield formula
                    case _: Prop.Bool | _: Prop.Call[?, ?] =>
                        predicate(current, binders, functions)
                            .map(term => LeafFormula.Test(leaf(term)))
                    case Prop.Equal(left, right) =>
                        equality(expressionSir(left), expressionSir(right)).map(test =>
                            LeafFormula.Test(leaf(compileSirFunction(binders, test, functions)))
                        )
                    case Prop.Denotes(expr) =>
                        val value = expressionSir(expr)
                        Right(
                          LeafFormula.Denotes(leaf(compileSirFunction(binders, value, functions)))
                        )
                    case Prop.And(left, right) =>
                        for l <- loop(left); r <- loop(right) yield LeafFormula.And(l, r)
                    case Prop.Or(left, right) =>
                        for l <- loop(left); r <- loop(right) yield LeafFormula.Or(l, r)
                    case Prop.Not(inner) => loop(inner).map(LeafFormula.Not(_))
                    case Prop.Implies(premise, conclusion) =>
                        for p <- loop(premise); c <- loop(conclusion)
                        yield LeafFormula.Implies(p, c)
                    case Prop.Iff(left, right) =>
                        for l <- loop(left); r <- loop(right)
                        yield LeafFormula.And(LeafFormula.Implies(l, r), LeafFormula.Implies(r, l))
                    case _: Prop.Forall[?] | _: Prop.Exists[?] =>
                        Left("a quantifier after the universal prefix is not supported")

                loop(body).map(formula => Lowered(binders, formula, leaves.toVector))
        }

    /** A type a quantified variable can have. Lean passes a quantified value to the tests'
      * programs, so its UPLC form must be one Lean can build. Values inside a test, such as a
      * call's arguments and result, can have any type: the compiler lowers them, and Lean never
      * sees them.
      */
    private def quantifiable(tp: SIRType): Boolean =
        tp == SIRType.Integer || tp == SIRType.Boolean || isData(tp)

    /** `Data`, or one of its constructors, such as the type of `Data.I(x)`. */
    private def isData(tp: SIRType): Boolean = tp match
        case SIRType.SumCaseClass(decl, _)         => decl.name == SIRType.Data.name
        case SIRType.CaseClass(_, _, Some(parent)) => isData(parent)
        case _                                     => false

    private def universalPrefix(prop: Prop): Either[String, (List[PropExpr.Ident[?]], Prop)] = {
        @annotation.tailrec
        def loop(
            current: Prop,
            binders: List[PropExpr.Ident[?]]
        ): Either[String, (List[PropExpr.Ident[?]], Prop)] = current match
            case Prop.Forall(ident, body) =>
                if quantifiable(ident.tp) then loop(body, ident :: binders)
                else
                    Left(
                      s"a ${ident.tp.show} binder is not supported, only BigInt, Boolean and Data"
                    )
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
        application(fn, values, result.tp, functions).map { called =>
            val continued = SIR.Apply(
              SIR.LamAbs(SIR.Var(result.name, result.tp, annotations), test, Nil, annotations),
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
    private def withDeclarations(data: List[DataDecl], expression: AnnotatedSIR): SIR =
        data.distinctBy(_.name).foldRight[SIR](expression)((decl, body) => SIR.Decl(decl, body))

    /** `fn` applied to `values`, one at a time: a function's program is curried. `fn` is an
      * `ExternalVar` that [[compileSirFunction]] links to the function's own program.
      *
      * The variable has the function's declared type, from its SIR, so the lowering passes each
      * value as the function's program takes it: an enum's constructor, such as `Circle(r)`, is
      * passed as the enum. A function without SIR has no declared type, so its type comes from the
      * values and the result. Only `BigInt`, `Boolean` and `Data` have one form whatever their
      * static type, so only those can be passed to it.
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
                              "only BigInt, Boolean and Data"
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

    /** A type without the wrappers that leave it what it is: annotations and type proxies. */
    private def unwrap(tp: SIRType): SIRType = tp match
        case SIRType.Annotated(inner, _)           => unwrap(inner)
        case SIRType.TypeProxy(ref) if ref != null => unwrap(ref)
        case other                                 => other

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
        if isData(left.tp) && isData(right.tp) then
            Right(withExpressions(left, right) { (l, r) =>
                val partial = SIR.Apply(
                  SIRBuiltins.equalsData,
                  l,
                  SIRType.Fun(SIRType.Data.tp, SIRType.Boolean),
                  annotations
                )
                SIR.Apply(partial, r, SIRType.Boolean, annotations)
            })
        else if left.tp != right.tp then
            Left(s"cannot compare ${left.tp.show} with ${right.tp.show}")
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
        val external = externalVariables(unlinkedBody).distinctBy(_._1)
        val linked = external.filter((name, _) => functions.contains(name))
        // Other external references, such as the compiler's own support functions behind
        // `d.to[A]`, are resolved by the lowering, which reports one it does not know.
        val unbound = freeVariables(unlinkedBody) -- binders.map(_.name) -- external.map(_._1)
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
            SIR.Var(name, tp, AnnotationsDecl.empty)

    /** Removes the module definitions of functions in `functions` from around an expression.
      * `Compiler.compile` put them there; their remaining `ExternalVar` occurrences become explicit
      * parameters, filled with the functions' own UPLC programs. Definitions of other functions
      * stay, if the expression uses them, and are compiled together with it.
      *
      * The expression's own `let`s sit in the same place as the module definitions, so a binding is
      * dropped only when nothing uses it and its value cannot fail. A statement such as
      * `require(c)` is an unused binding of a value that can.
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
            // A strict binding is evaluated where it stands. One whose value can fail, such as a
            // `require(...)` statement, stays whether or not anything uses it: only a value can be
            // dropped unused, as a module's function definitions are.
            val evaluated =
                if SIR.LetFlags.isLazy(flags) then Nil
                else candidates.filterNot(binding => isValue(binding.value))
            val roots = freeVariables(unlinkedBody) ++
                evaluated.flatMap(binding => freeVariables(binding.value) + binding.name)
            val liveNames = live(roots)
            val kept = candidates.filter(binding => liveNames.contains(binding.name))
            if kept.isEmpty then unlinkedBody else SIR.Let(kept, unlinkedBody, flags, anns)
        case SIR.Decl(data, term) => SIR.Decl(data, unlinkModuleDefinitions(term, functions))
        case other                => other

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
    private def replay(goal: Lowered, artifact: Artifact): VerificationResult =
        counterexample(goal.binders, artifact.output) match
            case Left(error) =>
                VerificationResult.Inconclusive(s"cannot read Lean's counterexample: $error")
            case Right(values) => replayValues(goal, artifact, values)

    private def replayValues(
        goal: Lowered,
        artifact: Artifact,
        values: List[Constant]
    ): VerificationResult = {
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

    /** The values of Blaster's counterexample, in binder order, or why they cannot be read. A
      * binder the model leaves unconstrained, which Blaster omits or Z3 names instead of valuing,
      * takes `0`, `false` or `I 0`; the replay checks the completed assignment.
      *
      * A value can span several lines: a `Data` value is printed as a term over the lines after
      * `- xN:`, each indented.
      */
    private def counterexample(
        binders: List[PropExpr.Ident[?]],
        output: String
    ): Either[String, List[Constant]] = {
        // (values found so far, whether the last line belonged to a value)
        val (found, _) = output.linesIterator.foldLeft((List.empty[(Int, String)], false)) {
            case ((found, continuing), line) =>
                line match
                    case counterexampleLine(index, value) => ((index.toInt -> value) :: found, true)
                    case _ if continuing && line.headOption.exists(_.isWhitespace) =>
                        val (index, value) = found.head
                        ((index -> s"$value ${line.trim}") :: found.tail, true)
                    case _ => (found, false)
        }
        val reported = found.toMap
        binders.zipWithIndex.foldRight[Either[String, List[Constant]]](Right(Nil)) {
            case ((binder, index), rest) =>
                val text = reported.get(index)
                val value: Either[String, Constant] = binder.tp match
                    case SIRType.Integer =>
                        text.fold(Right(BigInt(0)))(SmtValues.integer).map(Constant.Integer(_))
                    case SIRType.Boolean =>
                        text.fold(Right(false))(SmtValues.boolean).map(Constant.Bool(_))
                    case tp if isData(tp) =>
                        text.fold(Right(Data.I(0)))(SmtValues.data).map(Constant.Data(_))
                    case other => Left(s"a ${other.show} binder has no counterexample value")
                for v <- value.left.map(error => s"${binder.name}: $error"); r <- rest yield v :: r
        }
    }

    private def display(value: Constant): String = value match
        case Constant.Integer(integer) => integer.toString
        case Constant.Bool(boolean)    => boolean.toString
        case Constant.Data(data)       => data.toString
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
        case SIRType.Integer  => "Integer"
        case SIRType.Boolean  => "Bool"
        case tp if isData(tp) => "Data"
        case other            => throw new IllegalStateException(s"unexpected ${other.show} binder")

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
           |open PlutusCore.Data (Data)
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
