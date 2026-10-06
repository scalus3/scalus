package scalus.verify.uplcblaster

import scalus.*
import scalus.cardano.ledger.ExUnits
import scalus.compiler.Options
import scalus.compiler.sir.{AnnotatedSIR, AnnotationsDecl, Binding, DataDecl, SIR, SIRBuiltins, SIRType}
import scalus.uplc.{Constant, DeBruijn, Program, Term}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.eval.{MachineError, NoLogger, OutOfExBudgetError, PlutusVM, RestrictingBudgetSpender}
import scalus.utils.{Hex, Utils}
import scalus.verify.*
import scalus.verify.lean.{Directories, Processes}

import java.nio.file.{Files, Path}
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.FiniteDuration

/** Proves [[scalus.verify.Prop]] statements about their compiled UPLC with Lean Blaster.
  *
  * The supported fragment has universal and existential quantifiers over `BigInt`, `Boolean`,
  * `ByteString`, `Data` and case classes of them, with tests, calls, `denotes`, `equal` and the
  * connectives. An `existsLet` is eliminated by applying its body to the supplied witness. Every
  * test is compiled to its own closed UPLC predicate over the quantified values in its scope (see
  * [[UplcBlaster.lower]]). The Lean CEK model runs each predicate for at most `budget` steps,
  * keeping a failing program apart from an exhausted budget, and Blaster decides the resulting
  * proposition. Each test is read according to its polarity (design doc §6.2), so a proof at any
  * budget holds without the budget, and a statement that a program fails can be proved. A closed
  * statement, without quantified variables, has nothing to search for: Lean decides it by running
  * its predicates, with `native_decide` ([[ProofKind.LeanNative]]). A counterexample is replayed on
  * the Scalus CEK before it is reported as a refutation. Other statement shapes are rejected during
  * preparation.
  */
final class UplcBlaster private (
    val budget: Int,
    val leanDirectory: Path,
    val timeout: Option[FiniteDuration]
) extends Tactic {
    override type Prepared = UplcBlaster.Lowered

    require(budget > 0, "the UPLC Blaster budget must be positive")
    require(timeout.forall(_.length > 0), "the UPLC Blaster timeout must be positive")

    override val name: String = "uplc-blaster"

    override def prepare(goal: Goal): Either[CompatibilityReport, Prepared] =
        UplcBlaster
            .lower(goal.statement.prop, goal.functions)
            .left
            .map(reason =>
                CompatibilityReport(
                  List(CompatibilityIssue.UnsupportedFeature(List(goal.statement.name), reason))
                )
            )

    override def run(prepared: Prepared): ExecutionResult =
        UplcBlaster.check(prepared, budget, leanDirectory, timeout)
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
        private[uplcblaster] val leafBinders: Vector[List[Int]]
    ) {

        /** Whether a quantifier of the statement asks for a witness: an `exists` in a positive
          * position, or a `forAll` in a negative one, under `!` or in the premise of `==>`. Either
          * side of `<=>` is in both.
          *
          * Without one, every variable ranges over all its values, and the statement is false where
          * its body is false of one assignment. With one, an assignment shows nothing: the witness
          * may be another value.
          */
        def hasExistential: Boolean = {
            def loop(formula: LeafFormula, positive: Boolean): Boolean = formula match
                case LeafFormula.Exists(_, body)  => positive || loop(body, positive)
                case LeafFormula.Forall(_, body)  => !positive || loop(body, positive)
                case LeafFormula.And(left, right) => loop(left, positive) || loop(right, positive)
                case LeafFormula.Or(left, right)  => loop(left, positive) || loop(right, positive)
                case LeafFormula.Not(inner)       => loop(inner, !positive)
                case LeafFormula.Implies(premise, conclusion) =>
                    loop(premise, !positive) || loop(conclusion, positive)
                case _: LeafFormula.Test | _: LeafFormula.Denotes => false
            loop(body, positive = true)
        }
    }

    /** The Scalus CEK budget for replaying a counterexample: a hundred times the mainnet
      * per-transaction limit. It only guards against a predicate that does not terminate.
      */
    private val replayBudget = ExUnits(memory = 1_400_000_000L, steps = 1_000_000_000_000L)

    private lazy val replayVm: PlutusVM = PlutusVM.makePlutusV3VM()

    /** Compile configuration supported by the current PlutusCoreBlaster model. */
    val options: Options = Options.releaseUntagged.copy(valueBuiltins = false)

    def apply(budget: Int): UplcBlaster =
        new UplcBlaster(budget, Path.of("scalus-verification", "src", "main", "lean"), None)

    def apply(budget: Int, leanDirectory: Path): UplcBlaster =
        new UplcBlaster(budget, leanDirectory, None)

    /** A tactic that stops Lean after `timeout` and is then inconclusive. Without a timeout a
      * statement Lean cannot finish, such as one whose program loops over a list of unknown length,
      * runs until the process is stopped from outside.
      */
    def apply(budget: Int, leanDirectory: Path, timeout: FiniteDuration): UplcBlaster =
        new UplcBlaster(budget, leanDirectory, Some(timeout))

    /** Lowers a statement in the supported fragment, or explains why it is outside it.
      *
      * The fragment has universal and existential quantifiers over `BigInt`, `Boolean`,
      * `ByteString`, `Data` and case classes of them. A variable of a case class becomes one
      * variable per field in [[Lowered.binders]], and each test builds it from them. `existsLet`
      * applies its body to the supplied witness and introduces no Lean quantifier. Leaves are
      * Boolean tests, calls whose continuation is a test or another total call, `denotes`, and
      * `equal` over `BigInt`, `Boolean` and `Data`. A call's arguments and result, and the operand
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
            val leaves = ArrayBuffer.empty[Program]
            val leafBinders = ArrayBuffer.empty[List[Int]]
            // Leaves that are one program over the same variables share it, as the calls of two
            // clauses of a contract do. The variable indices matter: equal program bytes under two
            // different quantifiers are two differently scoped leaves.
            val indices = scala.collection.mutable.Map.empty[(String, List[Int]), Int]

            def leaf(term: Term, scope: List[Int]): Int = {
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
              */
            def quantified(
                ident: PropExpr.Ident[?]
            ): Either[String, (List[Int], List[Binding])] =
                expand(ident.name, ident.tp).map { (variables, built) =>
                    val from = binders.size
                    binders ++= variables
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
                        returns <- returnsSir(fn, arg, result.tp, functions)
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
                case Prop.Forall(ident, body) =>
                    quantified(ident).flatMap { (introduced, bindings) =>
                        loop(body, scope ++ introduced, built ++ bindings)
                            .map(LeafFormula.Forall(introduced, _))
                    }
                case Prop.Exists(ident, None, body) =>
                    quantified(ident).flatMap { (introduced, bindings) =>
                        loop(body, scope ++ introduced, built ++ bindings)
                            .map(LeafFormula.Exists(introduced, _))
                    }
                case Prop.Exists(ident, Some(witness), body) =>
                    loop(instantiate(body, ident, witness), scope, built)
            }

            loop(prop, Nil, Nil).map(formula =>
                Lowered(binders.toList, formula, leaves.toVector, leafBinders.toVector)
            )
        }

    /** A type a call can pass to a function that has no SIR, and take back from it: one whose
      * values have one form whatever their static type. It says nothing of quantified variables:
      * [[leanVariable]] does, and differs for one constructor of `Data`.
      */
    private def quantifiable(tp: SIRType): Boolean =
        tp == SIRType.Integer || tp == SIRType.Boolean || tp == SIRType.ByteString || isData(tp)

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
    private def leanVariable(tp: SIRType): Boolean = tp match
        case SIRType.Integer | SIRType.Boolean | SIRType.ByteString => true
        case other                                                  => isWholeData(other)

    /** `Data` itself, and not one of its constructors. */
    private def isWholeData(tp: SIRType): Boolean = tp match
        case SIRType.SumCaseClass(decl, _) => decl.name == SIRType.Data.name
        case _                             => false

    /** The variables Lean quantifies over for a statement's variable `name` of type `tp`, and the
      * variable's value built from them, when it is not one of them itself.
      *
      * A variable of a case class ranges over the values of its constructor (prop-semantics.md §1),
      * so it stands for one variable per field, named `name.field`, and its value is the
      * constructor applied to them. A field that is a case class is expanded in turn: a `Config`
      * with a `PubKeyHash` ends in the hash's `ByteString`.
      */
    private def expand(
        name: String,
        tp: SIRType
    ): Either[String, (List[PropExpr.Ident[?]], Option[AnnotatedSIR])] = unwrap(tp) match
        case plain if leanVariable(plain) =>
            Right(List(new PropExpr.Ident[Any](name, 0L, plain)) -> None)
        case product @ SIRType.CaseClass(constructor, typeArguments, None) =>
            val arguments = constructor.typeParams.zip(typeArguments).toMap
            val fields = constructor.params.map { field =>
                val fieldName = s"$name.${field.name}"
                val fieldType = SIRType.substitute(field.tp, arguments, Map.empty)
                expand(fieldName, fieldType).map { (binders, built) =>
                    val value: AnnotatedSIR =
                        built.getOrElse(SIR.Var(fieldName, binders.head.tp, AnnotationsDecl.empty))
                    binders -> value
                }
            }
            fields.collectFirst { case Left(reason) => reason } match
                case Some(reason) => Left(reason)
                case None =>
                    val (binders, values) = fields.collect { case Right(field) => field }.unzip
                    // The declaration of a class that is its own only constructor, as the
                    // compiler makes it.
                    val declaration = DataDecl(
                      constructor.name,
                      List(constructor),
                      constructor.typeParams,
                      constructor.annotations
                    )
                    Right(
                      binders.flatten -> Some(
                        SIR.Constr(
                          constructor.name,
                          declaration,
                          values,
                          product,
                          AnnotationsDecl.empty
                        )
                      )
                    )
        case other =>
            Left(
              s"a ${other.show} binder is not supported, only BigInt, Boolean, ByteString, Data " +
                  "and case classes of them"
            )

    /** `sir` with the variables in `built` that it uses bound to their values, inside its data
      * declarations.
      */
    private def withBuilt(built: List[Binding], sir: SIR): SIR = {
        val free = freeVariables(sir)
        val used = built.filter(binding => free.contains(binding.name))
        if used.isEmpty then sir
        else
            val (data, expression) = declarations(sir)
            val constructed = used.flatMap(binding => constructorDeclarations(binding.value))
            withDeclarations(
              constructed ++ data,
              SIR.Let(used, expression, SIR.LetFlags.None, AnnotationsDecl.empty)
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
      * them, in the witness as in the expression.
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
                val (definitions, witnessValue) = moduleDefinitions(linkedWitness)
                val (bodyDeclarations, body) = declarations(sir)
                val bound: AnnotatedSIR = SIR.Let(
                  List(Binding(ident.name, ident.tp, witnessValue)),
                  body,
                  SIR.LetFlags.None,
                  AnnotationsDecl.empty
                )
                PropExpr.SIRExpr[T](
                  withDeclarations(
                    witnessDeclarations ++ bodyDeclarations,
                    definitions.foldRight(bound)((definition, inner) => definition(inner))
                  )
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

    /** The module definitions `Compiler.compile` put around an expression, outermost first, each as
      * the `let` it is around what it is given, and the expression. A module definition has a
      * qualified name, which a value of the expression's own never has.
      */
    private def moduleDefinitions(
        sir: AnnotatedSIR
    ): (List[AnnotatedSIR => AnnotatedSIR], AnnotatedSIR) = sir match
        case SIR.Let(bindings, body: AnnotatedSIR, flags, anns)
            if bindings.forall(_.name.contains('.')) =>
            val (inner, expression) = moduleDefinitions(body)
            val definition = (rest: AnnotatedSIR) => SIR.Let(bindings, rest, flags, anns)
            (definition :: inner) -> expression
        case expression => Nil -> expression

    /** The data declarations of the constructors a built value applies. */
    private def constructorDeclarations(sir: SIR): List[DataDecl] = sir match
        case SIR.Constr(_, data, arguments, _, _) =>
            data :: arguments.flatMap(constructorDeclarations)
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
      * `Leaf<n>.flat`, and `Check.lean`, whose path it returns. The tactic runs
      * `lake env lean Check.lean` in its workspace; a check written with this can be read, or run
      * by hand with other options.
      */
    def writeCheck(goal: Lowered, budget: Int, directory: Path): Path = {
        val flats = goal.leaves.zipWithIndex.map { (program, index) =>
            val flat = directory.resolve(s"Leaf$index.flat")
            Files.writeString(flat, Hex.bytesToHex(program.cborEncoded).toLowerCase)
            flat
        }
        val source = directory.resolve("Check.lean")
        Files.writeString(source, renderCheck(goal, flats, budget))
        source
    }

    private def check(
        goal: Lowered,
        budget: Int,
        leanDirectory: Path,
        timeout: Option[FiniteDuration]
    ): ExecutionResult = {
        if !Files.isDirectory(leanDirectory) then
            return VerificationResult.Failed(
              s"Lean workspace does not exist: ${leanDirectory.toAbsolutePath}"
            )
        val temporary = Files.createTempDirectory("scalus-uplc-blaster-")
        try
            val source = writeCheck(goal, budget, temporary)
            // Lean's output goes to a file, so that waiting for the process can time out.
            val log = temporary.resolve("Check.out")
            val exit =
                try
                    execute(
                      List("lake", "env", "lean", source.toString),
                      leanDirectory.toAbsolutePath,
                      log,
                      timeout
                    )
                catch
                    case error: java.io.IOException =>
                        return VerificationResult.Failed(
                          s"cannot start Lean: ${error.getMessage}"
                        )
            exit match
                case Some(code) => verdict(goal, budget, code, Files.readString(log))
                case None =>
                    VerificationResult.Inconclusive(
                      s"Lean did not finish within ${timeout.get}"
                    )
        finally Directories.remove(temporary)
    }

    /** Runs `command` in `directory`, with its output in `log`, and returns its exit code, or
      * `None` when it did not finish within `timeout`.
      *
      * However the wait ends, with a result, at the time limit, or because this thread was
      * interrupted, the process and the ones it started are stopped before this returns or throws:
      * nothing is left running in a directory the caller removes.
      */
    private[uplcblaster] def execute(
        command: List[String],
        directory: Path,
        log: Path,
        timeout: Option[FiniteDuration]
    ): Option[Int] = {
        val process = new ProcessBuilder(command*)
            .directory(directory.toFile)
            .redirectErrorStream(true)
            .redirectOutput(log.toFile)
            .start()
        try
            val finished = timeout match
                case Some(limit) => process.waitFor(limit.length, limit.unit)
                case None        => process.waitFor(); true
            Option.when(finished)(process.exitValue())
        finally Processes.stop(process)
    }

    /** The result Lean's exit code and output stand for. */
    private[uplcblaster] def verdict(
        goal: Lowered,
        budget: Int,
        exit: Int,
        output: String
    ): ExecutionResult = {
        val closed = goal.binders.isEmpty
        val artifact = Artifact(
          goal.leaves.toList.map(program =>
              Hex.bytesToHex(Utils.sha2_256(program.cborEncoded)).toLowerCase
          ),
          budget,
          output.trim,
          Nil,
          if closed then ProofKind.LeanNative else ProofKind.Blaster
        )
        def failure: ExecutionResult = VerificationResult.Failed(
          s"Lean exited with code $exit: ${concise(output)}"
        )
        if closed then
            if exit == 0 then VerificationResult.Proven(Proof(artifact))
            else if output.contains("`native_decide` evaluated that the proposition") then
                replay(goal, artifact)
            else failure
        else if output.contains("✅ Valid") then VerificationResult.Proven(Proof(artifact))
        else if output.contains("❌ Falsified") then
            if goal.hasExistential then
                VerificationResult.Inconclusive(
                  "UPLC Blaster falsified a statement that asks for a witness, with an exists, " +
                      "or with a forAll under a negation or in a premise, but did not provide " +
                      "a finite certificate that the Scalus CEK can replay to establish that no " +
                      "witness exists"
                )
            else replay(goal, artifact)
        else if output.contains("⚠️ Undetermined") then
            VerificationResult.Inconclusive(
              "UPLC Blaster was undetermined"
            )
        else failure
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
    private def replay(goal: Lowered, artifact: Artifact): ExecutionResult =
        counterexample(goal.binders, artifact.output) match
            case Left(SmtValues.Unreadable.Malformed(error)) =>
                VerificationResult.Failed(s"cannot read Lean's counterexample: $error")
            // Lean's model has more values than the variable's type, so this one refutes nothing.
            case Left(SmtValues.Unreadable.OutsideType(error)) =>
                VerificationResult.Inconclusive(
                  s"Lean's counterexample is no value of its variable's type: $error"
                )
            case Right(values) => replayValues(goal, artifact, values)

    private def replayValues(
        goal: Lowered,
        artifact: Artifact,
        values: List[Constant]
    ): ExecutionResult = {
        val shown = goal.binders
            .zip(values)
            .map((binder, value) => s"${binder.name} = ${display(value)}")
            .mkString(", ")
        val falsification =
            if shown.isEmpty then "Lean's falsification" else s"Lean's counterexample ($shown)"
        val outcomes = goal.leaves.zip(goal.leafBinders).map { (program, scope) =>
            evaluate(program, scope.map(values))
        }
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
      * `- xN:`, up to the one that closes its parentheses. They are not all indented: Z3 starts a
      * nested `let` of a large value on a new line.
      */
    private def counterexample(
        binders: List[PropExpr.Ident[?]],
        output: String
    ): Either[SmtValues.Unreadable, List[Constant]] = {
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
        binders.zipWithIndex.foldRight[Either[SmtValues.Unreadable, List[Constant]]](Right(Nil)) {
            case ((binder, index), rest) =>
                val text = reported.get(index)
                val value: Either[SmtValues.Unreadable, Constant] = binder.tp match
                    case SIRType.Integer =>
                        text.fold(Right(BigInt(0)))(SmtValues.integer).map(Constant.Integer(_))
                    case SIRType.Boolean =>
                        text.fold(Right(false))(SmtValues.boolean).map(Constant.Bool(_))
                    case SIRType.ByteString =>
                        text.fold(Right(ByteString.empty))(SmtValues.bytes)
                            .map(Constant.ByteString(_))
                    case tp if isWholeData(tp) =>
                        text.fold(Right(Data.I(0)))(SmtValues.data).map(Constant.Data(_))
                    case other =>
                        Left(
                          SmtValues.Unreadable.Malformed(
                            s"a ${other.show} binder has no counterexample value"
                          )
                        )
                // The variable the value is of, in the reason.
                val named = value.left.map {
                    case SmtValues.Unreadable.Malformed(reason) =>
                        SmtValues.Unreadable.Malformed(s"${binder.name}: $reason")
                    case SmtValues.Unreadable.OutsideType(reason) =>
                        SmtValues.Unreadable.OutsideType(s"${binder.name}: $reason")
                }
                for v <- named; r <- rest yield v :: r
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
            // Replay supplies one concrete value for every binder, and evaluates the quantified
            // body at that assignment. A statement that asks for a witness never reaches replay
            // (`Lowered.hasExistential`), so every quantifier here ranges over all values: a
            // `forAll` in a positive position, an `exists` in a negative one.
            case LeafFormula.Forall(_, body) => holds(body, outcomes)
            case LeafFormula.Exists(_, body) => holds(body, outcomes)

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
        case SIRType.Integer       => "Integer"
        case SIRType.Boolean       => "Bool"
        case SIRType.ByteString    => "ByteString"
        case tp if isWholeData(tp) => "Data"
        case other => throw new IllegalStateException(s"unexpected ${other.show} binder")

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
        positive: Boolean
    ): String =
        def render(inner: LeafFormula, positive: Boolean) =
            renderFormula(inner, state, binder, positive)
        def quantified(symbol: String, binders: List[Int], body: LeafFormula): String =
            if binders.isEmpty then render(body, positive)
            else s"($symbol ${binders.map(binder).mkString(" ")}, ${render(body, positive)})"
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
            case LeafFormula.Forall(binders, body) => quantified("∀", binders, body)
            // Blaster accepts Exists directly, but the equivalent ¬∀¬ form keeps generated
            // quantifiers uniform and lets the ordinary universal path handle their binders.
            case LeafFormula.Exists(binders, body) =>
                render(
                  LeafFormula.Not(LeafFormula.Forall(binders, LeafFormula.Not(body))),
                  positive
                )

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
                s"example : ${renderFormula(goal.body, state, _ => "", positive = true)} := by native_decide"
            else
                val names = goal.binders.indices.map(index => s"x$index").toList
                val declarations = goal.binders.zip(names).map { (binder, name) =>
                    s"($name : ${leanType(binder.tp)})"
                }
                val arguments = goal.leafBinders.zipWithIndex.map { (scope, leaf) =>
                    val parameters = scope.map(declarations)
                    val terms = scope.map { index =>
                        val binder = goal.binders(index)
                        s"Term.Const $$ Const.${leanType(binder.tp)} ${names(index)}"
                    }
                    ((s"def arguments$leaf" +: parameters) :+
                        s": List Term := [${terms.mkString(", ")}]").mkString(" ")
                }
                val prepared = flats.indices.map(index =>
                    s"#prep_uplc_run prepared$index leaf$index arguments$index $budget"
                )
                val state = (leaf: Int) => {
                    val arguments = goal.leafBinders(leaf).map(index => " " + names(index)).mkString
                    s"(prepared$leaf$arguments)"
                }
                val declaration = (index: Int) => declarations(index)
                val formula = renderFormula(goal.body, state, declaration, positive = true)
                s"""${arguments.mkString("\n")}
                   |
                   |${prepared.mkString("\n")}
                   |
                   |#blaster (gen-cex: 1) [$formula]""".stripMargin

        s"""import ScalusProofs.Run
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
           |$check
           |
           |end ScalusProofs.Runtime
           |""".stripMargin
    }
}
