package scalus.verify.uplcblaster

import scalus.compiler.sir.*
import scalus.compiler.sir.transform.EraseSpecifications
import scalus.uplc.Constant
import scalus.verify.*

import scala.collection.mutable

/** [[UplcBlaster]] on a statement whose walks over lists are unknowns: the tactic of calls as
  * unknowns (docs/design/verification-details/multi-transaction.md).
  *
  * Each call of a walk in the statement's programs is replaced by an unknown value, a variable of
  * the statement, one for each call: two calls of the same function on the same arguments, as the
  * validator's and a clause's, share one. The programs left have no loop, so one budget covers
  * every value of the statement's variables, lists of any length among them. A proof for every
  * value of the unknowns holds for the values the walks return, and so of the statement. A
  * refutation may give the unknowns values that the walks do not return: it is replayed on the
  * statement as written, and where the statement holds there, it is spurious.
  *
  * The tie of a program to its form with unknowns is trusted: see [[CallsAsUnknowns.rewrite]].
  *
  * @param inner
  *   the tactic that checks the statement with unknowns
  * @param library
  *   the functions whose calls are walks where they loop: the others are inlined, so that the walks
  *   in their code are found. By default the ledger's and the prelude's
  *   ([[CallsAsUnknowns.inLibrary]]).
  */
final class CallsAsUnknowns(val inner: UplcBlaster, library: String => Boolean) extends Tactic {

    def this(inner: UplcBlaster) = this(inner, CallsAsUnknowns.inLibrary)

    override type Prepared = CallsAsUnknowns.Prepared

    override val name: String = s"${inner.name}, calls as unknowns"

    override def prepare(goal: Goal): Either[CompatibilityReport, Prepared] =
        CallsAsUnknowns.rewrite(goal.statement.prop, library) match
            case Left(reason) =>
                Left(
                  CompatibilityReport(
                    List(CompatibilityIssue.UnsupportedFeature(List(goal.statement.name), reason))
                  )
                )
            case Right(rewritten) =>
                val statement = new Statement(
                  goal.statement.name,
                  rewritten.prop,
                  goal.statement.origin,
                  goal.statement.expected
                )
                // An unknown whose value has lists is one `Data`: Lean has no list of it to build a
                // term of, and the programs read of it what they read.
                val asData = rewritten.unknowns
                    .filterNot(unknown => CallsAsUnknowns.isScalar(unknown.resultType))
                    .map(_.ident.name)
                    .toSet
                val tactic = inner.withVariablesAsData(asData)
                tactic
                    .prepare(goal.copy(statement = statement))
                    .map(lowered =>
                        CallsAsUnknowns.Prepared(
                          tactic,
                          CallsAsUnknowns.unrestricted(lowered, rewritten.unknowns),
                          inner.prepare(goal).toOption,
                          rewritten.unknowns
                        )
                    )

    override def run(prepared: Prepared, earlier: Option[Kept]): ExecutionResult =
        prepared.tactic.run(prepared.lowered, earlier) match
            case refuted @ VerificationResult.Refuted(proof) =>
                CallsAsUnknowns.replayed(prepared, proof).getOrElse(refuted)
            case other => other

    override def fingerprint(prepared: Prepared): Option[Fingerprint] =
        prepared.tactic.fingerprint(prepared.lowered)

    override def identity(prepared: Prepared): Option[(String, List[String])] =
        prepared.tactic.identity(prepared.lowered)
}

object CallsAsUnknowns {

    /** A statement prepared with its walks as unknowns.
      *
      * @param lowered
      *   the statement with unknowns, lowered by the inner tactic
      * @param asWritten
      *   the statement as written, lowered, on which a refutation is replayed; none where it cannot
      *   be lowered, and then no refutation is taken
      * @param unknowns
      *   the calls made unknowns
      */
    final case class Prepared(
        tactic: UplcBlaster,
        lowered: UplcBlaster.Lowered,
        asWritten: Option[UplcBlaster.Lowered],
        unknowns: List[Unknown]
    )

    /** Whether a value of `tp` is one Lean variable: an integer, a Boolean, a byte string, a string
      * or the unit. Another type's unknown is one `Data`.
      */
    private def isScalar(tp: SIRType): Boolean = SirTypes.unwrap(tp) match
        case SIRType.Integer | SIRType.Boolean | SIRType.ByteString | SIRType.String |
            SIRType.Unit =>
            true
        case _ => false

    /** `lowered` with no premise of the unknowns' types: a statement of every value of an unknown,
      * of its type or not, holds of the values its call returns, which are of the type. Lean then
      * walks no list of an unknown's value to check that it is one of the type's.
      */
    private def unrestricted(
        lowered: UplcBlaster.Lowered,
        unknowns: List[Unknown]
    ): UplcBlaster.Lowered = {
        val names = unknowns.map(_.ident.name).toSet
        def ofUnknown(name: String): Boolean =
            names.exists(n => name == n || name.startsWith(s"$n."))
        def freed(goal: UplcBlaster.Lowered): UplcBlaster.Lowered =
            goal.copy(predicates =
                goal.predicates.zipWithIndex.map((premise, index) =>
                    if goal.binders.lift(index).exists(binder => ofUnknown(binder.name)) then None
                    else premise
                )
            )
        freed(lowered).copy(unrewritten = lowered.unrewritten.map(freed))
    }

    /** A call made an unknown: the statement's variable, of an `Option` of the call's type, which
      * is `None` where the call fails, and the call as written.
      */
    final case class Unknown(ident: PropExpr.Ident[Any], resultType: SIRType, written: String)

    /** A statement with its walks as unknowns, and the calls made unknowns. */
    final case class Rewritten(prop: Prop, unknowns: List[Unknown])

    /** The functions of the ledger's types and of the prelude: their calls are walks where they
      * loop.
      */
    def inLibrary(name: String): Boolean =
        name.startsWith("scalus.cardano.onchain.plutus.") || name.startsWith("scalus.uplc.")

    /** What a call computes, of the statement's variables: two calls of one key compute one value.
      * Variables bound in the key itself are numbered by depth ([[Key.Bound]]), so that two copies
      * of one function have one key. A key with a [[Key.Free]] variable, bound by a lambda outside
      * the call, is of no value of the statement's variables, and no unknown is made of it.
      */
    enum Key {
        case Variable(name: String)
        case Bound(depth: Int)
        case Free(name: String)
        case Global(name: String)
        case Literal(value: Constant)
        case Field(of: Key, field: String)
        case Projection(constructor: String, index: Int, of: Key)
        case Applied(function: Key, arguments: List[Key])
        case Builtin(name: String)
        case Construct(constructor: String, arguments: List[Key])
        case Lambda(arity: Int, body: Key)
        case Recursive(function: Key, body: Key)
        case Choice(condition: Key, ifTrue: Key, ifFalse: Key)
        case Cases(scrutinee: Key, cases: List[(String, Key)])
        case Conjunction(a: Key, b: Key)
        case Disjunction(a: Key, b: Key)
        case Negation(a: Key)
        case Failure

        /** Whether the key is of the statement's variables alone. */
        def isOfVariables: Boolean = this match
            case Free(_)              => false
            case Field(of, _)         => of.isOfVariables
            case Projection(_, _, of) => of.isOfVariables
            case Applied(function, arguments) =>
                function.isOfVariables && arguments.forall(_.isOfVariables)
            case Construct(_, arguments)   => arguments.forall(_.isOfVariables)
            case Lambda(_, body)           => body.isOfVariables
            case Recursive(function, body) => function.isOfVariables && body.isOfVariables
            case Choice(c, t, f)           => c.isOfVariables && t.isOfVariables && f.isOfVariables
            case Cases(scrutinee, cases) =>
                scrutinee.isOfVariables && cases.forall(_._2.isOfVariables)
            case Conjunction(a, b) => a.isOfVariables && b.isOfVariables
            case Disjunction(a, b) => a.isOfVariables && b.isOfVariables
            case Negation(a)       => a.isOfVariables
            case _                 => true
    }

    private val optionName = "scalus.cardano.onchain.plutus.prelude.Option"

    private val fromData = "scalus.uplc.builtin.internal.UniversalDataConversion$.fromData"
    private val toData = "scalus.uplc.builtin.internal.UniversalDataConversion$.toData"

    /** `prop` with its walks as unknowns.
      *
      * A statement's leading `forAll`s are its variables. In each program of its body, a call is a
      * walk where it loops: a recursive `let` applied to arguments, as an inline helper's local
      * `go`, or a call of a function of `library`, or of a recursive one, whose code loops. A call
      * of another function whose code loops is inlined, its parameters bound by `let`s in the order
      * the call evaluates them, so that the walks in its code are found. A walk whose arguments are
      * of the variables, of the values the program binds of them and of earlier walks is replaced
      * by an unknown, a variable added after the statement's own: at the call, `None` fails, as the
      * walk would, and `Some(v)` is `v`. A walk under a lambda, of the lambda's parameter, stays.
      *
      * The tie is trusted, of three facts. A walk ends on every value, as a walk over a list does.
      * Two calls of one key compute one value: a key is the call with each variable the program
      * binds replaced by the value it binds, each pattern variable by its field of the value
      * matched, and `fromData(toData(x))` by `x`, of the types whose `Data` is their value. A
      * program behaves as its form with unknowns, each the result of its call. The statement holds
      * where it holds for every value of the unknowns.
      */
    def rewrite(prop: Prop, library: String => Boolean): Either[String, Rewritten] = {
        def prefix(p: Prop, variables: List[PropExpr.Ident[?]]): (List[PropExpr.Ident[?]], Prop) =
            p match
                case Prop.Forall(ident, body) => prefix(body, ident :: variables)
                case other                    => (variables.reverse, other)
        val (variables, body) = prefix(prop, Nil)
        val table = Unknowns(option)
        val names = variables.map(_.name).toSet
        val rewritten = rewriteProp(body, sir => rewriteLeaf(sir, names, library, table))
        // A statement with no walk is left as it is, its specifications among it.
        if table.unknowns.isEmpty then Right(Rewritten(prop, Nil))
        else
            val withUnknowns = table.unknowns.foldRight(rewritten)((unknown, inner) =>
                Prop.Forall(unknown.ident, inner)
            )
            Right(
              Rewritten(
                variables.foldRight(withUnknowns)((ident, inner) =>
                    Prop.Forall(ident.asInstanceOf[PropExpr.Ident[Any]], inner)
                ),
                table.unknowns
              )
            )
    }

    /** The prelude's `Option`, of which an unknown is: `None` where its call fails. */
    private lazy val option: DataDecl =
        SirTypes.unwrap(
          Props.variableType(
            scalus.compiler.compile((o: scalus.cardano.onchain.plutus.prelude.Option[BigInt]) => o)
          )
        ) match
            case SIRType.SumCaseClass(declaration, _) => declaration
            case other =>
                throw IllegalStateException(s"the prelude's Option compiles to ${other.show}")

    /** Replays the refutation `proof` of the statement with unknowns on the statement as written:
      * where the statement holds there, the unknowns had values their walks do not return, and the
      * refutation is spurious. None where it refutes the statement as written.
      */
    private def replayed(prepared: Prepared, proof: Proof): Option[ExecutionResult] =
        (proof.artifact, prepared.asWritten) match
            case (artifact: UplcBlaster.Artifact, Some(asWritten)) =>
                UplcBlaster.holdsAsWrittenOn(asWritten, artifact.counterexample.toMap) match
                    case Right(Some(false)) => None
                    case Right(Some(true)) =>
                        Some(
                          inconclusive(
                            "Lean's counterexample gives the unknowns values that their walks do " +
                                "not return: the statement as written holds there " +
                                s"(${described(prepared.unknowns)})"
                          )
                        )
                    case Right(None) =>
                        Some(
                          inconclusive(
                            "replaying Lean's counterexample on the statement as written " +
                                "exhausted the replay budget"
                          )
                        )
                    case Left(reason) =>
                        Some(
                          inconclusive(
                            s"Lean's counterexample is not replayed on the statement as written: $reason"
                          )
                        )
            case _ =>
                Some(
                  inconclusive(
                    "Lean's counterexample of the statement with unknowns is not replayed on the " +
                        "statement as written, which is not lowered"
                  )
                )

    private def inconclusive(reason: String): ExecutionResult =
        VerificationResult.Inconclusive(reason)

    /** The unknowns, as their calls are written. */
    def described(unknowns: List[Unknown]): String =
        unknowns.map(u => s"${u.ident.name} = ${u.written}").mkString("; ")

    // The leaves

    private def rewriteProp(prop: Prop, leaf: SIR => SIR): Prop = {
        def expr[A](e: PropExpr[A]): PropExpr[A] = e match
            case PropExpr.SIRExpr(sir) => PropExpr.SIRExpr(leaf(sir))
            case other                 => other
        def go(p: Prop): Prop = p match
            case Prop.Bool(e)       => Prop.Bool(expr(e))
            case Prop.Denotes(e)    => Prop.Denotes(expr(e))
            case Prop.Equal(a, b)   => Prop.Equal(expr(a), expr(b))
            case Prop.And(a, b)     => Prop.And(go(a), go(b))
            case Prop.Or(a, b)      => Prop.Or(go(a), go(b))
            case Prop.Implies(a, b) => Prop.Implies(go(a), go(b))
            case Prop.Rejects(a, b) => Prop.Rejects(go(a), go(b))
            case Prop.Iff(a, b)     => Prop.Iff(go(a), go(b))
            case Prop.Not(a)        => Prop.Not(go(a))
            // A quantifier inside the body binds variables that the leading ones are not: its
            // programs keep their walks.
            case other => other
        go(prop)
    }

    /** A definition the linker put in a program: a module's function or value. */
    private final case class Definition(name: String, value: SIR, recursive: Boolean)

    private def isModuleName(name: String): Boolean = name.contains("$.")

    /** `sir`, a test's program, with its walks as unknowns: `table` keeps them for all the leaves.
      */
    private def rewriteLeaf(
        sir: SIR,
        variables: Set[String],
        library: String => Boolean,
        table: Unknowns
    ): SIR = {
        val definitions = mutable.LinkedHashMap.empty[String, Definition]
        var declared = Set.empty[String]
        def peel(current: SIR): (SIR => SIR, SIR) = current match
            case SIR.Decl(data, term) =>
                declared += data.name
                val (rebuild, expression) = peel(term)
                (inner => SIR.Decl(data, rebuild(inner)), expression)
            case SIR.Let(bindings, body, flags, anns)
                if bindings.nonEmpty && bindings.forall(b => isModuleName(b.name)) =>
                bindings.foreach(b =>
                    definitions(b.name) = Definition(b.name, b.value, flags.isRec)
                )
                val (rebuild, expression) = peel(body)
                (inner => SIR.Let(bindings, rebuild(inner), flags, anns), expression)
            case other => (identity, other)
        val (rebuild, expression) = peel(sir)
        val loops = loopingDefinitions(definitions.values.toList)
        val leaf = Leaf(definitions.toMap, loops, library, table)
        val initial = variables.map(name => name -> Key.Variable(name)).toMap
        val rewritten = rebuild(leaf.goSir(expression, initial))
        if table.unknowns.nonEmpty && !declared.contains(optionName) then
            SIR.Decl(table.option, rewritten)
        else rewritten
    }

    /** The definitions whose code loops: a recursive one, one with a recursive `let`, and one that
      * calls one of those.
      */
    private def loopingDefinitions(definitions: List[Definition]): Set[String] = {
        val names = definitions.map(_.name).toSet
        val direct = definitions.filter(d => d.recursive || hasRecursiveLet(d.value)).map(_.name)
        val calls = definitions.map(d => d.name -> referenced(d.value, names)).toMap
        var loops = direct.toSet
        var changed = true
        while changed do
            val more = definitions
                .map(_.name)
                .filter(name => !loops(name) && calls(name).exists(loops))
            changed = more.nonEmpty
            loops ++= more
        loops
    }

    private def hasRecursiveLet(sir: SIR): Boolean = exists(sir) {
        case SIR.Let(_, _, flags, _) => flags.isRec
        case _                       => false
    }

    private def referenced(sir: SIR, names: Set[String]): Set[String] = {
        val found = mutable.Set.empty[String]
        exists(sir) {
            case SIR.Var(name, _, _) if names(name)            => found += name; false
            case SIR.ExternalVar(_, name, _, _) if names(name) => found += name; false
            case _                                             => false
        }
        found.toSet
    }

    /** Whether some node of `sir` is one `p` holds of. */
    private def exists(sir: SIR)(p: SIR => Boolean): Boolean = {
        def go(s: SIR): Boolean = p(s) || (s match
            case SIR.Decl(_, term)                 => go(term)
            case SIR.Let(bindings, body, _, _)     => bindings.exists(b => go(b.value)) || go(body)
            case SIR.LamAbs(_, term, _, _)         => go(term)
            case SIR.Apply(f, arg, _, _)           => go(f) || go(arg)
            case SIR.Select(scrutinee, _, _, _)    => go(scrutinee)
            case SIR.And(a, b, _)                  => go(a) || go(b)
            case SIR.Or(a, b, _)                   => go(a) || go(b)
            case SIR.Not(a, _)                     => go(a)
            case SIR.IfThenElse(c, t, f, _, _)     => go(c) || go(t) || go(f)
            case SIR.Error(msg, _, _)              => go(msg)
            case SIR.Constr(_, _, args, _, _)      => args.exists(go)
            case SIR.Match(scrutinee, cases, _, _) => go(scrutinee) || cases.exists(c => go(c.body))
            case SIR.Cast(term, _, _)              => go(term)
            case _                                 => false)
        go(sir)
    }

    private def spine(sir: SIR): (SIR, List[SIR]) = sir match
        case SIR.Apply(f, arg, _, _) =>
            val (head, arguments) = spine(f)
            (head, arguments :+ arg)
        case other => (other, Nil)

    private def lambdas(sir: SIR): (List[SIR.LamAbs], SIR) = sir match
        case lambda @ SIR.LamAbs(_, term, _, _) =>
            val (more, body) = lambdas(term)
            (lambda :: more, body)
        case other => (Nil, other)

    /** The unknowns of a statement, by key, shared by its leaves. */
    private final class Unknowns(val option: DataDecl) {
        private val byKey = mutable.LinkedHashMap.empty[Key, Unknown]
        private var values = 0

        def unknowns: List[Unknown] = byKey.values.toList

        private def constructor(suffix: String): ConstrDecl =
            option.constructors
                .find(_.name.endsWith(suffix))
                .getOrElse(throw IllegalStateException(s"the prelude's Option has no $suffix"))

        /** The unknown of `key`, made where there is none, as the program reads it: `None` fails,
          * `Some(v)` is `v`.
          */
        def reference(key: Key, tp: SIRType, written: String, anns: AnnotationsDecl): SIR.Match = {
            val unknown = byKey.getOrElseUpdate(
              key, {
                  val index = byKey.size + 1
                  Unknown(
                    new PropExpr.Ident[Any](
                      s"walk$index",
                      Long.MaxValue - index,
                      SIRType.SumCaseClass(option, List(tp))
                    ),
                    tp,
                    written
                  )
              }
            )
            values += 1
            val value = s"walk-value-$values"
            SIR.Match(
              SIR.Var(unknown.ident.name, unknown.ident.tp, anns),
              List(
                SIR.Case(
                  SIR.Pattern.Constr(constructor("$.None"), Nil, Nil),
                  SIR.Error(s"the walk ${unknown.ident.name} failed", anns),
                  anns
                ),
                SIR.Case(
                  SIR.Pattern.Constr(constructor("$.Some"), List(value), List(unknown.resultType)),
                  SIR.Var(value, unknown.resultType, anns),
                  anns
                )
              ),
              unknown.resultType,
              anns
            )
        }
    }

    /** The rewrite of one leaf, with the definitions linked into it. */
    private final class Leaf(
        definitions: Map[String, Definition],
        loops: Set[String],
        library: String => Boolean,
        table: Unknowns
    ) {
        private var inlined = 0

        private def global(sir: SIR): Option[String] = sir match
            case SIR.Var(name, _, _) if definitions.contains(name)            => Some(name)
            case SIR.ExternalVar(_, name, _, _) if definitions.contains(name) => Some(name)
            case _                                                            => None

        private def arity(name: String): Int = lambdas(definitions(name).value)._1.size

        private def isFunction(tp: SIRType): Boolean = SirTypes.unwrap(tp) match
            case SIRType.Fun(_, _)        => true
            case SIRType.TypeLambda(_, _) => true
            case _                        => false

        def goSir(sir: SIR, values: Map[String, Key]): SIR = sir match
            case SIR.Decl(data, term) => SIR.Decl(data, goSir(term, values))
            case a: AnnotatedSIR      => go(a, values)

        def go(sir: AnnotatedSIR, values: Map[String, Key]): AnnotatedSIR = sir match
            case SIR.Let(List(Binding(self, _, _: SIR.LamAbs)), body, flags, anns)
                if flags.isRec && spine(body)._1.match
                    case SIR.Var(name, _, _) => name == self
                    case _                   => false
                =>
                walk(sir, values, anns).getOrElse(structural(sir, values))
            case SIR.Apply(_, _, tp, anns) =>
                val (head, arguments) = spine(sir)
                // Specifications are no part of a program, and are erased before it is compiled:
                // their clauses' walks would be unknowns of no program. The body of an `ensuring`
                // is the program's.
                nameOf(head) match
                    case Some(EraseSpecifications.Expects | EraseSpecifications.Ensures) => sir
                    case Some(EraseSpecifications.Ensuring) =>
                        sir match
                            case SIR.Apply(SIR.Apply(f, body, bodyTp, bodyAnns), condition, _, _) =>
                                SIR.Apply(
                                  SIR.Apply(f, go(body, values), bodyTp, bodyAnns),
                                  condition,
                                  tp,
                                  anns
                                )
                            case other => other
                    case _ => call(sir, head, arguments, tp, anns, values)
            case _ => structural(sir, values)

        private def nameOf(head: SIR): Option[String] = head match
            case SIR.Var(name, _, _)            => Some(name)
            case SIR.ExternalVar(_, name, _, _) => Some(name)
            case _                              => None

        /** `sir`, a call of `head` on `arguments`: a walk, the code of a function inlined, or the
          * call with its arguments rewritten.
          */
        private def call(
            sir: AnnotatedSIR,
            head: SIR,
            arguments: List[SIR],
            tp: SIRType,
            anns: AnnotationsDecl,
            values: Map[String, Key]
        ): AnnotatedSIR =
            global(head) match
                case Some(name)
                    if loops(name) && arguments.size == arity(name) && !isFunction(tp) =>
                    val definition = definitions(name)
                    if definition.recursive || library(name) then
                        walk(sir, values, anns).getOrElse(structural(sir, values))
                    else
                        inlinedCall(definition, arguments, anns)
                            .fold(structural(sir, values))(go(_, values))
                case _ => structural(sir, values)

        /** The unknown of the walk `sir`, where its key is of the statement's variables. */
        private def walk(
            sir: AnnotatedSIR,
            values: Map[String, Key],
            anns: AnnotationsDecl
        ): Option[AnnotatedSIR] = {
            val key = keyOf(sir, values, Map.empty, 0)
            Option.when(key.isOfVariables)(
              table.reference(key, sir.tp, written(sir), anns)
            )
        }

        private def written(sir: SIR): String = {
            val (head, _) = spine(sir)
            head match
                case SIR.Var(name, _, _)            => name
                case SIR.ExternalVar(_, name, _, _) => name
                case SIR.Let(List(binding), _, _, anns) =>
                    s"a loop (${binding.name}) at ${anns.pos.file}:${anns.pos.startLine + 1}"
                case _ => "a walk"
        }

        /** The code of `definition` applied to `arguments`, each bound to a parameter by a `let` in
          * order. None for a function with type parameters, which a `let` does not bind.
          */
        private def inlinedCall(
            definition: Definition,
            arguments: List[SIR],
            anns: AnnotationsDecl
        ): Option[AnnotatedSIR] = {
            val (parameters, body) = lambdas(definition.value)
            Option.when(parameters.forall(_.typeParams.isEmpty)) {
                inlined += 1
                val renamed =
                    parameters.map(p => p.param.name -> s"${p.param.name}-inlined$inlined")
                val renamedBody = SIR.renameFreeVars(body, renamed.toMap)
                parameters
                    .zip(arguments)
                    .zip(renamed)
                    .foldRight(renamedBody) { case (((p, argument), (_, name)), inner) =>
                        SIR.Let(
                          List(Binding(name, p.param.tp, argument)),
                          inner,
                          SIR.LetFlags.None,
                          anns
                        )
                    }
                    .asInstanceOf[AnnotatedSIR]
            }
        }

        private def structural(sir: AnnotatedSIR, values: Map[String, Key]): AnnotatedSIR =
            sir match
                case SIR.Let(bindings, body, flags, anns) if !flags.isRec =>
                    var scope = values
                    val rewritten = bindings.map { binding =>
                        val value = goSir(binding.value, scope)
                        scope = scope + (binding.name -> keyOf(binding.value, scope, Map.empty, 0))
                        binding.copy(value = value)
                    }
                    SIR.Let(rewritten, goSir(body, scope), flags, anns)
                case SIR.Let(bindings, body, flags, anns) =>
                    val scope = values ++ bindings.map(b => b.name -> Key.Free(b.name))
                    SIR.Let(
                      bindings.map(b => b.copy(value = goSir(b.value, scope))),
                      goSir(body, scope),
                      flags,
                      anns
                    )
                case SIR.LamAbs(param, term, typeParams, anns) =>
                    SIR.LamAbs(
                      param,
                      goSir(term, values + (param.name -> Key.Free(param.name))),
                      typeParams,
                      anns
                    )
                case SIR.Apply(f, arg, tp, anns) =>
                    SIR.Apply(go(f, values), go(arg, values), tp, anns)
                case SIR.Select(scrutinee, field, tp, anns) =>
                    SIR.Select(goSir(scrutinee, values), field, tp, anns)
                case SIR.And(a, b, anns) => SIR.And(go(a, values), go(b, values), anns)
                case SIR.Or(a, b, anns)  => SIR.Or(go(a, values), go(b, values), anns)
                case SIR.Not(a, anns)    => SIR.Not(go(a, values), anns)
                case SIR.IfThenElse(c, t, f, tp, anns) =>
                    SIR.IfThenElse(go(c, values), go(t, values), go(f, values), tp, anns)
                case SIR.Error(msg, anns, cause) => SIR.Error(go(msg, values), anns, cause)
                case SIR.Constr(name, data, args, tp, anns) =>
                    SIR.Constr(name, data, args.map(goSir(_, values)), tp, anns)
                case SIR.Match(scrutinee, cases, tp, anns) =>
                    val matched = keyOf(scrutinee, values, Map.empty, 0)
                    SIR.Match(
                      go(scrutinee, values),
                      cases.map(c => c.copy(body = goSir(c.body, withPattern(values, c, matched)))),
                      tp,
                      anns
                    )
                case SIR.Cast(term, tp, anns) => SIR.Cast(go(term, values), tp, anns)
                case other                    => other

        private def withPattern(
            values: Map[String, Key],
            c: SIR.Case,
            matched: Key
        ): Map[String, Key] =
            c.pattern match
                case SIR.Pattern.Constr(constructor, bindings, _) =>
                    values ++ bindings.zipWithIndex.map((name, index) =>
                        name -> Key.Projection(constructor.name, index, matched)
                    )
                case _ => values

        /** The key of `sir`: `values` has the keys of the variables the program binds, and `bound`
          * the depth of those the key binds itself.
          */
        def keyOf(sir: SIR, values: Map[String, Key], bound: Map[String, Int], depth: Int): Key =
            sir match
                case SIR.Decl(_, term) => keyOf(term, values, bound, depth)
                case SIR.Var(name, _, _) =>
                    bound.get(name) match
                        case Some(level) => Key.Bound(level)
                        case None =>
                            values.getOrElse(
                              name,
                              if definitions.contains(name) || isModuleName(name) then
                                  Key.Global(name)
                              else Key.Free(name)
                            )
                case SIR.ExternalVar(_, name, _, _) => Key.Global(name)
                case SIR.Const(constant, _, _)      => Key.Literal(constant)
                case SIR.Builtin(bn, _, _)          => Key.Builtin(bn.toString)
                case SIR.Apply(_, _, _, _) =>
                    val (head, arguments) = spine(sir)
                    val function = keyOf(head, values, bound, depth)
                    val keys = arguments.map(keyOf(_, values, bound, depth))
                    (function, keys) match
                        // A value of a type whose `Data` is the value, encoded and decoded again.
                        case (
                              Key.Global(`fromData`),
                              List(Key.Applied(Key.Global(`toData`), List(of)))
                            ) =>
                            of
                        case _ => Key.Applied(function, keys)
                case SIR.LamAbs(_, _, _, _) =>
                    val (parameters, body) = lambdas(sir)
                    val inner =
                        bound ++ parameters.zipWithIndex.map((p, i) => p.param.name -> (depth + i))
                    Key.Lambda(parameters.size, keyOf(body, values, inner, depth + parameters.size))
                case SIR.Let(bindings, body, flags, _) if flags.isRec =>
                    val inner = bound ++ bindings.zipWithIndex.map((b, i) => b.name -> (depth + i))
                    val next = depth + bindings.size
                    bindings
                        .map(b => keyOf(b.value, values, inner, next))
                        .foldRight(keyOf(body, values, inner, next))((f, b) => Key.Recursive(f, b))
                case SIR.Let(bindings, body, _, _) =>
                    val scope = bindings.foldLeft((values, bound)) { case ((vs, bs), binding) =>
                        (
                          vs + (binding.name -> keyOf(binding.value, vs, bs, depth)),
                          bs - binding.name
                        )
                    }
                    keyOf(body, scope._1, scope._2, depth)
                case SIR.Select(scrutinee, field, _, _) =>
                    Key.Field(keyOf(scrutinee, values, bound, depth), field)
                case SIR.Constr(name, _, args, _, _) =>
                    Key.Construct(name, args.map(keyOf(_, values, bound, depth)))
                case SIR.Match(scrutinee, cases, _, _) =>
                    val matched = keyOf(scrutinee, values, bound, depth)
                    Key.Cases(
                      matched,
                      cases.map { c =>
                          val name = c.pattern match
                              case SIR.Pattern.Constr(constructor, _, _) => constructor.name
                              case SIR.Pattern.Const(value)              => value.uplcConst.toString
                              case SIR.Pattern.Wildcard                  => "_"
                          val patternBindings = c.pattern match
                              case SIR.Pattern.Constr(_, names, _) => names
                              case _                               => Nil
                          name -> keyOf(
                            c.body,
                            withPattern(values, c, matched),
                            bound -- patternBindings,
                            depth
                          )
                      }
                    )
                case SIR.IfThenElse(c, t, f, _, _) =>
                    Key.Choice(
                      keyOf(c, values, bound, depth),
                      keyOf(t, values, bound, depth),
                      keyOf(f, values, bound, depth)
                    )
                case SIR.And(a, b, _) =>
                    Key.Conjunction(keyOf(a, values, bound, depth), keyOf(b, values, bound, depth))
                case SIR.Or(a, b, _) =>
                    Key.Disjunction(keyOf(a, values, bound, depth), keyOf(b, values, bound, depth))
                case SIR.Not(a, _)        => Key.Negation(keyOf(a, values, bound, depth))
                case SIR.Error(_, _, _)   => Key.Failure
                case SIR.Cast(term, _, _) => keyOf(term, values, bound, depth)
    }
}
