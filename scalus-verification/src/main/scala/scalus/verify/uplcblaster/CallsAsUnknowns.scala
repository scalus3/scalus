package scalus.verify.uplcblaster

import scalus.compiler.sir.*
import scalus.compiler.sir.transform.EraseSpecifications
import scalus.uplc.Constant
import scalus.verify.*

import java.nio.file.Files
import scala.collection.mutable
import scala.util.hashing.MurmurHash3

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
  * The tie of a program to its form with unknowns is trusted: see [[CallsAsUnknowns.rewrite]]. A
  * proof says so by its kind, [[ProofKind.BlasterOfUnknowns]].
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

    override val name: String = s"${inner.name}-unknowns"

    /** The same tactic over `f` of its inner one: as a suite gives its own mapping and the
      * directory of its checks to every tactic it runs.
      */
    def withInner(f: UplcBlaster => UplcBlaster): CallsAsUnknowns =
        new CallsAsUnknowns(f(inner), library)

    override def prepare(goal: Goal): Either[CompatibilityReport, Prepared] =
        CallsAsUnknowns.rewrite(goal.statement.prop, library) match {
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
                        new CallsAsUnknowns.Prepared(
                          tactic,
                          CallsAsUnknowns.unrestricted(lowered, rewritten.unknowns),
                          inner.prepare(goal).toOption,
                          rewritten.unknowns
                        )
                    )
        }

    override def run(prepared: Prepared, earlier: Option[Kept]): ExecutionResult =
        CallsAsUnknowns.finished(prepared, prepared.tactic.run(prepared.lowered, earlier))

    // The statement with unknowns is the one checked, and its programs are what a result rests on.
    // Its plan is the statement's own, and no test's: the unknowns of records are `Data`.

    override def fingerprint(prepared: Prepared): Option[Fingerprint] =
        prepared.tactic.servers.workspace
            .filter(workspace => Files.isRegularFile(workspace.resolve("lean-toolchain")))
            .map(UplcBlaster.fingerprint(prepared.lowered, _))

    override def identity(prepared: Prepared): Option[(String, List[String])] =
        Some(UplcBlaster.identity(prepared.lowered))

    override def keep(prepared: Prepared, result: ExecutionResult): Option[Kept] =
        UplcBlaster.kept(prepared.lowered, result)

    override def restore(prepared: Prepared, kept: Kept): Either[String, ExecutionResult] =
        UplcBlaster.restored(prepared.lowered, kept).map(CallsAsUnknowns.finished(prepared, _))

    override def restoreOnAnotherSide(
        prepared: Prepared,
        kept: Kept
    ): Either[String, ExecutionResult] =
        UplcBlaster
            .restoredOnAnotherSide(prepared.lowered, kept)
            .map(CallsAsUnknowns.finished(prepared, _))
}

object CallsAsUnknowns {

    /** A statement prepared with its walks as unknowns.
      *
      * @param tactic
      *   the inner tactic, with the unknowns of records as `Data`
      * @param lowered
      *   the statement with unknowns, lowered by `tactic`
      * @param lowerAsWritten
      *   the statement as written, lowered, on which a refutation is replayed; none where it cannot
      *   be lowered, and then no refutation is taken. It is lowered only for a refutation.
      * @param unknowns
      *   the calls made unknowns
      */
    final class Prepared(
        val tactic: UplcBlaster,
        val lowered: UplcBlaster.Lowered,
        lowerAsWritten: => Option[UplcBlaster.Lowered],
        val unknowns: List[Unknown]
    ) {
        lazy val asWritten: Option[UplcBlaster.Lowered] = lowerAsWritten
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

    /** What a call computes, of the statement's variables: two calls of one key compute one value,
      * or both fail. A key keeps what may fail of a call: a `let` inside it is one ([[Bind]]),
      * whether its body uses it or not. Variables bound in the key itself are numbered by depth
      * ([[Bound]]), so that two copies of one function have one key. A key with a [[Free]]
      * variable, bound by a lambda outside the call, is of no value of the statement's variables,
      * and one of no variable is of a constant: no unknown is made of either ([[hoistable]]).
      *
      * Keys share their parts, as the program's `let`s do, so a key caches its hash and what it has
      * of variables, which a walk over it would otherwise visit once for each path to a part.
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
        case Bind(value: Key, body: Key)

        /** `toData` of a value of the type `tp` ([[SirTypes.typeKey]]). */
        case Encoded(of: Key, tp: String)

        /** `fromData` to the type `tp`, of a `Data` that is not one of that type encoded. */
        case Decoded(of: Key, tp: String)
        case Choice(condition: Key, ifTrue: Key, ifFalse: Key)
        case Cases(scrutinee: Key, cases: List[(String, Key)])
        case Conjunction(a: Key, b: Key)
        case Disjunction(a: Key, b: Key)
        case Negation(a: Key)
        case Failure

        private def parts: List[Key] = this match {
            case Field(of, _)              => List(of)
            case Projection(_, _, of)      => List(of)
            case Applied(function, args)   => function :: args
            case Construct(_, args)        => args
            case Lambda(_, body)           => List(body)
            case Recursive(function, body) => List(function, body)
            case Bind(value, body)         => List(value, body)
            case Encoded(of, _)            => List(of)
            case Decoded(of, _)            => List(of)
            case Choice(c, t, f)           => List(c, t, f)
            case Cases(scrutinee, cases)   => scrutinee :: cases.map(_._2)
            case Conjunction(a, b)         => List(a, b)
            case Disjunction(a, b)         => List(a, b)
            case Negation(a)               => List(a)
            case _                         => Nil
        }

        private lazy val cachedHash: Int =
            MurmurHash3.orderedHash(productIterator, productPrefix.hashCode)

        override def hashCode(): Int = cachedHash

        /** Whether a variable a lambda outside the key binds is in it. */
        lazy val hasFree: Boolean = this match
            case Free(_) => true
            case _       => parts.exists(_.hasFree)

        /** Whether a variable of the statement is in it. */
        lazy val hasVariable: Boolean = this match
            case Variable(_) => true
            case _           => parts.exists(_.hasVariable)

        /** Whether it is of the statement's variables alone, and of one at least. */
        def hoistable: Boolean = hasVariable && !hasFree
    }

    private val optionName = "scalus.cardano.onchain.plutus.prelude.Option"

    private val fromData = "scalus.uplc.builtin.internal.UniversalDataConversion$.fromData"
    private val toData = "scalus.uplc.builtin.internal.UniversalDataConversion$.toData"

    /** `prop` with its walks as unknowns.
      *
      * A statement's leading `forAll`s are its variables. In each program of its body, a call is a
      * walk where it loops: a recursive `let` applied to all its arguments, as an inline helper's
      * local `go`, or a call of a function of `library`, or of a recursive one, whose code loops,
      * applied to all its arguments, and not to a function. A call of another function whose code
      * loops is inlined, its parameters bound by `let`s in the order the call evaluates them, so
      * that the walks in its code are found. A walk whose key is of the variables, of the values
      * the program binds of them and of earlier walks is replaced by an unknown, a variable added
      * after the statement's own: at the call, `None` fails, as the walk would, and `Some(v)` is
      * `v`. A walk under a lambda, of the lambda's parameter, stays, as does a walk of constants.
      *
      * The tie is trusted, of three facts. A walk ends on every value, as a walk over a list does.
      * Two calls of one key compute one value, or both fail: a key is the call with each variable
      * the program bound before it replaced by the value it bound, which it has, each pattern
      * variable by its field of the value matched, and `fromData` of `toData` of a value of a type
      * to that type by the value; a `let` inside the call is kept. A program behaves as its form
      * with unknowns, each the result of its call. The statement holds where it holds for every
      * value of the unknowns.
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

    /** Whether a value of `tp` is one Lean variable: an integer, a Boolean, a byte string, a string
      * or the unit. Another type's unknown is one `Data`.
      */
    private def isScalar(tp: SIRType): Boolean = SirTypes.unwrap(tp) match
        case SIRType.Integer | SIRType.Boolean | SIRType.ByteString | SIRType.String |
            SIRType.Unit =>
            true
        case _ => false

    /** `lowered` with its unknowns standing for every value Lean has, of their types or not: no
      * premise of an unknown's type, and a value of one is never outside it. A statement of every
      * value of an unknown holds of the values its call returns, which are of the type. Lean then
      * walks no list of an unknown's value to check that it is one of the type's.
      */
    private def unrestricted(
        lowered: UplcBlaster.Lowered,
        unknowns: List[Unknown]
    ): UplcBlaster.Lowered = {
        val names = unknowns.map(_.ident.name).toSet
        def ofUnknown(name: String): Boolean =
            names.exists(n => name == n || name.startsWith(s"$n."))
        def freed(goal: UplcBlaster.Lowered): UplcBlaster.Lowered = {
            val indices = goal.binders.indices.filter(i => ofUnknown(goal.binders(i).name)).toSet
            goal.copy(
              predicates = goal.predicates.zipWithIndex.map((premise, index) =>
                  if indices(index) then None else premise
              ),
              unrestricted = indices
            )
        }
        freed(lowered).copy(unrewritten = lowered.unrewritten.map(freed))
    }

    /** `result` of the statement with unknowns, as a result of the statement: a proof is of the
      * kind that says it rests on the unknowns, and a refutation is replayed on the statement as
      * written.
      */
    private def finished(prepared: Prepared, result: ExecutionResult): ExecutionResult =
        result match {
            case VerificationResult.Proven(proof) => VerificationResult.Proven(ofUnknowns(proof))
            case VerificationResult.ProvenWithin(proof, bound) =>
                VerificationResult.ProvenWithin(ofUnknowns(proof), bound)
            case refuted @ VerificationResult.Refuted(proof) =>
                replayed(prepared, proof).getOrElse(refuted)
            case other => other
        }

    private def ofUnknowns(proof: Proof): Proof = proof.artifact match
        case artifact: UplcBlaster.Artifact =>
            Proof(artifact.copy(kind = ProofKind.BlasterOfUnknowns), proof.usedLemmas)
        case _ => proof

    /** Replays the refutation `proof` of the statement with unknowns on the statement as written:
      * where the statement holds there, the unknowns had values their walks do not return, and the
      * refutation is spurious. None where it refutes the statement as written.
      */
    private def replayed(prepared: Prepared, proof: Proof): Option[ExecutionResult] =
        (proof.artifact, prepared.asWritten) match {
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
        }

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

    /** Whether `name` is of a module's definition, as
      * `scalus.cardano.onchain.plutus.v1.Value$.zero`. Not
      * [[scalus.compiler.sir.linking.SIRLinker.isDefinition]], which takes any qualified name: a
      * program's own values have qualified names too, as the plugin's
      * `_Validator.scala_15_8_match_scrutinee_1`, and a test's program begins with them.
      */
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

    private def hasRecursiveLet(sir: SIR): Boolean =
        SIR.accumulate[Boolean](
          sir,
          false,
          Set.empty,
          (node, _, found) =>
              found || (node match
                  case SIR.Let(_, _, flags, _) => flags.isRec
                  case _                       => false)
        )

    /** The definitions of `names` that `sir` refers to, and not a local value of one's name. */
    private def referenced(sir: SIR, names: Set[String]): Set[String] =
        SIR.accumulate[Set[String]](
          sir,
          Set.empty,
          Set.empty,
          (node, locals, found) =>
              node match
                  case SIR.Var(name, _, _) if names(name) && !locals(name) => found + name
                  case SIR.ExternalVar(_, name, _, _) if names(name)       => found + name
                  case _                                                   => found
        )

    private def spine(sir: SIR): (SIR, List[SIR]) = sir match
        case SIR.Apply(f, arg, _, _) =>
            val (head, arguments) = spine(f)
            (head, arguments :+ arg)
        case other => (other, Nil)

    /** The lambdas `sir` begins with, and what they are of. */
    private def lambdas(sir: SIR): (List[SIR.LamAbs], SIR) = sir match
        case lambda @ SIR.LamAbs(_, term, _, _) =>
            val (more, body) = lambdas(term)
            (lambda :: more, body)
        case other => (Nil, other)

    /** The parameters of a function's value, through a declaration or a cast around it. */
    private def arity(value: SIR): Int = value match
        case SIR.Decl(_, term)         => arity(term)
        case SIR.Cast(term, _, _)      => arity(term)
        case SIR.LamAbs(_, term, _, _) => 1 + lambdas(term)._1.size
        case _                         => 0

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

        private def nameOf(head: SIR): Option[String] = head match
            case SIR.Var(name, _, _)            => Some(name)
            case SIR.ExternalVar(_, name, _, _) => Some(name)
            case _                              => None

        /** Whether a call's value can be an unknown, a Lean variable of data: it is no function,
          * plain or polymorphic ([[SIRType.isPolyFunOrFun]]), which a program cannot read from such
          * a variable. A call applies a function to its arguments, so its type is no type lambda of
          * another kind.
          */
        private def isValueType(tp: SIRType): Boolean = !SIRType.isPolyFunOrFun(tp)

        /** Whether `body` applies the local function `self` to all its `parameters`. */
        private def appliesFully(body: SIR, self: String, parameters: Int): Boolean =
            spine(body) match
                case (SIR.Var(name, _, _), arguments) =>
                    name == self && parameters > 0 && arguments.size == parameters
                case _ => false

        def goSir(sir: SIR, values: Map[String, Key]): SIR = sir match
            case SIR.Decl(data, term) => SIR.Decl(data, goSir(term, values))
            case a: AnnotatedSIR      => go(a, values)

        def go(sir: AnnotatedSIR, values: Map[String, Key]): AnnotatedSIR = sir match {
            case SIR.Let(List(Binding(self, _, function: SIR.LamAbs)), body, flags, anns)
                if flags.isRec && isValueType(sir.tp) &&
                    appliesFully(body, self, lambdas(function)._1.size) =>
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
        }

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
                    if loops(name) && arguments.size == arity(definitions(name).value) &&
                        isValueType(tp) =>
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
            Option.when(key.hoistable)(table.reference(key, sir.tp, written(sir), anns))
        }

        private def written(sir: SIR): String = spine(sir)._1 match
            case SIR.Var(name, _, _)            => name
            case SIR.ExternalVar(_, name, _, _) => name
            case SIR.Let(List(binding), _, _, anns) =>
                s"a loop (${binding.name}) at ${anns.pos.file}:${anns.pos.startLine + 1}"
            case _ => "a walk"

        /** The code of `definition` applied to `arguments`, each bound to a parameter by a `let` in
          * order. None for a function with type parameters, which a `let` does not bind, and for
          * one whose value is not its lambdas.
          */
        private def inlinedCall(
            definition: Definition,
            arguments: List[SIR],
            anns: AnnotationsDecl
        ): Option[AnnotatedSIR] = {
            val (parameters, body) = lambdas(definition.value)
            Option.when(
              parameters.nonEmpty && parameters.size == arguments.size &&
                  parameters.forall(_.typeParams.isEmpty)
            ) {
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
            sir match {
                // A value the program binds is the value of its key where a later call is made: the
                // program evaluates the `let` before it goes on.
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
            }

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

        /** The key of a call of the definition `name`, where it does not loop: the key of its code
          * on `arguments`, so that a function called and its code written out have one key, as the
          * clause's `configOf(datum)` and the validator's `datum.getOrFail(...).to[Config]`. An
          * argument that is a value, a variable, a constant or a lambda, ends without failing, and
          * stands for its parameter. Another is bound to it as a `let` in a call is, and stays in
          * the key, as it may fail. None for a definition that loops, which is a walk, for one with
          * type parameters, and for a call of another number of arguments.
          */
        private def unfolded(
            name: String,
            arguments: List[SIR],
            values: Map[String, Key],
            bound: Map[String, Int],
            depth: Int
        ): Option[Key] =
            definitions.get(name).filterNot(_ => loops(name)).flatMap { definition =>
                val (parameters, body) = lambdas(definition.value)
                Option.when(
                  parameters.nonEmpty && parameters.size == arguments.size &&
                      parameters.forall(_.typeParams.isEmpty)
                ) {
                    // The definition's code is closed but for its parameters and other definitions:
                    // the names of the caller are not in its scope.
                    def nest(
                        rest: List[(SIR.LamAbs, SIR)],
                        scope: Map[String, Key],
                        inner: Map[String, Int],
                        level: Int
                    ): Key = rest match
                        case Nil => keyOf(body, scope, inner, level)
                        case (parameter, argument) :: more =>
                            val p = parameter.param.name
                            val of = keyOf(argument, values, bound, depth)
                            argument match
                                case _: SIR.Var | _: SIR.ExternalVar | _: SIR.Const |
                                    _: SIR.LamAbs =>
                                    nest(more, scope + (p -> of), inner - p, level)
                                case _ =>
                                    Key.Bind(
                                      of,
                                      nest(more, scope - p, inner + (p -> level), level + 1)
                                    )
                    nest(parameters.zip(arguments), Map.empty, Map.empty, depth)
                }
            }

        /** The key of `sir`: `values` has the keys of the variables the program binds, and `bound`
          * the depth of those the key binds itself.
          */
        def keyOf(sir: SIR, values: Map[String, Key], bound: Map[String, Int], depth: Int): Key =
            sir match {
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
                case SIR.Apply(_, _, tp, _) =>
                    val (head, arguments) = spine(sir)
                    val function = keyOf(head, values, bound, depth)
                    val keys = arguments.map(keyOf(_, values, bound, depth))
                    (function, arguments, keys) match
                        case (Key.Global(`toData`), List(argument), List(of)) =>
                            Key.Encoded(of, SirTypes.typeKey(argument.tp))
                        // A value of a type, encoded and decoded to that type again.
                        case (Key.Global(`fromData`), List(_), List(of)) =>
                            val decodedTo = SirTypes.typeKey(tp)
                            of match
                                case Key.Encoded(value, encodedFrom) if encodedFrom == decodedTo =>
                                    value
                                case _ => Key.Decoded(of, decodedTo)
                        case (Key.Global(name), _, _) =>
                            unfolded(name, arguments, values, bound, depth)
                                .getOrElse(Key.Applied(function, keys))
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
                // A `let` inside a call is evaluated before its body, used or not: it is kept, so
                // that a call whose `let` may fail has another key than one that has none.
                case SIR.Let(bindings, body, _, _) =>
                    def nest(rest: List[Binding], scope: Map[String, Int], level: Int): Key =
                        rest match
                            case Nil => keyOf(body, values, scope, level)
                            case binding :: more =>
                                Key.Bind(
                                  keyOf(binding.value, values, scope, level),
                                  nest(more, scope + (binding.name -> level), level + 1)
                                )
                    nest(bindings, bound, depth)
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
}
