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
  * Where the function of a walk states a contract in its code, with `ensuring` or `Spec.ensures`,
  * its unknown is not any value: the statement assumes the contract of it, as Hoare logic's rule
  * for a call does (docs/design/verification-details/call-rule.md). A proof then rests on the
  * contract, and names its clauses ([[Proof.premises]]).
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
  * @param contracts
  *   whether the statement assumes the contracts the functions it calls state: with none, a proof
  *   rests on no contract, and a call is an unknown only where it is a walk ([[withoutContracts]])
  */
final class CallsAsUnknowns(
    val inner: UplcBlaster,
    library: String => Boolean,
    contracts: Boolean
) extends Tactic {

    def this(inner: UplcBlaster, library: String => Boolean) = this(inner, library, true)

    def this(inner: UplcBlaster) = this(inner, CallsAsUnknowns.inLibrary, true)

    /** The same tactic, assuming no contract: for a statement that needs none, so that its proof
      * rests on none.
      */
    def withoutContracts: CallsAsUnknowns = new CallsAsUnknowns(inner, library, false)

    override type Prepared = CallsAsUnknowns.Prepared

    override val name: String = s"${inner.name}-unknowns"

    /** The same tactic over `f` of its inner one: as a suite gives its own mapping and the
      * directory of its checks to every tactic it runs.
      */
    def withInner(f: UplcBlaster => UplcBlaster): CallsAsUnknowns =
        new CallsAsUnknowns(f(inner), library, contracts)

    override def prepare(goal: Goal): Either[CompatibilityReport, Prepared] =
        CallsAsUnknowns.rewrite(goal.statement.prop, library, contracts) match {
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
                          rewritten.unknowns,
                          rewritten.assumed
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
      * @param assumed
      *   the clauses of the contracts the statement with unknowns assumes
      */
    final class Prepared(
        val tactic: UplcBlaster,
        val lowered: UplcBlaster.Lowered,
        lowerAsWritten: => Option[UplcBlaster.Lowered],
        val unknowns: List[Unknown],
        val assumed: List[Clause]
    ) {
        lazy val asWritten: Option[UplcBlaster.Lowered] = lowerAsWritten
    }

    /** A call made an unknown: the statement's variable, of an `Option` of the call's type, which
      * is `None` where the call fails, and the call as written.
      */
    final case class Unknown(ident: PropExpr.Ident[Any], resultType: SIRType, written: String)

    /** A statement with its walks as unknowns, the calls made unknowns, and the clauses of the
      * contracts it assumes of them.
      */
    final case class Rewritten(prop: Prop, unknowns: List[Unknown], assumed: List[Clause])

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
      * A call of a function whose code states a contract is a walk whether it loops or not, as the
      * call rule replaces it. The statement assumes the contract of its unknown, a premise of its
      * own: `match u { None => true; Some(r) => let p = a in (!expects(p) || ensures(p, r)) }`,
      * with the values the program binds that the arguments `a` read bound around it again. A call
      * in the premise is rewritten as one in a program: so a walk in the arguments, which the
      * unknown of the call stands for, gets an unknown and a premise of its own, and the premise of
      * `minus` speaks of the `getLovelace` of its result, which a clause reads. A premise holds of
      * the values the walks return, by the contract, so the statement holds where it holds for
      * every value of the unknowns that its premises allow. A function whose code is a call of
      * another, on all its arguments, as `valueEq` of `eq`, has the other's contract. Premises are
      * made inside premises up to [[mostNested]] deep.
      *
      * The tie is trusted, of three facts. A walk ends on every value, as a walk over a list does.
      * Two calls of one key compute one value, or both fail: a key is the call with each variable
      * the program bound before it replaced by the value it bound, which it has, each pattern
      * variable by its field of the value matched, and `fromData` of `toData` of a value of a type
      * to that type by the value; a `let` inside the call is kept. A program behaves as its form
      * with unknowns, each the result of its call. The statement holds where it holds for every
      * value of the unknowns.
      */
    def rewrite(prop: Prop, library: String => Boolean): Either[String, Rewritten] =
        rewrite(prop, library, contracts = true)

    /** [[rewrite]], with the contracts of the functions the statement calls as premises, or, where
      * `contracts` is false, none: a call of a function with a contract is then an unknown only
      * where it is a walk.
      */
    def rewrite(
        prop: Prop,
        library: String => Boolean,
        contracts: Boolean
    ): Either[String, Rewritten] = {
        def prefix(p: Prop, variables: List[PropExpr.Ident[?]]): (List[PropExpr.Ident[?]], Prop) =
            p match
                case Prop.Forall(ident, body) => prefix(body, ident :: variables)
                case other                    => (variables.reverse, other)
        val (variables, body) = prefix(prop, Nil)
        val table = Unknowns(option)
        val names = variables.map(_.name).toSet
        val rewritten =
            rewriteProp(body, sir => rewriteLeaf(sir, names, library, contracts, table))
        // A statement with no walk is left as it is, its specifications among it.
        if table.unknowns.isEmpty then Right(Rewritten(prop, Nil, Nil))
        else
            val assuming = table.premises.foldRight(rewritten)((premise, inner) =>
                Prop.Implies(premise.prop, inner)
            )
            val withUnknowns = table.unknowns.foldRight(assuming)((unknown, inner) =>
                Prop.Forall(unknown.ident, inner)
            )
            Right(
              Rewritten(
                variables.foldRight(withUnknowns)((ident, inner) =>
                    Prop.Forall(ident.asInstanceOf[PropExpr.Ident[Any]], inner)
                ),
                table.unknowns,
                table.premises.flatMap(_.clauses).distinct
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
            case VerificationResult.Proven(proof) =>
                VerificationResult.Proven(ofUnknowns(proof, prepared.assumed))
            case VerificationResult.ProvenWithin(proof, bound) =>
                VerificationResult.ProvenWithin(ofUnknowns(proof, prepared.assumed), bound)
            case refuted @ VerificationResult.Refuted(proof) =>
                replayed(prepared, proof).getOrElse(refuted)
            case other => other
        }

    /** `proof`, of the statement with unknowns: of the kind that says so, and resting on the
      * contracts `assumed`.
      */
    private def ofUnknowns(proof: Proof, assumed: List[Clause]): Proof = {
        val artifact = proof.artifact match
            case artifact: UplcBlaster.Artifact => artifact.copy(kind = ProofKind.BlasterOfUnknowns)
            case other                          => other
        Proof(artifact, proof.usedLemmas, (proof.premises ++ assumed).distinct)
    }

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

    /** A definition the linker put in a program: a module's function or value. `value` is its code
      * without its specification, which is no part of the program, and `specified` its code as
      * written, with the clauses of its contract.
      */
    private final case class Definition(
        name: String,
        value: SIR,
        specified: SIR,
        recursive: Boolean
    )

    /** How deep premises are made inside premises: the premise of a walk in the arguments of a walk
      * is one deeper than that walk's.
      */
    val mostNested: Int = 4

    /** Whether `name` is of a module's definition, as
      * `scalus.cardano.onchain.plutus.v1.Value$.zero`: the functions whose calls may be walks.
      */
    private def isModuleName(name: String): Boolean = name.contains("$.")

    /** `sir`, a test's program, with its walks as unknowns: `table` keeps them for all the leaves.
      */
    private def rewriteLeaf(
        sir: SIR,
        variables: Set[String],
        library: String => Boolean,
        contracts: Boolean,
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
                    definitions(b.name) =
                        Definition(b.name, EraseSpecifications(b.value), b.value, flags.isRec)
                )
                val (rebuild, expression) = peel(body)
                (inner => SIR.Let(bindings, rebuild(inner), flags, anns), expression)
            case other => (identity, other)
        val (rebuild, expression) = peel(sir)
        val loops = loopingDefinitions(definitions.values.toList)
        // A premise is a program of its own, among the definitions of the leaf it comes from, and
        // reads an unknown, of the prelude's Option.
        def close(premise: AnnotatedSIR): SIR =
            if declared.contains(optionName) then rebuild(premise)
            else SIR.Decl(table.option, rebuild(premise))
        val leaf = Leaf(definitions.toMap, loops, library, contracts, variables, table, close)
        val initial = Scope(variables.map(name => name -> Key.Variable(name)).toMap, Map.empty)
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

    /** Whether `body` is a call that reads `name` as one of its arguments, or as the function it
      * calls, and so evaluates it whenever it is evaluated: the value or a field of it.
      */
    private def readsAsArgument(body: SIR, name: String): Boolean = {
        def reads(sir: SIR): Boolean = sir match
            case SIR.Var(read, _, _)            => read == name
            case SIR.Select(scrutinee, _, _, _) => reads(scrutinee)
            case SIR.Cast(term, _, _)           => reads(term)
            case _                              => false
        spine(unwrapped(body)) match
            case (head, arguments) if arguments.nonEmpty => reads(head) || arguments.exists(reads)
            case _                                       => false
    }

    /** The names `sir` reads and does not bind. */
    private def freeNames(sir: SIR): Set[String] =
        SIR.accumulate[Set[String]](
          sir,
          Set.empty,
          Set.empty,
          (node, locals, found) =>
              node match
                  case SIR.Var(name, _, _) if !locals(name) => found + name
                  case _                                    => found
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

    /** `sir` through a declaration or a cast around it. */
    private def unwrapped(sir: SIR): SIR = sir match
        case SIR.Decl(_, term)    => unwrapped(term)
        case SIR.Cast(term, _, _) => unwrapped(term)
        case other                => other

    /** The parameters of a function's value, through a declaration or a cast around it. */
    private def arity(value: SIR): Int = value match
        case SIR.Decl(_, term)         => arity(term)
        case SIR.Cast(term, _, _)      => arity(term)
        case SIR.LamAbs(_, term, _, _) => 1 + lambdas(term)._1.size
        case _                         => 0

    private val noAnnotations = AnnotationsDecl.empty

    private val truth: AnnotatedSIR = SIR.Const(Constant.Bool(true), SIRType.Boolean, noAnnotations)

    /** A condition, which a premise has in an expression's place: none for one that declares a data
      * type.
      */
    private def annotated(sir: SIR): Option[AnnotatedSIR] = sir match
        case value: AnnotatedSIR => Some(value)
        case SIR.Decl(_, _)      => None

    /** The name a contract's guarantee gives the result, which no parameter has: so a result
      * variable named as a parameter, or as a parameter of a function that calls this one, is not
      * taken for it.
      */
    private val resultName = "$contract$result"

    /** The contract of a function, as its code states it ([[Specifications.stated]]): of its
      * `parameters`, where `expects` holds and the function returns the value [[resultName]] names,
      * `ensures` holds. The code states it with `clauses`: a proof that assumes it rests on them.
      */
    private final case class Contract(
        parameters: List[SIR.Var],
        expects: Option[AnnotatedSIR],
        ensures: Option[AnnotatedSIR],
        returns: Option[AnnotatedSIR],
        clauses: List[Clause]
    ) {

        /** The tests the contract makes of a call on `arguments` that returned the value named
          * `returned`: the precondition, of the arguments, where there is one; the guarantee, of
          * them and of what the call returned; and the condition where the call returns, of the
          * arguments. The parameters are bound to the arguments under names of their own, with
          * `suffix`.
          */
        def at(
            arguments: List[SIR],
            returned: String,
            suffix: String
        ): (Option[AnnotatedSIR], Option[AnnotatedSIR], Option[AnnotatedSIR]) = {
            val names = parameters.map(p => p.name -> s"${p.name}$$contract$suffix").toMap
            def bound(test: AnnotatedSIR, renames: Map[String, String]) =
                bindParameters(parameters, arguments, names, test, renames)
            (
              expects.map(bound(_, Map.empty)),
              ensures.map(bound(_, Map(resultName -> returned))),
              returns.map(bound(_, Map.empty))
            )
        }

        /** This contract, of a function whose code calls this one on `arguments`, of its own
          * parameters `outer`. The parameters of this one are bound to the arguments under names of
          * their own, with `suffix`, which no other contract of the chain has.
          */
        def through(outer: List[SIR.Var], arguments: List[SIR], suffix: String): Contract = {
            val names = parameters.map(p => p.name -> s"${p.name}$$of$suffix").toMap
            def bound(test: AnnotatedSIR) =
                bindParameters(parameters, arguments, names, test, Map.empty)
            Contract(outer, expects.map(bound), ensures.map(bound), returns.map(bound), clauses)
        }

        /** The clauses of the guarantees of what the call returns, and of where it returns. */
        def ofResult: List[Clause] = clauses.filter(_.kind != Clause.Kind.ReturnsWhen)
        def ofReturning: List[Clause] = clauses.filter(_.kind == Clause.Kind.ReturnsWhen)
    }

    /** `test`, of `parameters`, with each bound to its argument under its name in `names`, in one
      * `let` evaluated in order, and `renames` for other names of `test`. The names are new ones:
      * so an argument, of the caller's names, reads no parameter bound before it, as it would where
      * a parameter of the caller and one of the function it calls had one name.
      */
    private def bindParameters(
        parameters: List[SIR.Var],
        arguments: List[SIR],
        names: Map[String, String],
        test: AnnotatedSIR,
        renames: Map[String, String]
    ): AnnotatedSIR =
        SIR.Let(
          parameters
              .zip(arguments)
              .map((parameter, argument) => Binding(names(parameter.name), parameter.tp, argument)),
          SIR.renameFreeVars(test, names ++ renames),
          SIR.LetFlags.None,
          noAnnotations
        )

    private object Contract {

        /** The contract a function of `parameters` states in its code, as `stated`: none where a
          * condition declares a data type, which a premise cannot have in an expression's place.
          * The guarantee of the result speaks of it as [[resultName]]; those of the parameters do
          * not, whatever the result's name.
          */
        def of(parameters: List[SIR.Var], stated: Specifications.Stated): Option[Contract] = {
            val ofParameters = stated.ofParameters.map((unit, condition) =>
                Some(Obligations.bind(unit, Obligations.unit, condition))
            )
            val ofResult = stated.ofResult.map((result, condition) =>
                annotated(condition).map(c =>
                    SIR.renameFreeVarsInExpr(c, Map(result.name -> resultName))
                )
            )
            val returning = stated.returning.map((unit, condition) =>
                Some(Obligations.bind(unit, Obligations.unit, condition))
            )
            val guarantees = ofParameters ++ ofResult
            def all(tests: List[AnnotatedSIR]) = tests.reduceOption(SIR.And(_, _, noAnnotations))
            Option.when((guarantees ++ returning).forall(_.nonEmpty))(
              Contract(
                parameters,
                all(stated.preconditions),
                all(guarantees.flatten),
                all(returning.flatten),
                stated.clauses
              )
            )
        }
    }

    /** A premise of a statement with unknowns, and the clauses of the contract it is of: where the
      * contract has a precondition, `program` holds where `condition` does.
      */
    private final case class Premise(condition: Option[SIR], program: SIR, clauses: List[Clause]) {

        def prop: Prop = {
            val holds = Prop.Bool(PropExpr.SIRExpr[Boolean](program))
            condition.fold(holds)(c => Prop.Implies(Prop.Bool(PropExpr.SIRExpr[Boolean](c)), holds))
        }
    }

    /** The unknowns of a statement, by key, shared by its leaves, and the premises of their
      * contracts.
      */
    private final class Unknowns(val option: DataDecl) {
        private val byKey = mutable.LinkedHashMap.empty[Key, Unknown]
        private var values = 0
        private val assumed = mutable.ListBuffer.empty[Premise]

        def unknowns: List[Unknown] = byKey.values.toList

        def premises: List[Premise] = assumed.toList

        /** Whether `name` is the variable of an unknown. */
        def isUnknown(name: String): Boolean = byKey.valuesIterator.exists(_.ident.name == name)

        def assume(premise: Premise): Unit = assumed += premise

        /** A name of its own for the bindings of the next premise. */
        def premiseSuffix(): String = s"${assumed.size + 1}-$values"

        private def constructor(suffix: String): ConstrDecl =
            option.constructors
                .find(_.name.endsWith(suffix))
                .getOrElse(throw IllegalStateException(s"the prelude's Option has no $suffix"))

        /** The unknown of `key`, made where there is none, as the program reads it: `None` fails,
          * `Some(v)` is `v`; and the unknown, where it is made now.
          */
        def reference(
            key: Key,
            tp: SIRType,
            written: String,
            anns: AnnotationsDecl
        ): (SIR.Match, Option[Unknown]) = {
            val made = !byKey.contains(key)
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
            val read = SIR.Match(
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
            (read, Option.when(made)(unknown))
        }

        /** `test` of the value `unknown` names as `name`, and `otherwise` where its call fails. */
        def whereReturned(
            unknown: Unknown,
            name: String,
            test: AnnotatedSIR,
            otherwise: Boolean
        ): AnnotatedSIR =
            SIR.Match(
              SIR.Var(unknown.ident.name, unknown.ident.tp, noAnnotations),
              List(
                SIR.Case(
                  SIR.Pattern.Constr(constructor("$.None"), Nil, Nil),
                  SIR.Const(Constant.Bool(otherwise), SIRType.Boolean, noAnnotations),
                  noAnnotations
                ),
                SIR.Case(
                  SIR.Pattern.Constr(constructor("$.Some"), List(name), List(unknown.resultType)),
                  test,
                  noAnnotations
                )
              ),
              SIRType.Boolean,
              noAnnotations
            )
    }

    /** What a program binds where a call is made: the key of each variable ([[Key]]), and, by name,
      * the local values among them that a premise of the call binds again, as numbers of
      * [[Local]]s.
      */
    private final case class Scope(keys: Map[String, Key], locals: Map[String, Int]) {

        /** `name`, bound to a value of `key` that a premise does not bind again: a parameter of a
          * lambda, or a recursive function.
          */
        def opaque(name: String, key: Key): Scope = Scope(keys + (name -> key), locals - name)

        def local(name: String, key: Key, local: Int): Scope =
            Scope(keys + (name -> key), locals + (name -> local))
    }

    /** A local value of a program, bound by a `let` or by a pattern: `bind` binds it around a test,
      * as the program binds it there. It is computed of `value`, where the program has the locals
      * `visible` in scope: which of them it reads is found where a premise needs it.
      */
    private final case class Local(
        bind: AnnotatedSIR => AnnotatedSIR,
        value: SIR,
        visible: Map[String, Int]
    )

    /** The rewrite of one leaf, with the definitions linked into it. `close` makes a premise a
      * program of its own, among those definitions.
      */
    private final class Leaf(
        definitions: Map[String, Definition],
        loops: Set[String],
        library: String => Boolean,
        contracts: Boolean,
        variables: Set[String],
        table: Unknowns,
        close: AnnotatedSIR => SIR
    ) {
        private var inlined = 0
        private val locals = mutable.ArrayBuffer.empty[Local]
        private val needed = mutable.Map.empty[Int, Set[Int]]
        private var nesting = 0
        private var throughs = 0
        private val read = mutable.Map.empty[String, Option[Contract]]

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

        /** The contract of the definition `name`, where its code states one, or where its code is a
          * call of a function that has one, on all its arguments; none where the rewrite takes no
          * contracts.
          */
        private def contractOf(name: String): Option[Contract] = {
            if !contracts then None
            else
                read.get(name) match
                    case Some(known) => known
                    case None =>
                        val contract = readContract(name, Set(name))
                        read(name) = contract
                        contract
        }

        private def readContract(name: String, seen: Set[String]): Option[Contract] =
            definitions.get(name).flatMap { definition =>
                val (parameters, body) = lambdas(unwrapped(definition.specified))
                if parameters.isEmpty || parameters.exists(_.typeParams.nonEmpty) then None
                else
                    Specifications.stated(body, name) match
                        case Some(stated) => Contract.of(parameters.map(_.param), stated)
                        case None =>
                            val (head, arguments) = spine(unwrapped(body))
                            global(head)
                                .filterNot(seen)
                                .flatMap(callee => readContract(callee, seen + callee))
                                .filter(_.parameters.size == arguments.size)
                                .map { contract =>
                                    throughs += 1
                                    contract.through(
                                      parameters.map(_.param),
                                      arguments,
                                      throughs.toString
                                    )
                                }
            }

        private def contracted(name: String): Boolean = contractOf(name).nonEmpty

        /** `scope` with `names` bound, as one local value that `bind` binds and that is computed of
          * `value`.
          */
        private def withLocal(
            scope: Scope,
            names: List[(String, Key)],
            bind: AnnotatedSIR => AnnotatedSIR,
            value: SIR
        ): Scope = {
            val local = locals.size
            locals += Local(bind, value, scope.locals)
            names.foldLeft(scope)((inner, named) => inner.local(named._1, named._2, local))
        }

        /** The locals of `scope` that `sir` reads, with those their values read. */
        private def needs(sir: SIR, scope: Scope): Set[Int] =
            freeNames(sir).flatMap(scope.locals.get).flatMap(local => needsOf(local) + local)

        /** The locals that the value of the local `local` reads, with theirs: found once, where a
          * premise first needs it. A local reads only locals bound before it.
          */
        private def needsOf(local: Int): Set[Int] =
            needed.get(local) match
                case Some(known) => known
                case None =>
                    val of = locals(local)
                    val found = freeNames(of.value)
                        .flatMap(of.visible.get)
                        .flatMap(earlier => needsOf(earlier) + earlier)
                    needed(local) = found
                    found

        /** `test`, made of the values a program binds in `scope`, with each it reads bound around
          * it, in the order the program binds them.
          */
        private def hoisted(test: AnnotatedSIR, scope: Scope): AnnotatedSIR =
            needs(test, scope).toList.sorted.foldRight(test)((local, inner) =>
                locals(local).bind(inner)
            )

        def goSir(sir: SIR, scope: Scope): SIR = sir match
            case SIR.Decl(data, term) => SIR.Decl(data, goSir(term, scope))
            case a: AnnotatedSIR      => go(a, scope)

        def go(sir: AnnotatedSIR, scope: Scope): AnnotatedSIR = sir match {
            case SIR.Let(List(Binding(self, _, function: SIR.LamAbs)), body, flags, anns)
                if flags.isRec && isValueType(sir.tp) &&
                    appliesFully(body, self, lambdas(function)._1.size) =>
                walk(sir, scope, anns).getOrElse(structural(sir, scope))
            // Clauses at the head of a loop, as an inline function's are where it is inlined.
            case SIR.Let(_, _, _, anns) if startsWithClause(sir) =>
                stated(sir, scope, anns).getOrElse(structural(sir, scope))
            case SIR.Apply(_, _, tp, anns) =>
                val (head, arguments) = spine(sir)
                // Specifications are no part of a program, and are erased before it is compiled:
                // their clauses' walks would be unknowns of no program. The body of an `ensuring`
                // is the program's.
                nameOf(head) match
                    case Some(EraseSpecifications.Expects | EraseSpecifications.Ensures) => sir
                    case Some(EraseSpecifications.Ensuring) =>
                        stated(sir, scope, anns).getOrElse {
                            sir match
                                case SIR.Apply(
                                      SIR.Apply(f, body, bodyTp, bodyAnns),
                                      condition,
                                      _,
                                      _
                                    ) =>
                                    SIR.Apply(
                                      SIR.Apply(f, go(body, scope), bodyTp, bodyAnns),
                                      condition,
                                      tp,
                                      anns
                                    )
                                case other => other
                        }
                    case _ => call(sir, head, arguments, tp, anns, scope)
            case _ => structural(sir, scope)
        }

        /** `sir`, a call of `head` on `arguments`: a walk, the code of a function inlined, or the
          * call with its arguments rewritten. A call of a function with a contract is a walk.
          */
        private def call(
            sir: AnnotatedSIR,
            head: SIR,
            arguments: List[SIR],
            tp: SIRType,
            anns: AnnotationsDecl,
            scope: Scope
        ): AnnotatedSIR =
            global(head) match
                case Some(name)
                    if (loops(name) || contracted(name)) &&
                        arguments.size == arity(definitions(name).value) && isValueType(tp) =>
                    val definition = definitions(name)
                    if definition.recursive || library(name) || contracted(name) then
                        walk(sir, scope, anns).getOrElse(structural(sir, scope))
                    else
                        inlinedCall(definition, arguments, anns)
                            .fold(structural(sir, scope))(go(_, scope))
                case _ => structural(sir, scope)

        /** The unknown of the walk `sir`, where its key is of the statement's variables, with the
          * premises of its function's contract, or of `stated`, the contract stated around it,
          * where it has one, made with the unknown.
          */
        private def walk(
            sir: AnnotatedSIR,
            scope: Scope,
            anns: AnnotationsDecl,
            stated: Option[Contract] = None
        ): Option[AnnotatedSIR] = {
            val key = keyOf(sir, scope.keys, Map.empty, 0)
            Option.when(key.hoistable) {
                val (read, made) = table.reference(key, sir.tp, written(sir), anns)
                made.foreach(assume(sir, key, _, scope, stated))
                read
            }
        }

        /** Whether `sir` starts with a clause written as a statement. */
        private def startsWithClause(sir: SIR): Boolean = sir match
            case SIR.Let(binding :: _, _, _, _) =>
                spine(unwrapped(binding.value))._1 match
                    case SIR.ExternalVar(_, name, _, _) =>
                        name == EraseSpecifications.Expects || name == EraseSpecifications.Ensures ||
                        name == EraseSpecifications.ReturnsWhen
                    case _ => false
            case _ => false

        /** The unknown of the loop that `sir` states a contract around, with the premises of that
          * contract: an inline function's, where it is inlined. Its conditions are the code's,
          * written of the arguments the inlining put in place of the parameters, so the contract
          * has none. None where the code under the contract is no loop, or no unknown.
          */
        private def stated(
            sir: AnnotatedSIR,
            scope: Scope,
            anns: AnnotationsDecl
        ): Option[AnnotatedSIR] =
            if !contracts then None
            else
                for
                    loop <- annotated(Specifications.code(sir))
                    if Specifications.isLoop(loop) && isValueType(loop.tp)
                    written <- Specifications.stated(sir, "an inline function")
                    contract <- Contract.of(Nil, written)
                    read <- walk(loop, scope, anns, Some(contract))
                yield read

        /** Adds the premise of the contract of the function the walk `sir` calls, where it has one,
          * about `unknown`, the unknown of its key `key`: where the call returns, the contract
          * holds of the arguments and of what it returns. The premise is rewritten as a program is,
          * in `scope`, with what the call returns of the key `key`: a call in its arguments or its
          * condition is the call in the program, or in a clause, of that key. It holds where the
          * call fails, and everything it computes, the values of the program it reads among them,
          * it computes where the call returns.
          */
        private def assume(
            sir: AnnotatedSIR,
            key: Key,
            unknown: Unknown,
            scope: Scope,
            stated: Option[Contract]
        ): Unit = {
            val (head, called) = spine(sir)
            val found = stated
                .map(_ -> Nil)
                .orElse(
                  global(head)
                      .flatMap(contractOf)
                      .filter(_.parameters.size == called.size)
                      .map(_ -> called)
                )
            for (contract, arguments) <- found if nesting < mostNested do
                val suffix = table.premiseSuffix()
                val returned = s"returned$$$suffix"
                def where(test: AnnotatedSIR, otherwise: Boolean) =
                    table.whereReturned(unknown, returned, test, otherwise)
                val inScope = withLocal(
                  scope,
                  List(returned -> key),
                  where(_, true),
                  SIR.Var(unknown.ident.name, unknown.ident.tp, noAnnotations)
                )
                val (expects, ensures, returns) = contract.at(arguments, returned, suffix)
                nesting += 1
                val precondition = expects.map(go(_, inScope))
                val guarantee = ensures.map(go(_, inScope))
                val returning = returns.map(go(_, scope))
                nesting -= 1
                // The values the arguments read are bound where the call returned: they are part
                // of its key, so they are computed there. Bound outside, one that fails would make
                // the premise fail where the call does not return, and the premise is false there.
                // A precondition is a premise of its own, whose program is false where the call
                // fails: where it fails or is false, the guarantee is not assumed.
                val condition = precondition.map(test => where(hoisted(test, scope), false))
                guarantee.foreach { test =>
                    val program = where(hoisted(test, scope), true)
                    if (condition.toList :+ program).forall(closed) then
                        table.assume(
                          Premise(condition.map(close), close(program), contract.ofResult)
                        )
                }
                // Where the call returns is said of the arguments, whatever the call does: so its
                // condition is computed where the call is made, with the precondition's. Where it
                // fails or is false, nothing is assumed.
                returning.foreach { test =>
                    val where = precondition.fold(test)(pre => SIR.And(pre, test, noAnnotations))
                    val holds = hoisted(where, scope)
                    val returned = table.whereReturned(
                      unknown,
                      s"returned$$$suffix",
                      SIR.Const(Constant.Bool(true), SIRType.Boolean, noAnnotations),
                      false
                    )
                    if closed(holds) then
                        table.assume(
                          Premise(Some(close(holds)), close(returned), contract.ofReturning)
                        )
                }
        }

        /** Whether `program`, a premise, reads only what every premise has in scope: the
          * statement's variables, the unknowns and the definitions. A premise binds the arguments
          * again as the call has them, and a call whose key dropped a lambda's parameter, as that
          * of a helper whose code does not read its argument, has arguments that read it: it gets
          * no premise.
          */
        private def closed(program: AnnotatedSIR): Boolean =
            freeNames(program).forall(name =>
                variables(name) || table.isUnknown(name) || definitions.contains(name) ||
                    isModuleName(name)
            )

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

        private def structural(sir: AnnotatedSIR, scope: Scope): AnnotatedSIR =
            sir match {
                // A value the program binds is the value of its key where a later call is made: the
                // program evaluates the `let` before it goes on. A premise of the call binds it
                // again.
                case SIR.Let(bindings, body, flags, anns) if !flags.isRec =>
                    var inner = scope
                    val rewritten = bindings.map { binding =>
                        val value = goSir(binding.value, inner)
                        val rebound = binding.copy(value = value)
                        inner = withLocal(
                          inner,
                          List(binding.name -> keyOf(binding.value, inner.keys, Map.empty, 0)),
                          test => SIR.Let(List(rebound), test, flags, anns),
                          value
                        )
                        rebound
                    }
                    SIR.Let(rewritten, goSir(body, inner), flags, anns)
                case SIR.Let(bindings, body, flags, anns) =>
                    val inner =
                        bindings.foldLeft(scope)((s, b) => s.opaque(b.name, Key.Free(b.name)))
                    SIR.Let(
                      bindings.map(b => b.copy(value = goSir(b.value, inner))),
                      goSir(body, inner),
                      flags,
                      anns
                    )
                case SIR.LamAbs(param, term, typeParams, anns) =>
                    SIR.LamAbs(
                      param,
                      goSir(term, scope.opaque(param.name, Key.Free(param.name))),
                      typeParams,
                      anns
                    )
                case SIR.Apply(f, arg, tp, anns) =>
                    SIR.Apply(go(f, scope), go(arg, scope), tp, anns)
                case SIR.Select(scrutinee, field, tp, anns) =>
                    SIR.Select(goSir(scrutinee, scope), field, tp, anns)
                case SIR.And(a, b, anns) => SIR.And(go(a, scope), go(b, scope), anns)
                case SIR.Or(a, b, anns)  => SIR.Or(go(a, scope), go(b, scope), anns)
                case SIR.Not(a, anns)    => SIR.Not(go(a, scope), anns)
                case SIR.IfThenElse(c, t, f, tp, anns) =>
                    SIR.IfThenElse(go(c, scope), go(t, scope), go(f, scope), tp, anns)
                case SIR.Error(msg, anns, cause) => SIR.Error(go(msg, scope), anns, cause)
                case SIR.Constr(name, data, args, tp, anns) =>
                    SIR.Constr(name, data, args.map(goSir(_, scope)), tp, anns)
                case SIR.Match(scrutinee, cases, tp, anns) =>
                    val matched = keyOf(scrutinee, scope.keys, Map.empty, 0)
                    val read = go(scrutinee, scope)
                    SIR.Match(
                      read,
                      cases.map(c =>
                          c.copy(body = goSir(c.body, matchedBy(scope, c, cases, matched, read)))
                      ),
                      tp,
                      anns
                    )
                case SIR.Cast(term, tp, anns) => SIR.Cast(go(term, scope), tp, anns)
                case other                    => other
            }

        /** `scope` in the case `c` of a match of `read`, of the key `matched`: the variables of its
          * pattern are one local value, which a premise binds by the match, and where the value
          * matched is of another case, the premise holds.
          */
        private def matchedBy(
            scope: Scope,
            c: SIR.Case,
            cases: List[SIR.Case],
            matched: Key,
            read: AnnotatedSIR
        ): Scope =
            c.pattern match
                case SIR.Pattern.Constr(constructor, bindings, _) if bindings.nonEmpty =>
                    def bind(test: AnnotatedSIR): AnnotatedSIR =
                        SIR.Match(
                          read,
                          cases.map(other => other.copy(body = if other eq c then test else truth)),
                          SIRType.Boolean,
                          c.anns
                        )
                    withLocal(
                      scope,
                      bindings.zipWithIndex.map((name, index) =>
                          name -> Key.Projection(constructor.name, index, matched)
                      ),
                      bind,
                      read
                    )
                case _ => scope

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
          * the key, as it may fail. None for a definition that loops or has a contract, whose call
          * is a walk, for one with type parameters, and for a call of another number of arguments.
          */
        private def unfolded(
            name: String,
            arguments: List[SIR],
            values: Map[String, Key],
            bound: Map[String, Int],
            depth: Int
        ): Option[Key] =
            definitions.get(name).filterNot(_ => loops(name) || contracted(name)).flatMap {
                definition =>
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

        /** Whether the key of the call `body` keeps the keys of all its arguments: it calls no
          * definition whose call's key is that of its code ([[unfolded]]), which may drop one. The
          * function called is known by its key, of `values` and `bound` at `depth`, as [[keyOf]]
          * knows it: a local value that is a definition, as an inline instance's
          * `let self = given_Ord in self(a, b)`, is that definition.
          */
        private def keepsArguments(
            body: SIR,
            values: Map[String, Key],
            bound: Map[String, Int],
            depth: Int
        ): Boolean =
            keyOf(spine(unwrapped(body))._1, values, bound, depth) match
                case Key.Global(name) =>
                    name == toData || name == fromData || !definitions.contains(name) ||
                    loops(name) || contracted(name)
                case _ => true

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
                // that a call whose `let` may fail has another key than one that has none. One
                // whose value the body reads as an argument of a call, as an inline operator's
                // `let other = b in minus(a, other)`, is evaluated there anyway: the key is the
                // body's with the value in its place, as `minus(a, b)` has. Only where the call's
                // key keeps its arguments' keys, as a walk's does: the key of a call written out
                // into its code may drop an argument its code does not read, and with it the
                // `let`'s failure.
                case SIR.Let(bindings, body, _, _) =>
                    def nest(
                        rest: List[Binding],
                        named: Map[String, Key],
                        scope: Map[String, Int],
                        level: Int
                    ): Key =
                        rest match
                            case Nil => keyOf(body, named, scope, level)
                            case binding :: more =>
                                val value = keyOf(binding.value, named, scope, level)
                                // The body as the key would have it, with the value in place.
                                val placed = named + (binding.name -> value)
                                if readsAsArgument(body, binding.name) &&
                                    keepsArguments(body, placed, scope - binding.name, level)
                                then nest(more, placed, scope - binding.name, level)
                                else
                                    Key.Bind(
                                      value,
                                      nest(more, named, scope + (binding.name -> level), level + 1)
                                    )
                    nest(bindings, values, bound, depth)
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
