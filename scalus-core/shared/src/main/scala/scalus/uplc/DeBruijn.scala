package scalus.uplc
import Term.*

import scala.collection.mutable

object DeBruijn:
    def deBruijnProgram(p: Program): DeBruijnedProgram =
        val term = DeBruijn.deBruijnTerm(p.term)
        DeBruijnedProgram(version = p.version, term = term)

    def fromDeBruijnProgram(p: DeBruijnedProgram): Program =
        val term = DeBruijn.fromDeBruijnTerm(p.term)
        Program(version = p.version, term = term)

    /** Converts a term with named variables to a term with De Bruijn indices. We use unique
      * negative indices to represent free variables.
      * @param term
      *   the term with named variables
      * @return
      *   the term with De Bruijn indices
      */
    def deBruijnTerm(term: Term): Term =
        deBruijnTerm(term, false)

    def deBruijnTerm(term: Term, throwOnFreeVariable: Boolean): Term =
        var unique = 0
        // Level (count of enclosing binders, from 1) of the innermost binder of each name in scope,
        // so a variable resolves in O(1) rather than by searching the enclosing binders.
        val levels = mutable.HashMap.empty[String, Int]

        def process(term: Term, level: Int): Term =
            term match
                case Var(name, ann) =>
                    val binderLevel = levels.getOrElse(name.name, 0)
                    if binderLevel == 0 then
                        if throwOnFreeVariable then
                            val inScope = levels.toSeq.sortBy(-_._2).map(_._1) // innermost first
                            throw new IllegalArgumentException(
                              s"Unresolved variable '${name.name}' in De Bruijn conversion. Available variables in scope: [${inScope.mkString(", ")}]"
                            )
                        else
                            unique -= 1
                            Var(name.copy(index = unique), ann) // free variable
                    else Var(name.copy(index = level - binderLevel + 1), ann) // 1-based index
                case LamAbs(name, body, ann) =>
                    val shadowed = levels.getOrElse(name, 0)
                    levels(name) = level + 1
                    val processed = process(body, level + 1)
                    if shadowed == 0 then levels.remove(name) else levels(name) = shadowed
                    LamAbs(name, processed, ann)
                case Apply(f, arg, ann) =>
                    Apply(process(f, level), process(arg, level), ann)
                case Force(term, ann)       => Force(process(term, level), ann)
                case Delay(term, ann)       => Delay(process(term, level), ann)
                case Constr(tag, args, ann) => Constr(tag, args.map(process(_, level)), ann)
                case Case(arg, cases, ann) =>
                    Case(process(arg, level), cases.map(process(_, level)), ann)
                case _: Const   => term
                case _: Builtin => term
                case _: Error   => term

        process(term, 0)

    def fromDeBruijnTerm(term: Term): Term =
        var idx = 0
        // Binder names in scope, innermost on top: De Bruijn index i names binders(i - 1).
        val binders = new mutable.Stack[String](64)

        def go(term: Term): Term = term match
            case Var(name, ann) =>
                if name.index < 0 then term // free variable keeps its name
                else Var(name.copy(name = binders(name.index - 1)), ann) // 1-based index
            case LamAbs(_, term, ann) =>
                val binderName = "i" + idx
                idx += 1
                binders.push(binderName)
                val body = go(term)
                binders.pop()
                LamAbs(binderName, body, ann)
            case Apply(f, arg, ann) => Apply(go(f), go(arg), ann)
            case Force(term, ann)   => Force(go(term), ann)
            case Delay(term, ann)   => Delay(go(term), ann)
            case Constr(tag, args, ann) =>
                Constr(tag, args.map(go), ann)
            case Case(arg, cases, ann) =>
                Case(go(arg), cases.map(go), ann)
            case _: Const   => term
            case _: Builtin => term
            case _: Error   => term

        go(term)

    /** Checks that a De Bruijn term is closed, exactly as the Cardano ledger checks a script before
      * running it (Plutus `mkTermToEvaluate` runs `UntypedPlutusCore.Check.Scope.checkScope`).
      *
      * Every variable must have an index from 1 to the number of enclosing binders. Like Plutus,
      * this does not look inside `Constr` or `Case`: an out-of-scope index there passes, and fails
      * only if evaluation reaches it. That is a Plutus quirk the ledger rules now depend on, see
      * https://github.com/IntersectMBO/plutus/issues/7965.
      *
      * @return
      *   the first variable out of scope, or `None` if there is none
      */
    def checkScope(term: Term): Option[NamedDeBruijn] =
        def go(term: Term, level: Int): Option[NamedDeBruijn] = term match
            case Var(name, _) =>
                if name.index > 0 && name.index <= level then None else Some(name)
            case LamAbs(_, body, _) => go(body, level + 1)
            case Apply(f, arg, _) =>
                val inF = go(f, level)
                if inF.isDefined then inF else go(arg, level)
            case Force(term, _) => go(term, level)
            case Delay(term, _) => go(term, level)
            case _              => None
        go(term, 0)
