package scalus.uplc
import Term.*

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

        def process(term: Term, env: List[String]): Term =
            term match
                case Var(name, ann) =>
                    val idx = env.indexOf(name.name)
                    if idx == -1 then
                        if throwOnFreeVariable then
                            throw new IllegalArgumentException(
                              s"Unresolved variable '${name.name}' in De Bruijn conversion. Available variables in scope: [${env.mkString(", ")}]"
                            )
                        else
                            unique -= 1
                            Var(name.copy(index = unique), ann) // free variable
                    else Var(name.copy(index = idx + 1), ann) // 1-based index
                case LamAbs(name, term, ann) => LamAbs(name, process(term, name :: env), ann)
                case Apply(f, arg, ann)      => Apply(process(f, env), process(arg, env), ann)
                case Force(term, ann)        => Force(process(term, env), ann)
                case Delay(term, ann)        => Delay(process(term, env), ann)
                case Constr(tag, args, ann)  => Constr(tag, args.map(process(_, env)), ann)
                case Case(arg, cases, ann) =>
                    Case(process(arg, env), cases.map(process(_, env)), ann)
                case _: Const   => term
                case _: Builtin => term
                case _: Error   => term

        process(term, Nil)

    def fromDeBruijnTerm(term: Term): Term =
        var idx = 0
        def go(term: Term, env: List[String]): Term = term match
            case Var(name, ann) =>
                val binderName =
                    if name.index < 0 then name.name else env(name.index - 1) // 1-based index
                Var(name.copy(name = binderName), ann)
            case LamAbs(_, term, ann) =>
                val binderName = s"i$idx"
                idx += 1
                LamAbs(binderName, go(term, binderName :: env), ann)
            case Apply(f, arg, ann) => Apply(go(f, env), go(arg, env), ann)
            case Force(term, ann)   => Force(go(term, env), ann)
            case Delay(term, ann)   => Delay(go(term, env), ann)
            case Constr(tag, args, ann) =>
                Constr(tag, args.map(go(_, env)), ann)
            case Case(arg, cases, ann) =>
                Case(go(arg, env), cases.map(go(_, env)), ann)
            case _: Const   => term
            case _: Builtin => term
            case _: Error   => term

        go(term, Nil)
