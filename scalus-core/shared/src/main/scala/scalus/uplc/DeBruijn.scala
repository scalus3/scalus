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
                case Var(name, pos) =>
                    val idx = env.indexOf(name.name)
                    if idx == -1 then
                        if throwOnFreeVariable then
                            throw new IllegalArgumentException(
                              s"Unresolved variable '${name.name}' in De Bruijn conversion. Available variables in scope: [${env.mkString(", ")}]"
                            )
                        else
                            unique -= 1
                            Var(name.copy(index = unique), pos) // free variable
                    else Var(name.copy(index = idx + 1), pos) // 1-based index
                case LamAbs(name, term, pos) => LamAbs(name, process(term, name :: env), pos)
                case Apply(f, arg, pos)      => Apply(process(f, env), process(arg, env), pos)
                case Force(term, pos)        => Force(process(term, env), pos)
                case Delay(term, pos)        => Delay(process(term, env), pos)
                case Constr(tag, args, pos)  => Constr(tag, args.map(process(_, env)), pos)
                case Case(arg, cases, pos) =>
                    Case(process(arg, env), cases.map(process(_, env)), pos)
                case _: Const   => term
                case _: Builtin => term
                case _: Error   => term

        process(term, Nil)

    def fromDeBruijnTerm(term: Term): Term =
        var idx = 0
        def go(term: Term, env: List[String]): Term = term match
            case Var(name, pos) =>
                val binderName =
                    if name.index < 0 then name.name else env(name.index - 1) // 1-based index
                Var(name.copy(name = binderName), pos)
            case LamAbs(_, term, pos) =>
                val binderName = s"i$idx"
                idx += 1
                LamAbs(binderName, go(term, binderName :: env), pos)
            case Apply(f, arg, pos) => Apply(go(f, env), go(arg, env), pos)
            case Force(term, pos)   => Force(go(term, env), pos)
            case Delay(term, pos)   => Delay(go(term, env), pos)
            case Constr(tag, args, pos) =>
                Constr(tag, args.map(go(_, env)), pos)
            case Case(arg, cases, pos) =>
                Case(go(arg, env), cases.map(go(_, env)), pos)
            case _: Const   => term
            case _: Builtin => term
            case _: Error   => term

        go(term, Nil)
