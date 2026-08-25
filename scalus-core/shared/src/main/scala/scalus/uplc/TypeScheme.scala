package scalus.uplc

enum TypeScheme:
    case Type(argType: DefaultUni)
    case App(f: TypeScheme, arg: TypeScheme)
    case Arrow(argType: TypeScheme, t: TypeScheme)
    case All(name: String, t: TypeScheme)
    case TVar(name: String)

    // Plain eager vals, on purpose. TypeScheme values are tiny, acyclic, and built once per
    // builtin, then arity/numTypeVars are read per node in the UPLC optimizer's hot path.
    // Children are constructor arguments, so the recursion only touches already-initialized
    // instances. JMH (steady state, 1024 reads over ~80 shared schemes, JDK 25): plain val
    // ~277ns, @threadUnsafe lazy val ~285ns (racy), thread-safe lazy val ~874ns (volatile
    // Object field + Integer unbox per read), def ~4240ns (recomputes the recursion).
    val arity: Int = this match
        case Arrow(_, t) => 1 + t.arity
        case All(_, t)   => t.arity
        case _           => 0

    val numTypeVars: Int = this match
        case All(_, t) => 1 + t.numTypeVars
        case _         => 0

    infix def ->:(t: TypeScheme): TypeScheme = Arrow(t, this)
    infix def ->:(t: DefaultUni): TypeScheme = Arrow(Type(t), this)
    infix def $(t: TypeScheme): TypeScheme = App(this, t)
    infix def $(t: String): TypeScheme = App(this, TVar(t))
