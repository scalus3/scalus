package scalus.uplc

import scala.annotation.threadUnsafe

enum TypeScheme:
    case Type(argType: DefaultUni)
    case App(f: TypeScheme, arg: TypeScheme)
    case Arrow(argType: TypeScheme, t: TypeScheme)
    case All(name: String, t: TypeScheme)
    case TVar(name: String)

    @threadUnsafe lazy val arity: Int = this match
        case Arrow(_, t) => 1 + t.arity
        case All(_, t)   => t.arity
        case _           => 0

    @threadUnsafe lazy val numTypeVars: Int = this match
        case All(_, t) => 1 + t.numTypeVars
        case _         => 0

    infix def ->:(t: TypeScheme): TypeScheme = Arrow(t, this)
    infix def ->:(t: DefaultUni): TypeScheme = Arrow(Type(t), this)
    infix def $(t: TypeScheme): TypeScheme = App(this, t)
    infix def $(t: String): TypeScheme = App(this, TVar(t))
