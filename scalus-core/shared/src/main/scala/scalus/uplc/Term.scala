package scalus.uplc

import scalus.*
import scalus.cardano.ledger.Word64

import scala.collection.immutable

case class NamedDeBruijn(name: String, index: Int = 0):
    override def toString: String =
        if index == 0 then s"NamedDeBruijn(\"$name\")"
        else s"NamedDeBruijn(\"$name\", $index)"

enum Term:
    case Var(name: NamedDeBruijn) extends Term
    case LamAbs(name: String, term: Term) extends Term
    case Apply(f: Term, arg: Term) extends Term
    case Force(term: Term) extends Term
    case Delay(term: Term) extends Term
    case Const(const: Constant) extends Term
    case Builtin(bn: DefaultFun) extends Term
    case Error extends Term
    case Constr(tag: Word64, args: immutable.List[Term])
    case Case(arg: Term, cases: immutable.List[Term])

    /** Applies the argument to the term. */
    infix def $(rhs: Term): Term = Term.Apply(this, rhs)

    /** Forces the term. */
    def unary_! : Term = Term.Force(this)

    /** Delays the term. */
    def unary_~ : Term = Term.Delay(this)

    def applyToList: (Term, immutable.List[Term]) =
        this match
            case Term.Apply(f, arg) =>
                val (f1, args) = f.applyToList
                (f1, args :+ arg)
            case f => (f, Nil)

    def collectBuiltins: Set[DefaultFun] = {
        this match
            case Term.Builtin(bn)                         => Set(bn)
            case Term.Var(_) | Term.Const(_) | Term.Error => Set.empty
            case Term.LamAbs(_, body)                     => body.collectBuiltins
            case Term.Force(body)                         => body.collectBuiltins
            case Term.Delay(body)                         => body.collectBuiltins
            case Term.Apply(f, arg) => f.collectBuiltins ++ arg.collectBuiltins
            case Term.Constr(_, args) =>
                args.foldLeft(Set.empty[DefaultFun])((acc, x) => acc ++ x.collectBuiltins)
            case Term.Case(arg, cases) =>
                cases.foldLeft(arg.collectBuiltins)((acc, x) => acc ++ x.collectBuiltins)
    }

    override def toString: String = this match
        case Var(name)          => s"Var(NamedDeBruijn(\"${name.name}\"))"
        case LamAbs(name, term) => s"LamAbs(\"$name\", $term)"
        case Apply(f, arg)      => s"Apply($f, $arg)"
        case Force(term)        => s"Force($term)"
        case Delay(term)        => s"Delay($term)"
        case Const(const)       => s"Const($const)"
        case Builtin(bn)        => s"Builtin($bn)"
        case Error              => "Error"
        case Constr(tag, args)  => s"Constr($tag, ${args.mkString(", ")})"
        case Case(arg, cases)   => s"Case($arg, ${cases.mkString(", ")})"

object Term:

    def alphaEq(t1: Term, t2: Term): Boolean =

        def eqName(n1: NamedDeBruijn, n2: NamedDeBruijn): Boolean =
            assert(n1.index != 0)
            assert(n2.index != 0)
            n1.index == n2.index

        def equals(self: Term, other: Term): Boolean = (self, other) match
            case (Var(n1), Var(n2))               => eqName(n1, n2)
            case (LamAbs(n1, t1), LamAbs(n2, t2)) => equals(t1, t2)
            case (Apply(f1, a1), Apply(f2, a2))   => equals(f1, f2) && equals(a1, a2)
            case (Force(t1), Force(t2))           => equals(t1, t2)
            case (Delay(t1), Delay(t2))           => equals(t1, t2)
            case (Const(c1), Const(c2))           => c1 == c2
            case (Builtin(b1), Builtin(b2))       => b1 == b2
            case (Error, Error)                   => true
            case (Constr(tag1, args1), Constr(tag2, args2)) =>
                tag1 == tag2 && args1.size == args2.size && args1
                    .zip(args2)
                    .forall((t1, t2) => equals(t1, t2))
            case (Case(arg1, cases1), Case(arg2, cases2)) =>
                equals(arg1, arg2) && cases1.size == cases2.size && cases1
                    .zip(cases2)
                    .forall((t1, t2) => equals(t1, t2))
            case _ => false

        equals(t1, t2)

    extension (sc: StringContext)
        /** Creates a variable term.
          * @param args
          *   the arguments
          * @example
          *   {{{
          *   val idx = 0
          *   vr"foo${idx}" == Var(NamedDeBruijn("foo0"))
          *   }}}
          */
        def vr(args: Any*): Term = Term.Var(NamedDeBruijn(sc.parts.head))

    extension [A: Constant.LiftValue](a: A)
        def asTerm: Term = Term.Const(summon[Constant.LiftValue[A]].lift(a))

    def λ(names: String*)(term: Term): Term = lam(names*)(term)
    def λλ(name: String)(f: Term => Term): Term = lam(name)(f(vr(name)))
    def lam(names: String*)(term: Term): Term = names.foldRight(term)(Term.LamAbs(_, _))
    def vr(name: String): Term = Term.Var(NamedDeBruijn(name))

    /** Parse UPLC term from string using the default version (1, 1, 0) */
    def parseUplc(s: String): Either[String, Term] = UplcParser().parseTerm(s)

    /** Parse UPLC term from string using a specific version */
    def parseUplc(s: String, version: (Int, Int, Int)): Either[String, Term] =
        UplcParser(version).parseTerm(s)
