package scalus.uplc

import scalus.*
import scalus.cardano.ledger.Word64
import scalus.serialization.flat
import scalus.serialization.flat.{DecoderState, EncoderState, Flat, given}
import scalus.uplc.CommonFlatInstances.{flatConstant, given}
import scalus.uplc.FlatInstantces.given_Flat_Data

import scala.annotation.targetName
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

    /** Alpha-equivalence check between two terms.
      *
      * @note
      *   This method assumes that the terms are debruijn-indexed properly. Call
      *   [[DeBruijn.deBruijnTerm]] before using this method to ensure correct indexing.
      */
    @targetName("alphaEq")
    infix def α_==(that: Term): Boolean = {
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

        equals(this, that)
    }

object Term {

    @deprecated("Use alphaEq or α_== methods instead", "0.13.0")
    def alphaEq(t1: Term, t2: Term): Boolean = t1 α_== t2

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

    given Flat[Term] with
        val termTagWidth = 4
        def bitSize(a: Term): Int = a match
            case Var(name) =>
                // in Plutus See Note [Index (Word64) (de)serialized through Natural]
                if name.index < 0 then
                    throw new IllegalArgumentException(
                      s"Cannot serialize UPLC Var with negative de Bruijn index. " +
                          s"Variable '${name.name}' has index ${name.index}, which indicates an unbound/free variable. " +
                          s"This usually means the variable is not properly bound in the scope."
                    )
                termTagWidth + summon[Flat[Word64]].bitSize(Word64(name.index))
            case Const(c)     => termTagWidth + flatConstant.bitSize(c)
            case Apply(f, x)  => termTagWidth + bitSize(f) + bitSize(x)
            case LamAbs(x, t) => termTagWidth + bitSize(t)
            case Force(term)  => termTagWidth + bitSize(term)
            case Delay(term)  => termTagWidth + bitSize(term)
            case Builtin(bn)  => termTagWidth + summon[Flat[DefaultFun]].bitSize(bn)
            case Error        => termTagWidth
            case Constr(tag, args) =>
                termTagWidth + summon[Flat[Word64]].bitSize(tag) + summon[Flat[List[Term]]]
                    .bitSize(args)
            case Case(arg, cases) =>
                termTagWidth + bitSize(arg) + summon[Flat[List[Term]]].bitSize(cases)

        def encode(a: Term, enc: EncoderState): Unit =
            a match
                case Term.Var(name) =>
                    if name.index < 0 then
                        throw new IllegalArgumentException(
                          s"Cannot serialize UPLC Var with negative de Bruijn index. " +
                              s"Variable '${name.name}' has index ${name.index}, which indicates an unbound/free variable. " +
                              s"This usually means the variable is not properly bound in the scope."
                        )
                    enc.bits(termTagWidth, 0)
                    summon[Flat[Word64]].encode(Word64(name.index), enc)
                case Term.Delay(term) =>
                    enc.bits(termTagWidth, 1)
                    encode(term, enc)
                case Term.LamAbs(name, term) =>
                    enc.bits(termTagWidth, 2)
                    encode(term, enc)
                case Term.Apply(f, arg) =>
                    enc.bits(termTagWidth, 3)
                    encode(f, enc); encode(arg, enc)
                case Term.Const(const) =>
                    enc.bits(termTagWidth, 4)
                    flatConstant.encode(const, enc)
                case Term.Force(term) =>
                    enc.bits(termTagWidth, 5)
                    encode(term, enc)
                case Term.Error =>
                    enc.bits(termTagWidth, 6)
                case Term.Builtin(bn) =>
                    enc.bits(termTagWidth, 7)
                    flat.encode(bn, enc)
                case Constr(tag, args) =>
                    enc.bits(termTagWidth, 8)
                    flat.encode(tag, enc)
                    flat.encode(args, enc)
                case Case(arg, cases) =>
                    enc.bits(termTagWidth, 9)
                    flat.encode(arg, enc)
                    flat.encode(cases, enc)

        def decode(decoder: DecoderState): Term =
            val tag = decoder.bits8(termTagWidth)
            tag match
                case 0 =>
                    val index = summon[Flat[Word64]].decode(decoder).value.toInt
                    val name = s"i$index"
                    Term.Var(NamedDeBruijn(name, index))
                case 1 =>
                    val term = decode(decoder)
                    Term.Delay(term)
                case 2 =>
                    val term = decode(decoder)
                    // in plutus-core it's super-duper over complicated, but it all boils down to this
                    // https://github.com/input-output-hk/plutus/blob/a56c96598b4b25c9e28215214d25189331087244/plutus-core/plutus-core/src/PlutusCore/Flat.hs#L357
                    Term.LamAbs("i0", term)
                case 3 =>
                    val f = decode(decoder)
                    val arg = decode(decoder)
                    Term.Apply(f, arg)
                case 4 =>
                    Term.Const(flatConstant.decode(decoder))
                case 5 =>
                    val term = decode(decoder)
                    Term.Force(term)
                case 6 =>
                    Term.Error
                case 7 =>
                    Term.Builtin(flat.decode(decoder))
                case 8 =>
                    Term.Constr(
                      tag = flat.decode(decoder),
                      args = flat.decode(decoder)
                    )
                case 9 =>
                    Term.Case(arg = decode(decoder), cases = flat.decode(decoder))

}
