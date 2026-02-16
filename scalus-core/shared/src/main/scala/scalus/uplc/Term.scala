package scalus.uplc

import org.typelevel.paiges
import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc.*
import org.typelevel.paiges.Style.XTerm.Fg
import scalus.*
import scalus.cardano.ledger.Word64
import scalus.compiler.sir.PrettyPrinter
import scalus.serialization.flat
import scalus.serialization.flat.{DecoderState, EncoderState, Flat, given}
import scalus.uplc.Constant.flatConstant
import scalus.uplc.eval.*
import scalus.utils.{Macros, Pretty, ScalusSourcePos, Style}

import scala.annotation.targetName
import scala.collection.immutable

case class NamedDeBruijn(name: String, index: Int = 0):
    override def toString: String =
        if index == 0 then s"NamedDeBruijn(\"$name\")"
        else s"NamedDeBruijn(\"$name\", $index)"

enum Term:
    def annotation: UplcAnnotation

    case Var(name: NamedDeBruijn, annotation: UplcAnnotation = UplcAnnotation.empty) extends Term
    case LamAbs(name: String, term: Term, annotation: UplcAnnotation = UplcAnnotation.empty)
        extends Term
    case Apply(f: Term, arg: Term, annotation: UplcAnnotation = UplcAnnotation.empty) extends Term
    case Force(term: Term, annotation: UplcAnnotation = UplcAnnotation.empty) extends Term
    case Delay(term: Term, annotation: UplcAnnotation = UplcAnnotation.empty) extends Term
    case Const(const: Constant, annotation: UplcAnnotation = UplcAnnotation.empty) extends Term
    case Builtin(bn: DefaultFun, annotation: UplcAnnotation = UplcAnnotation.empty) extends Term
    case Error(annotation: UplcAnnotation = UplcAnnotation.empty) extends Term
    case Constr(
        tag: Word64,
        args: immutable.List[Term],
        annotation: UplcAnnotation = UplcAnnotation.empty
    )
    case Case(
        arg: Term,
        cases: immutable.List[Term],
        annotation: UplcAnnotation = UplcAnnotation.empty
    )

    /** Backwards-compatible accessor for the source position. */
    final def sourcePos: ScalusSourcePos = annotation.pos

    /** Returns a copy of this term with the given annotation. */
    def withAnnotation(ann: UplcAnnotation): Term = this match
        case t: Var     => t.copy(annotation = ann)
        case t: LamAbs  => t.copy(annotation = ann)
        case t: Apply   => t.copy(annotation = ann)
        case t: Force   => t.copy(annotation = ann)
        case t: Delay   => t.copy(annotation = ann)
        case t: Const   => t.copy(annotation = ann)
        case t: Builtin => t.copy(annotation = ann)
        case t: Error   => t.copy(annotation = ann)
        case t: Constr  => t.copy(annotation = ann)
        case t: Case    => t.copy(annotation = ann)

    /** Returns a copy of this term with the given source position. */
    def withPos(pos: ScalusSourcePos): Term = withAnnotation(UplcAnnotation(pos))

    /** Sets the annotation on this term and recursively on subterms, but only where the annotation
      * is currently empty. Recursion stops at terms that already have an annotation.
      */
    def withAnnotationIfEmpty(ann: UplcAnnotation): Term =
        if !annotation.isEmpty then this
        else
            this match
                case t: Var => t.copy(annotation = ann)
                case t: LamAbs =>
                    t.copy(term = t.term.withAnnotationIfEmpty(ann), annotation = ann)
                case t: Apply =>
                    t.copy(
                      f = t.f.withAnnotationIfEmpty(ann),
                      arg = t.arg.withAnnotationIfEmpty(ann),
                      annotation = ann
                    )
                case t: Force => t.copy(term = t.term.withAnnotationIfEmpty(ann), annotation = ann)
                case t: Delay => t.copy(term = t.term.withAnnotationIfEmpty(ann), annotation = ann)
                case t: Const => t.copy(annotation = ann)
                case t: Builtin => t.copy(annotation = ann)
                case t: Error   => t.copy(annotation = ann)
                case t: Constr =>
                    t.copy(args = t.args.map(_.withAnnotationIfEmpty(ann)), annotation = ann)
                case t: Case =>
                    t.copy(
                      arg = t.arg.withAnnotationIfEmpty(ann),
                      cases = t.cases.map(_.withAnnotationIfEmpty(ann)),
                      annotation = ann
                    )

    /** Sets the source position on this term and recursively on subterms, but only where the
      * position is currently empty. Recursion stops at terms that already have a position.
      */
    def withPosIfEmpty(pos: ScalusSourcePos): Term = withAnnotationIfEmpty(UplcAnnotation(pos))

    /** Applies the argument to the term. */
    infix def $(rhs: Term): Term = Term.Apply(this, rhs)

    /** Forces the term. */
    def unary_! : Term = Term.Force(this)

    /** Delays the term. */
    def unary_~ : Term = Term.Delay(this)

    def applyToList: (Term, immutable.List[Term]) =
        this match
            case Term.Apply(f, arg, _) =>
                val (f1, args) = f.applyToList
                (f1, args :+ arg)
            case f => (f, Nil)

    def collectBuiltins: Set[DefaultFun] = {
        this match
            case Term.Builtin(bn, _)                               => Set(bn)
            case Term.Var(_, _) | Term.Const(_, _) | _: Term.Error => Set.empty
            case Term.LamAbs(_, body, _)                           => body.collectBuiltins
            case Term.Force(body, _)                               => body.collectBuiltins
            case Term.Delay(body, _)                               => body.collectBuiltins
            case Term.Apply(f, arg, _) => f.collectBuiltins ++ arg.collectBuiltins
            case Term.Constr(_, args, _) =>
                args.foldLeft(Set.empty[DefaultFun])((acc, x) => acc ++ x.collectBuiltins)
            case Term.Case(arg, cases, _) =>
                cases.foldLeft(arg.collectBuiltins)((acc, x) => acc ++ x.collectBuiltins)
    }

    override def toString: String = this match
        case Var(name, _)          => s"Var(NamedDeBruijn(\"${name.name}\"))"
        case LamAbs(name, term, _) => s"LamAbs(\"$name\", $term)"
        case Apply(f, arg, _)      => s"Apply($f, $arg)"
        case Force(term, _)        => s"Force($term)"
        case Delay(term, _)        => s"Delay($term)"
        case Const(const, _)       => s"Const($const)"
        case Builtin(bn, _)        => s"Builtin($bn)"
        case Error(_)              => "Error"
        case Constr(tag, args, _)  => s"Constr($tag, ${args.mkString(", ")})"
        case Case(arg, cases, _)   => s"Case($arg, ${cases.mkString(", ")})"

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
            case (Var(n1, _), Var(n2, _))               => eqName(n1, n2)
            case (LamAbs(n1, t1, _), LamAbs(n2, t2, _)) => equals(t1, t2)
            case (Apply(f1, a1, _), Apply(f2, a2, _))   => equals(f1, f2) && equals(a1, a2)
            case (Force(t1, _), Force(t2, _))           => equals(t1, t2)
            case (Delay(t1, _), Delay(t2, _))           => equals(t1, t2)
            case (Const(c1, _), Const(c2, _))           => c1 == c2
            case (Builtin(b1, _), Builtin(b2, _))       => b1 == b2
            case (Error(_), Error(_))                   => true
            case (Constr(tag1, args1, _), Constr(tag2, args2, _)) =>
                tag1 == tag2 && args1.size == args2.size && args1
                    .zip(args2)
                    .forall((t1, t2) => equals(t1, t2))
            case (Case(arg1, cases1, _), Case(arg2, cases2, _)) =>
                equals(arg1, arg2) && cases1.size == cases2.size && cases1
                    .zip(cases2)
                    .forall((t1, t2) => equals(t1, t2))
            case _ => false

        equals(this, that)
    }

    /** Position-ignoring structural equality. Like `α_==` but works on named terms too and ignores
      * annotation fields.
      */
    infix def ~=~(that: Term): Boolean = (this, that) match
        case (Var(n1, _), Var(n2, _))               => n1 == n2
        case (LamAbs(n1, t1, _), LamAbs(n2, t2, _)) => n1 == n2 && (t1 ~=~ t2)
        case (Apply(f1, a1, _), Apply(f2, a2, _))   => (f1 ~=~ f2) && (a1 ~=~ a2)
        case (Force(t1, _), Force(t2, _))           => t1 ~=~ t2
        case (Delay(t1, _), Delay(t2, _))           => t1 ~=~ t2
        case (Const(c1, _), Const(c2, _))           => c1 == c2
        case (Builtin(b1, _), Builtin(b2, _))       => b1 == b2
        case (Error(_), Error(_))                   => true
        case (Constr(t1, a1, _), Constr(t2, a2, _)) => t1 == t2 && a1.zip(a2).forall(_ ~=~ _)
        case (Case(a1, c1, _), Case(a2, c2, _))     => (a1 ~=~ a2) && c1.zip(c2).forall(_ ~=~ _)
        case _                                      => false

    /** Position-ignoring structural inequality. Negation of `~=~`. */
    infix def ~!=~(that: Term): Boolean = !(this ~=~ that)

    /** Pretty-print the term. */
    def pretty: Doc = Pretty[Term].pretty(this, Style.Normal)

    /** Pretty-print the term with XTerm syntax highlighting. */
    def prettyXTerm: Doc = Pretty[Term].pretty(this, Style.XTerm)

    /** Show the term as a string. */
    def show: String = pretty.render(80)

    /** Show the term as a string with XTerm syntax highlighting. */
    def showHighlighted: String = prettyXTerm.render(80)

    /** Show the term as a short string (truncated to 60 chars, first line only). */
    def showShort: String = Term.truncateForDisplay(pretty.render(100), 60)

    /** Evaluate the term using the given VM.
      * @note
      *   This method just runs the CEK machine on the term. It does not follow Plutus specification
      *   like CIP-117
      *
      * @throws RuntimeException
      *   on evaluation error
      */
    def evaluate(using vm: PlutusVM): Term =
        vm.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(this))

    /** Evaluate the term using the given VM.
      * @note
      *   This method just runs the CEK machine on the term. It does not follow Plutus specification
      *   like CIP-117
      *
      * @return
      *   [[scalus.uplc.eval.Result]] with the evaluation result and the spent budget
      */
    def evaluateDebug(using vm: PlutusVM): Result =
        val spenderLogger = TallyingBudgetSpenderLogger(CountingBudgetSpender())
        try
            val result = vm.evaluateDeBruijnedTerm(
              DeBruijn.deBruijnTerm(this),
              spenderLogger,
              spenderLogger
            )
            Result.Success(
              result,
              spenderLogger.getSpentBudget,
              spenderLogger.costs.toMap,
              spenderLogger.getLogsWithBudget
            )
        catch
            case e: Exception =>
                Result.Failure(
                  e,
                  spenderLogger.getSpentBudget,
                  spenderLogger.costs.toMap,
                  spenderLogger.getLogsWithBudget
                )

    /** Wrap the term in a Plutus V1 program. */
    def plutusV1: Program = Program.plutusV1(this)

    /** Wrap the term in a Plutus V2 program. */
    def plutusV2: Program = Program.plutusV2(this)

    /** Wrap the term in a Plutus V3 program. */
    def plutusV3: Program = Program.plutusV3(this)

object Term {

    /** Truncate a string to a maximum length, showing only first line if multiline */
    private[uplc] def truncateForDisplay(s: String, maxLength: Int = 60): String =
        val firstLine = s.linesIterator.nextOption().getOrElse("")
        if firstLine.length <= maxLength then firstLine
        else firstLine.take(maxLength) + "..."

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

    def λ(name: String, names: String*)(term: Term): Term = lam(name, names*)(term)
    @deprecated("Use lam or λ methods instead", "0.13.0")
    def λλ(name: String)(f: Term => Term): Term = lam(name)(f(vr(name)))
    def lam(name: String, names: String*)(term: Term): Term =
        names.view.prepended(name).foldRight(term)(Term.LamAbs(_, _))

    /** Converts a lambda value of type [[Term]] => [[Term]] into a UPLC [[LamAbs]] term expression.
      *
      * A lambda argument `x` becomes a `Term.Var(NamedDeBruijn("x"))`` variable in the UPLC term.
      *
      * @example
      *   {{{λ(x => x $ x)}}}
      */
    inline def λ(inline f: Term => Term): Term = ${ Macros.lamTermMacro('f) }
    def vr(name: String): Term = Term.Var(NamedDeBruijn(name))

    /** Parse UPLC term from string using the default version (1, 1, 0) */
    def parseUplc(s: String): Either[String, Term] = UplcParser().parseTerm(s)

    /** Parse UPLC term from string using a specific version */
    def parseUplc(s: String, version: (Int, Int, Int)): Either[String, Term] =
        UplcParser(version).parseTerm(s)

    given Flat[Term] with
        val termTagWidth = 4
        def bitSize(a: Term): Int = a match
            case Var(name, _) =>
                // in Plutus See Note [Index (Word64) (de)serialized through Natural]
                if name.index < 0 then
                    throw new IllegalArgumentException(
                      s"Cannot serialize UPLC Var with negative de Bruijn index. " +
                          s"Variable '${name.name}' has index ${name.index}, which indicates an unbound/free variable. " +
                          s"This usually means the variable is not properly bound in the scope."
                    )
                termTagWidth + summon[Flat[Word64]].bitSize(Word64(name.index))
            case Const(c, _)     => termTagWidth + flatConstant.bitSize(c)
            case Apply(f, x, _)  => termTagWidth + bitSize(f) + bitSize(x)
            case LamAbs(x, t, _) => termTagWidth + bitSize(t)
            case Force(term, _)  => termTagWidth + bitSize(term)
            case Delay(term, _)  => termTagWidth + bitSize(term)
            case Builtin(bn, _)  => termTagWidth + summon[Flat[DefaultFun]].bitSize(bn)
            case Error(_)        => termTagWidth
            case Constr(tag, args, _) =>
                termTagWidth + summon[Flat[Word64]].bitSize(tag) + summon[Flat[List[Term]]]
                    .bitSize(args)
            case Case(arg, cases, _) =>
                termTagWidth + bitSize(arg) + summon[Flat[List[Term]]].bitSize(cases)

        def encode(a: Term, enc: EncoderState): Unit =
            a match
                case Term.Var(name, _) =>
                    if name.index < 0 then
                        throw new IllegalArgumentException(
                          s"Cannot serialize UPLC Var with negative de Bruijn index. " +
                              s"Variable '${name.name}' has index ${name.index}, which indicates an unbound/free variable. " +
                              s"This usually means the variable is not properly bound in the scope."
                        )
                    enc.bits(termTagWidth, 0)
                    summon[Flat[Word64]].encode(Word64(name.index), enc)
                case Term.Delay(term, _) =>
                    enc.bits(termTagWidth, 1)
                    encode(term, enc)
                case Term.LamAbs(name, term, _) =>
                    enc.bits(termTagWidth, 2)
                    encode(term, enc)
                case Term.Apply(f, arg, _) =>
                    enc.bits(termTagWidth, 3)
                    encode(f, enc); encode(arg, enc)
                case Term.Const(const, _) =>
                    enc.bits(termTagWidth, 4)
                    flatConstant.encode(const, enc)
                case Term.Force(term, _) =>
                    enc.bits(termTagWidth, 5)
                    encode(term, enc)
                case Term.Error(_) =>
                    enc.bits(termTagWidth, 6)
                case Term.Builtin(bn, _) =>
                    enc.bits(termTagWidth, 7)
                    flat.encode(bn, enc)
                case Constr(tag, args, _) =>
                    enc.bits(termTagWidth, 8)
                    flat.encode(tag, enc)
                    flat.encode(args, enc)
                case Case(arg, cases, _) =>
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
                    Term.Error()
                case 7 =>
                    Term.Builtin(flat.decode(decoder))
                case 8 =>
                    Term.Constr(
                      tag = flat.decode(decoder),
                      args = flat.decode(decoder)
                    )
                case 9 =>
                    Term.Case(arg = decode(decoder), cases = flat.decode(decoder))

    /** Pretty[Term] instance with rainbow brackets based on nesting depth */
    given Pretty[Term] with
        def pretty(term: Term, style: Style): Doc =
            prettyTermWithDepth(TermSanitizer.sanitizeNames(term), style, depth = 0)

        private def prettyTermWithDepth(term: Term, style: Style, depth: Int): Doc =
            import Pretty.{kw, rainbowChar}

            // Local extension that captures 'style' from enclosing scope
            extension (d: Doc)
                def styled(s: paiges.Style): Doc =
                    if style == Style.XTerm then d.style(s) else d

            term match
                case Var(name, _) => text(name.name)

                case LamAbs(name, body, _) =>
                    // (lam name body) with rainbow parens at current depth
                    val openP = rainbowChar('(', depth, style)
                    val closeP = rainbowChar(')', depth, style)
                    val prefix = openP + kw("lam", style) & text(name)
                    ((prefix / prettyTermWithDepth(body, style, depth + 1))
                        .nested(2)
                        .grouped + closeP).grouped

                case a @ Apply(_, _, _) =>
                    // [f arg1 arg2 ...] with rainbow brackets
                    val (t, args) = a.applyToList
                    val openB = rainbowChar('[', depth, style)
                    val closeB = rainbowChar(']', depth, style)
                    val allTerms = (t :: args).map(prettyTermWithDepth(_, style, depth + 1))
                    val body = intercalate(lineOrSpace, allTerms)
                    ((openB + body).nested(2).grouped + closeB).grouped

                case Force(t, _) =>
                    // (force term)
                    val openP = rainbowChar('(', depth, style)
                    val closeP = rainbowChar(')', depth, style)
                    (openP + kw("force", style) & prettyTermWithDepth(
                      t,
                      style,
                      depth + 1
                    )).grouped + closeP

                case Delay(t, _) =>
                    // (delay term)
                    val openP = rainbowChar('(', depth, style)
                    val closeP = rainbowChar(')', depth, style)
                    (openP + kw("delay", style) & prettyTermWithDepth(
                      t,
                      style,
                      depth + 1
                    )).grouped + closeP

                case Const(const, _) =>
                    // (con type value) - no depth increase, leaf node
                    val openP = rainbowChar('(', depth, style)
                    val closeP = rainbowChar(')', depth, style)
                    openP + kw("con", style) & const.pretty.styled(Fg.colorCode(64)) + closeP

                case Builtin(bn, _) =>
                    val openP = rainbowChar('(', depth, style)
                    val closeP = rainbowChar(')', depth, style)
                    openP + kw("builtin", style) & PrettyPrinter
                        .pretty(bn)
                        .styled(Fg.colorCode(176)) + closeP

                case Error(_) =>
                    kw("(error)", style)

                case Constr(tag, args, _) =>
                    // (constr tag arg1 arg2 ...)
                    val openP = rainbowChar('(', depth, style)
                    val closeP = rainbowChar(')', depth, style)
                    val argDocs = args.map(prettyTermWithDepth(_, style, depth + 1))
                    val body = kw("constr", style) & str(tag.value)
                    if argDocs.isEmpty then openP + body + closeP
                    else
                        ((openP + body & intercalate(lineOrSpace, argDocs))
                            .nested(2)
                            .grouped + closeP).grouped

                case Case(arg, cases, _) =>
                    // (case scrutinee branch1 branch2 ...)
                    val openP = rainbowChar('(', depth, style)
                    val closeP = rainbowChar(')', depth, style)
                    val argDoc = prettyTermWithDepth(arg, style, depth + 1)
                    val caseDocs = cases.map(prettyTermWithDepth(_, style, depth + 1))
                    val body = kw("case", style) & argDoc
                    if caseDocs.isEmpty then openP + body + closeP
                    else
                        ((openP + body & intercalate(lineOrSpace, caseDocs))
                            .nested(2)
                            .grouped + closeP).grouped

}
