package scalus.uplc

import scalus.uplc.Term.{Apply, Case, Const, Constr, Delay, Error, Force, LamAbs}

/** Marker injected into a compiled UPLC program to identify it as Scalus-generated.
  *
  * The tag replaces the first `(error)` node of the program with `[(error) (con integer 3)]`. The
  * argument of an application is never evaluated when the function position is `error`, and an
  * `(error)` node reached on a successful run would make the script fail, so every `(error)` node
  * in a script that succeeds is unreachable by construction. The tag therefore costs **zero**
  * execution budget: only the 3 extra bytes of the constant. (On the failing path it adds one
  * `Apply` step, but that transaction is invalid anyway.)
  *
  * `3` is Scalus's compiler id in CIP-171 (`compiler_type = 3`, constructor `#6.124`).
  *
  * A program with no `(error)` node - a pure computation that cannot fail - has nowhere to carry a
  * free tag and is left untagged.
  *
  * This is the same mechanism Aiken uses (`delay [(error) (force (error))]`, the branch its
  * validators take when the body returns `False`) and a cheaper variant of plu-ts's dead `case`
  * branch (`case (constr 0) <contract> (con integer 42)`).
  *
  * Injection must happen **after** the UPLC optimizer has run, so that the set of `(error)` nodes
  * is final.
  */
object ScalusTag {

    /** The constant payload used as the tag marker. */
    val marker: Constant = Constant.Integer(BigInt(3))

    /** The payload used by the pre-1.1 root-wrapper tag, still present in deployed scripts. */
    val legacyMarker: Constant = Constant.String("S")

    /** Tags `term` by marking its first `(error)` node. Returns `term` unchanged if it has none. */
    def wrap(term: Term): Term = tagFirstError(term).getOrElse(term)

    /** True if `term` carries a Scalus tag, in either the current or the legacy shape. */
    def isTagged(term: Term): Boolean = isLegacyTagged(term) || containsMarkedError(term)

    /** True if `term` carries the pre-1.1 root wrapper `[(lam _ body) (con string "S")]`. */
    def isLegacyTagged(term: Term): Boolean = term match
        case Apply(LamAbs(_, _, _), Const(`legacyMarker`, _), _) => true
        case _                                                   => false

    /** Replaces the first `(error)` node in pre-order traversal, if there is one. */
    private def tagFirstError(term: Term): Option[Term] = term match
        case e: Term.Error           => Some(Apply(e, Const(marker), e.annotation))
        case LamAbs(name, body, ann) => tagFirstError(body).map(LamAbs(name, _, ann))
        case Apply(f, arg, ann) =>
            tagFirstError(f)
                .map(Apply(_, arg, ann))
                .orElse(tagFirstError(arg).map(Apply(f, _, ann)))
        case Force(body, ann)       => tagFirstError(body).map(Force(_, ann))
        case Delay(body, ann)       => tagFirstError(body).map(Delay(_, ann))
        case Constr(tag, args, ann) => tagFirstInList(args).map(Constr(tag, _, ann))
        case Case(scrutinee, cases, ann) =>
            tagFirstError(scrutinee)
                .map(Case(_, cases, ann))
                .orElse(tagFirstInList(cases).map(Case(scrutinee, _, ann)))
        case _ => None

    private def tagFirstInList(terms: List[Term]): Option[List[Term]] = terms match
        case Nil => None
        case head :: tail =>
            tagFirstError(head)
                .map(_ :: tail)
                .orElse(tagFirstInList(tail).map(head :: _))

    private def containsMarkedError(term: Term): Boolean = term match
        case Apply(_: Term.Error, Const(`marker`, _), _) => true
        case LamAbs(_, body, _)                          => containsMarkedError(body)
        case Apply(f, arg, _)   => containsMarkedError(f) || containsMarkedError(arg)
        case Force(body, _)     => containsMarkedError(body)
        case Delay(body, _)     => containsMarkedError(body)
        case Constr(_, args, _) => args.exists(containsMarkedError)
        case Case(scrutinee, cases, _) =>
            containsMarkedError(scrutinee) || cases.exists(containsMarkedError)
        case _ => false
}
