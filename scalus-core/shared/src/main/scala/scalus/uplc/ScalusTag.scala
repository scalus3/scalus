package scalus.uplc

import scalus.uplc.Term.λ
import scalus.uplc.TermDSL.given

/** Marker injected at the top of a compiled UPLC program to identify it as Scalus-generated.
  *
  * The tag wraps the program term as `[(lam _scalusTag body) (con string "S")]`, which evaluates to
  * `body` but leaves a distinctive, pattern-matchable shape on the outermost node for offchain
  * tooling (explorers, indexers, the Scalus CLI).
  *
  * Injection must happen **after** the UPLC optimizer has run — the Inliner would otherwise
  * eliminate the wrapping as dead code.
  */
object ScalusTag {

    /** The constant payload used as the tag marker. */
    val marker: Constant = Constant.String("S")

    /** Wraps `term` in the Scalus tag application. */
    def wrap(term: Term): Term = λ("_scalusTag")(term) $ marker

    /** True if `term` matches the Scalus tag shape at its outermost node. */
    def isTagged(term: Term): Boolean = term match
        case Term.Apply(
              Term.LamAbs(_, _, _),
              Term.Const(Constant.String("S"), _),
              _
            ) =>
            true
        case _ => false
}
