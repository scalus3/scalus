package scalus.uplc.eval.jitcommon

import scalus.uplc.Term

object UplcTermHelper {

    // Helper to check if a term always produces Value (no Apply/Force)
    def isSimpleTerm(term: Term): Boolean = term match {
        case Term.Var(_)       => true
        case Term.Const(_)     => true
        case Term.Builtin(_)   => true
        case Term.LamAbs(_, _) => true
        case Term.Delay(_)     => true // Delay wraps in Return
        case _                 => false
    }

}
