package scalus.uplc.jit

import scalus.uplc.Term

object UplcTermHelper {

    // Helper to check if a term always produces Value (no Apply/Force)
    def isSimpleTerm(term: Term): Boolean = term match {
        case Term.Var(_, _)       => true
        case Term.Const(_, _)     => true
        case Term.Builtin(_, _)   => true
        case Term.LamAbs(_, _, _) => true
        case Term.Delay(_, _)     => true // Delay wraps in Return
        case _                    => false
    }

    // Helper to check if a term is a fully-applied 2-argument builtin with simple arguments
    def isApplyBuiltin2WithSimpleArgs(term: Term): Boolean = {
        term match
            case Term.Apply(Term.Apply(Term.Builtin(bn, _), arg1, _), arg2, _) =>
                isSimpleTerm(arg1) && isSimpleTerm(arg2) && BuiltinAppliedGenerator.isSupported2(bn)
            case _ => false
    }

    // Helper to check if a term is a fully-applied 1-argument builtin with simple argument
    def isApplyBuiltin1WithSimpleArg(term: Term): Boolean = {
        term match
            case Term.Apply(Term.Builtin(bn, _), arg, _) =>
                isSimpleTerm(arg) && BuiltinAppliedGenerator.isSupported1(bn)
            case _ => false
    }

}
