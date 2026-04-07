package scalus.uplc.transform

import scalus.uplc.NamedDeBruijn
import scalus.uplc.Term
import scalus.uplc.Term.*

/** Shared naming utilities for optimizer passes that hoist subterms into fresh bindings.
  *
  * Used by both [[CommonSubexpressionElimination]] and [[CommonContextExtraction]] to produce
  * descriptive variable names like `__cse_UnConstr_Tl_Hd_x` or `__cce_Hd_UnConstr`, which are
  * easier to read than opaque counter-based names.
  */
private[transform] object TermNaming {

    // @formatter:off
    val builtinAbbreviations: Map[String, String] = Map(
        "HeadList"              -> "Hd",
        "TailList"              -> "Tl",
        "FstPair"               -> "Fst",
        "SndPair"               -> "Snd",
        "UnConstrData"          -> "UnConstr",
        "UnMapData"             -> "UnMap",
        "UnListData"            -> "UnList",
        "UnIData"               -> "UnI",
        "UnBData"               -> "UnB",
        "NullList"              -> "Null",
        "MkCons"                -> "Cons",
        "IfThenElse"            -> "If",
        "ChooseList"            -> "Choose",
        "EqualsData"            -> "EqD",
        "EqualsInteger"         -> "EqI",
        "EqualsByteString"      -> "EqBs",
        "LessThanInteger"       -> "LtI",
        "LessThanEqualsInteger" -> "LeI",
        "AddInteger"            -> "Add",
        "SubtractInteger"       -> "Sub",
        "MultiplyInteger"       -> "Mul",
        "DivideInteger"         -> "Div",
        "AppendByteString"      -> "AppBs",
    )
    // @formatter:on

    def abbreviateBuiltin(name: String): String =
        builtinAbbreviations.getOrElse(name, name)

    /** Extracts a readable name from a function term (stripping Force wrappers).
      *
      * @param excludeVar
      *   variable name to skip (e.g. CCE's HOLE sentinel). When the term is `Var(excludeVar)` the
      *   fallback `"app"` is returned.
      */
    def extractFunctionName(t: Term, excludeVar: String = ""): String = t match
        case Builtin(bn, _)  => bn.toString
        case Force(inner, _) => extractFunctionName(inner, excludeVar)
        case Var(NamedDeBruijn(name, _), _) if name != excludeVar => name
        case _                                                    => "app"

    /** Walks a term in pre-order producing a flat list of descriptive components.
      *
      * Components are short, readable tokens (abbreviated builtins, variable names, structural
      * markers) that callers can join into a final variable name.
      *
      * Force/Delay wrappers are skipped because they add no information a reader needs to identify
      * the body. Already-optimized names with `__cse_` / `__cce_` prefixes are stripped to avoid
      * doubling up when nested optimizer passes run.
      */
    def describeTerm(t: Term, excludeVar: String = ""): List[String] = t match
        case Var(NamedDeBruijn(name, _), _) =>
            if name == excludeVar then Nil
            else
                val stripped =
                    if name.startsWith("__cse_") then name.stripPrefix("__cse_")
                    else if name.startsWith("__cce_") then name.stripPrefix("__cce_")
                    else name
                List(stripped)
        case Builtin(bn, _)        => List(abbreviateBuiltin(bn.toString))
        case Const(_, _)           => List("c")
        case Force(inner, _)       => describeTerm(inner, excludeVar)
        case Delay(inner, _)       => describeTerm(inner, excludeVar)
        case Apply(f, arg, _)      => describeTerm(f, excludeVar) ++ describeTerm(arg, excludeVar)
        case LamAbs(_, body, _)    => "Lam" :: describeTerm(body, excludeVar)
        case Case(scrutinee, _, _) => "Cs" :: describeTerm(scrutinee, excludeVar)
        case Constr(tag, args, _) =>
            s"Ct$tag" :: args.flatMap(describeTerm(_, excludeVar))
        case Error(_) => List("Err")

    /** Builds a descriptive name fragment from a term, joining components with `_`.
      *
      * Stops adding components once the next one would push the joined string past `maxLength`,
      * keeping names readable instead of ending mid-word. Falls back to `"app"` for empty results.
      */
    def termDescription(t: Term, maxLength: Int = 70, excludeVar: String = ""): String = {
        val parts = describeTerm(t, excludeVar)
        if parts.isEmpty then "app"
        else
            val sb = new StringBuilder
            val it = parts.iterator
            var done = false
            while it.hasNext && !done do
                val next = it.next()
                if sb.isEmpty then
                    if next.length <= maxLength then sb.append(next)
                    else done = true
                else if sb.length + 1 + next.length <= maxLength then sb.append('_').append(next)
                else done = true
            if sb.isEmpty then "app" else sb.toString
    }
}
