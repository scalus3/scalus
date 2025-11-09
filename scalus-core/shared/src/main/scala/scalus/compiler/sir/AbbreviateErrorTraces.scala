package scalus.compiler.sir

import scalus.compiler.sir.SIR.Case
import scalus.compiler.sir.{AnnotatedSIR, Binding, Module, SIR, SIRType}
import scalus.uplc.Constant

import scala.collection.mutable

/** Transforms error messages in SIR to abbreviated forms and collects a mapping of abbreviations to
  * original messages.
  *
  * This transformation helps reduce the size of error messages in the IR while maintaining
  * traceability through a mapping of abbreviated forms to their original messages.
  *
  * @example
  *   {{{
  *     // Input SIR containing: Error("Failed validation"), Error("Failed execution")
  *     // Transforms to: Error("FV"), Error("FE")
  *     // With mapping: Map("FV" -> "Failed validation", "FE" -> "Failed execution")
  *     val transformer = new AbbreviateErrorTraces
  *     val result = transformer.transformSIR(input)
  *     val abbrevMap = transformer.getAbbreviationMap
  *   }}}
  */
class AbbreviateErrorTraces {
    private val abbreviationMap = mutable.Map.empty[String, String]
    private val messageToAbbrev = mutable.Map.empty[String, String]

    /** Returns the current mapping of abbreviations to original error messages.
      *
      * @return
      *   A map of abbreviations to their corresponding full error messages
      */
    def getAbbreviationMap: collection.Map[String, String] = abbreviationMap

    /** Creates a unique abbreviation for an error message.
      *
      * @param msg
      *   The original error message
      * @return
      *   A unique abbreviation for the message
      */
    private def createAbbreviation(msg: String): String = {
        if messageToAbbrev.contains(msg)
        then messageToAbbrev(msg)
        else
            // Split message into words and take first letters
            val words = msg.split("\\s+")
            val baseAbbrev = words.map(_.take(1).toLowerCase.capitalize).mkString

            // If the abbreviation exists, add numbers until unique
            var abbrev = baseAbbrev
            var counter = 1
            while abbreviationMap.contains(abbrev) do
                abbrev = s"$baseAbbrev$counter"
                counter += 1

            abbreviationMap(abbrev) = msg
            messageToAbbrev(msg) = abbrev
            abbrev
    }

    /** Transforms a Module by abbreviating all error messages.
      *
      * @param module
      *   The module to transform
      * @return
      *   The transformed module
      */
    def transformModule(module: Module): Module = {
        module.copy(defs = module.defs.map(transformBinding))
    }

    /** Transforms a Binding by abbreviating all error messages.
      *
      * @param binding
      *   The binding to transform
      * @return
      *   The transformed binding
      */
    private def transformBinding(binding: Binding): Binding = {
        binding.copy(value = transformSIR(binding.value))
    }

    /** Transforms a Case by abbreviating all error messages.
      *
      * @param cse
      *   The case to transform
      * @return
      *   The transformed case
      */
    private def transformCase(cse: Case): Case = {
        cse.copy(body = transformSIR(cse.body))
    }

    /** Transforms SIR by abbreviating all error messages.
      *
      * @param sir
      *   The SIR to transform
      * @return
      *   The transformed SIR
      */
    def transformSIR(sir: SIR): SIR = sir match {
        case a: AnnotatedSIR      => transformAnnotatedSIR(a)
        case SIR.Decl(data, term) => SIR.Decl(data, transformSIR(term))
    }

    /** Transforms SIR by abbreviating all error messages.
      *
      * @param sir
      *   The SIR to transform
      * @return
      *   The transformed SIR
      */
    def transformAnnotatedSIR(sir: AnnotatedSIR): AnnotatedSIR = sir match {
        case SIR.Error(SIR.Const(Constant.String(msg), SIRType.String, msgAnn), ann, cause) =>
            SIR.Error(
              SIR.Const(Constant.String(createAbbreviation(msg)), SIRType.String, msgAnn),
              ann,
              cause
            )
        case SIR.Error(msg, ann, cause) => SIR.Error(transformAnnotatedSIR(msg), ann, cause)
        case SIR.Let(bindings, body, flags, ann) =>
            SIR.Let(bindings.map(transformBinding), transformSIR(body), flags, ann)
        case SIR.LamAbs(name, term, tp, ann) =>
            SIR.LamAbs(name, transformSIR(term), tp, ann)
        case SIR.Apply(f, arg, tp, ann) =>
            SIR.Apply(transformAnnotatedSIR(f), transformAnnotatedSIR(arg), tp, ann)
        case SIR.And(a, b, ann) =>
            SIR.And(transformAnnotatedSIR(a), transformAnnotatedSIR(b), ann)
        case SIR.Or(a, b, ann) =>
            SIR.Or(transformAnnotatedSIR(a), transformAnnotatedSIR(b), ann)
        case SIR.Not(a, ann) =>
            SIR.Not(transformAnnotatedSIR(a), ann)
        case SIR.IfThenElse(cond, t, f, tp, ann) =>
            SIR.IfThenElse(
              transformAnnotatedSIR(cond),
              transformAnnotatedSIR(t),
              transformAnnotatedSIR(f),
              tp,
              ann
            )
        case SIR.Constr(name, data, args, tp, ann) =>
            SIR.Constr(name, data, args.map(transformSIR), tp, ann)
        case SIR.Match(scrutinee, cases, tp, ann) =>
            SIR.Match(transformAnnotatedSIR(scrutinee), cases.map(transformCase), tp, ann)
        case _ => sir
    }
}
