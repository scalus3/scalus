package scalus.compiler.sir.lowering.simple

import scalus.cardano.ledger.{Language, Word64}
import scalus.compiler.sir.SIR.Pattern
import scalus.compiler.sir.*
import scalus.uplc.*

@deprecated("Use SumOfProductsLowering instead", "0.13.0")
type SirToUplc110Lowering = SumOfProductsLowering

/** UPLC 1.1.0-based lowering from Scalus Intermediate Representation [[SIR]] to UPLC
  * [[scalus.uplc.Term]].
  *
  * We use UPLC version 1.1.0 and generate Sums of Products (SoP) constructors `case` and `constr`
  * to represent data types. Also we optimize `newtype` kind of constructors to be represented as
  * just values.
  *
  * @example
  *   {{{
  *  case class Wrapper(a: BigInt)
  *  val x = Wrapper(1) // lowers to just (const integer 1)
  *   }}}
  *
  * @param sir
  *   the Scalus Intermediate Representation to lower
  * @param generateErrorTraces
  *   whether to generate error traces
  * @param targetLanguage
  *   the target Plutus language version
  */
class SumOfProductsLowering(
    sir: SIR,
    generateErrorTraces: Boolean = false,
    targetLanguage: Language = Language.PlutusV3
) extends BaseSimpleLowering(sir, generateErrorTraces, targetLanguage):

    /** For wildcard patterns in SirToUplc110Lowering, use unused binding names */
    override protected def getWildcardBindings(constrDecl: ConstrDecl): List[String] =
        constrDecl.params.map(p => s"__scalus_unused_binding_${p.name}")

    override protected def lowerConstr(
        name: String,
        data: DataDecl,
        args: List[SIR],
        tp: SIRType,
        anns: AnnotationsDecl
    ): Term =
        // Check if this is a Data type constructor
        if isDataConstructor(name) then lowerDataConstr(name, args, anns)
        else
            /*
              data Newtype(a) is represented as a

              data List a = Nil | Cons a (List a)
                Nil is represented as (constr 0 [])
                Cons is represented as (constr 1 [h, tl])
             */
            if data.constructors.size == 1 && data.constructors.head.params.size == 1 then
                assert(args.size == 1)
                lowerInner(args.head)
            else
                val tag = data.constructors.indexWhere(_.name == name)
                if tag == -1 then
                    throw new IllegalArgumentException(s"Constructor $name not found in $data")
                Term.Constr(Word64(tag), args.map(lowerInner))

    override protected def lowerMatch(matchExpr: SIR.Match): Term =
        val scrutinee = matchExpr.scrutinee

        // Check if this is a primitive type match
        if isPrimitiveType(scrutinee.tp) then lowerPrimitiveMatch(matchExpr)
        // Check if this is a Data type match
        else if isDataType(scrutinee.tp) then lowerDataMatch(matchExpr)
        else {
            /* list match
                case Nil -> error
                case Cons(h, tl) -> 2

                lowers to (case list [error, \h tl -> 2])

                newtype match
                    case Newtype(a) -> error

                lowers to (\a -> error) newtype
             */
            val cases = matchExpr.cases
            val scrutineeTerm = lowerInner(scrutinee)
            val constructors = findConstructors(scrutinee.tp)
            val orderedCases = expandAndSortCases(matchExpr, constructors)

            val casesTerms = orderedCases.map {
                case SIR.Case(Pattern.Constr(constr, bindings, _), body, anns) =>
                    constr.params match
                        case Nil => lowerInner(body)
                        case _ =>
                            bindings.foldRight(lowerInner(body)) { (binding, acc) =>
                                Term.LamAbs(binding, acc)
                            }
                case SIR.Case(Pattern.Const(_), _, anns) =>
                    val pos = anns.pos
                    throw new IllegalArgumentException(
                      s"Constant pattern not supported in SirToUplc110Lowering at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                    )
                case SIR.Case(Pattern.Wildcard, _, anns) =>
                    val pos = anns.pos
                    throw new IllegalArgumentException(
                      s"Wildcard case must have been eliminated at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                    )
            }

            if constructors.size == 1 && constructors.head.params.size == 1 then
                assert(orderedCases.size == 1)
                assert(casesTerms.size == 1)
                orderedCases.head match
                    case SIR.Case(Pattern.Constr(constrDecl, bindings, _), body, _) =>
                        // newtype match
                        //   case Newtype(a) -> expr
                        // lowers to (\a -> expr) newtype
                        Term.Apply(casesTerms.head, scrutineeTerm)
                    case SIR.Case(Pattern.Const(_), _, anns) =>
                        val pos = anns.pos
                        throw new IllegalArgumentException(
                          s"Constant pattern not supported in SirToUplc110Lowering at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                        )
                    case SIR.Case(Pattern.Wildcard, body, _) =>
                        // newtype match
                        //   case _ -> expr
                        // lowers to expr
                        lowerInner(body)
            else Term.Case(scrutineeTerm, casesTerms)
        }

    override protected def lowerSelect(
        scrutinee: SIR,
        field: String,
        tp: SIRType,
        anns: AnnotationsDecl
    ): Term =
        // Check if this is a Data variant type
        if isDataVariantType(scrutinee.tp) then lowerDataSelect(scrutinee, field, tp, anns)
        else
            /*  x.field2
                lowers to
                (case x [\f1 f2 ... -> f2])

                newtype.field1
                lowers to
                newtype
             */
            val constrDecl = findConstructorDecl(scrutinee.tp, anns)
            val fieldIndex = constrDecl.params.indexWhere(_.name == field)
            if fieldIndex == -1 then
                val pos = anns.pos
                throw new IllegalArgumentException(
                  s"Field $field not found in constructor ${constrDecl} at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                )
            val instance = lowerInner(scrutinee)

            if constrDecl.params.size == 1 then instance
            else
                val s0 = Term.Var(NamedDeBruijn(field))
                val lam = constrDecl.params.foldRight(s0) { case (f, acc) =>
                    Term.LamAbs(f.name, acc)
                }
                Term.Case(instance, List(lam))
