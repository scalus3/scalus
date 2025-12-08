package scalus.compiler.sir.lowering.simple

import scalus.cardano.ledger.Language
import scalus.compiler.sir.SIR.Pattern
import scalus.compiler.sir.{AnnotationsDecl, DataDecl, SIR, SIRType}
import scalus.uplc.*

@deprecated("Use ScottEncodingLowering instead", "0.13.0")
type SimpleSirToUplcLowering = ScottEncodingLowering

/** Scott encoding-based lowering from Scalus Intermediate Representation [[SIR]] to UPLC [[Term]].
  *
  * This implementation uses Scott encoding for data types:
  *   - `Nil` is represented as `\Nil Cons -> force Nil`
  *   - `Cons h tl` is represented as `(\head tail Nil Cons -> Cons head tail) h tl`
  *
  * @param sir
  *   the Scalus Intermediate Representation to lower
  * @param generateErrorTraces
  *   whether to generate error traces
  * @param targetLanguage
  *   the target Plutus language version
  */
class ScottEncodingLowering(
    sir: SIR,
    generateErrorTraces: Boolean = false,
    targetLanguage: Language = Language.PlutusV3
) extends BaseSimpleLowering(sir, generateErrorTraces, targetLanguage):

    override protected def lowerConstr(
        name: String,
        data: DataDecl,
        args: List[SIR],
        tp: SIRType,
        anns: AnnotationsDecl
    ): Term =
        /* data List a = Nil | Cons a (List a)
            Nil is represented as \Nil Cons -> force Nil
            Cons is represented as (\head tail Nil Cons -> Cons head tail) h tl
         */
        val constrs = data.constructors.map(_.name)
        val ctorParams = data.constructors.find(_.name == name) match
            case None =>
                throw new IllegalArgumentException(s"Constructor $name not found in $data")
            case Some(value) => value.params

        // force Nil | Cons head tail
        val appInner = ctorParams match
            case Nil => Term.Force(Term.Var(NamedDeBruijn(name)))
            case _ =>
                ctorParams.foldLeft(Term.Var(NamedDeBruijn(name)))((acc, param) =>
                    acc $ Term.Var(NamedDeBruijn(param.name))
                )
        // \Nil Cons -> ...
        val ctor = constrs.foldRight(appInner) { (constr, acc) =>
            Term.LamAbs(constr, acc)
        }
        // \head tail Nil Cons -> ...
        val ctorParamsLambda = ctorParams.foldRight(ctor) { (param, acc) =>
            Term.LamAbs(param.name, acc)
        }
        // (\Nil Cons -> force Nil) | (\head tail Nil Cons -> ...) h tl
        args.foldLeft(ctorParamsLambda) { (acc, arg) =>
            Term.Apply(acc, lowerInner(arg))
        }

    override protected def lowerMatch(matchExpr: SIR.Match): Term =
        val scrutinee = matchExpr.scrutinee

        // Check if this is a primitive type match
        if isPrimitiveType(scrutinee.tp) then lowerPrimitiveMatch(matchExpr)
        else {
            /* list match
                case Nil -> 1
                case Cons(h, tl) -> 2

                lowers to list (delay 1) (\h tl -> 2)
             */
            val scrutineeTerm = lowerInner(scrutinee)
            val constructors = findConstructors(scrutinee.tp)
            val orderedCases = expandAndSortCases(matchExpr, constructors)

            val casesTerms = orderedCases.map {
                case SIR.Case(Pattern.Constr(constr, bindings, _), body, anns) =>
                    constr.params match
                        case Nil => ~lowerInner(body)
                        case _ =>
                            bindings.foldRight(lowerInner(body)) { (binding, acc) =>
                                Term.LamAbs(binding, acc)
                            }
                case SIR.Case(Pattern.Const(_), _, anns) =>
                    val pos = anns.pos
                    throw new IllegalArgumentException(
                      s"Constant pattern not supported in SimpleSirToUplcLowering at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                    )
                case SIR.Case(Pattern.Wildcard, _, anns) =>
                    val pos = anns.pos
                    throw new IllegalArgumentException(
                      s"Wildcard case must have been eliminated at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                    )
            }

            casesTerms.foldLeft(scrutineeTerm) { (acc, caseTerm) =>
                Term.Apply(acc, caseTerm)
            }
        }

    override protected def lowerSelect(
        scrutinee: SIR,
        field: String,
        tp: SIRType,
        anns: AnnotationsDecl
    ): Term =
        val constrDecl = findConstructorDecl(scrutinee.tp, anns)
        val fieldIndex = constrDecl.params.indexWhere(_.name == field)
        if fieldIndex == -1 then
            val pos = anns.pos
            throw new IllegalArgumentException(
              s"Field $field not found in constructor ${constrDecl} at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
            )
        val instance = lowerInner(scrutinee)
        val s0 = Term.Var(NamedDeBruijn(field))
        val lam = constrDecl.params.foldRight(s0) { case (f, acc) =>
            Term.LamAbs(f.name, acc)
        }
        Term.Apply(instance, lam)
