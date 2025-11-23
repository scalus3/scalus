package scalus.uplc.transform

import scalus.cardano.ledger.Word64
import scalus.uplc.Term
import scalus.uplc.Term.{Apply, Builtin, Case, Const, Constr, Delay, Error, Force, LamAbs, Var}
import scalus.uplc.eval.{Log, Logger}

import scala.collection.mutable.ArrayBuffer

/** Replace nested Apply with Case/Constr
  *
  * For example, replace `(apply (apply (apply f a) b) c)` with `(case (constr 0 [a, b, c]) f)`.
  * This is more memory/cpu efficient than nested Apply at least in Plutus V3 Plomin HF, protocol
  * version 10.
  *
  * With current machine costs, Apply costs 100 memory and 16000 cpu, same for Case/Constr. Hence
  * (case (constr 0 [a, b, c]) f) costs 200 memory and 32000 cpu, while `(apply (apply (apply f a)
  * b) c)` costs 300 memory and 48000 cpu.
  */
class CaseConstrApply(logger: Logger = new Log()) extends Optimizer {
    def apply(term: Term): Term = extractPass(term)

    def logs: Seq[String] = logger.getLogs.toSeq

    /** Main inlining function */
    private def extractPass(term: Term): Term =
        def go(term: Term): Term = term match
            case _: Apply =>
                term.applyToList match
                    case (f, args) if args.sizeCompare(2) > 0 =>
                        logger.log(s"Replacing ${args.size} Apply with Case/Constr")
                        Case(Constr(Word64.Zero, args.map(go)), go(f) :: Nil)
                    case (f, args) =>
                        args.foldLeft(go(f)) { case (acc, arg) => Apply(acc, go(arg)) }
            case LamAbs(name, body) => LamAbs(name, go(body))
            case Force(t)           => Force(go(t))
            case Delay(t)           => Delay(go(t))
            case Constr(tag, args)  => Constr(tag, args.map(arg => go(arg)))
            case Case(scrutinee, cases) =>
                Case(
                  go(scrutinee),
                  cases.map(go)
                )
            case _: Var | _: Const | _: Builtin | Error => term
        go(term)
}

object CaseConstrApply:
    def apply(term: Term): Term = new CaseConstrApply().apply(term)
