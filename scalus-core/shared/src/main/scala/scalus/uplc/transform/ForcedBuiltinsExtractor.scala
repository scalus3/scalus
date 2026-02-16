package scalus.uplc.transform

import scalus.*
import scalus.uplc.Term.*
import scalus.uplc.{DefaultFun, Meaning, Term}
import scalus.uplc.eval.{Log, Logger}

import scala.collection.mutable

/** Extract forced builtins to top level
  *
  * For example, replace `(force (force (builtin fstPair)))` with (lam builtin_FstPair
  * (builtin_FstPair (pair true false)) (! (! __builtin_FstPair))). This is more memory/cpu
  * efficient than nested Force at least in Plutus V3 Plomin HF, protocol version 10.
  *
  * With current machine costs, Force costs 100 memory and 16000 cpu, same for Builtin. Hence (lam
  * builtin_FstPair (builtin_FstPair (pair true false)) (! (! __builtin_FstPair))) costs 200 memory
  * and 32000 cpu, while `(force (force (builtin fstPair)))` costs 300 memory and 48000 cpu.
  */
class ForcedBuiltinsExtractor(logger: Logger = new Log(), exceptBuiltins: Set[DefaultFun] = Set())
    extends Optimizer {
    def apply(term: Term): Term = extract(term)

    def logs: Seq[String] = logger.getLogs.toSeq

    private def extract(term: Term): Term = {
        var counter = 0

        def freshName(base: String, env: Map[String, Term]): String =
            var name = base
            while env.contains(name) do
                name = s"${base}_$counter"
                counter += 1
            name

        val extracted = mutable.Map.empty[DefaultFun, (Term, String)]

        def go(term: Term, env: Map[String, Term]): Term = term match
            case Apply(f, arg, ann)      => Apply(go(f, env), go(arg, env), ann)
            case LamAbs(name, body, ann) => LamAbs(name, go(body, env - name), ann)
            case Force(Force(Builtin(bn, _), _), _)
                if Meaning.allBuiltins.getBuiltinRuntime(bn).typeScheme.numTypeVars == 2
                    && !exceptBuiltins.contains(bn) =>
                val (_, name) =
                    extracted.getOrElseUpdate(bn, (term, freshName(s"__builtin_$bn", env)))
                logger.log(s"Replacing Forced builtin with Var: $name")
                vr(name)
            case Force(Builtin(bn, _), _)
                if Meaning.allBuiltins.getBuiltinRuntime(bn).typeScheme.numTypeVars == 1
                    && !exceptBuiltins.contains(bn) =>
                val (_, name) =
                    extracted.getOrElseUpdate(bn, (term, freshName(s"__builtin_$bn", env)))
                logger.log(s"Replacing Forced builtin with Var: $name")
                vr(name)
            case Force(t, ann)          => Force(go(t, env), ann)
            case Delay(t, ann)          => Delay(go(t, env), ann)
            case Constr(tag, args, ann) => Constr(tag, args.map(arg => go(arg, env)), ann)
            case Case(scrutinee, cases, ann) =>
                Case(
                  go(scrutinee, env),
                  cases.map(c => go(c, env)),
                  ann
                )
            case _: Var | _: Const | _: Builtin | _: Error => term

        val term1 = go(term, Map.empty)
        // optimization should be deterministic, so we sort the extracted terms by name
        // to have a lambdas in a deterministic order.
        val sortedExtracted = extracted.toArray.sortBy(_._2._2)
        val lams = sortedExtracted.foldRight(term1) { case ((_, (_, name)), acc) =>
            LamAbs(name, acc)
        }
        val withVars = sortedExtracted.foldLeft(lams) { case (acc, (_, (term, _))) =>
            acc $ term
        }
        withVars
    }
}

object ForcedBuiltinsExtractor:
    def apply(term: Term): Term = new ForcedBuiltinsExtractor().apply(term)
    def apply(term: Term, logger: String => Unit): Term = {
        val log = new Log()
        val result = new ForcedBuiltinsExtractor(log).apply(term)
        log.getLogs.foreach(logger)
        result
    }
    def apply(term: Term, exceptBuiltins: Set[DefaultFun], logger: String => Unit): Term = {
        val log = new Log()
        val result = new ForcedBuiltinsExtractor(log, exceptBuiltins).apply(term)
        log.getLogs.foreach(logger)
        result
    }
