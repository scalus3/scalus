package scalus.uplc.transform

import scalus.*
import scalus.uplc.Term.*
import scalus.uplc.{DefaultFun, Meaning, Term}

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
object ForcedBuiltinsExtractor {
    def apply(term: Term): Term =
        apply(term = term, logger = _ => (), exceptBuiltins = Set())

    def apply(
        term: Term,
        exceptBuiltins: Set[DefaultFun] = Set(),
        logger: String => Unit = s => (),
    ): Term = {
        var counter = 0

        def freshName(base: String, env: Map[String, Term]): String =
            var name = base
            while env.contains(name) do
                name = s"${base}_$counter"
                counter += 1
            name

        val extracted = mutable.Map.empty[Term, String]

        def go(term: Term, env: Map[String, Term]): Term = term match
            case Apply(f, arg)      => Apply(go(f, env), go(arg, env))
            case LamAbs(name, body) => LamAbs(name, go(body, env - name))
            case Force(Force(Builtin(bn)))
                if Meaning.allBuiltins.getBuiltinRuntime(bn).typeScheme.numTypeVars == 2
                    && !exceptBuiltins.contains(bn) =>
                val name = extracted.getOrElseUpdate(term, freshName(s"__builtin_$bn", env))
                logger(s"Replacing Forced builtin with Var: $name")
                vr(name)
            case Force(Builtin(bn))
                if Meaning.allBuiltins.getBuiltinRuntime(bn).typeScheme.numTypeVars == 1
                    && !exceptBuiltins.contains(bn) =>
                val name = extracted.getOrElseUpdate(term, freshName(s"__builtin_$bn", env))
                logger(s"Replacing Forced builtin with Var: $name")
                vr(name)
            case Force(t)          => Force(go(t, env))
            case Delay(t)          => Delay(go(t, env))
            case Constr(tag, args) => Constr(tag, args.map(arg => go(arg, env)))
            case Case(scrutinee, cases) =>
                Case(
                  go(scrutinee, env),
                  cases.map(c => go(c, env))
                )
            case _: Var | _: Const | _: Builtin | Error => term

        val term1 = go(term, Map.empty)
        // optimization should be deterministic, so we sort the extracted terms by name
        // to have a lambdas in a deterministic order.
        val sortedExtracted = extracted.toArray.sortBy(_._2)
        val lams = sortedExtracted.foldRight(term1) { case ((_, name), acc) =>
            LamAbs(name, acc)
        }
        val withVars = sortedExtracted.foldLeft(lams) { case (acc, (term, _)) =>
            acc $ term
        }
        withVars
    }
}
