package scalus.verify.uplcblaster

import scalus.uplc.{DeBruijn, Program, Term}

/** A compiled program written as a term of PlutusCore's Lean model, `Program.Program
  * (Version.Version 1 1 0) (...)`: what a lemma of Scalus's Lean library about a compiled function
  * is of (`ScalusProofs.Mappings`). The library is the Lean source alone, so a program is written
  * into a module as a term, where a statement's module imports one from a file (`#import_uplc`).
  *
  * A lemma about a compiled function holds of the program the compiler gave when it was proved. The
  * test of a mapping writes the program the compiler gives now, and fails where it is another: the
  * lemma is then to be proved again.
  */
object LeanPrograms {

    /** `program` as a Lean term, its variables by de Bruijn index from 0, as Lean's model has them.
      */
    def program(program: Program): String = {
        val (major, minor, patch) = program.version
        s"Program.Program (Version.Version $major $minor $patch)\n  (${term(DeBruijn.deBruijnTerm(program.term))})"
    }

    private def term(t: Term): String = t match
        // Scalus numbers a variable's binder from 1, Lean's model from 0.
        case Term.Var(name, _)          => s".Var ${name.index - 1}"
        case Term.LamAbs(_, body, _)    => s".Lam (${term(body)})"
        case Term.Apply(f, argument, _) => s".Apply (${term(f)}) (${term(argument)})"
        case Term.Force(inner, _)       => s".Force (${term(inner)})"
        case Term.Delay(inner, _)       => s".Delay (${term(inner)})"
        case Term.Const(constant, _)    => s".Const (${UplcBlaster.leanValue(constant)})"
        case Term.Builtin(builtin, _)   => s".Builtin .$builtin"
        case Term.Error(_)              => ".Error"
        case Term.Constr(tag, arguments, _) =>
            s".Constr ${tag.value} [${arguments.map(term).mkString(", ")}]"
        case Term.Case(scrutinee, cases, _) =>
            s".Case (${term(scrutinee)}) [${cases.map(term).mkString(", ")}]"
}
