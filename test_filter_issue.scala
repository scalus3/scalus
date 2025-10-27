import scalus.*
import scalus.Compiler.compile
import scalus.sir.simpleLowering.SirToUplc110Lowering
import scalus.prelude.List

object TestFilterIssue extends App {
    val sir = compile {
        val lst = List.Cons(1, List.Nil)
        lst.filter(_ => true)
    }

    println("=== SIR ===")
    println(sir.show)
    println()

    println("=== Checking for ExternalVars ===")
    def findExternalVars(sir: SIR): Unit = sir match {
        case SIR.ExternalVar(moduleName, name, _, _) =>
            println(s"Found ExternalVar: moduleName='$moduleName', name='$name'")
        case SIR.Decl(_, body) => findExternalVars(body)
        case SIR.Let(bindings, body, _, _) =>
            bindings.foreach(b => findExternalVars(b.value))
            findExternalVars(body)
        case SIR.LamAbs(_, term, _, _) => findExternalVars(term)
        case SIR.Apply(f, arg, _, _) =>
            findExternalVars(f)
            findExternalVars(arg)
        case SIR.Match(scrutinee, cases, _, _) =>
            findExternalVars(scrutinee)
            cases.foreach(c => findExternalVars(c.body))
        case SIR.Constr(_, _, args, _, _) =>
            args.foreach(findExternalVars)
        case _ => ()
    }
    findExternalVars(sir)

    println("\n=== Lowering to UPLC ===")
    try {
        val lowered = SirToUplc110Lowering(sir, generateErrorTraces = false).lower()
        println("Success!")
        println(lowered.pretty.render(80))
    } catch {
        case e: Exception =>
            println(s"Error: ${e.getMessage}")
            e.printStackTrace()
    }
}
