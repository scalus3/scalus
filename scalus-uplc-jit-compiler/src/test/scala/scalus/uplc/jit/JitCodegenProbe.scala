package scalus.uplc.jit

import scalus.uplc.{DeBruijnedProgram, Term}

import java.nio.file.{Files, Paths}
import scala.quoted.staging

/** Times `genCodeFromTerm` alone on one script, `n` times, each with a fresh staging compiler.
  *
  * Usage: `JitCodegenProbe <script.flat> [n]`. For profiling the code generator.
  * `-Dprobe.checkMacros=true` compiles with `-Xcheck-macros`, which checks the owners of the
  * generated trees. `-Dprobe.reuse=true` reuses one compiler for all runs.
  */
object JitCodegenProbe {

    def main(args: Array[String]): Unit = {
        val term =
            DeBruijnedProgram.fromFlatEncoded(Files.readAllBytes(Paths.get(args(0)))).toProgram.term
        val n = if args.length > 1 then args(1).toInt else 1
        println(s"nodes=${nodes(term)} depth=${depth(term)}")
        val compilerArgs =
            if sys.props.get("probe.checkMacros").contains("true") then List("-Xcheck-macros")
            else Nil
        def newCompiler() = staging.Compiler.make(getClass.getClassLoader)(using
          staging.Compiler.Settings.make(compilerArgs = compilerArgs)
        )
        val shared = newCompiler()
        val reuse = sys.props.get("probe.reuse").contains("true")
        for i <- 1 to n do
            given staging.Compiler = if reuse then shared else newCompiler()
            var codegenNs = 0L
            val start = System.nanoTime()
            staging.run {
                val t0 = System.nanoTime()
                val expr = nativestack.JIT.genCodeFromTerm(term)
                codegenNs = System.nanoTime() - t0
                expr
            }
            val total = System.nanoTime() - start
            println(f"run $i: codegen ${codegenNs / 1e9}%.2f s, total ${total / 1e9}%.2f s")
    }

    def nodes(t: Term): Long = t match
        case Term.LamAbs(_, b, _)  => 1 + nodes(b)
        case Term.Apply(f, a, _)   => 1 + nodes(f) + nodes(a)
        case Term.Force(b, _)      => 1 + nodes(b)
        case Term.Delay(b, _)      => 1 + nodes(b)
        case Term.Constr(_, as, _) => 1 + as.map(nodes).sum
        case Term.Case(s, cs, _)   => 1 + nodes(s) + cs.map(nodes).sum
        case _                     => 1

    /** Maximum number of nested LamAbs/Delay (closure) levels on any path. */
    def depth(t: Term): Int = t match
        case Term.LamAbs(_, b, _)  => 1 + depth(b)
        case Term.Delay(b, _)      => 1 + depth(b)
        case Term.Apply(f, a, _)   => math.max(depth(f), depth(a))
        case Term.Force(b, _)      => depth(b)
        case Term.Constr(_, as, _) => as.map(depth).maxOption.getOrElse(0)
        case Term.Case(s, cs, _)   => (s :: cs).map(depth).max
        case _                     => 0
}
