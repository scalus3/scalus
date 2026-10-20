package scalus.uplc.jit

import scalus.uplc.{DeBruijnedProgram, Term}

import java.lang.management.ManagementFactory
import java.nio.file.{Files, Paths}
import javax.management.ObjectName
import scala.quoted.staging

/** Times `genCodeFromTerm` alone on one script, `n` times, each with a fresh staging compiler.
  *
  * Usage: `JitCodegenProbe <script.flat> [n]`. For profiling the code generator.
  * `-Dprobe.checkMacros=true` compiles with `-Xcheck-macros`, which checks the owners of the
  * generated trees. `-Dprobe.reuse=true` reuses one compiler for all runs. `-Dprobe.jitUplc=true`
  * compiles with `jitUplc` instead, with the compiler policy of the JIT. `-Dprobe.histoAt=50,100`
  * prints the live staging-compiler objects after those runs (a class histogram).
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
        val viaJitUplc = sys.props.get("probe.jitUplc").contains("true")
        val histoAt =
            sys.props.get("probe.histoAt").toList.flatMap(_.split(",")).map(_.trim.toInt).toSet
        for i <- 1 to n do
            given staging.Compiler = if reuse then shared else newCompiler()
            var codegenNs = 0L
            val start = System.nanoTime()
            if viaJitUplc then nativestack.JIT.jitUplc(term)
            else
                val pool = ConstantPool(term)
                staging.run {
                    val t0 = System.nanoTime()
                    val expr = nativestack.JIT.genCodeFromTerm(
                      term,
                      pool,
                      nativestack.NativeStackContext.DefaultMaxDepth
                    )
                    codegenNs = System.nanoTime() - t0
                    expr
                }(pool.values)
            val total = System.nanoTime() - start
            println(f"run $i: codegen ${codegenNs / 1e9}%.2f s, total ${total / 1e9}%.2f s")
            if histoAt.contains(i) then printHistogram(i)
    }

    /** Prints the live instances of the compiler symbol classes and the total live heap. A class
      * histogram runs a full GC first, so it counts only reachable objects.
      */
    private def printHistogram(run: Int): Unit = {
        val out = ManagementFactory.getPlatformMBeanServer
            .invoke(
              ObjectName("com.sun.management:type=DiagnosticCommand"),
              "gcClassHistogram",
              Array[AnyRef](Array.empty[String]),
              Array("[Ljava.lang.String;")
            )
            .asInstanceOf[String]
        val lines = out.linesIterator.map(_.trim.split("\\s+")).toList
        def count(cls: String) = lines
            .collect { case Array(_, n, _, c, _*) if c == cls => n }
            .headOption
            .getOrElse("0")
        val total =
            lines.collectFirst { case Array("Total", _, bytes) => bytes.toLong }.getOrElse(0L)
        val prefix = "dotty.tools.dotc.core."
        println(
          s"histo run $run: SymDenotation=${count(prefix + "SymDenotations$SymDenotation")} " +
              s"ClassDenotation=${count(prefix + "SymDenotations$ClassDenotation")} " +
              s"Symbol=${count(prefix + "Symbols$Symbol")} " +
              s"ClassSymbol=${count(prefix + "Symbols$ClassSymbol")} " +
              f"live=${total / 1e6}%.0f MB"
        )
    }

    /** Compiles `term` on the given compiler, as `jitUplc` does on its own. */
    def compile(term: Term)(using staging.Compiler): CompiledTerm = {
        val pool = ConstantPool(term)
        staging.run(
          nativestack.JIT
              .genCodeFromTerm(term, pool, nativestack.NativeStackContext.DefaultMaxDepth)
        )(pool.values)
    }

    def nodes(t: Term): Long = StagingCompilerThread.nodeCount(t)

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
