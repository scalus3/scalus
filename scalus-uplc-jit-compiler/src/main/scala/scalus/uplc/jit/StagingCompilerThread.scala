package scalus.uplc.jit

import scalus.uplc.Term

import java.util.concurrent.{Callable, ExecutionException, Executors}
import scala.quoted.{staging, Expr, Quotes}

/** Runs the staging compiles of all JIT backends, one at a time, on one dedicated thread.
  *
  * A `staging.Compiler` may only be used from the thread that first ran it (the compiler asserts
  * "illegal multithreaded access to ContextBase"), so callers on several threads cannot share one
  * compiler. The compiles go to this thread instead. This costs no parallelism:
  * `staging.Compiler.run` is `synchronized`, so the compiles were serialized already.
  *
  * A reused compiler keeps the typed trees of all its past compiles, 2-3.5 KB per term node.
  * scala3-staging gives every run the same run id, so the base type caches of library classes are
  * never cleared and keep the local symbols of old runs, and with them their trees. A new compiler
  * costs about 1 s more on its first compile. So the thread replaces its compiler after it has
  * compiled [[NodesPerCompiler]] term nodes: this keeps at most 100-175 MB and adds 4-6 % to the
  * compile time.
  */
private[jit] object StagingCompilerThread {

    /** Term nodes a compiler compiles before it is replaced. */
    private val NodesPerCompiler = 50_000L

    /** Stack of the compiler thread. Code generation and the compiler recurse on the depth of the
      * term.
      */
    private val StackSize = 256L << 20

    @volatile private var thread: Thread | Null = null

    private val executor = Executors.newSingleThreadExecutor { r =>
        val t = new Thread(null, r, "scalus-jit-compiler", StackSize)
        t.setDaemon(true)
        thread = t
        t
    }

    // Used only on the compiler thread.
    private var compiler: staging.Compiler | Null = null
    private var compiledNodes = 0L

    /** Compiles the code generated for `term` and returns its value. Safe to call from any thread.
      */
    def run[T](term: Term)(code: Quotes ?=> Expr[T]): T =
        // a compile that calls back would wait for itself; the nested staging.run fails instead
        if Thread.currentThread() eq thread then compile(term, code)
        else
            val task: Callable[T] = () => compile(term, code)
            try executor.submit(task).get()
            catch case e: ExecutionException => throw e.getCause

    private def compile[T](term: Term, code: Quotes ?=> Expr[T]): T = {
        if compiler == null || compiledNodes >= NodesPerCompiler then
            compiler = staging.Compiler.make(getClass.getClassLoader)
            compiledNodes = 0
        compiledNodes += nodeCount(term)
        staging.run(code)(using compiler.nn)
    }

    private def nodeCount(t: Term): Long = t match
        case Term.LamAbs(_, b, _)  => 1 + nodeCount(b)
        case Term.Apply(f, a, _)   => 1 + nodeCount(f) + nodeCount(a)
        case Term.Force(b, _)      => 1 + nodeCount(b)
        case Term.Delay(b, _)      => 1 + nodeCount(b)
        case Term.Constr(_, as, _) => 1 + as.map(nodeCount).sum
        case Term.Case(s, cs, _)   => 1 + nodeCount(s) + cs.map(nodeCount).sum
        case _                     => 1
}
