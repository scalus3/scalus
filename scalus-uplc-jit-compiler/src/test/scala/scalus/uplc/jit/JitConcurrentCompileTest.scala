package scalus.uplc.jit

import org.scalatest.funsuite.AnyFunSuiteLike
import scalus.*
import scalus.compiler.compile
import scalus.uplc.Constant.given
import scalus.uplc.Term
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.{NoBudgetSpender, NoLogger, PlutusVM}
import scalus.uplc.jit.hybrid.HybridJIT
import scalus.uplc.jit.nativestack.JIT

import java.util.concurrent.{Callable, Executors, TimeUnit}
import scala.jdk.CollectionConverters.*

/** `jitUplc` from several threads at once. A staging compiler may only be used by the thread that
  * first ran it, so a compiler shared by all callers fails on the second thread.
  */
class JitConcurrentCompileTest extends AnyFunSuiteLike {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    private val addOne: Term = compile((i: BigInt) => i + 1).toUplc(true)

    for jit <- List(JIT, mincont.JIT, HybridJIT) do
        test(s"jitUplc from 4 threads at once for ${jit.getClass.getName}") {
            val pool = Executors.newFixedThreadPool(4)
            try {
                val tasks = (1 to 8).map { i =>
                    val task: Callable[Any] = () =>
                        jit.jitUplc(addOne $ BigInt(i).asTerm)(
                          NoLogger,
                          NoBudgetSpender,
                          summon[PlutusVM].machineParams
                        )
                    task
                }
                val results = pool.invokeAll(tasks.asJava, 2, TimeUnit.MINUTES).asScala.map(_.get())
                assert(results.toList == (2 to 9).map(BigInt(_)).toList)
            } finally pool.shutdownNow()
        }
}
