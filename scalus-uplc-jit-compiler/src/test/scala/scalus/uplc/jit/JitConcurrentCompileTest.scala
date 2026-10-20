package scalus.uplc.jit

import org.scalatest.funsuite.AnyFunSuiteLike
import scalus.*
import scalus.compiler.compile
import scalus.uplc.Constant.given
import scalus.uplc.{Constant, Term}
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.{NoBudgetSpender, NoLogger, PlutusVM}

import java.util.concurrent.{Callable, Executors, TimeUnit}
import scala.jdk.CollectionConverters.*

/** `jitUplc` from several threads at once. A staging compiler may only be used by the thread that
  * first ran it, so a compiler shared by all callers fails on the second thread.
  */
class JitConcurrentCompileTest extends AnyFunSuiteLike {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    private val addOne: Term = compile((i: BigInt) => i + 1).toUplc(true)

    test("jitUplc from 4 threads at once") {
        val pool = Executors.newFixedThreadPool(4)
        try {
            val tasks = (1 to 8).map { i =>
                val task: Callable[UplcValue] = () =>
                    nativestack.JIT.jitUplc(addOne $ BigInt(i).asTerm)(
                      NoLogger,
                      NoBudgetSpender,
                      summon[PlutusVM].machineParams
                    )
                task
            }
            val results = pool.invokeAll(tasks.asJava, 2, TimeUnit.MINUTES).asScala.map(_.get())
            assert(results.toList == (2 to 9).map(i => Constant.Integer(i)).toList)
        } finally pool.shutdownNow()
    }
}
