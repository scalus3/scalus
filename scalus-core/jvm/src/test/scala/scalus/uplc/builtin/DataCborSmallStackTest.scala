package scalus.uplc.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.{Constant, DefaultFun, Term}
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.uplc.test.DeepData

import java.util.concurrent.atomic.AtomicReference

/** `serialiseData` does not recurse over [[Data]] (#398): data nested 100000 levels deep is costed
  * and written on a 256 KiB stack, where the recursive encoder overflowed the stack before 1000
  * levels.
  */
class DataCborSmallStackTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    private val depth = 100000

    // `depth` opening bytes of indefinite arrays, the innermost integer, `depth` break bytes
    private val expected =
        Array.fill(depth)(0x9f.toByte) ++ Array(1.toByte) ++ Array.fill(depth)(0xff.toByte)

    private def nestedLists: Data = DeepData.nestedLists(depth)

    private def onSmallStack[A](body: => A): A = {
        val result = new AtomicReference[Either[Throwable, A]]()
        val thread = new Thread(
          null,
          () =>
              result.set(
                try Right(body)
                catch { case e: Throwable => Left(e) }
              ),
          "data-cbor-small-stack",
          256L * 1024
        )
        // a regression of the encoding loop is more likely to loop forever than to fail, and a
        // thread stuck in a loop must not keep the test JVM alive
        thread.setDaemon(true)
        thread.start()
        thread.join(60000)
        if thread.isAlive then fail("did not finish within 60 s on a 256 KiB stack")
        result.get().fold(e => fail(s"failed on a 256 KiB stack: $e"), identity)
    }

    // The stack size of a thread is only a hint to the JVM. A recursion over the nesting, which
    // takes less stack per level than the recursive encoder did, must overflow this stack, or the
    // tests below prove nothing.
    test("a recursion over 100000 levels overflows the 256 KiB stack") {
        def depthOf(d: Data): Int = d match
            case Data.List(PList.Cons(inner, _)) => 1 + depthOf(inner)
            case _                               => 0
        val overflowed = onSmallStack {
            try
                depthOf(nestedLists)
                false
            catch case _: StackOverflowError => true
        }
        assert(overflowed)
    }

    test("serialiseData and Data.toCbor of 100000 nested lists on a 256 KiB stack") {
        val (serialised, cbor) = onSmallStack {
            val d = nestedLists
            (Builtins.serialiseData(d).bytes, d.toCbor)
        }
        assert(serialised.sameElements(expected))
        assert(cbor.sameElements(expected))
    }

    test("the CEK machine costs and runs serialiseData of 100000 nested lists on a 256 KiB stack") {
        val result = onSmallStack {
            Term.Apply(
              Term.Builtin(DefaultFun.SerialiseData),
              Term.Const(Constant.Data(nestedLists))
            ).evaluateDebug
        }
        result match
            case success: Result.Success =>
                success.term match
                    case Term.Const(Constant.ByteString(bytes), _) =>
                        assert(bytes.bytes.sameElements(expected))
                    case other => fail(s"unexpected result term: ${other.getClass.getSimpleName}")
            // the exception's message would print the deep data
            case failure: Result.Failure => fail(s"failed: ${failure.exception.getClass.getName}")
    }
}
