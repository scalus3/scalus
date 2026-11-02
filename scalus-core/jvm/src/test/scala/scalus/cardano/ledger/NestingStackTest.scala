package scalus.cardano.ledger

import io.bullet.borer.Borer
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.RecursiveNestingTest.{nestedAllOf, nestedLists, nestedMaps, reencoded}
import scalus.serialization.cbor.Cbor
import scalus.uplc.test.SmallStack
import scalus.utils.Hex.hexToBytes

import java.util.concurrent.FutureTask

/** The decoders of metadata and of native scripts read as deep as their limit on a thread with a
  * small stack, and fail with their `Overflow` beyond it; their encoders write as deep. How deep a
  * value nests does not depend on the stack of the thread that reads or writes it.
  * RecursiveNestingTest has the limits.
  *
  * The decoders that recursed once per level of nesting needed about the JVM's default stack of a
  * thread, 1 MiB on a 64-bit platform, to read as deep as the limit: in a suite run in parallel
  * they overflowed it, at random, and on the small stack every time.
  */
class NestingStackTest extends AnyFunSuite {

    private val limit = Cbor.maxMetadataAndScriptNestingLevels

    /** How deep a recursion that does nothing else goes before it overflows the stack. */
    private def deepestRecursion(): Int = {
        def down(depth: Int): Int =
            try down(depth + 1)
            catch case _: StackOverflowError => depth
        down(0)
    }

    /** The levels of arrays and maps of one item around the innermost value, counted without
      * recursing.
      */
    private def levels(metadatum: Metadatum): Int = {
        var count = 0
        var current = metadatum
        var inner = true
        while inner do
            current match
                case Metadatum.List(items) if items.size == 1 =>
                    count += 1
                    current = items.head
                case Metadatum.Map(entries) if entries.size == 1 =>
                    count += 1
                    current = entries.head._2
                case _ => inner = false
        count
    }

    /** The levels of `AllOf` of one script around the innermost script, counted without recursing.
      */
    private def levels(timelock: Timelock): Int = {
        var count = 0
        var current = timelock
        var inner = true
        while inner do
            current match
                case Timelock.AllOf(scripts) if scripts.size == 1 =>
                    count += 1
                    current = scripts.head
                case _ => inner = false
        count
    }

    // The stack size of a thread is only a hint to the JVM. Unless the small stack is smaller than
    // a thread's default, on which the recursive decoders overflowed at the limit only at random,
    // the tests below prove nothing.
    test("the small stack is smaller than a thread's default") {
        val small = SmallStack.run(deepestRecursion())
        val onDefault = FutureTask[Int](() => deepestRecursion())
        Thread(onDefault).start()
        val default = onDefault.get()
        assert(small * 2 < default, s"$small levels on the small stack, $default on the default")
    }

    test("metadata as deep as the limit decodes on a small stack") {
        val lists = SmallStack.run(Cbor.decode[Metadatum](nestedLists(limit).hexToBytes))
        assert(levels(lists) == limit)
        val maps = SmallStack.run(Cbor.decode[Metadatum](nestedMaps(limit).hexToBytes))
        assert(levels(maps) == limit)
    }

    test("metadata deeper than the limit fails with Overflow on a small stack, not by the stack") {
        for deeper <- List(nestedLists(limit + 1), nestedMaps(limit + 1)) do
            val error = SmallStack.run(
              intercept[Borer.Error.Overflow[?]](Cbor.decode[Metadatum](deeper.hexToBytes))
            )
            assert(error.getMessage.contains(s"Exceeded $limit nesting levels of metadata"))
    }

    test("metadata as deep as the limit encodes on a small stack") {
        for hex <- List(nestedLists(limit), nestedMaps(limit)) do
            val metadatum = Cbor.decode[Metadatum](hex.hexToBytes)
            assert(SmallStack.run(reencoded(metadatum)) == hex)
    }

    test("a native script as deep as the limit decodes and encodes on a small stack") {
        // 2 * 499 + 1 = 999 levels of arrays
        val deepest = (limit - 1) / 2
        val hex = nestedAllOf(deepest)
        val script = SmallStack.run(Cbor.decode[Timelock](hex.hexToBytes))
        assert(levels(script) == deepest)
        assert(SmallStack.run(reencoded(script)) == hex)
    }

    test("a native script deeper than the limit fails with Overflow on a small stack") {
        val error = SmallStack.run(
          intercept[Borer.Error.Overflow[?]](
            Cbor.decode[Timelock](nestedAllOf((limit - 1) / 2 + 1).hexToBytes)
          )
        )
        assert(error.getMessage.contains(s"Exceeded $limit nesting levels of a native script"))
    }
}
