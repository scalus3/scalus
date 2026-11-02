package scalus.cardano.ledger

import io.bullet.borer.Borer
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.RecursiveNestingTest.*
import scalus.serialization.cbor.Cbor
import scalus.uplc.test.SmallStack
import scalus.utils.Hex.hexToBytes

/** The decoders of metadata and of native scripts read as deep as their limit on a thread with a
  * small stack, and fail with their `Overflow` beyond it; their encoders write as deep. How deep a
  * value nests does not depend on the stack of the thread that reads or writes it.
  * RecursiveNestingTest has the limits.
  *
  * The decoders that recursed once per level of nesting needed about the JVM's default stack of a
  * thread, 1 MiB on a 64-bit platform, to read as deep as the limit: in a suite run in parallel
  * they overflowed it, at random, and on the small stack every time.
  *
  * The size of a thread's stack is only a hint to the JVM. Where it does not give the small stack
  * that is asked for, these tests prove nothing, and are canceled. On CI (OpenJDK 21, x86_64
  * Linux), a recursion went as deep on a thread that asked for 256 KiB as on one of the default
  * size.
  */
class NestingStackTest extends AnyFunSuite {

    private val limit = Cbor.maxMetadataAndScriptNestingLevels

    /** How deep a recursion that does nothing else goes before it overflows the stack. */
    private def deepestRecursion(): Int = {
        def down(depth: Int): Int = {
            try down(depth + 1)
            catch case _: StackOverflowError => depth
        }
        down(0)
    }

    /** How deep the recursion goes on the small stack, on a thread's default and on 8 MiB. It runs
      * first to be compiled, so that every stack is measured with the same frames.
      */
    private lazy val depths: (Int, Int, Int) = {
        for _ <- 1 to 3 do SmallStack.onStack(0)(deepestRecursion())
        (
          SmallStack.run(deepestRecursion()),
          SmallStack.onStack(0)(deepestRecursion()),
          SmallStack.onStack(8L * 1024 * 1024)(deepestRecursion())
        )
    }

    /** The depths of the recursion, in words. */
    private def measured: String = {
        val (small, default, large) = depths
        s"the recursion went $small levels deep on 256 KiB, $default on a thread's default and " +
            s"$large on 8 MiB"
    }

    /** Cancels the test where the JVM does not give a thread the small stack it asks for: smaller,
      * by far, than one it asks 8 MiB for.
      */
    private def requireSmallStack(): Unit = {
        val (small, _, large) = depths
        assume(
          small * 4 < large,
          s"the JVM does not give a thread the stack size it asks for, so a small stack cannot " +
              s"be tested here: $measured"
        )
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

    // The recursive decoders needed about 1 MiB at the limit, and 256 KiB is far less: where the JVM
    // gives what is asked, the tests below test the decoders, and are canceled where it does not.
    test("the JVM gives a thread the small stack it asks for") {
        info(measured)
        requireSmallStack()
    }

    test("metadata as deep as the limit decodes on a small stack") {
        requireSmallStack()
        for hex <- List(nestedLists(limit), nestedIndefiniteLists(limit), nestedMaps(limit)) do
            val metadatum = SmallStack.run(Cbor.decode[Metadatum](hex.hexToBytes))
            assert(levels(metadatum) == limit, hex.take(8))
    }

    test("metadata deeper than the limit fails with Overflow on a small stack, not by the stack") {
        requireSmallStack()
        val inputs = List(nestedLists, nestedIndefiniteLists, nestedMaps).map(_(limit + 1))
        for deeper <- inputs do
            val error = SmallStack.run(
              intercept[Borer.Error.Overflow[?]](Cbor.decode[Metadatum](deeper.hexToBytes))
            )
            assert(error.getMessage.contains(s"Exceeded $limit nesting levels of metadata"))
    }

    test("metadata as deep as the limit encodes on a small stack") {
        requireSmallStack()
        for hex <- List(nestedLists(limit), nestedMaps(limit)) do
            val metadatum = Cbor.decode[Metadatum](hex.hexToBytes)
            assert(SmallStack.run(reencoded(metadatum)) == hex)
    }

    test("a native script as deep as the limit decodes and encodes on a small stack") {
        requireSmallStack()
        // 2 * 499 + 1 = 999 levels of arrays
        val deepest = (limit - 1) / 2
        val hex = nestedAllOf(deepest)
        val script = SmallStack.run(Cbor.decode[Timelock](hex.hexToBytes))
        assert(levels(script) == deepest)
        assert(SmallStack.run(reencoded(script)) == hex)
    }

    test("a native script deeper than the limit fails with Overflow on a small stack") {
        requireSmallStack()
        val error = SmallStack.run(
          intercept[Borer.Error.Overflow[?]](
            Cbor.decode[Timelock](nestedAllOf((limit - 1) / 2 + 1).hexToBytes)
          )
        )
        assert(error.getMessage.contains(s"Exceeded $limit nesting levels of a native script"))
    }
}
