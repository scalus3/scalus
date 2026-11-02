package scalus.cardano.ledger

import io.bullet.borer.Borer
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.RecursiveNestingTest.{nestedAllOf, nestedLists, nestedMaps}
import scalus.serialization.cbor.Cbor
import scalus.utils.Hex.hexToBytes

import java.util.concurrent.FutureTask

/** The decoders of metadata and of native scripts read as deep as their limit on a thread with a
  * small stack, and fail with their `Overflow` beyond it: how deep a value they read nests does not
  * depend on the stack of the thread that reads it. RecursiveNestingTest has the limits.
  *
  * A decoder that recursed once per level of nesting needed about the JVM's default stack of a
  * thread, 1 MiB on a 64-bit platform, to read as deep as the limit: in a suite run in parallel it
  * overflowed it, at random.
  */
class NestingStackTest extends AnyFunSuite {

    private val limit = Cbor.maxRecursiveNestingLevels

    /** The stack of the thread the decoders read on: a quarter of the default. */
    private val smallStack = 256L * 1024

    /** What `body` returns, run on a new thread with a stack of [[smallStack]] bytes. What it
      * throws is rethrown here, in an `ExecutionException`.
      */
    private def onSmallStack[A](body: => A): A = {
        val task = FutureTask[A](() => body)
        Thread(null, task, "small-stack", smallStack).start()
        task.get()
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

    test("metadata as deep as the limit decodes on a small stack") {
        val lists = onSmallStack(Cbor.decode[Metadatum](nestedLists(limit).hexToBytes))
        assert(levels(lists) == limit)
        val maps = onSmallStack(Cbor.decode[Metadatum](nestedMaps(limit).hexToBytes))
        assert(levels(maps) == limit)
    }

    test("metadata deeper than the limit fails with Overflow on a small stack, not by the stack") {
        val error = onSmallStack(
          intercept[Borer.Error.Overflow[?]](
            Cbor.decode[Metadatum](nestedLists(limit + 1).hexToBytes)
          )
        )
        assert(error.getMessage.contains(s"Exceeded $limit nesting levels of metadata"))
        onSmallStack(
          intercept[Borer.Error.Overflow[?]](
            Cbor.decode[Metadatum](nestedMaps(limit + 1).hexToBytes)
          )
        )
    }

    test("a native script as deep as the limit decodes on a small stack") {
        // 2 * 499 + 1 = 999 levels of arrays
        val deepest = (limit - 1) / 2
        val script = onSmallStack(Cbor.decode[Timelock](nestedAllOf(deepest).hexToBytes))
        assert(levels(script) == deepest)
    }

    test("a native script deeper than the limit fails with Overflow on a small stack") {
        val error = onSmallStack(
          intercept[Borer.Error.Overflow[?]](
            Cbor.decode[Timelock](nestedAllOf((limit - 1) / 2 + 1).hexToBytes)
          )
        )
        assert(error.getMessage.contains(s"Exceeded $limit nesting levels of a native script"))
    }
}
