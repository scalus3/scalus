package scalus.serialization.cbor

import io.bullet.borer.{Reader, Writer}

import scala.collection.mutable.ArrayBuffer

/** Reading and writing CBOR nested as deep as a value chooses without recursing over the nesting:
  * what has been opened is kept on the heap, so how deep a codec reads or writes does not depend on
  * the stack of the thread. The codecs of metadata and of native scripts work so, as borer reads
  * and writes their collections: an array as an `IndexedSeq`, definite or indefinite, and a map
  * item by item.
  */
private[scalus] object NestedCbor {

    /** The values a value holds, which [[writeNested]] writes after it, in the order of `items`;
      * `array` says whether they are the items of an array opened with `writeArrayOpen`, which
      * `writeArrayClose` ends.
      */
    final class Held[A](val items: Iterator[A], val array: Boolean)

    /** Writes `value` and all it holds without recursing over the nesting. `head` writes a value
      * without the values it holds, so all of one that holds none, and returns what it holds, or
      * null. What is open is kept on the heap, one iterator per level: a value that holds none is
      * written without it.
      */
    def writeNested[A](w: Writer, value: A)(head: A => Held[A] | Null): Writer = {
        val outermost = head(value)
        if outermost ne null then
            val open = ArrayBuffer[Held[A]](outermost)
            while open.nonEmpty do
                val innermost = open.last
                if innermost.items.hasNext then
                    val held = head(innermost.items.next())
                    if held ne null then open += held
                else
                    open.dropRightInPlace(1)
                    if innermost.array then w.writeArrayClose()
        w
    }

    /** An array or a map being read, which is a value of type `A` once it is complete. */
    abstract class Open[A] {

        /** Whether all its items are read. Reads the break that ends an indefinite one. */
        def isComplete(r: Reader): Boolean

        def add(item: A): Unit

        /** The value read, once it is complete. */
        def close(r: Reader): A
    }

    /** Reads the items of `outermost` and all they hold, and returns its value. `next` reads the
      * next item, held in as many arrays and maps as its argument says are open: a value that holds
      * no other, or one more array or map, opened. What is open is kept on the heap, innermost
      * last.
      */
    def readNested[A](outermost: Open[A], r: Reader)(next: Int => A | Open[A]): A = {
        val open = ArrayBuffer[Open[A]](outermost)
        var result: Option[A] = None
        while result.isEmpty do
            val innermost = open.last
            if innermost.isComplete(r) then
                open.dropRightInPlace(1)
                val value = innermost.close(r)
                if open.isEmpty then result = Some(value) else open.last.add(value)
            else
                next(open.size) match
                    case nested: Open[A @unchecked] => open += nested
                    case item                       => innermost.add(item.asInstanceOf[A])
        result.get
    }

    /** Whether an array or a map with `remaining` items to read, or -1 if indefinite, is read.
      * Reads the break that ends an indefinite one.
      */
    def isEnd(r: Reader, remaining: Int): Boolean = {
        if remaining < 0 then r.tryReadBreak() else remaining == 0
    }

    /** The number of items in the header of an array (`what` is `Iterable`) or a map (`Map`), as
      * borer reads a collection: at most `Int.MaxValue`.
      */
    def definite(r: Reader, size: Long, what: String): Int = {
        if size <= Int.MaxValue then size.toInt
        else r.overflow(s"Cannot deserialize $what with size $size (> Int.MaxValue)")
    }

    /** Fails where an array or a map held in `depth` arrays and maps would be deeper than
      * [[Cbor.maxMetadataAndScriptNestingLevels]]: `of` says what is read, as `metadata`.
      */
    def checkDepth(r: Reader, depth: Int, of: String): Unit = {
        if depth >= Cbor.maxMetadataAndScriptNestingLevels then
            r.overflow(s"Exceeded ${Cbor.maxMetadataAndScriptNestingLevels} nesting levels of $of")
    }
}
