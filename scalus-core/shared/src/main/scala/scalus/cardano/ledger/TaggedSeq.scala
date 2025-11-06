package scalus.cardano.ledger

import io.bullet.borer.*

/** Tags in sets
  *
  * Conway introduced optional 258 tags in certain items that are considered sets semantically but
  * encoded as arrays in CBOR.
  *
  * Semantics: Tag 258 can be applied to a CBOR array data item to indicate that it is a set. Sets
  * should be handled similarly to CBOR maps: a set that has duplicate items may be well-formed, but
  * it is not valid. Like CBOR map keys, data items in a set do not need to be of the same type.
  *
  * https://github.com/input-output-hk/cbor-sets-spec/blob/master/CBOR_SETS.md
  */
trait TaggedSeq(tag: Tag = Tag.Other(258)):

    def checkNonEmpty[A](src: IterableOnce[A]): IndexedSeq[A] =
        val seq = IndexedSeq.from(src)
        require(seq.nonEmpty, "Empty list found, expected non-empty")
        seq

    def checkDuplicates[A](src: IterableOnce[A]): IndexedSeq[A] =
        val seq = IndexedSeq.from(src)
        val set = Set.from(seq)
        require(
          seq.size == set.size,
          s"Final number of elements: ${set.size}" +
              s" does not match the total count that was decoded: ${seq.size}"
        )
        seq

    def readTagged[A: Decoder](r: Reader): IndexedSeq[A] =
        if r.dataItem() == DataItem.Tag then
            val t = r.readTag()
            if t != tag then r.validationFailure(s"Expected tag $tag, got $t")
        Decoder.fromFactory[A, IndexedSeq].read(r)

    def writeTagged[A: Encoder](w: Writer, v: IterableOnce[A]): Writer =
        val s = IndexedSeq.from(v)
        w.writeTag(tag)
        w.writeArrayHeader(s.size)
        s.foreach(w.write(_))
        w
