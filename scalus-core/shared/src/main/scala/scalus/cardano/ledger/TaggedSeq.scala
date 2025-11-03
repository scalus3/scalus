package scalus.cardano.ledger

import io.bullet.borer.*

// indefinite array tag (258)
trait TaggedSeq(tag: Tag = Tag.Other(258)):

    def checkDuplicates[A](seq: IndexedSeq[A]): IndexedSeq[A] =
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
        val s = Seq.from(v)
        w.writeTag(tag)
        w.writeArrayHeader(s.size)
        s.foreach(w.write(_))
        w
