package scalus.uplc.eval

import scala.collection.immutable
import scala.scalajs.js

private[eval] object CekEnvPlatform {

    /** An empty environment backed by a JavaScript array. */
    val empty: CekValEnv = new JsArraySeq(js.Array())
}

/** An immutable sequence over a JavaScript array that is never mutated. Appending copies the array
  * with the native `concat`; Scala.js copies an `ArraySeq` element by element and allocates the new
  * array through reflection.
  */
private final class JsArraySeq[+A](values: js.Array[? <: A]) extends immutable.IndexedSeq[A] {
    def apply(i: Int): A = values(i)
    def length: Int = values.length
    override def appended[B >: A](elem: B): immutable.IndexedSeq[B] =
        new JsArraySeq[B](values.asInstanceOf[js.Array[B]].concat(js.Array(elem)))
}
