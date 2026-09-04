package tsfixtures

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}

/** A class with more than one constructor.
  *
  * Every constructor is public because Scala.js has no other option: on a non-native JS class a
  * private constructor is a private method, and "private methods in non-native JS classes cannot be
  * overloaded", so `class Ctors private (...)` with a secondary is a compile error. The collector
  * therefore has to reach past `primaryConstructor` to find the secondary, not merely stop skipping
  * private primaries.
  */
@JSExportTopLevel("Ctors")
class Ctors(head: String) extends js.Object {
    def this(head: String, tail: String) = this(head + tail)
    def size: Double = head.length.toDouble
}

/** A class whose constructor is retired in favour of a factory.
  *
  * The `@deprecated` sits on the constructor, not on the class. A constructor does not go through
  * `docOf`, so without folding the annotation in by hand the .d.ts presented the retired
  * constructor as the supported way to build the thing.
  */
@JSExportTopLevel("RetiredCtor")
class RetiredCtor @deprecated("use RetiredCtor.of", "1.2.0") (val n: Double) extends js.Object

object RetiredCtor {

    /** The replacement. */
    @JSExportStatic
    @annotation.nowarn("cat=deprecation")
    def of(n: Double): RetiredCtor = new RetiredCtor(n)
}
