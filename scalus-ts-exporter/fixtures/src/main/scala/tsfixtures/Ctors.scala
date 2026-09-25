package tsfixtures

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}
import scalus.interop.TsIgnore

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

/** Two constructors, and a `js.UndefOr` parameter on the primary.
  *
  * `js.UndefOr[A]` is the Scala 2 pseudo-union `js.|[A, Unit]` in the scalajs-library pickles. A
  * compiler in Scala.js mode unpickles it as the union `A | Unit`, which erases to `Object`; one
  * without `-scalajs` keeps the class `js.|`. A second constructor makes every reference to the
  * primary `<init>` match by exact signature, so an inspector without `-scalajs` fails to unpickle
  * this class with `undefined: this # -1`.
  */
@JSExportTopLevel("OptionalCtor")
class OptionalCtor(val head: String, val detail: js.UndefOr[String]) extends js.Object {
    def this(head: String) = this(head, js.undefined)
}

/** A primary constructor kept out of the declarations.
  *
  * Scala.js dispatches exported constructors by argument count and gives no default for a missing
  * argument, so a TypeScript overload with optional trailing parameters would accept calls that
  * throw at runtime. `@TsIgnore` on the primary leaves only the secondary in the `.d.ts`.
  */
@JSExportTopLevel("IgnoredPrimary")
class IgnoredPrimary @TsIgnore() (val head: String, val detail: js.UndefOr[String])
    extends js.Object {
    def this(head: String) = this(head, js.undefined)
}
