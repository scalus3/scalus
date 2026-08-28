package tsfixtures

import scala.scalajs.js
import scala.scalajs.js.annotation.*

/** A rectangle.
  *
  * @constructor
  *   Creates a rectangle from its width and height.
  * @param width
  *   the width in pixels
  */
@JSExportTopLevel("Rect")
class Rect(
    /** The width, measured in pixels. */
    val width: Double,
    val height: Double
) extends js.Object

/** The doc below belongs to `a` alone; `b` must not inherit it. */
@JSExportTopLevel("OneLiners")
@JSExportAll
class OneLiners {
    // format: off
    /** Doc for a. */
    @JSExport def a(): Double = 1.0
    @JSExport def b(): Double = 2.0
    // format: on
}

/** Rich documentation.
  *
  * A link to an exported name stays live: [[Point]]. A Scala-only target degrades to code:
  * [[scala.Predef]].
  *
  * @example
  *   ```ts
  *   const d = new Documented();
  *   d.pick("a");
  *   ```
  */
@JSExportTopLevel("Documented")
class Documented extends js.Object {

    /** Picks a value.
      *
      * Accepts one of
      *   - a string
      *   - a number
      *
      * @tparam A
      *   the element type
      * @param a
      *   the value to pick
      * @return
      *   the value it was given
      * @throws IllegalArgumentException
      *   never, in practice
      */
    def pick[A](a: A): A = a
}
