package tsfixtures

import scala.scalajs.js
import scala.scalajs.js.annotation.*

/** Generic types used from member signatures. */
@JSExportTopLevel("Generics")
class Generics extends js.Object {

    /** A generic exported class as a return type. */
    def getBox(): Box[String] = new Box("x")

    /** Nested generics. */
    def boxes(xs: js.Array[Box[String]]): js.Array[Box[js.Array[Double]]] = js.Array()

    /** A generic chased trait, with the type arguments applied. */
    def pair(p: GenPair[String, Double]): GenPair[Double, Box[String]] = p.asInstanceOf

    /** A generic method. */
    def pick[A](a: A, b: A): A = a

    /** A generic method with a bounded type parameter. */
    def widen[A <: js.Object](a: A): Box[A] = new Box(a)
}

/** Referenced but not exported: a generic chased trait. */
trait GenPair[A, B] extends js.Object {
    val first: A
    val second: B
}

/** A generic exported class with a bounded type parameter. */
@JSExportTopLevel("BoundedBox")
class BoundedBox[A <: js.Object](val value: A) extends js.Object
