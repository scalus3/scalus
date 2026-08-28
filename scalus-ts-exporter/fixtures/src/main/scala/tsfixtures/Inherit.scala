package tsfixtures

import scala.scalajs.js
import scala.scalajs.js.annotation.*

/** A base class whose members are part of every subclass's JavaScript API. */
@JSExportTopLevel("Shape")
class Shape(val kind: String) extends js.Object {

    /** A human-readable description. */
    def describe(): String = s"a $kind"

    /** Overridden below; only the most derived signature is emitted. */
    def sides(): Double = 0
}

/** Inherits `kind` and `describe` from [[Shape]] and overrides `sides`. */
@JSExportTopLevel("Circle")
class Circle(val radius: Double) extends Shape("circle") {
    override def sides(): Double = 1
}

/** Extends a generic base: its members cannot be re-emitted without their type arguments. */
@JSExportTopLevel("StringBox")
class StringBox extends Box[String]("x")
