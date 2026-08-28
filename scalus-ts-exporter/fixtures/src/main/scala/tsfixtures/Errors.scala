package tsfixtures

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@JSExportTopLevel("BadLong")
class BadLong extends js.Object {
    def bad(x: Long): Long = x
}

@JSExportTopLevel("BadOption")
class BadOption extends js.Object {
    def bad(x: Option[String]): Option[String] = x
}

@JSExportTopLevel("BadColl")
class BadColl extends js.Object {
    def bad(): List[String] = Nil
}

@JSExportTopLevel("BadOpaque")
class BadOpaque extends js.Object {
    def bad(): java.time.Instant = java.time.Instant.EPOCH
}

/** These two collide on the export name "Duplicated"; the collector must report it. */
@JSExportTopLevel("Duplicated")
class BadDupA extends js.Object

@JSExportTopLevel("Duplicated")
class BadDupB extends js.Object
