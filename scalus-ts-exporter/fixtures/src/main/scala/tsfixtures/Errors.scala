package tsfixtures

import scalus.interop.TsName

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

// TypeScript's type and value spaces are separate, but each space must stay unambiguous.
@TsName("BadDuplicateType")
trait BadDuplicateTypeA extends js.Object

@TsName("BadDuplicateType")
trait BadDuplicateTypeB extends js.Object

@TsName("BadClassType")
trait BadClassShape extends js.Object

@JSExportTopLevel("BadClassType")
class BadClassType extends js.Object

@JSExportTopLevel("BadClassValue")
class BadClassValue extends js.Object

@JSExportTopLevel("BadClassValue")
object BadClassValueFactory extends js.Object

@JSExportTopLevel("BadDuplicateValue")
object BadDuplicateValueFactory extends js.Object

@TsName("BadAliasType")
trait BadAliasShape extends js.Object

@JSExportTopLevel("BadAliasClass")
@JSExportTopLevel("BadAliasType")
class BadAliasClass extends js.Object

@TsName("BadFunctionAliasType")
trait BadFunctionAliasShape extends js.Object

@JSExportTopLevel("BadAliasValue")
object BadAliasValueFactory extends js.Object

@TsName("BadThreeWay")
trait BadThreeWayShape extends js.Object

@JSExportTopLevel("BadThreeWay")
object BadThreeWayA extends js.Object

@JSExportTopLevel("BadThreeWay")
object BadThreeWayB extends js.Object

object BadCollisionExports {
    @JSExportTopLevel("BadDuplicateValue")
    def duplicateValue(): Unit = ()

    @JSExportTopLevel("BadAliasFunction")
    @JSExportTopLevel("BadAliasValue")
    @JSExportTopLevel("BadFunctionAliasType")
    def aliasedFunction(): Unit = ()

    @JSExportTopLevel("BadCollisionInputs")
    def accept(
        a: BadDuplicateTypeA,
        b: BadDuplicateTypeB,
        c: BadClassShape,
        d: BadAliasShape,
        e: BadFunctionAliasShape,
        f: BadThreeWayShape
    ): Unit = ()
}
