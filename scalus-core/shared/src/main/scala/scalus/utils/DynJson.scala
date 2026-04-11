package scalus.utils

import scala.collection.mutable.ArrayBuffer
import scala.language.dynamics
import upickle.core.LinkedHashMap

/** A lightweight wrapper around [[ujson.Value]] that uses Scala's [[Dynamic]] trait to enable
  * dot-syntax navigation of JSON structures.
  *
  * Instead of writing `json("foo")("bar")(1).num`, you can write `json.dyn.foo.bar(1).num`.
  *
  * {{{
  * import scalus.utils.DynJson.*
  *
  * val json = ujson.read("""{"foo": {"bar": [1, 2, 3]}}""")
  * json.dyn.foo.bar(1).num // 2.0
  * }}}
  * Throws the same exceptions as [[ujson.Value]] on missing keys or type mismatches. Use
  * `strOpt`/`numOpt`/`boolOpt` for safe access.
  */
final class DynJson(val value: ujson.Value) extends Dynamic {

    def selectDynamic(name: String): DynJson =
        new DynJson(value(name))

    def applyDynamic(name: String)(index: Int): DynJson =
        new DynJson(value(name)(index))

    def apply(index: Int): DynJson =
        new DynJson(value(index))

    def str: String = value.str
    def num: Double = value.num
    def bool: Boolean = value.bool
    def arr: ujson.Arr = value.arr
    def obj: ujson.Obj = value.obj
    def isNull: Boolean = value.isNull

    def strOpt: Option[String] = value.strOpt
    def numOpt: Option[Double] = value.numOpt
    def boolOpt: Option[Boolean] = value.boolOpt
    def arrOpt: Option[ArrayBuffer[ujson.Value]] = value.arrOpt
    def objOpt: Option[LinkedHashMap[String, ujson.Value]] = value.objOpt

    override def toString: String = ujson.write(value)
}

object DynJson {
    extension (v: ujson.Value) def dyn: DynJson = new DynJson(v)
}
