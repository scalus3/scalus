package tsfixtures

import scalus.interop.{TsIgnore, TsName, TsType}

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.js.typedarray.Uint8Array

/** A point.
  *
  * @param x
  *   the x coordinate
  */
@JSExportTopLevel("Point")
class Point(val x: Double, val y: Double) extends js.Object {

    /** Distance to [[Point]] `other`. */
    def dist(other: Point): Double = 0.0
}

/** Exported under a canonical name plus a deprecated alias. */
@JSExportTopLevel("NewName")
@JSExportTopLevel("OldName")
class Renamed(val n: Int) extends js.Object

/** Non-js.Object class: only annotated members are exported. */
@JSExportTopLevel("Partial")
class Partial(secret: String) {
    @JSExport
    def visible(a: js.BigInt): js.BigInt = a
    def hidden(): String = secret
}

@JSExportTopLevel("Statics")
class Statics(val v: Double) extends js.Object
object Statics {
    @JSExportStatic
    val mainnet: Statics = new Statics(1)
    @JSExportStatic
    def make(v: Double, tag: String = "x"): Statics = new Statics(v)

    /** Exported as `of`, not as `create`. */
    @JSExportStatic("of")
    def create(v: Double): Statics = new Statics(v)
}

/** Generic exported class. */
@JSExportTopLevel("Box")
class Box[A](val value: A) extends js.Object

/** Every mappable type in one class. */
@JSExportTopLevel("Kitchen")
class Kitchen extends js.Object {
    def prims(a: Boolean, b: Int, c: Double, d: String): Unit = ()
    def big(x: js.BigInt): js.BigInt = x
    def arr(xs: js.Array[String]): js.Array[js.Array[Double]] = js.Array()
    def undef(x: js.UndefOr[String]): js.UndefOr[Double] = 0.0
    def union(x: js.BigInt | Null): Uint8Array | Null = null
    def dict(d: js.Dictionary[String]): js.Dictionary[js.Array[Double]] = js.Dictionary()
    def promise(): js.Promise[String] = js.Promise.resolve[String]("a")
    def fun(f: js.Function1[Double, String]): js.Function0[Unit] = () => ()
    def dyn(x: js.Dynamic): js.Any = x
    def bytes(u: Uint8Array): Uint8Array = u
    def obj(o: js.Object): js.Object = o
    def opt(a: Double, b: js.UndefOr[String], c: js.UndefOr[Double]): Unit = ()
    def dflt(a: Double, b: String = "x"): Double = a
    val ro: Double = 1.0
    var rw: String = "s"
    def getter: Double = 2.0
    def overloaded(a: Double): Double = a
    def overloaded(a: Double, b: String): String = b
    @TsType("\"key\" | \"script\"")
    def credType(): String = "key"
    def config(c: Conf): Conf = c
    @TsIgnore
    def scalaOnly(i: java.time.Instant): java.time.Instant = i
}

/** Referenced but not exported: becomes an interface, renamed via @TsName. */
@TsName("Config")
trait Conf extends js.Object {

    /** Nested reference chases transitively. */
    val nested: js.UndefOr[js.Array[Inner]]
    val flag: Boolean
}

trait Inner extends js.Object {
    val id: String
}

/** Top-level exported functions from an object. */
@JSExportTopLevel("Tools")
object Tools {

    /** The library version. */
    @JSExport
    val version: String = "1.1.1"

    /** Doubles. */
    @JSExport
    @JSExportTopLevel("twice")
    @JSExportTopLevel("double")
    def twice(x: Double): Double = x * 2
    @JSExport
    def concat(a: String, b: js.UndefOr[String]): String = a
    private def internal(): Unit = ()
}

/** An exported js.Object singleton: every public member is exported. */
@JSExportTopLevel("Consts")
object Consts extends js.Object {

    /** The answer. */
    val answer: Double = 42
    def negate(x: Double): Double = -x
    private def secret(): Double = 0
}

/** Intersection types. */
@JSExportTopLevel("Intersections")
class Intersections extends js.Object {

    /** A user intersection maps to a TypeScript intersection. */
    def both(x: Conf & Inner): Conf & Inner = x

    /** js.Object on one side is kept rather than one side being silently picked. */
    def withObject(x: js.Object & Inner): js.Object & Inner = x
}

/** Only ever passed IN, like EmulatorInitialState: its arrays accept a caller's ReadonlyArray, and
  * that input position propagates to the traits nested inside it.
  */
trait InputCfg extends js.Object {
    val entries: js.Array[CfgEntry]
    val grid: js.Array[js.Array[Double]]
}

trait CfgEntry extends js.Object {
    val tags: js.Array[String]
}

/** Only ever handed BACK, like SubmitResult: its arrays stay mutable. */
trait OutputInfo extends js.Object {
    val notes: js.Array[String]
}

@JSExportTopLevel("Directional")
class Directional extends js.Object {
    def accept(cfg: InputCfg): Unit = ()
    def produce(): OutputInfo = null
}

/** A constructor `val` carrying @TsType narrows BOTH faces: the property and the constructor
  * parameter. Annotated on the parameter here, and via the field meta-target in [[BothFacesMeta]].
  */
@JSExportTopLevel("BothFaces")
class BothFaces(@TsType("\"a\" | \"b\"") val kind: String) extends js.Object

@JSExportTopLevel("BothFacesMeta")
class BothFacesMeta(
    @(TsType @scala.annotation.meta.field)("\"a\" | \"b\"") val kind: String
) extends js.Object

// --- direction settling: DirShared is reached from an INPUT subtree (DirIn) one pass before an
// OUTPUT subtree (DirOut -> DirNested) reaches it, so emitting during discovery would freeze it
// as input-only and wrongly mark its array readonly.
trait DirIn extends js.Object {
    val shared: DirShared
}

trait DirOut extends js.Object {
    val nested: DirNested
}

trait DirNested extends js.Object {
    val shared: DirShared
}

trait DirShared extends js.Object {
    val xs: js.Array[String]
}

@JSExportTopLevel("DirOrder")
class DirOrder extends js.Object {
    def take(a: DirIn): Unit = ()
    def give(): DirOut = null
}

/** A callback we invoke: its parameters are values we hand OUT, so they must stay mutable. */
@JSExportTopLevel("Callbacks")
class Callbacks extends js.Object {
    def each(f: js.Function1[js.Array[String], Unit]): Unit = ()
}

/** Scala's `@deprecated` must reach the .d.ts as the TSDoc tag: with a message and a version, with
  * a message alone, bare, and never duplicating a `@deprecated` already written by hand.
  */
@JSExportTopLevel("Legacy")
@deprecated("Use Point instead", "1.2.0")
class Legacy extends js.Object {
    @deprecated("Use Point.dist", "1.3.0")
    def distance(): Double = 0.0

    @deprecated("No replacement")
    def orphan(): Double = 0.0

    @deprecated
    val bare: Double = 1.0

    /** Still here for now.
      *
      * @deprecated
      *   hand-written wording wins over the annotation
      */
    @deprecated("annotation wording", "1.2.0")
    def handWritten(): Double = 0.0
}
