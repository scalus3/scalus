package scalus.utils.scalajs.internal

import io.bullet.borer.{Cbor, Decoder, Encoder}
import scalus.uplc.builtin.ByteString
import scalus.utils.Hex

import scala.annotation.tailrec
import scala.scalajs.js
import scala.scalajs.js.typedarray.{byteArray2Int8Array, int8Array2ByteArray, Int8Array, Uint8Array}
import scala.util.control.NonFatal

// Helpers for the Scala.js facades that the npm package exports. They are all in this one file, so
// a single read shows every helper there is.

// ---- Bytes ----
//
// Conversions between Scala byte arrays or `ByteString`s and JavaScript `Uint8Array`s.
//
// `byteArray2Int8Array` and `int8Array2ByteArray` are Scala.js optimizer intrinsics, each a single
// typed-array copy; element-wise alternatives such as `toJSArray` or `toArray.map(_.toByte)` loop
// over every byte.

extension (bytes: Array[Byte])
    /** A fresh `Uint8Array` holding a copy of `bytes`. */
    private[scalus] def toUint8Array: Uint8Array = new Uint8Array(byteArray2Int8Array(bytes).buffer)

extension (bs: ByteString)
    /** A fresh `Uint8Array` holding a copy of the bytes. */
    private[scalus] def toUint8Array: Uint8Array = bs.bytes.toUint8Array

extension (array: Uint8Array)
    /** A copy of the bytes this view covers, which may be part of a larger buffer. */
    private[scalus] def toByteArray: Array[Byte] =
        int8Array2ByteArray(new Int8Array(array.buffer, array.byteOffset, array.length))

    /** A `ByteString` holding a copy of the bytes this view covers. */
    private[scalus] def toByteString: ByteString = ByteString.unsafeFromArray(array.toByteArray)

/** CBOR to and from a JavaScript `Uint8Array`.
  *
  * An object rather than top-level functions: a wildcard-imported `toCbor` would collide with the
  * `Data.toCbor` extension.
  */
private[scalus] object JsCbor {
    def encode[A: Encoder](value: A): Uint8Array = Cbor.encode(value).toByteArray.toUint8Array

    def decode[A: Decoder](array: Uint8Array): A = Cbor.decode(array.toByteArray).to[A].value
}

// ---- Numbers ----
//
// Conversions between Scala integers and JavaScript `number`/`bigint`, and reading integers a
// JavaScript caller passed in.
//
// Scala.js has no direct conversion between `Long` and `js.BigInt`: `js.BigInt` is built from a
// `Double`, which is exact only up to 2^53, or from a `String`. The decimal string is the exact
// route, so both directions go through it here and nowhere else.
//
// The readers take `js.Any` because the shipped bundle does not check `asInstanceOf`: a field
// declared `bigint` may hold a `number`, a string or a fraction, and each must fail with a
// `TypeError` rather than become a wrong value.

extension (n: Long) private[scalus] def toJsBigInt: js.BigInt = js.BigInt(n.toString)

extension (n: BigInt) private[scalus] def toJsBigInt: js.BigInt = js.BigInt(n.toString)

/** A `number` that is a safe integer, or a `bigint` that fits 64 bits. */
private[scalus] def longOf(value: js.Any, name: String): Long =
    if js.typeOf(value) == "bigint" then
        value.toString.toLongOption.getOrElse(typeError(s"$name must fit in 64 bits"))
    else safeInteger(value, name).toLong

/** A safe integer that fits `Int`: `.toInt` on a `Double` saturates rather than failing. */
private[scalus] def intOf(value: js.Any, name: String): Int = {
    val safe = safeInteger(value, name)
    if !safe.isValidInt then typeError(s"$name must be an integer up to ${Int.MaxValue}")
    safe.toInt
}

/** A `number` that is an integer JavaScript represents exactly. */
private[scalus] def safeInteger(value: js.Any, name: String): Double = {
    if !js.Dynamic.global.Number.isSafeInteger(value).asInstanceOf[Boolean] then
        typeError(s"$name must be a safe integer")
    value.asInstanceOf[Double]
}

// ---- Inputs ----
//
// Readers for byte and array inputs a JavaScript caller passed in. Like `longOf`, each takes the
// name of what it reads, and throws a `TypeError` naming it when the value cannot be read.

/** The bytes of a `string | Uint8Array`: hex is parsed, bytes are copied. */
private[scalus] def bytesOf(value: js.Any, name: String): Array[Byte] =
    if js.typeOf(value) == "string" then
        try Hex.hexToBytes(value.asInstanceOf[String])
        catch case e: IllegalArgumentException => typeError(s"$name: ${e.getMessage}")
    else if value.isInstanceOf[Uint8Array] then value.asInstanceOf[Uint8Array].toByteArray
    else typeError(s"$name must be a hexadecimal string or Uint8Array")

/** The elements of an array, each paired with its name, `name[i]`.
  *
  * Checked, because anything else would read as length `undefined`: no elements, silently.
  */
private[scalus] def arrayOf(value: js.Any, name: String): IndexedSeq[(js.Any, String)] = {
    if !js.Array.isArray(value) then typeError(s"$name must be an array")
    val array = value.asInstanceOf[js.Array[js.Any]]
    IndexedSeq.tabulate(array.length)(i => (array(i), s"$name[$i]"))
}

/** The bytes of a `string | Uint8Array`, decoded; any failure to decode is a `TypeError`. */
private[scalus] def decodeOf[A](value: js.Any, name: String)(decode: Array[Byte] => A): A =
    decodeBytes(bytesOf(value, name), name)(decode)

/** Bytes decoded; any failure to decode is a `TypeError` naming them. */
private[scalus] def decodeBytes[A](bytes: Array[Byte], name: String)(decode: Array[Byte] => A): A =
    try decode(bytes)
    catch
        case e: js.JavaScriptException => throw e
        case NonFatal(e)               => typeError(s"$name is not valid: ${e.getMessage}")

/** The flat program bytes of a script as hex or bytes of raw flat, single or double CBOR. */
private[scalus] def flatOf(input: js.Any, name: String): Array[Byte] =
    stripLayers(bytesOf(input, name), name)

/** Strips CBOR byte-string layers until the flat program is reached: a flat program starts with its
  * major version, the CBOR integer 1, so the first item that is not a byte string is the program.
  */
@tailrec
private def stripLayers(bytes: Array[Byte], name: String): Array[Byte] =
    byteStringLayer(bytes, name) match
        case Some(inner) => stripLayers(inner, name)
        case None        => bytes

/** The content of `bytes` if they are exactly one CBOR byte string, `None` if they do not start
  * with one.
  */
private def byteStringLayer(bytes: Array[Byte], name: String): Option[Array[Byte]] =
    decodeBytes(bytes, name)(
      Cbor.decode(_).withPrefixOnly.to[Option[Array[Byte]]](using byteString).value
    )

private val byteString: Decoder[Option[Array[Byte]]] = Decoder { r =>
    if r.hasBytes then
        val content = r.readByteArray()
        r.readEndOfInput()
        Some(content)
    else None
}

// ---- Errors ----

private[scalus] def typeError(message: String): Nothing =
    throw js.JavaScriptException(new js.TypeError(message))

/** Runs `body` so that whatever escapes is something JavaScript can read: a JS error passes through
  * untouched, any other Scala failure becomes a plain `Error` with its message.
  */
private[scalus] def surfacingErrors[A](body: => A): A =
    try body
    catch
        case e: js.JavaScriptException => throw e
        case NonFatal(e) =>
            throw js.JavaScriptException(new js.Error(Option(e.getMessage).getOrElse(e.toString)))

// ---- Exports ----

/** Makes the `@JSExport` methods of an exported object callable detached, as
  * `const { evaluateTx } = evaluator` or `const f = Scalus.evalPlutusScripts`.
  *
  * Call it first in the body of every `@JSExportTopLevel` object.
  *
  * Why it is needed: Scala.js exports the members of an object as it exports those of a class, as
  * prototype methods that call `this.internalName(...)`. Called detached, `this` is undefined and
  * the call throws. JavaScript expects the members of a namespace object to work detached, as
  * `Math.max` and `JSON.parse` do, and the generated d.ts gives them no `this` type, so TypeScript
  * accepts the detached call. scalus 0.18.1 worked by accident: the Closure Compiler rewrote the
  * singleton's methods to plain closures. The Closure Compiler is gone (deprecated in Scala.js
  * 1.21), and so was the accident; `@lucid-evolution/scalus-uplc` 0.1.x broke on 1.x.
  *
  * Alternatives rejected:
  *   - `@JSExportTopLevel("uplc.applyArgs")`: Scala.js 1.x allows only plain identifiers, no
  *     namespaces.
  *   - `@JSExportTopLevel(name, moduleID = "uplc")` plus `export * as uplc` in the bundle entry:
  *     native ESM namespaces, but it needs a multi-module build and d.ts namespaces the exporter
  *     does not emit, and it cannot fix `Scalus`, whose shape is public.
  *   - `@JSExport val f: js.FunctionN`: the d.ts loses the parameter docs and optional parameters,
  *     and `def` to `val` breaks MiMa.
  *   - A post-link wrapper that binds in the bundle entry: fixes only the npm bundle, and the
  *     Scala.js tests never run it.
  *
  * Every function on the object's own prototype is bound, the mangled internal methods too; that is
  * harmless and needs no list of names to keep in step.
  */
private[scalus] def bindExports(self: AnyRef): Unit = {
    val obj = self.asInstanceOf[js.Dynamic]
    val proto = js.Object.getPrototypeOf(self.asInstanceOf[js.Object])
    for name <- js.Object.getOwnPropertyNames(proto) if name != "constructor" do
        val member = js.Object.getOwnPropertyDescriptor(proto, name).asInstanceOf[js.Dynamic].value
        if js.typeOf(member) == "function" then obj.updateDynamic(name)(member.bind(obj))
}
