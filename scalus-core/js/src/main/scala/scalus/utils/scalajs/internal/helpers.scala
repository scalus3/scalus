package scalus.utils.scalajs.internal

import io.bullet.borer.{Cbor, Decoder, Encoder}
import scalus.uplc.builtin.ByteString

import scala.scalajs.js
import scala.scalajs.js.typedarray.{byteArray2Int8Array, int8Array2ByteArray, Int8Array, Uint8Array}

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

// ---- Errors ----

private[scalus] def typeError(message: String): Nothing =
    throw js.JavaScriptException(new js.TypeError(message))
