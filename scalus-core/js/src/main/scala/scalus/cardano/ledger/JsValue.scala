package scalus.cardano.ledger

import scalus.interop.TsName
import scalus.uplc.builtin.ByteString

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}

/** One native asset: a policy, a name under it, and how much of it.
  *
  * `policyId` and `assetName` are hex. `unit` is the two concatenated, which is what
  * lucid-evolution and MeshJS call a unit - provided so adapters need no string arithmetic.
  */
@JSExportTopLevel("Asset")
class JsAsset(
    private val policyIdHex: String,
    private val assetNameHex: String,
    private val amount: js.BigInt
) extends js.Object {
    // `def`, not `val`, on every handle accessor: see the plain-object rule below.
    def policyId: String = policyIdHex
    def assetName: String = assetNameHex
    def quantity: js.BigInt = amount
    def unit: String = policyIdHex + assetNameHex

    /** A plain object with the same fields. Assert on this, never on the handle. */
    def toObject(): JsPlainAsset = js.Dynamic
        .literal(policyId = policyId, assetName = assetName, quantity = quantity, unit = unit)
        .asInstanceOf[JsPlainAsset]
}

/** The structural form of [[JsAsset]]. */
@TsName("PlainAsset")
trait JsPlainAsset extends js.Object {
    val policyId: String
    val assetName: String
    val quantity: js.BigInt
    val unit: String
}

/** The structural form of [[JsValue]]. */
@TsName("PlainValue")
trait JsPlainValue extends js.Object {
    val coin: js.BigInt
    val assets: js.Array[JsPlainAsset]
}

/** An amount of ada together with any native assets beside it.
  *
  * This wraps the ledger's own `Value`: the object handed to you by a query holds the real thing,
  * so passing it back to the emulator costs no re-encoding.
  */
// Implementation note, deliberately NOT scaladoc - this file's scaladoc ships to npm as
// scalus.d.ts, where the Scala-side class names below mean nothing to the reader.
//
// The wrapped value is a `private var` that `wrap` overwrites, rather than a second, private
// constructor taking it. Scala.js rejects the latter outright - "private methods in non-native JS
// classes cannot be overloaded" - for plain and for qualified `private` alike, so a handle can have
// either one constructor or several public ones, never a private one beside a public one.
// Overwriting is the same idiom `JEmulator.replaceEmulator` uses, and it keeps the wrapped value
// stored by reference: `wrap` copies nothing.
@JSExportTopLevel("Value")
class JsValue(lovelace: js.BigInt) extends js.Object {

    private var value: Value = Value(Coin(BigInt(lovelace.toString).toLong))

    /** Lovelace plus native assets. Arity tells this apart from the lovelace-only constructor. */
    def this(lovelace: js.BigInt, assets: js.Array[JsAsset]) = {
        this(lovelace)
        value = Value(value.coin, JsValue.multiAssetOf(assets))
    }

    /** Lovelace. A `bigint` because the ada supply exceeds `Number.MAX_SAFE_INTEGER`. */
    def coin: js.BigInt = js.BigInt(value.coin.value.toString)

    /** The native assets, in ledger order. Empty for pure ada. */
    def assets: js.Array[JsAsset] = {
        val out = js.Array[JsAsset]()
        value.assets.assets.foreach { case (policyId, byName) =>
            byName.foreach { case (name, quantity) =>
                out.push(
                  new JsAsset(policyId.toHex, name.bytes.toHex, js.BigInt(quantity.toString))
                )
            }
        }
        out
    }

    /** This value plus another. Neither operand is modified. */
    def plus(other: JsValue): JsValue = JsValue.wrap(value + other.value)

    // parens required: a JS class member without them is a property, not a method
    override def toString(): String = value.toString

    /** A plain object with the same fields.
      *
      * Handle accessors live on the prototype, so `JSON.stringify`, spread and (the dangerous one)
      * vitest's `toEqual` all see an empty object, which makes `expect(a).toEqual(b)` pass for two
      * different values. Assert through this instead.
      */
    def toObject(): JsPlainValue = js.Dynamic
        .literal(coin = coin, assets = assets.map(_.toObject()))
        .asInstanceOf[JsPlainValue]
}

object JsValue {

    /** Internal bridge: wrap a ledger value without copying. Not exported. */
    private[scalus] def wrap(value: Value): JsValue = {
        val handle = new JsValue(js.BigInt("0"))
        handle.value = value
        handle
    }

    /** Internal bridge: the wrapped ledger value. Not exported.
      *
      * An extension method rather than a member of the class, because neither visibility a member
      * could have is safe.
      *
      * A public member is exported to JavaScript and emitted into `scalus.d.ts`, where `Value` has
      * no TypeScript representation, so `generateDts` fails on it. It is also a linker export root,
      * and a root keeps everything it reaches in `scalus.js`: see the ~800 KB timezone database
      * described on `TsIgnore` in `scalus.interop`.
      *
      * A `private[scalus]` member is invisible to JavaScript - Scala.js hides a qualified-private
      * member as it hides a plain `private` one - but `ExportCollector` emits it anyway, because
      * `visibleMember` filters on `Flags.Private`, which dotty does not set for qualified-private
      * symbols. The `.d.ts` would still name an unmappable type.
      */
    extension (self: JsValue) private[scalus] def underlying: Value = self.value

    /** `n` ada, as lovelace.
      *
      * A named factory rather than a constructor overload on purpose: `new Value(5n)` already means
      * five *lovelace*, so a same-arity constructor meaning five *ada* would be indistinguishable
      * at the call site and silently a million times off. The unit has to be in the name.
      */
    // The multiplication stays in `js.BigInt`, so it is exact, and the single narrowing to `Coin`'s
    // `Long` is the primary constructor's. Converting first and multiplying in `Long` - as this did
    // - overflows a million times sooner, silently.
    @JSExportStatic
    def ada(ada: js.BigInt): JsValue = new JsValue(ada * js.BigInt(1_000_000))

    private def multiAssetOf(assets: js.Array[JsAsset]): MultiAsset =
        assets.toSeq.foldLeft(MultiAsset.empty) { (acc, a) =>
            acc + MultiAsset.asset(
              ScriptHash.fromHex(a.policyId),
              AssetName(ByteString.fromHex(a.assetName)),
              BigInt(a.quantity.toString).toLong
            )
        }
}
