package scalus.cardano.ledger

import io.bullet.borer.Cbor
import scalus.cardano.address.Address
import scalus.interop.TsName
import scalus.uplc.builtin.Data

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}
import scala.scalajs.js.typedarray.{byteArray2Int8Array, Uint8Array}
import scala.util.control.NonFatal

/** The structural form of [[JsUtxo]]. */
@TsName("PlainUtxo")
trait JsPlainUtxo extends js.Object {
    val txHash: String
    val outputIndex: Double
    val address: String
    val value: JsPlainValue
    val datumHash: js.UndefOr[String]
    val inlineDatum: js.UndefOr[Uint8Array]
    val scriptRef: js.UndefOr[Uint8Array]
    val scriptLanguage: js.UndefOr[String]
}

/** One unspent output: where it is, whose it is, and what it holds.
  *
  * Holds the ledger's own input and output, so a `Utxo` a query hands you can be handed straight
  * back - to `evaluateTx` or `addUtxo` - with no encoding step in between. Constructing one from
  * strings costs nothing until an accessor needs the parsed form.
  */
// Implementation notes, deliberately NOT scaladoc: this file's scaladoc is published to npm as
// scalus.d.ts, where `JsUtxo`, `JsValue` and plan task numbers mean nothing to the reader.
//
// SHAPE. A handle can have one constructor, or several public ones, never a private one beside a
// public one (Scala.js: "Private methods in non-native JS classes cannot be overloaded", and
// qualified `private` fails identically). So `wrap` has to reach the ledger pair in through the
// public constructor, and overwrite.
//
// ONE REPRESENTATION. The instance holds the ledger's `TransactionInput`/`TransactionOutput` and
// nothing else; the constructor's strings are parsed on the spot and not kept. An earlier version
// stored the raw strings too and decoded lazily, which cost more than it saved: two stores that
// could disagree, accessors that hex-decoded a string only to re-encode it back, and an invalid
// argument surfacing later from a *getter* instead of from the call that passed it.
//
// WHY THE SENTINEL. `wrap` already has the parsed pair, and must not pay for a decode to get an
// instance to put it in. Constructing through the public constructor with a placeholder address
// would bech32-decode that placeholder once per row of a `getUtxos()` result. So `wrap` passes
// `WrapPlaceholder` - compared by reference, so no JavaScript caller can collide with it - and the
// constructor skips parsing for it alone. `JsValue.wrap` pays its equivalent cost, a
// `Value(Coin(0))` allocation, because allocating a zero is free and decoding an address is not.
@JSExportTopLevel("Utxo")
class JsUtxo(txHash0: String, outputIndex0: Double, address0: String, value0: JsValue)
    extends js.Object {

    // `in`/`out`, not `input`/`output`: those names are reserved for the companion's extension
    // methods, which other Scalus-internal code uses to read the wrapped pair. Both are written
    // once - here, or by `wrap` - and never again.
    private var in: TransactionInput = null
    private var out: TransactionOutput = null

    // Set for a handle built from strings, where the caller's bech32 is the answer `address` owes
    // them. Left null by `wrap`, which has no string and fills this on first use instead: encoding
    // is the expensive direction, and `toObject`/`toString` both ask for it.
    private var addressMemo: String = null

    if !(address0 eq JsUtxo.WrapPlaceholder) then {
        in = TransactionInput(TransactionHash.fromHex(txHash0), outputIndex0.toInt)
        // `Address.fromString` tries bech32 and then Byron Base58, and reports whichever decoder
        // got furthest - `Invalid Base58 character: '-'` for `not-bech32`, which does not tell the
        // caller that the constructor's third argument is an address.
        val parsed =
            try Address.fromString(address0)
            catch
                case NonFatal(e) =>
                    throw new IllegalArgumentException(
                      s"`$address0` is not a Cardano address (addr1.../addr_test1... or a " +
                          s"Byron Base58 address): ${e.getMessage}"
                    )
        out = TransactionOutput(parsed, value0.underlying)
        addressMemo = address0
    }

    def txHash: String = in.transactionId.toHex
    def outputIndex: Double = in.index.toDouble

    /** Bech32 address.
      *
      * Throws when the ledger address cannot be bech32-encoded - which happens only for an address
      * on a network id outside `{Mainnet, Testnet}` (`Network.Other`), since bech32 has no defined
      * human-readable prefix for an arbitrary network id. This is a genuine "cannot represent"
      * case, not malformed input, so it fails loudly rather than degrading to hex: a caller
      * comparing, filtering on, or handing this value to a wallet needs to know its address is
      * unrepresentable, not receive a string that silently is not one.
      */
    def address: String = {
        if addressMemo == null then
            addressMemo = out.address.encode.getOrElse(
              throw new IllegalStateException(
                s"Address ${out.address.toHex} cannot be encoded to bech32: no human-readable prefix is defined for its network"
              )
            )
        addressMemo
    }

    def value: JsValue = JsValue.wrap(out.value)

    /** The datum hash, when the output references a datum rather than carrying one. */
    def datumHash: js.UndefOr[String] = out.datumOption match
        case Some(DatumOption.Hash(h)) => h.toHex
        case _                         => js.undefined

    /** The datum itself as CBOR, when the output carries it inline. */
    def inlineDatum: js.UndefOr[Uint8Array] = out.datumOption match
        case Some(DatumOption.Inline(d)) => toUint8Array(Cbor.encode(d).toByteArray)
        case _                           => js.undefined

    /** The reference script as CBOR, when the output carries one. */
    def scriptRef: js.UndefOr[Uint8Array] =
        out.scriptRef.map(r => toUint8Array(Cbor.encode(r).toByteArray)).orUndefined

    /** Which language `scriptRef` is written in. */
    def scriptLanguage: js.UndefOr[String] = out.scriptRef
        .map(_.script)
        .map {
            case _: Script.Native   => "Native"
            case _: Script.PlutusV1 => "PlutusV1"
            case _: Script.PlutusV2 => "PlutusV2"
            case _: Script.PlutusV3 => "PlutusV3"
            case _: Script.PlutusV4 => "PlutusV4"
        }
        .orUndefined

    /** This UTxO as a one-entry CBOR map from input to output, the shape `getUtxosCbor` uses. */
    def toCbor(): Uint8Array =
        toUint8Array(Cbor.encode(Map(in -> out): Utxos).toByteArray)

    /** A copy carrying `hash` as a datum hash, in place of whatever datum this output had. */
    def withDatumHash(hash: String): JsUtxo =
        JsUtxo.wrap(in, withOutput(datumOption = Some(DatumOption.Hash(DataHash.fromHex(hash)))))

    /** A copy carrying the CBOR-decoded value of `cbor` as its inline datum. */
    def withInlineDatum(cbor: Uint8Array): JsUtxo = {
        val data = Cbor.decode(cbor.toArray.map(_.toByte)).to[Data].value
        JsUtxo.wrap(in, withOutput(datumOption = Some(DatumOption.Inline(data))))
    }

    /** A copy carrying the CBOR-decoded value of `cbor` as its reference script. */
    def withScriptRef(cbor: Uint8Array): JsUtxo = {
        val scriptRef = Cbor.decode(cbor.toArray.map(_.toByte)).to[ScriptRef].value
        JsUtxo.wrap(in, withOutput(scriptRefOpt = Some(scriptRef)))
    }

    /** A new output over this one's address and value, changing only what is passed in. Always
      * Babbage-shaped: that is the only era able to hold an inline datum or a script reference.
      */
    private def withOutput(
        datumOption: Option[DatumOption] = out.datumOption,
        scriptRefOpt: Option[ScriptRef] = out.scriptRef
    ): TransactionOutput =
        TransactionOutput.Babbage(out.address, out.value, datumOption, scriptRefOpt)

    /** A plain object with the same fields.
      *
      * A handle's fields are accessors on the prototype, so `JSON.stringify`, object spread and
      * (the dangerous one) a test framework's `toEqual` all see an empty object on the handle
      * itself. Assert on this instead.
      */
    def toObject(): JsPlainUtxo = js.Dynamic
        .literal(
          txHash = txHash,
          outputIndex = outputIndex,
          address = address,
          value = value.toObject(),
          datumHash = datumHash,
          inlineDatum = inlineDatum,
          scriptRef = scriptRef,
          scriptLanguage = scriptLanguage
        )
        .asInstanceOf[JsPlainUtxo]

    override def toString(): String = s"Utxo($txHash#$outputIndex at $address)"

    private def toUint8Array(bytes: Array[Byte]): Uint8Array =
        new Uint8Array(byteArray2Int8Array(bytes).buffer)
}

object JsUtxo {

    /** The one address string the constructor does not parse. Compared by reference, so a
      * JavaScript caller passing an equal string still gets parsed normally - `eq` on a `String`
      * asks whether this is the very instance below, which only `wrap` can hand over.
      */
    private val WrapPlaceholder: String = new String("")

    /** Internal bridge: wrap a ledger pair with no re-encoding. Not exported.
      *
      * Writes the pair straight in, so a `getUtxos()` result never pays a hex or bech32 decode it
      * does not need - see the class doc on the sentinel.
      */
    private[scalus] def wrap(input: TransactionInput, output: TransactionOutput): JsUtxo = {
        val handle = new JsUtxo(WrapPlaceholder, 0.0, WrapPlaceholder, null)
        handle.in = input
        handle.out = output
        handle
    }

    /** Internal bridge: the wrapped ledger pair. Not exported - see `JsValue.underlying` for why
      * this is an extension method in the companion rather than a member of the class.
      */
    extension (self: JsUtxo) {
        private[scalus] def input: TransactionInput = self.in
        private[scalus] def output: TransactionOutput = self.out
    }

    /** Read back what `toCbor` wrote: a CBOR map holding exactly one input-to-output entry.
      *
      * A map of any other size is rejected, an empty one and a many-entry one alike. Taking the
      * first entry of a many-entry map would drop the rest without a word, and which one survived
      * would be whichever the decoded `Map` happened to iterate first - so a caller who passed a
      * whole UTxO set here (the shape `Emulator.getUtxosCbor` returns) would get one arbitrary UTxO
      * back and no hint that the others existed.
      *
      * @throws Error
      *   if the map does not hold exactly one entry.
      */
    @JSExportStatic
    def fromCbor(cbor: Uint8Array): JsUtxo = {
        val utxos = Cbor.decode(cbor.toArray.map(_.toByte)).to[Utxos].value
        if utxos.size != 1 then
            throw new IllegalArgumentException(
              s"expected a CBOR map holding exactly one UTxO, got ${utxos.size} entries"
            )
        val (input, output) = utxos.head
        wrap(input, output)
    }
}
