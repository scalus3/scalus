package scalus.uplc.eval

import io.bullet.borer.Cbor
import scalus.interop.TsType
import scalus.uplc.DeBruijnedProgram
import scalus.uplc.builtin.Data
import scalus.utils.Hex
import scalus.utils.scalajs.internal.*

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.scalajs.js.typedarray.Uint8Array

/** UPLC scripts as bytes.
  *
  * The primitives work on flat program bytes: [[decodeToFlat]] takes a script in any form a caller
  * holds, and [[applyArgs]] applies arguments to it. Together with `cbor.wrapBytes` and
  * `bytesToHex` they compose into any envelope a tool expects. [[applyParamsToScript]] is the
  * common composition in one call. Every member throws a `TypeError` for input it cannot read.
  */
@JSExportTopLevel("uplc")
object JUplc {
    bindExports(this)

    /** The flat program bytes of a script, whatever form it arrives in.
      *
      * Takes hex or bytes of raw flat, single CBOR or double CBOR. Only the envelopes are read, so
      * this is cheap and the script's own bytes come back unchanged; the program is decoded where
      * it is used, by [[applyArgs]] or the evaluator.
      *
      * @param script
      *   the script, as hex or bytes
      * @return
      *   the flat program bytes
      */
    @JSExport
    def decodeToFlat(@TsType("string | Uint8Array") script: js.Any): Uint8Array =
        flatOf(script, "script").toUint8Array

    /** Applies `Data` arguments to a flat program, left to right.
      *
      * @param flat
      *   the flat program bytes, as [[decodeToFlat]] returns them
      * @param args
      *   the arguments, each a CBOR-encoded `Data`, as hex or bytes
      * @return
      *   the applied program, as flat bytes
      */
    @JSExport
    def applyArgs(
        flat: Uint8Array,
        @TsType("readonly (string | Uint8Array)[]") args: js.Any
    ): Uint8Array = {
        val program = decodeOf(flat, "flat")(DeBruijnedProgram.fromFlatEncoded)
        applyAll(program, decodeArgs(args)).flatEncoded.toUint8Array
    }

    /** Applies CBOR-encoded `Data` parameters to a script and returns it as double-CBOR hex.
      *
      * The same contract as Lucid's and Mesh's `applyParamsToScript`: any script form in, the
      * double-CBOR hex their script objects hold out. It is
      * `bytesToHex(cbor.wrapBytes(cbor.wrapBytes(uplc.applyArgs(uplc.decodeToFlat(script),
      * params))))`.
      *
      * @param script
      *   the script, as hex or bytes of raw flat, single CBOR or double CBOR
      * @param params
      *   the parameters, each a CBOR-encoded `Data`, as hex or bytes
      * @return
      *   the applied script, double-CBOR, as lowercase hex
      */
    @JSExport
    def applyParamsToScript(
        @TsType("string | Uint8Array") script: js.Any,
        @TsType("readonly (string | Uint8Array)[]") params: js.Any
    ): String = {
        applyAll(decodeScript(script), decodeArgs(params, "params")).doubleCborHex
    }

    /** Decodes a script in any form a caller holds.
      *
      * Only what reading the bytes requires is checked. The program's version, its padding and any
      * bytes after it are not: whether a chain would admit this script is
      * `PlutusScript.isWellFormed`'s question.
      */
    private[eval] def decodeScript(input: js.Any): DeBruijnedProgram =
        decodeBytes(flatOf(input, "script"), "script")(DeBruijnedProgram.fromFlatEncoded)

    private[eval] def decodeArgs(args: js.Any, name: String = "args"): List[Data] =
        arrayOf(args, name).map(decodeOf(_, _)(Data.fromCbor)).toList

    private[eval] def applyAll(program: DeBruijnedProgram, args: List[Data]): DeBruijnedProgram =
        args.foldLeft(program)(_.applyArg(_))
}

/** A CBOR byte-string envelope, one layer at a time. */
@JSExportTopLevel("cbor")
object JCbor {
    bindExports(this)

    /** Wraps bytes in one CBOR byte string: flat to single CBOR, or single to double. */
    @JSExport
    def wrapBytes(bytes: Uint8Array): Uint8Array =
        JsCbor.encode(bytesOf(bytes, "bytes"))

    /** Removes one CBOR byte-string layer: double CBOR to single, or single to flat.
      *
      * @throws TypeError
      *   if the bytes are not a CBOR byte string
      */
    @JSExport
    def unwrapBytes(bytes: Uint8Array): Uint8Array =
        decodeOf(bytes, "bytes")(Cbor.decode(_).to[Array[Byte]].value).toUint8Array
}

/** Hex encoding, for the tools that pass bytes around as strings. */
object JHex {

    /** The bytes as lowercase hex. */
    @JSExportTopLevel("bytesToHex")
    def bytesToHex(bytes: Uint8Array): String = Hex.bytesToHex(bytesOf(bytes, "bytes"))

    /** The bytes a hex string spells, in either case.
      *
      * @throws TypeError
      *   if the string has an odd length or a non-hex character
      */
    @JSExportTopLevel("hexToBytes")
    def hexToBytes(@TsType("string") hex: js.Any): Uint8Array = {
        if js.typeOf(hex) != "string" then typeError("hex must be a string")
        bytesOf(hex, "hex").toUint8Array
    }
}
