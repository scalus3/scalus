package scalus.cardano.ledger

import scalus.interop.TsType
import scalus.uplc.builtin.{platform, ByteString}
import scalus.utils.scalajs.internal.*

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** The hashes the ledger identifies scripts and datums by. */
object JsHashes {

    /** The hash a script is identified by: its policy id, or its payment credential.
      *
      * Takes the script as `Utxo.withScriptRef` does: `{ type, script }`, with a Plutus program in
      * any CBOR wrapping, or the ledger's `script_ref` CBOR. The wrapping does not change the hash.
      *
      * @return
      *   the 28-byte hash, as lowercase hex
      * @throws TypeError
      *   if `script` is neither form, its hex is invalid, or a `"Native"` script or a `script_ref`
      *   does not decode
      */
    @JSExportTopLevel("scriptHash")
    def scriptHash(
        @TsType(
          "Uint8Array | { readonly type: \"Native\" | \"PlutusV1\" | \"PlutusV2\" | \"PlutusV3\"; readonly script: string | Uint8Array }"
        ) script: js.Any
    ): String = JsUtxo.scriptOf(script).scriptHash.toHex

    /** The hash a datum is identified by, in an output's datum hash or a witness set.
      *
      * Hashes the CBOR exactly as given: two encodings of the same `Data` hash differently, as on
      * the ledger.
      *
      * @param data
      *   the datum, CBOR-encoded `Data`, as hex or bytes
      * @return
      *   the 32-byte hash, as lowercase hex
      */
    @JSExportTopLevel("dataHash")
    def dataHash(@TsType("string | Uint8Array") data: js.Any): String =
        platform.blake2b_256(ByteString.unsafeFromArray(bytesOf(data, "data"))).toHex
}
