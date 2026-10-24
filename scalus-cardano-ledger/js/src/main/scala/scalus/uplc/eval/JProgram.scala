package scalus.uplc.eval

import scalus.interop.{TsIgnore, TsType}
import scalus.uplc.DeBruijnedProgram
import scalus.utils.scalajs.internal.*

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.typedarray.Uint8Array

/** A UPLC program, decoded once, to evaluate many times without decoding it again.
  *
  * Pass it to `PlutusVM.evaluateScript` or `evaluator.evaluateScript` in place of the script bytes.
  * `uplc.decodeProgram(script)` makes one too.
  *
  * ```ts
  * const program = uplc.decodeProgram(scriptHex)
  * const vm = new PlutusVM(EvaluationOptions.mainnet("PlutusV3"))
  * vm.evaluateScript(program, [datum, redeemer, context])
  * ```
  *
  * @param script
  *   the script, as hex or bytes of raw flat, single CBOR or double CBOR
  * @throws TypeError
  *   if the script cannot be read
  */
@JSExportTopLevel("Program")
class JProgram(@TsType("string | Uint8Array") script: js.Any) extends js.Object {

    @TsIgnore private[eval] final val underlying: DeBruijnedProgram = JUplc.decodeScript(script)

    /** The program as flat bytes, as `uplc.decodeToFlat` returns them. */
    def toFlat(): Uint8Array = underlying.flatEncoded.toUint8Array
}

private[eval] object JProgram {

    /** The program of a script argument: the one a `Program` holds, else the decoded bytes. Throws
      * a `TypeError` for anything else. A `Program` of another copy of this package is not a
      * `Program` here: `instanceof` tells them apart.
      */
    def of(script: js.Any): DeBruijnedProgram =
        if script.isInstanceOf[JProgram] then script.asInstanceOf[JProgram].underlying
        else if js.typeOf(script) == "string" || script.isInstanceOf[Uint8Array] then
            JUplc.decodeScript(script)
        else typeError("script must be a hexadecimal string, a Uint8Array or a Program")
}
