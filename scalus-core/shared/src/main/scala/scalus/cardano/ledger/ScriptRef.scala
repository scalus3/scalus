package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.Tag.EmbeddedCBOR
import org.typelevel.paiges.Doc.text
import scalus.utils.Pretty
import scalus.utils.Pretty.{ctr, inParens}

/** Represents a reference to a script in Cardano */
case class ScriptRef(script: Script)

object ScriptRef:
    /** CBOR encoder for ScriptRef */
    given Encoder[ScriptRef] with
        def write(w: Writer, value: ScriptRef): Writer =
            // Tag 24 is used for embedded CBOR
            w.writeTag(EmbeddedCBOR)

            // Serialize the script to CBOR bytes
            val scriptBytes = Cbor.encode(value.script).toByteArray

            // Write the bytes
            w.writeBytes(scriptBytes)
            w

    /** CBOR decoder for ScriptRef */
    given Decoder[ScriptRef] with
        def read(r: Reader): ScriptRef =
            // Check for tag 24 (embedded CBOR)
            val tag = r.readTag()
            if tag != EmbeddedCBOR then
                r.validationFailure(s"Expected tag 24 for ScriptRef, got $tag")

            // Read the embedded CBOR bytes
            val bytes: Array[Byte] = r.readBytes()

            // Parse the bytes as a Script
            val script = Cbor.decode(bytes).to[Script].value

            ScriptRef(script)

    /** Pretty prints ScriptRef with script hash (concise) or full hex (detailed) */
    given Pretty[ScriptRef] = Pretty.instanceWithDetailed(
      concise = (ref, style) => Pretty[Script].pretty(ref.script, style),
      detailed = (ref, style) =>
          ref.script match
              case Script.Native(s) =>
                  ctr("Native", style) + inParens(Pretty[Timelock].pretty(s, style))
              case Script.PlutusV1(s) =>
                  ctr("PlutusV1", style) + inParens(text(s.toHex))
              case Script.PlutusV2(s) =>
                  ctr("PlutusV2", style) + inParens(text(s.toHex))
              case Script.PlutusV3(s) =>
                  ctr("PlutusV3", style) + inParens(text(s.toHex))
    )
