package scalus.cardano.ledger

import io.bullet.borer.*
import org.typelevel.paiges.Doc
import scalus.utils.{Pretty, Style}

/** Represents a Delegated Representative (DRep) in the Cardano blockchain.
  *
  * A DRep can be one of several types:
  *   - KeyHash: A DRep identified by a key hash
  *   - ScriptHash: A DRep identified by a script hash
  *   - AlwaysAbstain: A special DRep that always abstains from voting
  *   - AlwaysNoConfidence: A special DRep that always votes no confidence
  */
enum DRep {

    /** A DRep identified by a key hash.
      *
      * @param keyHash
      *   The key hash of the DRep
      */
    case KeyHash(keyHash: AddrKeyHash)

    /** A DRep identified by a script hash.
      *
      * @param scriptHash
      *   The script hash of the DRep
      */
    case ScriptHash(scriptHash: scalus.cardano.ledger.ScriptHash)

    /** A special DRep that always abstains from voting.
      */
    case AlwaysAbstain

    /** A special DRep that always votes no confidence.
      */
    case AlwaysNoConfidence
}

object DRep {

    /** CBOR Encoder for DRep. Encodes as a tagged array:
      *   - [0, keyHash] for KeyHash
      *   - [1, scriptHash] for ScriptHash
      *   - [2] for AlwaysAbstain
      *   - [3] for AlwaysNoConfidence
      */
    given Encoder[DRep] with
        def write(w: Writer, value: DRep): Writer = value match {
            case DRep.KeyHash(keyHash) =>
                w.writeArrayOpen(2)
                    .writeInt(0)
                    .write(keyHash)
                    .writeArrayClose()

            case DRep.ScriptHash(scriptHash) =>
                w.writeArrayOpen(2)
                    .writeInt(1)
                    .write(scriptHash)
                    .writeArrayClose()

            case DRep.AlwaysAbstain =>
                w.writeArrayOpen(1)
                    .writeInt(2)
                    .writeArrayClose()

            case DRep.AlwaysNoConfidence =>
                w.writeArrayOpen(1)
                    .writeInt(3)
                    .writeArrayClose()
        }

    /** CBOR Decoder for DRep. Decodes from a tagged array:
      *   - [0, keyHash] for KeyHash
      *   - [1, scriptHash] for ScriptHash
      *   - [2] for AlwaysAbstain
      *   - [3] for AlwaysNoConfidence
      */
    given Decoder[DRep] with {
        def read(r: Reader): DRep = {
            r.readArrayHeader()
            val tag = r.readInt()

            val drep = tag match
                case 0 =>
                    DRep.KeyHash(r.read[AddrKeyHash]())

                case 1 =>
                    DRep.ScriptHash(r.read[scalus.cardano.ledger.ScriptHash]())

                case 2 =>
                    DRep.AlwaysAbstain

                case 3 =>
                    DRep.AlwaysNoConfidence

                case _ =>
                    r.validationFailure(s"Invalid DRep type: $tag")

            drep
        }
    }

    import Doc.*
    import Pretty.inParens

    /** Pretty prints DRep */
    given Pretty[DRep] with
        def pretty(a: DRep, style: Style): Doc = a match
            case DRep.KeyHash(hash)      => text("DRep.KeyHash") + inParens(text(hash.toHex))
            case DRep.ScriptHash(hash)   => text("DRep.ScriptHash") + inParens(text(hash.toHex))
            case DRep.AlwaysAbstain      => text("DRep.AlwaysAbstain")
            case DRep.AlwaysNoConfidence => text("DRep.AlwaysNoConfidence")
}
