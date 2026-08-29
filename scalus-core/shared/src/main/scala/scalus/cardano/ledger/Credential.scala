package scalus.cardano.ledger

import io.bullet.borer.Codec
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.derivation.key
import org.typelevel.paiges.Doc
import scalus.utils.{Pretty, Style}

/** Represents a credential in the Cardano blockchain. A credential can be either a key hash or a
  * script hash.
  */
enum Credential derives Codec.All:
    /** Key hash credential */
    @key(0) case KeyHash(keyHash: AddrKeyHash)

    /** Script hash credential */
    @key(1) case ScriptHash(scriptHash: scalus.cardano.ledger.ScriptHash)

    /** Check if this credential is a key hash */
    def isKeyHash: Boolean = this match
        case KeyHash(_) => true
        case _          => false

    /** Check if this credential is a script hash */
    def isScriptHash: Boolean = this match
        case ScriptHash(_) => true
        case _             => false

    /** Get the script hash if this credential is a `ScriptHash` */
    def scriptHashOption: Option[scalus.cardano.ledger.ScriptHash] = this match
        case ScriptHash(hash) => Some(hash)
        case _                => None

    /** Get the key hash if this credential is a `KeyHash` */
    def keyHashOption: Option[AddrKeyHash] = this match
        case KeyHash(hash) => Some(hash)
        case _             => None

object Credential:
    import Doc.*
    import Pretty.inParens

    /** Ordering matches the ledger's derived `Ord (Credential kr)`, which is derived on
      * `ScriptHashObj | KeyHashObj` (`libs/cardano-ledger-core/.../Credential.hs:98-101`), so
      * **script credentials sort before key credentials**, then by hash.
      *
      * This is the order the ledger's `Set (Credential ColdCommitteeRole)` iterates in, and hence
      * the order a governance action's removed-committee-member list is serialised in.
      */
    given Ordering[Credential] with
        private def tag(c: Credential): Int = c match
            case Credential.ScriptHash(_) => 0
            case Credential.KeyHash(_)    => 1

        def compare(x: Credential, y: Credential): Int =
            tag(x).compare(tag(y)) match
                case 0 =>
                    (x, y) match
                        case (Credential.ScriptHash(a), Credential.ScriptHash(b)) =>
                            Ordering[scalus.cardano.ledger.ScriptHash].compare(a, b)
                        case (Credential.KeyHash(a), Credential.KeyHash(b)) =>
                            Ordering[AddrKeyHash].compare(a, b)
                        case _ => 0 // unreachable: equal tags imply the same constructor
                case c => c

    /** Pretty prints Credential as `KeyHash(hash)` or `ScriptHash(hash)` */
    given Pretty[Credential] with
        def pretty(a: Credential, style: Style): Doc = a match
            case Credential.KeyHash(hash) =>
                Pretty.ctr("KeyHash", style) + inParens(Pretty.typ(text(hash.toHex), style))
            case Credential.ScriptHash(hash) =>
                Pretty.ctr("ScriptHash", style) + inParens(Pretty.typ(text(hash.toHex), style))
