package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc.*
import scalus.uplc.builtin.{platform, ByteString}
import scalus.utils.Pretty.ctr
import scalus.utils.{Pretty, Style}

/** Represents a bootstrap witness in Cardano (for Byron-era addresses) */
case class BootstrapWitness(
    /** Public key (32 bytes) */
    publicKey: ByteString,

    /** Signature (64 bytes) */
    signature: ByteString,

    /** Chain code. Conventionally 32 bytes, but the ledger does not constrain it: the CDDL says
      * `chain_code : bytes` (conway.cddl:778) and Haskell holds it as an unconstrained
      * `ChainCode ByteArray` (Keys/Bootstrap.hs:67). Requiring 32 here made us reject transactions
      * the chain accepts.
      */
    chainCode: ByteString,

    /** Attributes */
    attributes: ByteString
) derives Codec:
    // vkey and signature ARE constrained by the CDDL (`bytes .size 32` / `.size 64`), so those
    // checks stay; chain_code and attributes are plain `bytes` and must not be checked.
    require(publicKey.size == 32, s"Public key must be 32 bytes, got ${publicKey.size}")
    require(signature.size == 64, s"Signature must be 64 bytes, got ${signature.size}")

    /** Rebuilds the `addrRoot` of the corresponding Byron address, matching cardano-ledger's
      * `bootstrapWitKeyHash` (Cardano/Ledger/Keys/Bootstrap.hs): Blake2b-224 of SHA3-256 of the
      * CBOR payload `[addrType 0, [spending-tag 0, bytes64(publicKey ++ chainCode)], attributes]`,
      * assembled with the constant prefix `0x83 0x00 0x82 0x00 0x58 0x40` (the attributes field is
      * the witness's raw CBOR blob, appended verbatim).
      */
    @transient lazy val addrKeyHash: AddrKeyHash = {
        val prefix = ByteString.fromHex("830082005840")
        val bytes = prefix ++ publicKey ++ chainCode ++ attributes
        Hash(platform.blake2b_224(platform.sha3_256(bytes)))
    }

object BootstrapWitness:
    given Ordering[BootstrapWitness] =
        Ordering.by[BootstrapWitness, ByteString](_.addrKeyHash)

    /** Pretty prints BootstrapWitness showing address key hash and truncated signature */
    given Pretty[BootstrapWitness] with
        def pretty(a: BootstrapWitness, style: Style): Doc =
            val fields = List(
              text("addrKeyHash:") & text(a.addrKeyHash.toHex),
              text("sig:") & text(a.signature.toHex.take(16) + "...")
            )
            (ctr("BootstrapWitness", style) + fill(comma + space, fields).tightBracketBy(
              char('('),
              char(')')
            )).grouped
