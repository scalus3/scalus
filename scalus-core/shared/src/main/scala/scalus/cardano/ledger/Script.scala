package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.derivation.key
import org.typelevel.paiges.Doc
import scalus.uplc.builtin.{platform, ByteString}
import scalus.uplc.{DeBruijnedProgram, ProgramFlatCodec}
import scalus.utils.{Pretty, Style}

import scala.util.control.NonFatal

/** Represents a script in Cardano */
sealed trait Script {

    def scriptHash: ScriptHash
}

sealed trait PlutusScript extends Script {
    def script: ByteString

    /** Get script language */
    def language: Language

    /** Get the De Bruijn-indexed program from the script bytes */
    def deBruijnedProgram: DeBruijnedProgram =
        DeBruijnedProgram.fromCbor(script.bytes)

    def toHex: String = script.toHex

    def isWellFormed(majorProtocolVersion: MajorProtocolVersion): Boolean = {
        PlutusScript.isWellFormed(script, language, majorProtocolVersion)
    }
}

object PlutusScript {
    def isWellFormed(
        script: ByteString,
        language: Language,
        majorProtocolVersion: MajorProtocolVersion
    ): Boolean = {
        if majorProtocolVersion < language.introducedInVersion then return false

        val ProgramFlatCodec.DecodeResult(DeBruijnedProgram(_, term), remaining) =
            try DeBruijnedProgram.fromCborWithRemainingBytes(script.bytes)
            catch case NonFatal(_) => return false

        if language != Language.PlutusV1 && language != Language.PlutusV2 && remaining.nonEmpty
        then return false

        val collectedBuiltins = term.collectBuiltins
        val foundBuiltinsIntroducedIn =
            Builtins.findBuiltinsIntroducedIn(language, majorProtocolVersion)

        collectedBuiltins.subsetOf(foundBuiltinsIntroducedIn)
    }
}

object Script {

    /** Native script (timelock) */
    @key(0) final case class Native(script: Timelock) extends Script derives Codec {

        /** Get the script hash for this native script */
        @transient lazy val scriptHash: ScriptHash = script.scriptHash
    }

    /** Plutus V1 script */
    @key(1) final case class PlutusV1(override val script: ByteString) extends PlutusScript
        derives Codec {

        /** Get the script hash for this Plutus V1 script */
        @transient lazy val scriptHash: ScriptHash = Hash(
          platform.blake2b_224(ByteString.unsafeFromArray(1 +: script.bytes))
        )

        def language: Language = Language.PlutusV1
    }

    /** Plutus V2 script */
    @key(2) final case class PlutusV2(override val script: ByteString) extends PlutusScript
        derives Codec {

        /** Get the script hash for this Plutus V2 script */
        @transient lazy val scriptHash: ScriptHash = Hash(
          platform.blake2b_224(ByteString.unsafeFromArray(2 +: script.bytes))
        )

        def language: Language = Language.PlutusV2
    }

    /** Plutus V3 script */
    @key(3) final case class PlutusV3(override val script: ByteString) extends PlutusScript
        derives Codec {

        /** Get the script hash for this Plutus V3 script */
        @transient lazy val scriptHash: ScriptHash = Hash(
          platform.blake2b_224(ByteString.unsafeFromArray(3 +: script.bytes))
        )

        def language: Language = Language.PlutusV3
    }

    given Codec[Script] = deriveCodec

    import Doc.*
    import Pretty.inParens

    /** Pretty prints Script as `Native(hash)`, `PlutusV1(hash)`, etc. */
    given prettyScript: Pretty[Script] with
        def pretty(a: Script, style: Style): Doc =
            val hashDoc = inParens(text(a.scriptHash.toHex))
            a match
                case Script.Native(_)   => text("Native") + hashDoc
                case Script.PlutusV1(_) => text("PlutusV1") + hashDoc
                case Script.PlutusV2(_) => text("PlutusV2") + hashDoc
                case Script.PlutusV3(_) => text("PlutusV3") + hashDoc

    // Variant instances delegate to the main Pretty[Script]
    given Pretty[Script.Native] = (a, style) => prettyScript.pretty(a, style)
    given Pretty[Script.PlutusV1] = (a, style) => prettyScript.pretty(a, style)
    given Pretty[Script.PlutusV2] = (a, style) => prettyScript.pretty(a, style)
    given Pretty[Script.PlutusV3] = (a, style) => prettyScript.pretty(a, style)
}
