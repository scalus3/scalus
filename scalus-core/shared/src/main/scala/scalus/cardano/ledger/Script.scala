package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.derivation.key
import org.typelevel.paiges.Doc
import scalus.uplc.builtin.{platform, ByteString}
import scalus.uplc.{DeBruijnedProgram, Program, ProgramFlatCodec}
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

    /** Cached in-memory program, set via companion factory to avoid CBOR round-trip. */
    @transient @volatile private[ledger] var _cachedProgram: Program | Null = null

    /** Get the program, preferring the cached in-memory version over CBOR deserialization. */
    def program: Program = {
        val p = _cachedProgram
        if p != null then p
        else
            val deserialized = Program.fromCborByteString(script)
            _cachedProgram = deserialized
            deserialized
    }

    /** Get the De Bruijn-indexed program, preserving source annotations when available. */
    def deBruijnedProgram: DeBruijnedProgram = program.deBruijnedProgram

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

    object PlutusV1 {

        /** Create from an in-memory Program, caching it to preserve source annotations. */
        def apply(program: Program): PlutusV1 = {
            val s = new PlutusV1(program.cborByteString)
            s._cachedProgram = program
            s
        }
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

    object PlutusV2 {

        /** Create from an in-memory Program, caching it to preserve source annotations. */
        def apply(program: Program): PlutusV2 = {
            val s = new PlutusV2(program.cborByteString)
            s._cachedProgram = program
            s
        }
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

    object PlutusV3 {

        /** Create from an in-memory Program, caching it to preserve source annotations. */
        def apply(program: Program): PlutusV3 = {
            val s = new PlutusV3(program.cborByteString)
            s._cachedProgram = program
            s
        }
    }

    /** Plutus V4 script (introduced in the Dijkstra hard fork). */
    @key(4) final case class PlutusV4(override val script: ByteString) extends PlutusScript
        derives Codec {

        /** Get the script hash for this Plutus V4 script */
        @transient lazy val scriptHash: ScriptHash = Hash(
          platform.blake2b_224(ByteString.unsafeFromArray(4 +: script.bytes))
        )

        def language: Language = Language.PlutusV4
    }

    object PlutusV4 {

        /** Create from an in-memory Program, caching it to preserve source annotations. */
        def apply(program: Program): PlutusV4 = {
            val s = new PlutusV4(program.cborByteString)
            s._cachedProgram = program
            s
        }
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
                case Script.PlutusV4(_) => text("PlutusV4") + hashDoc

    // Variant instances delegate to the main Pretty[Script]
    given Pretty[Script.Native] = (a, style) => prettyScript.pretty(a, style)
    given Pretty[Script.PlutusV1] = (a, style) => prettyScript.pretty(a, style)
    given Pretty[Script.PlutusV2] = (a, style) => prettyScript.pretty(a, style)
    given Pretty[Script.PlutusV3] = (a, style) => prettyScript.pretty(a, style)
    given Pretty[Script.PlutusV4] = (a, style) => prettyScript.pretty(a, style)
}
