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

    /** Program decoded straight from [[script]], for scripts built without an in-memory Program. */
    @transient @volatile private var _cachedDeBruijned: DeBruijnedProgram | Null = null

    /** Get the De Bruijn-indexed program, preserving source annotations when available.
      *
      * A script decoded from CBOR has no annotations to preserve, so it is decoded straight to the
      * De Bruijn form the evaluator runs, without a round trip through named variables.
      */
    def deBruijnedProgram: DeBruijnedProgram = {
        val p = _cachedProgram
        if p != null then p.deBruijnedProgram
        else
            val d = _cachedDeBruijned
            if d != null then d
            else
                val decoded = DeBruijnedProgram.fromCbor(script.bytes)
                _cachedDeBruijned = decoded
                decoded
    }

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

    /** Native script (timelock), with the CBOR it arrived in.
      *
      * Haskell memoizes these bytes (`MemoBytes`), and so does this: the script hash, the
      * reference-script size and the encoder use them, and two native scripts are equal only if
      * their bytes are. Build one with `Native(timelock)` or `Native.fromCbor(cbor)`, and read its
      * timelock with `script` or `case Native(t)`.
      */
    @key(0) final case class Native private (binaryScript: KeepRaw[Timelock]) extends Script {

        /** The timelock. */
        def script: Timelock = binaryScript.value

        /** `blake2b_224(0x00 ++ bytes)` of the original bytes, `hashScript` in cardano-ledger. */
        @transient lazy val scriptHash: ScriptHash = Hash(
          platform.blake2b_224(ByteString.unsafeFromArray(0 +: binaryScript.raw))
        )

        override def toString: String = s"Native($script)"
    }

    object Native {

        /** A native script encoded canonically. */
        def apply(script: Timelock): Native = new Native(KeepRaw(script))

        /** A native script decoded from `cbor` that keeps these bytes, a non-minimal encoding
          * included.
          */
        def fromCbor(cbor: Array[Byte]): Native =
            new Native(KeepRaw.unsafe(Timelock.fromCbor(cbor), cbor))

        /** Binds the timelock. */
        def unapply(native: Native): Some[Timelock] = Some(native.script)

        /** Writes the original bytes. Canonical bytes go through the timelock encoder, so borer's
          * validating writer still accepts them; other bytes need `scalus.serialization.cbor.Cbor`.
          */
        given Encoder[Native] = (w, native) =>
            if java.util.Arrays.equals(native.binaryScript.raw, native.script.toCbor) then
                w.write(native.script)
            else w.write(native.binaryScript)

        /** Keeps the bytes the timelock is decoded from. */
        given decoder(using OriginalCborByteArray): Decoder[Native] =
            Decoder(r => new Native(r.read[KeepRaw[Timelock]]()))
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

    given Encoder[Script] = deriveEncoder

    /** Decodes a native script keeping its bytes, which `OriginalCborByteArray` holds. */
    given decoder(using OriginalCborByteArray): Decoder[Script] = deriveDecoder

    /** A script decoded from `cbor`, `[language, script]`, that keeps the bytes of a native script.
      */
    def fromCbor(cbor: Array[Byte]): Script = {
        given OriginalCborByteArray = OriginalCborByteArray(cbor)
        Cbor.decode(cbor).to[Script].value
    }

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
