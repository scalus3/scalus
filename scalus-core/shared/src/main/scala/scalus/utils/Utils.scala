package scalus.utils

import scalus.cardano.ledger.Language
import scalus.cardano.ledger.Language.*
import scalus.uplc.DeBruijnedProgram
import scalus.uplc.Program
import upickle.default.*

import java.nio.file.*

case class PlutusTextEnvelope(`type`: String, description: String, cborHex: String)
    derives ReadWriter

object Utils:
    export Hex.bytesToHex
    export Hex.hexToBytes
    // First character to lowercase
    def lowerFirst(s: String): String =
        if s == null || s.length == 0 || !s.charAt(0).isUpper then s
        else s.updated(0, s.charAt(0).toLower)

    def sha2_256(bytes: Array[Byte]): Array[Byte] =
        val digest = java.security.MessageDigest.getInstance("SHA-256")
        digest.update(bytes)
        digest.digest()

    def sha2_512(bytes: Array[Byte]): Array[Byte] =
        val digest = java.security.MessageDigest.getInstance("SHA-512")
        digest.update(bytes)
        digest.digest()

    def writePlutusFile(
        path: String,
        program: DeBruijnedProgram,
        plutusVersion: Language
    ): Unit =
        val content = programToPlutusFileContent(program, plutusVersion)
        Files.write(Paths.get(path), content.getBytes("UTF-8"))

    def programToPlutusFileContent(
        program: DeBruijnedProgram,
        plutusVersion: Language
    ): String =
        val `type` = plutusVersion match
            case PlutusV1 => "PlutusScriptV1"
            case PlutusV2 => "PlutusScriptV2"
            case PlutusV3 => "PlutusScriptV3"
            case PlutusV4 => "PlutusScriptV3"
        write(PlutusTextEnvelope(`type`, "", program.doubleCborHex))

    private val supportedEnvelopeTypes: Set[String] =
        Set("PlutusScriptV1", "PlutusScriptV2", "PlutusScriptV3")

    def readPlutusFileContent(content: String): Program =
        val envelope = read[PlutusTextEnvelope](content)
        if !supportedEnvelopeTypes.contains(envelope.`type`) then
            throw new IllegalArgumentException(
              s"Unsupported Plutus text envelope type '${envelope.`type`}', " +
                  s"expected one of ${supportedEnvelopeTypes.mkString(", ")}"
            )
        val program = Program.fromDoubleCborHex(envelope.cborHex)
        // Plutus Core 1.1.0 (constr/case) was PlutusV3-only until the van Rossem hard fork, which
        // introduced it for PlutusV1 and PlutusV2 too. A 1.1.0 V1/V2 script only runs at protocol
        // version 11 and later, but it is a valid script and must be readable here, so the
        // accepted versions no longer depend on the envelope type.
        val supportedVersions: Set[(Int, Int, Int)] = Set((1, 0, 0), (1, 1, 0))
        if !supportedVersions.contains(program.version) then
            throw new IllegalArgumentException(
              s"Unsupported Plutus Core version ${program.version}, " +
                  s"expected one of ${supportedVersions.mkString(", ")}"
            )
        program

    def readPlutusFile(path: String): Program =
        val content = new String(Files.readAllBytes(Paths.get(path)), "UTF-8")
        readPlutusFileContent(content)
