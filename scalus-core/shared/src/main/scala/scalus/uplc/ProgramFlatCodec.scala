package scalus.uplc

import scalus.*
import scalus.serialization.flat.DecoderState
import scalus.serialization.flat.EncoderState
import scalus.serialization.flat.Flat

object ProgramFlatCodec:
    private val flatCodec = summon[Flat[DeBruijnedProgram]]

    /** Encodes [[DeBruijnedProgram]] as Flat encoded bytes.
      */
    def encodeFlat(deBruijned: DeBruijnedProgram): Array[Byte] =
        // FIXME, why the hell + 2? +1 should always work with post align.
        val encoderState = new EncoderState(flatCodec.bitSize(deBruijned) / 8 + 2)
        flatCodec.encode(deBruijned, encoderState)
        encoderState.filler()
        val encoded = encoderState.result
        encoded

    /** Encodes [[Program]] as Flat encoded bytes. It assumes the program is correctly de-bruijned.
      * Use it if you know what you're doing.
      */
    def unsafeEncodeFlat(program: Program): Array[Byte] =
        encodeFlat(DeBruijnedProgram(program.version, program.term))

    /** Decodes Flat-encoded [[DeBruijnedProgram]] from bytes */
    def decodeFlat(encoded: Array[Byte]): DeBruijnedProgram =
        val decoderState = DecoderState(encoded)
        flatCodec.decode(decoderState)

    case class DecodeResult(program: DeBruijnedProgram, remainder: Array[Byte])

    /** Decodes Flat-encoded [[DeBruijnedProgram]] from bytes, returning any remaining bytes that
      * were not part of the program.
      */
    def decodeFlatWithRemainingBytes(
        encoded: Array[Byte]
    ): DecodeResult =
        val decoderState = DecoderState(encoded)
        val deBruijnedProgram = flatCodec.decode(decoderState)
        decoderState.filler()
        DecodeResult(deBruijnedProgram, decoderState.remainingBytes())
