package scalus.uplc

import scalus.*
import scalus.serialization.flat.DecoderState
import scalus.serialization.flat.EncoderState
import scalus.serialization.flat.Flat
import scalus.serialization.flat.FlatDecodingError

object ProgramFlatCodec:
    private val flatCodec = summon[Flat[DeBruijnedProgram]]

    /** The first UPLC version that can carry `constr`/`case` terms. */
    val minVersionForConstrCase: (Int, Int, Int) = (1, 1, 0)

    /** Rejects a program that declares a version below 1.1.0 but contains `constr`/`case`.
      *
      * This is the check the Plutus decoder makes ("'constr' is not allowed before version 1.1.0"),
      * applied on both sides of this codec: on encode, so Scalus cannot produce bytes the ledger
      * will reject; on decode, so Scalus does not accept bytes the ledger would not.
      *
      * @throws IllegalArgumentException
      *   if the program version is below 1.1.0 and the term uses `constr` or `case`
      */
    def requireVersionCanCarry(program: DeBruijnedProgram): Unit =
        import scala.math.Ordering.Implicits.infixOrderingOps
        require(
          program.version >= minVersionForConstrCase || !program.term.usesConstrOrCase,
          s"UPLC program version ${program.version} cannot carry constr/case terms: they need " +
              s"version $minVersionForConstrCase or later. The ledger would reject this script at " +
              "deserialization."
        )

    /** Encodes [[DeBruijnedProgram]] as Flat encoded bytes.
      *
      * @throws IllegalArgumentException
      *   see [[requireVersionCanCarry]]
      */
    def encodeFlat(deBruijned: DeBruijnedProgram): Array[Byte] =
        requireVersionCanCarry(deBruijned)
        // bitSize bits + the mandatory trailing filler byte: after writing bitSize bits the
        // encoder has filled bitSize / 8 whole bytes, and filler() consumes exactly one more
        // slot, so bitSize / 8 + 1 is the exact upper bound (same sizing as FlatCodec.encode).
        val encoderState = new EncoderState(flatCodec.bitSize(deBruijned) / 8 + 1)
        flatCodec.encode(deBruijned, encoderState)
        encoderState.filler()
        val encoded = encoderState.result
        encoded

    /** Encodes [[Program]] as Flat encoded bytes. It assumes the program is correctly de-bruijned.
      * Use it if you know what you're doing.
      */
    def unsafeEncodeFlat(program: Program): Array[Byte] =
        encodeFlat(DeBruijnedProgram(program.version, program.term))

    /** Decodes Flat-encoded [[DeBruijnedProgram]] from bytes.
      *
      * The bytes must hold the program and its filler and nothing else, as `unflat` in Plutus
      * requires: the filler, any number of 0 bits and then a 1 bit, ends on the last bit of the
      * input. A filler longer than it needs to be is allowed. Use [[decodeFlatWithRemainingBytes]]
      * when other bytes follow the program.
      *
      * @throws FlatDecodingError
      *   if the filler is missing or ends inside a byte, or if bytes follow it
      * @throws IllegalArgumentException
      *   see [[requireVersionCanCarry]]
      */
    def decodeFlat(encoded: Array[Byte]): DeBruijnedProgram = {
        val decoderState = DecoderState(encoded)
        val program = decodeWithFiller(decoderState)
        val remaining = decoderState.availableBytes()
        if remaining != 0 then
            throw new FlatDecodingError(s"$remaining bytes follow the filler of the program")
        program
    }

    case class DecodeResult(program: DeBruijnedProgram, remainder: Array[Byte])

    /** Decodes Flat-encoded [[DeBruijnedProgram]] from bytes, returning any remaining bytes that
      * were not part of the program.
      *
      * @throws FlatDecodingError
      *   if the filler is missing or ends inside a byte
      * @throws IllegalArgumentException
      *   see [[requireVersionCanCarry]]
      */
    def decodeFlatWithRemainingBytes(
        encoded: Array[Byte]
    ): DecodeResult = {
        val decoderState = DecoderState(encoded)
        val deBruijnedProgram = decodeWithFiller(decoderState)
        DecodeResult(deBruijnedProgram, decoderState.remainingBytes())
    }

    /** Decodes a program and the filler after it, which must end on a byte boundary, as `dFiller`
      * and `strictDecoder` in Plutus read it.
      */
    private def decodeWithFiller(decoderState: DecoderState): DeBruijnedProgram = {
        val program = flatCodec.decode(decoderState)
        try decoderState.filler()
        catch
            case e: FlatDecodingError =>
                throw new FlatDecodingError("The program has no filler after it", e)
        if decoderState.usedBits != 0 then
            throw new FlatDecodingError("The filler of the program ends inside a byte")
        requireVersionCanCarry(program)
        program
    }
