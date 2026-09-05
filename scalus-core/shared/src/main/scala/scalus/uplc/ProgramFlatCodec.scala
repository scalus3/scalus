package scalus.uplc

import scalus.*
import scalus.serialization.flat.DecoderState
import scalus.serialization.flat.EncoderState
import scalus.serialization.flat.Flat

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
      * @throws IllegalArgumentException
      *   see [[requireVersionCanCarry]]
      */
    def decodeFlat(encoded: Array[Byte]): DeBruijnedProgram =
        val decoderState = DecoderState(encoded)
        val program = flatCodec.decode(decoderState)
        requireVersionCanCarry(program)
        program

    case class DecodeResult(program: DeBruijnedProgram, remainder: Array[Byte])

    /** Decodes Flat-encoded [[DeBruijnedProgram]] from bytes, returning any remaining bytes that
      * were not part of the program.
      *
      * @throws IllegalArgumentException
      *   see [[requireVersionCanCarry]]
      */
    def decodeFlatWithRemainingBytes(
        encoded: Array[Byte]
    ): DecodeResult =
        val decoderState = DecoderState(encoded)
        val deBruijnedProgram = flatCodec.decode(decoderState)
        decoderState.filler()
        requireVersionCanCarry(deBruijnedProgram)
        DecodeResult(deBruijnedProgram, decoderState.remainingBytes())
