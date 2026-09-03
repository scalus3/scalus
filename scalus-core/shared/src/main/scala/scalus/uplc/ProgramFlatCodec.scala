package scalus.uplc

import scalus.*
import scalus.serialization.flat.DecoderState
import scalus.serialization.flat.EncoderState
import scalus.serialization.flat.Flat

object ProgramFlatCodec:
    private val flatCodec = summon[Flat[DeBruijnedProgram]]

    /** The first UPLC version that can carry `constr`/`case` terms. */
    val minVersionForConstrCase: (Int, Int, Int) = (1, 1, 0)

    /** Encodes [[DeBruijnedProgram]] as Flat encoded bytes.
      *
      * Mirrors the check the Plutus decoder makes ("'constr' is not allowed before version 1.1.0"):
      * a program that declares a version below 1.1.0 but contains `constr`/`case` can never be
      * deserialized by the ledger, so refusing to encode it turns a script that fails on submission
      * into an error at the point the bad version was chosen.
      *
      * @throws IllegalArgumentException
      *   if the program version is below 1.1.0 and the term uses `constr` or `case`
      */
    def encodeFlat(deBruijned: DeBruijnedProgram): Array[Byte] =
        import scala.math.Ordering.Implicits.infixOrderingOps
        require(
          deBruijned.version >= minVersionForConstrCase || !deBruijned.term.usesConstrOrCase,
          s"UPLC program version ${deBruijned.version} cannot carry constr/case terms: they need " +
              s"version $minVersionForConstrCase or later. The ledger would reject this script at " +
              "deserialization."
        )
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
