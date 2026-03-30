package scalus.uplc.transform

import scalus.uplc.*
import scalus.uplc.Term.*

/** Prepares a UPLC term for flat serialization by replacing BLS12-381 constants that cannot be
  * directly flat-encoded with equivalent runtime expressions.
  *
  * Standalone BLS constants become uncompress(compressed_bytes):
  *   - `Const(BLS12_381_G1_Element(v))` →
  *     `Apply(Builtin(Bls12_381_G1_uncompress), Const(ByteString(compress(v))))`
  *
  * List constants containing BLS elements are unrolled into MkCons chains:
  *   - `Const(List(G1, [v1, v2]))` →
  *     `MkCons(uncompress(compress(v1)), MkCons(uncompress(compress(v2)), Const(List(G1, []))))`
  *     The empty `List[G1Element]` at the end is flat-serializable because the stub Flat instance
  *     for BLS types is never invoked for empty lists.
  *
  * Applied automatically before flat encoding in [[DeBruijnedProgram.flatEncoded]].
  */
object PrepareForSerialization {

    private val ann = UplcAnnotation.empty

    def apply(term: Term): Term = transform(term)

    /** Check if a DefaultUni type contains BLS elements */
    private def containsBLSElement(uni: DefaultUni): Boolean = uni match
        case DefaultUni.BLS12_381_G1_Element               => true
        case DefaultUni.BLS12_381_G2_Element               => true
        case DefaultUni.BLS12_381_MlResult                 => true
        case DefaultUni.Apply(DefaultUni.ProtoList, elem)  => containsBLSElement(elem)
        case DefaultUni.Apply(DefaultUni.ProtoArray, elem) => containsBLSElement(elem)
        case DefaultUni.Apply(DefaultUni.Apply(DefaultUni.ProtoPair, a), b) =>
            containsBLSElement(a) || containsBLSElement(b)
        case _ => false

    private def transform(term: Term): Term = term match
        case Const(c, a) if containsBLSElement(c.tpe) =>
            transformBLSConstant(c, a)
        case Apply(f, arg, a)     => Apply(transform(f), transform(arg), a)
        case LamAbs(n, body, a)   => LamAbs(n, transform(body), a)
        case Force(t, a)          => Force(transform(t), a)
        case Delay(t, a)          => Delay(transform(t), a)
        case Constr(tag, args, a) => Constr(tag, args.map(transform), a)
        case Case(scrut, alts, a) => Case(transform(scrut), alts.map(transform), a)
        case _                    => term

    /** Transform a BLS constant into a runtime expression */
    private def transformBLSConstant(c: Constant, a: UplcAnnotation): Term = c match
        case Constant.BLS12_381_G1_Element(value) =>
            Apply(
              Builtin(DefaultFun.Bls12_381_G1_uncompress),
              Const(Constant.ByteString(value.toCompressedByteString)),
              a
            )
        case Constant.BLS12_381_G2_Element(value) =>
            Apply(
              Builtin(DefaultFun.Bls12_381_G2_uncompress),
              Const(Constant.ByteString(value.toCompressedByteString)),
              a
            )
        case Constant.List(elemType, values) =>
            // Unroll into: MkCons(uncompress(v1), MkCons(uncompress(v2), Const(List(G1, []))))
            // MkCons is polymorphic: Force(Builtin(MkCons))
            val mkCons = Force(Builtin(DefaultFun.MkCons), ann)
            val nil: Term = Const(Constant.List(elemType, scala.List.empty), ann)
            values.foldRight(nil) { (elem, acc) =>
                val uncompressed = transformBLSConstant(elem, ann)
                Apply(Apply(mkCons, uncompressed, ann), acc, ann)
            }
        case Constant.BLS12_381_MlResult(_) =>
            throw new IllegalArgumentException(
              "BLS12_381_MlResult constants cannot be serialized"
            )
        case _: Constant.Pair =>
            throw new IllegalArgumentException(
              "Pair constants containing BLS elements are not supported in flat encoding"
            )
        case _: Constant.Array =>
            throw new IllegalArgumentException(
              "Array constants containing BLS elements are not supported in flat encoding"
            )
        case _ =>
            throw new IllegalArgumentException(
              s"Unexpected BLS constant type: ${c.tpe}"
            )
}
